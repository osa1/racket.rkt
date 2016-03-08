#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide instr-sel collect-vars collect-vars-instrs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (instr-sel pgm)
  (match pgm
    [`(program . ,defs)
     ; (printf "stmts: ~s~n" stmts)
     ; FIXME: We're creating fresh variables for allocation-related statements.
     `(program ,@(map instr-sel-def defs))]
    [_ (unsupported-form 'instr-sel pgm)]))

(define (instr-sel-def def)
  (match def
    [`(define ,tag : ,ret-ty . ,stmts)
     (let ([instrs (save-callee-saves (append-map instr-sel-stmt stmts))])
       `(define ,tag : ,ret-ty ,@instrs))]
    [_ (unsupported-form 'instr-sel-def def)]))

; We move callee-save registers to temporary variables, and restore them just
; before the return. It's register allocator's job to be smart and remove these
; movs when not necessary.
(define (save-callee-saves instrs)
  (define temps (map (lambda (reg-sym)
                       (cons `(var ,(fresh (string-append "save_" (symbol->string reg-sym) "_")))
                             `(reg ,reg-sym)))
                     callee-save-syms))

  (append (map (lambda (r) `(movq ,(cdr r) ,(car r))) temps)
          instrs
          (map (lambda (r) `(movq ,(car r) ,(cdr r))) temps)))

; TEMP: I don't want to do big refactorings, adding a mutable argument for now.
(define (instr-sel-stmt stmt)
  (let iter ([stmt stmt])
    (match stmt
      [`(assign ,var ,expr)
       (instr-sel-expr var expr)]

      [`(return ,arg)
       `((movq ,(arg->x86-arg arg) (reg rax)))]

      [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
       `((if (eq? ,(arg->x86-arg arg1) ,(arg->x86-arg arg2))
           ,(append-map iter pgm-t)
           ,(append-map iter pgm-f)))]

      [`(if (collection-needed? ,bytes-needed) ,pgm-t ,pgm-f)
       (let ([free-ptr-updated (fresh "free_ptr_updated")]
             [fromspace-end (fresh "fromspace_end")])
         ; Here's how instructions used here work:
         ;
         ;    cmpq: is setting the comparison flags.
         ;    setl: "set byte if less". result will be 1 if free-ptr-updated is
         ;          less than fromspace_end.
         `((movq (global-value free_ptr) (var ,free-ptr-updated))
           (addq (int ,bytes-needed) (var ,free-ptr-updated))
           (movq (global-value fromspace_end) (var ,fromspace-end))
           (cmpq (var ,free-ptr-updated) (var ,fromspace-end))

           ; NOTE the setl instead of sete! We need to check if free-ptr-updated
           ; is bigger than fromspace_end!
           (setl (byte-reg al))

           ; reusing free-ptr-updated here for the eq? test
           (movzbq (byte-reg al) (var ,free-ptr-updated))
           ; do we really need eq? test here? generated code will suffer a little
           (if (eq? (int 0) (var ,free-ptr-updated))
             ; rax == 0 means free-ptr-updated is smaller than fromspace_end, so
             ; no allocatins needed
             ,(append-map iter pgm-f)
             ,(append-map iter pgm-t))))]

      [`(call-live-roots ,roots (collect ,bytes-needed))
       (let ([rootstack-ptr (fresh "rootstack")])
         `(; Step 1: Move roots to the root stack
           ,@(if (not (null? roots))
               `((movq (global-value rootstack_begin) (var ,rootstack-ptr))
                 ,@(map (lambda (idx root)
                          `(movq ,(arg->x86-arg root) (offset (var ,rootstack-ptr) ,(* 8 idx))))
                        (range (length roots)) roots))
               `())

           ; Step 2: Set up arguments for `collect`
           (movq (global-value rootstack_begin) (reg rdi))
           ,@(if (not (null? roots))
               `((addq (int ,(* 8 (length roots))) (reg rdi)))
               `())
           (movq (int ,bytes-needed) (reg rsi))

           ; Step 3: Call the collector
           (callq (toplevel-fn collect))

           ; Step 4: Move new roots back to the variables
           ,@(if (not (null? roots))
               `((movq (global-value rootstack_begin) (var ,rootstack-ptr))
                 ,@(map (lambda (idx root)
                          `(movq (offset (var ,rootstack-ptr) ,(* 8 idx))
                                 ,(arg->x86-arg root)))
                        (range (length roots)) roots))
               `())))]

      [`(vector-set! ,vec ,idx ,val)
       (let ([offset (+ 8 (* 8 idx))])
         `((movq ,(arg->x86-arg val) (offset ,(arg->x86-arg vec) ,offset))))]

      [_ (unsupported-form 'instr-sel-stmt stmt)])))

(define (instr-sel-expr bind-to expr)
  (match expr
    [(or (? fixnum?) (? symbol?) (? boolean?))
     `(,(instr-sel-arg bind-to expr))]

    [`(- ,arg)
     `(,(instr-sel-arg bind-to arg)
       (negq ,(arg->x86-arg bind-to)))]

    [`(not ,arg)
     `(,(instr-sel-arg bind-to arg)
       (xorq (int 1) ,(arg->x86-arg bind-to)))]

    [`(eq? ,arg1 ,arg2)
     (let ([arg2-x86 (arg->x86-arg arg2)])
       `(; cmpq wants immediate value to be the first arg
         ,(if (arg-imm? arg2-x86)
            `(cmpq ,arg2-x86 ,(arg->x86-arg arg1))
            `(cmpq ,(arg->x86-arg arg1) ,arg2-x86))
         (sete (byte-reg al))
         (movzbq (byte-reg al) ,(arg->x86-arg bind-to))))]

    [`(+ ,arg1 ,arg2)
     `(,(instr-sel-arg bind-to arg1)
       (addq ,(arg->x86-arg arg2) ,(arg->x86-arg bind-to)))]

    [`(allocate ,obj-types)
     ; We need to allocate 8 bytes (for header) + 1 dword for each object
     (let* ([alloc-size (+ 8 (* 8 (length obj-types)))]
            [ptr-idxs (filter-nulls (map (lambda (idx obj-type)
                                           (if (and (list? obj-type)
                                                    (eq? (car obj-type) 'Vector))
                                             idx '()))
                                         (range (length obj-types)) obj-types))]

            [length-bits (arithmetic-shift (length obj-types) 1)]
            ; TODO: We need to do some range checking here.
            [bitfield (arithmetic-shift (bitfield-from-bit-idxs ptr-idxs) 7)]
            [obj-tag (bitwise-ior length-bits bitfield 1)]
            [vec-arg (arg->x86-arg bind-to)])
       `(; Step 0: Read the free_ptr
         (movq (global-value free_ptr) ,vec-arg)
         ; Step 1: Do the actual allocation (bump the pointer)
         (addq (int ,alloc-size) (global-value free_ptr))
         ; Step 2: Write the info tag
         (movq (int ,obj-tag) (offset ,vec-arg 0))))]

    [`(vector-ref ,vec ,idx)
     `((movq (offset ,(arg->x86-arg vec) ,(+ 8 (* 8 idx))) ,(arg->x86-arg bind-to)))]

    [`(vector-set! ,vec ,idx ,val)
     ;; Note that we ignore bind-to here!
     `((movq ,(arg->x86-arg val) (offset ,(arg->x86-arg vec) ,(+ 8 (* 8 idx)))))]

    [`(toplevel-fn ,_)
     `((leaq ,(arg->x86-arg expr) ,(arg->x86-arg bind-to)))]

    [`(app ,f . ,args)
     ; Since all arguments are word-sized this is easy. Otherwise we'd need
     ; some type annotations here and the handling would be more complex.
     ;
     ; Passing in registers
     ; ~~~~~~~~~~~~~~~~~~~~
     ; %rdi, %rsi, %rdx, %rcx, %r8 and %r9 <- 6 arguments
     ; (note that these are a subset of caller-save registers, which is good
     ; because this will make of register allocator easier -- they will be
     ; dead before the function call)
     ;
     ;
     ; Passing on stack
     ; ~~~~~~~~~~~~~~~~
     ; Push the rest to the stack in reversed order. E.g. last argument
     ; pushed first.
     (let-values ([(reg-args stack-args) (split-at-max args (length arg-regs))])
       ; Push stack args in reverse order
       (let* ([stack-args (reverse stack-args)]
              ; [regs-to-save caller-save-regs]
              ; [save-temps (map (lambda (reg)
              ;                    (cons
              ;                      `(var ,(fresh (string-append "save_"
              ;                                                    (symbol->string (cadr reg))
              ;                                                    "_")))
              ;                      reg))
              ;                  regs-to-save)]
              )

         ; FIXME: We don't support passing arguments on stack.
         (when (not (null? stack-args))
           (error 'instr-sel "We don't support passing arguments on stack: ~a~n" expr))

         `(; NOTE: We don't save caller-save registers here! It's taken care of
           ; by the register allocation, because we interfere caller-save
           ; registers with live variables after a callq. So if a variable
           ; needs to live across a function call, it gets spilled (or saved to
           ; another register) by the register allocator. Cool!

           ; Save caller-save registers
           ; ,@(map (lambda (r) `(movq ,(cdr r) ,(car r))) save-temps)

           ; Move register args
           ,@(map (lambda (arg reg)
                    `(movq ,(arg->x86-arg arg) ,reg)) reg-args (take arg-regs (length reg-args)))

           (callq ,(arg->x86-arg f))

           ; Move return value to its destination
           (movq (reg rax) ,(arg->x86-arg bind-to))

           ; Restore caller-save registers
           ; ,@(map (lambda (r) `(movq ,(car r) ,(cdr r))) save-temps)
           )))]

    [_ (unsupported-form 'instr-sel-expr expr)]))

(define (instr-sel-arg bind-to arg)
  `(movq ,(arg->x86-arg arg) ,(arg->x86-arg bind-to)))

(define (arg->x86-arg arg)
  (match arg
    [(? symbol? arg) `(var ,arg)]
    [(? fixnum? arg) `(int ,arg)]
    [(? boolean? arg) `(int ,(if arg 1 0))]
    [`(toplevel-fn ,_) arg]
    [_ (unsupported-form 'arg->x86-arg arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-vars def)
  (match def
    [`(define ,_ : ,_ . ,instrs)
     (collect-vars-instrs instrs)]
    [_ (unsupported-form 'collect-vars def)]))

(define (collect-vars-instrs instrs)
  (foldl set-union (set) (map collect-vars-instr instrs)))

(define (collect-vars-instr instr)
  (match instr
    [(or `(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
         `(if (eq? ,arg1 ,arg2) ,pgm-t ,_ ,pgm-f ,_))
     (set-union (collect-vars-arg arg1)
                (collect-vars-arg arg2)
                (collect-vars-instrs pgm-t)
                (collect-vars-instrs pgm-f))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,arg1 ,arg2)
     (set-union (collect-vars-arg arg1) (collect-vars-arg arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     (collect-vars-arg  arg)]

    [`(callq ,arg) (collect-vars-arg arg)]

    [`(retq) (set)]

    [`(,(or 'sete 'setl) (byte-reg al)) (set)]

    [`(movzbq (byte-reg al) ,arg) (collect-vars-arg arg)]

    [_ (unsupported-form 'collect-vars-instr instr)]))

(define (collect-vars-arg arg)
  (match arg
    [`(int ,_) (set)]
    [`(reg ,_) (set)]
    [`(stack ,_) (set)]
    [`(global-value ,_) (set)]
    [`(offset ,arg ,offset)
     (set-union (collect-vars-arg arg))]
    [`(toplevel-fn ,_) (set)]
    [`(mem-loc ,_) (set)]
    [`(var ,var) (set var)]
    [_ (unsupported-form 'collect-vars-arg arg)]))
