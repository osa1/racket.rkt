#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide instr-sel collect-vars collect-vars-instrs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (instr-sel pgm)
  (match pgm
    [`(program . ,defs)
     (define toplevels (init-toplevels))
     `(program (,toplevels) ,@(map (instr-sel-def toplevels) defs))]
    [_ (unsupported-form 'instr-sel pgm)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We maintain a map `type -> (symbol, serialization of the type)` to avoid
; re-serialization of same types again and again and to be able to call
; inject() runtime function without doing allocations of messing with vararg
; passing.

(define (init-toplevels) (make-hash))

(define (add-toplevel-type toplevels type)
  (define map-ent (hash-ref toplevels type #f))
  (if map-ent
    (car map-ent)
    (let ([bytes (encode-type type)]
          [sym (fresh "type")])
      (begin
        (hash-set! toplevels type (cons sym bytes))
        sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (instr-sel-def toplevels)
  (lambda (def)
    (match def
      [`(define main : void . ,stmts)
       (let ([instrs (save-callee-saves (append-map (instr-sel-stmt toplevels) stmts))])
         `(define main : void ,@instrs))]

      [`(define (,fname . ,args) : ,ret-ty . ,stmts)
       (let ([instrs
               (save-callee-saves
                 (append (move-arg-regs (map car args))
                         (append-map (instr-sel-stmt toplevels) stmts)))])
         `(define (,fname ,@args) : ,ret-ty ,@instrs))]

      [`(define-closure-wrapper . ,_) def]

      [_ (unsupported-form 'instr-sel-def def)])))

(define (move-arg-regs args)

  ; We do 'cdr' on arg-reg here because the first argument is always a pointer
  ; to the closure.

  (define-values (reg-args stack-args) (split-at-max args (length arg-regs)))

  (append
    (map (lambda (arg reg)
           `(movq ,reg (var ,arg)))
         reg-args (take arg-regs (length reg-args)))

    (map (lambda (idx arg)
           ; Using negative index as indicator
           `(movq (mem-loc ,(- idx)) (var ,arg)))
         (range 1 (+ (length stack-args) 1)) stack-args)))

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
(define (instr-sel-stmt toplevels)
  (lambda (stmt)
    (let iter ([stmt stmt])
      (match stmt

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; GC-related parts

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
           `(,@(if (not (null? roots))
                 `(; Move roots to the root stack
                   (movq (global-value rootstack_ptr) (var ,rootstack-ptr))
                   ,@(map (lambda (idx root)
                            `(movq ,(arg->x86-arg root) (offset (var ,rootstack-ptr) ,(* 8 idx))))
                          (range (length roots)) roots)

                   ; Update rootstack_ptr
                   (addq (int ,(* 8 (length roots))) (global-value rootstack_ptr)))
                 `())

             ; Set up the argument for `collect`
             (movq (int ,bytes-needed) (reg rdi))

             ; Call the collector
             (callq 2 (toplevel-fn collect))

             ,@(if (not (null? roots))
                 `(; Restore rootstack_ptr
                   (subq (int ,(* 8 (length roots))) (global-value rootstack_ptr))

                   ; Move new roots back to the variables
                   (movq (global-value rootstack_ptr) (var ,rootstack-ptr))
                   ,@(map (lambda (idx root)
                            `(movq (offset (var ,rootstack-ptr) ,(* 8 idx))
                                   ,(arg->x86-arg root)))
                          (range (length roots)) roots))
                 `())))]

        [`(call-live-roots ,roots ,stmt)
         (let ([rootstack-ptr (fresh "rootstack")])
           `(,@(if (not (null? roots))
                 `(; Move roots to the root stack
                   (movq (global-value rootstack_ptr) (var ,rootstack-ptr))
                   ,@(map (lambda (idx root)
                            `(movq ,(arg->x86-arg root) (offset (var ,rootstack-ptr) ,(* 8 idx))))
                          (range (length roots)) roots)

                   ; Bump the rootstack ptr
                   (addq (int ,(* 8 (length roots))) (global-value rootstack_ptr)))
                 `())

             ; Run the main thing
             ,@((instr-sel-stmt toplevels) stmt)

             ,@(if (not (null? roots))
                 `(; Restore the rootstack ptr
                   (subq (int ,(* 8 (length roots))) (global-value rootstack_ptr))

                   ; Move new roots back to the variables
                   (movq (global-value rootstack_ptr) (var ,rootstack-ptr))
                   ,@(map (lambda (idx root)
                            `(movq (offset (var ,rootstack-ptr) ,(* 8 idx))
                                   ,(arg->x86-arg root)))
                          (range (length roots)) roots))
                 `())))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        [`(assign ,var ,expr)
         (instr-sel-expr toplevels var expr)]

        [`(return ,arg)
         `((movq ,(arg->x86-arg arg) (reg rax)))]

        [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
         `((if (eq? ,(arg->x86-arg arg1) ,(arg->x86-arg arg2))
             ,(append-map iter pgm-t)
             ,(append-map iter pgm-f)))]

        [`(vector-set! ,vec ,idx ,val)
         (let ([offset (+ 8 (* 8 idx))])
           `((movq ,(arg->x86-arg val) (offset ,(arg->x86-arg vec) ,offset))))]

        [`(vector-set!-dynamic ,vec ,idx ,val)
         (define vec-arg (arg->x86-arg vec))
         (define idx-arg (arg->x86-arg idx))
         (define val-arg (arg->x86-arg val))
         `((addq (int 1) ,idx-arg)
           (imulq (int 8) ,idx-arg)
           (addq ,idx-arg ,vec-arg)
           (movq ,val-arg (offset ,vec-arg 0)))]

        [_ (unsupported-form 'instr-sel-stmt stmt)]))))

(define (instr-sel-expr toplevels bind-to expr)
  (match expr
    [(or (? fixnum?) (? symbol?) (? boolean?))
     `(,(instr-sel-arg bind-to expr))]

    [`(void) '()]

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

    [`(eq?-dynamic ,arg1 ,arg2)
     `((movq ,(arg->x86-arg arg1) (reg rdi))
       (movq ,(arg->x86-arg arg2) (reg rsi))
       (callq 2 (toplevel-fn eq_dynamic))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(+ ,arg1 ,arg2)
     `(,(instr-sel-arg bind-to arg1)
       (addq ,(arg->x86-arg arg2) ,(arg->x86-arg bind-to)))]

    [`(* ,arg1 ,arg2)
     `(,(instr-sel-arg bind-to arg1)
       (imulq ,(arg->x86-arg arg2) ,(arg->x86-arg bind-to)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Integer comparisons

    ; lahf: AH <- EFLAGS(SF:ZF:0:AF:0:PF:1:CF)
    ;
    ; SF=1 means the result of subtraction was negative, i.e. arg2 is greater,
    ; so the test has succeeded.
    ;
    ; ZF=1 means the result of subtraction was zero, i.e. argrs are equal.
    ;
    ; SF is 8th bit of %ah, 16th bit of %ax, %eax and %rax, after a 'lahf'.
    ; ZF is 7th bit of %ah, 15th bit of %ax, %eax and %rax, after a 'lahf'.
    ;
    ; NOTE: Assuming everything except 0 is considered 'true'.

    [`(< ,arg1 ,arg2)
     `((cmpq ,(arg->x86-arg arg2) ,(arg->x86-arg arg1))
       (lahf)
       (movq (int #b1000000000000000) ,(arg->x86-arg bind-to))
       ; 'and' operand sizes need to be of same size
       (andq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(<= ,arg1 ,arg2)
     `((cmpq ,(arg->x86-arg arg2) ,(arg->x86-arg arg1))
       (lahf)
       (movq (int #b1100000000000000) ,(arg->x86-arg bind-to))
       ; 'and' operand sizes need to be of same size
       (andq (reg rax) ,(arg->x86-arg bind-to)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [`(allocate ,obj-types)
     ; We need to allocate 8 bytes (for header) + 1 dword for each object
     (let* ([alloc-size (+ 8 (* 8 (length obj-types)))]
            [obj-tag (vec-info-field obj-types)]
            [vec-arg (arg->x86-arg bind-to)])
       `(; Step 0: Read the free_ptr
         (movq (global-value free_ptr) ,vec-arg)
         ; Step 1: Do the actual allocation (bump the pointer)
         (addq (int ,alloc-size) (global-value free_ptr))
         ; Step 2: Write the info tag
         (movq (int ,obj-tag) (offset ,vec-arg 0))))]

    [`(vector-ref ,vec ,idx)
     `((movq (offset ,(arg->x86-arg vec) ,(+ 8 (* 8 idx))) ,(arg->x86-arg bind-to)))]

    [`(vector-ref-dynamic ,e1 ,e2)
     (define e1-arg (arg->x86-arg e1))
     (define e2-arg (arg->x86-arg e2))
     (define bind-to-arg (arg->x86-arg bind-to))
     `((addq (int 1) ,e2-arg)
       (imulq (int 8) ,e2-arg)
       (addq ,e2-arg ,e1-arg)
       (movq (offset ,e1-arg 0) ,bind-to-arg))]

    [`(vector-set! ,vec ,idx ,val)
     ;; Note that we ignore bind-to here!
     `((movq ,(arg->x86-arg val) (offset ,(arg->x86-arg vec) ,(+ 8 (* 8 idx)))))]

    [`(vector-set!-dynamic ,vec ,idx ,val)
     (define vec-arg (arg->x86-arg vec))
     (define idx-arg (arg->x86-arg idx))
     (define val-arg (arg->x86-arg val))
     `((addq (int 1) ,idx-arg)
       (imulq (int 8) ,idx-arg)
       (addq ,idx-arg ,vec-arg)
       (movq ,val-arg (offset ,vec-arg 0)))]

    [`(toplevel-fn ,_)
     `((leaq ,(arg->x86-arg expr) ,(arg->x86-arg bind-to)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; inject/project and type

    [`(integer? ,arg)
     `((movq ,(arg->x86-arg arg) (reg rdi))
       (callq 1 (toplevel-fn is_integer))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(boolean? ,arg)
     `((movq ,(arg->x86-arg arg) (reg rdi))
       (callq 1 (toplevel-fn is_boolean))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(vector? ,arg)
     `((movq ,(arg->x86-arg arg) (reg rdi))
       (callq 1 (toplevel-fn is_vector))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(procedure? ,arg)
     `((movq ,(arg->x86-arg arg) (reg rdi))
       (callq 1 (toplevel-fn is_procedure))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(inject . ,_)
     (error 'instr-sel-expr "inject should have been eliminated in expose-allocations: ~a~n" expr)]

    [`(project ,arg1 ,ty)
     ; Write encoding of ty to a labelled address, pass pointer to that address
     ; to the project() runtime function.
     (define ty-label (add-toplevel-type toplevels ty))
     `((movq ,(arg->x86-arg arg1) (reg rdi))
       (leaq (global-value ,ty-label) (reg rsi))
       (callq 2 (toplevel-fn project))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(project-boolean ,arg1)
     `((movq ,(arg->x86-arg arg1) (reg rdi))
       (callq 1 (toplevel-fn project_boolean))
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         `(; NOTE: We don't save caller-save registers here! It's taken care of
           ; by the register allocation, because we interfere caller-save
           ; registers with live variables after a callq. So if a variable
           ; needs to live across a function call, it gets spilled (or saved to
           ; another register) by the register allocator. Cool!

           ; Save caller-save registers
           ; ,@(map (lambda (r) `(movq ,(cdr r) ,(car r))) save-temps)

           ; Move register args
           ,@(map (lambda (arg reg)
                    `(movq ,(arg->x86-arg arg) ,reg))
                  reg-args (take arg-regs (length reg-args)))

           ; Move stack args.
           ; I don't like pushq/popq, but that's all I could think of at the moment.
           ; Make sure the stack will stay aligned:
           ,@(if (not (null? stack-args))
               (if (odd? (length stack-args))
                 `((subq (int 8) (reg rsp)))
                 `())
               `())

           ,@(map (lambda (arg) `(pushq ,(arg->x86-arg arg))) stack-args)

           (callq ,(length args) ,(arg->x86-arg f))

           ,@(if (not (null? stack-args))
               (if (odd? (length stack-args))
                 `((addq (int ,(* 8 (+ (length stack-args) 1))) (reg rsp)))
                 `((addq (int ,(* 8    (length stack-args)   )) (reg rsp))))
               `())

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

    [`(,(or 'addq 'subq 'imulq 'movq 'leaq 'cmpq 'xorq 'andq) ,arg1 ,arg2)
     (set-union (collect-vars-arg arg1) (collect-vars-arg arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     (collect-vars-arg  arg)]

    [`(callq ,_ ,arg) (collect-vars-arg arg)]

    [`(retq) (set)]

    [`(lahf) (set)]

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
