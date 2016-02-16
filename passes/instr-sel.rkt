#lang racket

(require "utils.rkt")

(provide instr-sel)

; In this pass, we also generate an arg (var x).

;; NOTE: Instructions selection doesn't flatten if-statements. The reason is
;; because we need branches for some analysis in next passes (liveness analysis,
;; which effects register allocation, and probably some other passes), and we
;; don't have basic blocks.

(define (instr-sel pgm)
  (match pgm
    [`(program ,vs . ,stmts)
     ; (printf "stmts: ~s~n" stmts)
     ; FIXME: We're creating fresh variables for allocation-related statements.
     (let* ([new-vars (mutable-set)]
            [instrs (append-map (lambda (stmt) (instr-sel-stmt new-vars stmt)) stmts)])
       `(program ,(append vs (set->list new-vars)) ,@instrs))]
    [_ (unsupported-form 'instr-sel pgm)]))

; TEMP: I don't want to do big refactorings, adding a mutable argument for now.
(define (instr-sel-stmt new-vars stmt)
  (let iter ([stmt stmt])
    (match stmt
      [`(assign ,var ,expr)
       (instr-sel-expr new-vars var expr)]

      [`(return ,arg)
       `((movq ,(arg->x86-arg arg) (reg rax)))]

      [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
       `((if (eq? ,(arg->x86-arg arg1) ,(arg->x86-arg arg2))
           ,(append-map iter pgm-t)
           ,(append-map iter pgm-f)))]

      [`(if (collection-needed? ,bytes-needed) ,pgm-t ,pgm-f)
       (let ([free-ptr-updated (gensym "free_ptr_updated")]
             [fromspace-end (gensym "fromspace_end")])
         (set-add! new-vars free-ptr-updated)
         (set-add! new-vars fromspace-end)
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
           ; TODO: We're assuming the space grows upwards here. Make sure this is
           ; really the case? (it should be as this is heap)
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
       (let ([rootstack-ptr (gensym "rootstack")])
         (set-add! new-vars rootstack-ptr)
         `(; Step 1: Move roots to the root stack
           (movq (global-value rootstack_begin) (var ,rootstack-ptr))
           ,@(map (lambda (idx root)
                    `(movq ,(arg->x86-arg root) (offset (var ,rootstack-ptr) ,(* 8 idx))))
                  (range (length roots)) roots)

           ; Step 2: Set up arguments for `collect`
           ; TODO: I don't understand why we need to pass root stack pointer here.
           (movq (global-value rootstack_begin) (reg rdi))
           (movq (int ,bytes-needed) (reg rsi))

           ; Step 3: Call the collector
           (callq collect)

           ; Step 4: Move new roots back to the variables
           (movq (global-value rootstack_begin) (var ,rootstack-ptr))
           ,@(map (lambda (idx root)
                    `(movq (offset (var ,rootstack-ptr) ,(* 8 idx))
                           ,(arg->x86-arg root)))
                  (range (length roots)) roots)))]

      [`(vector-set! ,vec ,idx ,val)
       (let ([offset (+ 8 (* 8 idx))])
         ;; TODO: Does `offset` form work here? What happens if vec is on stack?
         `((movq ,(arg->x86-arg val) (offset ,(arg->x86-arg vec) ,offset))))]

      [_ (unsupported-form 'instr-sel-stmt stmt)])))

(define (instr-sel-expr new-vars bind-to expr)
  (match expr
    [(or (? fixnum?) (? symbol?) (? boolean?))
     `(,(instr-sel-arg bind-to expr))]

    [`(read)
     `((callq read_int)
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

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
                                                    (eq? (car obj-type) 'vector))
                                             idx '()))
                                         (range (length obj-types)) obj-types))]

            [length-bits (arithmetic-shift (length obj-types) 1)]
            ; TODO: We need to do some range checking here.
            [bitfield (arithmetic-shift (bitfield-from-bit-idxs ptr-idxs) 7)]
            [obj-tag (bitwise-ior length-bits bitfield)]
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

    [_ (unsupported-form 'instr-sel-expr expr)]))

(define (instr-sel-arg bind-to arg)
  `(movq ,(arg->x86-arg arg) ,(arg->x86-arg bind-to)))

(define (arg->x86-arg arg)
  (cond [(symbol? arg) `(var ,arg)]
        [(fixnum? arg) `(int ,arg)]
        [(boolean? arg) `(int ,(if arg 1 0))]
        ; [(boolean? arg) `(bool ,arg)]
        [else (unsupported-form 'arg->x86-arg arg)]))
