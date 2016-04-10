#lang racket

(require "../utils.rkt")

(provide gen-live-afters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live-after sets

; OUTPUT: (values updated def, a list of live-after sets).
; We update the program to annotate branches of if-statements with live-after
; set lists, to be able to build interference graph by traversing the branches.
(define (gen-live-afters def)
  (match def
    [`(define ,tag : ,ret-ty . ,instrs)
     (let-values [((instrs live-before live-afters)
                   (gen-live-afters-instrs
                     (reverse instrs)
                     ; Accumulator for live-after sets, we add the live-after
                     ; set for the last instruction here. See NOTE [Live-afters
                     ; for the last instruction].
                     (list (list->set callee-save-regs))))]
       ; Here's a sanity check: Having variables in live-after sets doesn't
       ; make sense
       (for ([set live-afters])
         (unless (null? (filter is-reg? (set->list live-afters)))
           (error 'gen-live-afters "Register found in live-after set: ~a~n~a~n" set live-afters)))

       (values `(define ,tag : ,ret-ty ,@(reverse instrs)) live-afters))]
    [_ (unsupported-form 'gen-live-afters def)]))

;; NOTE: Instructions should be reversed! E.g. first instruction in this
;; argument should be the last instruction in the block!
(define (gen-live-afters-instrs instrs live-afters)
  (match instrs
    [(list)
     ; We also need to return live set for the whole block here to be able to
     ; process if-statements properly. Suppose we have something like this:
     ;
     ;   (if (eq? y #t) ((assign a b)) ((assign c d)))  -- lives: ???
     ;   ...                                            -- lives: { b, d }
     ;
     ; The ??? part should be { b, d, a, c, y }, but we can only generate this
     ; if we know live-before set of the first instructions in branches.
     ;
     ; Then we can just do
     ;
     ;    (live-before true-branch) \union (live-before false-branch) \union {y}
     ;
     ; Head of live-afters is live-after of current instruction, so it's
     ; life-before of the first instruction in the block.
     (values '() (car live-afters) (cdr live-afters))]

    [(cons instr instrs)
     (let [(current-live-after (car live-afters))]
       (let-values [((instr current-live-before) (gen-live-afters-instr instr current-live-after))]
         (let-values [((instrs live-before live-afters)
                       (gen-live-afters-instrs instrs (cons current-live-before live-afters)))]
           (values (cons instr instrs) live-before live-afters))))]))

(define (gen-live-afters-instr instr lives)
  ; (printf "gen-live-afters-instr ~a ~a~n" lives instr)
  (match instr
    [`(,(or 'addq 'subq 'cmpq 'xorq 'andq) ,arg1 ,arg2)
     (values instr (add-live lives arg1 arg2))]

    [`(pushq ,arg1)
     (values instr (add-live lives arg1))]

    [`(popq ,arg1)
     (values instr (remove-live lives arg1))]

    [`(lahf)
     (values instr (remove-live lives `(reg rax)))]

    [`(,(or 'sete 'setl) (byte-reg al))
     (values instr (remove-live lives `(reg rax)))]

    [`(movzbq (byte-reg al) ,arg2)
     (values instr (remove-live lives arg2))]

    [`(,(or 'movq 'leaq) ,arg1 ,arg2)
     (values instr (add-live (remove-live lives arg2) arg1))]

    [`(negq ,arg1)
     (values instr (add-live lives arg1))]

    [`(callq ,n ,arg1)
     ; callq reads argument registers! Here's a bug that happens when we're not
     ; careful about this:
     ;
     ;   (movq (reg rdi) (var arg0))
     ;   ...
     ;   (movq (var arg0) (reg rdi))
     ;   (callq fn)
     ;
     ; Now we coalesce (var arg0) and (reg rdi). Previously arg0 was live just
     ; before the function call, but now it's dead, because it's now %rdi and
     ; %rdi is dead, which means we can freely use %rdi in ... part, which is
     ; wrong.
     ;
     ; Note that while adding all argument registers would solve the problem,
     ; it has another problem, namely, it can make more register live than
     ; necessary. In our example, we only need to make sure %rdi is live, other
     ; arg registers could be used freely. If they need to live across the
     ; function call, interference graph will have an edge and so the register
     ; allocator will take care of that.
     ;
     ; So instead here we annotate callq instructions with number of arguments
     ; the function is expecting, and use that to add only the actual argument
     ; registers to the live set.
     ;
     ; TODO: We need to handle stack arguments once we start doing stack
     ; location coalescing.
     (values instr (apply add-live lives
                          (cons arg1 (take arg-regs (min n (length arg-regs))))))]

    [`(retq)
     ; FIXME: This is only true if return type is not void. Otherwise it's OK
     ; to return garbage.
     (values instr (add-live lives `(reg rax)))]

    [(or `(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
         ; It's important that we handle this pattern here. We sometimes
         ; (redundantly) generate live-afters in some later passes for things
         ; like saving caller-save registers etc.
         `(if (eq? ,arg1 ,arg2) ,pgm-t ,_ ,pgm-f ,_))
     (let-values [((pgm-t-reversed lives-before-t lives-t)
                   (gen-live-afters-instrs (reverse pgm-t) (list lives)))

                  ((pgm-f-reversed lives-before-f lives-f)
                   (gen-live-afters-instrs (reverse pgm-f) (list lives)))]
       ; (printf "lives-before-t: ~s~n" lives-before-t)
       ; (printf "lives-after-t: ~s~n" lives-before-f)
       ; (assert lives-before-t set-equal?)
       ; (assert lives-before-f set-equal?)
       (let* [(lives-if-wo-cond (set-union lives-before-t lives-before-f))
              (if-lives (add-live lives-if-wo-cond arg1 arg2))]
         (values `(if (eq? ,arg1 ,arg2) ,(reverse pgm-t-reversed) ,lives-t
                                        ,(reverse pgm-f-reversed) ,lives-f)
                 if-lives)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [_ (unsupported-form 'gen-live-afters-instr instr)]))

(define (add-live lives . args)
  (foldl (lambda (arg lives)
           (match arg
             [`(var ,_) (set-add lives arg)]
             [`(offset (,(or 'var 'reg) ,_) ,_) (set-add lives (cadr arg))]
             [(or `(int ,_) `(stack ,_) `(global-value ,_))
              lives]
             [`(toplevel-fn ,_) lives]
             [`(reg ,reg) (set-add lives arg)]
             [`(mem-loc ,_) lives]
             [_ (unsupported-form 'add-live arg)]))
         lives args))

(define (remove-live lives . args)
  (foldl (lambda (arg lives)
           (match arg
             [`(var ,_) (set-remove lives arg)]

             [`(offset (,(or 'var 'reg) ,_) ,_)
              ; TODO: Maybe we should do this in the call site. The idea is
              ; that the arguments used for relative addressing are not killed
              ; even if they're in the destination position, as we actually use
              ; the argument for reading.
              (add-live lives arg)]

             [(or `(int ,_) `(stack ,_) `(global-value ,_))
              lives]
             [`(reg ,_) (set-remove lives arg)]
             [`(mem-loc ,_) lives]
             [_ (unsupported-form 'remove-live arg)]))
         lives args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NOTE [Live-afters after the last instruction]
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
; Callee-save registers need to be alive after the last instruction, otherwise
; we may end up skipping some save/restore instructions thinking that
; callee-save regs are dead the whole time. E.g. something like this:
;
;   (movq (reg r12) (var save-callee-save-2))
;   ...
;   (movq (var save-callee-save-1) (reg rbx)) -- lives: {save-callee-save-2}
;   (movq (var save-callee-save-2) (reg r12)) -- lives: {}
;   (retq) -- lives: {}
;
; Now we can coalesce save-callee-save-2 and r12, and get this:
;
;   ...
;   (movq (var save-callee-save-1) (reg rbx)) -- lives: {}
;   (retq) -- lives: {}
;
; Now r12 is basically free in the function. That means we don't need to save
; it anymore, which is wrong. Instead, if we make callee-saves live after the
; last instruction, we get this: (not all callee-saves shown)
;
;   (movq (reg r12) (var save-callee-save-2))
;   ...
;   (movq (var save-callee-save-1) (reg rbx)) -- lives: {save-callee-save-2}
;   (movq (var save-callee-save-2) (reg r12)) -- lives: {rbx}
;   (retq) -- lives: {r12, rbx}
;
; Now we again coalesce and it gives us:
;
;   ...
;   (movq (var save-callee-save-1) (reg rbx)) -- lives: {r12}
;   (retq) -- lives: {r12, rbx}
;
; (NOTE: These are live-afters)
;
; As an example how this helps, suppose we had a function call in before
; restoration:
;
;   ...
;   (callq foo)
;   ...
;   (movq (var save-callee-save-1) (reg rbx)) -- lives: {r12}
;   (retq) -- lives: {r12, rbx}
;
; Since caller-save registers interfere with lives when we have a callq, r12
; will now interfere with caller-save registers and so the register allocator
; will take care of saving r12.
