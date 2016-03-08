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
                     ; accumulator for live-after sets, live-after set for the
                     ; last (first in the list, as we reverse the instructions)
                     ; instruction is an empty set so we add it here
                     (list (set `(reg rax)))))]
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
    [`(,(or 'addq 'subq 'cmpq 'xorq) ,arg1 ,arg2)
     (values instr (add-live lives arg1 arg2))]

    [`(pushq ,arg1)
     (values instr (add-live lives arg1))]

    [`(popq ,arg1)
     (values instr (remove-live lives arg1))]

    ; Don't do anything -- argument has to be a register.
    [`(,(or 'sete 'setl) ,_)
     (values instr lives)]

    [`(movzbq (byte-reg al) ,arg2)
     (values instr (remove-live lives arg2))]

    [`(,(or 'movq 'leaq) ,arg1 ,arg2)
     (values instr (add-live (remove-live lives arg2) arg1))]

    [`(negq ,arg1)
     (values instr (add-live lives arg1))]

    [`(callq ,arg1)
     (values instr (add-live lives arg1))]

    [`(retq)
     (values instr lives)]

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
             [`(offset (var ,_) ,_) (set-add lives (cadr arg))]
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
             [`(offset (var ,_) ,_) lives]
             [(or `(int ,_) `(stack ,_) `(global-value ,_))
              lives]
             [`(reg ,_) (set-remove lives arg)]
             [`(mem-loc ,_) lives]
             [_ (unsupported-form 'remove-live arg)]))
         lives args))
