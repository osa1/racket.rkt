#lang racket

(require "utils.rkt")
(require (only-in "../public/utilities.rkt" caller-save))

(require (only-in "reg-alloc.rkt" gen-live-afters))

(provide save-regs)

;; This pass saves caller-save registers when necessary (e.g. when the registers
;; are live after a function call)

; FIXME: Properly refactor this.
(define caller-save-regs (list->set (set-map caller-save (lambda (reg) `(reg ,reg)))))

(define (save-regs pgm)
  (match pgm
    [(list-rest 'program meta instrs)
     ;; FIXME: Bad! We generate live vars again!
     (let-values [((pgm lives) (gen-live-afters pgm))]
       ; We should use the program with annotated live vars on if branches
       ; here.
       `(program ,meta ,@(save-regs-instrs lives (cddr pgm))))]
    [_ (unsupported-form 'save-regs pgm)]))

(define (save-regs-instrs lives instrs)
  (append-map save-regs-instr lives instrs))

(define (save-regs-instr lives instr)
  (match instr
    [`(callq ,_)
     (let* [(should-save (set->list (set-intersect caller-save-regs lives)))
            (align-stack (not (even? (length should-save))))]
       (append (map (lambda (reg) `(pushq (reg ,(cadr reg)))) should-save)
               (if align-stack `((subq (int 8) (reg rsp))) `())
               (list instr)
               (if align-stack `((addq (int 8) (reg rsp))) `())
               (map (lambda (reg) `(popq (reg ,(cadr reg)))) (reverse should-save))))]

    [`(if ,_ ,_ ,_)
     (error 'save-regs-instr "If block doesn't have live var annotations!~n~s~n" instr)]

    [`(if ,cond ,pgm-t ,lives-t ,pgm-f ,lives-f)
     ; We remove annotations here, probably OK
     `((if ,cond ,(save-regs-instrs lives-t pgm-t)
                 ,(save-regs-instrs lives-f pgm-f)))]

    [_ (list instr)]))
