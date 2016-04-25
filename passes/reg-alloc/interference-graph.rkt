#lang racket

(require "../../graph.rkt")
(require "../utils.rkt")

(require (only-in "../instr-sel.rkt" collect-vars-instrs))

(provide mk-interference-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interference graphs

; INVARIANT: Every variable in the program has a node, even if it doesn't have
; any edges.

(define (mk-interference-graph def live-sets)
  (match def
    [`(define ,_ : ,_ . ,instrs)
     (let [(graph (mk-graph))
           (all-vars (collect-vars-instrs instrs))]
       ; Create a node for every variable
       (for ([var all-vars])
         (add-node graph `(var ,var)))
       ; Built the actual graph
       (build-int-graph-instrs instrs live-sets graph)
       graph)]
    [_ (unsupported-form 'mk-interference-graph def)]))

(define (add-int graph arg1 arg2)

  (define (int-ok? arg)
    (match arg
      [`(reg ,_) arg]
      [`(var ,_) arg]
      [`(offset ,arg ,_) (int-ok? arg)]
      [_ #f]))

  (define int-val-arg1 (int-ok? arg1))
  (define int-val-arg2 (int-ok? arg2))

  (when (and int-val-arg1 int-val-arg2)
    (add-edge graph int-val-arg1 int-val-arg2)))

(define (build-int-graph-instrs instrs live-sets graph)
  (map (lambda (instr lives) (build-int-graph instr (set->list lives) graph)) instrs live-sets))

(define (build-int-graph instr lives graph)
  (match instr
    [`(,(or 'addq 'subq 'imulq 'xorq 'andq) ,s ,d)
     (for ([live lives])
       (unless (equal? live d)
         (add-int graph d live)))]

    [`(cmpq ,_ ,_) (void)]

    [`(,(or 'pushq 'popq 'negq) ,d)
     (for ([live lives])
       (unless (equal? live d)
         (add-int graph d live)))]

    [`(,(or 'sete 'setl) (byte-reg al))
     (for ([live lives])
       (unless (equal? live `(reg rax))
         (add-int graph `(reg rax) live)))]

    [`(movzbq (byte-reg al) ,d)
     (for ([live lives])
       (unless (or (equal? live `(reg rax)) (equal? live d))
         (add-int graph d live)))]

    [`(,(or 'movq 'leaq) ,s ,d)
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-int graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Handling movs with relative addressing

    [(or `(movq ,s (offset ,d ,_))
         `(movq (offset ,s ,_) ,d))
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-int graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [`(retq) (void)]

    [`(lahf)
     (for ([live lives])
       (unless (equal? live `(reg rax))
         (add-int graph `(reg rax) live)))]

    [`(callq ,_ ,s)
     ; Variables need to stay alive across a function call interfere with the
     ; caller-save registers.
     (for ([live lives])
       (unless (equal? live s)
         (for ([save (cons '(reg rax) caller-save-regs)])
           (add-int graph save live))))]

    [`(if (eq? ,_ ,_) ,pgm-t ,t-lives ,pgm-f ,f-lives)
     (build-int-graph-instrs pgm-t t-lives graph)
     (build-int-graph-instrs pgm-f f-lives graph)]

    [`(if (eq? ,_ ,_) ,_ ,_)
     (error 'build-int-graph "if doesn't have live-after annotations on branches!~n~s~n" instr)]

    [_ (unsupported-form 'build-int-graph instr)]))
