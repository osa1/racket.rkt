#lang racket

(require "../../graph.rkt")
(require "../utils.rkt")

(provide mk-interference-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interference graphs

(define (mk-interference-graph def live-sets)
  (match def
    [`(define ,_ : ,_ ,_ . ,instrs)
     (let [(graph (mk-graph))]
       (build-int-graph-instrs instrs live-sets graph)
       graph)]
    [_ (unsupported-form 'mk-interference-graph def)]))

(define (build-int-graph-instrs instrs live-sets graph)
  (map (lambda (instr lives) (build-int-graph instr (set->list lives) graph)) instrs live-sets))

(define (build-int-graph instr lives graph)
  (match instr
    [`(,(or 'addq 'subq 'xorq) ,s ,d)
     (for ([live lives])
       (unless (equal? live d)
         (add-edge graph d live)))]

    [`(cmpq ,_ ,_) (void)]

    [(or `(,(or 'sete 'setl 'pushq 'popq 'negq) ,d)
         `(movzbq (byte-reg al) ,d))
     (for ([live lives])
       (unless (equal? live d)
         (add-edge graph d live)))]

    [`(,(or 'movq 'leaq) ,s ,d)
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-edge graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Handling movs with relative addressing

    [(or `(movq ,s (offset ,d ,_))
         `(movq (offset ,s ,_) ,d))
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-edge graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [`(retq) (void)]

    [`(callq ,s)
     ;; TODO: Do we need to do something with the argument here?
     (for ([live lives])
       (for ([save (cons 'rax (set->list caller-save))])
         (unless (equal? live s)
           (add-edge graph `(reg ,save) live))))]

    [`(if (eq? ,_ ,_) ,pgm-t ,t-lives ,pgm-f ,f-lives)
     (build-int-graph-instrs pgm-t t-lives graph)
     (build-int-graph-instrs pgm-f f-lives graph)]

    [`(if (eq? ,_ ,_) ,_ ,_)
     (error 'build-int-graph "if doesn't have live-after annotations on branches!~n~s~n" instr)]

    [_ (unsupported-form 'build-int-graph instr)]))
