#lang racket

(require "../utils.rkt")
(require "../../graph.rkt")

(provide mk-mov-rel-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move relation graph

(define (mk-mov-rel-graph def int-graph)
  (match def
    [`(define ,_ : ,_ ,_ . ,instrs)
     (let [(graph (mk-graph))]
       (mk-move-relation-instrs graph int-graph instrs)
       graph)]
    [_ (unsupported-form 'mk-mov-rel-graph def)]))

(define (mk-move-relation-instrs graph int-graph instrs)
  (for ([instr instrs]) (mk-move-rel-iter graph int-graph instr)))

(define (mk-move-rel-iter graph int-graph instr)
  (define (can-relate? arg)
    (match arg
      [`(,(or 'reg 'stk 'var) ,_) #t]
      [_ #f]))

  (define (mk-edge graph arg1 arg2)
    (when (and (can-relate? arg1) (can-relate? arg2))
      ; but do they interfere?
      (unless (has-edge? int-graph arg1 arg2)
        (add-edge graph arg1 arg2))))

  (match instr
    [`(movq ,s ,d) (mk-edge graph s d)]

    [`(if ,_ ,pgm-t ,pgm-f)
     (mk-move-rel-iter graph int-graph pgm-t)
     (mk-move-rel-iter graph int-graph pgm-f)]

    [_ (void)]))
