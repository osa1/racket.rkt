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

      [`(if ,_ ,pgm-t ,pgm-f)
       (mk-move-relation-instrs graph int-graph pgm-t)
       (mk-move-relation-instrs graph int-graph pgm-f)]

      [_ #f]))

  (define (mk-edge graph arg1 arg2)
    (when (and (can-relate? arg1) (can-relate? arg2))
      ; but do they interfere?
      (let* [(arg1-adjs (neighbors int-graph arg1))
             (interfere (set-member? arg1-adjs arg2))]
        (unless interfere
          (add-edge graph arg1 arg2)))))

  (match instr
    [`(movq ,s ,d) (mk-edge graph s d)]
    [_ (void)]))
