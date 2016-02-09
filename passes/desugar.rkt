#lang racket

(require "utils.rkt")

(provide desugar)

;; Currently only syntactic sugar is `and`.

(define (desugar pgm)
  (match pgm
    [`(program ,e) `(program ,(desugar-expr e))]
    [_ (unsupported-form 'desugar pgm)]))

(define (desugar-expr e0)
  (match e0
    [(or (? fixnum?) (? boolean?) (? symbol?)) e0]

    [`(,(or '- 'not) ,e1)
     (list (car e0) (desugar-expr e1))]

    [`(,(or '+ 'eq?) ,e1 ,e2)
     (list (car e0) (desugar-expr e1) (desugar-expr e2))]

    [`(and ,e1 ,e2)
     (let [(e1-ds (desugar-expr e1))
           (e2-ds (desugar-expr e2))]
       `(if (eq? ,e1-ds #t) ,e2-ds #f))]

    [`(if ,e1 ,e2 ,e3)
     (list 'if (desugar-expr e1) (desugar-expr e2) (desugar-expr e3))]

    [`(let ([,var ,e1]) ,e2)
     `(let ([,var ,(desugar-expr e1)]) ,(desugar-expr e2))]

    [`(read) e0]

    [_ (unsupported-form 'desugar-expr e0)]))
