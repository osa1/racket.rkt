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

    [`(,(or '+ 'eq? 'vector-ref) ,e1 ,e2)
     (list (car e0) (desugar-expr e1) (desugar-expr e2))]

    [`(and ,e1 ,e2)
     (let [(e1-ds (desugar-expr e1))
           (e2-ds (desugar-expr e2))]
       `(if (eq? ,e1-ds #t) Bool ,e2-ds #f))]

    [`(if ,e1 ,ret-ty ,e2 ,e3)
     (list 'if (desugar-expr e1) ret-ty (desugar-expr e2) (desugar-expr e3))]

    [`(let ([,var ,var-ty ,e1]) ,e2)
     `(let ([,var ,var-ty ,(desugar-expr e1)]) ,(desugar-expr e2))]

    [`(read) e0]

    [`(vector ,elem-tys . ,elems)
     `(vector ,elem-tys ,@(map desugar-expr elems))]

    [_ (unsupported-form 'desugar-expr e0)]))
