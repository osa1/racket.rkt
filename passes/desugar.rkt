#lang racket

(require "utils.rkt")

(provide desugar)

;; Currently only syntactic sugar is `and`.

(define (desugar pgm)
  (pretty-print pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       `(program ,@(map (lift-def desugar-expr) defs) ,(desugar-expr expr)))]
    [_ (unsupported-form 'desugar pgm)]))

(define (desugar-expr e0)
  (match (cdr e0)
    [(or (? fixnum?) (? boolean?) (? symbol?)) e0]

    [`(lambda: ,args : ,ret-ty ,body)
     `(,(car e0) . (lambda: ,args : ,ret-ty ,(desugar-expr body)))]

    [`(,(or '- 'not) ,e1)
     `(,(car e0) . (,(cadr e0) ,(desugar-expr e1)))]

    [`(,(or '+ 'eq?) ,e1 ,e2)
     `(,(car e0) . (,(cadr e0) ,(desugar-expr e1) ,(desugar-expr e2)))]

    [`(and ,e1 ,e2)
     (let [(e1-ds (desugar-expr e1))
           (e2-ds (desugar-expr e2))]
       `((car e0) . (if (Boolean . (eq? ,e1-ds (Boolean . #t))) ,e2-ds (Boolean . #f))))]

    [`(if ,e1 ,e2 ,e3)
     `(,(car e0) . (if ,(desugar-expr e1) ,(desugar-expr e2) ,(desugar-expr e3)))]

    [`(let ([,var ,e1]) ,e2)
     `(,(car e0) . (let ([,var ,(desugar-expr e1)]) ,(desugar-expr e2)))]

    [`(read) e0]

    [`(vector-ref ,e1 ,e2)
     `(,(car e0) . (vector-ref ,(desugar-expr e1) ,e2))]

    [`(vector-set! ,vec ,idx ,e)
     `(,(car e0) . (vector-set! ,(desugar-expr vec) ,idx ,(desugar-expr e)))]

    [`(vector . ,elems)
     `(,(car e0) . (vector ,@(map desugar-expr elems)))]

    [`(,f . ,args)
     `(,(car e0) . (,(desugar-expr f) ,@(map desugar-expr args)))]

    [_ (unsupported-form 'desugar-expr e0)]))
