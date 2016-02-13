#lang racket

(require "utils.rkt")

(provide uniquify)

(define (uniquify pgm)
  (match pgm
    [`(program ,e) `(program ,(uniquify-expr (hash) e))]
    [_ (unsupported-form 'uniquify pgm)]))

(define (uniquify-expr rns e0)
  (match e0
    [(or (? fixnum?) (? boolean?) `(read))
     e0]

    [`(,(or '- 'not) ,e1)
     (list (car e0) (uniquify-expr rns e1))]

    [`(,(or '+ 'eq?) ,e1 ,e2)
     (list (car e0) (uniquify-expr rns e1) (uniquify-expr rns e2))]

    [`(if ,e1 ,ret-ty ,e2 ,e3)
     (list 'if (uniquify-expr rns e1) ret-ty (uniquify-expr rns e2) (uniquify-expr rns e3))]

    [(? symbol?)
     (hash-ref rns e0)]

    [`(let ([,var ,var-ty ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (hash-set rns var fresh)])
       `(let ([,fresh ,var-ty ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [`(vector-ref ,ret-ty ,e1 ,e2)
     (list 'vector-ref ret-ty (uniquify-expr rns e1) (uniquify-expr rns e2))]

    [`(vector ,elem-tys . ,elems)
     `(vector ,elem-tys ,@(map (lambda (elem) (uniquify-expr rns elem)) elems))]

    [unsupported
     (unsupported-form 'uniquify-expr unsupported)]))
