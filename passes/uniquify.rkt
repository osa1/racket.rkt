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

    [`(,(or '+ 'eq? 'vector-ref) ,e1 ,e2)
     (list (car e0) (uniquify-expr rns e1) (uniquify-expr rns e2))]

    [`(if ,e1 ,e2 ,e3)
     (list 'if (uniquify-expr rns e1) (uniquify-expr rns e2) (uniquify-expr rns e3))]

    [(? symbol?)
     (hash-ref rns e0)]

    [`(let ([,var ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (hash-set rns var fresh)])
       `(let ([,fresh ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [`(vector . ,elems)
     `(vector ,@(map (lambda (elem) (uniquify-expr rns elem)) elems))]

    [unsupported
     (unsupported-form 'uniquify-expr unsupported)]))
