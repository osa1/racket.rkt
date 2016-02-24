#lang racket

(require "utils.rkt")

(provide uniquify)

(define (uniquify pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       `(program ,@(map (lift-def (lambda (expr) (uniquify-expr (hash) expr))) defs)
                        ,(uniquify-expr (hash) expr)))]
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
     ; Not in map = toplevel, so just return.
     (hash-ref rns e0 e0)]

    [`(let ([,var ,var-ty ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (hash-set rns var fresh)])
       `(let ([,fresh ,var-ty ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [`(vector-ref ,ret-ty ,e1 ,e2)
     (list 'vector-ref ret-ty (uniquify-expr rns e1) (uniquify-expr rns e2))]

    [`(vector-set! ,vec ,idx ,e)
     (list 'vector-set!  (uniquify-expr rns vec) idx (uniquify-expr rns e))]

    [`(vector ,elem-tys . ,elems)
     `(vector ,elem-tys ,@(map (lambda (elem) (uniquify-expr rns elem)) elems))]

    [`(,f . ,args)
     `(,(uniquify-expr rns f) ,@(map (lambda (expr) (uniquify-expr rns expr)) args))]

    [unsupported
     (unsupported-form 'uniquify-expr unsupported)]))
