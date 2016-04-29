#lang racket

(require (only-in "utils.rkt" unsupported-form))

(provide print-pgm print-pgm-typeless)

(define (print-pgm msg)
  (lambda (pgm)
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    (display msg)
    (newline)
    (pretty-print pgm)
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    pgm))

(define (print-pgm-typeless msg)
  (lambda (pgm)
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    (display msg)
    (newline)
    (pretty-print (remove-tys-pgm pgm))
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    pgm))

(define (remove-tys-pgm pgm)
  (match pgm
    [`(program . ,defs) (map remove-tys-def defs)]
    [_ (unsupported-form 'remove-tys-pgm pgm)]))

(define (remove-tys-def def)
  (match def
    [`(define ,tag : ,ret-ty ,body)
     `(define ,tag : ,ret-ty ,(remove-tys-expr body))]
    [_ (unsupported-form 'remove-tys-def def)]))

(define (remove-tys-expr expr)
  (match (cdr expr)
    [(or (? fixnum?) (? boolean?) (? symbol?) `(read) `(void))
     (cdr expr)]

    [`(lambda: ,args : ,ret-ty ,body)
     `(lambda: ,args : ,ret-ty ,(remove-tys-expr body))]

    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure? 'project-boolean) ,e1)
     `(,(cadr expr) ,(remove-tys-expr e1))]

    [`(,(or 'project 'inject) ,e1 ,ty)
     `(,(cadr expr) ,(remove-tys-expr e1) ,ty)]

    [`(,(or '+ '* 'eq? 'eq?-dynamic '< '<= '> '>= 'vector-ref-dynamic) ,e1 ,e2)
     `(,(cadr expr) ,(remove-tys-expr e1) ,(remove-tys-expr e2))]

    [`(if ,e1 ,e2 ,e3)
     `(if ,(remove-tys-expr e1) ,(remove-tys-expr e2) ,(remove-tys-expr e3))]

    [`(let ([,var ,e1]) ,body)
     `(let ([,var ,(remove-tys-expr e1)]) ,(remove-tys-expr body))]

    [`(vector-ref ,e1 ,idx)
     `(vector-ref ,(remove-tys-expr e1) ,idx)]

    [`(vector-set! ,vec ,idx ,e)
     `(vector-set! ,(remove-tys-expr vec) ,idx ,(remove-tys-expr e))]

    [`(vector-set!-dynamic ,vec ,idx ,e)
     `(vector-set!-dynamic ,(remove-tys-expr vec)
                           ,(remove-tys-expr idx)
                           ,(remove-tys-expr e))]

    [`(vector . ,elems)
     `(vector ,@(map remove-tys-expr elems))]

    [`(app-noalloc ,f . ,args)
     `(app-noalloc ,(remove-tys-expr f) ,@(map remove-tys-expr args))]

    [`(,f . ,args)
     `(,(remove-tys-expr f) ,@(map remove-tys-expr args))]

    [_ (unsupported-form 'remove-tys-expr expr)]))
