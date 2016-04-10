#lang racket

(require "utils.rkt")

(provide compile-r7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-r7 pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       `(program ,@(map compile-def defs) ,(compile-expr expr)))]

    [_ (unsupported-form 'compile-f7 pgm)]))

(define (compile-def def)
  (match def
    [`(define (,name . ,args) ,body)
     `(define (,name ,@(map (lambda (arg) `(,arg : Any)) args)) : Any
        ,(compile-expr body))]

    [_ (unsupported-form 'compile-def def)]))

(define (compile-expr expr)
  (match expr
    [(? fixnum?)
     (mk-integer expr)]

    [(? boolean?)
     (mk-boolean expr)]

    [(? symbol?) expr]

    [`(read)
     (mk-integer expr)]

    [`(- ,e1)
     (mk-integer `(- ,(get-integer (compile-expr e1))))]

    [`(+ ,e1 ,e2)
     (mk-integer `(+ ,(get-integer (compile-expr e1))
                     ,(get-integer (compile-expr e2))))]

    [`(let ([,var ,e1]) ,body)
     `(let ([,var ,(compile-expr e1)]) ,(compile-expr body))]

    [`(and ,e1 ,e2)
     (mk-boolean `(and ,(get-boolean (compile-expr e1))
                       ,(get-boolean (compile-expr e2))))]

    [`(not ,e1)
     (mk-boolean `(not ,(get-boolean (compile-expr e1))))]

    [`(,(or '> '>= '< '<=) ,e1 ,e2)
     (mk-boolean `(,(car expr) ,(get-integer (compile-expr e1))
                               ,(get-integer (compile-expr e2))))]

    [`(,(or 'and 'or) ,e1 ,e2)
     (mk-boolean `(,(car expr) ,(get-boolean (compile-expr e1))
                               ,(get-boolean (compile-expr e2))))]

    [`(if ,cond ,pgm-t ,pgm-f)
     `(if ,(get-boolean (compile-expr cond))
        ,(compile-expr pgm-t)
        ,(compile-expr pgm-f))]

    [`(void)
     ; TODO: Why do we have that in the front-end?
     `(inject ,expr Void)]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Vector ops

    [`(vector . ,exprs)
     (mk-vector `(vector ,@(map compile-expr exprs)))]

    [`(vector-ref ,e1 ,e2)
     `(vector-ref-dynamic ,(get-vector (compile-expr e1)) ,(get-integer (compile-expr e2)))]

    [`(vector-set! ,e1 ,e2 ,e3)
     `(vector-set!-dynamic
        ,(get-vector (compile-expr e1))
        ,(get-integer (compile-expr e2))
        ,(compile-expr e2))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Function and applications

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [_ (unsupported-form 'compile-expr expr)]))

(define (mk-integer expr) `(inject ,expr Integer))
(define (mk-boolean expr) `(inject ,expr Boolean))
(define (mk-vector expr)  `(inject ,expr (Vectorof Any)))

(define (get-integer expr) `(project ,expr Integer))
(define (get-boolean expr) `(project ,expr Boolean))
(define (get-vector expr)  `(project ,expr (Vectorof Any)))
