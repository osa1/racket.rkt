#lang racket

(require "utils.rkt")

(provide uniquify)

(define (uniquify pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map uniquify-def defs))]
    [_ (unsupported-form 'uniquify pgm)]))

(define (uniquify-def def)
  (match def
    [`(define (,fname . ,args) : ,ret-ty ,expr)
     (let* ([rns (map (lambda (arg) (cons (car arg) (fresh "arg"))) args)]
            [args (map (lambda (old rn) `(,(cdr rn) : ,(caddr old))) args rns)]
            [expr (uniquify-expr (make-immutable-hash rns) expr)])
       `(define (,fname ,@args) : ,ret-ty ,expr))]
    [`(define ,name : ,ret-ty ,expr)
     `(define ,name : ,ret-ty ,(uniquify-expr (make-immutable-hash) expr))]
    [_ (unsupported-form 'uniquify-def def)]))

(define (uniquify-expr rns e0)
  (match (cdr e0)
    [(or (? fixnum?) (? boolean?) `(read))
     e0]

    [`(lambda: ,args : ,ret-ty ,body)
     ; FIXME: These definitions are copied from above
     (let* ([rn-maps (map (lambda (arg) (cons (car arg) (fresh "lam-arg"))) args)]
            [args    (map (lambda (old rn) `(,(cdr rn) : ,(caddr old))) args rn-maps)]
            [rns     (foldl (lambda (rn h) (hash-set h (car rn) (cdr rn))) rns rn-maps)])
       `(,(car e0) . (lambda: ,args : ,ret-ty ,(uniquify-expr rns body))))]

    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     `(,(car e0) . (,(cadr e0) ,(uniquify-expr rns e1)))]

    [`(,(or 'inject 'project) ,e1 ,ty)
     `(,(car e0) . (,(cadr e0) ,(uniquify-expr rns e1) ,ty))]

    [`(,(or '+ 'eq?) ,e1 ,e2)
     `(,(car e0) . (,(cadr e0) ,(uniquify-expr rns e1) ,(uniquify-expr rns e2)))]

    [`(if ,e1 ,e2 ,e3)
     `(,(car e0) . (if ,(uniquify-expr rns e1) ,(uniquify-expr rns e2) ,(uniquify-expr rns e3)))]

    [(? symbol?)
     ; Not in map = toplevel, so just return.
     `(,(car e0) . ,(hash-ref rns (cdr e0) (cdr e0)))]

    [`(let ([,var ,e1]) ,body)
     (let* ([fresh (fresh "x")]
            [rns1 (hash-set rns var fresh)])
       `(,(car e0) .
         (let ([,fresh ,(uniquify-expr rns e1)])
           ,(uniquify-expr rns1 body))))]

    [`(vector-ref ,e1 ,idx)
     `(,(car e0) . (vector-ref ,(uniquify-expr rns e1) ,idx))]

    [`(vector-set! ,vec ,idx ,e)
     `(,(car e0) . (vector-set! ,(uniquify-expr rns vec) ,idx ,(uniquify-expr rns e)))]

    [`(vector . ,elems)
     `(,(car e0) . (vector ,@(map (lambda (elem) (uniquify-expr rns elem)) elems)))]

    [`(,f . ,args)
     `(,(car e0) . (,(uniquify-expr rns f) ,@(map (lambda (expr) (uniquify-expr rns expr)) args)))]

    [unsupported
     (unsupported-form 'uniquify-expr unsupported)]))
