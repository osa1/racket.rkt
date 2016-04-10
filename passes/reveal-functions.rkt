#lang racket

(require (only-in "typecheck.rkt" mk-toplevel-ty-env is-fun-ty?))
(require "utils.rkt")

(provide reveal-functions)

(define (reveal-functions pgm)
  (match pgm
    [`(program . ,defs)
     (let ([toplevel-ty-env (mk-toplevel-ty-env (filter toplevel-def? defs))])
       `(program ,@(map (reveal-funs-def toplevel-ty-env) defs)))]
    [_ (unsupported-form 'reveal-functions pgm)]))

(define (reveal-funs-def toplevel-ty-env)
  (lambda (def)
    (match def
      [`(define ,tag : ,ret-ty ,body)
       `(define ,tag : ,ret-ty ,((reveal-funs-expr toplevel-ty-env) body))]
      [`(define-closure-wrapper . ,_) def]
      [_ (unsupported-form 'reveal-funs-def def)])))

(define (reveal-funs-expr toplevel-ty-env)
  (define (iter e0)
    (match (cdr e0)
      [(or (? fixnum?) (? boolean?) `(read))
       e0]

      [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
       `(,(car e0) . (,(cadr e0) ,(iter e1)))]

      [`(,(or 'inject 'project) ,e1 ,ty)
       `(,(car e0) . (,(cadr e0) ,(iter e1) ,ty))]

      [`(,(or '+ 'eq? '< '<= '> '>= 'vector-ref-dynamic) ,e1 ,e2)
       `(,(car e0) . (,(cadr e0) ,(iter e1) ,(iter e2)))]

      [(? symbol?)
       (match (hash-ref toplevel-ty-env (cdr e0) 'nop)
         ['nop e0]
         [_ `(,(car e0) . (toplevel-fn ,(cdr e0)))])]

      [`(toplevel-closure ,_) e0]

      [`(let ([,var ,e1]) ,body)
       `(,(car e0) . (let ([,var ,(iter e1)]) ,(iter body)))]

      [`(if ,e1 ,e2 ,e3)
       `(,(car e0) . (if ,(iter e1) ,(iter e2) ,(iter e3)))]

      [`(vector-ref ,e1 ,idx)
       `(,(car e0) . (vector-ref ,(iter e1) ,idx))]

      [`(vector-set! ,vec ,idx ,e)
       `(,(car e0) . (vector-set! ,(iter vec) ,idx ,(iter e)))]

      [`(vector-set!-dynamic ,vec ,idx ,e)
       `(,(car e0) . (vector-set!-dynamic ,(iter vec) ,(iter idx) ,(iter e)))]

      [`(vector . ,elems)
       `(,(car e0) . (vector ,@(map iter elems)))]

      [`(,f . ,args)
       `(,(car e0) . (app ,(iter f) ,@(map iter args)))]

      [_ (unsupported-form 'reveal-funs-expr (cdr e0))]))

  iter)
