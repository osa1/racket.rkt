#lang racket

(require "utils.rkt")

(provide elim-dyns elim-dyn-expr)

(define (elim-dyns pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map elim-dyn-def defs))]

    [_ (unsupported-form 'uniquify pgm)]))

(define (elim-dyn-def def)
  (match def
    [`(define ,tag : ,ret-ty ,expr)
     `(define ,tag : ,ret-ty ,(elim-dyn-expr expr))]

    [_ (unsupported-form 'elim-dyn-def def)]))

(define (elim-dyn-expr e0)
  (match (cdr e0)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; These do the actual work

    [`(project ,e1 ,ty1)
     (define e1-elim (elim-dyn-expr e1))
     (match (cdr e1-elim)
       [`(inject ,_ ,ty2) #:when (equal? ty1 ty2)
        (caddr e1-elim)]
       [_
        `(,(car e0) . (project ,e1-elim ,ty1))])]

    [`(project-boolean ,e1)
     (define e1-elim (elim-dyn-expr e1))
     (match (cdr e1-elim)
       [`(inject ,e1 Boolean)
        e1]
       [_
        `(,(car e0) . (project-boolean ,e1-elim))])]

    [`(if ,e1 ,e2 ,e3)
     (define e1-elim (elim-dyn-expr e1))
     (define e2-elim (elim-dyn-expr e2))
     (define e3-elim (elim-dyn-expr e3))
     (match (cons (cdr e2-elim) (cdr e3-elim))
       [`((inject ,_ ,ty1) . (inject ,_ ,ty2)) #:when (equal? ty1 ty2)
        `(,(car e0) . (inject (,ty1 . (if ,e1-elim ,(caddr e2-elim) ,(caddr e3-elim))) ,ty1))]
       [_
        `(,(car e0) . (if ,e1-elim ,e2-elim ,e3-elim))])]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Walk the tree

    [(or (? fixnum?) (? boolean?) (? symbol?) `(read) `(void))
     e0]

    [`(lambda: ,args : ,ret-ty ,body)
     `(,(car e0) . (lambda: ,args : ,ret-ty ,(elim-dyn-expr body)))]

    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     `(,(car e0) . (,(cadr e0) ,(elim-dyn-expr e1)))]

    [`(,(or 'inject 'project) ,e1 ,ty)
     `(,(car e0) . (,(cadr e0) ,(elim-dyn-expr e1) ,ty))]

    [`(,(or '+ 'eq? 'eq?-dynamic '< '<= '> '>=) ,e1 ,e2)
     `(,(car e0) . (,(cadr e0) ,(elim-dyn-expr e1) ,(elim-dyn-expr e2)))]

    [`(let ([,var ,e1]) ,body)
       `(,(car e0) .
         (let ([,var ,(elim-dyn-expr e1)])
           ,(elim-dyn-expr body)))]

    [`(vector-ref ,e1 ,idx)
     `(,(car e0) . (vector-ref ,(elim-dyn-expr e1) ,idx))]

    [`(vector-ref-dynamic ,e1 ,idx)
     (define e1-elim  (elim-dyn-expr e1))
     (define idx-elim (elim-dyn-expr idx))
     (if (fixnum? (cdr idx-elim))
       `(,(car e0) . (vector-ref ,e1-elim ,(cdr idx-elim)))
       `(,(car e0) . (vector-ref-dynamic ,e1-elim ,idx-elim)))]

    [`(vector-set! ,vec ,idx ,e)
     `(,(car e0) . (vector-set! ,(elim-dyn-expr vec) ,idx ,(elim-dyn-expr e)))]

    [`(vector-set!-dynamic ,vec ,idx ,e)
     (define vec-elim (elim-dyn-expr vec))
     (define idx-elim (elim-dyn-expr idx))
     (define e-elim   (elim-dyn-expr e))
     (if (fixnum? (cdr idx-elim))
       `(,(car e0) . (vector-set! ,vec-elim ,(cdr idx-elim) ,e-elim))
       `(,(car e0) . (vector-set!-dynamic ,vec-elim ,idx-elim ,e-elim)))]

    [`(vector . ,elems)
     `(,(car e0) . (vector ,@(map (lambda (elem) (elim-dyn-expr elem)) elems)))]

    [`(app-noalloc ,f . ,args)
     `(,(car e0) . (app-noalloc ,(elim-dyn-expr f)
                                ,@(map (lambda (expr) (elim-dyn-expr expr)) args)))]

    [`(,f . ,args)
     `(,(car e0) . (,(elim-dyn-expr f) ,@(map (lambda (expr) (elim-dyn-expr expr)) args)))]

    [unsupported
     (unsupported-form 'elim-dyn-expr unsupported)]))
