#lang racket

(require "utils.rkt")

(provide compile-r7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-r7 pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       (let ([toplevels (mk-toplevel-env defs)])
         `(program ,@(map (compile-def toplevels) defs)
                   (project ,(compile-expr toplevels expr) Integer))))]

    [_ (unsupported-form 'compile-f7 pgm)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extract-toplevel-name def)
  (match def
    [`(define (,name . ,_) ,_) name]
    [_ (unsupported-form 'extract-toplevel-name def)]))

(define (extract-toplevel-arity def)
  (match def
    [`(define (,_ . ,args) ,_) (length args)]
    [_ (unsupported-form 'extract-toplevel-arity def)]))

(define (mk-toplevel-env defs)
  (make-immutable-hash
    (map (lambda (def)
           (cons (extract-toplevel-name def) (extract-toplevel-arity def)))
         defs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-def ts)
  (lambda (def)
    (match def
      [`(define (,name . ,args) ,body)
       `(define (,name ,@(map (lambda (arg) `(,arg : Any)) args)) : Any
          ,(compile-expr ts body))]

      [_ (unsupported-form 'compile-def def)])))

(define (compile-expr ts expr)
  (match expr
    [(? fixnum?)
     (mk-integer expr)]

    [(? boolean?)
     (mk-boolean expr)]

    [(? symbol?)
     (let ([toplvl (hash-ref ts expr #f)])
       (if toplvl
         `(inject ,expr (,@(replicate 'Any toplvl) -> Any))
         expr))]

    [`(read)
     (mk-integer expr)]

    [`(- ,e1)
     (mk-integer `(- ,(get-integer (compile-expr ts e1))))]

    [`(+ ,e1 ,e2)
     (mk-integer `(+ ,(get-integer (compile-expr ts e1))
                     ,(get-integer (compile-expr ts e2))))]

    [`(let ([,var ,e1]) ,body)
     `(let ([,var ,(compile-expr ts e1)]) ,(compile-expr ts body))]

    [`(and ,e1 ,e2)
     (mk-boolean `(and ,(get-boolean (compile-expr ts e1))
                       ,(get-boolean (compile-expr ts e2))))]

    [`(not ,e1)
     (mk-boolean `(not ,(get-boolean (compile-expr ts e1))))]

    [`(,(or '> '>= '< '<=) ,e1 ,e2)
     (mk-boolean `(,(car expr) ,(get-integer (compile-expr ts e1))
                               ,(get-integer (compile-expr ts e2))))]

    [`(,(or 'and 'or) ,e1 ,e2)
     (mk-boolean `(,(car expr) ,(get-boolean (compile-expr ts e1))
                               ,(get-boolean (compile-expr ts e2))))]

    [`(if ,cond ,pgm-t ,pgm-f)
     `(if ,(get-boolean (compile-expr ts cond))
        ,(compile-expr ts pgm-t)
        ,(compile-expr ts pgm-f))]

    [`(void)
     ; TODO: Why do we have that in the front-end?
     `(inject ,expr Void)]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Vector ops

    ; vector-ref-dynamic and vector-set!-dynamic are like vector-ref and
    ; vector-set!, but can get dynamic index arguments.

    [`(vector . ,exprs)
     (mk-vector `(vector ,@(map (lambda (e) (compile-expr ts e)) exprs)))]

    [`(vector-ref ,e1 ,e2)
     `(vector-ref-dynamic ,(get-vector (compile-expr ts e1))
                          ,(get-integer (compile-expr ts e2)))]

    [`(vector-set! ,e1 ,e2 ,e3)
     `(vector-set!-dynamic
        ,(get-vector (compile-expr ts e1))
        ,(get-integer (compile-expr ts e2))
        ,(compile-expr ts e3))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Function and applications

    [`(lambda ,args ,body)
     (mk-function `(lambda: ,(map (lambda (arg) `(,arg : Any)) args) : Any
                            ,(compile-expr ts body))
                  (length args))]

    [`(,fn . ,args)
     `(,(get-function (compile-expr ts fn) (length args))
       ,@(map (lambda (e) (compile-expr ts e)) args))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [_ (unsupported-form 'compile-expr expr)]))

(define (mk-integer expr) `(inject ,expr Integer))
(define (mk-boolean expr) `(inject ,expr Boolean))
(define (mk-vector expr)  `(inject ,expr (Vectorof Any)))
(define (mk-function expr arity) `(inject ,expr (,@(replicate 'Any arity) -> Any)))

(define (get-integer expr) `(project ,expr Integer))
(define (get-boolean expr) `(project ,expr Boolean))
(define (get-vector expr)  `(project ,expr (Vectorof Any)))
(define (get-function expr arity) `(project ,expr (,@(replicate 'Any arity) -> Any)))
