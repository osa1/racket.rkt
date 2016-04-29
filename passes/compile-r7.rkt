#lang racket

(require "utils.rkt")

(provide compile-r7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE [Special case for (project any Boolean)]
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
; Racket treats every variable other than #f as #t. In the dynamically typed
; language, when we have
;
;   (if x t f)
;
; We generate
;
;   (if (project x Boolean) t f) (omitting wrappers around t, f, the whole expr)
;
; Now, we want this to work as expected in runtime, without giving away type
; safety in the typed language with explicit Any values. E.g. this should still
; fail in the typed language (in runtime, of course):
;
;   (if (project (inject 1 Integer) Boolean) t f)
;
; So we can't change runtime function project(). Instead, we need a new project
; function that takes casre of extra coercions to coerce basically everything
; to a boolean. The form is called `project-boolean` and the corresponding
; runtime function is `project_boolean()` defined in runtime.c.
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

    [`(void) (mk-void expr)]

    [`(- ,e1)
     (mk-integer `(- ,(get-integer (compile-expr ts e1))))]

    [`(,(or '+ '*) ,e1 ,e2)
     (mk-integer `(,(car expr) ,(get-integer (compile-expr ts e1))
                               ,(get-integer (compile-expr ts e2))))]

    [`(let ([,var ,e1]) ,body)
     `(let ([,var ,(compile-expr ts e1)]) ,(compile-expr ts body))]

    [`(and ,e1 ,e2)
     (mk-boolean `(and ,(get-boolean (compile-expr ts e1))
                       ,(get-boolean (compile-expr ts e2))))]

    [`(not ,e1)
     (mk-boolean `(not ,(get-boolean (compile-expr ts e1))))]

    [`(,(or '> '>= '< '<= 'eq?) ,e1 ,e2)
     (mk-boolean `(,(car expr) ,(compile-expr ts e1)
                               ,(compile-expr ts e2)))]

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
    ; Type predicates

    [`(,(or 'integer? 'boolean? 'vector? 'procedure?) ,e1)
     (mk-boolean `(,(car expr) ,(compile-expr ts e1)))]

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
(define (mk-void expr)    `(inject ,expr Void))

(define (get-integer expr) `(project ,expr Integer))

; Note the special form `project-boolean` here. We can't use `project`. See
; NOTE [Special case for (project any Boolean)] above.
(define (get-boolean expr) `(project-boolean ,expr))

(define (get-vector expr)  `(project ,expr (Vectorof Any)))
(define (get-function expr arity) `(project ,expr (,@(replicate 'Any arity) -> Any)))
