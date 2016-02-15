#lang racket

(require "utils.rkt")

(provide typecheck typechecker op-ret-ty typecheck-ignore)

;; This is used for ignoring type-checking step. The problem with type-checking
;; is that it's only defined in front-end language. When we want to run
;; compiler-tests on some intermediate language etc. we have to either implement
;; a type checker for all the intermediate languages, or skip the type-checking.
(define (typecheck-ignore _) #t)

;; This ignores the annotated program. Useful for compiler-tests.
(define (typechecker pgm)
  (with-handlers ([exn:fail? (lambda (e)
                               ; (printf "Type checking failed: ~s~n" e)
                               #f)])
    (begin (typecheck pgm) #t)))

;; Currently we only annotate vector, let and if expressions with types.
;; TODO: This shouldn't be necessary. Apparently the typechecker is run as a
;; pass by the compiler-tests.
(define (typecheck pgm)
  (match pgm
    [`(program ,e)
     (let-values ([(e _) (typecheck-iter e (hash))])
       `(program ,e))]
    [_ (unsupported-form 'typecheck pgm)]))

(define (ty-err expr expected found)
  (error 'ty-err "Type error in ~s: Expected ~s, found ~s~n" expr expected found))

(define (wrap-values f . args)
  (let-values ([(ret1 ret2) (apply f args)])
    `(,ret1 . ,ret2)))

(define (assert-ty expr expected found)
  (if (equal? expected found)
    (void)
    (ty-err expr expected found)))

(define (typecheck-iter expr env)
  (match expr
    [(? fixnum?) (values expr 'Integer)]
    [(? boolean?) (values expr 'Bool)]
    [(? symbol?) (values expr (hash-ref env expr))]

    [`(- ,e1)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)])
       (assert-ty e1 'Integer e1-ty)
       (values `(- ,e1) 'Integer))]

    [`(+ ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)]
                  [(e2 e2-ty) (typecheck-iter e2 env)])
       (assert-ty e1 'Integer e1-ty)
       (assert-ty e2 'Integer e2-ty)
       (values `(+ ,e1 ,e2) 'Integer))]

    [`(and ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)]
                  [(e2 e2-ty) (typecheck-iter e2 env)])
       (assert-ty e1 'Bool e1-ty)
       (assert-ty e2 'Bool e2-ty)
       (values `(and ,e1 ,e2) 'Bool))]

    [`(not ,e1)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)])
       (assert-ty e1 'Bool e1-ty)
       (values `(not ,e1) 'Bool))]

    [`(eq? ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)]
                  [(e2 e2-ty) (typecheck-iter e2 env)])
       (assert-ty e2 e1-ty e2-ty)
       (values `(eq? ,e1 ,e2) 'Bool))]

    [`(if ,e1 ,e2 ,e3)
     (let-values ([(e1 e1-ty) (typecheck-iter e1 env)]
                  [(e2 e2-ty) (typecheck-iter e2 env)]
                  [(e3 e3-ty) (typecheck-iter e3 env)])
       (assert-ty e1 'Bool e1-ty)
       (assert-ty e2 e2-ty e3-ty)
       (values `(if ,e1 ,e2-ty ,e2 ,e3) e3-ty))]

    [`(let ([,var ,e1]) ,body)
     (let*-values ([(e1 e1-ty) (typecheck-iter e1 env)]
                   [(body body-ty) (typecheck-iter body (hash-set env var e1-ty))])
       ; note how we're annotating the expression with binder's type
       (values `(let ([,var ,e1-ty ,e1]) ,body) body-ty))]

    [`(read) (values expr 'Integer)]

    [`(vector . ,elems)
     (let-values ([(elems elem-types)
                   (unzip (map (lambda (elem)
                                 (let-values ([(elem elem-ty) (typecheck-iter elem env)])
                                   `(,elem . ,elem-ty)))
                               elems))])
       ; note how we're annotating the vector with element types
       (values `(vector ,elem-types ,@elems) `(vector ,@elem-types)))]

    [`(vector-ref ,vec ,idx)
     (unless (fixnum? idx)
       (error 'typecheck "vector-ref invalid index in ~s: ~s~n" vec idx))
     (let-values ([(vec vec-ty) (typecheck-iter vec env)])
       (match vec-ty
         [`(vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let [(ret-ty (list-ref elems idx))]
            (values `(vector-ref ,ret-ty ,vec ,idx) ret-ty))]
         [_ (ty-err vec 'Vector vec-ty)]))]

    [_ (unsupported-form 'typecheck-iter expr)]))

(define (op-ret-ty op)
  (match op
    [(or '+ '-) 'Integer]
    [(or 'and 'not 'eq?) 'Bool]
    [_ (unsupported-form 'op-ret-ty op)]))
