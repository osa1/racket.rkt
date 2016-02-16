#lang racket

(require "utils.rkt")

(provide typecheck typechecker op-ret-ty typecheck-ignore)

;; This is used for ignoring type-checking step. The problem with type-checking
;; is that it's only defined in front-end language. When we want to run
;; compiler-tests on some intermediate language etc. we have to either implement
;; a type checker for all the intermediate languages, or skip the type-checking.
(define (typecheck-ignore _) #t)

;; Currently we only annotate vector, vector-ref, let and if expressions with
;; types.
;; NOTE: Typecheck is just a compiler pass like any other, it transforms
;; programs! (adds type annotations)
(define (typecheck pgm)
  (match pgm
    [`(program ,e)
     (let-values ([(e _) (typecheck-iter '() e (hash))])
       `(program ,e))]
    [_ (unsupported-form 'typecheck pgm)]))

(define typechecker typecheck)

(define (ty-err context expr expected found)
  (printf "Type error in ~s: Expected ~s, found ~s~n" expr expected found)
  (for ([expr context])
    (printf "in expression: ~n~s~n~n" expr))
  (error 'ty-err ""))

(define (wrap-values f . args)
  (let-values ([(ret1 ret2) (apply f args)])
    `(,ret1 . ,ret2)))

(define (assert-ty context expr expected found)
  (if (equal? expected found)
    (void)
    (ty-err context expr expected found)))

(define (typecheck-iter context expr env)
  (match expr
    [(? fixnum?) (values expr 'Integer)]
    [(? boolean?) (values expr 'Bool)]
    [(? symbol?) (values expr (hash-ref env expr))]

    [`(- ,e1)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)])
       (assert-ty context e1 'Integer e1-ty)
       (values `(- ,e1) 'Integer))]

    [`(+ ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-iter (cons expr context) e2 env)])
       (assert-ty context e1 'Integer e1-ty)
       (assert-ty context e2 'Integer e2-ty)
       (values `(+ ,e1 ,e2) 'Integer))]

    [`(and ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-iter (cons expr context) e2 env)])
       (assert-ty context e1 'Bool e1-ty)
       (assert-ty context e2 'Bool e2-ty)
       (values `(and ,e1 ,e2) 'Bool))]

    [`(not ,e1)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)])
       (assert-ty context e1 'Bool e1-ty)
       (values `(not ,e1) 'Bool))]

    [`(eq? ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-iter (cons expr context) e2 env)])
       (assert-ty context e2 e1-ty e2-ty)
       (values `(eq? ,e1 ,e2) 'Bool))]

    [`(if ,e1 ,e2 ,e3)
     (let-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-iter (cons expr context) e2 env)]
                  [(e3 e3-ty) (typecheck-iter (cons expr context) e3 env)])
       (assert-ty context e1 'Bool e1-ty)
       (assert-ty context e2 e3-ty e2-ty)
       (values `(if ,e1 ,e2-ty ,e2 ,e3) e3-ty))]

    [`(let ([,var ,e1]) ,body)
     (let*-values ([(e1 e1-ty) (typecheck-iter (cons expr context) e1 env)]
                   [(body body-ty) (typecheck-iter (cons expr context) body (hash-set env var e1-ty))])
       ; note how we're annotating the expression with binder's type
       (values `(let ([,var ,e1-ty ,e1]) ,body) body-ty))]

    [`(read) (values expr 'Integer)]

    [`(vector . ,elems)
     (let-values ([(elems elem-types)
                   (unzip (map (lambda (elem)
                                 (let-values ([(elem elem-ty)
                                               (typecheck-iter (cons expr context) elem env)])
                                   `(,elem . ,elem-ty)))
                               elems))])
       ; note how we're annotating the vector with element types
       (values `(vector ,elem-types ,@elems) `(vector ,@elem-types)))]

    [`(vector-ref ,vec ,idx)
     (unless (fixnum? idx)
       (error 'typecheck "vector-ref invalid index in ~s: ~s~n" vec idx))
     (let-values ([(vec vec-ty) (typecheck-iter (cons expr context) vec env)])
       (match vec-ty
         [`(vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let [(ret-ty (list-ref elems idx))]
            (values `(vector-ref ,ret-ty ,vec ,idx) ret-ty))]
         [_ (ty-err vec 'Vector vec-ty)]))]

    [`(vector-set! ,vec ,idx ,e)
     (unless (fixnum? idx)
       (error 'typecheck "vector-set! invalid index in ~s: ~s~n" vec idx))
     (let-values ([(vec vec-ty) (typecheck-iter (cons expr context) vec env)])
       (match vec-ty
         [`(vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let ([vec-elem-ty (list-ref elems idx)])
            (let-values ([(e e-ty) (typecheck-iter (cons expr context) e env)])
              (assert-ty context e vec-elem-ty e-ty)
              (values `(vector-set! ,vec ,idx ,e) 'void)))]
         [_ (ty-err vec 'Vector vec-ty)]))]

    [_ (unsupported-form 'typecheck-iter expr)]))

(define (op-ret-ty op)
  (match op
    [(or '+ '-) 'Integer]
    [(or 'and 'not 'eq?) 'Bool]
    [_ (unsupported-form 'op-ret-ty op)]))
