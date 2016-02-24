#lang racket

(require "utils.rkt")

(provide typecheck typechecker op-ret-ty typecheck-ignore mk-toplevel-ty-env)

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
    [`(program . ,things)
     (let-values ([(defines expr) (split-last things)])
       (let* ([initial-env (mk-toplevel-ty-env defines)]
              [defines (map (lambda (def) (typecheck-toplevel initial-env def)) defines)])
         (let-values ([(expr _) (typecheck-expr '() expr initial-env)])
           `(program ,@defines ,expr))))]
    [_ (unsupported-form 'typecheck pgm)]))

(define typechecker typecheck)

(define (mk-toplevel-ty-env defs)
  (make-immutable-hash
    (map (lambda (def)
           (cons (extract-toplevel-name def) (extract-toplevel-ty def)))
         defs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting stuff from stuff

(define (extract-toplevel-name def)
  (match def
    [`(define (,name . ,_) : ,_ ,_)
     name]
    [_ (unsupported-form 'extract-toplevel-name def)]))

(define (extract-toplevel-ty def)
  (match def
    [`(define (,_ . ,args) : ,ret-ty ,_)
     `(,@(map extract-arg-ty args) -> ,ret-ty)]
    [_ (unsupported-form 'extract-toplevel-ty def)]))

(define (extract-arg-ty arg)
  (match arg
    [`(,_ : ,arg-ty) arg-ty]
    [_ (unsupported-form 'extract-arg-ty arg)]))

(define (split-fun-ty ty)
  (let-values ([(args ret-lst)
                (splitf-at ty (lambda (ty) (not (eq? ty '->))))])
    (unless (eq? 2 (length ret-lst))
      (error 'split-fun-ty "Unexpected return type: ~a~n" ret-lst))
    (values args (cadr ret-lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertions

(define (ty-err context expr expected found)
  (printf "Type error in ~s: Expected ~s, found ~s~n" expr expected found)
  (for ([expr context])
    (printf "in expression: ~n~s~n~n" expr))
  (error 'ty-err ""))

(define (assert-ty context expr expected found)
  (if (equal? expected found)
    (void)
    (ty-err context expr expected found)))

(define (assert-arg-tys context expr expected-tys arg-tys)
  (for ([p (map cons expected-tys arg-tys)])
    (assert-ty context expr (car p) (cdr p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking top-level things

(define (typecheck-toplevel env def)
  (match def
    [`(define (,fname . ,args) : ,ret-ty ,body)
     (let* ([env (foldl (lambda (arg env)
                          (hash-set env (car arg) (caddr arg)))
                        env args)]
            [context (list def)])
       (let-values ([(body body-ty) (typecheck-expr context body env)])
         (assert-ty context body ret-ty body-ty)
         `(define (,fname ,@args) : ,ret-ty ,body)))]

    [_ (unsupported-form 'typecheck-toplevel def)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking expressions

(define (typecheck-expr context expr env)
  (match expr
    [(? fixnum?) (values expr 'Integer)]
    [(? boolean?) (values expr 'Bool)]
    [(? symbol?) (values expr (hash-ref env expr))]

    [`(- ,e1)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Integer e1-ty)
       (values `(- ,e1) 'Integer))]

    [`(+ ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e1 'Integer e1-ty)
       (assert-ty context e2 'Integer e2-ty)
       (values `(+ ,e1 ,e2) 'Integer))]

    [`(and ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e1 'Bool e1-ty)
       (assert-ty context e2 'Bool e2-ty)
       (values `(and ,e1 ,e2) 'Bool))]

    [`(not ,e1)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Bool e1-ty)
       (values `(not ,e1) 'Bool))]

    [`(eq? ,e1 ,e2)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e2 e1-ty e2-ty)
       (values `(eq? ,e1 ,e2) 'Bool))]

    [`(if ,e1 ,e2 ,e3)
     (let-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)]
                  [(e2 e2-ty) (typecheck-expr (cons expr context) e2 env)]
                  [(e3 e3-ty) (typecheck-expr (cons expr context) e3 env)])
       (assert-ty context e1 'Bool e1-ty)
       (assert-ty context e2 e3-ty e2-ty)
       (values `(if ,e1 ,e2-ty ,e2 ,e3) e3-ty))]

    [`(let ([,var ,e1]) ,body)
     (let*-values ([(e1 e1-ty) (typecheck-expr (cons expr context) e1 env)]
                   [(body body-ty) (typecheck-expr (cons expr context) body (hash-set env var e1-ty))])
       ; note how we're annotating the expression with binder's type
       (values `(let ([,var ,e1-ty ,e1]) ,body) body-ty))]

    [`(read) (values expr 'Integer)]

    [`(vector . ,elems)
     (let-values ([(elems elem-types)
                   (unzip (map (lambda (elem)
                                 (let-values ([(elem elem-ty)
                                               (typecheck-expr (cons expr context) elem env)])
                                   `(,elem . ,elem-ty)))
                               elems))])
       ; note how we're annotating the vector with element types
       (values `(vector ,elem-types ,@elems) `(Vector ,@elem-types)))]

    [`(vector-ref ,vec ,idx)
     (unless (fixnum? idx)
       (error 'typecheck "vector-ref invalid index in ~s: ~s~n" vec idx))
     (let-values ([(vec vec-ty) (typecheck-expr (cons expr context) vec env)])
       (match vec-ty
         [`(Vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let [(ret-ty (list-ref elems idx))]
            (values `(vector-ref ,ret-ty ,vec ,idx) ret-ty))]
         [_ (ty-err context expr 'Vector vec-ty)]))]

    [`(vector-set! ,vec ,idx ,e)
     (unless (fixnum? idx)
       (error 'typecheck "vector-set! invalid index in ~s: ~s~n" vec idx))
     (let-values ([(vec vec-ty) (typecheck-expr (cons expr context) vec env)])
       (match vec-ty
         [`(Vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let ([vec-elem-ty (list-ref elems idx)])
            (let-values ([(e e-ty) (typecheck-expr (cons expr context) e env)])
              (assert-ty context e vec-elem-ty e-ty)
              (values `(vector-set! ,vec ,idx ,e) 'void)))]
         [_ (ty-err context expr 'Vector vec-ty)]))]

    [`(,fn . ,args)
     (match (hash-ref env fn 'nop)
       ['nop (error "fn not in scope: ~a~n" fn)]
       [ty
        (let ([context (cons expr context)])
          (let-values ([(expected-arg-tys ret-ty) (split-fun-ty ty)])
            (unless (eq? (length expected-arg-tys) (length args))
              (error 'typecheck-expr
                     "Function has arity ~a, but is applied to ~a arguments.~n~a~n~a~n"
                     (length expected-arg-tys) (length args) expr
                     ty))
            (let-values
              ([(args arg-tys)
                (map-unzip (lambda (arg) (typecheck-expr (cons arg context) arg env)) args)])
              (assert-arg-tys context expr expected-arg-tys arg-tys)
              (values `(,fn ,@args) ret-ty))))])]

    [_ (unsupported-form 'typecheck-expr expr)]))

(define (op-ret-ty op)
  (match op
    [(or '+ '-) 'Integer]
    [(or 'and 'not 'eq?) 'Bool]
    [_ (unsupported-form 'op-ret-ty op)]))
