#lang racket

(require "utils.rkt")

(provide typecheck typechecker typecheck-ignore
         mk-toplevel-ty-env is-fun-ty?
         extract-toplevel-name extract-toplevel-args
         extract-arg-name extract-arg-ty)

;; This is used for ignoring type-checking step. The problem with type-checking
;; is that it's only defined in front-end language. When we want to run
;; compiler-tests on some intermediate language etc. we have to either implement
;; a type checker for all the intermediate languages, or skip the type-checking.
(define (typecheck-ignore _) #t)

;; NOTE: Typecheck is just a compiler pass like any other, it transforms
;; programs! (adds type annotations) The syntax is that car is always a type,
;; cdr is the expression. (TODO: Maybe start using records/structs)
(define (typecheck pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       (let* ([initial-env (mk-toplevel-ty-env defs)]
              [defs (map (lambda (def) (typecheck-toplevel initial-env def)) defs)])
         (let ([expr (typecheck-expr '() expr initial-env)])
           `(program ,@defs ,expr))))]
    [_ (unsupported-form 'typecheck pgm)]))

(define typechecker typecheck)

(define (mk-toplevel-ty-env defs)
  (make-immutable-hash
    (append
      (map (lambda (def)
             (cons (extract-toplevel-name def) (extract-toplevel-ty def)))
           defs)
      rts-funs)))

(define rts-funs
  `((print-int . (Integer -> Void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting stuff from stuff

(define (extract-toplevel-name def)
  (match def
    [`(define (,name . ,_) : ,_ ,_)
     name]
    [`(define ,name : ,_ ,_)
     #:when (symbol? name)
     name]
    [_ (unsupported-form 'extract-toplevel-name def)]))

(define (extract-toplevel-args def)
  (match def
    [`(define (,_ . ,args) : ,_ . ,_) args]
    [`(define ,_ : ,_ . ,_) `()]
    [_ (unsupported-form 'extract-toplevel-args def)]))

(define (extract-toplevel-ty def)
  (match def
    [`(define (,_ . ,args) : ,ret-ty ,_)
     `(,@(map extract-arg-ty args) -> ,ret-ty)]
    [`(define ,(? symbol?) : ,ret-ty ,_)
     ret-ty]
    [_ (unsupported-form 'extract-toplevel-ty def)]))

(define (extract-arg-ty arg)
  (match arg
    [`(,_ : ,arg-ty) arg-ty]
    [_ (unsupported-form 'extract-arg-ty arg)]))

(define (extract-arg-name arg)
  (match arg
    [`(,arg-name : ,_) arg-name]
    [_ (unsupported-form 'extract-arg-ty arg)]))

(define (split-fun-ty ty)
  (let-values ([(args ret-lst)
                (splitf-at ty (lambda (ty) (not (eq? ty '->))))])
    (unless (eq? 2 (length ret-lst))
      (error 'split-fun-ty "Unexpected return type: ~a~n" ret-lst))
    (values args (cadr ret-lst))))

(define (is-fun-ty? ty)
  (and (list? ty) (member '-> ty)))

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
       (match-let ([(cons body-ty body) (typecheck-expr context body env)])
         (assert-ty context body ret-ty body-ty)
         `(define (,fname ,@args) : ,ret-ty ,(cons body-ty body))))]

    [_ (unsupported-form 'typecheck-toplevel def)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking expressions

(define (typecheck-expr context expr env)
  (match expr
    [(? fixnum?) `(Integer . ,expr)]
    [(? boolean?) `(Boolean . ,expr)]
    [(? symbol?) `(,(hash-ref env expr) . ,expr)]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Dynamic typing stuff

    [`(inject ,e1 (Vectorof Any))
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (match (car e1)
         [`(Vector Any ...)
          `(Any . (inject ,e1 (Vectorof Any)))]
         [ty (ty-err context expr 'Vector ty)]))]

    ; Inject a type to Any
    [`(inject ,e1 ,ty)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty (cons expr context) e1 ty (car e1))
       `(Any . (inject ,e1 ,ty)))]

    ; Project an Any to the given type - can fail at runtime
    [`(project ,e1 ,ty)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty (cons expr context) e1 'Any (car e1))
       `(,ty . (project ,e1 ,ty)))]

    ; See NOTE [Special case for (project any Boolean)] in compile-r7.rkt
    [`(project-boolean ,e1)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty (cons expr context) e1 'Any (car e1))
       `(Boolean . (project-boolean ,e1)))]

    [`(,(or 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Any (car e1))
       `(Boolean . (,(car expr) ,e1)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [`(lambda: ,args : ,ret-ty ,body)
     (let* ([env (foldl (lambda (arg env) (hash-set env (car arg) (caddr arg))) env args)]
            [body (typecheck-expr (cons expr context) body env)])
       ; (printf "type checking lambda\n")
       (assert-ty context expr ret-ty (car body))
       (define ret `(,@(map caddr args) -> ,ret-ty))
       ; (pretty-print ret)
       `(,ret . (lambda: ,args : ,ret-ty ,body)))]

    [`(- ,e1)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Integer (car e1))
       `(Integer . (- ,e1)))]

    [`(+ ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e1 'Integer (car e1))
       (assert-ty context e2 'Integer (car e2))
       `(Integer . (+ ,e1 ,e2)))]

    [`(and ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e1 'Boolean (car e1))
       (assert-ty context e2 'Boolean (car e2))
       `(Boolean . (and ,e1 ,e2)))]

    [`(,(or '< '<= '> '>=) ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e1 'Integer (car e1))
       (assert-ty context e2 'Integer (car e2))
       `(Boolean . (,(car expr) ,e1 ,e2)))]

    [`(eq? ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e2 (car e1) (car e2))

       ; FIXME: This is not the right place to do this.
       ; We have two kinds of eq? checks now, once is plain old eq? which
       ; compares for value/ptr equality. Other one is a RTS function that can
       ; handle vectors, dynamically typed objects. Depending on the type here
       ; we choose which one to use.

       (if (is-ptr-obj? (car e1))
         `(Boolean . (eq?-dynamic ,e1 ,e2))
         `(Boolean . (eq? ,e1 ,e2))))]

    [`(not ,e1)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Boolean (car e1))
       `(Boolean . (not ,e1)))]

    [`(eq? ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e2 (car e1) (car e2))
       `(Boolean . (eq? ,e1 ,e2)))]

    [`(if ,e1 ,e2 ,e3)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)]
           [e3 (typecheck-expr (cons expr context) e3 env)])
       (assert-ty context e1 'Boolean (car e1))
       (assert-ty context e3 (car e2) (car e3))
       `(,(car e2) . (if ,e1 ,e2 ,e3)))]

    [`(let ([,var ,e1]) ,body)
     (let* ([e1 (typecheck-expr (cons expr context) e1 env)]
            [body (typecheck-expr (cons expr context) body (hash-set env var (car e1)))])
       `(,(car body) . (let ([,var ,e1]) ,body)))]

    [`(read) `(Integer . ,expr)]

    [`(void) `(Void . ,expr)]

    [`(vector . ,elems)
     (let* ([elems (map (lambda (expr) (typecheck-expr (cons expr context) expr env)) elems)]
            [elem-tys (map car elems)])
       `((Vector ,@elem-tys) . (vector ,@elems)))]

    [`(vector-ref ,vec ,idx)
     (unless (fixnum? idx)
       (error 'typecheck "vector-ref invalid index in ~s: ~s~n" vec idx))
     (let ([vec (typecheck-expr (cons expr context) vec env)])
       (match (car vec)
         [`(Vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          `(,(list-ref elems idx) . (vector-ref ,vec ,idx))]
         [`(Vectorof Any)
          `(Any . (vector-ref ,vec ,idx))]
         [_ (ty-err context expr 'Vector (car vec))]))]

    ; Like vector-ref, but takes a dynamic Integer expression as index. Also,
    ; only works on Vectorof Any.
    [`(vector-ref-dynamic ,e1 ,e2)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e2 'Integer (car e2))
       (match (car e1)
         [`(Vectorof Any)
          `(Any . (vector-ref-dynamic ,e1 ,e2))]
         [_ (ty-err context expr '(Vectorof Any) (car e1))]))]

    [`(vector-set! ,vec ,idx ,e)
     (unless (fixnum? idx)
       (error 'typecheck "vector-set! invalid index in ~s: ~s~n" vec idx))
     (let ([vec (typecheck-expr (cons expr context) vec env)])
       (match (car vec)
         [`(Vector . ,elems)
          (unless (< idx (length elems))
            (error 'typecheck
                   "Invalid vector index: ~s vector size: ~s expression: ~s~n"
                   idx (length elems) expr))
          (let ([e (typecheck-expr (cons expr context) e env)])
            (assert-ty context (cdr e) (list-ref elems idx) (car e))
            `(Void . (vector-set! ,vec ,idx ,e)))]
         [_ (ty-err context expr 'Vector (car vec))]))]

    ; Like vector-set!, but takes a dynamic Integer expression as index. Also,
    ; only works on Vectorof Any.
    [`(vector-set!-dynamic ,vec ,idx ,e)
     (let ([vec (typecheck-expr (cons expr context) vec env)]
           [idx (typecheck-expr (cons expr context) idx env)]
           [e   (typecheck-expr (cons expr context) e   env)])
       (assert-ty context idx 'Integer (car idx))
       (assert-ty context e   'Any     (car e))
       (match (car vec)
         [`(Vectorof Any)
          `(Any . (vector-set!-dynamic ,vec ,idx ,e))]
         [_ (ty-err context expr '(Vectorof Any) (car vec))]))]

    [`(,fn . ,args)
     (let ([fn (typecheck-expr context fn env)])
       (match (car fn)
         [`(,expected-arg-tys ... -> ,ret-ty)
          (unless (eq? (length expected-arg-tys) (length args))
            (error 'typecheck-expr
                   "Function has arity ~a, but is applied to ~a arguments.~n~a~n~a~n"
                   (length expected-arg-tys) (length args) expr (car fn)))
          (let* ([args
                  (map (lambda (arg) (typecheck-expr (cons arg context) arg env)) args)]
                 [arg-tys (map car args)])
            (assert-arg-tys context expr expected-arg-tys arg-tys)
            `(,ret-ty . (,fn ,@args)))]
         [_ (ty-err context expr 'Function (car fn))]))]

    [_ (unsupported-form 'typecheck-expr expr)]))
