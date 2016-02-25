#lang racket

(require "utils.rkt")

(provide typecheck typechecker typecheck-ignore mk-toplevel-ty-env is-fun-ty?)

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
     (let-values ([(defines expr) (split-last things)])
       (let* ([initial-env (mk-toplevel-ty-env defines)]
              [defines (map (lambda (def) (typecheck-toplevel initial-env def)) defines)])
         (let ([expr (typecheck-expr '() expr initial-env)])
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
    [(? boolean?) `(Bool . ,expr)]
    [(? symbol?) `(,(hash-ref env expr) . ,expr)]

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
       (assert-ty context e1 'Bool (car e1))
       (assert-ty context e2 'Bool (car e2))
       `(Bool . (and ,e1 ,e2)))]

    [`(not ,e1)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)])
       (assert-ty context e1 'Bool (car e1))
       `(Bool . (not ,e1)))]

    [`(eq? ,e1 ,e2)
     (let([e1 (typecheck-expr (cons expr context) e1 env)]
          [e2 (typecheck-expr (cons expr context) e2 env)])
       (assert-ty context e2 (car e1) (car e2))
       `(Bool . (eq? ,e1 ,e2)))]

    [`(if ,e1 ,e2 ,e3)
     (let ([e1 (typecheck-expr (cons expr context) e1 env)]
           [e2 (typecheck-expr (cons expr context) e2 env)]
           [e3 (typecheck-expr (cons expr context) e3 env)])
       (assert-ty context e1 'Bool (car e1))
       (assert-ty context e3 (car e2) (car e3))
       `(,(car e2) . (if ,e1 ,e2 ,e3)))]

    [`(let ([,var ,e1]) ,body)
     (let* ([e1 (typecheck-expr (cons expr context) e1 env)]
            [body (typecheck-expr (cons expr context) body (hash-set env var (car e1)))])
       `(,(car body) . (let ([,var ,e1]) ,body)))]

    [`(read) `(Integer . ,expr)]

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
         [_ (ty-err context expr 'Vector (car vec))]))]

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
            `(void . (vector-set! ,vec ,idx ,e)))]
         [_ (ty-err context expr 'Vector (car vec))]))]

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
