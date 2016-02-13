#lang racket

(require "utils.rkt")

(provide typecheck typecheck-ignore)

;; This is used for ignoring type-checking step. The problem with type-checking
;; is that it's only defined in front-end language. When we want to run
;; compiler-tests on some intermediate language etc. we have to either implement
;; a type checker for all the intermediate languages, or skip the type-checking.
(define (typecheck-ignore _) #t)

(define (typecheck pgm)
  (match pgm
    [`(program ,e) (typecheck-iter e (hash))]
    [_ (unsupported-form 'typecheck pgm)]))

(define (typecheck-iter expr env)
  (match expr
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Bool]
    [(? symbol?) (hash-ref env expr)]

    [`(- ,e1)
     (if (eq? (typecheck-iter e1 env) 'Integer) 'Integer #f)]

    [`(+ ,e1 ,e2)
     (if (eq? (typecheck-iter e1 env) 'Integer)
       (if (eq? (typecheck-iter e2 env) 'Integer)
         'Integer
         #f)
       #f)]

    [`(and ,e1 ,e2)
     (if (eq? (typecheck-iter e1 env) 'Bool)
       (if (eq? (typecheck-iter e2 env) 'Bool)
         'Bool
         #f)
       #f)]

    [`(not ,e1)
     (if (eq? (typecheck-iter e1 env) 'Bool) 'Bool #f)]

    [`(eq? ,e1 ,e2)
     (let [(e1-ty (typecheck-iter e1 env))
           (e2-ty (typecheck-iter e2 env))]
       (if (and (not (eq? e1-ty #f)) (eq? e1-ty e2-ty))
         'Bool
         #f))]

    [`(if ,e1 ,e2 ,e3)
     (if (eq? (typecheck-iter e1 env) 'Bool)
       (let [(e2-ty (typecheck-iter e2 env))
             (e3-ty (typecheck-iter e3 env))]
         (if (and (not (eq? e2-ty #f)) (eq? e2-ty e3-ty))
           e2-ty
           #f))
       #f)]

    [`(let ([,var ,e1]) ,body)
     (let [(e1-ty (typecheck-iter e1 env))]
       (typecheck-iter body (hash-set env var e1-ty)))]

    [`(read) 'Integer]

    [`(vector . ,elems)
     (let [(elem-types
             (filter id (map (lambda (elem) (typecheck-iter elem env)) elems)))]
       (if (eq? (length elem-types) (length elems))
         (cons 'vector elem-types)
         #f))]

    [`(vector-ref ,vec ,idx)
     (if (fixnum? idx)
       (match (typecheck-iter vec env)
         [`(vector . ,elems)
          (if (< idx (length elems))
            (list-ref elems idx)
            #f)]
         [_ #f])
       #f)]

    [_
     ; Uhh.. test-typecheck is ignoring exceptions.
     (printf "typecheck: unsupported form: ~s~n" expr)
     (unsupported-form 'typecheck-iter expr)]))
