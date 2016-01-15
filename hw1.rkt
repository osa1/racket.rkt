#lang racket

(require racket/fixnum)
(require "utilities.rkt")
(require "interp.rkt")

; exp ::= int | (read) | (- exp) | (+ exp exp)
;       | var | (let ([var exp]) exp)
;
; R1  ::= (program exp)

; arg  ::= int | var
; exp  ::= arg | (read) | (- arg) | (+ arg arg)
; stms ::= (assign var exp) | (return arg)
; C0   ::= (program (var*) stmt+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify

(define (uniquify pgm)
  ; (printf "pgm: ~s~n" pgm)
  (match pgm
    [`(program ,e) `(program ,(uniquify-expr '() e))]
    [_ (error 'uniquify "Expected a (program ...) form, found: ~s~n" pgm)]))

(define (uniquify-expr rns e0)
  ; (printf "rns: ~s~n" rns)

  (match e0

    [(or (? fixnum?) `(read))
     e0]

    [`(- ,e1)
     `(- ,(uniquify-expr rns e1))]

    [`(+ ,e1 ,e2)
     `(+ ,(uniquify-expr rns e1) ,(uniquify-expr rns e2))]

    [(? symbol?)
     (car (lookup e0 rns))]

    [`(let ([,var ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (cons `(,var ,fresh) rns)])
       `(let ([,fresh ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [unsupported
     (error 'uniquify-expr "unsupported form: ~s~n" unsupported)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flatten

(define (flatten pgm)
  (match pgm
    [`(program ,e)
     (let-values ([(_ pgm e) (flatten-expr '() '() e)])
       ; (printf "collect-binds result: ~s~n" (collect-binds pgm))
       `(program ,(collect-binds pgm) ,(reverse (cons `(return ,e) pgm))))]

    [_ (error 'flatten "Expected a (program ...) form, found ~s~n" pgm)]))

(define (collect-binds pgm)
  ; (printf "collect-binds: ~s~n" pgm)
  (match pgm
    [(list) '()]
    [(cons `(assign ,x ,_) t)
     (cons x (collect-binds t))]
    [_ (error 'collect-binds "unsupported form: ~s~n" pgm)]))

(define (arg? e) (or (fixnum? e) (symbol? e)))

; flatten-expr : [(var, var)] -> [stmt] -> expr -> arg
(define (flatten-expr binds pgm expr)
  (match expr

    [(? fixnum?)
     (values binds pgm expr)]

    [`(read)
     (let [(fresh (gensym "tmp"))]
       (values binds (cons `(assign ,fresh (read)) pgm) fresh))]

    [`(- ,e1)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym "tmp"))]
         (values binds (cons `(assign ,fresh (- ,e1)) pgm) fresh)))]

    [`(+ ,e1 ,e2)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let-values ([(binds pgm e2) (flatten-expr binds pgm e2)])
         (let [(fresh (gensym "tmp"))]
           (values binds (cons `(assign ,fresh (+ ,e1 ,e2)) pgm) fresh))))]

    [(? symbol?)
     (values binds pgm (car (lookup expr binds)))]

    [`(let ([,var ,e1]) ,body)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym "tmp"))]
         (let-values ([(binds pgm body)
                       (flatten-expr (cons `(,var ,fresh) binds)
                                     (cons `(assign ,fresh ,e1) pgm)
                                     body)])
           (values binds pgm body))))]

    [_ (error 'flatten-expr "unsupported form: ~s~n" expr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(interp-tests "uniquify"
              `(("uniquify" ,uniquify ,interp-scheme)
                ("flatten" ,flatten ,interp-C))
              interp-scheme
              "uniquify"
              (range 1 6))

(interp-tests "flatten"
              `(("flatten" ,flatten ,interp-C))
              interp-C
              "flatten"
              (range 1 4))
