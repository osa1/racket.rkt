#lang racket

(require racket/fixnum)
(require "utilities.rkt")

; exp ::= int | (read) | (- exp) | (+ exp exp)
;       | var | (let ([var exp]) exp)
;
; R1  ::= (program exp)

(define (uniquify pgm)
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
     (lookup e0 rns)]

    [`(let ([,var ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (cons `(,var ,fresh) rns)])
       `(let ([,fresh ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [unsupported
     (error 'uniquify-expr "unsupported form: ~s~n" unsupported)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(uniquify '(program (+ 1 2)))
(uniquify '(program (+ 1 (- 2))))
(uniquify-expr '((a x)) '(+ 1 a))
(uniquify '(program (let [(x 1)] (+ x x))))
(uniquify '(program (let [(x 1)] (+ x x))))
(uniquify '(program (let [(x (let [(x 20)]
                               (let [(y 30)] (+ x y))))]
                      (+ x x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
