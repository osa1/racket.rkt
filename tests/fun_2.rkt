(define (id [i : Integer]) : Integer
  i)

(define (add [i1 : Integer]
             [i2 : Integer]) : Integer
  (+ (id i1) i2))

(add 20 (id 22))
