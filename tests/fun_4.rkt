(define (id [i : Integer]) : Integer
  i)

(define (add [i1 : Integer]
             [i2 : Integer]) : Integer
  (+ (id i1) i2))

(add (add (id (- 42)) (id 42)) 42)
