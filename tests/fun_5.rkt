(define (id [i : Integer]) : Integer
  i)

(define (add2 [i1 : Integer]
              [i2 : Integer]) : Integer
  (+ (id i1) (id i2)))

(define (add3 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]) : Integer
  (+ (id i1) (add2 i2 i3)))

(define (add4 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]
              [i4 : Integer]) : Integer
  (+ (id i1) (add3 i2 i3 i4)))

(define (add5 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]
              [i4 : Integer]
              [i5 : Integer]) : Integer
  (+ (id i1) (add4 i2 i3 i4 i5)))

(add5 1 2 3 4 32)
