(define (ret1 [i1 : Integer] [i2 : Integer]) : (Integer -> Integer)
  (lambda: ([i2 : Integer]) : Integer
           (+ i1 i2)))

((ret1 20 13) 22)
