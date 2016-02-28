(define (add9 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]
              [i4 : Integer]
              [i5 : Integer]
              [i6 : Integer]) : Integer
  (+ (id i1)
     (id (+ (id i2)
            (id (+ (id i3)
                   (id (+ (id i4)
                          (id (+ i5 (id i6)))))))))))

(define (id [i : Integer]) : Integer
  i)

(add9 1 2 3 4 5 6)
