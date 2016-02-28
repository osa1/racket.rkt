(define (add9 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]
              [i4 : Integer]
              [i5 : Integer]
              [i6 : Integer]
              [i7 : Integer]
              [i8 : Integer]
              [i9 : Integer]) : Integer
  (+ (id i1)
     (id (+ (id i2)
            (id (+ (id i3)
                   (id (+ (id i4)
                          (id (+ (id i5)
                                 (id (+ (id i6)
                                        (id (+ (id i7)
                                               (id (+ (id i8) (id i9)))))))))))))))))

(define (id [i : Integer]) : Integer
  i)

(add9 1 2 3 4 5 6 7 8 6)
