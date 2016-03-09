(define (add6 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]
              [i4 : Integer]
              [i5 : Integer]
              [i6 : Integer]) : Integer
  (+ (id i1)
     (id (+ (id i2)
            (id (+ (id i3)
                   (id (+ (id i4)
                          (id (+ (id i5)
                                 (id i6)))))))))))

(define (id [i : Integer]) : Integer
  i)

(+ (add6 1 2 3 4 5 6) 21)
