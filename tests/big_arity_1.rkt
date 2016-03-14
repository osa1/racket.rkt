(define (big-arity [i1 : Integer]
                   [i2 : Integer]
                   [i3 : Integer]
                   [i4 : Integer]
                   [i5 : Integer]
                   [i6 : Integer]
                   [i7 : Integer]
                   [i8 : Integer]
                   [i9 : Integer]
                   [i10 : Integer]
                   [i11 : Integer]
                   [i12 : Integer]) : Integer
  (+ i1 (+ i2 (+ i3 (+ i4 (+ i5 (+ i6 (+ i7 (+ i8 (+ i9 (+ i10 (+ i11 i12))))))))))))

(big-arity 1 2 3 4 5 6 7 8 9 10 11 (- 24))
