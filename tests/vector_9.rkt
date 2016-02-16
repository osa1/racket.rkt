(let ([t (vector 22 #t (vector 20))])
  (if (vector-ref t 1)
    (+ (vector-ref (vector-ref t 2) 0) (vector-ref t 0))
    123))
