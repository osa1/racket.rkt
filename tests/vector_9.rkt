(let ([t (vector 40 #t (vector 42))])
  (if (vector-ref t 1)
    (+ (vector-ref (vector-ref t 2) 0) (vector-ref t 0))
    123))
