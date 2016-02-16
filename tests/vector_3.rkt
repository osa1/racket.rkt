(vector-ref
  (let ([t (vector 0 7)])
    (let ([blah (vector-set! t 0 42)])
      t))
  0)
