(let ([v (vector 0)])
  (let ([blah (vector-set! v (read) 42)])
    (vector-ref v (read))))
