(let ([fun-vec (vector (lambda: ([i : Integer]) : Integer (+ i i))
                       (lambda: ([i : Integer]) : Integer i))])
  ((vector-ref fun-vec 0) ((vector-ref fun-vec 1) 21)))
