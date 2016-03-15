(let ([fun-vec (vector (lambda: ([i : Integer]) : Integer (+ i i)))])
  ((vector-ref fun-vec 0) 21))
