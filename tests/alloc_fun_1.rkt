(define (alloc-vec-2 [i : Integer]) : (Vector Integer Integer)
  (vector i i))

(vector-ref (alloc-vec-2 42) 0)
