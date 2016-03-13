(define (alloc-vec-2 [i : Integer]) : (Vector Integer Integer)
  (vector i i))

(let ([vec1 (alloc-vec-2 10)])
  (let ([vec2 (alloc-vec-2 11)])
    (+ (+ (vector-ref vec1 0) (vector-ref vec2 1)) 21)))
