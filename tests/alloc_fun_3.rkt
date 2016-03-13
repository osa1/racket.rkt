(define (alloc-vec-2 [i : Integer]) : (Vector Integer Integer)
  (vector i i))

(define (alloc-vec-3 [i : Integer]) : (Vector Integer Integer Integer)
  (vector i i i))

(let ([vec1 (alloc-vec-2 10)])
  (let ([vec2 (alloc-vec-2 11)])
    (let ([vec3 (alloc-vec-3 5)])
      (+ (+ (+ (vector-ref vec1 0) (vector-ref vec2 1)) (vector-ref vec3 2)) 16))))
