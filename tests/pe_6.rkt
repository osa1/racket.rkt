(define (map-vec [f : (Integer -> Integer)] [v : (Vector Integer Integer)]) : Void
  (let ([garbage1 (vector-set! v 0 (f (vector-ref v 0)))])
    (let ([garbage2 (vector-set! v 1 (f (vector-ref v 1)))])
      (void))))

(let ([v (vector 1 2)])
  (let ([garbage (map-vec (lambda: ([x : Integer]) : Integer (+ x 1)) v)])
    (vector-ref v 1)))
