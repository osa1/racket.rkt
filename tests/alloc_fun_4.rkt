(define (choose-alloc [i : Boolean])
      : (Integer -> (Vector Integer Integer Integer Integer Integer Integer))
  (if i alloc-vec-2 alloc-vec-1))

(define (alloc-vec-1 [i : Integer])
      : (Vector Integer Integer Integer Integer Integer Integer)
  (vector 0 0 0 0 0 0))

(define (alloc-vec-2 [i : Integer])
      : (Vector Integer Integer Integer Integer Integer Integer)
  (vector i i i i i i))

(define (sum [v : (Vector Integer Integer Integer Integer Integer Integer)]) : Integer
  (+ (vector-ref v 0)
     (+ (vector-ref v 1)
        (+ (vector-ref v 2)
           (+ (vector-ref v 3)
              (+ (vector-ref v 4) (vector-ref v 5)))))))

(let ([vec-alloc (choose-alloc (eq? (read) 1))])
  (let ([vec (vec-alloc 5)])
    (+ (sum vec) (+ 42 (- 30)))))
