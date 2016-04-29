(define (pow [x : Integer] [n : Integer]) : Integer
  (if (eq? n 0)
    1
    (* x (pow x (+ n (- 1))))))

(+ (pow (read) (read)) (read))
