(define (pow x n)
  (if (eq? n 0)
    1
    (* x (pow x (+ n (- 1))))))

(+ (pow 2 (read)) (read))
