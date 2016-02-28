(define (mul [x : Integer] [y : Integer]) : Integer
  (if (eq? y 0)
	0
	(if (eq? y 1)
	  x
	  (+ x (mul x (+ y (- 1)))))))

(+ (+ (mul 3 4) (mul 6 5)) (mul 100 0))
