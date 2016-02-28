(define (mul [x : Integer] [y : Integer]) : Integer
  (if (eq? y 0)
	0
	(if (eq? y 1)
	  x
	  (+ x (mul x (+ y (- 1)))))))

(define (fac [x : Integer]) : Integer
  (if (eq? x 1)
	1
	(mul x (fac (+ x (- 1))))))

(fac 3)
