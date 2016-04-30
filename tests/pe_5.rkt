(define (even? [x : Integer]) : Boolean
  (if (eq? x 0)
    #t
    (odd? (+ x (- 1)))))

(define (odd? [x : Integer]) : Boolean
  (if (eq? x 1)
    #t
    (even? (+ x (- 1)))))

(if (even? 10) 42 0)
