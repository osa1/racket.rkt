(define (cons h t) (vector h t))

(define (car lst) (vector-ref lst 0))

(define (cdr lst) (vector-ref lst 1))

(define (nth lst n)
  (if (eq? n 0)
    (car lst)
    (nth (cdr lst) (+ n (- 1)))))

(nth (cons 0 (cons 42 123)) 1)
