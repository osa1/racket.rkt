(define (cons h t) (vector h t))

(define (car lst) (vector-ref lst 0))

(car (cons 42 123))
