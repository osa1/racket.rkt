(define (cons h t) (vector h t))

(define (car lst) (vector-ref lst 0))

(define (cdr lst) (vector-ref lst 1))

(car (cdr (cons 0 (cons 42 123))))
