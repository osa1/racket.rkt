(define (cons h t) (vector h t))

(define (cdr lst) (vector-ref lst 1))

(cdr (cons 0 42))
