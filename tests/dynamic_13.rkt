(define (cons h t) (vector h t))

(define (nil) 999)

(define (car lst) (vector-ref lst 0))

(define (cdr lst) (vector-ref lst 1))

(define (nth lst n)
  (if (eq? n 0)
    (car lst)
    (nth (cdr lst) (+ n (- 1)))))

(define (range n) (range-iter n 0))

(define (range-iter n cur)
  (if (eq? cur n)
    (nil)
    (cons cur (range-iter n (+ cur 1)))))

(nth (range 100) 42)
