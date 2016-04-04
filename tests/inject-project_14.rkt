(define (cons [h : Any] [t : Any]) : Any
  (inject (vector h t) (Vector Any Any)))

(define (nil) : Any
  (inject 0 Integer))

; Not quite range but whatever
(define (range [i : Integer]) : Any
  (if (eq? i 0)
    (nil)
    (cons (inject i Integer) (range (+ (- 1) i)))))

(define (head [l : Any]) : Any
  (vector-ref (project l (Vector Any Any)) 0))

(define (tail [l : Any]) : Any
  (vector-ref (project l (Vector Any Any)) 1))

(define (nth [lst : Any] [i : Integer]) : Any
  (if (eq? i 0)
    (head lst)
    (nth (tail lst) (+ (- 1) i))))

(project (nth (range 42) 0) Integer)
