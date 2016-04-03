(define (cons [h : Any] [t : Any]) : Any
  (inject (vector h t) (Vector Any Any)))

(define (nil) : Any
  (inject 0 Integer))

(define (head [l : Any]) : Any
  (if (vector? l)
    (vector-ref (project l (Vector Any Any)) 0)
    (inject 0 Integer)))

(project (head (cons (inject 42 Integer) (nil))) Integer)
