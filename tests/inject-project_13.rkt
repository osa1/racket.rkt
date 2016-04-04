(define (cons [h : Any] [t : Any]) : Any
  (inject (vector h t) (Vector Any Any)))

(define (nil) : Any
  (inject 0 Integer))

(define (head [l : Any]) : Any
  (vector-ref (project l (Vector Any Any)) 0))

(define (tail [l : Any]) : Any
  (vector-ref (project l (Vector Any Any)) 1))

(project (head (tail (cons (inject 123 Integer) (cons (inject 42 Integer) (nil))))) Integer)
