(define (cons [h : Any]) : (Vector Any)
  (vector h))

(project (vector-ref (cons (inject 42 Integer)) 0) Integer)
