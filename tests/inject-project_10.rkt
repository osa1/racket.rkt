(define (cons [h : Integer] [t : Any]) : (Vector Integer Any)
  (vector h t))

(define (nil) : Any
  (inject (vector) (Vector)))

(vector-ref (cons 42 (nil)) 0)
