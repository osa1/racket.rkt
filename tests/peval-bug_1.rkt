(define (nth [lst : Any] [n : Any]) : Integer
  (let ([ni (project n Integer)])
    (let ([lst-v (project lst (Vectorof Any))])
      (if (eq? ni 0)
        (project (vector-ref lst-v 0) Integer)
        (nth
          (vector-ref lst-v 1)
          (inject (+ ni (- 1)) Integer))))))

(nth
  (inject
    (vector (inject 0 Integer)
            (inject (vector (inject 42 Integer) (inject 123 Integer)) (Vectorof Any)))
    (Vectorof Any))
  (inject 1 Integer))
