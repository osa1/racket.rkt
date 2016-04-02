(let ([fn (lambda: ([x : Any]) : Any x)])
  (let ([arg 42])
    (let ([vec (vector (inject fn (Any -> Any)) (inject arg Integer))])
      (project
        ((project (vector-ref vec 0) (Any -> Any))
         (vector-ref vec 1))
        Integer))))
