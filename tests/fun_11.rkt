(define (fun1 [i : Integer]) : Integer
  i)

(define (fun2 [i : Integer]) : Integer
  0)

((if (eq? (read) 1) fun1 fun2) 42)
