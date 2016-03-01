(define (fun1 [i : Integer]) : Integer
  i)

(define (fun2 [i : Integer]) : Integer
  0)

(define (fun3 [i : Integer]) : Integer
  ((if (eq? i 1) fun1 fun2) 42))

(fun3 (read))
