(define (to-int [a : Any]) : Integer
  (if (procedure? a)
    1
    (if (vector? a)
      2
      (if (boolean? a)
        3
        (if (integer? a)
          4
          0)))))

(+ (to-int (lambda: ([x : Integer]) : Integer x)) 41)
