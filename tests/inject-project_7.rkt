(let ([fn (lambda: ([x : Integer]) : Integer x)])
  ((project (inject fn (Integer -> Integer)) (Integer -> Integer)) 42))
