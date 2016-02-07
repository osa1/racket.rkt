(let [(cond1 (eq? (read) 0))]
  (let [(cond2 (eq? (read) 0))]
    (if cond1
      777
      (+ 2 (if cond2
             40
             444)))))
