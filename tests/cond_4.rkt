(let [(cond1 (eq? (read) 0))]
  (let [(cond2 (eq? (read) 0))]
    (let [(nested-if (if cond2 40 444))]
      (let [(outer-if (if cond1 777 (+ 2 nested-if)))]
        outer-if))))
