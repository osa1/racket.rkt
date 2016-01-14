(let [(x (let [(x 20)]
           (let [(y 30)] (+ x y))))]
  (+ x x))
