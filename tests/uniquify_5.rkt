(let [(x (let [(x 20)]
           (let [(y 30)] (+ x (+ y (- (let [(y 1)] y)))))))]
  (+ x (- (let [(x (+ x (- 1)))] x))))
