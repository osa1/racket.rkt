(let [(x (let [(y #t)]
           (and y #f)))]
  (if x (read) (read)))
