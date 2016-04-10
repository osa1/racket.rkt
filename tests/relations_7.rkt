; All at once

(+ (if (>= (read) (read)) 9 0)
   (+ (if (> (read) (read)) 0 10)
      (+ (if (<= (read) (read)) 11 0)
         (if (< (read) (read)) 12 0))))
