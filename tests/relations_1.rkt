; NOTE: This only tests choose-branch - we need to eliminate these comparisons
; at some point because 'cmpq' doesn't accept two imm arguments.

(+ (if (< 1 1) 0 10)
   (+ (if (> 1 1) 0 10)
      (+ (if (<= 1 1) 10 0)
         (if (>= 1 1) 12 0))))
