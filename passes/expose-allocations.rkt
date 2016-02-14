#lang racket

(require "utils.rkt")

(provide expose-allocations)

;; NOTE: expose-allocations removes type annotations! Remove this if types are
;; needed in some later pass.

(define (expose-allocations pgm)
  (match pgm
    [`(program ,vs . ,stmts)
     `(program ,vs ,@(append-map expose-allocations-stmt stmts))]
    [_ (unsupported-form 'expose-allocations pgm)]))

(define (expose-allocations-stmt stmt)
  (match stmt
    [`(assign ,x (vector . ,tys) (vector . ,elems))
     (let ([bytes-needed
             ; one byte for info, one byte for each element in the vector
             (+ 8 (* 8 (length tys)))])
       `((if (collection-needed? ,bytes-needed)
           ((collect ,bytes-needed))
           ())

         (assign ,x (allocate ,(length tys) ,tys))

         ,@(map (lambda (idx elem)
                  `(vector-set! ,x ,idx ,elem))
                (range (length elems))
                elems)))]

    [`(assign ,x ,_ ,arg) `((assign ,x ,arg))]

    [`(if ,cond ,_ ,pgm-t ,pgm-f) `((if ,cond ,(append-map expose-allocations-stmt pgm-t)
                                              ,(append-map expose-allocations-stmt pgm-f)))]

    [`(return ,x) `((return ,x))]

    [_ (unsupported-form 'expose-allocations-stmt stmt)]))
