#lang racket

(require "utils.rkt")

(provide expose-allocations)

(define (expose-allocations pgm)
  (match pgm
    [`(program ,vs . ,stmts)
     `(program ,vs ,@(append-map expose-allocations-stmt stmts))]
    [_ (unsupported-form 'expose-allocations pgm)]))

(define (expose-allocations-stmt stmt)
  (match stmt
    [`(assign ,x (Vector . ,tys) (vector . ,elems))
     (let ([bytes-needed
             ; one byte for info, one byte for each element in the vector
             (+ 8 (* 8 (length tys)))])
       `((if (collection-needed? ,bytes-needed)
           ((collect ,bytes-needed))
           ())

         (assign ,x (allocate ,tys))

         ,@(map (lambda (idx elem)
                  `(vector-set! ,x ,idx ,elem))
                (range (length elems))
                elems)))]

    [`(assign ,x ,_ ,arg) `((assign ,x ,arg))]

    [`(if ,cond ,pgm-t ,pgm-f) `((if ,cond ,(append-map expose-allocations-stmt pgm-t)
                                           ,(append-map expose-allocations-stmt pgm-f)))]

    [`(return ,x) `((return ,x))]

    [_ (unsupported-form 'expose-allocations-stmt stmt)]))
