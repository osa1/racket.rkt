#lang racket

(require "utils.rkt")

(provide expose-allocations)

(define (expose-allocations pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map expose-allocations-def defs))]
    [_ (unsupported-form 'expose-allocations pgm)]))

(define (expose-allocations-def def)
  (match def
    [`(define ,tag : ,ret-ty ,meta . ,pgm)
     `(define ,tag : ,ret-ty ,meta ,@(append-map expose-allocations-stmt pgm))]
    [_ (unsupported-form 'expose-allocations-def def)]))

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
