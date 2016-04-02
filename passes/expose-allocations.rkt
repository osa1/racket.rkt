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
    [`(define-closure-wrapper . ,_) def]
    [_ (unsupported-form 'expose-allocations-def def)]))

(define (expose-allocations-stmt stmt)
  (match stmt
    [`(assign ,x (Vector . ,tys) (vector . ,elems))
     (let ([bytes-needed
             ; one word for info, one word for each element in the vector
             (+ 8 (* 8 (length tys)))])
       `((if (collection-needed? ,bytes-needed)
           ((collect ,bytes-needed))
           ())

         (assign ,x (allocate ,tys))

         ,@(map (lambda (idx elem)
                  `(vector-set! ,x ,idx ,elem))
                (range (length elems))
                elems)))]

    [`(assign ,x Any (inject ,arg ,ty))
     ; How many vector slots we need depends on the size of the encoding
     (define ty-encoding (encode-type ty))
     (define size (length ty-encoding))
     (define quadwords (byte-list-to-quadword-list ty-encoding))
     `((if (collection-needed? ,size)
         ((collect ,size))
         ())

       (assign ,x (allocate ,(append (replicate 'Integer (length quadwords)) `(,ty))))

       ,@(map (lambda (q-idx qword) `(vector-set! ,x ,q-idx ,qword))
              (range (length quadwords)) quadwords)

       (vector-set! ,x ,(length quadwords) ,arg))]

    ; Just to make sure
    [`(assign ,_ ,not-any (inject ,_ ,_))
     (error 'expose-allocations-stmt "inject returns Any: ~a~n" stmt)]

    [`(assign ,x ,_ ,arg) `((assign ,x ,arg))]

    [`(if ,cond ,pgm-t ,pgm-f) `((if ,cond ,(append-map expose-allocations-stmt pgm-t)
                                           ,(append-map expose-allocations-stmt pgm-f)))]

    [`(return ,x) `((return ,x))]

    [_ (unsupported-form 'expose-allocations-stmt stmt)]))
