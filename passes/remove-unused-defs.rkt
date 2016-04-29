#lang racket

(require "utils.rkt")
(require (only-in "closure-convert.rkt" fvs))

(provide rm-unused-defs)

(define (rm-unused-defs pgm)
  (match pgm
    [`(program . ,defs)
     (define-values (funs main) (split-last defs))

     (match main
       [`(define main : void ,body)
        (define initial-useds (list->set (set-map (fvs body) cdr)))
        (define useds (loop initial-useds funs))
        `(program
           ,@(filter (lambda (def)
                       (match def
                         [`(define (,fname . ,_) . ,_)
                          (set-member? useds fname)]
                         [_ (unsupported-form 'rm-unused-defs-filter def)]))
                     funs)
           ,main)]
       [_ (unsupported-form 'remove-unused-defs main)])]
    [_ (unsupported-form 'remove-unused-defs pgm)]))

(define (loop useds defs)

  (define useds-next
    (apply
      set-union
      useds
      (map (lambda (def)
             (match def
               [`(define (,fname . ,_) : ,_ ,body)
                 (if (set-member? useds fname)
                   (list->set (set-map (fvs body) cdr))
                   (set))]
               [_ (unsupported-form 'loop def)]))
           defs)))

  (if (equal? useds useds-next)
    useds
    (loop useds-next defs)))
