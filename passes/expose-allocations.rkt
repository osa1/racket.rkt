#lang racket

(require "utils.rkt")

(provide expose-allocations)

;; TODO: We need type of variables here. brb modifying the previous passes.

(define (expose-allocations pgm)
  (match pgm
    [`(program ,vs . ,stmts)
     `(program ,vs ,@(append-map expose-allocations-stmt stmts))]
    [_ (unsupported-form 'expose-allocations pgm)]))

(define (expose-allocations-stmt stmt)
  stmt)
