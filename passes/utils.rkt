#lang racket

(provide (all-defined-out))

(define (not-null? e) (not (null? e)))

(define (filter-nulls lst) (filter not-null? lst))

(define (unsupported-form fname form)
  (error fname "Unsupported form: ~s~n" form))
