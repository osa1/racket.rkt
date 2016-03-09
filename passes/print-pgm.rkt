#lang racket

(provide print-pgm)

(define (print-pgm msg)
  (lambda (pgm)
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    (display msg)
    (newline)
    (pretty-print pgm)
    (display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (newline)
    pgm))
