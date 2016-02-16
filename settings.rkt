#lang racket

(provide (all-defined-out))

(define use-move-rels
  (make-parameter #t))

(define use-regs
  (make-parameter #t))

;; Initial heap size, in bytes.
(define initial-heap-size
  (make-parameter 10000))

;; Initial root stack size, in bytes.
(define initial-root-stack-size
  (make-parameter 10000))
