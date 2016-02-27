#lang racket

(require "utils.rkt")

(provide elim-movs)

;; Eliminate redundant movs

(define (elim-movs pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map elim-mov-instrs-def defs))]
    [_ (unsupported-form 'patch-instructions pgm)]))

(define (elim-mov-instrs-def def)
  (match def
    [`(define ,tag : ,ret-ty ,meta . ,instrs)
     `(define ,tag : ,ret-ty ,meta ,@(elim-mov-instrs instrs))]
    [_ (unsupported-form 'elim-mov-instrs-def def)]))

(define (elim-mov-instrs instrs)
  (filter-nulls (map elim-mov-instr instrs)))

(define (elim-mov-instr instr)
  (match instr
    [`(movq ,arg1 ,arg2) (if (equal? arg1 arg2) '() instr)]
    [`(if ,cond ,pgm-t ,pgm-f)
     `(if ,cond ,(elim-mov-instrs pgm-t)
                ,(elim-mov-instrs pgm-f))]
    [_ instr]))
