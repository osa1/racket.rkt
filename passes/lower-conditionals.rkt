#lang racket

(require "utils.rkt")

(provide lower-conditionals)

;; Replace if blocks with jumps

(define (lower-conditionals pgm)
  (match pgm
    [(list-rest 'program meta instrs)
     `(program ,meta ,@(append-map lower-conditionals-instr instrs))]
    [_ (unsupported-form 'lower-conditionals pgm)]))

(define (lower-conditionals-instr instr)
  (match instr
    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     (let [(then-lbl (gensym "t_branch"))
           (end-lbl  (gensym "end_branch"))
           (t-instrs (append-map lower-conditionals-instr pgm-t))
           (f-instrs (append-map lower-conditionals-instr pgm-f))]
       `((cmpq ,arg1 ,arg2)
         (je ,then-lbl)
         ,@f-instrs
         (jmp ,end-lbl)
         (label ,then-lbl)
         ,@t-instrs
         (label ,end-lbl)))]

    [`(if ,_ ,_ ,_ ,_ ,_)
     (error 'lower-conditionals "Found if with meta data!~n~s~n" instr)]

    [_ (list instr)]))
