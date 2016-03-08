#lang racket

(require "utils.rkt")

(provide lower-conditionals)

;; Replace if blocks with jumps

(define (lower-conditionals pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map lower-conditionals-def defs))]
    [_ (unsupported-form 'lower-conditionals pgm)]))

(define (lower-conditionals-def def)
  (match def
    [`(define ,tag : ,ret-ty ,meta . ,instrs)
     `(define ,tag : ,ret-ty ,meta ,@(append-map lower-conditionals-instr instrs))]
    [_ (unsupported-form 'lower-conditionals-def def)]))

(define (lower-conditionals-instr instr)
  (match instr
    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     (let [(then-lbl (fresh "t_branch"))
           (end-lbl  (fresh "end_branch"))
           (t-instrs (append-map lower-conditionals-instr pgm-t))
           (f-instrs (append-map lower-conditionals-instr pgm-f))]
       `(; If one of the arguments is a immediate value, it needs to be the
         ; first one. This is just how cmpq works.
         ,(if (arg-imm? arg2)
            `(cmpq ,arg2 ,arg1)
            `(cmpq ,arg1 ,arg2))
         (je ,then-lbl)
         ,@f-instrs
         (jmp ,end-lbl)
         (label ,then-lbl)
         ,@t-instrs
         (label ,end-lbl)))]

    [`(if ,_ ,_ ,_ ,_ ,_)
     (error 'lower-conditionals "Found if with meta data!~n~s~n" instr)]

    [_ (list instr)]))
