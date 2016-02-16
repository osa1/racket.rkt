#lang racket

(require "utils.rkt")

(provide patch-instructions)

;; If the instructions takes two arguments and both of the arguments are memory
;; locations, just make the destination a %rax, then movq %rax mem.

(define (patch-instructions pgm)
  (match pgm
    [(list-rest 'program s instrs)
     `(program ,s ,@(append-map patch-instructions-instr instrs))]
    [_ (unsupported-form 'patch-instructions pgm)]))

(define (patch-instructions-instr instr)
  (match instr
    [`(,(or 'movq 'cmpq) ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       `((movq ,arg1 (reg rax))
         (,(car instr) (reg rax) ,arg2))
       `(,instr))]

    [`(,(or 'addq 'subq) ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       `((movq ,arg2 (reg rax))
         (,(car instr) ,arg1 (reg rax))
         (movq (reg rax) ,arg2))
       `(,instr))]

    [`(if ,cond ,pgm-t ,pgm-f)
     `((if ,cond ,(append-map patch-instructions-instr pgm-t)
                 ,(append-map patch-instructions-instr pgm-f)))]

    [_ `(,instr)]))

(define (arg-mem? arg)
  (match arg
    [`(stack ,_) #t]
    [`(,(or 'reg 'int) ,_) #f]
    [`(global-value ,_) #t]
    [`(offset ,_ ,_) #t]
    [_ (unsupported-form 'arg-mem? arg)]))
