#lang racket

(require "utils.rkt")

(provide patch-instructions)

;; If the instructions takes two arguments and both of the arguments are memory
;; locations, just make the destination a %rax, then movq %rax mem.

(define (patch-instructions pgm)
  (match pgm
    [(list-rest 'program s stmts)
     `(program ,s ,@(append-map patch-instructions-stmt stmts))]
    [_ (unsupported-form 'patch-instructions pgm)]))

(define (arg-mem? arg)
  (match arg
    [`(stack ,_) #t]
    [`(,(or 'reg 'int) ,_) #f]
    [_ (unsupported-form 'arg-mem? arg)]))

(define (patch-instructions-stmt stmt)
  (match stmt
    [`(movq ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       (list `(movq ,arg1 (reg rax))
             `(movq (reg rax) ,arg2))
       (list stmt))]

    [`(,(or 'addq 'subq) ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       (list (list 'movq arg2 '(reg rax))
             (list (car stmt) arg1 '(reg rax))
             (list 'movq '(reg rax) arg2))
       (list stmt))]

    [`(if ,cond ,pgm-t ,pgm-f)
     `((if ,cond ,(append-map patch-instructions-stmt pgm-t)
                 ,(append-map patch-instructions-stmt pgm-f)))]

    [_ (list stmt)]))
