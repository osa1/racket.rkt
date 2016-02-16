#lang racket

(require "utils.rkt")

(provide patch-instructions)

;; Patch instructions so that:
;;
;; - We don't use memory locations in both arguments. (use rax as temp)
;;
;; - movzbq doesn't have memory location as its destination. (use rax as temp)

(define (patch-instructions pgm)
  (match pgm
    [(list-rest 'program s instrs)
     `(program ,s ,@(append-map patch-instructions-instr instrs))]
    [_ (unsupported-form 'patch-instructions pgm)]))

(define (patch-instructions-instr instr)
  (match instr
    [`(,(or 'movq 'cmpq) ,arg1 ,arg2)
     (cond
       [(and (stack-offset? arg1) (stack-offset? arg2))
        ; We can't fix this without two temp regs. Good thing that the
        ; instruction selector doesn't generate such code.
        (error 'patch-instructions-instr "Both arguments are stack offsets: ~s~n" instr)]

       [(and (arg-mem? arg1) (stack-offset? arg2))
        ; We need two temp registers. We currently generate this code when both
        ; vectors are on stack.
        (error 'patch-instructions-instr "Second argument is a stack offset: ~s~n" instr)]

       [(and (stack-offset? arg1) (arg-mem? arg2))
        (match arg1
          [`(offset (stack ,stack-off) ,disp)
           `((movq (stack ,stack-off) (reg rax))
             (movq (offset (reg rax) ,disp) (reg rax))
             (,(car instr) (reg rax) ,arg2))])]

       [(and (arg-mem? arg1) (arg-mem? arg2))
        `((movq ,arg1 (reg rax))
          (,(car instr) (reg rax) ,arg2))]

       [else
        `(,instr)])]

    [`(,(or 'addq 'subq) ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       `((movq ,arg2 (reg rax))
         (,(car instr) ,arg1 (reg rax))
         (movq (reg rax) ,arg2))
       `(,instr))]

    [`(movzbq ,arg1 ,arg2)
     (if (arg-mem? arg2)
       `((movzbq ,arg1 (reg rax))
         (movq (reg rax) ,arg2))
       `(,instr))]

    [`(if ,cond ,pgm-t ,pgm-f)
     `((if ,cond ,(append-map patch-instructions-instr pgm-t)
                 ,(append-map patch-instructions-instr pgm-f)))]

    [_ `(,instr)]))

(define (stack-offset? arg)
  (match arg
    [`(offset (stack ,_) ,_)
     #t]
    [_ #f]))
