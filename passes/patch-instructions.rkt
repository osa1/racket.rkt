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
    [`(program . ,defs)
     `(program ,@(map patch-instructions-def defs))]

    [_ (unsupported-form 'patch-instructions pgm)]))

(define (patch-instructions-def def)
  (match def
    [`(define ,tag : ,ret-ty ,meta . ,instrs)
     `(define ,tag : ,ret-ty ,meta ,@(append-map patch-instructions-instr instrs))]
    [_ (unsupported-form 'patch-instructions-def)]))

(define (patch-instructions-instr instr)
  (match instr
    [`(,(or 'movq 'cmpq) ,arg1 ,arg2)
     (cond
       [(and (stack-offset? arg1) (stack-offset? arg2))
        ; We can't fix this without two temp regs. Good thing that the
        ; instruction selector doesn't generate such code.
        (error 'patch-instructions-instr "Both arguments are stack offsets: ~s~n" instr)]

       [(and (arg-mem? arg1) (stack-offset? arg2))
        ; No way to do this without two spare registers!
        ; (error 'patch-instructions-instr
        ;        (string-append "Second argument is a stack offset: ~s~n"
        ;                       "No way to do this without two temp registers!~n"
        ;                       "(so, fix your codegen)~n")
        ;        instr)
        ; That's funny, it seems like I can use pusq/popq for mem->mem movs
        (match arg2
          [`(offset (stack ,stack-off) ,disp)
           `((pushq ,arg1)
             (movq (stack ,stack-off) (reg rax))
             (popq (offset (reg rax) ,disp)))])]

       [(and (stack-offset? arg1) (arg-mem? arg2))
        (match arg1
          [`(offset (stack ,stack-off) ,disp)
           `((movq (stack ,stack-off) (reg rax))
             (movq (offset (reg rax) ,disp) (reg rax))
             (,(car instr) (reg rax) ,arg2))])]

       [(stack-offset? arg1)
        (match arg1
          [`(offset (stack ,stack-off) ,disp)
           `((movq (stack ,stack-off) (reg rax))
             (movq (offset (reg rax) ,disp) ,arg2))])]

       [(stack-offset? arg2)
        (match arg2
          [`(offset (stack ,stack-off) ,disp)
           `((movq (stack ,stack-off) (reg rax))
             (movq ,arg1 (offset (reg rax) ,disp)))])]

       [(and (arg-mem? arg1) (arg-mem? arg2))
        `((movq ,arg1 (reg rax))
          (,(car instr) (reg rax) ,arg2))]

       [else
        `(,instr)])]

    [`(leaq ,arg1 ,arg2)
     (if (arg-mem? arg2)
       `((leaq ,arg1 (reg rax))
         (movq (reg rax) ,arg2))
       instr)]

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
    [`(offset (stack ,_) ,_) #t]
    [_ #f]))
