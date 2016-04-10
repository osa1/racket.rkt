#lang racket

(require "../utils.rkt")

(provide remove-mov)

; - Remove (movq var1 var2) and (movq var2 var1)
; - Replace var1 and var2 with new-var everywhere
(define (remove-mov var1 var2 new-var instrs)

  ; (printf "remove-mov: replacing ~a and ~a with ~a~n" var1 var2 new-var)

  (define (replace-arg arg)
    (if (or (equal? arg var1) (equal? arg var2))
      new-var
      arg))

  (define (iter instr)
    (match instr
      [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
       `((if (eq? ,(replace-arg arg1) ,(replace-arg arg2))
           ,(append-map iter pgm-t)
           ,(append-map iter pgm-f)))]

      [`(movq (offset ,arg1 ,offset) ,arg2)
       `((movq (offset ,(replace-arg arg1) ,offset) ,(replace-arg arg2)))]

      [`(movq ,arg1 (offset ,arg2 ,offset))
       `((movq ,(replace-arg arg1) (offset ,(replace-arg arg2) ,offset)))]

      [`(movq ,arg1 ,arg2)
       (if (or (and (equal? arg1 var1) (equal? arg2 var2))
               (and (equal? arg1 var2) (equal? arg2 var1)))
         `() ; Remove the mov
         `((movq ,(replace-arg arg1) ,(replace-arg arg2))))]

      [`(,(or 'addq 'subq 'imulq 'leaq 'cmpq 'xorq 'andq) ,arg1 ,arg2)
       `((,(car instr) ,(replace-arg arg1) ,(replace-arg arg2)))]

      [`(,(or 'negq 'pushq 'popq) ,arg)
       `((,(car instr) ,(replace-arg arg)))]

      [`(callq ,n ,arg)
       `((callq ,n ,(replace-arg arg)))]

    [`(retq) `(,instr)]

    [`(lahf) `(,instr)]

    [`(,(or 'sete 'setl) (byte-reg al)) `(,instr)]

    [`(movzbq (byte-reg al) ,arg)
     `((movzbq (byte-reg al) ,(replace-arg arg)))]

    [_ (unsupported-form 'remove-mov instr)]))

  (append-map iter instrs))
