#lang racket

(require "../utils.rkt")

(provide assign-homes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assigning vars to their locations on the machine

(define (assign-homes def mapping)
  ; (printf "============ assign homes ==========~n")
  ; (pretty-print def)
  ; (pretty-print mapping)
  ; (printf "====================================~n")
  (match def
    [`(define ,tag : ,ret-ty ,stack-locs-used . ,instrs)
     `(define ,tag : ,ret-ty
        (,(align-stack (* 8 stack-locs-used)))
        ,@(assign-home-instrs mapping instrs))]
    [_ (unsupported-form 'assign-homes def)]))

(define (align-stack stack)
  ; TODO: Document this. Since we don't push %rbp anymore we need to play this
  ; game.
  ; TODO: Only do this if the function calls functions.
  (+ stack 8))

(define (assign-home-instrs asgns instrs)
  (map (lambda (instr) (assign-home-instr asgns instr)) instrs))

(define (assign-home-instr asgns instr)
  (match instr
    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     `(if (eq? ,(assign-home-arg asgns arg1) ,(assign-home-arg asgns arg2))
        ,(assign-home-instrs asgns pgm-t)
        ,(assign-home-instrs asgns pgm-f))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,arg1 ,arg2)
     `(,(car instr) ,(assign-home-arg asgns arg1) ,(assign-home-arg asgns arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     `(,(car instr) ,(assign-home-arg asgns arg))]

    [`(callq ,arg) `(callq ,(assign-home-arg asgns arg))]

    [`(retq) instr]

    [`(,(or 'sete 'setl) (byte-reg al)) instr]

    [`(movzbq (byte-reg al) ,arg)
     `(movzbq (byte-reg al) ,(assign-home-arg asgns arg))]

    [_ (unsupported-form 'assign-home-instr instr)]))

(define (assign-home-arg asgns arg)
  (match arg
    [`(int ,_) arg]
    [`(reg ,_) arg]
    [`(mem-loc ,l) `(stack ,(* 8 l))]
    [`(global-value ,_) arg]
    [`(offset ,arg ,offset)
     `(offset ,(assign-home-arg asgns arg) ,offset)]
    [`(toplevel-fn ,_) arg]
    [`(var ,var)
     (let [(asgn (hash-ref asgns `(var ,var) '()))]
       (cond [(null? asgn)
              (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
             [(symbol? asgn) `(reg ,asgn)]
             [#t (error 'assign-home-arg "Variable mapped to an unknown thing: ~s~n"
                        asgn)]))]
    [_ (unsupported-form 'assign-home-arg arg)]))
