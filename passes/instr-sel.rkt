#lang racket

(require "utils.rkt")

(provide instr-sel)

; In this pass, we also generate an arg (var x).

;; NOTE: Instructions selection doesn't flatten if-statements. The reason is
;; because we need branches for some analysis in next passes (liveness analysis,
;; which effects register allocation, and probably some other passes), and we
;; don't have basic blocks.

(define (instr-sel pgm)
  (match pgm
    [(list-rest 'program vs stmts)
     ; (printf "stmts: ~s~n" stmts)
     `(program ,vs ,@(append-map instr-sel-stmt stmts))]

    [_ (unsupported-form 'instr-sel pgm)]))

(define (instr-sel-stmt stmt)
  (match stmt
    [`(assign ,var ,expr)
     (instr-sel-expr var expr)]

    [`(return ,arg)
     `((movq ,(arg->x86-arg arg) (reg rax)))]

    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     `((if (eq? ,(arg->x86-arg arg1) ,(arg->x86-arg arg2))
         ,(append-map instr-sel-stmt pgm-t)
         ,(append-map instr-sel-stmt pgm-f)))]

    [_ (unsupported-form 'instr-sel-stmt stmt)]))

(define (instr-sel-expr bind-to expr)
  (match expr
    [(or (? fixnum?) (? symbol?) (? boolean?))
     `(,(instr-sel-arg bind-to expr))]

    [`(read)
     `((callq read_int)
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(- ,arg)
     `(,(instr-sel-arg bind-to arg)
       (negq ,(arg->x86-arg bind-to)))]

    [`(not ,arg)
     `(,(instr-sel-arg bind-to arg)
       (xorq (int 1) ,(arg->x86-arg bind-to)))]

    [`(eq? ,arg1 ,arg2)
     `((cmpq ,(arg->x86-arg arg1) ,(arg->x86-arg arg2))
       (sete (byte-reg al))
       (movzbq (byte-reg al) ,(arg->x86-arg bind-to)))]

    [`(+ ,arg1 ,arg2)
     `(,(instr-sel-arg bind-to arg1)
       (addq ,(arg->x86-arg arg2) ,(arg->x86-arg bind-to)))]

    [_ (unsupported-form 'instr-sel-expr expr)]))

(define (instr-sel-arg bind-to arg)
  `(movq ,(arg->x86-arg arg) ,(arg->x86-arg bind-to)))

(define (arg->x86-arg arg)
  (cond [(symbol? arg) `(var ,arg)]
        [(fixnum? arg) `(int ,arg)]
        [(boolean? arg) `(int ,(if arg 1 0))]
        ; [(boolean? arg) `(bool ,arg)]
        [else (unsupported-form 'arg->x86-arg arg)]))
