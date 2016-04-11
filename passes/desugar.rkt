#lang racket

(require "utils.rkt")

(provide desugar)

; - Desugar 'and'.
; - Generate a 'main' define form from the expression in the program.
;
; NOTE: We can't desugar '>' and '>=' to '<' and '<=', as that would change the
; evaluation order of things. Example:
;
;   (>= (read) (read)) ~> (<= (read) (read))
;   stdin: 1 2
;
; Previously the result was 0, now it's 1.
;
; The place to do this is 'flatten' as that step has control over evaluation
; order.

(define (desugar pgm)
  (match pgm
    [`(program . ,things)
     (let-values ([(defs expr) (split-last things)])
       (let ([defs (map (lift-def desugar-expr) defs)]
             [expr (desugar-expr expr)])
         `(program ,@defs
                   (define main : void
                     (void . (((Integer -> void) . print-int) ,expr))))))]
    [_ (unsupported-form 'desugar pgm)]))

(define (desugar-expr e0)
  (match (cdr e0)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Important cases

    [`(and ,e1 ,e2)
     (let ([e1-ds (desugar-expr e1)]
           [e2-ds (desugar-expr e2)])
       `(,(car e0) . (if (Boolean . (eq? ,e1-ds (Boolean . #t))) ,e2-ds (Boolean . #f))))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [(or (? fixnum?) (? boolean?) (? symbol?)) e0]

    [`(lambda: ,args : ,ret-ty ,body)
     `(,(car e0) . (lambda: ,args : ,ret-ty ,(desugar-expr body)))]

    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     `(,(car e0) . (,(cadr e0) ,(desugar-expr e1)))]

    [`(,(or '+ 'eq? 'eq?-dynamic '< '<= '> '>= 'vector-ref-dynamic) ,e1 ,e2)
     `(,(car e0) . (,(cadr e0) ,(desugar-expr e1) ,(desugar-expr e2)))]

    [`(,(or 'inject 'project) ,e1 ,ty)
     `(,(car e0) . (,(cadr e0) ,(desugar-expr e1) ,ty))]

    [`(if ,e1 ,e2 ,e3)
     `(,(car e0) . (if ,(desugar-expr e1) ,(desugar-expr e2) ,(desugar-expr e3)))]

    [`(let ([,var ,e1]) ,e2)
     `(,(car e0) . (let ([,var ,(desugar-expr e1)]) ,(desugar-expr e2)))]

    [`(read) e0]

    [`(vector-ref ,e1 ,e2)
     `(,(car e0) . (vector-ref ,(desugar-expr e1) ,e2))]

    [`(vector-set! ,vec ,idx ,e)
     `(,(car e0) . (vector-set! ,(desugar-expr vec) ,idx ,(desugar-expr e)))]

    [`(vector-set!-dynamic ,vec ,idx ,e)
     `(,(car e0) . (vector-set!-dynamic ,(desugar-expr vec) ,(desugar-expr idx) ,(desugar-expr e)))]

    [`(vector . ,elems)
     `(,(car e0) . (vector ,@(map desugar-expr elems)))]

    [`(,f . ,args)
     `(,(car e0) . (,(desugar-expr f) ,@(map desugar-expr args)))]

    [_ (unsupported-form 'desugar-expr e0)]))
