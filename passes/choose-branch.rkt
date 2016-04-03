#lang racket

(require "utils.rkt")

(provide choose-branch)

;; TODO: I'm very tired right now -- this does more than what it advertises --
;; it simplifies eq?s etc.

(define (choose-branch pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map (lift-def choose-branch-expr) defs))]
    [_ (unsupported-form 'choose-branch pgm)]))

;; TODO: We can be much more aggressive here, by doing a simple form of
;; compile-time evaluation.
(define (choose-branch-expr e0)
  (match (cdr e0)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Important cases

    [`(if ,e1 ,e2 ,e3)
     (match (choose-branch-expr e1)
       [`(,_ . #t) (choose-branch-expr e2)]
       [`(,_ . #f) (choose-branch-expr e3)]
       [e1 `(,(car e0) . (if ,e1 ,(choose-branch-expr e2) ,(choose-branch-expr e3)))])]

    [`(eq? ,e1 ,e2)
     (match-let ([`(,e1-ty . ,e1) (choose-branch-expr e1)]
                 [`(,e2-ty . ,e2) (choose-branch-expr e2)])
       (cond [(and (or (fixnum? e1) (boolean? e1))
                   (or (fixnum? e2) (boolean? e2)))
              `(Boolean . ,(equal? e1 e2))]

             [(and (symbol? e1) (symbol? e2) (equal? e1 e2))
              `(Boolean . #t)]

             [else `(,(car e0) . (eq? (,e1-ty . ,e1) (,e2-ty . ,e2)))]))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Simple cases

    [(or (? fixnum?) (? boolean?) (? symbol?) `(read))
     e0]

    [`(lambda: ,args : ,ret-ty ,body)
     `(,(car e0) . (lambda: ,args : ,ret-ty ,(choose-branch-expr body)))]

    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     `(,(car e0) . (,(cadr e0) ,(choose-branch-expr e1)))]

    [`(,(or 'project 'inject) ,e1 ,ty)
     `(,(car e0) . (,(cadr e0) ,(choose-branch-expr e1) ,ty))]

    [`(,(or '+) ,e1 ,e2)
     `(,(car e0) . (,(cadr e0) ,(choose-branch-expr e1) ,(choose-branch-expr e2)))]

    [`(let ([,var ,e1]) ,body)
     `(,(car e0) . (let ([,var ,(choose-branch-expr e1)]) ,(choose-branch-expr body)))]

    [`(vector-ref ,e1 ,idx)
     `(,(car e0) . (vector-ref ,(choose-branch-expr e1) ,idx))]

    [`(vector-set! ,vec ,idx ,e)
     `(,(car e0) . (vector-set! ,(choose-branch-expr vec) ,idx ,(choose-branch-expr e)))]

    [`(vector ,elem-tys . ,elems)
     `(,(car e0) . (vector ,elem-tys ,@(map choose-branch-expr elems)))]

    [`(vector) e0]

    [`(,f . ,args)
     `(,(car e0) . (,(choose-branch-expr f) ,@(map choose-branch-expr args)))]

    [_ (unsupported-form 'choose-branch-expr (cdr e0))]))
