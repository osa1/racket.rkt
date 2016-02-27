#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide initialize-rts)

(define (initialize-rts pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(initialize-rts-iter defs))]
    [_ (unsupported-form 'initialize-rts pgm)]))

(define (initialize-rts-iter defs)
  (match defs
    [`((define main : void ,meta . ,stmts) . ,rest)
     (cons `(define main : void ,meta
              (movq (int ,(initial-root-stack-size)) (reg rdi))
              (movq (int ,(initial-heap-size)) (reg rsi))
              (callq (toplevel-fn initialize))
              ,@stmts)
           rest)]
    [`(,def . ,defs)
     (cons def (initialize-rts-iter defs))]

    [`()
     (error 'initialize-rts-iter "Can't find main.")]))
