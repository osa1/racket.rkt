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

              ; Initialize RTS
              (assign ,(fresh "unused") 'void
                      (app (toplevel-fn initialize)
                           ,(initial-root-stack-size) ,(initial-heap-size)))

              ; Original program
              ,@stmts

              ; Shutdown RTS
              (assign ,(fresh "unused") 'void (app (toplevel-fn shutdown))))
           rest)]
    [`(,def . ,defs)
     (cons def (initialize-rts-iter defs))]

    [`()
     (error 'initialize-rts-iter "Can't find main.")]))
