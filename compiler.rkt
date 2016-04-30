#lang racket

; In the order that they run
(require "passes/compile-r7.rkt")
(require "passes/typecheck.rkt")
(require "passes/desugar.rkt")
(require "passes/choose-branch.rkt")
(require "passes/uniquify.rkt")
(require "passes/elim-dyns.rkt")
(require "passes/partial-eval.rkt")
(require "passes/remove-unused-defs.rkt")
(require "passes/closure-convert.rkt")
(require "passes/reveal-functions.rkt")
(require "passes/flatten.rkt")
(require "passes/expose-allocations.rkt")
(require "passes/annotate-lives.rkt")
(require "passes/uncover-call-live-roots.rkt")
(require "passes/instr-sel.rkt")
(require "passes/initialize-rts.rkt")
(require "passes/reg-alloc/color.rkt")
(require "passes/lower-conditionals.rkt")
(require "passes/print-x86.rkt")

; debugging
(require "passes/print-pgm.rkt")

(require (only-in "passes/utils.rkt" reset-fresh-counter-pass))

(provide r1-passes r2-passes r3-passes r4-passes r5-passes r6-passes r7-passes

         ; export individual passes for testing purposes
         ; (see test.rkt)

         ; compiling the dynamically typed language to the statically typed one
         compile-r7

         ; type checking
         typechecker
         typecheck typecheck-ignore

         ; scheme passes
         desugar choose-branch uniquify elim-dyns peval rm-unused-defs reveal-functions flatten

         ; C passes
         initialize-rts expose-allocations annotate-lives uncover-call-live-roots instr-sel

         ; asm passes
         reg-alloc lower-conditionals

         print-x86_64

         ; settings
         do-peval print-peval)

(define do-peval (make-parameter #f))

; Print the program before and after partial evaluation.
; Only used when do-peval is #t.
(define print-peval (make-parameter #f))

(define (r1-passes)
  `(; Reset the fresh name generator counter before each compilation to get
    ; deterministic outputs when running batch compilations.
    ; FIXME: What happens if we use (fresh) in type checker?
    ("reset-fresh-counter" ,reset-fresh-counter-pass #f)
    ("desugar" ,desugar #f)
    ("choose-branch" ,choose-branch #f)
    ("uniquify" ,uniquify #f)
    ("elim-dyns" ,elim-dyns #f)

    ; ("print-pgm" ,(print-pgm-typeless "before remove-unused-defs") #f)
    ("remove-unused-defs" ,rm-unused-defs #f)
    ; ("print-pgm" ,(print-pgm-typeless "after remove-unused-defs") #f)

    ,@(if (do-peval)
        `(,@(if (print-peval)
              `(("print-pgm" ,(print-pgm-typeless "before partial-eval") #f))
              `())
          ("partial-eval" ,peval #f)
          ("elim-dyns" ,elim-dyns #f)
          ("remove-unused-defs" ,rm-unused-defs #f)
          ,@(if (print-peval)
              `(("print-pgm" ,(print-pgm-typeless "after partial-eval") #f))
              `())
          )
        `())

    ("closure-convert" ,closure-convert #f)
    ("reveal-functions" ,reveal-functions #f)
    ("flatten" ,flatten #f)
    ("initialize-rts" ,initialize-rts #f)
    ("expose-allocations" ,expose-allocations #f)
    ("annotate-lives" ,annotate-lives #f)                   ; <- uses expr-vs
    ("uncover-call-live-roots" ,uncover-call-live-roots #f) ; <- defines and uses expr-vs
    ("instr-sel" ,instr-sel #f)
    ; ("print-pgm" ,(print-pgm "after instr-sel") #f)
    ("reg-alloc" ,(reg-alloc "???") #f)
    ; ("print-pgm" ,(print-pgm "after reg-alloc") #f)
    ; ("print-pgm" ,(print-pgm "after patch-instructions") #f)
    ("lower-conditionals" ,lower-conditionals #f)
    ; ("print-pgm" ,(print-pgm "after lower-conditionals") #f)
    ("print-x86" ,print-x86_64 #f)))

; so much for the composable nanopass approach
(define r2-passes r1-passes)
(define r3-passes r1-passes)
(define r4-passes r1-passes)
(define r5-passes r1-passes)
(define r6-passes r1-passes)

; r7 is different - it's dynamically typed
(define (r7-passes)
  `(("compile-r7" ,compile-r7 #f)
    ; ("print-pgm" ,(print-pgm "after compile-r7") #f)
    ("typecheck" ,typecheck #f)
    ,@(r6-passes)))
