#lang racket

(require "public/utilities.rkt")
(require "public/interp.rkt")

; In the order that they run
(require "passes/typecheck.rkt")
(require "passes/desugar.rkt")
(require "passes/choose-branch.rkt")
(require "passes/uniquify.rkt")
(require "passes/reveal-functions.rkt")
(require "passes/flatten.rkt")
(require "passes/expose-allocations.rkt")
(require "passes/annotate-lives.rkt")
(require "passes/uncover-call-live-roots.rkt")
(require "passes/instr-sel.rkt")
(require "passes/initialize-rts.rkt")
(require "passes/reg-alloc/color.rkt")
(require "passes/elim-movs.rkt")
(require "passes/lower-conditionals.rkt")
(require "passes/print-x86.rkt")

; debugging
(require "passes/print-pgm.rkt")

(require (only-in "passes/utils.rkt" reset-fresh-counter-pass))

(provide r1-passes r2-passes r3-passes r4-passes
         ; export individual passes for testing purposes
         ; (see test.rkt)

         ;; type checking
         typechecker
         typecheck typecheck-ignore

         ;; scheme passes
         desugar choose-branch uniquify reveal-functions flatten

         ;; C passes
         initialize-rts expose-allocations annotate-lives uncover-call-live-roots instr-sel

         ;; asm passes
         reg-alloc elim-movs lower-conditionals

         print-x86_64)

(define r1-passes
  `(; Reset the fresh name generator counter before each compilation to get
    ; deterministic outputs when running batch compilations.
    ; FIXME: What happens if we use (fresh) in type checker?
    ("reset-fresh-counter" ,reset-fresh-counter-pass ,interp-scheme)

    ("desugar" ,desugar ,interp-scheme)

    ;; TODO: Think about the best place for this. One of the goals here is to
    ;; avoid generating illegal instructions for code like (if (eq? 1 1) _ _).
    ("choose-branch" ,choose-branch ,interp-scheme)

    ("uniquify" ,uniquify ,interp-scheme)
    ("reveal-functions" ,reveal-functions ,interp-scheme)
    ("flatten" ,flatten ,interp-C)
    ("initialize-rts" ,initialize-rts ,interp-x86)
    ("expose-allocations" ,expose-allocations ,interp-C)
    ("annotate-lives" ,annotate-lives ,interp-C)
    ("uncover-call-live-roots" ,uncover-call-live-roots ,interp-C)
    ("instr-sel" ,instr-sel ,interp-x86)
    ; ("print-pgm" ,(print-pgm "after instr-sel") ,interp-x86)
    ("reg-alloc" ,(reg-alloc "???") ,interp-x86)
    ; ("print-pgm" ,(print-pgm "after reg-alloc") ,interp-x86)
    ; ("print-pgm" ,(print-pgm "after patch-instructions") ,interp-x86)
    ("elim-movs" ,elim-movs ,interp-x86)
    ; ("print-pgm" ,(print-pgm "after elim-movs") ,interp-x86)
    ("lower-conditionals" ,lower-conditionals ,interp-x86)
    ; ("print-pgm" ,(print-pgm "after lower-conditionals") ,interp-x86)
    ("print-x86" ,print-x86_64 #f)))

; so much for the composable nanopass approach
(define r2-passes r1-passes)
(define r3-passes r1-passes)
(define r4-passes r1-passes)
