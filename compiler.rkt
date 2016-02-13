#lang racket

(require "public/utilities.rkt")
(require "public/interp.rkt")

; In the order that they run
(require "passes/typecheck.rkt")
(require "passes/desugar.rkt")
(require "passes/choose-branch.rkt")
(require "passes/uniquify.rkt")
(require "passes/flatten.rkt")
(require "passes/expose-allocations.rkt")
(require "passes/instr-sel.rkt")
(require "passes/reg-alloc.rkt")
(require "passes/patch-instructions.rkt")
(require "passes/elim-movs.rkt")
(require "passes/save-regs.rkt")
(require "passes/lower-conditionals.rkt")
(require "passes/print-x86.rkt")

(provide r1-passes r2-passes r3-passes
         ; export individual passes for testing purposes
         ; (see test.rkt)

         ;; type checking
         typechecker
         typecheck typecheck-ignore

         ;; scheme passes
         desugar choose-branch uniquify flatten

         ;; C passes
         expose-allocations instr-sel

         ;; asm passes
         assign-homes patch-instructions elim-movs save-regs lower-conditionals

         print-x86_64)

(define r1-passes
  `(; typecheck is a compiler pass now, as it's now annotating programs with
    ; type information. Unfortunately this also means none of the interpreters
    ; work as they don't expect expressions with type annotations.
    ("typecheck" ,typecheck ,interp-scheme)
    ("desugar" ,desugar ,interp-scheme)

    ;; TODO: Think about the best place for this. One of the goals here is to
    ;; avoid generating illegal instructions for code like (if (eq? 1 1) _ _).
    ("choose-branch" ,choose-branch ,interp-scheme)

    ("uniquify" ,uniquify ,interp-scheme)
    ("flatten" ,flatten ,interp-C)

    ;; NOTE: This is where we remove type annotations. Interpreters could be
    ;; used for testing purposes in the rest of the passes.
    ("expose-allocations" ,expose-allocations ,interp-C)
    ("instr-sel" ,instr-sel ,interp-x86)
    ("assign-homes" ,assign-homes ,interp-x86)
    ("patch-instructions" ,patch-instructions ,interp-x86)
    ("elim-movs" ,elim-movs ,interp-x86)
    ("save-regs" ,save-regs ,interp-x86)
    ("lower-conditionals" ,lower-conditionals ,interp-x86)
    ("print-x86" ,print-x86_64 #f)))

; so much for the composable nanopass approach
(define r2-passes r1-passes)
(define r3-passes r1-passes)
