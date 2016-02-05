#lang racket

(require "public/interp.rkt")
(require "public/utilities.rkt")

(require "compiler.rkt")

(compiler-tests "first assignment" typecheck
                r1-passes
                "uniquify"
                (range 1 6))

(compiler-tests "first assignment" typecheck
                r1-passes
                "flatten"
                (range 1 5))

(compiler-tests "select instructions" typecheck-ignore
                `(("instr-sel" ,instr-sel ,interp-x86)
                  ("assign-homes" ,assign-homes ,interp-x86)
                  ("patch-instructions" ,patch-instructions ,interp-x86)
                  ("print-x86" ,print-x86_64 ,interp-x86))
                "select_instructions"
                (range 1 4))

(compiler-tests "patch instructions" typecheck-ignore
                `(("patch-instructions" ,patch-instructions ,interp-x86)
                  ("print-x86" ,print-x86_64 ,interp-x86))
                "patch_instructions"
                (range 1 4))

(compiler-tests "r0" typecheck
                r1-passes
                "r0"
                (range 1 5))

(compiler-tests "r1" typecheck r1-passes "r1" (range 1 22))
(compiler-tests "r1a" typecheck r1-passes "r1a" (range 1 9))

(compiler-tests "forum" typecheck r1-passes "forum" (range 1 2))

(compiler-tests "crazy" typecheck r1-passes "crazy" (range 1 3))
