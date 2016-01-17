#lang racket

(require "public/interp.rkt")
(require "public/utilities.rkt")

(require "compiler.rkt")

(compiler-tests "first assignment"
				r1-passes
                "uniquify"
                (range 1 6))

(compiler-tests "first assignment"
				r1-passes
                "flatten"
                (range 1 5))

(compiler-tests "select instructions"
                `(("instr-sel" ,instr-sel ,interp-x86)
                  ("assign-homes" ,assign-homes ,interp-x86)
                  ("patch-instructions" ,patch-instructions ,interp-x86)
                  ("print-x86" ,print-x86_64 ,interp-x86))
                "select_instructions"
                (range 1 4))

(compiler-tests "patch instructions"
                `(("patch-instructions" ,patch-instructions ,interp-x86)
                  ("print-x86" ,print-x86_64 ,interp-x86))
                "patch_instructions"
                (range 1 4))
