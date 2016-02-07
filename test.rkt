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

(define (typecheck-pgm file [should-fail? #f])
  (let [(pgm (read-program file))]
    (if (or (typecheck pgm) should-fail?)
      (begin (display file) (display " "))
      (error 'typecheck-pgm "type checking failed: ~a~n" file))))

(typecheck-pgm "tests/ty_1.rkt")
(typecheck-pgm "tests/ty_2.rkt")
(typecheck-pgm "tests/ty_3.rkt")
(typecheck-pgm "tests/ty_4.rkt")
(typecheck-pgm "tests/ty_5.rkt")
(typecheck-pgm "tests/ty_6.rkt" #t)
(typecheck-pgm "tests/ty_7.rkt" #t)
(typecheck-pgm "tests/ty_8.rkt" #t)
(typecheck-pgm "tests/ty_9.rkt" #t)
(typecheck-pgm "tests/ty_10.rkt" #t)

(define conditionals-passes
  `(("desugar" ,desugar ,interp-scheme)
    ("uniquify" ,uniquify ,interp-scheme)
    ("flatten" ,flatten ,interp-C)
    ("select-instructions" ,instr-sel ,interp-x86)
    ("assign-homes" ,assign-homes ,interp-x86)))

(interp-tests "conditionals" typecheck conditionals-passes interp-scheme "cond" (range 1 5))

(define (show-steps steps file)
  (let [(pgm (read-program file))]
    (printf "initial program:~n~a~n~n" pgm)
    (show-steps-iter steps pgm)))

(define (show-steps-iter steps pgm)
  (match steps
    ['() '()]
    [(list-rest `(,step-name ,step) steps)
     (printf "Running step ~s~n" step-name)
     (let [(step-ret (step pgm))]
       (pretty-print step-ret)
       (newline)
       (show-steps-iter steps step-ret))]))

(define steps
  `(("desugar" ,desugar)
    ("uniquify" ,uniquify)
    ("flatten" ,flatten)
    ("select-instructions" ,instr-sel)
    ("assign-homes" ,assign-homes)))

; (show-steps steps "tests/cond_1.rkt")
; (show-steps steps "tests/cond_2.rkt")
; (show-steps steps "tests/cond_3.rkt")
