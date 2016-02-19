#lang racket

(require "public/interp.rkt")
(require "public/utilities.rkt")

(require "compiler.rkt")
(require "settings.rkt")

;; Start running passes from the given pass
(define (start-from step passes)
  (cond [(null? passes)
         (error 'start-from "run out of passes")]
        [(equal? (caar passes) step)
         passes]
        [else
         (start-from step (cdr passes))]))

(define (run-all-tests)
  ; (interp-tests "first assignment" typecheck r1-passes interp-scheme "uniquify" (range 1 6))
  (compiler-tests "first assignment" typechecker r1-passes "uniquify" (range 1 6))

  ; (interp-tests "first assignment" typecheck r1-passes interp-scheme "flatten" (range 1 5))
  (compiler-tests "first assignment" typechecker r1-passes "flatten" (range 1 5))

  ; (interp-tests "select instructions" typecheck-ignore (start-from "instr-sel" r1-passes) interp-C
  ;               "select_instructions" (range 1 4))
  (compiler-tests "select instructions" typecheck-ignore (start-from "instr-sel" r1-passes)
                  "select_instructions" (range 1 4))


  ; (interp-tests "patch instructions" typecheck-ignore (start-from "patch-instructions" r1-passes)
  ;               interp-x86 "patch_instructions" (range 1 4))
  (compiler-tests "patch instructions" typecheck-ignore (start-from "patch-instructions" r1-passes)
                  "patch_instructions" (range 1 4))

  ; (interp-tests "r0" typecheck r1-passes interp-scheme "r0" (range 1 5))
  (compiler-tests "r0" typechecker r1-passes "r0" (range 1 5))

  ; (interp-tests "r1" typecheck r1-passes interp-scheme "r1" (range 1 22))
  (compiler-tests "r1" typechecker r1-passes "r1" (range 1 22))

  ; This fails because apparently x86 interpreter doesn't support pushq and popq
  ; (interp-tests "r1a" typecheck r1-passes interp-scheme "r1a" (range 1 9))
  (compiler-tests "r1a" typechecker r1-passes "r1a" (range 1 9))


  ; (interp-tests "forum" typecheck r1-passes interp-scheme "forum" (range 1 2))
  (compiler-tests "forum" typechecker r1-passes "forum" (range 1 2))


  ; Similarly fails because of pushq/popq
  ; (interp-tests "crazy" typecheck r1-passes interp-scheme "crazy" (range 1 3))
  (compiler-tests "crazy" typechecker r1-passes "crazy" (range 1 3))

  ; (interp-tests "ty" typechecker r1-passes interp-scheme "ty" (range 1 11))
  (compiler-tests "ty" typechecker r1-passes "ty" (range 1 12))

  ; (interp-tests "conditionals-1" typecheck r2-passes interp-scheme "cond" (range 1 5))
  (compiler-tests "conditionals-1" typechecker r1-passes "cond" (range 1 5))


  ; (interp-tests "conditionals-2" typecheck r1-passes interp-scheme "ty" (range 1 6))
  (compiler-tests "conditionals-2" typechecker r1-passes "ty" (range 1 6))

  ; (interp-tests "r2" typecheck r2-passes interp-scheme "r2" (range 1 24))
  (compiler-tests "r2" typechecker r2-passes "r2" (range 1 24))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vectors

  ; (debug-level 4)
  ; (interp-tests "vec" typecheck r3-passes interp-scheme "vector" (range 1 10))
  (compiler-tests "vec" typechecker r2-passes "vector" (range 1 10))

  ; It takes forever after vector-ref-crazy_20
  ; FIXME: Benchmark this. Could it be gcc's fault?
  (compiler-tests "vec-ref-crazy" typechecker r2-passes "vector-ref-crazy" (range 1 15))

  (compiler-tests "vector-other" typechecker r2-passes "vector-other" (range 1 2)))

(printf "================================================================================~n")
(printf "= Testing with default settings ================================================~n")
(printf "================================================================================~n")
(run-all-tests)
(newline)
(newline)

(printf "================================================================================~n")
(printf "= Testing with move relation disabled ==========================================~n")
(printf "================================================================================~n")
(use-move-rels #f)
(printf "use-move-rels: ~a~n" (use-move-rels))
(run-all-tests)
(newline)
(newline)

(printf "================================================================================~n")
(printf "= Testing with registers disabled ==============================================~n")
(printf "================================================================================~n")
(use-move-rels #f)
(use-regs #f)
(printf "use-move-rels: ~a~n" (use-move-rels))
(printf "use-regs ~a~n" (use-regs))
(run-all-tests)
(newline)
(newline)

(printf "================================================================================~n")
(printf "= Testing with 16 byte heap and root stack =====================================~n")
(printf "================================================================================~n")
(use-move-rels #t)
(use-regs #t)
(initial-heap-size 16)
(printf "use-move-rels: ~a~n" (use-move-rels))
(printf "use-regs ~a~n" (use-regs))
(printf "initial-heap-size ~a~n" (initial-heap-size))
(printf "initial-root-stack-size ~a~n" (initial-root-stack-size))
(run-all-tests)
(newline)
(newline)
