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

(provide r1-passes r2-passes
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

; Synonym for typecheck, this is required by the assignment but I don't like
; giving it a noun, as all other passes have verb names.
(define typechecker typecheck)

(define r1-passes
  `(("desugar" ,desugar ,interp-scheme)

    ;; TODO: Think about the best place for this. One of the goals here is to
    ;; avoid generating illegal instructions for code like (if (eq? 1 1) _ _).
    ("choose-branch" ,choose-branch ,interp-scheme)

    ("uniquify" ,uniquify ,interp-scheme)
    ("flatten" ,flatten ,interp-C)
    ("expose-allocations" ,expose-allocations ,interp-C)
    ("instr-sel" ,instr-sel ,interp-x86)
    ("assign-homes" ,assign-homes ,interp-x86)
    ("patch-instructions" ,patch-instructions ,interp-x86)
    ("elim-movs" ,elim-movs ,interp-x86)
    ("save-regs" ,save-regs ,interp-x86)
    ("lower-conditionals" ,lower-conditionals ,interp-x86)
    ("print-x86" ,print-x86_64 #f)))

(define r2-passes r1-passes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging stuff

(define (print-lives instrs livess)
  (for-each (lambda (instr lives)
              (printf "~a\t~a~n" instr lives)) instrs livess))

(define (print-lives-rkt path)
  (print-lives-x86-pgm path (instr-sel (flatten (uniquify (read-program path))))))

; This takes as input a file path of a pseudo-x86 (with variables) probal, and
; compiles it using register allocation etc. Also generates a .dot file for
; interference graph to the same path.
(define (print-lives-x86-pgm path pgm [regs general-registers])
  (let* [(lives (gen-live-afters pgm))
         (int-graph (build-interference-graph pgm lives))
         (move-rels (mk-move-relation pgm int-graph))]
    (let-values [((allocations last-stack-loc) (reg-alloc int-graph move-rels regs))]
      (print-lives (cddr pgm) lives)
      (printf "interference graph: ~s~n~n" int-graph)
      (newline)
      (print-dot int-graph (string-append path ".int.dot"))
      (print-dot move-rels (string-append path ".mov.dot"))
      (printf "allocations: ~s~n" allocations)
      (newline)
      (printf "move relations: ~s~n" move-rels)
      (newline))))

(define (print-lives-x86 path [regs general-registers])
  (let* [(pgm (read-program path))]
    (print-lives-x86-pgm path pgm regs)))

; (print-lives-rkt "tests/uniquify_5.rkt")
; (print-lives-rkt "tests/r0_1.rkt")

; (print-lives-x86 "tests/lives_1.rkt")
; (print-lives-x86 "tests/lives_1.rkt" (list))
; (print-lives-rkt "tests/flatten_3.rkt")
