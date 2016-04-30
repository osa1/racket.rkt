#lang racket

(require "compiler.rkt")
(require "settings.rkt")

(define (compiler-tests name typechecker passes test-family test-nums)
  (define compiler (compile-file typechecker passes))
  (for ([test-number (in-list test-nums)])
    (define test-name  (format "~a_~a" test-family test-number))
    (define type-error-expected (file-exists? (format "tests/~a.tyerr" test-name)))
    (define compiler-ret (compiler (format "tests/~a.rkt" test-name)))

    (if compiler-ret
      (when type-error-expected
        (error (format "test ~a failed, unexpected type error" test-name)))
      (unless type-error-expected
        (error (format "test ~a passed typechecking but should not have." test-name))))

    (when compiler-ret
      ; Compile the file
      (match-let
        ([`(,stdout ,stdin ,pid ,stderr ,proc)
          (process (format "gcc -g runtime.o tests/~a.s" test-name))])

        (proc 'wait)
        (define ret (proc 'status))

        (define stdout-str (port->string stdout))
        (define stderr-str (port->string stderr))

        (close-input-port stdout)
        (close-input-port stderr)
        (close-output-port stdin)

        (match ret
          ['done-ok
           (void)]
          ['running
           (error "bug -- apparently (proc 'wait) didn't really wait")]
          ['done-error
           (error (format "gcc failed.~nstdout: ~a~nstderr: ~a~n" stdout-str stderr-str))]))

      ; Run the compiled program
      (let* ([input (if (file-exists? (format "tests/~a.in" test-name))
                      (format " < tests/~a.in" test-name)
                      "")]

             ; I now hate the number 42
             [expected-output
               (if (file-exists? (format "tests/~a.res" test-name))
                 (call-with-input-file
                   (format "tests/~a.res" test-name)
                   (lambda (f) (read-line f)))
                 "42")])

        (match-define `(,stdout ,stdin ,_ ,stderr ,proc) (process (format "./a.out~a" input)))

        (proc 'wait)
        (define ret (proc 'status))
        (define stdout-str (port->string stdout))
        (define stderr-str (port->string stderr))

        (close-input-port stdout)
        (close-input-port stderr)
        (close-output-port stdin)

        (if (eq? ret 'done-ok)

          (if (equal? stdout-str expected-output)
            (begin (display test-name) (display " ") (flush-output))
            (error (format "test ~a failed, output: ~a, expected ~a"
                           test-name stdout-str expected-output)))

          (error
            (format "test ~a error in x86 execution, exit code: ~a"
                    test-name (proc 'exit-code))))))))

(define (run-all-tests)
  (compiler-tests "spill" typechecker (r1-passes) "spill" (range 1 16))
  (compiler-tests "first assignment" typechecker (r1-passes) "uniquify" (range 0 6))
  (compiler-tests "first assignment" typechecker (r1-passes) "flatten" (range 1 5))
  (compiler-tests "r0" typechecker (r1-passes) "r0" (range 1 5))
  (compiler-tests "r1" typechecker (r1-passes) "r1" (range 1 22))
  (compiler-tests "r1a" typechecker (r1-passes) "r1a" (range 1 9))
  (compiler-tests "forum" typechecker (r1-passes) "forum" (range 1 2))
  (compiler-tests "crazy" typechecker (r1-passes) "crazy" (range 1 3))
  (compiler-tests "ty" typechecker (r1-passes) "ty" (range 1 12))
  (compiler-tests "conditionals-1" typechecker (r1-passes) "cond" (range 1 5))
  (compiler-tests "conditionals-2" typechecker (r1-passes) "ty" (range 1 6))
  (compiler-tests "r2" typechecker (r2-passes) "r2" (range 1 24))
  (compiler-tests "vector-nested" typechecker (r2-passes) "vector-nested" (range 0 1))
  (compiler-tests "vector" typechecker (r2-passes) "vector" (range 0 10))
  (compiler-tests "vec-ref-crazy" typechecker (r2-passes) "vector-ref-crazy" (range 1 15))
  (compiler-tests "vector-other" typechecker (r2-passes) "vector-other" (range 1 2))
  (compiler-tests "r3" typechecker (r2-passes) "r3" (range 1 16))
  (compiler-tests "fun" typechecker (r2-passes) "fun" (range 1 13))
  (compiler-tests "r4" typechecker (r2-passes) "r4" (range 1 20))
  (compiler-tests "alloc_fun" typechecker (r2-passes) "alloc_fun" (range 1 5))
  (compiler-tests "big-arity" typechecker (r2-passes) "big_arity" (range 1 3))
  (compiler-tests "lambda" typechecker (r2-passes) "lambda" (range 1 6))
  (compiler-tests "r5" typechecker (r2-passes) "r5" (range 1 13))
  (compiler-tests "relations" typechecker (r2-passes) "relations" (range 1 8))
  (compiler-tests "inject-project" typechecker (r2-passes) "inject-project" (range 1 19))
  (compiler-tests "unsafe" typechecker (r6-passes) "unsafe" (range 1 3))
  (compiler-tests "peval-bug" typechecker (r6-passes) "peval-bug" (range 1 2))
  (compiler-tests "dynamic" #f (r7-passes) "dynamic" (range 1 16))
  (compiler-tests "r7" #f (r7-passes) "r7" (range 0 12))
  (compiler-tests "pe" typechecker (r6-passes) "pe" (range 1 7)))

(printf "================================================================================~n")
(printf "= Testing with default settings ================================================~n")
(printf "================================================================================~n")
(run-all-tests)
(newline)
(newline)

(printf "================================================================================~n")
(printf "= Testing with 8-byte initial heap =============================================~n")
(printf "================================================================================~n")
(initial-heap-size 8)
(run-all-tests)
(newline)
(newline)

(printf "================================================================================~n")
(printf "= Testing with partial evaluation (heap size 8-byte) ===========================~n")
(printf "================================================================================~n")
(do-peval #t)
(initial-heap-size 8)
(run-all-tests)
(newline)
(newline)
