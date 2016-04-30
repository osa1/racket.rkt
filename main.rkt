#lang racket

(require "compiler.rkt")

(require (only-in "public/utilities.rkt" compile-file))
(require "settings.rkt")

(let ([files (current-command-line-arguments)])
  (for ([file files])
    (unless (file-exists? file)
      (error 'main "File does not exist: ~a" file)))

  (initial-heap-size 8)
  (use-regs #t)
  (do-peval #t)
  (print-peval #t)

  (for ([file files])
    (let ([path (string->path file)])
      (let-values ([(_1 name _2) (split-path path)])
    (let ([name (car (string-split (path->string name) "."))])
      (if ((compile-file typechecker (r3-passes)) file)
        (system (format "gcc -g runtime.o tests/~a.s -o ~a" name name))
        (printf "Compilation failed: ~a~n" file)))))))
