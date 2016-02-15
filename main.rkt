#lang racket

(require "compiler.rkt")

(require "public/utilities.rkt")

(let ([files (current-command-line-arguments)])
  (for ([file files])
    (unless (file-exists? file)
      (error 'main "File does not exist: ~a" file)))

  (for ([file files])
    (let ([path (string->path file)])
      (let-values ([(_1 name _2) (split-path path)])
	(let ([name (car (string-split (path->string name) "."))])
	  ((compile-file typechecker r3-passes) file)
	  (system (format "gcc -g runtime.o tests/~a.s -o ~a" name name)))))))
