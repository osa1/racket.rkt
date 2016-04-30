#lang racket

(require "compiler.rkt")
(require (only-in "passes/utils.rkt" unsupported-form))

(require "settings.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string or path in ~s" path))

  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))

  (call-with-input-file
    path
    (lambda (f)
      `(program . ,(for/list ([e (in-port read f)]) e)))))

(define (run-passes passes pgm)
  (match passes
    [`() pgm]
    [`((,_ ,pass ,_) . ,ps)
     (run-passes ps (pass pgm))]
    [_ (unsupported-form 'run-passes passes)]))

(define (compile-file typechecker passes)
  (lambda (prog-file-name)
    (define file-base (string-trim prog-file-name ".rkt"))
    (define out-file-name (string-append file-base ".s"))
    (call-with-output-file
      out-file-name
      #:exists 'replace
      (lambda (out-file)
        (define sexp (read-program prog-file-name))
        (define tsexp (typechecker sexp))
        (if tsexp
          (let ([x86 (run-passes passes tsexp)])
            (cond
              [(string? x86)
               (write-string x86 out-file)
               (newline out-file)
               #t]
              [else
                (unsupported-form 'compile-file x86)]))
          #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry

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
