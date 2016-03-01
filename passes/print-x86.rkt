#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide print-x86_64)

(define (print-x86_64 pgm)
  (match pgm
    [`(program . ,defs)
     (let ([def-strs (map print-x86_64-def defs)])
       (string-join def-strs))]
    [_ (unsupported-form 'print-x86_64 pgm)]))

;; print main-prelude somewhere

(define (print-x86_64-def def)
  (match def
    [`(define ,tag : ,_ (,s) . ,stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)]
           [symbol (match tag
                     [`(,f . ,_) f]
                     [_ tag])])
       (string-append (format ".globl ~s~n" (encode-symbol symbol))
                      (format "~s:~n" (encode-symbol symbol))
                      (mk-def-prelude s)
                      "\n"
                      (string-join stmt-lines)
                      "\n"
                      (mk-def-conclusion s)))]

    [_ (unsupported-form 'printx86_64-def def)]))

(define (print-x86_64-stmt stmt)
  (match stmt
    [`(cmpq (int ,_) (int ,_))
     (error 'print-x86_64-stmt "~ncmpq has two literal arguments: ~s~n" stmt)]

    ;; Special case for cmpq: If one of the arguments is literal and the other
    ;; one is mem/reg, the literal one should come first.
    [`(cmpq (reg ,r) (int ,i))
     (print-x86_64-stmt `(cmpq (int ,i) (reg ,r)))]

    ;; Special case for callq: Put * before the argument if it's not a symbol.
    [`(callq (toplevel-fn ,f))
     (format instr2-format 'callq (encode-symbol f))]

    [`(callq ,arg)
     (format instr2-format 'callq (string-append "*" (print-x86_64-arg arg)))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'movzbq 'xorq) ,arg1 ,arg2)
     (format instr3-format (car stmt) (print-x86_64-arg arg1) (print-x86_64-arg arg2))]

    [`(,(or 'negq 'pushq 'popq 'je 'jmp 'sete 'setl) ,arg1)
     (format instr2-format (car stmt) (print-x86_64-arg arg1))]

    [`(label ,lbl)
     (string-append "\n" (symbol->string lbl) ":\n")]

    [`(retq) "\tret\n"]

    [_ (unsupported-form 'print-x86_64-stmt stmt)]))

(define (print-x86_64-arg arg)
  (match arg
    [`(int ,int) (format "$~s" int)]
    [`(reg ,reg) (format "%~s" reg)]
    [`(byte-reg ,reg) (format "%~s" reg)]
    [`(stack ,offset) (format "~s(%rbp)" offset)]
    [`(global-value ,var) (format "~s(%rip)" var)]
    [`(offset ,arg ,offset) (format "~s(~a)" offset (print-x86_64-arg arg))]
    [`(toplevel-fn ,f) (format "~s" (encode-symbol f))]
    [(? symbol?) arg] ; must be a function call or jmp
    [_ (unsupported-form 'print-x86_64-arg arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define enc-tbl
  `(("-" . "_")
    ("?" . "ha")
    ("->" . "_arr_")))

(define (encode-symbol sym)
  (string->symbol
    (foldl (lambda (enc str)
             (string-replace str (car enc) (cdr enc)))
           (symbol->string sym) enc-tbl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main-prelude
"\t.globl main
main:\n")

(define main-conclusion "\tretq")

(define (mk-def-prelude stack-size)
  (string-append
"\tpushq %rbp
\tmovq %rsp, %rbp\n"
(if (eq? stack-size 0) "" (format "\tsubq $~a, %rsp\n" stack-size))))

(define (mk-def-conclusion stack-size)
  (let [(ls2
"\tpopq %rbp\n
\tretq\n\n")]
    (if (eq? stack-size 0)
      ls2
      (string-append (format "\taddq $~a, %rsp\n" stack-size) ls2))))

(define instr3-format "\t~a ~a, ~a\n")
(define instr2-format "\t~a ~a\n")
