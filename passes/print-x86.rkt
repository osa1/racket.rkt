#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide print-x86_64)

(define (print-x86_64 pgm)
  (match pgm
    [`(program . ,defs)
     (let ([def-strs (map print-x86_64-def defs)])
       (string-join def-strs "\n\n"))]
    [_ (unsupported-form 'print-x86_64 pgm)]))

(define (print-x86_64-def def)
  (match def
    [`(define ,tag : ,_ (,s) . ,stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)]
           [symbol (match tag
                     [`(,f . ,_) f]
                     [_ tag])])
       (string-append (format "\t.globl ~s~n" (encode-symbol symbol))
                      (format "~s:~n" (encode-symbol symbol))
                      (mk-def-prelude s)
                      (string-join stmt-lines "\n")
                      "\n"
                      (mk-def-conclusion s)))]

    [_ (unsupported-form 'print-x86_64-def def)]))

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
     (instr2 'callq (encode-symbol f))]

    [`(callq ,arg)
     (instr2 'callq (string-append "*" (print-x86_64-arg arg)))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'movzbq 'xorq) ,arg1 ,arg2)
     (instr3 (car stmt) (print-x86_64-arg arg1) (print-x86_64-arg arg2))]

    [`(,(or 'negq 'pushq 'popq 'je 'jmp 'sete 'setl) ,arg1)
     (instr2 (car stmt) (print-x86_64-arg arg1))]

    [`(label ,lbl)
     (string-append (symbol->string lbl) ":")]

    [`(retq) (instr1 (car stmt))]

    [_ (unsupported-form 'print-x86_64-stmt stmt)]))

(define (print-x86_64-arg arg)
  (match arg
    [`(int ,int) (format "$~s" int)]
    [`(reg ,reg) (format "%~s" reg)]
    [`(byte-reg ,reg) (format "%~s" reg)]
    [`(stack ,offset) (format "~s(%rsp)" offset)]
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

(define (mk-def-prelude stack-size)
  (if (eq? stack-size 0)
    ""
    (string-append
      (instr3 'subq
              (print-x86_64-arg `(int ,stack-size))
              (print-x86_64-arg '(reg rsp)))
      "\n")))

(define (mk-def-conclusion stack-size)
  (if (eq? stack-size 0)
    (instr1 'retq)
    (string-join `(,(instr3 'addq
                            (print-x86_64-arg `(int ,stack-size))
                            (print-x86_64-arg `(reg rsp)))
                   ,(instr1 'retq)) "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define instr1-format "\t~a")
(define instr2-format "\t~a ~a")
(define instr3-format "\t~a ~a, ~a")

(define (instr1 instr) (format instr1-format instr))
(define (instr2 instr arg1) (format instr2-format instr arg1))
(define (instr3 instr arg1 arg2) (format instr3-format instr arg1 arg2))
