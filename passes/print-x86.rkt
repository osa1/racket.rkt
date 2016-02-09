#lang racket

(require "utils.rkt")

(provide print-x86_64)

(define main-prelude
"\t.globl main
main:\n")

(define main-conclusion "\tretq")

(define (mk-pgm-prelude stack-size)
  (let [(ls
"\tpushq %rbp
\tmovq %rsp, %rbp\n")]
    (if (eq? stack-size 0)
      ls
      (string-append ls (format "\tsubq $~a, %rsp\n" stack-size)))))

(define (mk-pgm-conclusion stack-size)
  (let [(ls1
"\tmovq %rax, %rdi
\tcallq print_int\n")
        (ls2
"\tmovq $0, %rax
\tpopq %rbp\n")]
    (if (eq? stack-size 0)
      (string-append ls1 "\n" ls2)
      (string-append ls1 "\n" (format "\taddq $~a, %rsp\n" stack-size) ls2))))

(define (print-x86_64 pgm)
  (match pgm
    [(list-rest 'program `(,s) stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)])
       (string-append main-prelude
                      (mk-pgm-prelude s)
                      "\n"
                      (string-join stmt-lines)
                      "\n"
                      (mk-pgm-conclusion s)
                      main-conclusion))]

    [_ (unsupported-form 'printx86_64 pgm)]))

(define instr3-format "\t~a ~a, ~a\n")
(define instr2-format "\t~a ~a\n")

(define (print-x86_64-stmt stmt)
  (match stmt
    [`(cmpq (int ,_) (int ,_))
     (error 'print-x86_64-stmt "~ncmpq has two literal arguments: ~s~n" stmt)]

    ;; Special case for cmpq: If one of the arguments is literal and the other
    ;; one is mem/reg, the literal one should come first.
    [`(cmpq (reg ,r) (int ,i))
     (print-x86_64-stmt `(cmpq (int ,i) (reg ,r)))]

    [`(,(or 'addq 'subq 'movq 'cmpq 'movzbq 'xorq) ,arg1 ,arg2)
     (format instr3-format (car stmt) (print-x86_64-arg arg1) (print-x86_64-arg arg2))]

    [`(,(or 'negq 'pushq 'popq 'callq 'je 'jmp 'sete) ,arg1)
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
    [(? symbol?) arg] ; must be a function call or jmp
    [_ (unsupported-form 'print-x86_64-arg arg)]))
