#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(provide print-x86_64)

(define (print-x86_64 pgm)
  (match pgm
    [`(program (,meta) . ,defs)
     (let ([def-strs (map print-x86_64-def defs)]
           [type-strs (print-type-sers meta)])
       (string-join (cons type-strs (append def-strs (list shims))) "\n\n"))]
    [_ (unsupported-form 'print-x86_64 pgm)]))

(define (print-type-sers table)
  (string-join
    (append-map
      (lambda (p)
        (define type (car p))
        (define sym (cadr p))
        (define bytes (cddr p))
        `(,(format "# ~a" type)
          ,(format "~s:" (encode-symbol sym))
          ,@(map (lambda (byte) (instr2 ".byte" (number->string byte))) bytes)))
      (hash->list table))
    "\n"))

(define (print-x86_64-def def)
  (match def
    [`(define ,tag : ,_ (,s) . ,stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)]
           [symbol (match tag
                     [`(,f . ,_) f]
                     [_ tag])])
       (string-join
         (filter non-empty-string?
                 (list (format "\t.globl ~s" (encode-symbol symbol))
                       (format "~s:" (encode-symbol symbol))
                       (mk-def-prelude s)
                       (string-join stmt-lines "\n")
                       (mk-def-conclusion s)))
         "\n"))]

    [`(define-closure-wrapper ,closure-name ,fname)
     (toplevel-closure closure-name fname)]

    [_ (unsupported-form 'print-x86_64-def def)]))

; FIXME: I'm using Integer type here just to generate the tag for a vector with
; single, non-pointer (function) field.
(define toplevel-closure-tag (vec-info-field '(Integer)))

; FIXME: Garbage collector copies these values around for no reason. Need to
; somehow mark these as "immovable"/"not-GCed".

(define (toplevel-closure closure-name fname)
  (define closure-name-enc (symbol->string (encode-symbol closure-name)))
  (string-join
    `(,(format "\t.globl ~a" closure-name-enc)
      ,(format "~a:" closure-name-enc)
      ,(instr2 ".quad" (number->string toplevel-closure-tag))
      ,(instr2 ".quad" (symbol->string (encode-symbol fname))))
    "\n"))

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

(define (encode-symbol sym) (encode-symbol-str (symbol->string sym)))

(define (encode-str str)
  (foldl (lambda (enc str)
           (string-replace str (car enc) (cdr enc)))
         str enc-tbl))

(define (encode-symbol-str str)
  (string->symbol (encode-str str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mk-def-prelude stack-size)
  (if (eq? stack-size 0)
    ""
    (instr3 'subq
            (print-x86_64-arg `(int ,stack-size))
            (print-x86_64-arg '(reg rsp)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define shims
  (string-join
    `("print_int_closure:"
      ,(instr2 ".quad" (number->string toplevel-closure-tag))
      ,(instr2 ".quad" "print_int")
      "read_int_closure:"
      ,(instr2 ".quad" (number->string toplevel-closure-tag))
      ,(instr2 ".quad" "read_int"))
    "\n"))
