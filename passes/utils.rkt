#lang racket

(provide (all-defined-out))

(define (not-null? e) (not (null? e)))

(define (filter-nulls lst) (filter not-null? lst))

(define (id x) x)

(define (unsupported-form fname form)
  (error fname "Unsupported form: ~s~n" form))

(define (unzip lst)
  (match lst
    [`() (values '() '())]
    [(list-rest (cons x y) rest)
     (let-values ([(xs ys) (unzip rest)])
       (values (cons x xs) (cons y ys)))]
    [_ (unsupported-form 'unzip lst)]))

(define (map-unzip f lst)
  (match lst
    [(list) (values '() '())]
    [(list-rest x xs)
     (let-values ([(vals1 vals2) (map-unzip f xs)]
                  [(val1 val2) (f x)])
       (values (cons val1 vals1) (cons val2 vals2)))]))

(define (bitfield-from-bit-idxs bit-idxs)
  (foldl (lambda (bit-idx acc)
           (bitwise-ior acc (arithmetic-shift 1 bit-idx)))
         0 bit-idxs))

(define (split-last lst)
  (match lst
    [`()
     (error 'split-last "Empty list")]
    [`(,x)
     (values '() x)]
    [`(,x . ,xs)
     (let-values ([(xs last) (split-last xs)])
       (values (cons x xs) last))]))

(define (split-at-max lst n)
  (if (<= n (length lst))
    (split-at lst n)
    (values lst '())))

(define (min-by fn lst)

  (define (iter lst min-so-far min-value)
    (match lst
      [`() min-so-far]
      [`(,h . ,t)
       (let ([val (fn h)])
         (if (< val min-value)
           (iter t h val)
           (iter t min-so-far min-value)))]))

  (iter (cdr lst) (car lst) (fn (car lst))))

(define (max-by fn lst)

  (define (iter lst max-so-far max-value)
    (match lst
      [`() max-so-far]
      [`(,h . ,t)
       (let ([val (fn h)])
         (if (> val max-value)
           (iter t h val)
           (iter t max-so-far max-value)))]))

  (iter (cdr lst) (car lst) (fn (car lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lift-def fn)
  (lambda (def)
    (match def
      [`(define ,name : ,ret-ty ,expr)
       `(define ,name : ,ret-ty ,(fn expr))]
      [_ (unsupported-form 'lift-def def)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (any p lst)
  (match lst
    [`() #f]
    [`(,h . ,t)
     (if (p h)
       #t
       (any p t))]))

(define (all p lst)
  (match lst
    [`() #t]
    [`(,h . ,t)
     (if (p h)
       (any p t)
       #f)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find these guys a safe place

(define (arg-imm? arg)
  (match arg
    [`(int ,_) #t]
    [`(,(or 'stack 'reg 'global-value 'offset 'var) ,_) #f]
    [_ (unsupported-form 'arg-imm? arg)]))

(define (arg-mem? arg)
  (match arg
    [`(stack ,_) #t]
    [`(,(or 'reg 'int) ,_) #f]
    [`(global-value ,_) #t]
    [`(offset ,_ ,_) #t]
    [`(toplevel-fn ,_) #f]
    [_ (unsupported-form 'arg-mem? arg)]))

(define (is-reg? arg)
  (match arg
    [`(reg ,_) #t]
    [_         #f]))

(define (not-reg? arg) (not (is-reg? arg)))

(define arg-reg-syms `(rdi rsi rdx rcx r8 r9))
(define arg-regs (map (lambda (reg) `(reg ,reg)) arg-reg-syms))

(define caller-save '(rdx rcx rsi rdi r8 r9 r10 r11))
(define callee-save '(rbx r12 r13 r14 r15 rbp))

(define all-reg-syms (list->set (append caller-save callee-save)))
(define all-regs (list->set (map (lambda (s) `(reg ,s)) (append caller-save callee-save))))
