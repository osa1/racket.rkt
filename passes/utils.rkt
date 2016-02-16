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

; TODO: Rewrite this as a fold
; Generates 64-bits
(define (bitfield-from-bit-idxs bit-idxs)
  (let iter ([acc 0] [bit-idxs bit-idxs])
    (match bit-idxs
      [`() acc]
      [`(,idx . ,idxs)
       (let ([mask (arithmetic-shift 1 idx)])
         (iter (bitwise-ior acc mask) idxs))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Find these guys a safe place

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
    [_ (unsupported-form 'arg-mem? arg)]))
