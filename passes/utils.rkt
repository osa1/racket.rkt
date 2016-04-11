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

(define (replicate x n)
  (if (eq? n 0)
    '()
    (cons x (replicate x (- n 1)))))

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

(define (toplevel-def? def)
  (match def
    [`(define ,_ : ,_ . ,_)
     #t]
    [_ #f]))

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

(define (mk-reg r)
  (match r
    [`(reg ,_) (error 'mk-reg "Argument is already a register! ~a" r)]
    [(? symbol?) `(reg ,r)]
    [_ (unsupported-form 'mk-reg r)]))

(define (reg-sym r)
  (match r
    [`(reg ,r) r]
    [_ (unsupported-form 'reg-sym r)]))

(define arg-reg-syms `(rdi rsi rdx rcx r8 r9))
(define arg-regs (map mk-reg arg-reg-syms))

(define caller-save-syms '(rdx rcx rsi rdi r8 r9 r10 r11))
(define caller-save-regs (map mk-reg caller-save-syms))

(define callee-save-syms '(rbx r12 r13 r14 r15 rbp))
(define callee-save-regs (map mk-reg callee-save-syms))

(define all-reg-syms (list->set (append caller-save-syms callee-save-syms)))
(define all-regs-set (list->set (map mk-reg (append caller-save-syms callee-save-syms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-ptr-obj? obj-type)
  (match obj-type
    [`(Vector . ,_) #t]
    [`(Vectorof Any) #t]
    ['Any #t]
    [`(,_ ... -> ,_) #t]
    ['Integer #f]
    ['Boolean #f]
    [_ (unsupported-form 'is-ptr-obj? obj-type)]))

; Generating info fields for vectors
(define (vec-info-field field-types)

  ; Format:
  ;
  ;  Most significant bit                       Least significant bit
  ;  V                                                              V
  ; +----------------------------------------------------------------+ 64-bit
  ; |       ppppppppppppppppppppppppppppppppppppppppppppppppppllllllf|
  ; +----------------------------------------------------------------+
  ;
  ; f: Forwarding bit - 1 means not forwarding,
  ;                     0 means this whole thing is actually a pointer
  ; l: 6-bits, length
  ; p: 50-bits, pointer mask
  ; empty space: 7-bits, unused

  (define ptr-idxs
    (append-map (lambda (idx field-type)
                  (if (is-ptr-obj? field-type) `(,idx) '()))
                (range (length field-types)) field-types))

  ; TODO: We need to do some range checking here.

  (define length-bits (arithmetic-shift (length field-types) 1))
  (define bitfield (arithmetic-shift (bitfield-from-bit-idxs ptr-idxs) 7))
  (define obj-tag (bitwise-ior length-bits bitfield 1))

  obj-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode-type-bits type)

  ; So far we have two base types: Integer and Boolean.
  ; Two type constructors: Vector and (->).
  ;
  ; We pack things tightly for space efficiency. Least significant three bits:
  ;
  ;   000 -> Integer
  ;   001 -> Boolean
  ;   010 -> Vector
  ;   011 -> Function
  ;   100 -> Any
  ;   111 -> Vectorof Any
  ;
  ; If the type is Vector then next 5 bits give the length. If it's Function
  ; that next 5 bits give the arity. Then bytes that encode Vector fields or
  ; Function arguments follow. If type is a Function, after arguments the
  ; return type comes.

  (match type
    ['Integer `(0 0 0)]
    ['Boolean `(1 0 0)]
    ['Any     `(0 0 1)]
    ['(Vectorof Any) `(1 1 1)]
    [`(Vector . ,fields)
     (append `(0 1 0)
             (to-bit-list (length fields) 5)
             (append-map encode-type-bits fields))]
    [`(,tys ... -> ,ret-ty)
     (append `(1 1 0)
             (to-bit-list (length tys) 5)
             (append-map encode-type-bits (append tys (list ret-ty))))]
    [_ (unsupported-form 'encode-type-bits type)]))

; Returns a list of bytes.
(define (encode-type type)
  (bit-list-to-byte-list (encode-type-bits type)))

; NOTE: Least significant bit comes first in the returned list!
(define (to-bit-list int len)
  (when (>= int (expt 2 len))
    (error 'to-bit-list "Can't encode ~a in ~a bits" int len))

  (when (< int 0)
    (error 'to-bit-list "Can't encode negative number: ~a" int))

  (if (eq? int 0)
    (replicate 0 len)
    (cons
      (if (eq? (modulo int 2) 0) 0 1)
      (to-bit-list (arithmetic-shift int (- 1)) (- len 1)))))

; NOTE: Least significant bit should come first in the list! i.e. bits are reversed.
; Does not re-order bytes.
(define (bit-list-to-byte-list bit-list)

  (define (iter bytes byte current-bit-idx bits)
    (match bits
      [`() (reverse (cons byte bytes))]
      [`(,bit . ,bits)
       (cond [(eq? current-bit-idx 8)
              (iter (cons byte bytes) 0 0 (cons bit bits))]
             [(eq? bit 1)
              (iter bytes (bitwise-ior byte (arithmetic-shift 1 current-bit-idx))
                    (+ current-bit-idx 1) bits)]
             [#t
              (iter bytes byte (+ current-bit-idx 1) bits)])]))

  (iter '() 0 0 bit-list))

(define (byte-list-to-quadword-list byte-list)

  (define (iter quads quad current-byte-idx bytes)
    (match bytes
      [`() (reverse (cons quad quads))]
      [`(,byte . ,bytes)
       (cond [(eq? current-byte-idx 8)
              (iter (cons quad quads) 0 0 (cons byte bytes))]
             [#t
              (iter quads (bitwise-ior quad (arithmetic-shift byte (* 8 current-byte-idx)))
                    (+ current-byte-idx 1) bytes)])]))

  (iter '() 0 0 byte-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gensym is the worst thing ever. It's not deterministic, which means if
; gensym-generated symbols are used as map keys etc. they make iteration order
; non-deterministic and debugging impossible.

(define fresh-counter 0)

(define (reset-fresh-counter)
  (set! fresh-counter 0))

(define (reset-fresh-counter-pass pgm)
  (set! fresh-counter 0)
  pgm)

(define (fresh [prefix #f])
  (let ([next-fresh fresh-counter])
    (set! fresh-counter (+ next-fresh 1))
    (string->symbol
      (if prefix
        (string-append prefix "_fresh_" (number->string next-fresh))
        (string-append "fresh_" (number->string next-fresh))))))
