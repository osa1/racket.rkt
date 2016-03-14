#lang racket

(require "utils.rkt")
(require (only-in "typecheck.rkt" extract-toplevel-name))

(provide closure-convert)

(define (closure-convert pgm)
  (match pgm
    [`(program . ,defs)
     (define closure-fns (make-hash))
     (let* ([toplevel-names (list->set (map extract-toplevel-name defs))]
            [defs (map (closure-convert-def toplevel-names closure-fns) defs)])
       `(program ,@(hash-values closure-fns) ,@(append-map mk-closure-wrapper defs) ,@defs))]
    [_ (unsupported-form 'closure-convert pgm)]))

; Define a closure from a top-level function. The closure lives in the
; top-level, so no allocation is needed when calling toplevel functions.
; FIXME: GC still copies these things around though.
(define (mk-closure-wrapper def)
  (match def
    [`(define (,fname . ,_) : ,_ ,_)
     `((define-closure-wrapper
         ,(string->symbol (string-append (symbol->string fname) "_closure"))
         ,fname))]
    [_ `()]))

(define (closure-convert-def toplevel-names closure-fns)

  (define (mk-lets free-lst closure-arg body [idx 1])
    (match free-lst
      [`() body]
      [`(,free . ,frees)
       `(let ([,(cdr free) (,(car free) . (vector-ref (Vector . ,closure-arg) ,idx))])
          ,(mk-lets frees closure-arg body (+ idx 1)))]))

  (define (closure-convert-expr e0)
    (match (cdr e0)
      [(or (? fixnum?) (? boolean?) (? symbol?) `(read))
       e0]

      [`(lambda: ,args : ,ret-ty ,body)
       (define args-set  (list->set (map (lambda (arg) (cons (caddr arg) (car arg))) args)))
       (define bounds    (set-union toplevel-names args-set))
       (define frees-lst (set->list (set-subtract (fvs body) bounds)))
       (define fname     (fresh "fn"))
       (define closure-arg (fresh "closure-arg"))

       (define toplevel-fn-body (mk-lets frees-lst closure-arg body))

       (define toplevel-fn
         `(define (,fname (,closure-arg : Vector) ,@args) : ,ret-ty
            ,toplevel-fn-body))

       (hash-set! closure-fns fname toplevel-fn)

       `((Vector ,(car e0) ,@(map car frees-lst))
         . (vector (,(car e0) . (toplevel-closure ,fname)) ,@frees-lst))]

      [`(,(or '- 'not) ,e1)
       `(,(car e0) . (,(cadr e0) ,(closure-convert-expr e1)))]

      [`(,(or '+ 'eq?) ,e1 ,e2)
       `(,(car e0) . (,(cadr e0) ,(closure-convert-expr e1) ,(closure-convert-expr e2)))]

      [`(if ,e1 ,e2 ,e3)
       `(,(car e0) . (if ,(closure-convert-expr e1)
                       ,(closure-convert-expr e2)
                       ,(closure-convert-expr e3)))]

      [`(let ([,var ,e1]) ,body)
       `(,(car e0) . (let ([,var ,(closure-convert-expr e1)])
                       ,(closure-convert-expr body)))]

      [`(vector-ref ,e1 ,idx)
       `(,(car e0) . (vector-ref ,(closure-convert-expr e1) ,idx))]

      [`(vector-set! ,vec ,idx ,e)
       `(,(car e0) . (vector-set! ,(closure-convert-expr vec) ,idx ,(closure-convert-expr e)))]

      [`(vector . ,elems)
       `(,(car e0) . (vector ,@(map closure-convert-expr elems)))]

      [`(,f . ,args)
       `(,(car e0) . (,(closure-convert-expr f) ,@(map closure-convert-expr args)))]

      [_ (unsupported-form 'closure-convert-expr (cdr e0))]))

  (lambda (def)
    (match def
      [`(define (,fname . ,args) : ,ret-ty ,expr)
       ; FIXME: We should probably leave top-level functions alone and handle
       ; this closure argument in toplevel-closure-wrappers.
       (define closure-arg (fresh "cls-unused"))
       `(define (,fname (,closure-arg : (Vector)) ,@args) : ,ret-ty ,(closure-convert-expr expr))]

      [`(define main : void ,expr)
       `(define main : void ,(closure-convert-expr expr))]

      [_ (unsupported-form 'closure-convert-def def)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: Need to remove top-level stuff from the result.
(define (fvs e0)
  (match (cdr e0)
    [(or (? fixnum?) (? boolean?) `(read)) (set)]

    [(? symbol?) (set e0)]

    [`(lambda: ,args : ,ret-ty ,body)
     (foldl (lambda (arg s) (set-remove s (car arg))) (fvs body) args)]

    [`(,(or '- 'not) ,e1) (fvs e1)]

    [`(,(or '+ 'eq?) ,e1 ,e2) (set-union (fvs e1) (fvs e2))]

    [`(if ,e1 ,e2 ,e3) (set-union (fvs e1) (fvs e2) (fvs e3))]

    [`(let ([,var ,e1]) ,body)
     (set-union (fvs e1) (set-remove (fvs body) var))]

    [`(vector-ref ,e1 ,_) (fvs e1)]

    [`(vector-set! ,vec ,_ ,e) (set-union (fvs vec) (fvs e))]

    [`(vector . ,elems) (foldl set-union (set) (map fvs elems))]

    [`(,f . ,args) (foldl set-union (set) (map fvs (cons f args)))]

    [_ (unsupported-form 'fvs (cdr e0))]))
