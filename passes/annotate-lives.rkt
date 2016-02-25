#lang racket

(require "utils.rkt")
(require (only-in "uncover-call-live-roots.rkt" expr-vs))

(provide annotate-lives)

;; Annotates every statement with live variables after that statement.
;; Return value is thus a list of (statement . lives set).
(define (annotate-lives pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map annotate-lives-def defs))]
    [_ (unsupported-form 'annotate-lives pgm)]))

(define (annotate-lives-def def)
  (match def
    [`(define ,tag : ,ret-ty ,meta . ,stmts)
     (let-values ([(_ stmts) (annotate-lives-iter stmts)])
       `(define ,tag : ,ret-ty ,meta ,@stmts))]
    [_ (unsupported-form 'annotate-lives-def def)]))

(define (annotate-lives-iter stmts)
  (match stmts
    [`() (values (set) `())]
    [`(,stmt . ,stmts)
     (match stmt
       [`(assign ,var ,expr)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-union (set-add lives var) (expr-vs expr))])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(return ,expr)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-union lives (expr-vs expr))])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(collect ,_)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (values lives (cons (cons stmt lives) stmts)))]

       [`(vector-set! ,var ,_ ,val)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-union (set-add lives var) (expr-vs val))])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(if ,c ,pgm-t ,pgm-f)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)]
                     [(pgm-t-lives pgm-t) (annotate-lives-iter pgm-t)]
                     [(pgm-f-lives pgm-f) (annotate-lives-iter pgm-f)])
          (let* ([if-lives (set-union lives (expr-vs c) pgm-t-lives pgm-f-lives)]
                 [pgm-t (map (lambda (s) (cons (car s)
                                               (set-union lives (cdr s)))) pgm-t)]
                 [pgm-f (map (lambda (s) (cons (car s)
                                               (set-union lives (cdr s)))) pgm-f)])
            (values if-lives (cons (cons `(if ,c ,pgm-t ,pgm-f) if-lives) stmts))))]

       [_ (unsupported-form 'annotate-lives-iter stmt)])]))
