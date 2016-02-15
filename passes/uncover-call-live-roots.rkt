#lang racket

(require "utils.rkt")

(provide uncover-call-live-roots)

(define (uncover-call-live-roots pgm)
  (match pgm
    [`(program ,vs . ,stmts)
     ;; We discard the symbol table (symbol -> type) at this point
     `(program ,(hash-keys vs) ,@(uncover-call-live-roots-iter vs (annotate-lives stmts)))]

    [_ (unsupported-form 'uncover-call-live-roots pgm)]))

;; Annotates every statement with live variables after that statement.
;; Return value is thus a list of (statement . lives set).
(define (annotate-lives stmts)
  (let-values ([(_ stmts) (annotate-lives-iter stmts)])
    stmts))

(define (annotate-lives-iter stmts)
  (match stmts
    [`() (values (set) `())]
    [`(,stmt . ,stmts)
     (match stmt
       [`(assign ,var ,expr)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-union (set-add lives var) (expr-lives expr))])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(return ,expr)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-union lives (expr-lives expr))])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(collect ,_)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (values lives (cons (cons stmt lives) stmts)))]

       [`(vector-set! ,var ,_ ,_)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)])
          (let ([lives (set-add lives var)])
            (values lives (cons (cons stmt lives) stmts))))]

       [`(if ,c ,pgm-t ,pgm-f)
        (let-values ([(lives stmts) (annotate-lives-iter stmts)]
                     [(pgm-t-lives pgm-t) (annotate-lives-iter pgm-t)]
                     [(pgm-f-lives pgm-f) (annotate-lives-iter pgm-f)])
          (let* ([if-lives (set-union lives (expr-lives c) pgm-t-lives pgm-f-lives)]
                 [pgm-t (map (lambda (s) (cons (car s)
                                               (set-union lives (cdr s)))) pgm-t)]
                 [pgm-f (map (lambda (s) (cons (car s)
                                               (set-union lives (cdr s)))) pgm-f)])
            (values if-lives (cons (cons `(if ,c ,pgm-t ,pgm-f) if-lives) stmts))))]

       [_ (unsupported-form 'annotate-lives-iter stmt)])]))

(define (expr-lives expr)
  (match expr
    [`(,(or '+ 'eq? 'vector-ref) ,v1 ,v2) (add-live (set) v1 v2)]
    [`(,(or '- 'not 'allocate 'collection-needed?) ,v1) (add-live (set) v1 )]
    [`(read) (set)]
    [(? symbol?) (set expr)]
    [(or (? fixnum?) (? boolean?)) (set)]
    [_ (unsupported-form 'expr-lives expr)]))

(define (add-live lives . args)
  (foldl (lambda (arg lives) (if (symbol? arg) (set-add lives arg) lives))
         lives args))

(define (uncover-call-live-roots-iter vars stmts)
  (match stmts
    [`() `()]
    [`((,stmt . ,stmt-lives) . ,stmts)
     (match stmt
       [(or `(assign ,_ ,_) `(return ,_) `(vector-set! ,_ ,_ ,_))
        (cons stmt (uncover-call-live-roots-iter vars stmts))]

       [`(if ,expr ,pgm-t ,pgm-f)
        (cons `(if ,expr ,(uncover-call-live-roots-iter vars pgm-t)
                         ,(uncover-call-live-roots-iter vars pgm-f))
              (uncover-call-live-roots-iter vars stmts))]

       [`(collect ,bytes)
        (cons `(call-live-roots ,(filter-allocateds vars (set->list stmt-lives)) (collect ,bytes))
              (uncover-call-live-roots-iter vars stmts))]

       [_ (unsupported-form 'uncover-call-live-roots-iter stmt)])]))

(define (filter-allocateds var-tys vs)
  (filter (lambda (v)
            (match (hash-ref var-tys v)
              [`(vector . ,_) #t]
              [_ #f])) vs))
