#lang racket

(require "utils.rkt")

(provide uncover-call-live-roots expr-vs)

(define (uncover-call-live-roots pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map uncover-roots-def defs))]
     ; `(program ,(hash-keys vs) ,@(uncover-call-live-roots-iter vs (set) stmts))]

    [_ (unsupported-form 'uncover-call-live-roots pgm)]))

(define (uncover-roots-def def)
  (match def
    [`(define ,tag : ,ret-ty ,vs . ,stmts)
     `(define ,tag : ,ret-ty ,@(uncover-call-live-roots-iter vs (set) stmts))]
    [`(define-closure-wrapper . ,_) def]
    [_ (unsupported-form 'uncover-roots-def def)]))

(define (uncover-call-live-roots-iter vs mentioned-so-far stmts)
  (match stmts
    [`() `()]
    [`((,stmt . ,stmt-lives) . ,stmts)
     (match stmt

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       [`(collect ,_)
        (let* ([live-roots
                 (filter-allocateds vs (set->list (set-intersect mentioned-so-far stmt-lives)))])
          (cons `(call-live-roots ,live-roots ,stmt)
                (uncover-call-live-roots-iter vs mentioned-so-far stmts)))]

       ; Since the callee can run collect(), we need to push our roots to the
       ; stack, and pull on function return. This adds huge function call
       ; overhead, but alternatives I know all need significant effort to
       ; implement.
       ;
       ; One interesting idea that still keeps generated code simple is to do
       ; some static analysis (k-CFA kind of things) to see if the callee
       ; allocates.
       [`(assign ,_ (app . ,_))
        (let* ([live-roots
                 (filter-allocateds vs (set->list (set-intersect mentioned-so-far stmt-lives)))])
          (cons `(call-live-roots ,live-roots ,stmt)
                (uncover-call-live-roots-iter vs (set-union mentioned-so-far (stmt-vs stmt)) stmts)))]

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       [`(assign ,_ ,_)
        (cons stmt (uncover-call-live-roots-iter
                     vs
                     (set-union mentioned-so-far (stmt-vs stmt))
                     stmts))]

       [`(return ,v)
        (cons stmt (uncover-call-live-roots-iter vs (set-add mentioned-so-far v) stmts))]

       [`(vector-set! ,_ ,_ ,_)
        (cons stmt (uncover-call-live-roots-iter
                     vs
                     (set-union mentioned-so-far (stmt-vs stmt))
                     stmts))]

       [`(if ,c ,pgm-t ,pgm-f)
        (let* ([mentioned-so-far (set-union mentioned-so-far (expr-vs c))]
               [pgm-t-mentioneds (map stmt-vs pgm-t)]
               [pgm-f-mentioneds (map stmt-vs pgm-f)]
               [pgm-t (uncover-call-live-roots-iter vs mentioned-so-far pgm-t)]
               [pgm-f (uncover-call-live-roots-iter vs mentioned-so-far pgm-f)]
               ; FIXME: we have an exponential behavior here (nested ifs)
               [mentioned-rest
                 (foldl set-union (set)
                        (cons mentioned-so-far
                              (append pgm-t-mentioneds pgm-f-mentioneds)))])
          (cons `(if ,c ,pgm-t ,pgm-f)
                (uncover-call-live-roots-iter vs mentioned-rest stmts)))]

       [_ (unsupported-form 'uncover-call-live-roots-iter stmt)])]))

(define (stmt-vs stmt)
  (match stmt
    ; We handle statements with annotations too. This is because we use this
    ; function in here and in annotate-lives, so it needs to work on two
    ; different ASTs.
    [`(,stmt . ,lives)
     #:when (set? lives)
     (stmt-vs stmt)]

    [`(assign ,var ,expr)
     (set-add (expr-vs expr) var)]
    [`(return ,var) (set var)]
    [`(collect ,_) (set)]
    [`(vector-set! ,var ,_ ,val) (add-live (set) var val)]
    [`(if ,c ,pgm-t ,pgm-f)
     (foldl set-union (set) (append (map stmt-vs pgm-t) (map stmt-vs pgm-f) (list (expr-vs c))))]
    [_ (unsupported-form 'stmt-vs stmt)]))

(define (expr-vs expr)
  (match expr
    ;; TODO: I'm a bit confused about this. Do we really need a special case
    ;; here?
    [`(app ,v . ,vs) (foldl (lambda (v s) (add-live s v)) (set) (cons v vs))]
    [`(toplevel-fn ,_) (set)]

    [`(,(or '+ 'eq? 'vector-ref) ,v1 ,v2) (add-live (set) v1 v2)]
    [`(,(or '- 'not 'allocate 'collection-needed? 'procedure? 'vector? 'boolean? 'integer?) ,v1)
     (add-live (set) v1)]
    [`(project ,v1 ,_) (add-live (set) v1)]
    [`(vector-set! ,vec ,_ ,val) (add-live (set) vec val)]
    [`(read) (set)]
    [(? symbol?) (set expr)]
    [(or (? fixnum?) (? boolean?)) (set)]
    [_ (unsupported-form 'expr-vs expr)]))

(define (add-live lives . args)
  (foldl (lambda (arg lives) (if (symbol? arg) (set-add lives arg) lives))
         lives args))

(define (filter-allocateds var-tys vs)
  (filter (lambda (v)
            (match (hash-ref var-tys v)
              [`(Vector . ,_) #t]
              [_ #f]))
          vs))
