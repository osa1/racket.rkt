#lang racket

(require racket/hash)

(require (only-in "typecheck.rkt" mk-toplevel-ty-env))
(require "utils.rkt")

(provide flatten)

;; NOTE: Flatten doesn't completely flatten the program. Namely, it returns
;; programs with if-expressions, which have two program branches.

;; NOTE: Flatten annotates each assign and if with its type. So the form is
;; now:
;;
;;   (assign var var-type expr)
;;   (if cond ret-ty expr expr)

;; NOTE: The variables in meta-data field of the program now has types.

(define (flatten pgm)
  (match pgm
    [`(program . ,things)
      (let-values ([(defs expr) (split-last things)])
        (let* ([pgm-main (flatten-body expr)]
               [main-vs (collect-binds pgm-main)]
               [defs (map flatten-def defs)])
          `(program ,@defs (define main : void ,main-vs ,@pgm-main))))]

    [_ (unsupported-form 'flatten pgm)]))

(define (flatten-body expr)
  (let-values ([(_ pgm arg) (flatten-expr (hash) '() expr)])
    (reverse (cons `(return ,arg) pgm))))

(define (flatten-def def)
  (match def
    [`(define ,tag : ,ret-ty ,body)
     (let* ([pgm (flatten-body body)]
            [vs (collect-binds pgm)])
       `(define ,tag : ,ret-ty ,vs ,@pgm))]

    [_ (unsupported-form 'flatten-def def)]))

(define (flatten-expr-list binds pgm exprs)
  (match exprs
    [`() (values binds pgm exprs)]
    [`(,expr . ,exprs)
     (let*-values ([(binds pgm expr) (flatten-expr binds pgm expr)]
                   [(binds pgm exprs) (flatten-expr-list binds pgm exprs)])
       (values binds pgm (cons expr exprs)))]))

(define (flatten-expr binds pgm e0)
  (match (cdr e0)
    [(or (? fixnum?) (? boolean?))
     (values binds pgm (cdr e0))]

    [`(read)
     (let [(fresh (gensym "tmp"))]
       (values binds (cons `(assign ,fresh 'Integer (read)) pgm) fresh))]

    [`(,(or '- 'not) ,e1)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym "tmp"))]
         (values binds (cons `(assign ,fresh ,(car e0) (,(cadr e0) ,e1)) pgm) fresh)))]

    [`(,(or '+ 'eq?) ,e1 ,e2)
     (let*-values ([(binds pgm e1) (flatten-expr binds pgm e1)]
                   [(binds pgm e2) (flatten-expr binds pgm e2)])
       (let [(fresh (gensym "tmp"))]
         (values binds (cons `(assign ,fresh ,(car e0) (,(cadr e0) ,e1 ,e2)) pgm) fresh)))]

    [(? symbol?)
     ; Similar to the uniquify step: If not in map, assume global definition.
     (values binds pgm (hash-ref binds (cdr e0) (cdr e0)))]

    [`(let ([,var ,e1]) ,body)
     (let-values ([(binds pgm e1-flat) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym (string-append "tmp_" (symbol->string var) "_")))]
         (let-values ([(binds pgm body)
                       (flatten-expr (hash-set binds var fresh)
                                     (cons `(assign ,fresh ,(car e1) ,e1-flat) pgm)
                                     body)])
           (values binds pgm body))))]

    [`(if ,e1 ,e2 ,e3)
     (let [(fresh (gensym "tmp-if"))]
       (let*-values ([(binds pgm e1) (flatten-expr binds pgm e1)]
                     [(_ pgm-t ret-t) (flatten-expr binds '() e2)]
                     [(_ pgm-f ret-f) (flatten-expr binds '() e3)])
         (let* [(pgm-t (reverse (cons `(assign ,fresh ,(car e0) ,ret-t) pgm-t)))
                (pgm-f (reverse (cons `(assign ,fresh ,(car e0) ,ret-f) pgm-f)))]
           (values binds (cons `(if (eq? ,e1 #t) ,pgm-t ,pgm-f) pgm) fresh))))]

    [`(vector-ref ,e1 ,idx)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym "tmp"))]
         (values binds (cons `(assign ,fresh ,(car e0) (vector-ref ,e1 ,idx)) pgm) fresh)))]

    [`(vector-set! ,vec ,idx ,e)
     ;; Q: Why not make this a statement?
     ;; A: Because functional programming (everything is an expression) with
     ;;    side-effects. It sucks.
     (let*-values ([(binds pgm vec) (flatten-expr binds pgm vec)]
                   [(binds pgm e) (flatten-expr binds pgm e)])
       (let [(fresh (gensym "void"))]
         (values binds (cons `(assign ,fresh void (vector-set! ,vec ,idx ,e)) pgm) fresh)))]

    [`(vector . ,elems)
     (let-values ([(binds pgm es) (flatten-expr-list binds pgm elems)])
       (let [(fresh (gensym "tmp-vec"))]
         (values binds (cons `(assign ,fresh ,(car e0) (vector ,@es)) pgm) fresh)))]

    ;; We directly apply top-level functions without indirect jumps.
    [`(app (,_ . (toplevel-fn ,f)) . ,args)
     (let-values ([(binds pgm args) (flatten-expr-list binds pgm args)])
       (let ([fresh (gensym "funret")])
         (values binds (cons `(assign ,fresh ,(car e0) (app (toplevel-fn ,f) ,@args)) pgm) fresh)))]

    ;; Slow application
    [`(app ,f . ,args)
     (let*-values ([(binds pgm f) (flatten-expr binds pgm f)]
                   [(binds pgm args) (flatten-expr-list binds pgm args)])
       (let ([fresh (gensym "funret")])
         (values binds (cons `(assign ,fresh ,(car e0) (app ,f ,@args)) pgm) fresh)))]

    ;; References to functions are already values
    [(or `(toplevel-fn ,_) `(function-ref ,_))
     (values binds pgm (cdr e0))]

    ;; [`(app ,f . ,args)
    ;;  (let*-values ([(binds pgm f) (flatten-expr binds pgm f)]
    ;;                [(binds pgm args) (flatten-expr-list binds pgm args)])
    ;;    (let [(fresh (gensym "funret"))]
    ;;      (values binds (cons `(assign ,fresh ,(car e0) (app ,f ,@args)) pgm) fresh)))]

    [_ (unsupported-form 'flatten-expr (cdr e0))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove statements in form (assign x y) where y is a variable.
;; Does this by renaming x with y in the statements that follow this statement.
;; Does not effect semantics - for simplification purposes only.
;; NOTE: Statements should be in order - e.g. don't pass the list returned by
;; flatten-expr, it needs to be reversed!
(define (remove-var-asgns stmts)
  (match stmts
    [(list) '()]

    [(cons `(assign ,x ,x-ty ,y) t)
     (if (symbol? y)
       (remove-var-asgns (rename-stmts x y t))
       (cons `(assign ,x ,x-ty ,y) (remove-var-asgns t)))]

    ;; FIXME: This is not quite right: Last statement of if branches are always
    ;; assignments to a variable. Disabling this for now.
    ; [(cons `(if ,e ,pgm-t ,pgm-f) t)
    ;  (cons `(if ,e ,(remove-var-asgns pgm-t) ,(remove-var-asgns pgm-f))
    ;        (remove-var-asgns t))]

    [(cons s t)
     (cons s (remove-var-asgns t))]))

;; Substitute y for x in stmts
(define (rename-stmts x y stmts)
  (match stmts
    [(list) '()]

    [(cons `(assign ,x1 ,x-ty ,y1) t)
     (cond [(eq? x1 x)
            (error 'rename-stmts "BUG: the variable seen in LHS: ~s in ~s~n" x stmts)]
           [else
            (cons `(assign ,x1 ,x-ty ,(rename-expr x y y1)) (rename-stmts x y t))])]

    [(cons `(return ,y1) t)
     (if (eq? y1 x)
       (cons `(return ,y) (rename-stmts x y t))
       (cons `(return ,y1) (rename-stmts x y t)))]

    [(cons `(if ,e1 ,pgm-t ,pgm-f) t)
     (cons `(if ,(rename-expr x y e1) ,(rename-stmts x y pgm-t) ,(rename-stmts x y pgm-f))
           (rename-stmts x y t))]

    [(cons unsupported _)
     (unsupported-form 'rename-stmts unsupported)]))

(define (rename-expr x y expr)
  (match expr
    [`(read) expr]

    [`(,(or '- 'not) ,e1)
     `(,(car expr) ,(rename-arg x y e1))]

    [`(,(or '+ 'eq? 'vector-ref) ,e1 ,e2)
     `(,(car expr) ,(rename-arg x y e1) ,(rename-arg x y e2))]

    [(or (? fixnum?) (? boolean?) (? symbol?))
     (rename-arg x y expr)]

    [`(vector . ,as)
     `(vector ,@(map (lambda (arg) (rename-arg x y arg)) as))]

    [`(vector-set! ,vec ,idx ,e)
     `(vector-set! ,(rename-arg x y vec) ,idx ,(rename-arg x y e))]

    [_ (unsupported-form 'rename-expr expr)]))

(define (rename-arg x y arg)
  (match arg
    [(or (? fixnum?) (? boolean?)) arg]
    [(? symbol?) (if (eq? arg x) y arg)]
    [_ (unsupported-form 'rename-arg arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-binds pgm)
  ; (printf "collect-binds: ~s~n" pgm)
  (match pgm
    [`() (hash)]

    [`((assign ,x ,x-ty ,_) . ,t)
     (hash-set (collect-binds t) x x-ty)]

    [`((if ,x ,pgm1 ,pgm2) . ,t)
     (hash-union (collect-binds pgm1)
                 (collect-binds pgm2)
                 (collect-binds t)
                 #:combine (lambda (v1 v2) v1))]

    [`((return ,_) . ,t)
     (collect-binds t)]

    [_ (unsupported-form 'collect-binds pgm)]))
