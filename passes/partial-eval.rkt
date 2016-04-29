#lang racket

(require "utils.rkt")
(require (only-in "closure-convert.rkt" fvs))
(require (only-in "elim-dyns.rkt" elim-dyn-expr))
(require (only-in "typecheck.rkt" extract-arg-name extract-arg-ty))

(provide peval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: Copied from register allocator

(define debug-peval (make-parameter #f))

(define (debug-printf . args)
  (when (debug-peval)
    (apply printf args)))

(define (debug-pretty-print . args)
  (when (debug-peval)
    (apply pretty-print args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peval pgm)
  (match pgm
    [`(program . ,defs)
     (let-values ([(defs main) (split-last defs)])
       (define initial-env (mk-env defs))
       ; (debug-printf "initial-env:~n")
       ; (debug-pretty-print initial-env)
       (match main
         [`(define main : void ,main-expr)
          (define new-fns (make-hash))
          (define ret (peval-expr initial-env new-fns main-expr))
          ; (printf "peval ret:~n")
          ; (pretty-print ret)
          ; (printf "peval new-fns:~n")
          ; (pretty-print new-fns)
          ; pgm
          `(program
             ,@defs ; FIXME: Some of the definition are not necessary anymore,
                    ; maybe implement a garbage collection pass that removes
                    ; unused definitions.
             ,@(mk-defs new-fns)
             (define main : void ,ret))]
         [_ (unsupported-form 'peval main)]))]
    [_ (unsupported-form 'peval pgm)]))

(define (mk-defs fns)
  (map (lambda (def)
         (define def-name (car def))
         (define def-lambda (cddr def))
         (match def-lambda
           [`(lambda: ,args : ,ret-ty ,body)
            `(define (,def-name ,@args) : ,ret-ty ,body)]
           [_ (unsupported-form 'mk-defs def)]))
       (hash-values fns)))

(define (mk-env defs)
  (make-immutable-hash
    (map (lambda (def)
           (match def
             [`(define (,name . ,args) : ,ret-ty ,body)
              `(,name .
                ((,@(map caddr args) -> ,ret-ty) . (lambda: ,args : ,ret-ty ,body)))]
             [_ (unsupported-form 'mk-env def)]))
         defs)))

(define (peval-expr env fun-defs expr)

  (define ret
  (match (cdr expr)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Values

    ; Constants
    [(or (? fixnum?) (? boolean?) `(void))
     expr]

    [`(vector . ,elems)
     `(,(car expr) . (vector ,@(map (lambda (e)
                                      (peval-expr env fun-defs e))
                                    elems)))]

    ; Closures
    [`(lambda: ,args : ,ret-ty ,body)
     `(,(car expr) . (lambda: ,args : ,ret-ty ,(peval-expr env fun-defs body)))]

    ; Dynamic values
    [`(inject ,e1 ,ty)
     `(,(car expr) . (inject ,(peval-expr env fun-defs e1) ,ty))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; (read) is dynamic
    [`(read) expr]

    ; Variables may be unbound, e.g. we don't bind dynamic arguments in function
    ; applications
    [(? symbol?)
     (hash-ref env (cdr expr) expr)]

    [`(project ,e1 ,ty)
     (let ([e1 (peval-expr env fun-defs e1)])
       (if (val? e1)
         (match e1
           [`(inject ,val ,ty1) #:when (equal? ty ty1)
            val]
           [_ `(,(car expr) . (project ,e1 ,ty))])
         `(,(car expr) . (project ,e1 ,ty))))]

    [`(project-boolean ,e1)
     (let ([e1 (peval-expr env fun-defs e1)])
       (if (val? e1)
         (match e1
           [`(inject ,val ,ty1) `(,(car expr) . ,(if val #t #f))]
           [_ `(,(car expr) . (project-boolean ,e1))])
         `(,(car expr) . (project-boolean ,e1))))]

    [`(if ,e1 ,e2 ,e3)
     (let ([e1 (peval-expr env fun-defs e1)])
       (if (val? e1)
         (peval-expr env fun-defs (if (cdr e1) e2 e3))
         `(,(car expr) . (if ,e1
                           ,(peval-expr env fun-defs e2)
                           ,(peval-expr env fun-defs e3)))))]

    ; Unary operators
    [`(,(or '- 'not 'boolean? 'integer? 'vector? 'procedure?) ,e1)
     (let ([e1 (peval-expr env fun-defs e1)])
       `(,(car expr) .
         ,(if (val? e1)
            ((racket-fn (cadr expr)) (cdr (elim-dyn-expr e1)))
            `(,(cadr expr) ,e1))))]

    ; Binary operators
    [`(,(or '+ 'eq? '< '<= '> '>=) ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) .
         ,(if (and (val? e1) (val? e2))
            ((racket-fn (cadr expr)) (cdr (elim-dyn-expr e1))
                                     (cdr (elim-dyn-expr e2)))
            `(,(cadr expr) ,e1 ,e2))))]

    [`(eq?-dynamic ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) .
         ,(if (and (val? e1) (val? e2) (equal? (cdr e1) (cdr e2)))
            #t
            `(eq?-dynamic ,e1 ,e2))))]

    [`(vector-ref ,e1 ,idx)
     `(,(car expr) . (vector-ref ,(peval-expr env fun-defs e1) ,idx))]

    [`(vector-ref-dynamic ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) .
         ; Make it a static vector-ref if e2 is completely evaluated
         ,(if (and (val? e2) (fixnum? (cdr e2)))
            `(vector-ref ,e1 ,(cdr e2))
            `(vector-ref-dynamic ,e1 ,e2))))]

    [`(vector-set! ,e1 ,idx ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) . (vector-set! ,e1 ,idx ,e2)))]

    [`(vector-set!-dynamic ,e1 ,e2 ,e3)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)]
           [e3 (peval-expr env fun-defs e3)])
       `(,(car expr) .
         ; Make it a static vector-set! if e2 is completely evaluated
         ,(if (and (val? e2) (fixnum? (cdr e2)))
            `(vector-set! ,e1 ,(cdr e2) ,e3)
            `(vector-set!-dynamic ,e1 ,e2 ,e3))))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Let-bindings -- we only add values (e.g. always-inline stuff) to the
    ; environment, so the lookup code stays simple.

    [`(let ([,var ,e1]) ,body)
     (let ([e1 (peval-expr env fun-defs e1)])
       (if (val? e1)
         (peval-expr (hash-set env var e1) fun-defs body)
         `(,(car expr) . (let ([,var ,e1]) ,(peval-expr env fun-defs body)))))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Function application - the tricky part

    ; HACK: This is only used for rts functions for now, so no need to evaluate
    ; `f` and do the usual business with function applications.
    [`(app-noalloc ,f . ,args)
     `(,(car expr) . (app-noalloc ,f ,@(map (lambda (arg)
                                               (peval-expr env fun-defs arg))
                                             args)))]

    [`(,f0 . ,args)
     (let (; Evaluate the function, get the lambda form
           [f (peval-expr env fun-defs f0)]

           ; Evaluate arguments
           [args (map (lambda (arg)
                        (peval-expr env fun-defs arg))
                      args)])

       (match (cdr f)
         [`(lambda: ,as : ,ret-ty ,body)
          ; Determine static and dynamic arguments
          (define arg-vals (map (lambda (arg arg-val)
                                  (cons (car arg) arg-val))
                                as args))
          ; TODO: Implement `Data.List.span`
          ; [(arg-name, arg-value)]
          (define static-args  (filter (lambda (arg)      (val? (cdr arg)))  arg-vals))
          (define dynamic-args (filter (lambda (arg) (not (val? (cdr arg)))) arg-vals))

          (debug-printf "static-args:  ") (debug-pretty-print static-args)
          (debug-printf "dynamic-args: ") (debug-pretty-print dynamic-args)

          (define static-arg-env
            (foldr (lambda (kv m)
                     (hash-set m (car kv) (cdr kv)))
                   env static-args))

          (if (null? dynamic-args)

            ; Inline completely static applications.
            ; TODO: What happens if the function loops/crashes? In case of a
            ; crash we just residualize the term that crashes, so it'll crash in
            ; runtime instead. In case of a loop, I don't see anything that
            ; stops partial evaluator...
            (peval-expr static-arg-env fun-defs body)

            ; (Partially) dynamic application
            (let ()
              (define existing-fn-def (hash-ref fun-defs (list (cdr f) static-args) #f))

              ; Names of arguments of the memoized function
              (define dynamic-arg-names (map car dynamic-args))
              ; Types of arguments of the memoized function
              (define dynamic-arg-types
                (map (lambda (dyn-arg-name)
                       (extract-arg-ty
                         (findf (lambda (fun-arg)
                                  (equal? (extract-arg-name fun-arg) dyn-arg-name))
                                as)))
                     dynamic-arg-names))

              ; Lambda arg syntax, with types
              (define dynamic-fun-args
                (map (lambda (n t) `(,n : ,t)) dynamic-arg-names dynamic-arg-types))

              (define dynamic-fun-type `(,@dynamic-arg-types -> ,ret-ty))

              ; If a function for these static args exists, use that one
              (if existing-fn-def
                ; Generate the residual call
                `(,(car expr) . ((,dynamic-fun-type . ,(car existing-fn-def))
                                 ,@(map cdr dynamic-args)))
                ; Create a new function for the given static args
                (let ()
                  (define fun-name (fresh "pe"))
                  ; Evaluate the body with a placeholder for the new function,
                  ; so that a recursive call generates a residual application.
                  (hash-set! fun-defs (list (cdr f) static-args) (cons fun-name 'placeholder))
                  (define body-pe (peval-expr static-arg-env fun-defs body))

                  ; Replace the placeholder with the actual definition
                  (hash-set! fun-defs (list (cdr f) static-args)
                             (cons fun-name
                                   (cons dynamic-fun-type
                                         `(lambda: ,dynamic-fun-args : ,ret-ty
                                                   ,body-pe))))

                  ; Finally generate the residual call to our new function
                  `(,(car expr) .
                    ((,dynamic-fun-type . ,fun-name)
                     ,@(map cdr dynamic-args)))))))]

         ; TODO: There are some games we can play here. For example, if `f` is
         ; an if, we can push the arguments to the branches. (maybe only do that
         ; when all args are static) I'm not doing anything fancy for now.
         [_ `(,(car expr) . (,f . ,args))]))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [e1 (unsupported-form 'peval-expr e1)]))


  (debug-printf "peval-expr~n")
  (debug-printf "--- env:~n")
  (debug-pretty-print env)
  (debug-printf "--- fun-defs:~n")
  (debug-pretty-print fun-defs)
  (debug-printf "--- expr:~n")
  (debug-pretty-print expr)
  (debug-printf "--- ret:~n")
  (debug-pretty-print ret)
  (debug-printf "~n")

  ret)

(define (rts-fun? id)
  (match id
    [(or 'print-int) #t]
    [_ #f]))

(define (val? expr)
  (match (cdr expr)
    [(or (? fixnum?) (? boolean?) `(void)) #t]
    [`(vector . ,_) #f]
    [`(,(or 'inject 'project) ,expr ,_)
     ; TODO: How do I say "only inline if `inject` will be eliminated"?
     ; This implementation can lead to more allocations.
     (val? expr)]
    [`(lambda: . ,_)
     (set-empty? (fvs expr))]
    [_ #f]))

(define (racket-fn fn)
  (match fn
    ['+ +]
    ['- -]
    ['> >]
    ['< <]
    ['>= >=]
    ['<= <=]
    ['eq? eq?]
    ['not not]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; These work on Any

    ['integer? integer-val?]
    ['boolean? boolean-val?]
    ['procedure? procedure-val?]
    ['vector? vector-val?]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [_ (unsupported-form 'racket-fn fn)]))

(define (integer-val? val)
  (match val
    [`(inject (,_ . ,val) ,_) (integer-val? val)]
    [(? fixnum?) #t]
    [_ #f]))

(define (boolean-val? val)
  (match val
    [`(inject (,_ . ,val) ,_) (boolean-val? val)]
    [(? boolean?) #t]
    [_ #f]))

(define (procedure-val? val)
  (match val
    [`(inject (,_ . ,val) ,_) (procedure-val? val)]
    [`(lambda: . ,_) #t]
    [_ #f]))

(define (vector-val? val)
  (match val
    [`(inject (,_ . ,val) ,_) (vector-val? val)]
    [`(vector . ,_) #t]
    [_ #f]))
