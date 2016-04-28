#lang racket

(require "utils.rkt")
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
          (peval-expr initial-env (make-hash) main-expr)
          pgm]
         [_ (unsupported-form 'peval main)]))]
    [_ (unsupported-form 'peval pgm)]))

(define (mk-env defs)
  (make-immutable-hash
    (map (lambda (def)
           (match def
             [`(define (,name . ,args) : ,ret-ty ,body)
              ; Top-level functions don't have closure environments.
              `(,name .
                ((,@(map caddr args) -> ,ret-ty) . (lambda: ,args : ,ret-ty #f ,body)))]
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
     ; TODO: Not sure about this part. Do we need to also store fun-defs?
     `(,(car expr) . (lambda: ,args : ,ret-ty ,env ,body))]

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
         `(,(car expr) . ,(if (cdr e1) #t #f))
         `(,(car expr) . ,e1)))]

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
            ((racket-fn (cadr expr)) (cdr e1))
            `(,(cadr expr) ,e1))))]

    ; Binary operators
    [`(,(or '+ 'eq? '< '<= '> '>=) ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) .
         ,(if (and (val? e1) (val? e2))
            ((racket-fn (cadr expr)) (cdr e1) (cdr e2))
            `(,(cadr expr) ,e1 ,e2))))]

    [`(eq?-dynamic ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       (if (and (val? e1) (val? e2))
         `(,(car expr) . ,(equal? (cdr e1) (cdr e2)))
         `(,(car expr) . (eq?-dynamic ,e1 ,e2))))]

    ; TODO: This is probably a bit too restrictive: We only evaluate this when
    ; `e1` is completely evaluated. In theory only having nth element evaluated
    ; should be enough. (rest of the exprs need to be evaluated for the side
    ; effects, which can be handled by some let expressions)
    [`(vector-ref ,e1 ,idx)
     (let ([e1 (peval-expr env fun-defs e1)])
       (if (val? e1)
         (match (cdr e1)
           [`(vector . ,elems) #:when (> (length elems) idx)
            (list-ref elems idx)]
           [_
            `(,(car expr) . (vector-ref ,e1 ,idx))])
         `(,(car expr) . (vector-ref ,e1 ,idx))))]

    [`(vector-ref-dynamic ,e1 ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       ; Make it a static vector-ref if e2 is completely evaluated
       (if (and (val? e2) (fixnum? (cdr e2)))
         ; We can potentially evaluate this further, so the recursive call
         (peval-expr env fun-defs `(,(car expr) . (vector-ref ,e1 ,(cdr e2))))
         `(,(car expr) . (vector-ref-dynamic ,e1 ,e2))))]

    [`(vector-set! ,e1 ,idx ,e2)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)])
       `(,(car expr) . (vector-set! ,e1 ,idx ,e2)))]

    [`(vector-set!-dynamic ,e1 ,e2 ,e3)
     (let ([e1 (peval-expr env fun-defs e1)]
           [e2 (peval-expr env fun-defs e2)]
           [e3 (peval-expr env fun-defs e3)])
       ; Make it a static vector-set! if e2 is completely evaluated
       (if (and (val? e2) (fixnum? (cdr e2)))
         `(,(car expr) . (vector-set! ,e1 ,(cdr e2) ,e3))
         `(,(car expr) . (vector-set!-dynamic ,e1 ,e2 ,e3))))]

    [`(let ([,var ,e1]) ,body)
     (let ([e1 (peval-expr env fun-defs e1)])
       ; TODO: This part is tricky -- need to make sure this won't lead to work
       ; duplication. For now I'm only updating the environment if e1 is value.
       (if (val? e1)
         ; TODO: Do we need to keep the let here?
         (peval-expr (hash-set env var e1) fun-defs body)
         `(,(car expr) . (let ([,var ,e1]) ,(peval-expr env fun-defs body)))))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Function application - the tricky part

    [`(,f . ,args)
     (let (; Evaluate the function, get the lambda form
           [f (peval-expr env fun-defs f)]

           ; Evaluate arguments
           [args (map (lambda (arg)
                        (peval-expr env fun-defs arg))
                      args)])

       (match (cdr f)
         [`(lambda: ,as : ,ret-ty ,clo-env ,body)
          ; Determine static and dynamic arguments
          (define arg-vals (map (lambda (arg arg-val)
                                  (cons (car arg) arg-val))
                                as args))
          ; TODO: Implement `Data.List.span`
          ; [(arg-name, arg-value)]
          (define static-args  (filter (lambda (arg)      (val? (cdr arg)))  arg-vals))
          (define dynamic-args (filter (lambda (arg) (not (val? (cdr arg)))) arg-vals))

          (debug-printf "static-args: ") (debug-pretty-print static-args)
          (debug-printf "dynamic-args: ") (debug-pretty-print dynamic-args)

          (if (null? dynamic-args)
            ; Inline completely static applications.
            ; TODO: What happens if the function loops/crashes? In case of a
            ; crash we just residualize the term that crashes, so it'll crash in
            ; runtime instead. In case of a loop, I don't see anything that
            ; stops partial evaluator...
            (peval-expr (foldr (lambda (kv m)
                                 (hash-set m (car kv) (cdr kv)))
                               (if clo-env clo-env env) static-args)
                        fun-defs body)
            ; (Partially) dynamic application
            (let ()
              ; If a function for these static args exists, use that one
              (define existing-fn (hash-ref fun-defs (list (cdr f) static-args) #f))
              (if existing-fn
                ; Generate the resudial call
                `(,(car expr) . (,(car existing-fn) ,@(map cdr dynamic-args)))
                ; Create a new function for the given static args
                (let ()
                  (define fun-name (fresh "pe"))
                  ; Evaluate the body with a placeholder for the new function,
                  ; so that a recursive call generates a residual application.
                  (define pe-env (foldr (lambda (kv m)
                                          (hash-set m (car kv) (cdr kv)))
                                        (if clo-env clo-env (make-immutable-hash))
                                        static-args))
                  (hash-set! fun-defs (list (cdr f) static-args) (cons fun-name 'placeholder))
                  (define body-pe (peval-expr pe-env fun-defs body))

                  ; Names of arguments of the new function
                  (define dynamic-arg-names (map car dynamic-args))
                  ; Types of arguments of the new function
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

                  (define dynamic-fun-type `(,@dynamic-fun-args -> ,ret-ty))

                  ; Replace the placeholder with the actual definition
                  (hash-set! fun-defs (list (cdr f) static-args)
                             (cons fun-name
                                   ; Not sure about clo-env here
                                   (cons dynamic-fun-type
                                         `(lambda: ,dynamic-fun-args : ,ret-ty
                                                   ,clo-env
                                                   ,body-pe))))

                  ; Finally generate the residual call to our new function
                  `(,(car expr) . (,fun-name ,@(map cdr dynamic-args)))))))]

         ; We don't evaluate RTS function calls
         [(or 'print-int)
          `(,(car expr) . (,f ,@args))]


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
    [`(vector . ,elems) (all val? elems)]
    [`(inject ,expr ,_) (val? expr)]
    [`(lambda: . ,_) #t]
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
    ['integer? fixnum?]
    ['boolean? boolean?]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Can't use native Racket functions here, as we don't represent vectors and
    ; procedures as Racket vectors and procedures.

    ['procedure?
     (lambda (val)
       (match val
         [`(lambda: . ,_) #t]
         [_ #f]))]

    ['vector?
     (lambda (val)
       (match val
         [`(vector . ,_) #t]
         [_ #f]))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [_ (unsupported-form 'racket-fn fn)]))
