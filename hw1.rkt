#lang racket

(require racket/fixnum)
(require racket/set)
(require (rename-in racket/list [flatten flatten-list]))
(require "public/utilities.rkt")
(require "public/interp.rkt")

; exp ::= int | (read) | (- exp) | (+ exp exp)
;       | var | (let ([var exp]) exp)
;
; R1  ::= (program exp)

; arg  ::= int | var
; exp  ::= arg | (read) | (- arg) | (+ arg arg)
; stms ::= (assign var exp) | (return arg)
; C0   ::= (program (var*) stmt+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify

(define (uniquify pgm)
  ; (printf "pgm: ~s~n" pgm)
  (match pgm
    [`(program ,e) `(program ,(uniquify-expr '() e))]
    [_ (error 'uniquify "Expected a (program ...) form, found: ~s~n" pgm)]))

(define (uniquify-expr rns e0)
  ; (printf "rns: ~s~n" rns)

  (match e0

    [(or (? fixnum?) `(read))
     e0]

    [`(- ,e1)
     `(- ,(uniquify-expr rns e1))]

    [`(+ ,e1 ,e2)
     `(+ ,(uniquify-expr rns e1) ,(uniquify-expr rns e2))]

    [(? symbol?)
     (car (lookup e0 rns))]

    [`(let ([,var ,e1]) ,body)
     (let* ([fresh (gensym "x")]
            [rns1 (cons `(,var ,fresh) rns)])
       `(let ([,fresh ,(uniquify-expr rns e1)])
          ,(uniquify-expr rns1 body)))]

    [unsupported
     (error 'uniquify-expr "unsupported form: ~s~n" unsupported)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flatten

(define (flatten pgm)
  (match pgm
    [`(program ,e)
     (let-values ([(_ pgm e) (flatten-expr '() '() e)])
       ; (printf "collect-binds result: ~s~n" (collect-binds pgm))
       (let [(stats (reverse (cons `(return ,e) pgm)))]
         ; (printf "stats before remove-var-asgns: ~s~n" stats)
         ; (printf "stats after remove-var-asgns: ~s~n" (remove-var-asgns stats))
         `(program ,(collect-binds pgm) ,@(remove-var-asgns stats))))]

    [_ (error 'flatten "Expected a (program ...) form, found ~s~n" pgm)]))

(define (collect-binds pgm)
  ; (printf "collect-binds: ~s~n" pgm)
  (match pgm
    [(list) '()]
    [(cons `(assign ,x ,_) t)
     (cons x (collect-binds t))]
    [_ (error 'collect-binds "unsupported form: ~s~n" pgm)]))

(define (arg? e) (or (fixnum? e) (symbol? e)))

; flatten-expr : [(var, var)] -> [stmt] -> expr -> arg
(define (flatten-expr binds pgm expr)
  (match expr

    [(? fixnum?)
     (values binds pgm expr)]

    [`(read)
     (let [(fresh (gensym "tmp"))]
       (values binds (cons `(assign ,fresh (read)) pgm) fresh))]

    [`(- ,e1)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym "tmp"))]
         (values binds (cons `(assign ,fresh (- ,e1)) pgm) fresh)))]

    [`(+ ,e1 ,e2)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let-values ([(binds pgm e2) (flatten-expr binds pgm e2)])
         (let [(fresh (gensym "tmp"))]
           (values binds (cons `(assign ,fresh (+ ,e1 ,e2)) pgm) fresh))))]

    [(? symbol?)
     (values binds pgm (car (lookup expr binds)))]

    [`(let ([,var ,e1]) ,body)
     (let-values ([(binds pgm e1) (flatten-expr binds pgm e1)])
       (let [(fresh (gensym (string-append "tmp_" (symbol->string var) "_")))]
         (let-values ([(binds pgm body)
                       (flatten-expr (cons `(,var ,fresh) binds)
                                     (cons `(assign ,fresh ,e1) pgm)
                                     body)])
           (values binds pgm body))))]

    [_ (error 'flatten-expr "unsupported form: ~s~n" expr)]))

;; Remove statements in form (assign x y) where y is a variable.
;; Does this by renaming x with y in the statements that follow this statement.
;; Does not effect semantics - for simplification purposes only.
;; NOTE: Statements should be in order - e.g. don't pass the list returned by
;; flatten-expr, it needs to be reversed!
(define (remove-var-asgns stmts)
  (match stmts
    [(list) '()]

    [(cons `(assign ,x ,y) t)
     (if (symbol? y)
       (remove-var-asgns (rename-stmts x y t))
       (cons `(assign ,x ,y) (remove-var-asgns t)))]

    [(cons s t)
     (cons s (remove-var-asgns t))]))

;; Substitute y for x in stmts
(define (rename-stmts x y stmts)
  (match stmts
    [(list) '()]

    [(cons `(assign ,x1 ,y1) t)
     (cond [(eq? x1 x)
            (error 'rename-stmts "BUG: the variable seen in LHS: ~s in ~s~n" x stmts)]
           [else
            (cons `(assign ,x1 ,(rename-expr x y y1)) (rename-stmts x y t))])]

    [(cons `(return ,y1) t)
     (if (eq? y1 x)
       (cons `(return ,y) (rename-stmts x y t))
       (cons `(return ,y1) (rename-stmts x y t)))]))

(define (rename-expr x y expr)
  (match expr
    [`(read) expr]

    [`(- ,e1)
     `(- ,(rename-arg x y e1))]

    [`(+ ,e1 ,e2)
     `(+ ,(rename-arg x y e1) ,(rename-arg x y e2))]

    [_ (rename-arg x y expr)]))

(define (rename-arg x y arg)
  (match arg
    [(? fixnum?) arg]
    [x1 (if (eq? x1 x) y arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction selection

; Input:
;
; arg  ::= int | var
; exp  ::= arg | (read) | (- arg) | (+ arg arg)
; stms ::= (assign var exp) | (return arg)
; C0   ::= (program (var*) stmt+)
;
; Output:
; arg   ::= (int int) | (reg register) | (stack int)
; instr ::= (addq arg arg) | (subq arg arg) | (negq arg) | (movq arg arg)
;         | (callq label) | (pushq arg) | (popq arg) | (retq)
; x86_0 ::= (program int instr+)
;
; In this pass, we also generate an arg (var x).

(define (instr-sel pgm)
  (match pgm
    [(list-rest 'program vs stmts)
     ; (printf "stmts: ~s~n" stmts)
     (let ([total-vars (length vs)])
       `(program ,(* 8 total-vars) ,@(append-map instr-sel-stmt stmts)))]

    [_ (error 'instr-sel "unhandled form: ~s~n" pgm)]))

(define (instr-sel-stmt stmt)
  (match stmt
    [`(assign ,var ,expr)
     (instr-sel-expr var expr)]
    [`(return ,arg)
     `((movq ,(arg->x86-arg arg) (reg rax)))]))

(define (instr-sel-expr bind-to expr)
  (match expr
    [(or (? fixnum?) (? symbol?))
     `(,(instr-sel-arg bind-to expr))]

    [`(read)
     `((callq read_int)
       (movq (reg rax) ,(arg->x86-arg bind-to)))]

    [`(- ,arg)
     `(,(instr-sel-arg bind-to arg)
       (negq ,(arg->x86-arg bind-to)))]

    [`(+ ,arg1 ,arg2)
     `(,(instr-sel-arg bind-to arg1)
       (addq ,(arg->x86-arg arg2) ,(arg->x86-arg bind-to)))]

    [_ (error 'instr-sel-expr "unsupported form: ~s~n" expr)]))

(define (instr-sel-arg bind-to arg)
  `(movq ,(arg->x86-arg arg) ,(arg->x86-arg bind-to)))

(define (arg->x86-arg arg)
  (cond [(symbol? arg) `(var ,arg)]
        [(fixnum? arg) `(int ,arg)]
        [else (error 'arg->x86-arg "unsupported arg: ~s~n" arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction selection

; Input: x86 with (var) in arguments.
; Output: x86 without any (var)s.
(define (assign-homes pgm)
  (match pgm
    [(list-rest 'program stack-size instrs)
     (let* ([all-vars (collect-vars (set) instrs)]
            [var-asgns (assign-vars 0 (hash) (set->list all-vars))])
       ; (printf "all-vars: ~s~n" all-vars)
       `(program (,stack-size) ,@(map (lambda (instr) (assign-home-instr var-asgns instr)) instrs)))]

    [_ (error 'assign-homes "unsupported form: ~s~n" pgm)]))

(define (collect-vars vars instrs)
  (match instrs
    [(list) vars]
    [(list-rest instr instrs)
     (collect-vars (collect-vars-instr vars instr) instrs)]
    [_ (error 'collect-vars "unsupported form: ~s~n" instrs)]))

(define (collect-vars-instr vars instr)
  (match instr
    [`(,(or 'addq 'subq 'movq) ,arg1 ,arg2)
     (collect-vars-arg (collect-vars-arg vars arg1) arg2)]
    [`(,(or 'pushq 'popq 'negq) ,arg)
     (collect-vars-arg vars arg)]
    [`(callq ,_) vars]
    [`(retq) vars]
    [_ (error 'collect-vars-instr "unsupported form: ~s~n" instr)]))

(define (collect-vars-arg vars arg)
  (match arg
    [`(int ,_) vars]
    [`(reg ,_) vars]
    [`(stack ,_) vars]
    [`(var ,v) (set-add vars v)]
    [_ (error 'collect-vars-arg "unsupported form: ~s~n" arg)]))

(define (assign-vars stack-offset var-offsets all-vars)
  (match all-vars
    [(list) var-offsets]
    [(list-rest var vars)
     (match (hash-ref var-offsets var '())
       [(list) (assign-vars (- stack-offset 8)
                            (hash-set var-offsets var (- stack-offset 8))
                            vars)]
       [_ (assign-vars stack-offset var-offsets vars)])]
    [_ (error 'assign-vars "unsupported form: ~s~n" all-vars)]))

(define (assign-home-instr asgns instr)
  (match instr
    [`(,(or 'addq 'subq 'movq) ,arg1 ,arg2)
     (list (car instr) (assign-home-arg asgns arg1) (assign-home-arg asgns arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     (list (car instr) (assign-home-arg asgns arg))]

    [`(callq ,_) instr]

    [`(retq) instr]

    [_ (error 'assign-home-instr "unsupported form: ~s~n" instr)]))

(define (assign-home-arg asgns arg)
  (match arg
    [`(int ,_) arg]
    [`(reg ,_) arg]
    [`(stack ,_) arg]
    [`(var ,var)
     (match (hash-ref asgns var '())
       [`() (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
       [ret `(stack ,ret)])]
    [_ (error 'assign-home-arg "unsupported form: ~s~n" arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction patching
;;
;; From the book, it's not clear what we're supposed to do here. So I'm just
;; doing this:
;; If the instructions takes two arguments and both of the arguments are memory
;; locations, just make the destination a %rax, then movq %rax mem.

(define (patch-instructions pgm)
  (match pgm
    [(list-rest 'program s stmts)
     `(program ,s ,@(append-map patch-instructions-stmt stmts))]
    [_ (error 'patch-instructions "unsupported form: ~s~n" pgm)]))

(define (arg-mem? arg)
  (match arg
    [`(stack ,_) #t]
    [`(,(or 'reg 'int) ,_) #f]
    [_ (error 'arg-mem? "illegal arg: ~s~n" arg)]))

(define (patch-instructions-stmt stmt)
  (match stmt
    [`(movq ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       (list `(movq ,arg1 (reg rax))
             `(movq (reg rax) ,arg2))
       (list stmt))]

    [`(,(or 'addq 'subq) ,arg1 ,arg2)
     (if (and (arg-mem? arg1) (arg-mem? arg2))
       (list (list 'movq arg2 '(reg rax))
             (list (car stmt) arg1 '(reg rax))
             (list 'movq '(reg rax) arg2))
       (list stmt))]

     [_ (list stmt)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print X86_64

(define main-prelude
"\t.globl main
main:\n")

(define main-conclusion "\tretq\n")

(define (mk-pgm-prelude stack-size)
  (format
"\tpushq %rbp
\tmovq %rsp, %rbp
\tsubq $~a, %rsp\n" stack-size))

(define (mk-pgm-conclusion stack-size)
  (format
"\tmovq %rax, %rdi
\tcallq print_int
\taddq $~a, %rsp
\tmovq $0, %rax
\tpopq %rbp\n" stack-size))

(define (print-x86_64 pgm)
  (match pgm
    [(list-rest 'program `(,s) stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)])
       (string-append main-prelude
                      (mk-pgm-prelude s)
                      (string-join stmt-lines)
                      (mk-pgm-conclusion s)
                      main-conclusion))]

    [_ (error 'printx86_64 "unsupported form: ~s~n" pgm)]))

(define instr3-format "\t~a ~a, ~a\n")
(define instr2-format "\t~a ~a\n")

(define (print-x86_64-stmt stmt)
  (match stmt
    [`(,(or 'addq 'subq 'movq) ,arg1 ,arg2)
     (format instr3-format (car stmt) (print-x86_64-arg arg1) (print-x86_64-arg arg2))]
    [`(,(or 'negq 'pushq 'pushq 'callq) ,arg1)
     (format instr2-format (car stmt) (print-x86_64-arg arg1))]
    [`(retq) "\tret\n"]
    [_ (error 'print-x86_64-stmt "unsupported form: ~s~n" stmt)]))

(define (print-x86_64-arg arg)
  (match arg
    [`(int ,int) (format "$~s" int)]
    [`(reg ,reg) (format "%~s" reg)]
    [`(stack ,offset) (format "~s(%rbp)" offset)]
    [(? symbol?) arg] ;; must be a function call
    [_ (error 'print-x86_64-arg "unsupported form: ~s~n" arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(interp-tests "uniquify"
              `(("uniquify" ,uniquify ,interp-scheme)
                ("flatten" ,flatten ,interp-C)
                ("instr-sel" ,instr-sel ,interp-x86)
                ("assign-homes" ,assign-homes ,interp-x86)
                ("patch-instructions" ,patch-instructions ,interp-x86))
              interp-scheme
              "uniquify"
              (range 1 6))

(interp-tests "flatten"
              `(("uniquify" ,uniquify ,interp-scheme)
                ("flatten" ,flatten ,interp-C)
                ("instr-sel" ,instr-sel ,interp-x86)
                ("assign-homes" ,assign-homes ,interp-x86)
                ("patch-instructions" ,patch-instructions ,interp-x86))
              interp-scheme
              "flatten"
              (range 1 5))

(compiler-tests "first assignment"
                `(("uniquify" ,uniquify ,interp-scheme)
                  ("flatten" ,flatten ,interp-C)
                  ("instr-sel" ,instr-sel ,interp-x86)
                  ("assign-homes" ,assign-homes ,interp-x86)
                  ("patch-instructions" ,patch-instructions ,interp-x86)
                  ("print-x86" ,print-x86_64 ,interp-x86))
                "uniquify"
                (range 1 5))
