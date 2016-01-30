#lang racket

(require racket/fixnum)
(require racket/set)
(require (rename-in racket/list [flatten flatten-list]))
(require "public/utilities.rkt")
(require "public/interp.rkt")

(provide r1-passes
         ; export individual passes for testing purposes
         ; (see test.rkt)
         uniquify flatten instr-sel assign-homes patch-instructions print-x86_64)

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
     `(program ,vs ,@(append-map instr-sel-stmt stmts))]

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
;; Live-after sets

; NOTE: This should be run _before_ assign-homes as this assumes variable
; arguments.

(define (gen-live-afters pgm)
  (match pgm
    [(list-rest 'program _ instrs)
     ; generating in reversed order to avoid stack overflows
     (gen-live-afters-instrs (set) '() (reverse instrs))]
    [_ (error 'gen-live-afters "unsupported form: ~s~n" pgm)]))

(define (gen-live-afters-instrs lives acc instrs)
  (match instrs
    [(list) acc]
    [(cons instr instrs)
     (gen-live-afters-instrs (gen-live-afters-instr lives instr) (cons lives acc) instrs)]))

(define (gen-live-afters-instr lives instr)
  ; (printf "gen-live-afters-instr ~a ~a~n" lives instr)
  (match instr
    [(list (or 'addq 'subq) arg1 arg2)
     (let [(lives (match arg2
                    [`(,(or 'var 'reg) ,v) (set-add lives v)]
                    [_ lives]))]
       (match arg1
         [`(,(or 'var 'reg) ,v) (set-add lives v)]
         [_ lives]))]

    [(list (or 'pushq 'popq) (list (or 'var 'reg) v))
     (set-remove lives v)]

    [(list 'movq arg1 arg2)
     (let [(lives (match arg2
                    [`(,(or 'var 'reg) ,v) (set-remove lives v)]
                    [_ lives]))]
       (match arg1
         [`(,(or 'var 'reg) ,v) (set-add lives v)]
         [_ lives]))]

    [`(negq ,_) lives]

    ; not sure about this part
    [`(callq ,_) (set-remove lives 'rax)]

    [`(retq) lives]

    [_ (error 'gen-live-afters-instr "unsupported instruction form: ~s~n" instr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interference graphs

(define (build-interference-graph pgm live-sets)
  (match pgm
    [(list-rest 'program vs instrs)
     ; (printf "program vs ~s instrs ~s~n" vs instrs)
     (let [(graph (make-graph vs))]
       (map (lambda (instr lives)
              (build-int-graph instr (set->list lives) graph))
            instrs live-sets)
       graph)]
    [_ (error 'build-interference-graph "unsupported program: ~a~n" pgm)]))

(define (build-int-graph instr lives graph)
  (match instr
    [`(,(or 'addq 'subq) (,_ ,s) (,_ ,d))
     (map (lambda (live)
            (unless (equal? live d)
              (add-edge graph d live))) lives)]

    [`(,(or 'pushq 'popq 'negq) (,_ ,d))
     (map (lambda (live)
            (unless (equal? live d)
              (add-edge graph d live))) lives)]

    [`(movq (,_ ,s) (,_ ,d))
     (map (lambda (live)
            (unless (or (equal? live s) (equal? live d))
              (add-edge graph d live))) lives)]

    [`(retq) '()]

    [`(callq ,_)
     ; TODO: Find something like a cartesian product or list comprehension etc.
     ; and get rid of this awful nested map.
     (map (lambda (live)
            (map (lambda (save) (add-edge graph save live))
                 (set->list caller-save)))
          lives)]

    [_ (error 'build-graph "unsupported instruction: ~a~n" instr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move relation graph

(define (mk-move-relation pgm int-graph)
  (match pgm
    [(list-rest 'program vs instrs)
     (let [(graph (make-graph vs))]
       (map (lambda (instr) (mk-move-rel-iter graph int-graph instr)) instrs)
       graph)]
    [_ (error 'mk-move-relation "unsupported program: ~a~n" pgm)]))

(define (mk-move-rel-iter graph int-graph instr)
  (define (can-relate? arg)
    (match arg
      [`(,(or 'reg 'stk 'var) ,_) #t]
      [_ #f]))

  (define (extract arg)
    (match arg
      [`(,(or 'reg 'stk 'var) ,v) v]
      [_ arg]))

  (define (mk-edge graph arg1 arg2)
    (when (and (can-relate? arg1) (can-relate? arg2))
      ; but do they interfere?
      (let* [(arg1-adjs (hash-ref int-graph (extract arg1) (set)))
             (interfere (set-member? arg1-adjs (extract arg2)))]
        (unless interfere
          (add-edge graph (extract arg1) (extract arg2))))))

  (match instr
    [`(movq ,s ,d) (mk-edge graph s d)]
    [_ '()]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register allocation

; TODO: We can re-use stack locations when spilling, but that's not implemented
; at the moment.

; This is an ordered list! We should first use the registers that come first in
; the list. Latter ones are the ones we need to save.
(define general-registers
  (append (set->list callee-save) (set->list caller-save)))

(define (reg-alloc inter-graph move-rels [regs general-registers])
  (let [(inter-graph (hash-copy inter-graph))]
    (hash-remove! inter-graph 'rax)
    (reg-alloc-iter inter-graph move-rels (make-immutable-hash) 0 regs)))

(define (reg-alloc-iter int-graph move-rels mapping last-stack-loc regs)
  (if (eq? (hash-count int-graph) (hash-count mapping))
    (values mapping (- last-stack-loc))
    (let* [(all-vars    (list->set (hash-keys int-graph)))
           (mapped-vars (list->set (hash-keys mapping)))
           (not-mapped  (set-subtract all-vars mapped-vars))

           (most-constrained (find-most-constrained int-graph (set->list not-mapped)))

           (used-regs
             (list->set (filter-nulls
                          (map (lambda (nb) (hash-ref mapping nb '()))
                               (set->list (adjacent int-graph most-constrained))))))

           (available-regs
             (filter (lambda (reg) (not (set-member? used-regs reg))) regs))

           ; TODO: implement spilling when we run out of available registers
           (available-regs-lst (set->list available-regs))]

      (let* [(move-rel-vars (hash-ref move-rels most-constrained (set)))
             (interfered-vars (hash-ref int-graph most-constrained (set)))

             (move-rel-regs
               (filter-nulls
                 (map (lambda (arg) (hash-ref mapping arg '()))
                      (set->list move-rel-vars))))

             (interfered-regs
               (filter-nulls
                 (map (lambda (arg) (hash-ref mapping arg '()))
                      (set->list interfered-vars))))

             (move-rel-regs (set->list
                              (set-subtract (list->set move-rel-regs)
                                            (list->set interfered-regs))))]

        (cond [(not (null? move-rel-regs))
               (reg-alloc-iter int-graph move-rels
                               (hash-set mapping most-constrained (car move-rel-regs))
                               last-stack-loc regs)]

              [(not (null? available-regs-lst))
               (reg-alloc-iter int-graph move-rels
                               (hash-set mapping most-constrained (car available-regs-lst))
                               last-stack-loc regs)]

              [#t
               (reg-alloc-iter int-graph move-rels
                               (hash-set mapping most-constrained (- last-stack-loc 8))
                               (- last-stack-loc 8)
                               regs)])))))

(define (find-most-constrained inter-graph not-mapped)
  (let* [(cs (filter-nulls
               (hash-map inter-graph (lambda (key val)
                                       (if (set-member? not-mapped key)
                                         `(,(set-count val) . ,key)
                                         '())))))

         (min-key (foldl (lambda (e min_so_far)
                           (cond [(null? min_so_far) e]
                                 [(> (car min_so_far) (car e)) e]
                                 [#t min_so_far]))
                         '() cs))]
    (cdr min-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assigning vars to their locations on the machine

; Input: x86 with (var) in arguments.
; Output: x86 without any (var)s.
(define (assign-homes pgm)
  (match pgm
    [(list-rest 'program vs instrs)
     (let* ([live-sets (gen-live-afters pgm)]
            [int-graph (build-interference-graph pgm live-sets)]
            [move-rel (mk-move-relation pgm int-graph)])
       (let-values ([(homes stack-size) (reg-alloc int-graph move-rel)])
         ; (printf "all-vars: ~s~n" all-vars)
         `(program (,(align-stack stack-size))
                   ,@(map (lambda (instr) (assign-home-instr homes instr)) instrs))))]

    [_ (error 'assign-homes "unsupported form: ~s~n" pgm)]))

(define (align-stack stack) (+ stack (modulo stack 16)))

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
     (let [(asgn (hash-ref asgns var '()))]
       (cond [(null? asgn)
              (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
             [(fixnum? asgn) `(stack ,asgn)]
             [#t `(reg ,asgn)]))]
    [_ (error 'assign-home-arg "unsupported form: ~s~n" arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction patching
;;
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
;; Eliminate redundant movs

(define (elim-movs pgm)
  (match pgm
    [(list-rest 'program s instrs)
     `(program ,s ,@(filter-nulls (map elim-mov-instr instrs)))]
    [_ (error 'patch-instructions "unsupported form: ~s~n" pgm)]))

(define (elim-mov-instr instr)
  (match instr
    [`(movq ,arg1 ,arg2) (if (equal? arg1 arg2) '() instr)]
    [_ instr]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print X86_64

(define main-prelude
"\t.globl main
main:\n")

(define main-conclusion "\tretq")

(define (mk-pgm-prelude stack-size)
  (let [(ls
"\tpushq %rbp
\tmovq %rsp, %rbp\n")]
    (if (eq? stack-size 0)
      ls
      (string-append ls (format "\tsubq $~a, %rsp\n" stack-size)))))

(define (mk-pgm-conclusion stack-size)
  (let [(ls1
"\tmovq %rax, %rdi
\tcallq print_int\n")
        (ls2
"\tmovq $0, %rax
\tpopq %rbp\n")]
    (if (eq? stack-size 0)
      (string-append ls1 "\n" ls2)
      (string-append ls1 "\n" (format "\taddq $~a, %rsp\n" stack-size) ls2))))

(define (print-x86_64 pgm)
  (match pgm
    [(list-rest 'program `(,s) stmts)
     (let ([stmt-lines (map print-x86_64-stmt stmts)])
       (string-append main-prelude
                      (mk-pgm-prelude s)
                      "\n"
                      (string-join stmt-lines)
                      "\n"
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

(define r1-passes
  `(("uniquify" ,uniquify ,interp-scheme)
    ("flatten" ,flatten ,interp-C)
    ("instr-sel" ,instr-sel ,interp-x86)
    ("assign-homes" ,assign-homes ,interp-x86)
    ("patch-instructions" ,patch-instructions ,interp-x86)
    ("elim-movs" ,elim-movs ,interp-x86)
    ("print-x86" ,print-x86_64 #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (not-null? e) (not (null? e)))
(define (filter-nulls lst) (filter not-null? lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-lives instrs livess)
  (for-each (lambda (instr lives)
              (printf "~a\t~a~n" instr lives)) instrs livess))

(define (print-lives-rkt path)
  (print-lives-x86-pgm path (instr-sel (flatten (uniquify (read-program path))))))

; This takes as input a file path of a pseudo-x86 (with variables) probal, and
; compiles it using register allocation etc. Also generates a .dot file for
; interference graph to the same path.
(define (print-lives-x86-pgm path pgm [regs general-registers])
  (let* [(lives (gen-live-afters pgm))
         (int-graph (build-interference-graph pgm lives))
         (move-rels (mk-move-relation pgm int-graph))]
    (let-values [((allocations last-stack-loc) (reg-alloc int-graph move-rels regs))]
      (print-lives (cddr pgm) lives)
      (printf "interference graph: ~s~n~n" int-graph)
      (newline)
      (print-dot int-graph (string-append path ".int.dot"))
      (print-dot move-rels (string-append path ".mov.dot"))
      (printf "allocations: ~s~n" allocations)
      (newline)
      (printf "move relations: ~s~n" move-rels)
      (newline))))

(define (print-lives-x86 path [regs general-registers])
  (let* [(pgm (read-program path))]
    (print-lives-x86-pgm path pgm regs)))

; (print-lives-rkt "tests/uniquify_5.rkt")
; (print-lives-rkt "tests/r0_1.rkt")

; (print-lives-x86 "tests/lives_1.rkt")
; (print-lives-x86 "tests/lives_1.rkt" (list))
; (print-lives-rkt "tests/flatten_3.rkt")
