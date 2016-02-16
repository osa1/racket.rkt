#lang racket

(require "utils.rkt")

; TODO: Remove this line, use either directly a hash map roll custom graph
; functions. (hash-ref parts is problematic)
(require (only-in "../public/utilities.rkt"
                  make-graph add-edge adjacent
                  caller-save callee-save))

(provide assign-homes)

; For debugging purposes
; (TODO: Currently used in `save-regs` too)
(provide gen-live-afters build-interference-graph mk-move-relation reg-alloc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live-after sets

; NOTE: This should be run _before_ assign-homes as this assumes variable
; arguments.

; OUTPUT: (values updated pgm, a list of live-after sets).
; We update the program to annotate branches of if-statements with live-after
; set lists, to be able to build interference graph by traversing the branches.
(define (gen-live-afters pgm)
  (match pgm
    [(list-rest 'program metadata instrs)
     (let-values [((instrs live-before live-afters)
                   (gen-live-afters-instrs
                     (reverse instrs)
                     ; accumulator for live-after sets, live-after set for the
                     ; last (first in the list, as we reverse the instructions)
                     ; instruction is an empty set so we add it here
                     (list (set))))]
       (values `(program ,metadata ,@(reverse instrs)) live-afters))]
    [_ (unsupported-form 'gen-live-afters pgm)]))

;; NOTE: Instructions should be reversed! E.g. first instruction in this
;; argument should be the last instruction in the block!
(define (gen-live-afters-instrs instrs live-afters)
  (match instrs
    [(list)
     ; We also need to return live set for the whole block here to be able to
     ; process if-statements properly. Suppose we have something like this:
     ;
     ;   (if (eq? y #t) ((assign a b)) ((assign c d)))  -- lives: ???
     ;   ...                                            -- lives: { b, d }
     ;
     ; The ??? part should be { b, d, a, c, y }, but we can only generate this
     ; if we know live-before set of the first instructions in branches.
     ;
     ; Then we can just do
     ;
     ;    (live-before true-branch) \union (live-before false-branch) \union {y}
     ;
     ; Head of live-afters is live-after of current instruction, so it's
     ; life-before of the first instruction in the block.
     (values '() (car live-afters) (cdr live-afters))]

    [(cons instr instrs)
     (let [(current-live-after (car live-afters))]
       (let-values [((instr current-live-before) (gen-live-afters-instr instr current-live-after))]
         (let-values [((instrs live-before live-afters)
                       (gen-live-afters-instrs instrs (cons current-live-before live-afters)))]
           (values (cons instr instrs) live-before live-afters))))]))

(define (gen-live-afters-instr instr lives)
  ; (printf "gen-live-afters-instr ~a ~a~n" lives instr)
  (match instr
    [`(,(or 'addq 'subq 'cmpq 'xorq) ,arg1 ,arg2)
     (values instr (add-live lives arg1 arg2))]

    [`(pushq ,arg1)
     (values instr (add-live lives arg1))]

    [`(popq ,arg1)
     (values instr (remove-live lives arg1))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; XXX: We will have to update this two if we decide to use *ax in the
    ;; future.

    [`(,(or 'sete 'setl) ,_)
     (values instr lives)]

    [`(movzbq (byte-reg al) ,arg2)
     (values instr (remove-live lives arg2))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [(list 'movq arg1 arg2)
     (values instr (add-live (remove-live lives arg2) arg1))]

    [`(negq ,arg1)
     (values instr (add-live lives arg1))]

    ; not sure about this part, what to do about function arguments? what about
    ; caller-save registers?
    [`(callq ,_)
     (values instr (set-remove lives 'rax))]

    [`(retq)
     (values instr lives)]

    [(or `(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
         ; It's important that we handle this pattern here. We sometimes
         ; (redundantly) generate live-afters in some later passes for things
         ; like saving caller-save registers etc.
         `(if (eq? ,arg1 ,arg2) ,pgm-t ,_ ,pgm-f ,_))
     (let-values [((pgm-t-reversed lives-before-t lives-t)
                   (gen-live-afters-instrs (reverse pgm-t) (list lives)))

                  ((pgm-f-reversed lives-before-f lives-f)
                   (gen-live-afters-instrs (reverse pgm-f) (list lives)))]
       ; (printf "lives-before-t: ~s~n" lives-before-t)
       ; (printf "lives-after-t: ~s~n" lives-before-f)
       ; (assert lives-before-t set-equal?)
       ; (assert lives-before-f set-equal?)
       (let* [(lives-if-wo-cond (set-union lives-before-t lives-before-f))
              (if-lives (add-live lives-if-wo-cond arg1 arg2))]
         (values `(if (eq? ,arg1 ,arg2) ,(reverse pgm-t-reversed) ,lives-t
                                        ,(reverse pgm-f-reversed) ,lives-f)
                 if-lives)))]

    [_ (unsupported-form 'gen-live-afters-instr instr)]))

(define (add-live lives . args)
  (foldl (lambda (arg lives)
           (match arg
             [`(,(or 'var 'reg) ,v) (set-add lives v)]
             [`(offset (,(or 'var 'reg) ,v) ,_) (set-add lives v)]
             [(or `(int ,_) `(stack ,_) `(global-value ,_))
              lives]
             [_ (unsupported-form 'add-live arg)]))
         lives args))

(define (remove-live lives . args)
  (foldl (lambda (arg lives)
           (match arg
             [`(,(or 'var 'reg) ,v) (set-remove lives v)]
             ; [`(offset (,(or 'var 'reg) ,v) ,_) (set-remove lives v)]
             [`(offset (,(or 'var 'reg) ,_) ,_) lives]
             [(or `(int ,_) `(stack ,_) `(global-value ,_))
              lives]
             [_ (unsupported-form 'remove-live arg)]))
         lives args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interference graphs

(define (build-interference-graph pgm live-sets)
  (match pgm
    [(list-rest 'program vs instrs)
     ; (printf "program vs ~s instrs ~s~n" vs instrs)
     (let [(graph (make-graph vs))]
       (build-int-graph-instrs instrs live-sets graph)
       graph)]
    [_ (unsupported-form 'build-interference-graph pgm)]))

(define (build-int-graph-instrs instrs live-sets graph)
  (map (lambda (instr lives) (build-int-graph instr (set->list lives) graph)) instrs live-sets))

(define (build-int-graph instr lives graph)
  (match instr
    [`(,(or 'addq 'subq 'xorq) (,_ ,s) (,_ ,d))
     (for ([live lives])
       (unless (equal? live d)
         (add-edge graph d live)))]

    [`(cmpq ,_ ,_) (void)]
    [`(,(or 'sete 'setl) ,_) (void)]
    [`(movzbq (byte-reg al) (,_ ,d))
     (for ([live lives])
       (unless (equal? live d)
         (add-edge graph d live)))]

    [`(,(or 'pushq 'popq 'negq) (,_ ,d))
     (for ([live lives])
       (unless (equal? live d)
         (add-edge graph d live)))]

    [`(movq (,_ ,s) (,_ ,d))
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-edge graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Handling movs with relative addressing

    [(or `(movq ,s (offset (,_ ,d) ,_))
         `(movq (offset (,_ ,s) ,_) ,d))
     (for ([live lives])
       (unless (or (equal? live s) (equal? live d))
         (add-edge graph d live)))]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    [`(retq) (void)]

    [`(callq ,_)
     (for ([live lives]
           [save (set->list caller-save)])
       (add-edge graph save live))]

    [`(if (eq? ,_ ,_) ,pgm-t ,t-lives ,pgm-f ,f-lives)
     (build-int-graph-instrs pgm-t t-lives graph)
     (build-int-graph-instrs pgm-f f-lives graph)]

    [`(if (eq? ,_ ,_) ,_ ,_)
     (error 'build-int-graph "if doesn't have live-after annotations on branches!~n~s~n" instr)]

    [_ (unsupported-form 'build-int-graph instr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move relation graph

(define (mk-move-relation pgm int-graph)
  (match pgm
    [(list-rest 'program vs instrs)
     (let [(graph (make-graph vs))]
       (mk-move-relation-instrs graph int-graph instrs)
       graph)]
    [_ (unsupported-form 'mk-move-relation pgm)]))

(define (mk-move-relation-instrs graph int-graph instrs)
  (for ([instr instrs]) (mk-move-rel-iter graph int-graph instr)))

(define (mk-move-rel-iter graph int-graph instr)
  (define (can-relate? arg)
    (match arg
      [`(,(or 'reg 'stk 'var) ,_) #t]

      [`(if ,_ ,pgm-t ,pgm-f)
       (mk-move-relation-instrs graph int-graph pgm-t)
       (mk-move-relation-instrs graph int-graph pgm-f)]

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
    [_ (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register allocation

; This is an ordered list! We should first use the registers that come first in
; the list. Latter ones are the ones we need to save.
(define general-registers
  (append (set->list callee-save) (set->list caller-save)))

(define (reg-alloc int-graph move-rels [regs general-registers])
  (let [(int-graph (hash-copy int-graph))]
    (hash-remove! int-graph 'rax)
    (reg-alloc-iter int-graph move-rels (make-immutable-hash) 0 regs)))

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
    [`(program ,_ . ,instrs)
     (let-values [((pgm live-after-sets) (gen-live-afters pgm))]
       (let* ([int-graph (build-interference-graph pgm live-after-sets)]
              [move-rel (mk-move-relation pgm int-graph)])
         (let-values ([(homes stack-size) (reg-alloc int-graph move-rel)])
           `(program (,(align-stack stack-size))
                     ; Note that here we use the original instructions, e.g.
                     ; not the ones with live-after annotations on if branches.
                     ; Either way should work here.
                     ,@(assign-home-instrs homes instrs)))))]

    [_ (unsupported-form 'assign-homes pgm)]))

(define (align-stack stack) (+ stack (modulo stack 16)))

(define (assign-home-instrs asgns instrs)
  (map (lambda (instr) (assign-home-instr asgns instr)) instrs))

(define (assign-home-instr asgns instr)
  (match instr
    [`(,if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     `(if (eq? ,(assign-home-arg asgns arg1) ,(assign-home-arg asgns arg2))
        ,(assign-home-instrs asgns pgm-t)
        ,(assign-home-instrs asgns pgm-f))]

    [`(,(or 'addq 'subq 'movq 'cmpq 'xorq) ,arg1 ,arg2)
     (list (car instr) (assign-home-arg asgns arg1) (assign-home-arg asgns arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     (list (car instr) (assign-home-arg asgns arg))]

    [`(callq ,_) instr]

    [`(retq) instr]

    [`(,(or 'sete 'setl) (byte-reg al)) instr]

    [`(movzbq (byte-reg al) ,arg)
     `(movzbq (byte-reg al) ,(assign-home-arg asgns arg))]

    [_ (unsupported-form 'assign-home-instr instr)]))

(define (assign-home-arg asgns arg)
  (match arg
    [`(int ,_) arg]
    [`(reg ,_) arg]
    [`(stack ,_) arg]
    [`(global-value ,_) arg]
    [`(offset ,arg ,offset)
     ; TODO: What happens if the pointer is on stack?
     `(offset ,(assign-home-arg asgns arg) ,offset)]
    [`(var ,var)
     (let [(asgn (hash-ref asgns var '()))]
       (cond [(null? asgn)
              (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
             [(fixnum? asgn) `(stack ,asgn)]
             [#t `(reg ,asgn)]))]
    [_ (unsupported-form 'assign-home-arg arg)]))
