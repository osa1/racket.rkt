#lang racket

(require "utils.rkt")
(require "../settings.rkt")

(require "reg-alloc/live-after.rkt")
(require "reg-alloc/interference-graph.rkt")
(require "reg-alloc/move-rel.rkt")

; TODO: Remove this line, use either directly a hash map roll custom graph
; functions. (hash-ref parts is problematic)
(require (only-in "../public/utilities.rkt"
                  make-graph add-edge adjacent))

(provide assign-homes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE [Register in live-after sets and interference graph]
;
; Live-after sets are all about variables, it doesn't make sense to have
; registers in a live-after set. So we have some assertions about this in
; gen-live-afters.
;
; In theory, interference graph is also about variables. However, here we're
; actually treating registers as if they're variables. The problem this solves
; is that since we do instruction selection before register allocation, and
; since instruction selection can generate instruction that explicitly refers
; to some specific registers, we need to somehow handle those register here, to
; avoid overriding them. So here we do this by adding edges between variables
; and register in interference graph. Register allocator then takes this into
; account.
;
; A note on function calls: Whenever we see a function call, we should add an
; edge between caller-save register and live-after variables. Then register
; allocator should take care of the rest.
;
; Previously we were doing another pass (save-regs) to save caller-save
; register. This is bad for reasons:
;
; - Instruction selection is now doing argument passing to functions, but we
;   don't know when is the argument passing started happening in the program.
;   So when we save registers just before a callq, that messes up with the
;   stack layout (when passing things on stack).
;
; - Too much stack traffic in some cases.
;
; - TODO: What else?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register allocation

; This is an ordered list!
(define general-registers
  `(rdi rsi rdx rcx r8 r9 r10 r11 rbx r12 r13 r14 r15 rbp))

(define (map-args regs next-stack-loc args)
  ; We need to map args to stack locations in reverse order.
  (let-values ([(reg-args stack-args) (split-at-max args (length regs))])

    ; uh... I need mapAccumL here.
    (define (map-stk args next-stack-loc)
      (match args
        [`() `()]
        [`(,arg . ,args)
         (let ([mappings (map-stk args (+ 1 next-stack-loc))])
           (cons (cons `(var ,arg) next-stack-loc) mappings))]))

    (let* ([reg-mapping (map (lambda (arg reg) `((var ,arg) . ,reg))
                             reg-args (take regs (length reg-args)))]
           [regs-left (drop regs (length reg-args))]
           [stack-mapping (map-stk stack-args next-stack-loc)]
           [stack-slots-used (length stack-args)])
      (values (make-immutable-hash (append reg-mapping stack-mapping))
              stack-slots-used
              regs-left))))

(define (reg-alloc args int-graph move-rels has-callq [regs general-registers])
  (printf "has-callq: ~a~n" has-callq)
  (let ([int-graph (hash-copy int-graph)])
    (let-values ([(initial-mapping stack-slots-used regs-left)
                  (map-args arg-reg-syms 0 args)])

      ; If the function doesn't call any functions we just declare caller-save
      ; registers as free here. We do a bad job in register allocation but
      ; let's not be _that_ stupid.
      (let ([free-regs (if has-callq `() caller-save)])
        ; (printf "regs-left: ~a~n" regs-left)
        ; (printf "free-regs: ~a~n" free-regs)
        ; (printf "int-graph: ~a~n" int-graph)
        (let-values
          ([(mapping total-stack-slots-used)
            (reg-alloc-iter int-graph move-rels initial-mapping stack-slots-used free-regs)])
          (let* ([stack-size (* 8 total-stack-slots-used)]
                 [mapping (make-immutable-hash
                            (hash-map mapping
                                      (lambda (k v)
                                        (if (fixnum? v)
                                          (cons k (- (+ stack-size (* 8 (length callee-save)))
                                                     (* 8 (+ v 1 (length callee-save)))))
                                          (cons k v)))))])
            (values mapping stack-size)))))))

;; Returns (mapping, last stack location used)
(define (reg-alloc-iter int-graph move-rels mapping next-stack-loc regs)
  (let* [(all-vars    (list->set (hash-keys int-graph)))
         (mapped-vars (list->set (hash-keys mapping)))
         (not-mapped  (set-subtract all-vars mapped-vars))]
    (if (set-empty? not-mapped)
      (values mapping next-stack-loc)
      (let* [(most-constrained (find-most-constrained int-graph (set->list not-mapped)))

             (used-regs
               (list->set (filter-nulls
                            (map (lambda (nb) (hash-ref mapping nb '()))
                                 (set->list (adjacent int-graph most-constrained))))))

             (interfered-vars (hash-ref int-graph most-constrained (set)))

             (interfered-regs
               (filter-nulls
                 (map (lambda (arg) (hash-ref mapping arg '()))
                      (set->list interfered-vars))))

             (available-regs
               (filter (lambda (reg)
                         (and (not (set-member? used-regs reg))
                              (not (member reg interfered-regs))))
                         regs))

             ; (move-rel-vars (hash-ref move-rels most-constrained (set)))

             ; (move-rel-regs
             ;   (filter-nulls
             ;     (map (lambda (arg) (hash-ref mapping arg '()))
             ;          (set->list move-rel-vars))))

             ; (move-rel-regs (set->list
             ;                  (set-subtract (list->set move-rel-regs)
             ;                                (list->set interfered-regs))))
             ]

        ; (printf "regs: ~a~n" regs)
        ; (printf "most-constrained: ~a~n" most-constrained)
        ; (printf "used-regs: ~a~n" used-regs)
        ; (printf "interfered-vars: ~a~n" interfered-vars)
        ; (printf "interfered-regs: ~a~n" interfered-regs)
        ; (printf "available-regs: ~a~n" available-regs)

        (cond ; TODO: Disabling move relations. Will enable after making sure
              ; that the basics work.
              ; [(and (use-move-rels) (not (null? move-rel-regs)))
              ;  (reg-alloc-iter int-graph move-rels
              ;                  (hash-set mapping most-constrained (car move-rel-regs))
              ;                  next-stack-loc regs)]

              [(not (null? available-regs))
               (reg-alloc-iter int-graph move-rels
                               (hash-set mapping most-constrained (car available-regs))
                               next-stack-loc regs)]

              [#t
               (reg-alloc-iter int-graph move-rels
                               (hash-set mapping most-constrained next-stack-loc)
                               (+ 1 next-stack-loc)
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

(define (assign-homes pgm)
  (printf "============ assign homes ==========~n")
  (pretty-print pgm)
  (match pgm
    [`(program . ,defs)
     `(program ,@(map assign-homes-def defs))]
    [_ (unsupported-form 'assign-homes pgm)]))

(define (assign-homes-def def)
  (match def
    [`(define ,tag : ,ret-ty ,_ . ,instrs)
     (let-values [((def live-after-sets) (gen-live-afters def))]
       (let* ([int-graph (mk-interference-graph def live-after-sets)]
              [move-rel (mk-mov-rel-graph def int-graph)]
              [args (match tag
                      [`(,_ . ,args) (map car args)]
                      [_ `()])]
              [has-callq (any (lambda (instr) (eq? (car instr) 'callq)) instrs)])

         (let-values ([(homes stack-size) (reg-alloc args int-graph move-rel has-callq)])
           `(define ,tag : ,ret-ty (,(align-stack stack-size))
                    ; Note that here we use the original instructions, e.g.
                    ; not the ones with live-after annotations on if branches.
                    ; Either way should work here.
                    ,@(assign-home-instrs homes instrs)))))]

    [_ (unsupported-form 'assign-homes-def def)]))

(define (align-stack stack) (+ stack (modulo stack 16)))

(define (assign-home-instrs asgns instrs)
  (map (lambda (instr) (assign-home-instr asgns instr)) instrs))

(define (assign-home-instr asgns instr)
  (match instr
    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     `(if (eq? ,(assign-home-arg asgns arg1) ,(assign-home-arg asgns arg2))
        ,(assign-home-instrs asgns pgm-t)
        ,(assign-home-instrs asgns pgm-f))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,arg1 ,arg2)
     `(,(car instr) ,(assign-home-arg asgns arg1) ,(assign-home-arg asgns arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     `(,(car instr) ,(assign-home-arg asgns arg))]

    [`(callq ,arg) `(callq ,(assign-home-arg asgns arg))]

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
     `(offset ,(assign-home-arg asgns arg) ,offset)]
    [`(toplevel-fn ,_) arg]
    [`(var ,var)
     (let [(asgn (hash-ref asgns `(var ,var) '()))]
       (cond [(null? asgn)
              (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
             [(fixnum? asgn) `(stack ,asgn)]
             [#t `(reg ,asgn)]))]
    [_ (unsupported-form 'assign-home-arg arg)]))
