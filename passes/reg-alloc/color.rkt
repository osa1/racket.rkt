#lang racket

(require "live-after.rkt")
(require "interference-graph.rkt")
(require "move-rel.rkt")
(require "assign-homes.rkt")
(require (only-in "../instr-sel.rkt" collect-vars collect-vars-instrs))

(require "../utils.rkt")
(require "../../graph.rkt")

(provide simplify)

; For testing purposes
(require "../typecheck.rkt")
(require "../desugar.rkt")
(require "../choose-branch.rkt")
(require "../uniquify.rkt")
(require "../reveal-functions.rkt")
(require "../flatten.rkt")
(require "../expose-allocations.rkt")
(require "../annotate-lives.rkt")
(require "../uncover-call-live-roots.rkt")
(require "../instr-sel.rkt")
(require "../initialize-rts.rkt")
(require (only-in "../../public/utilities.rkt" read-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify graph num-available-regs)

  (define (loop key)
    (let ([ns (neighbors graph key)])
      (if (< (length ns) num-available-regs)
        (let ([node-nbs (remove-node graph key)])
          ; (printf "removing ~a~n" key)
          (cons key (set->list node-nbs)))
        #f)))

  (let ([iter (filter id (map loop (hash-keys graph)))])
    (if (null? iter)
      '()
      (append (simplify graph num-available-regs) iter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 'num-available-regs' is for sanity checking only. 'spill' doesn't really need
; to know 'k' as we should only run it when we can't simplify the graph
; further.
(define (spill graph num-available-regs)

  ; Make sure the graph can't be simplified - otherwise we have a bug.

  (let* ([graph-nodes (nodes graph)]
         [degrees (map (lambda (node) (node-degree graph node)) graph-nodes)])
    (unless (all (lambda (d) (>= d num-available-regs)) degrees)
      (error 'spill "Input graph can be simplified further: ~n~a~n" graph)))

  ; Just pick a node and remove it from the graph.

  (if (= (graph-size graph) 0)
    #f
    (let ([node (car (nodes graph))])
      (cons node (set->list (remove-node graph node))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Run simplify and spill until we have a work stack.
; 'iteration' is used for debugging.
(define (simplify-spill-loop graph num-available-regs [iteration 0])

  (define simplify-work-stack (simplify graph num-available-regs))
  (print-dot graph (format "iter-~a-simpl.dot" iteration) cadr)

  ; Simplifier recursively runs until it can't simplify anymore.
  (define spilled (spill graph num-available-regs))
  (print-dot graph (format "iter-~a-spill.dot" iteration) cadr)

  ; We need to keep simplifying after a successful spill.
  (if spilled
    (append (simplify-spill-loop graph num-available-regs (+ iteration 1))
            (list spilled)
            simplify-work-stack)
    (begin
      ; A sanity check: Spill failed so at this point the graph should be
      ; empty.
      (unless (= (graph-size graph) 0)
        (error 'simplify-spill-loop "Spill failed, but graph is not empty: ~a~n" graph))

      simplify-work-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Select returns two things:
;
; - An interference graph, which should be the same as the original one.
;   (can be used for debugging)
;
; - A hash table of variable to register mapping. If a variable is not in the
;   table, it means that we actully need to spill the variable.
;
(define (select work-stack num-available-regs)

  ; We build the interference graph again, using our work stack.
  (define int-graph (mk-graph))

  ; Variable-to-register mapping.
  (define mapping (make-hash))

  (define reg-set (list->set (range num-available-regs)))

  (define (select-iter work)
    (define node (car work))
    (define nbs (set->list (cdr work)))

    ; Register used by the interfering variables.
    (define used-regs
      (list->set
        (filter id (map (lambda (nb) (hash-ref mapping nb #f)) nbs))))

    (define avail-regs (set->list (set-subtract reg-set used-regs)))

    ; Map the variable, if possible.
    (unless (null? avail-regs)
      (hash-set! mapping node (car avail-regs)))

    ; Rebuild the interference graph.
    ; Some nodes don't interfere with any others, so we need this step.
    (add-node int-graph node)
    (for ([nb nbs])
      (add-edge int-graph node nb)))

  (for ([work work-stack])
    (select-iter work))

  (values int-graph mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coalesce int-graph mov-rel num-available-regs)

  ; George's method, it's easier to implement than Briggs'.

  ; Note that move-relation graph doesn't have edges between interfering nodes,
  ; so we don't need to check that here.

  (define (check-coalesce current-key move-related-key)
    ; FIXME: We do this lookup and list->set in every iteration for no reason.
    (let ([nbs (neighbors int-graph current-key)])
      (if (all
            ; For every neighbor t of a
            (lambda (nb)
              (or ; either t already interferes with b
                  (has-edge? int-graph nb move-related-key)
                  ; or t is of insignificant degree
                  (< (node-degree int-graph nb) num-available-regs)))
            nbs)
        move-related-key
        #f)))

  ; We need to check every move-related pair. We stop once we find one, to
  ; simplify and coalesce again.

  ; TODO: Any better ways to implement early returns in Racket? Maybe call/cc?

  (define (loop-nodes keys)
    (match keys
      [`() #f]
      [`(,key . keys)
       (let ([ret (loop-move-rels key (neighbors mov-rel key))])
         (if ret
           ret
           (loop-nodes keys)))]))

  (define (loop-move-rels key move-rels)
    (match move-rels
      [`() #f]
      [`(,move-rel . ,move-rels)
       (let ([ret (check-coalesce key move-rel)])
         (if ret
           (cons key ret)
           (loop-move-rels key move-rels)))]))

  (loop-nodes (nodes int-graph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reg-alloc pgm-name)
  (lambda (pgm)
    (match pgm
      [`(program . ,defs)
       (let ([defs (map (lambda (def)
                          (let-values ([(def mapping) (reg-alloc-def pgm-name def)])
                            ; (printf "register mapping:~n")
                            ; (pretty-print mapping)
                            (assign-homes def mapping))) defs)])
         `(program ,@defs))]
      [_ (unsupported-form 'reg-alloc pgm)])))

; Returns two things:
;
; - A new program which may have some new spill instructions.
; - A mapping from variables to registers.
;
(define (reg-alloc-def pgm-name def)
  (define (reg-alloc-iter def [next-mem-loc 0] [iteration 0])

    ; Simplify and spill. Upon finding a variable that actually needs to be
    ; spilled, map the variable to a memory location (memory locations start
    ; from 0 and increases) and insert movq's to read the variable from
    ; memory and write it back.

    (match def
      [`(define ,tag : ,ret-ty ,vs . ,instrs)
       (printf "~nregister allocating for program:~n")
       (pretty-print def)

       (define-values (def-w-lives live-after-sets) (gen-live-afters def))

       (define int-graph (mk-interference-graph def-w-lives live-after-sets))
       ; Copy the original interference graph, to be used for debugging later
       (define int-graph-copy (graph-copy int-graph))

       (define move-rels (mk-mov-rel-graph def-w-lives int-graph))

       ; (printf "~nlive-after sets:~n")
       ; (pretty-print live-after-sets)
       ; (printf "~ninterference graph:~n")
       ; (pretty-print int-graph)
       (print-dot int-graph (string-append pgm-name (format "-int-orig-~a.dot" iteration)) cadr)
       ; (printf "~nmove-relation graph:~n")
       ; (pretty-print move-rels)
       (print-dot move-rels (string-append pgm-name (format "-mov-~a.dot" iteration)) cadr)

       (define work-stack (simplify-spill-loop int-graph 2))
       (define-values (int-graph-rebuilt mapping) (select work-stack 2))

       ; The rebuilt interference graph should be the same as the original one
       ; FIXME: Disabling this for now. For some reason %rax is appearing in
       ; the rebuilt one.
       ; (unless (equal? int-graph-copy int-graph-rebuilt)
       ;   (error 'reg-alloc-def
       ;          "Rebuilt interference graph is different from the original one:~n~a~n~a~n"
       ;          int-graph-copy
       ;          int-graph-rebuilt))

       (define mapped-vars (list->set (hash-keys mapping)))
       (define spilled-vars
         (set->list
           (set-subtract
             (list->set (map (lambda (v) (list 'var v)) vs)) mapped-vars)))

       ; (printf "work-stack:~n")
       ; (pretty-print work-stack)
       ; (printf "mapping:~n")
       ; (pretty-print mapping)
       ; (printf "mapped:~n")
       ; (pretty-print mapped-vars)
       ; (printf "spilled:~n")
       ; (pretty-print spilled-vars)

       (print-dot int-graph-rebuilt
                  (string-append pgm-name (format "-final-~a.dot" iteration)) cadr)

       (if (not (null? spilled-vars))
         ; Generate spill instructions, loop
         (let* ([instrs-w-spills (generate-spills (car spilled-vars) next-mem-loc instrs)]
                [pgm-vars (set->list (collect-vars-instrs instrs))])
           (printf "generated spills, new vars: ~a~n" pgm-vars)
           (reg-alloc-iter `(define ,tag : ,ret-ty ,pgm-vars ,@instrs-w-spills)
                           (+ next-mem-loc 1) (+ iteration 1)))
         ; We're done
         (values def mapping))]

      [_ (unsupported-form 'reg-alloc-def def)]))

  (reg-alloc-iter def))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-spills var mem-loc instrs)
  (define var-sym (cadr var))
  (define mem-loc-arg `(mem-loc ,mem-loc))

  (printf "spilling ~a~n" var-sym)

  (define temp-prefix (string-append "spill_" (symbol->string var-sym)))
  (define (mk-temp-var) `(var ,(gensym temp-prefix)))

  (define (gen-spill instr)
    (match instr

      ; TODO: What happens if both s and d are the var to spill?

      [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,s ,d)
       (define temp-var (mk-temp-var))
       (cond
         [(equal? s var)
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,temp-var ,d))]

         [(equal? d var)
          `((movq ,s ,temp-var)
            (movq ,temp-var ,mem-loc-arg))]

         [#t `(,instr)])]

      [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
       (define temp-var (mk-temp-var))
       (cond
         [(equal? arg1 var)
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,temp-var ,arg1))]
         [(equal? arg2 var)
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,temp-var ,arg2))]
         [#t `()])

       `(,@spill
         (if (eq? ,arg1 ,arg2)
           ,(map gen-spill pgm-t)
           ,(map gen-spill pgm-f)))]

      [`(negq ,arg1)
       (if (equal? arg1 var)
         (let ([temp-var (mk-temp-var)])
           `((movq ,mem-loc-arg ,temp-var)
             (negq ,temp-var)
             (movq ,temp-var ,mem-loc-arg)))
         `(,instr))]

      [`(callq ,arg1)
       (if (equal? arg1 var)
         (let ([temp-var (mk-temp-var)])
           `((movq ,mem-loc-arg ,temp-var)
             (callq ,temp-var)))
         `(,instr))]

      [`(retq) `(,instr)]

      [`(movzbq (byte-reg al) ,arg1)
       (if (equal? arg1 var)
         (let ([temp-var (mk-temp-var)])
           `((movq ,mem-loc-arg ,temp-var)
             (movzbq (byte-reg al) ,temp-var)
             (movq ,temp-var ,mem-loc-arg)))
         `(,instr))]

      [_ (unsupported-form 'gen-spill instr)]))

  (append-map gen-spill instrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(define (reg-alloc-test file)
  (unless (file-exists? file)
    (error 'reg-alloc-test "File does not exist: ~a" file))

  (let ([path (string->path file)])
    (let-values ([(_1 name _2) (split-path path)])
      (let ([name (car (string-split (path->string name) "."))]
            [pgm (read-program file)])

        ((reg-alloc name)
          (initialize-rts
            (instr-sel
              (uncover-call-live-roots
                (annotate-lives
                  (expose-allocations
                    (flatten
                      (reveal-functions
                        (uniquify
                          (desugar
                            (typecheck pgm)))))))))))))))

(define test-graph (mk-graph))

(add-edge test-graph 'j 'f)
(add-edge test-graph 'j 'e)
(add-edge test-graph 'j 'k)
(add-edge test-graph 'j 'h)
(add-edge test-graph 'j 'g)
(add-edge test-graph 'j 'd)
(add-edge test-graph 'h 'g)
(add-edge test-graph 'k 'g)
(add-edge test-graph 'k 'd)
(add-edge test-graph 'k 'b)
(add-edge test-graph 'f 'e)
(add-edge test-graph 'f 'm)
(add-edge test-graph 'd 'b)
(add-edge test-graph 'd 'm)
(add-edge test-graph 'c 'b)
(add-edge test-graph 'c 'm)
(add-edge test-graph 'e 'b)
(add-edge test-graph 'e 'm)
(add-edge test-graph 'b 'm)

(simplify test-graph 3)

test-graph

(pretty-print (reg-alloc-test "tests/uniquify_5.rkt"))
