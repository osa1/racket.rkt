#lang racket

(require "live-after.rkt")
(require "interference-graph.rkt")
(require "move-rel.rkt")
(require "remove-mov.rkt")
(require "spill.rkt")
(require "assign-homes.rkt")
(require (only-in "../instr-sel.rkt" collect-vars collect-vars-instrs))

(require "../utils.rkt")
(require "../../graph.rkt")

(provide reg-alloc debug-reg-alloc)

; For testing purposes
(require "../typecheck.rkt")
(require "../desugar.rkt")
(require "../choose-branch.rkt")
(require "../uniquify.rkt")
(require "../reveal-functions.rkt")
(require "../flatten.rkt")
(require "../initialize-rts.rkt")
(require "../expose-allocations.rkt")
(require "../annotate-lives.rkt")
(require "../uncover-call-live-roots.rkt")
(require "../instr-sel.rkt")
(require (only-in "../../public/utilities.rkt" read-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define debug-reg-alloc (make-parameter #f))

(define (debug-printf . args)
  (when (debug-reg-alloc)
    (apply printf args)))

(define (debug-pretty-print . args)
  (when (debug-reg-alloc)
    (apply pretty-print args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify graph move-rels num-available-regs)

  (define (loop key)
    (let ([ns (neighbors graph key)])
      (if (and (< (length ns) num-available-regs) (not (has-node? move-rels key)))
        (let ([node-nbs (remove-node graph key)])
          (debug-printf "simplify: ~a~n" key)
          (cons key node-nbs))
        (begin
          ; (debug-printf "can't simplify! num-available-regs: ~a graph:~n" num-available-regs)
          ; (debug-pretty-print graph)
          (debug-printf "simplify: can't simplify.~n")
          #f))))

  (let ([iter (filter id (map loop (filter not-reg? (nodes graph))))])
    (if (null? iter)
      '()
      (append (simplify graph move-rels num-available-regs) (reverse iter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coalesce int-graph mov-rel num-available-regs)

  ; George's method, it's easier to implement than Briggs'.

  ; Note that move-relation graph doesn't have edges between interfering nodes,
  ; so we don't need to check that here.

  ; TODO: Should we coaslesce as much as possible before returning? Currently
  ; coalescing once at most.

  (define (check-coalesce current-key move-related-key)
    ; FIXME: We do this lookup and list->set in every iteration for no reason.
    (let ([nbs (neighbors int-graph current-key)])
      (if (all
            ; For every neighbor t of a
            (lambda (nb)
              (debug-printf "checking coalesce between ~a and ~a~n" current-key nb)
              (or ; either t already interferes with b
                  (has-edge? int-graph nb move-related-key)
                  ; or t is of insignificant degree
                  (< (node-degree int-graph nb) num-available-regs)))
            nbs)
        (begin
          ; (debug-printf "maybe coalesce ~a and ~a?~n" current-key move-related-key)
          move-related-key)
        #f)))

  ; We need to check every move-related pair. We stop once we find one, to
  ; simplify and coalesce again.

  ; TODO: Any better ways to implement early returns in Racket? Maybe call/cc?

  (define (loop-nodes keys)
    (match keys
      [`() #f]
      [`(,key . ,keys)
       (let ([ret (loop-move-rels key (neighbors mov-rel key))])
         (if ret
           ret
           (loop-nodes keys)))]
      [_ (unsupported-form 'loop-nodes keys)]))

  (define (loop-move-rels key move-rels)
    (match move-rels
      [`() #f]
      [`(,move-rel . ,move-rels)
       (let ([ret (check-coalesce key move-rel)])
         (if ret
           (cons key ret)
           (loop-move-rels key move-rels)))]
      [_ (unsupported-form 'loop-move-rels move-rels)]))

  (loop-nodes (filter not-reg? (nodes int-graph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Run simplify and coalesce until coalesce can't merge any more nodes.
(define (simplify-coalesce-loop instrs work-set graph move-rels num-available-regs
                                cs [iteration 0])
  (define simplify-work (simplify graph move-rels num-available-regs))
  ; (print-dot graph (format "scl-int-~a.dot" iteration) cadr)

  (define coalesce-mb
    (if cs
      (coalesce graph move-rels num-available-regs)
      #f))
  ; (print-dot graph (format "scl-int-coal-~a.dot" iteration) cadr)

  (if coalesce-mb
    (let ([node1 (car coalesce-mb)]
          [node2 (cdr coalesce-mb)])

      (debug-printf "coalesce: ~a ~a~n" node1 node2)

      ; As a sanity check, make sure node1 is in both the interference graph
      ; and move-relation graph.
      (unless (and (has-node? graph node1) (has-node? move-rels node1))
        (error 'simplify-coalesce-loop
               "Coalesced node is not in graph/move-rels: ~a" node1))

      ; If we're coalescing to a register, we need to do different updates in
      ; the graphs and we need to make sure we map the variable to the
      ; coalesced register.
      (if (is-reg? node2)

        ; We do a hackish thing here and push a special thing to the work set.
        ; 'select' needs to be aware of this to be able to map the variable to
        ; the reg.
        ;
        ; An alternative implementation could return a mapping but that needs
        ; more refactoring (need to update the call stack:
        ; 'simplify-coalesce-freeze-loop', 'reg-alloc-def'...)

        (let ([instrs (remove-mov node1 node2 node2 instrs)])
          (debug-printf "program after coalescing:~n")
          (debug-pretty-print instrs)

          ; Update graphs
          (replace-node graph node1 node2)
          (replace-node move-rels node1 node2)

          ; Update work set
          ; TODO: This code is too similary to the work-set update in the other
          ; branch.
          (let ([work-set
                  (map (lambda (work)
                         (let* ([nbs (cdr work)]
                                [was-nb (set-member? nbs node1)]
                                [new-set
                                  (if was-nb
                                    (set-add (set-remove nbs node1) node2)
                                    nbs)])
                           (cons (car work) new-set)))
                       (append simplify-work work-set))])

            ; Loop
            (simplify-coalesce-loop instrs work-set graph move-rels num-available-regs
                                    cs (+ iteration 1))))

        ; Update instructions (remove movs), update move-relation graph (remove the
        ; relation), update interference graph, update work stack, loop.
        ;
        ; 'coalesce' doesn't do any updates so we do it here.

        (let* ([new-node
                 `(var ,(gensym (string-append "c-"
                                               (symbol->string (car node1))
                                               "_"
                                               (symbol->string (car node2)))))]

               ; Update the program
               [instrs (remove-mov node1 node2 new-node instrs)])

          (debug-printf "program after coalescing:~n")
          (debug-pretty-print instrs)

          (when (is-reg? node1)
            (error "Coalescing a reg: ~a ~a~n" node1 node2))

          ; Update interference graph
          (add-node graph new-node)
          (replace-node graph node1 new-node)
          (replace-node graph node2 new-node)
          (remove-node graph node1)
          (remove-node graph node2)

          ; Update move-relation graph
          (remove-edge move-rels node1 node2)
          (add-node move-rels new-node)
          (replace-node move-rels node1 new-node)
          (replace-node move-rels node2 new-node)
          (remove-node move-rels node1)
          (remove-node move-rels node2)

          ; Update work set
          ; Work set can't have coalesced nodes as we don't remove move-related
          ; nodes in the simplify step.
          (let ([work-set
                  (map (lambda (work)
                         (let* ([nbs (cdr work)]
                                [was-nb (or (set-member? nbs node1) (set-member? nbs node2))]
                                [new-set
                                  (if was-nb
                                    (set-add (set-remove (set-remove nbs node1) node2) new-node)
                                    nbs)])
                           (cons (car work) new-set)))
                       (append simplify-work work-set))])

            ; Loop
            (simplify-coalesce-loop instrs work-set graph move-rels num-available-regs
                                    cs (+ iteration 1))))))

    ; End of loop, return update instructions and work set
    (values instrs (append simplify-work work-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Freeze chooses a node with non-significant degree from the graph.
; (weird name, keeping it to stay compatible with the book)
(define (freeze graph move-rels num-available-regs)
  (define move-related-nodes
    (set-intersect
      (list->set (filter not-reg? (nodes move-rels)))
      (list->set (filter not-reg? (nodes graph)))))

  (define node-degrees
    (set-map move-related-nodes (lambda (node) (cons node (node-degree graph node)))))

  (define (find-min nodes [min-so-far #f])
    (match nodes
      [`() (if (eq? min-so-far #f) #f min-so-far)]
      [`((,node . ,degree) . ,rest)
       (if (or (eq? min-so-far #f) (< degree (cdr min-so-far)))
         (find-min rest (cons node degree))
         (find-min rest min-so-far))]))

  (define min (find-min node-degrees))

  (if min
    (if (< (cdr min) num-available-regs)
      (car min)
      #f)
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-coalesce-freeze-loop
          instrs work-set graph move-rels num-available-regs coalesce [iteration 0])
  (let-values ([(instrs work-set)
                (simplify-coalesce-loop instrs work-set graph move-rels num-available-regs coalesce)])

    ; (print-dot graph (format "scfl-~a.dot" iteration) cadr)

    ; We're done if the graph is empty
    (if (eq? (length (filter not-reg? (nodes graph))) 0)
      (values instrs work-set)

      ; Otherwise, try to freeze (remove a move-relation)
      (let ([freeze-node (freeze graph move-rels num-available-regs)])
        (if freeze-node

          (begin
            (debug-printf "freeze: ~a~n" freeze-node)
            (remove-node move-rels freeze-node)
            (simplify-coalesce-freeze-loop
              instrs work-set graph move-rels num-available-regs (+ iteration 1)))

          ; Can't freeze, push the node to the stack as a potential spill,
          ; restart the loop.
          (let* ([node-to-spill (car (graph-find-max-degree graph not-reg?))]
                 [nbs (remove-node graph node-to-spill)]
                 [work-set (cons (cons node-to-spill nbs) work-set)])
            (debug-printf "spill: ~a~n" node-to-spill)
            (simplify-coalesce-freeze-loop
              instrs work-set graph move-rels num-available-regs (+ iteration 1))))))))

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

  ; (define reg-set (list->set (range num-available-regs)))
  (define reg-set all-regs-set)

  (define (select-iter work)
    (define node (car work))
    (define nbs-set (cdr work))

    ; (debug-printf "work ~a~n" node)
    (when (is-reg? node)
      (error 'select "Can't work a reg: ~a~n~a" node work-stack))

    (if (set? nbs-set)
      (let* ([nbs (set->list nbs-set)]
             ; Register used by the interfering variables.
             [used-regs
               (list->set
                 (map mk-reg
                      (filter id (map (lambda (nb)
                                        (if (is-reg? nb)
                                          (reg-sym nb)
                                          (hash-ref mapping nb #f))) nbs))))]

             [avail-regs (set->list (set-subtract reg-set used-regs))])

        (debug-printf "==== select ====~n")
        (debug-printf "node: ~s~n" node)
        (debug-printf "interfering nodes: ~s~n" nbs)
        (debug-printf "mapping: ~s~n" mapping)
        (debug-printf "used-regs: ~s~n" used-regs)
        (debug-printf "avail-regs: ~s~n" avail-regs)

        ; Map the variable, if possible.
        (if (null? avail-regs)
          (debug-printf "not mapping~n")
          (begin
            (hash-set! mapping node (reg-sym (car avail-regs)))
            (debug-printf "mapped to: ~a~n" (reg-sym (car avail-regs)))))

        (debug-printf "================~n")

        ; Rebuild the interference graph.
        ; Some nodes don't interfere with any others, so we need this step.
        (add-node int-graph node)
        (for ([nb nbs])
          (add-edge int-graph node nb)))

      (begin
        (error 'select "coalescing to register ~a ~a~n" node nbs-set))))

  (for ([work work-stack])
    (select-iter work))

  (values int-graph mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reg-alloc pgm-name)
  (lambda (pgm)
    (match pgm
      [`(program . ,defs)
       (let ([defs (map (lambda (def)
                          (let-values ([(def mapping) (reg-alloc-def pgm-name def)])
                            ; (debug-printf "register mapping:~n")
                            ; (debug-pretty-print mapping)
                            (assign-homes def mapping))) defs)])
         `(program ,@defs))]
       ; (for ([def defs])
       ;   (reg-alloc-def pgm-name def))
       ; `(program ,@defs)]
      [_ (unsupported-form 'reg-alloc pgm)])))

; Returns two things:
;
; - A new program which may have some new spill instructions.
; - A mapping from variables to registers.
;
(define (reg-alloc-def pgm-name def)
  (define (reg-alloc-iter def [cs #t] [last-mem-loc 0] [iteration 0])

    ; Run simplify and coalesce loop. The loop return only when it can't
    ; coalesce anymore, and simplify only returns after simplifying as much as
    ; possible. At this point we remove a move relation (``freeze'') and
    ; restart the loop.

    (match def
      [`(define ,tag : ,ret-ty . ,instrs)
       (debug-printf "===============================~n")
       (debug-printf "register allocation iteration ~a~n" iteration)
       (debug-pretty-print def)

       (define-values (def-w-lives live-after-sets) (gen-live-afters def))

       (define int-graph (mk-interference-graph def-w-lives live-after-sets))
       ; Copy the original interference graph, to be used for debugging later
       ;(define int-graph-copy (graph-copy int-graph))

       (define move-rels (mk-mov-rel-graph def-w-lives int-graph))

       ; (debug-printf "~nlive-after sets:~n")
       ; (debug-pretty-print live-after-sets)
       (debug-printf "interference graph:~n")
       (debug-pretty-print int-graph)
       ; (print-dot int-graph (string-append pgm-name (format "-int-orig-~a.dot" iteration)) cadr)
       (debug-printf "move-relation graph:~n")
       (debug-pretty-print move-rels)
       ; (print-dot move-rels (string-append pgm-name (format "-mov-~a.dot" iteration)) cadr)

       (define-values (coalesced-instrs work-stack)
         (simplify-coalesce-freeze-loop instrs `() int-graph move-rels 5 cs))

       (define-values (int-graph-rebuilt mapping) (select work-stack 5))

       ; The rebuilt interference graph should be the same as the original one
       ; FIXME: Disabling this for now. For some reason %rax is appearing in
       ; the rebuilt one.
       ; (unless (equal? int-graph-copy int-graph-rebuilt)
       ;   (error 'reg-alloc-def
       ;          "Rebuilt interference graph is different from the original one:~n~a~n~a~n"
       ;          int-graph-copy
       ;          int-graph-rebuilt))

       (define mapped-vars (list->set (map cadr (hash-keys mapping))))
       (define all-vars (collect-vars-instrs coalesced-instrs))
       (define spilled-vars (set->list (set-subtract all-vars mapped-vars)))

       ; (debug-printf "work-stack:~n")
       ; (debug-pretty-print work-stack)
       ; (debug-printf "coalesced instrs:~n")
       ; (debug-pretty-print coalesced-instrs)
       ; (debug-printf "mapping:~n")
       ; (debug-pretty-print mapping)
       ; (debug-printf "mapped:~n")
       ; (debug-pretty-print mapped-vars)
       ; (debug-printf "all-vars:~n")
       ; (debug-pretty-print all-vars)
       ; (debug-printf "spilled:~n")
       ; (debug-pretty-print spilled-vars)
       ; (debug-printf "~n~n~n~n~n")

       ; (print-dot int-graph-rebuilt
       ;            (string-append pgm-name (format "-final-~a.dot" iteration)) cadr)

       (if (not (null? spilled-vars))
         ; Generate spill instructions, loop
         (let ([instrs-w-spills
                 (generate-spills (car spilled-vars) (+ last-mem-loc 1) coalesced-instrs)])
           ; (debug-printf "instructions before actual spill:~n")
           ; (debug-pretty-print coalesced-instrs)
           (debug-printf "instructions after actual spill:~n")
           (debug-pretty-print instrs-w-spills)
           (reg-alloc-iter `(define ,tag : ,ret-ty ,@instrs-w-spills)
                           #t (+ last-mem-loc 1) (+ iteration 1)))
         ; We're done
         (let ([def `(define ,tag : ,ret-ty ,last-mem-loc ,@coalesced-instrs)])
           (values def mapping)))]

      [_ (unsupported-form 'reg-alloc-def def)]))

  (reg-alloc-iter def))

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
         (instr-sel
           (uncover-call-live-roots
             (annotate-lives
               (expose-allocations
                 (initialize-rts
                   (flatten
                     (reveal-functions
                       (uniquify
                         (desugar
                           (typecheck pgm)))))))))))))))

; (define test-graph (mk-graph))
;
; (add-edge test-graph 'j 'f)
; (add-edge test-graph 'j 'e)
; (add-edge test-graph 'j 'k)
; (add-edge test-graph 'j 'h)
; (add-edge test-graph 'j 'g)
; (add-edge test-graph 'j 'd)
; (add-edge test-graph 'h 'g)
; (add-edge test-graph 'k 'g)
; (add-edge test-graph 'k 'd)
; (add-edge test-graph 'k 'b)
; (add-edge test-graph 'f 'e)
; (add-edge test-graph 'f 'm)
; (add-edge test-graph 'd 'b)
; (add-edge test-graph 'd 'm)
; (add-edge test-graph 'c 'b)
; (add-edge test-graph 'c 'm)
; (add-edge test-graph 'e 'b)
; (add-edge test-graph 'e 'm)
; (add-edge test-graph 'b 'm)

; (simplify test-graph 3)

; test-graph

; (debug-pretty-print (reg-alloc-test "tests/uniquify_2.rkt"))
