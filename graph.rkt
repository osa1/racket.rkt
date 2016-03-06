#lang racket

; A hash-table based mutable graph implementation.

(require "passes/utils.rkt")

(provide (all-defined-out))

(define mk-graph make-hash)

(define (add-edge graph node1 node2)
  (let ([node1-set (hash-ref! graph node1 set)]
        [node2-set (hash-ref! graph node2 set)])
    (let ([node1-set (set-add node1-set node2)]
          [node2-set (set-add node2-set node1)])
      (hash-set! graph node1 node1-set)
      (hash-set! graph node2 node2-set))))

(define (add-node graph node)
  (hash-set! graph node (set)))

(define (remove-node graph node)
  (let ([nbs (hash-ref! graph node set)])
    (for ([nb (set->list nbs)])
      (let ([nb-set (hash-ref! graph nb #f)])
        (when nb-set
          (hash-set! graph nb (set-remove nb-set node)))))
    (let ([ret (hash-ref graph node)])
      (hash-remove! graph node)
      ret)))

(define (remove-edge graph node1 node2)
  (define node1-nbs (hash-ref! graph node1 #f))
  (when node1-nbs
    (hash-set! graph node1 (set-remove node1-nbs node2)))
  (define node2-nbs (hash-ref! graph node2 #f))
  (when node2-nbs
    (hash-set! graph node2 (set-remove node2-nbs node1))))

(define (replace-node graph node-old node-new)
  (define old-node-nbs (remove-node graph node-old))
  (for ([nb old-node-nbs])
    (add-edge node-new nb)))

(define graph-copy hash-copy)

(define nodes hash-keys)

(define (has-edge? graph node1 node2)
  (set-member? (neighbors graph node1) node2))

(define (has-node? graph node)
  (hash-ref graph node #f))

(define (neighbors graph node)
  (set->list (hash-ref! graph node set)))

(define (node-degree graph node)
  (set-count (hash-ref! graph node set)))

(define graph-elems hash->list)

(define graph-size hash-count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-find-min-degree graph)
  (define node-degrees (map (lambda (n) (cons (car n) (set-count (cdr n)))) (nodes graph)))
  (min-by cdr node-degrees))

(define (graph-find-max-degree graph)
  (define node-degrees (map (lambda (n) (cons (car n) (set-count (cdr n)))) (nodes graph)))
  (max-by cdr node-degrees))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-dot graph file-name fn [generate-png #t])
  (call-with-output-file
    file-name #:exists 'replace
    (lambda (out-file)
      (write-string "strict graph {" out-file) (newline out-file)

      (for ([n (nodes graph)])
        (write-string (format "~a;\n" (fn n)) out-file))

      (for ([n (nodes graph)])
        (for ([u (set->list (neighbors graph n))])
          (write-string (format "~a -- ~a;\n" (fn u) (fn n)) out-file)))

      (write-string "}" out-file)
      (newline out-file)))

  (when generate-png
    (system (format "dot -Tpng ~a -O" file-name))))
