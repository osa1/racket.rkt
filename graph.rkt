#lang racket

; A hash-table based mutable graph implementation.

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

(define nodes hash-keys)

(define (has-edge? graph node1 node2)
  (set-member? (neighbors graph node1) node2))

(define (neighbors graph node)
  (set->list (hash-ref! graph node set)))

(define (node-degree graph node)
  (set-count (hash-ref! graph node set)))

(define graph-elems hash->list)

(define graph-size hash-count)

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
