#lang racket

(require rackunit)

(require "../passes/utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; to-bit-list

(check-equal? (to-bit-list 1 8) '(1 0 0 0 0 0 0 0) "Bit-list of num is wrong")
(check-equal? (to-bit-list 4 8) '(0 0 1 0 0 0 0 0) "Bit-list of num is wrong")
(check-equal? (to-bit-list 5 8) '(1 0 1 0 0 0 0 0) "Bit-list of num is wrong")
(check-equal? (to-bit-list 255 8) '(1 1 1 1 1 1 1 1) "Bit-list of num is wrong")
(check-equal? (to-bit-list 0 8) '(0 0 0 0 0 0 0 0) "Bit-list of num is wrong")
(check-equal? (to-bit-list 63 6) '(1 1 1 1 1 1) "Bit-list of num is wrong")
(check-equal? (to-bit-list 10 6) '(0 1 0 1 0 0) "Bit-list of num is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bit-list-to-byte-list

(check-equal? (bit-list-to-byte-list '(1 1 1 1 1 1 1 1 1)) '(255 1) "Byte-list of bit-list is wrong")
(check-equal? (bit-list-to-byte-list '(1 1 1 1 1 1 1 1 1 1)) '(255 3) "Byte-list of bit-list is wrong")
(check-equal? (bit-list-to-byte-list '(1 0 1 1 1 1 1 1 1 1)) '(253 3) "Byte-list of bit-list is wrong")
(check-equal? (bit-list-to-byte-list '(1 0 1 0 1 1 1 1 1 1)) '(245 3) "Byte-list of bit-list is wrong")
(check-equal? (bit-list-to-byte-list '(1 0 1 0 1 1 1 1 0 1)) '(245 2) "Byte-list of bit-list is wrong")
(check-equal? (bit-list-to-byte-list
                '(1 0 1 0 1 1 1 1   1 1 1 1 1 1 1 1))
              '(245 255)
              "Byte-list of bit-list is wrong")

(check-equal? (bit-list-to-byte-list
                '(1 0 1 0 1 1 1 1   1 1 1 1 1 1 1 1    1))
              '(245 255 1)
              "Byte-list of bit-list is wrong")

(check-equal? (bit-list-to-byte-list '(0 0 1 0))
              '(4)
              "Byte-list of bit-list is wrong")

(check-equal? (bit-list-to-byte-list '(0 1 0 1 0 0 0 0   0 0 1 0))
              '(10 4)
              "Byte-list of bit-list is wrong")


(check-equal? (bit-list-to-byte-list '(0 1 0 1 0 0 0 0   0 0 1 0))
              '(10 4)
              "Byte-list of bit-list is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; encode-type-bits

(check-equal? (encode-type-bits '(Vector Integer Boolean))
              '(0 1 0 1 0 0 0 0   0 0 1 0)
              "Encoding of type is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; encode-type

(check-equal? (encode-type 'Integer) '(0) "Encoding of type is wrong")
(check-equal? (encode-type 'Boolean) '(1) "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer))
              '(6 0)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer Integer))
              '(10 0)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Integer))
              '(10 1)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer Boolean))
              '(10 4)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Boolean))
              '(10 5)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Boolean))
              '(10 5)
              "Encoding of type is wrong")
