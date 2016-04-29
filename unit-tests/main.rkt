#lang racket

(require rackunit)

(require "../passes/utils.rkt")
(require (only-in "../passes/closure-convert.rkt" fvs))

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
              '(0 1 0 0 1 0 0 0   0 0 0 1 0 0)
              "Encoding of type is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; encode-type

(check-equal? (encode-type 'Integer) '(0) "Encoding of type is wrong")
(check-equal? (encode-type 'Boolean) '(1) "Encoding of type is wrong")

(check-equal? (encode-type '(Vector))
              '(#b00000010)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer))
              '(#b00001010 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer Integer))
              '(#b00010010 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Integer))
              '(#b00010010 #b00000001)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Integer Boolean))
              '(#b00010010 #b00001000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Boolean))
              '(#b00010010 #b00001001)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Vector Boolean Boolean))
              '(#b00010010 #b00001001)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Integer -> Integer))
              '(#b00001011 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Integer Integer -> Integer))
              '(#b00010011 #b00000000 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Integer Integer Integer -> Integer))
              '(#b00011011 #b00000000 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Boolean Integer Integer -> Integer))
              '(#b00011011 #b00000001 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Boolean Boolean Integer -> Integer))
              '(#b00011011 #b00001001 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Boolean Boolean Boolean -> Integer))
              '(#b00011011 #b01001001 #b00000000)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Boolean Boolean Boolean -> Boolean))
              '(#b00011011 #b01001001 #b00000010)
              "Encoding of type is wrong")

(check-equal? (encode-type '(Any -> Any))
              '(#b00001011 #b00100100)
              "Encoding of type is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; byte-list-to-quadword-list

(check-equal? (byte-list-to-quadword-list '(255))
              '(255)
              "Encoding of type is wrong")

(check-equal? (byte-list-to-quadword-list '(255 255))
              (list (+ 255 (* 255 256)))
              "Encoding of type is wrong")

(check-equal? (byte-list-to-quadword-list '(0 255))
              (list (* 255 256))
              "Encoding of type is wrong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fvs

(define (filter-names lst) (map cdr lst))

(check-equal? (fvs `((Integer -> Integer) . (lambda: [(x : Integer)] : Integer (Integer . x))))
              (set))

(check-equal? (fvs `(Integer . (let ([x (Integer . 10)])
                                 (Integer . (+ (Integer . x) (Integer . y))))))
              (set `(Integer . y)))
