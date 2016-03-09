(define (id [i : Integer]) : Integer
  i)

(define (add2 [i1 : Integer]
              [i2 : Integer]) : Integer
  (+ (id i1) (id i2)))

(define (add3 [i1 : Integer]
              [i2 : Integer]
              [i3 : Integer]) : Integer
  (+ (id i1) (add2 i2 i3)))

(add3 1 2 39)

; (define (add4 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]) : Integer
;   (+ (id i1) (add3 i2 i3 i4)))
;
; (define (add5 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]
;               [i5 : Integer]) : Integer
;   (+ (id i1) (add4 i2 i3 i4 i5)))
;
; (define (add6 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]
;               [i5 : Integer]
;               [i6 : Integer]) : Integer
;   (+ (id i1) (add5 i2 i3 i4 i5 i6)))
;
; (add6 2 6 7 8 9 10)



; (define (add7 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]
;               [i5 : Integer]
;               [i6 : Integer]
;               [i7 : Integer]) : Integer
;   (+ (id i1) (add6 i2 i3 i4 i5 i6 i7)))
;
; (define (add8 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]
;               [i5 : Integer]
;               [i6 : Integer]
;               [i7 : Integer]
;               [i8 : Integer]) : Integer
;   (+ (id i1) (add7 i2 i3 i4 i5 i6 i7 i8)))
;
; (define (add9 [i1 : Integer]
;               [i2 : Integer]
;               [i3 : Integer]
;               [i4 : Integer]
;               [i5 : Integer]
;               [i6 : Integer]
;               [i7 : Integer]
;               [i8 : Integer]
;               [i9 : Integer]) : Integer
;   (+ (id i1) (add8 i2 i3 i4 i5 i6 i7 i8 i9)))
;
; (add9 1 2 3 4 5 6 7 8 6)
