#lang racket

(require "../utils.rkt")

(provide generate-spills)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-spills var-sym mem-loc instrs)
  (define mem-loc-arg `(mem-loc ,mem-loc))
  (define var `(var ,var-sym))

  (printf "spilling ~a~n" var-sym)

  (define temp-prefix (string-append "spill_" (symbol->string var-sym)))
  (define (mk-temp-var) `(var ,(fresh temp-prefix)))

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
       (define c
         (cond
           [(equal? arg1 var)
            `((movq ,mem-loc-arg ,temp-var)
              (movq ,temp-var ,arg1))]
           [(equal? arg2 var)
            `((movq ,mem-loc-arg ,temp-var)
              (movq ,temp-var ,arg2))]
           [#t `()]))
       `(,@c
         (if (eq? ,arg1 ,arg2)
           ,(append-map gen-spill pgm-t)
           ,(append-map gen-spill pgm-f)))]

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

      [`(,(or 'sete 'setl) (byte-reg al))
       `(,instr)]

      [`(movzbq (byte-reg al) ,arg1)
       (if (equal? arg1 var)
         (let ([temp-var (mk-temp-var)])
           `((movq ,mem-loc-arg ,temp-var)
             (movzbq (byte-reg al) ,temp-var)
             (movq ,temp-var ,mem-loc-arg)))
         `(,instr))]

      [_ (unsupported-form 'gen-spill instr)]))

  (append-map gen-spill instrs))
