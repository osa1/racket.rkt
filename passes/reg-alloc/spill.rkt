#lang racket

(require "../utils.rkt")

(provide generate-spills)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-spills var-sym mem-loc instrs)
  (define mem-loc-arg `(mem-loc ,mem-loc))
  (define var `(var ,var-sym))

  ; (printf "spilling ~a~n" var-sym)

  (define temp-prefix (string-append "spill_" (symbol->string var-sym)))
  (define (mk-temp-var) `(var ,(fresh temp-prefix)))

  (define (gen-spill instr)
    (match instr

      ; TODO: What happens if both s and d are the var to spill?

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Special case for offset
      ; Currently only place where a offset can appear is in mov instructions.

      [`(movq (offset ,s ,offset) ,d)
       (cond
         [(and (equal? s var) (equal? d var))
          (error 'gen-spill
                 "Ops! I wasn't expecting this: s and d are the same spilled var: ~a~n"
                 instr)]

         [(equal? s var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (movq (offset ,temp-var ,offset) ,d))]

         [(equal? d var)
          (define temp-var (mk-temp-var))
          `((movq (offset ,s ,offset) ,temp-var)
            (movq ,temp-var ,mem-loc-arg))]

         [#t `(,instr)])]

      [`(movq ,s (offset ,d ,offset))
       (cond
         [(and (equal? s var) (equal? d var))
          (error 'gen-spill
                 "Ops! I wasn't expecting this: s and d are the same spilled var: ~a~n"
                 instr)]

         [(equal? s var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,temp-var ,d))]

         [(equal? d var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,s (offset ,temp-var ,offset)))]

         [#t `(,instr)])]

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      [`(movq ,s ,d)
       (cond
         [(and (equal? s var) (equal? d var))
          (error 'gen-spill
                 "Ops! I wasn't expecting this: s and d are the same spilled var: ~a~n"
                 instr)]

         [(equal? s var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (movq ,temp-var ,d))]

         [(equal? d var)
          (define temp-var (mk-temp-var))
          `((movq ,s ,temp-var)
            (movq ,temp-var ,mem-loc-arg))]

         [#t `(,instr)])]

      [`(,(or 'addq 'subq 'cmpq 'xorq) ,s ,d)
       (cond
         [(and (equal? s var) (equal? d var))
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (,(car instr) ,temp-var ,temp-var)
            (movq ,temp-var ,mem-loc-arg))]

         [(equal? s var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (,(car instr) ,temp-var ,d))]

         [(equal? d var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (,(car instr) ,s ,temp-var)
            (movq ,temp-var ,mem-loc-arg))]

         [#t `(,instr)])]

      [`(leaq ,s ,d)
       (cond
         [(equal? s var)
          (error 'gen-spill "Weird leaq argument: ~a~n" instr)]

         [(equal? d var)
          (define temp-var (mk-temp-var))
          `((movq ,mem-loc-arg ,temp-var)
            (leaq ,s ,temp-var)
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
