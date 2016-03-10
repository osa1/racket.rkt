#lang racket

(require "../utils.rkt")

(provide assign-homes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assigning vars to their locations on the machine

(define (assign-homes def mapping)
  ; (printf "============ assign homes ==========~n")
  ; (pretty-print def)
  ; (pretty-print mapping)
  ; (printf "====================================~n")
  (match def
    [`(define ,tag : ,ret-ty ,stack-locs-used . ,instrs)
     (define stack-aligned (align-stack stack-locs-used instrs))
     `(define ,tag : ,ret-ty (,stack-aligned)
        ,@(assign-home-instrs mapping stack-aligned instrs))]
    [_ (unsupported-form 'assign-homes def)]))

(define (align-stack stack-locs-used instrs)

  ; According to the ABI, on function entry %rsp+8 should be 16-byte aligned.
  ; We know initially this is the case (because the caller makes sure) but here
  ; we need to manually align %rsp if we call other functions.
  ;
  ; In the 32-bit conventions we'd do something like this:
  ;
  ;   pushq %ebp
  ;   movq  %rsp, %ebp
  ;
  ; This makes %rsp 16-byte aligned. Now, in x86_64 we don't do this, so the
  ; alignment code is a bit different. If in the stack frame we have even
  ; number of things (assuming every "thing" is 8-byte), we need to increment
  ; stack pointer 8-byte more, so that when we call a function, caller find
  ; %rsp+8 16-byte aligned.

  (if (even? stack-locs-used)

    ; Here's one little optimization-like thing that we could do sometimes. If
    ; we don't call any functions in the function then we don't need to align
    ; anything. Sometimes saves us two instructions if the function doesn't
    ; need any stack space.
    (if (has-callq? instrs)
      (* 8 (+ 1 stack-locs-used))
      (* 8 stack-locs-used))
    (* 8 stack-locs-used)))

(define (assign-home-instrs asgns stack-size instrs)
  (map (lambda (instr) (assign-home-instr asgns stack-size instr)) instrs))

(define (assign-home-instr asgns stack-size instr)
  (match instr
    [`(if (eq? ,arg1 ,arg2) ,pgm-t ,pgm-f)
     `(if (eq? ,(assign-home-arg asgns stack-size arg1)
               ,(assign-home-arg asgns stack-size arg2))
        ,(assign-home-instrs asgns stack-size pgm-t)
        ,(assign-home-instrs asgns stack-size pgm-f))]

    [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,arg1 ,arg2)
     `(,(car instr) ,(assign-home-arg asgns stack-size arg1)
                    ,(assign-home-arg asgns stack-size arg2))]

    [`(,(or 'negq 'pushq 'popq) ,arg)
     `(,(car instr) ,(assign-home-arg asgns stack-size arg))]

    [`(callq ,arg) `(callq ,(assign-home-arg asgns stack-size arg))]

    [`(retq) instr]

    [`(,(or 'sete 'setl) (byte-reg al)) instr]

    [`(movzbq (byte-reg al) ,arg)
     `(movzbq (byte-reg al) ,(assign-home-arg asgns stack-size arg))]

    [_ (unsupported-form 'assign-home-instr instr)]))

(define (assign-home-arg asgns stack-size arg)
  (match arg
    [`(int ,_) arg]
    [`(reg ,_) arg]
    [`(mem-loc ,l)
     (cond
       [(eq? l 0)
        ; This slot is occupied by the return address pushed by callq.
        (error 'assign-home-arg "Found mem-loc 0.")]
       [(< l 0)
        ; An argument passed on stack
        `(stack ,(+ (* 8 (- l)) stack-size))]
       [#t
        ; Local variable
        `(stack ,(* 8 (- l 1)))])]
    [`(global-value ,_) arg]
    [`(offset ,arg ,offset)
     `(offset ,(assign-home-arg asgns stack-size arg) ,offset)]
    [`(toplevel-fn ,_) arg]
    [`(var ,var)
     (let [(asgn (hash-ref asgns `(var ,var) '()))]
       (cond [(null? asgn)
              (error 'assign-home-arg "can't find var in assignments: ~s ~s~n" var asgns)]
             [(symbol? asgn) `(reg ,asgn)]
             [#t (error 'assign-home-arg "Variable mapped to an unknown thing: ~s~n"
                        asgn)]))]
    [_ (unsupported-form 'assign-home-arg arg)]))

(define (has-callq? instrs)
  (match instrs
    [`() #f]
    [`(,instr . ,instrs)
     (match instr
       [`(if ,_ ,pgm-t ,pgm-f)
        (or (has-callq? pgm-t) (has-callq? pgm-f) (has-callq? instrs))]

       [`(,(or 'addq 'subq 'movq 'leaq 'cmpq 'xorq) ,_ ,_)
        (has-callq? instrs)]

       [`(,(or 'negq 'pushq 'popq) ,arg)
        (has-callq? instrs)]

       [`(callq ,_) #t]

       [`(retq) (has-callq? instrs)]

       [`(,(or 'sete 'setl) (byte-reg al)) (has-callq? instrs)]

       [`(movzbq (byte-reg al) ,_) (has-callq? instrs)]

       [_ (unsupported-form 'assign-home-instr instr)])]))
