# P523 Final Project

## Partial evaluation

As the final project I implemented some kind of online partial evaluator. The
idea is originally described in a partial evaluation tutorial [^1] by William R.
Cook and Ralf Lämmel. The language used in the tutorial is too simple to be
practical, it doesn't have expressions with side effects, or heap objects
aliased by different program variables (like a `vector` in our language that's
used as a mutable cell), or closures. The most important part in the tutorial is
function application, and I use the same idea for function applications in my
implementation. The rest is extended by me in a way that avoids work duplication
or breaking programs by inlining vectors etc.

## Problem and motivation

At the highest-level the problem is just performance. Abstraction is never free,
but ideally we want to pay the price in compile-time and leave only the
essential work to runtime.

Fancy inliners like [^2] and GHC's simplifier do great job at doing some of the
work in compile time. In fact, GHC's simplifier is so good it can remove a whole
layer of abstraction from programs [^3].

However, inliners can't help with recursive functions with partially available
arguments. This is where partial evaluation helps. As a classic example, suppose
we have this power function:

```scheme
(define (pow x n)
  (if (eq? n 0)
    1
    (* x (pow x (- n 1)))))
```

Suppose this function is used in four forms in a program:

1. `(pow (read) (read))`
2. `(pow (read) 5)`
3. `(pow 2 (read))`
4. `(pow 2 5)`

In (1) an optimizer can't really do anything, as none of the arguments are known
in compile time and the function is recursive.

We know this function recurses `n` times, and in (2) `n` is statically known. A
smart optimizer can maybe realize this and unroll the loop to generate something
like this:

```scheme
(let ([x (read)])
  (* x (* x (* x (* x (* x 1))))))
```

One optimization that can be done for (3) is to generate a `pow` that is
specialized for `2` as first argument. This specialized function would look like
this:

```scheme
(define (pow-2 n)
  (if (eq? n 0)
    1
    (* 2 (pow-2 (+ n (- 1))))))
```

However this is not something that inliners or constant folders etc. do in
general, as a new function needs to be generated.

(4) can be completely evaluated in compile time, however for this the compiler
needs to figure that `pow` really terminates. Even in that case it may not be
feasible though, as a terminating computation can take forever to actually
terminate. So in practice a compiler would probably (in addition to termination
checking) have an upper bound of number of steps it can take to evaluate an
expression, to limit time spent in compile time evaluation.

(Note that "termination checking" is just an approximation as the problem is
actually the halting problem)

GHC 7.10.2 can't do any of these optimizations, clang 3.6.1 can do (4), (3) and
(2), and it also inlines the loop so the function call is completely eliminated
in (1). (this part is a bit tricky to do in a functional IL as loops are usually
expressed as tail calls in functional languages)

Our partial evaluator can do (2), (3) and (4), without any offline decision
making (static analyses etc.) or working on an low-level IR that makes control
flow explicit. When partial evaluation is enabled, our compiler generates this
for (2):

```scheme
(define (pe_fresh_5 (arg_fresh_0 : Integer)) : Integer
  (* arg_fresh_0 (pe_fresh_6 arg_fresh_0)))

(define (pe_fresh_3 (arg_fresh_0 : Integer)) : Integer
  (* arg_fresh_0 (pe_fresh_4 arg_fresh_0)))

(define (pe_fresh_2 (arg_fresh_0 : Integer)) : Integer
  (* arg_fresh_0 (pe_fresh_3 arg_fresh_0)))

(define (pe_fresh_4 (arg_fresh_0 : Integer)) : Integer
  (* arg_fresh_0 (pe_fresh_5 arg_fresh_0)))

(define (pe_fresh_6 (arg_fresh_0 : Integer)) : Integer
  (* arg_fresh_0 (pe_fresh_7 arg_fresh_0)))

(define (pe_fresh_7 (arg_fresh_0 : Integer)) : Integer 1)

(define main : void (print-int (+ (pe_fresh_2 (read)) (read))))
```

Note how the loop is unrolled. After this step, it's just a matter of simple
post-processing to inline single-use functions to get this final form:

```scheme
(define main : void
  (print-int
    (+
      (let ([x (read)])
        (* x (* x (* x (* x (* x 1))))))
      (read))))
```

(This post-processing step is not implemented because of time constraints)

(3) is optimized to this:

```scheme
(define (pe_fresh_2 (arg_fresh_1 : Integer)) : Integer
  (if (eq? arg_fresh_1 0) 1 (* 2 (pe_fresh_2 (+ arg_fresh_1 -1)))))

(define main : void
  (print-int (+ (pe_fresh_2 (read)) (read))))
```

Note how `2` is inlined in the new function.

(4) is completely evaluated in compile time:

```scheme
(define main : void (app-noalloc print-int (+ 32 (read))))
```

(To test, run `racket main.rkt tests/pe_{1,2,3,4}.rkt`)

# How

The partial evaluator is just a program transformer -- it doesn't need any
static analysis, pre-processing (to generate control-flow graph) etc. The
implementation is in `passes/partial-eval.rkt`. Main transformation
(`peval-expr`) looks quite a lot like an interpreter. Constant expressions are
directly evaluated. In a let expression, environment is only extended when the
value is safe to inline. Safe to inline values are completely evaluated
integers, booleans and lambdas.

To avoid work duplication problems, we never add vectors to the environment.
This prevents some optimizations, but the implementation stays simple.

Lambda bodies are evaluated before lambdas are returned, to avoid redundant
closure generation (which prevents some optimizations). For example, in this
expression:

```
(let ([x <static>])
  (lambda ...))
```

`x` is inlined in lambda's body and then lambda is returned. This way we avoid
redundant captures which prevents optimizations as we can't duplicate let
expressions without duplicating work.

Most important part is function application. We first check function arguments.
If all of the arguments are values, we just evaluate the function application
(beta reduction followed by more evaluation steps). If only a subset of
arguments are static, we generate a new function that only takes arguments for
dynamic arguments. This new function's body is optimized using statically-known
arguments. For example, in this application `(pow 2 (read))` only one of the
arguments is static, so we generate a new pow. Original function was:

```
(define (pow x n)
  (if (eq? n 0)
    1
    (* x (pow x (- n 1)))))
```

The new function doesn't take `x` as argument as it's statically known to be `2`
in our case. Function's body will be partially evaluated with `x` bound to `2`
in the environment:

```
(define (pe_fresh_2 (arg_fresh_1 : Integer)) : Integer
  (if (eq? arg_fresh_1 0)
    1
    (* 2 (pe_fresh_2 (+ arg_fresh_1 -1)))))
```

Note that the recursive call calls itself, instead of the original `pow` with
`2` passed for `x`. This is implemented with another environment that keeps
track of specialized functions. In our case, the new function's body is
evaluated with a new entry to the specialized function environment. The entry
basically says that `pow` is already specialized for `x = 2`, and this
specialized function's name is `pe_fresh_2`. So when the evaluator sees an
application of `pow` with `2` for `x`, it generates an application to
`pe_fresh_2`.

This much is enough for generating specialized functions, but it's not good
enough for some programs. An example:

(`tests/pe_6.rkt`)

```
(define (map-vec [f : (Integer -> Integer)] [v : (Vector Integer Integer)]) : Void
  (vector-set! v 0 (f (vector-ref v 0)))
  (vector-set! v 1 (f (vector-ref v 1))))

(let ([v (vector 1 2)])
  (map-vec (lambda: ([x : Integer]) : Integer (+ x 1)) v)
  (vector-ref v 1))
```

Here we want a specialized `map-vec` for our lambda that adds one to its input.
For this to work, we need to evaluate the function application `f` in `map-vec`s
body, before our usual function application code runs and generates a top-level
function for the lambda (the usual function application code runs because
`vector-ref ...` is considered dynamic).

For this we add an exception to our function application rule. If a lambda is
linear in its arguments, we evaluate the application even if some of the
arguments are dynamic. So, our code becomes this:

```
(vector-set! v 0 ((lambda (x) (+ x 1)) (vector-ref v 0)))
```

Even though the argument is dynamic, the lambda is linear in its argument, so
it's always safe to evaluate this. As a result, we get this program:

```
(define (pe_fresh_7 (arg_fresh_1 : (Vector Integer Integer))) : Void
  (vector-set! arg_fresh_1 0 (+ (vector-ref arg_fresh_1 0) 1))
  (vector-set! arg_fresh_1 1 (+ (vector-ref arg_fresh_1 1) 1)))

(define main : void
  (print-int
   (let ((x_fresh_4 (vector 1 2)))
     (let ((x_fresh_5 (pe_fresh_7 x_fresh_4)))
       (vector-ref x_fresh_4 1)))))
```

(Here we use the word "linear" in the sense it's used in [^4])

# Evaluation

We tested our partial evaluator on 257 test programs. Most of the programs
without any dynamic input/output are completely evaluated in compile time, even
if it takes very long time. As an example, `tests/dynamic_16.rkt` is this simple
program:

```
(sum (range 100))
```

This should still be very fast to interpret, however, because of function call
overheads in our partial evaluator, this takes minutes to complete.

Programs that are not completely evaluated are programs that use vectors. To
avoid work duplication problems, we treated vectors as dynamic values.

Most programs with dynamic input generate big number of specialized functions.
Since we don't implement the inliner mentioned in the introduction, those
programs usually get slower. Fixing this is just a matter of implementing a very
simple inliner that inlines single-use functions.

For a production compiler, the times our compiler takes to evaluate programs is
unacceptable. To remedy this we could implement a some fixed upper bounds on
steps to take or time to spend. Some static analyses could also help to decide
when to bail out without evaluating an expression.

---

[^1]: http://www.cs.utexas.edu/~wcook/tutorial/

[^2]: "Fast and Effective Procedure Inlining" by Waddell and Dybvig

[^3]: GHC's Generics typeclass -based method implementations are a great
example. Using Generics, typeclasses can be defined on arbitrary user-defined
data types automatically. For this GHC first derives an implementation of
Generics for the user-defined data type, and then uses the typeclass's Generics
instance. A major problem with this approach is the extra work: Every time a
method is called the user-defined type is first converted into a Generics value
(reification), then the method works on this value. However, GHC's simplifier is
so powerful, it can eliminate the whole intermediate step (reification of
user-defined type to Generics) and derive efficient methods that work directly
on the user-defined type.

[^4]: "Deforestation: Transforming programs to eliminate trees"
