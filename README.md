P523 assignment repo

Group members:

- Omer Sinan Agacan (oagacan)

NOTE: This repo has the public repo as a submodule, so make sure you do `git
clone --recursive` or if you already cloned, run `git submodule update --init
--recursive`

After cloning the submodule, run `make runtime.o` to generate the object file.
Then `racket test.rkt` should just work.

# TODO

- Implement mem-loc coalescing. In programs like r1_12 we're using more stack
  space than necessary. Also, we don't re-use stack space allocated for
  arguments.

- Implement root stack overflow checks.

# Scratchpad

- Non-closure functions shouldn't pay the price of allocating an argument slot
  to the closure pointers.

- Calls to non-allocating functions shouldn't pay the price of saving things to
  the root stack and loading again on return.

Worker/wrapper transformations a la GHC are good for this kind of
optimizations, but that relies on excessive inlining. I think another
alternative is data flow analysis (k-CFA etc.). TODO: Think more.
