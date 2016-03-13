P523 assignment repo

Group members:

- Omer Sinan Agacan (oagacan)

NOTE: This repo has the public repo as a submodule, so make sure you do `git
clone --recursive` or if you already cloned, run `git submodule update --init
--recursive`

After cloning the submodule, run `make runtime.o` to generate the object file.
Then `racket test.rkt` should just work.

# TODO

- Most important TODO: I'm currently doing spilling and assign-homes wrong for
  offset arguments, so no vectors.

- Too much repetition in pattern matching. Implement lift functions for
  transforming function bodies etc.

- Implement some recursion schemes and functions like `mapAccumL`.

- Generated x86\_64 could use some optimizations.

- Implement mem-loc coalescing. In programs like r1_12 we're using more stack
  space than necessary. Also, we don't re-use stack space allocated for
  arguments.

- Implement root stack overflow checks.
