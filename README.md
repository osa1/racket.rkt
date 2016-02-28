P523 assignment repo

Group members:

- Omer Sinan Agacan (oagacan)

NOTE: This repo has the public repo as a submodule, so make sure you do `git
clone --recursive` or if you already cloned, run `git submodule update --init
--recursive`

After cloning the submodule, run `make runtime.o` to generate the object file.
Then `racket test.rkt` should just work.

# TODO

- Too much repetition in pattern matching. Implement lift functions for
  transforming function bodies etc.

- Implement some recursion schemes and functions like `mapAccumL`.

- Generated x86\_64 could use some optimizations.

- We're having some redundant stack traffic. As an example, identity function is compiled to this:

    ```assembly
    id:
        pushq %rbp
        movq %rsp, %rbp
        subq $16, %rsp

        movq %rdi, %rax

        addq $16, %rsp
        popq %rbp

        retq
    ```

  I think we should be able to compile this to a `movq ... ; retq`.
