(16)
(movq (int 10) (stack -8))
(movq (int 20) (stack -16))
(subq (stack -8) (stack -16))
(movq (stack -16) (reg rax))