.global _start
.global system_call

_start:
  # 61440
  lui sp, 15 
  call c_start
loop:
  j loop


system_call:
  addi x5, x10, 0
  addi x6, x11, 0
  addi x7, x12, 0
  ecall
  addi x10, x5, 0
  ret
