.global _start
_start:
  # 61440
  lui sp, 15 
  call c_start
loop:
  j loop
