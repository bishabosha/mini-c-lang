  .globl main
  .text
main:
  jal _main_
  move $a0, $v0
  li $v0, 17
  syscall
# 17: exit with argument
_main_:
  sub $t0, $s8, 3
  div $t1, $s5, $s8
  sub $t2, $s8, 2
  add $t3, $t0, $t1
  sle $t4, $s5, $t2
