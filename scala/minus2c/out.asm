  .globl main
  .text
main:
  jal _main_
  move $a0, $v0
  li $v0, 17
  syscall
# 17: exit with argument
_main_:
  li $s7, 9
  li $s7, 10
  move $v0, $s7
  jr $ra
  .data
# global data not linked to code yet
zz:
  .word 25
f:
  .word 0
