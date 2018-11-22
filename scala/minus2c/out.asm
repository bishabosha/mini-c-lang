  .globl main
  .text
main:
  jal _main_
  move $a0, $v0
  li $v0, 17
  syscall
# 17: exit with argument
_main_:
  li $t0, 21
  sw $t0, yy
  li $s3, 4
  li $s3, 10
  move $v0, $s3
  jr $ra
  .data
# global data read not yet possible
yy:
  .word 36
zz:
  .word 25
f:
  .word 0
