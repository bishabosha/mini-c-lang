  .globl main
  .text
main:
  jal main0
  move $a0, $v0
  li $v0, 17
  syscall
  #call 17: exit2
main0:
  li $v0, 5
  syscall
  #call 5: read_int
  move $s3, $v0
  li $v0, 5
  syscall
  #call 5: read_int
  move $s7, $v0
  mul $t0, $s3, $s7
  move $a0, $t0
  li $v0, 1
  syscall
  #call 1: print_int
  li $v0, 0
  jr $ra
