  .globl main
  .text
main:
  jal main0
  move $a0, $v0
  li $v0, 17
  syscall
  #call 17: exit2
main0:
  li $s7, 0
  li $s4, 0
  li $s8, 0
  li $s6, 0
  li $t0, 23
  sw $t0, beq0
  lw $t2, beq0
  move $a0, $t2
  li $v0, 1
  syscall
  #call 1: print_int
  li $s3, 0
  li $s5, 0
  li $s1, 0
  li $v0, 0
  jr $ra
  .data
beq0:
  .word 0
