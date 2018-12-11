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
  move $s5, $v0
  li $v0, 5
  syscall
  #call 5: read_int
  move $s3, $v0
  slt $t0, $s5, $s3
  beqz $t0, else0
  mul $t1, $s5, $s3
  move $a0, $t1
  li $v0, 1
  syscall
  #call 1: print_int
  j post0
else0:
  sgt $t3, $s5, 25
  beqz $t3, post1
  sub $t4, $s5, $s3
  move $a0, $t4
  li $v0, 1
  syscall
  #call 1: print_int
post1:
post0:
  li $v0, 0
  jr $ra
