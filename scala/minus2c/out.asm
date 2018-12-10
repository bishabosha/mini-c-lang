  .globl main
  .text
main:
  jal main0
  move $a0, $v0
  li $v0, 17
  syscall
# call 17: exit2
main0:
  li $v0, 5
  syscall
# call 5: read_int
  move $t0, $v0
  mul $t1, $t0, 2
  move $s7, $t1
  move $a0, $s7
  li $v0, 1
  syscall
# call 1: print_int
# $t2 for result of inlined print_int
  move $s2, $t2
  li $v0, 0
  jr $ra
