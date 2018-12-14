  .globl main
  .text
main:
  jal L1
  move $a0, $v0
  #call 17: exit2
  li $v0, 17
  syscall
L1: #debug: main~0
  #call 5: read_int
  li $v0, 5
  syscall
  move $s7, $v0
  #call 5: read_int
  li $v0, 5
  syscall
  move $s1, $v0
  sgt $t0, $s7, 30
  beqz $t0, else0
  li $a0, 1
  #call 1: print_int
  li $v0, 1
  syscall
  j join0
else0:
  slt $t2, $s1, 50
  beqz $t2, else1
  sle $t3, $s7, 20
  beqz $t3, else2
  li $a0, 2
  #call 1: print_int
  li $v0, 1
  syscall
  j join2
else2:
  li $a0, 3
  #call 1: print_int
  li $v0, 1
  syscall
join2:
  j join1
else1:
  li $a0, 4
  #call 1: print_int
  li $v0, 1
  syscall
join1:
join0:
  li $v0, 0
  jr $ra
