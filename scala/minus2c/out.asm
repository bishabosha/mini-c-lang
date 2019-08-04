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
  move $s8, $v0
  #call 5: read_int
  li $v0, 5
  syscall
  move $s0, $v0
  slt $t0, $s8, $s0
  beqz $t0, else0
  sgt $t1, $s8, 0
  beqz $t1, else1
  mul $t2, $s8, $s0
  move $a0, $t2
  #call 1: print_int
  li $v0, 1
  syscall
  j join1
else1:
  slt $t4, $s0, 0
  beqz $t4, else2
  add $t5, $s8, $s0
  move $a0, $t5
  #call 1: print_int
  li $v0, 1
  syscall
  j join2
else2:
  li $a0, 909
  #call 1: print_int
  li $v0, 1
  syscall
join2:
join1:
  j join0
else0:
  rem $t8, $s8, $s0
  move $a0, $t8
  #call 1: print_int
  li $v0, 1
  syscall
join0:
  li $v0, 0
  jr $ra
