.text
	move $fp, $sp
	addi $fp, $fp, -4
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($fp)
	addi $sp, $sp, -12
#_label_main_1
_label_main_1:
#_main_1
	lw $t0, -12($fp)
	li $t1, 10
	slt $t0, $t0, $t1
	sw $t0, -8($fp)
#_main_2
	lw $t0, -8($fp)
	bnez $t0, _label_main_2
#_main_3
	b _label_main_3
#_label_main_2
_label_main_2:
#_main_5
	lw $t0, -12($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -4($fp)
#_main_6
	lw $t0, -4($fp)
	sw $t0, -12($fp)
#_main_7
	li $a0, 33
	li $v0, 11
	syscall
#_main_8
	li $a0, 1
	li $v0, 11
	syscall
#_main_9
	b _label_main_1
#_label_main_3
_label_main_3:
	li $v0, 10
	syscall
atoi:
	move $t0, $a0
	li $t1, 0
	li $t2, 10
atoi_loop:
	lbu $t3, 0($t0)
	beq $t3, $zero, atoi_end
	li $t4, 48
	blt $t3, $t4, atoi_error
	li $t4, 57
	bgt $t3, $t4, atoi_error
	addi $t3, $t3, -48
	mul $t1, $t1, $t2
	add $t1, $t1, $t3
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data
