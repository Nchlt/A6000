.text
	move $fp, $sp
	addi $fp, $fp, -4
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($fp)
	addi $sp, $sp, -48
#_main_0
	li $t1, 1
#_main_1
	li $t1, 0
#_label_main_6
_label_main_6:
#_main_3
	lw $t0, -40($fp)
	bnez $t0, _label_main_7
#_main_4
	b _label_main_8
#_label_main_7
_label_main_7:
#_main_6
	li $t1, 0
#_main_7
	li $t1, 0
#_label_main_3
_label_main_3:
#_main_9
	lw $t1, 0($fp)
	li $t2, 1
	add $t0, $t1, $t2
#_main_10
	lw $t1, -48($fp)
	lw $t2, -32($fp)
	slt $t0, $t1, $t2
#_main_11
	lw $t0, -36($fp)
	bnez $t0, _label_main_4
#_main_12
	b _label_main_5
#_label_main_4
_label_main_4:
#_main_14
	lw $t1, -44($fp)
	lw $t2, -44($fp)
	mul $t0, $t1, $t2
#_main_15
	lw $t1, -48($fp)
	lw $t2, -48($fp)
	mul $t0, $t1, $t2
#_main_16
	lw $t1, -12($fp)
	lw $t2, -16($fp)
	add $t0, $t1, $t2
#_main_17
	lw $t1, 0($fp)
	lw $t2, 0($fp)
	mul $t0, $t1, $t2
#_main_18
	lw $t1, -20($fp)
	lw $t2, -24($fp)
	slt $t0, $t1, $t2
#_main_19
	lw $t0, -28($fp)
	bnez $t0, _label_main_1
#_main_20
	li $a0, 35
	li $v0, 11
	syscall
#_main_21
	b _label_main_2
#_label_main_1
_label_main_1:
#_main_23
	li $a0, 46
	li $v0, 11
	syscall
#_main_24
	li $t1, 1
#_label_main_2
_label_main_2:
#_main_26
	li $a0, 32
	li $v0, 11
	syscall
#_main_27
	lw $t1, -48($fp)
	li $t2, 1
	add $t0, $t1, $t2
#_main_28
	lw $t1, -8($fp)
#_main_29
	b _label_main_3
#_label_main_5
_label_main_5:
#_main_31
	li $a0, 10
	li $v0, 11
	syscall
#_main_32
	lw $t1, -44($fp)
	li $t2, 1
	add $t0, $t1, $t2
#_main_33
	lw $t1, -4($fp)
#_main_34
	b _label_main_6
#_label_main_8
_label_main_8:
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
