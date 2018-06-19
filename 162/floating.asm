.data
	A:	.float 12.0
	B:	.float 0.125
	C:	.float 0.0
	D:	.float 0.0

.text

FADD:
	#exponent check/isolation
	#andi $t0, $a0, 133693440
	srl $s0, $a0, 23

	#andi $t0, $a1, 133693440
	srl $s1, $a1, 23

	#mantissa isolation
	#andi $s2, $a0, 8388607
	sll $s2, $a0, 9
	srl $s2, $s2, 9

	#andi $s3, $a1, 8388607
	sll $s3, $a1, 9
	srl $s3, $s3, 9

	beq $s0, $s1, ADD_OPS
	bge $s0, $s1, SHIFT_S_ONE
SHIFT_S_ZERO:
	#t1 contains shift value
	subu $t1, $s1, $s0
	ori $s0, $s1, 0

	li $t5, 1
	sll $t5, $t5, 23
	add $s2, $s2, $t5

	srl $s2, $s2, $t1
	j ADD_OPS
SHIFT_S_ONE:
	subu $t1, $s0, $s1
	ori $s1, $s0, 0

	li $t5, 1
	sll $t5, $t5, 23
	add $s3, $s3, $t5
	
	srl $s3, $s3, $t1
ADD_OPS:	
	addu $t2, $s2, $s3
NORMALIZE:
	#checks 'overflow'
	#andi $t3, $t2, 8388608
	sll $t3, $t2, 8
	srl $t3, $t3, 31
	li $t4, 0
	beq $t3, $t4, NO_FLO
FLO:
	srl $t2, $t2, 1
	addiu $s0, $s0, 1

	li $t6, 254
	blt $t6, $s0, OVERFLOW_ADD
	li $t6, 1
	blt $s0, $t6, UNDERFLOW_ADD
	j NO_FLO

OVERFLOW_ADD:
	#exponent is 255
	#mantissa is all 1s
	#li $t0, 255
	#li $t1, 255
	j EXIT_ADD
UNDERFLOW_ADD:
	#exponent is 0
	#mantissa is all 1s
	#li $t0, 0
	#li $t1, 255
	j EXIT_ADD
NO_FLO:
	sll $s0, $s0, 23
	addu $s4, $s0, $t2
	
EXIT_ADD:
	sw $s4, C
	jr $ra
	




FMUL:
	#exponent check/isolation
	srl $s0, $a0, 23
	
	srl $s1, $a1, 23

	#mantissa isolation
	sll $s2, $a0, 9
	srl $s2, $s2, 9

	sll $s3, $a1, 9
	srl $s3, $s3,9
EXPONENTS:
	addu $t0, $s0, $s1
	li $t1, 127
	subu $t0, $t0, $t1
MANTISSA:
	li $t5, 1
	sll $t5, $t5, 23
	add $s3, $s3, $t5
	add $s2, $s2, $t5

	
	multu $s2, $s3
	mfhi $t2
	sll $t2, $t2, 9
	
MULT_NORMAL:
	subu $t2, $t2, $t5
	
	sll $t0, $t0, 23
	addu $s4, $t0, $t2
	sw $s4, D
	jr $ra
	
	




main:
	lw $a0, A
	lw $a1, B
	jal FMUL
	li $v0, 10
	syscall
	
