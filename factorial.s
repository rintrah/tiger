nfactor_6:
.globl nfactor_6
	pushl %ebp
	movl %esp, %ebp
	subl $8, %esp
L6: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %eax #8
	movl %eax, -8(%ebp) #92
	movl $0 , %edx #41
	movl 12(%ebp), %eax #22'
	cmpl %edx, %eax #15
	je L2 #16
L3: #17
	movl 12(%ebp), %eax #0
	movl %eax, -4(%ebp) #92
	movl 8(%ebp), %ebx #0
	movl 12(%ebp), %eax #0
	movl %eax, %edx #29
	movl $1 , %eax #41
	subl %eax, %edx #30
	movl %edx, %eax #9
	movl %eax, %eax #8
	pushl %eax #20
	pushl %ebx #20
	call nfactor_6 #10
	addl $8 ,%esp #11
	movl %eax, %eax #8
	movl %eax, %eax #8
	movl -4(%ebp), %edx #91
	imul %edx, %eax #32
	movl %eax, %eax #9
	movl %eax, %eax #8
L4: #17
	movl %eax, %eax #8
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl -8(%ebp), %edx #91
	movl %edx, %ebx #8
	jmp L5 #14
L2: #17
	movl $1, %eax #6
	jmp L4 #14
L5: #17
	# Instruccion trucha
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
ret
_tigermain:
.globl _tigermain
	pushl %ebp
	movl %esp, %ebp
L8: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	movl %ebp, %eax #8
	movl $3, %edx #6
	pushl %edx #20
	pushl %eax #20
	call nfactor_6 #10
	addl $8 ,%esp #11
	movl %eax, %eax #8
	movl %eax, %edx #8
	movl $48 , %eax #41
	movl %eax, %eax #27
	addl %edx, %eax #28
	movl %eax, %eax #9
	movl %eax, %eax #8
	pushl %eax #20
	call chr #10
	addl $4 ,%esp #11
	movl %eax, %eax #8
	movl %eax, %eax #8
	pushl %eax #20
	call print #10
	addl $4 ,%esp #11
	movl %eax, %edx #8
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	jmp L7 #14
L7: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
