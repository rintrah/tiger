b_3:
.globl b_3
	pushl %ebp
	movl %esp, %ebp
L6: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	movl 8(%ebp), %eax #0
	movl 12(%eax), %edx #0
	movl $65 , %eax #41
	addl %edx, %eax #28
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
	jmp L5 #14
L5: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
a_6:
.globl a_6
	pushl %ebp
	movl %esp, %ebp
L8: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	movl $1 , %edx #41
	movl 12(%ebp), %eax #22'
	cmpl %edx, %eax #15
	jg L2 #16
L3: #17
	pushl %ebp #20
	call b_3 #10
	addl $4 ,%esp #11
	movl $0, %edx #6
L4: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	jmp L7 #14
L2: #17
	movl 8(%ebp), %edx #0
	movl 12(%ebp), %eax #0
	movl $1 , %ecx #41
	subl %ecx, %eax #30
	movl %eax, %eax #8
	pushl %eax #20
	pushl %edx #20
	call a_6 #10
	addl $8 ,%esp #11
	movl $0, %edx #6
	jmp L4 #14
L7: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
_tigermain:
.globl _tigermain
	pushl %ebp
	movl %esp, %ebp
L10: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	movl %ebp, %eax #8
	movl $10, %edx #6
	pushl %edx #20
	pushl %eax #20
	call a_6 #10
	addl $8 ,%esp #11
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	jmp L9 #14
L9: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
