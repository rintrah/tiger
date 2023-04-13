f_8:
.globl f_8
	pushl %ebp
	movl %esp, %ebp
L3: #17
	movl %esi, %esi #8
	movl %edi, %ecx #8
	movl %ebx, %edx #8
	movl $2, %eax #6
	movl %eax, %eax #8
	movl 12(%ebp), %ebx #0
	addl %ebx, %eax #28
	movl %eax, %eax #8
	movl %esi, %esi #8
	movl %ecx, %edi #8
	movl %edx, %ebx #8
	jmp L2 #14
L2: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
L0:.long 5
.string "Hola\x0a"
L1:.long 1
.string "\x0a"
_tigermain:
.globl _tigermain
	pushl %ebp
	movl %esp, %ebp
L5: #17
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	movl $68, %edx #6
	movl $L0, %eax #7
	movl %ebp, %eax #8
	movl %edx, %edx #8
	pushl %edx #20
	pushl %eax #20
	call f_8 #10
	addl $8 ,%esp #11
	movl %eax, %eax #8
	movl %eax, %eax #8
	pushl %eax #20
	call chr #10
	addl $4 ,%esp #11
	movl %eax, %eax #8
	movl %eax, %eax #8
	pushl %eax #20
	call print #10
	addl $4 ,%esp #11
	movl %eax, %eax #8
	movl $L1, %eax #7
	pushl %eax #20
	call print #10
	addl $4 ,%esp #11
	movl %eax, %edx #8
	movl %esi, %esi #8
	movl %edi, %edi #8
	movl %ebx, %ebx #8
	jmp L4 #14
L4: #17
	# Instruccion trucha
	movl %ebp, %esp
	popl %ebp
ret
