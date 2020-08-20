	.file	"test.c"
	.intel_syntax noprefix
	.text
	.globl	s
	.data
	.align 8
	.type	s, @object
	.size	s, 8
s:
	.quad	12
	.text
	.globl	foo
	.type	foo, @function
foo:
	push	rbp
	mov	rbp, rsp
	sub rsp, 8
	mov r8, rdi
	mov r9, 2
	imul r8, r9
	mov -8[rbp], r8
	mov r8, -8[rbp]
	mov r9, 3
	imul r8, r9
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret
	.globl	main
	.type	main, @function
main:
	push	rbp
	mov	rbp, rsp
	sub rsp, 24
	mov r8, 8
	mov -8[rbp], r8
	mov r8, 44
	mov -24[rbp], r8
	mov r8, -8[rbp]
	mov r9, 7
	cmp r8, r9
	je .L2
	mov r8, 1
	mov -16[rbp], r8
	mov r8, -16[rbp]
	mov rax, r8                               # place the return into rax
	mov r8, 45
	mov -24[rbp], r8
	jmp .L5
.L2:
	mov r8, -8[rbp]
	mov r9, 6
	cmp r8, r9
	jg .L3
	mov r8, 2
	mov -16[rbp], r8
	mov r8, 46
	mov -24[rbp], r8
	jmp .L5
.L3:
	mov r8, -8[rbp]
	mov r9, 9
	cmp r8, r9
	jle .L4
	mov r8, 3
	mov -16[rbp], r8
	jmp .L5
.L4:
	mov r8, -8[rbp]
	mov r9, 7
	cmp r8, r9
	jne .L5
	mov r8, 4
	mov -16[rbp], r8
	jmp .L5
.L5:
	mov r8, -16[rbp]
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret