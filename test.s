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
	sub rsp, 40
	mov r8, 0
	mov -8[rbp], r8
	mov r8, 1
	mov -16[rbp], r8
	mov r8, 0
	mov -24[rbp], r8
	mov r8, 3
	mov -32[rbp], r8
	mov r8, 10
	mov -40[rbp], r8
	jmp .L2
.L3:
	mov r8, -8[rbp]
	mov r9, -16[rbp]
	add r8, r9
	mov -24[rbp], r8
	mov r8, -16[rbp]
	mov -8[rbp], r8
	mov r8, -24[rbp]
	mov -16[rbp], r8
	mov r8, -32[rbp]
	mov r9, 1
	add r8, r9
	mov -32[rbp], r8
.L2:
	mov r8, -32[rbp]
	mov r9, -40[rbp]
	cmp r8, r9
	jle .L3
	mov r8, -24[rbp]
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret