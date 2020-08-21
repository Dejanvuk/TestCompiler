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
	mov r8, 7
	mov -8[rbp], r8
	mov r8, 1
	mov -16[rbp], r8
	mov r8, 44
	mov -24[rbp], r8
	jmp .L2
.L3:
	mov r8, -8[rbp]
	mov r9, 1
	add r8, r9
	mov -8[rbp], r8
	jmp .L4                               # break out of while loop
.L2:
	mov r8, -8[rbp]
	mov r9, 9
	cmp r8, r9
	jl .L3
.L4:                               # the next statements after the while's block
	mov r8, -8[rbp]
	mov r9, 10
	cmp r8, r9
	jne .L5
	mov r8, 99
	mov -24[rbp], r8
	jmp .L6
.L5:
	mov r8, 45
	mov -24[rbp], r8
.L6:
	mov r8, -8[rbp]
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret
	.ident	"TC: Linux #46~18.04.1-Ubuntu SMP Fri Jul 10 07:21:24 UTC 2020"
