	.file	"test.c"
	.intel_syntax noprefix
	.text	.globl	s
	.data	.align 8
	.type	s, @object
	.size	s, 8
s:
	.quad	12
	.text
	.globl	sev
	.type	sev, @function
sev:
	push	rbp
	mov	rbp, rsp
	sub rsp, 8
	mov r10, rdi
	mov r11, rsi
	add r10, r11
	mov r11, rdx
	add r10, r11
	mov r11, rcx
	add r10, r11
	mov r11, r8
	add r10, r11
	mov r11, r9
	add r10, r11
	mov r11, 8[rbp]
	add r10, r11
	mov r11, 16[rbp]
	add r10, r11
	mov -8[rbp], r10
	mov r10, -8[rbp]
	mov rax, r10                               # place the return into rax
	mov rsp, 8                               # clean the stack
	mov	rsp, rbp
	pop	rbp
	ret
	.globl	main
	.type	main, @function
main:
	push	rbp
	mov	rbp, rsp
	sub rsp, 8
	push 8
	push 7
	mov r9, 6
	mov r8, 5
	mov rcx, 4
	mov rdx, 3
	mov rsi, 2
	mov rdi, 1
	call	sev                               # called function "sev"
	mov r8, rax
	mov -8[rbp], r8
	mov r8, -8[rbp]
	mov rax, r8                               # place the return into rax
	mov rsp, 8                               # clean the stack
	mov	rsp, rbp
	pop	rbp
	ret