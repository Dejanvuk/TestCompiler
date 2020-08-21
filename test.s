	.file	"test.c"
	.intel_syntax noprefix
	.text
	.globl	s
	.data
	.align 8
	.type	s, @object
	.size	s, 8
s:
	.quad	2
	.text
	.globl	gas
	.type	gas, @function
gas:
	push	rbp
	mov	rbp, rsp
	mov r8, s[rip]
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret
	.globl	sev
	.type	sev, @function
sev:
	push	rbp
	mov	rbp, rsp
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
	mov r11, 16[rbp]
	add r10, r11
	mov r11, 24[rbp]
	add r10, r11
	mov rax, r10                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret
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
	mov r8, s[rip]
	mov r9, 2
	imul r8, r9
	mov -8[rbp], r8
	mov r8, 1
	mov -16[rbp], r8
	mov r9, -8[rbp]
	push r9
	push 7
	mov r9, 6
	mov r10, -8[rbp]
	mov r8, r10
	mov rcx, 4
	mov r10, -16[rbp]
	mov rdx, r10
	mov r10, -8[rbp]
	mov rsi, r10
	mov r10, -16[rbp]
	mov rdi, r10
	call	sev                               # called function "sev"
	mov r8, rax
	mov -24[rbp], r8
	mov r10, -24[rbp]
	mov rdi, r10
	call	foo                               # called function "foo"
	mov r8, rax
	mov -32[rbp], r8
	mov r8, -16[rbp]
	mov r10, -24[rbp]
	add r8, r10
	mov r10, -32[rbp]
	add r8, r10
	mov -40[rbp], r8
	mov r8, -40[rbp]
	mov rax, r8                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret