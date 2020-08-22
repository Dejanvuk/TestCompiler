# TestCompiler
A simple compiler for a small subset of ANSI C -> x86-64 assembly made for learning purposes

The assembler used is GNU Assembler GAS

## Features:

* complex arithmetic expressions
* Global variables
* Functions
* type specifier: int
* while loop
* simple not-nested if/else-if/else conditionals
* the return from main is printed to the console

Example:

test.c
```C
int s = 5;

int foo(int a, int b, int c, int d, int e, int f, int g, int h)
{
    int k = a + b + c + d + e + f + g + h;
    return k * 3;
}

int fib(int len) {
    int f1 = 0;
    int f2 = 1;
    int f3 = 0;
    int i = 3;
    while(i<=len)           
    {
        f3=f1+f2;                     
        f1=f2;
        f2=f3;
        i=i+1;                  
    }

    return f3;
}

int main(int argc, int argv)
{
    int firsta = s;
    int s = 1;
    int g = 4 + (s * 5 / (2 * 3 - 1) * 3 * firsta - 9) * 2;
    int f = foo(1,2,3,4,5,6,7,8);
    while (firsta < 100)
    {
        firsta = firsta + f + g;
        if (firsta >= 200)
        {
            f = 2;
        }
        else if(firsta <= 110)
        {
            f = 3;
        }
        else if(firsta >= 111)
        {
            f = 4;
        }
    }

    int result;

    if (f == 2)
    {
        result = 10;
    }
    else if(f == 3) {
        result = 11;
    }
    else {
        result = 12;
    }

    return fib(result);
}
```

test.s
```asm
	.file	"test.c"
	.intel_syntax noprefix
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	s
	.data
	.align 8
	.type	s, @object
	.size	s, 8
s:
	.quad	5
	.text
	.globl	foo
	.type	foo, @function
foo:
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
	mov r11, 16[rbp]
	add r10, r11
	mov r11, 24[rbp]
	add r10, r11
	mov -8[rbp], r10
	mov r10, -8[rbp]
	mov r11, 3
	imul r10, r11
	mov rax, r10                               # place the return into rax
	mov	rsp, rbp
	pop	rbp
	ret
	.globl	fib
	.type	fib, @function
fib:
	push	rbp
	mov	rbp, rsp
	sub rsp, 32
	mov r8, 0
	mov -8[rbp], r8
	mov r8, 1
	mov -16[rbp], r8
	mov r8, 0
	mov -24[rbp], r8
	mov r8, 3
	mov -32[rbp], r8
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
	mov r9, rdi
	cmp r8, r9
	jle .L3
.L4:                               # the next statements after the while's block
	mov r8, -24[rbp]
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
	mov -8[rbp], r8
	mov r8, 1
	mov -16[rbp], r8
	mov r8, 4
	mov r9, -16[rbp]
	mov r10, 5
	imul r9, r10
	mov r10, 2
	mov r11, 3
	imul r10, r11
	mov r11, 1
	sub r10, r11
	mov rax, r9
	cqo
	idiv r10
	mov r9, rax
	mov r10, 3
	imul r9, r10
	mov r10, -8[rbp]
	imul r9, r10
	mov r10, 9
	sub r9, r10
	mov r10, 2
	imul r9, r10
	add r8, r9
	mov -24[rbp], r8
	push 8
	push 7
	mov r9, 6
	mov r8, 5
	mov rcx, 4
	mov rdx, 3
	mov rsi, 2
	mov rdi, 1
	call	foo                               # called function "foo"
	mov r8, rax
	mov -32[rbp], r8
	jmp .L5
.L6:
	mov r8, -8[rbp]
	mov r10, -32[rbp]
	add r8, r10
	mov r10, -24[rbp]
	add r8, r10
	mov -8[rbp], r8
	mov r8, -8[rbp]
	mov r10, 200
	cmp r8, r10
	jl .L8
	mov r8, 2
	mov -32[rbp], r8
	jmp .L10
.L8:
	mov r8, -8[rbp]
	mov r10, 110
	cmp r8, r10
	jg .L9
	mov r8, 3
	mov -32[rbp], r8
	jmp .L10
.L9:
	mov r8, -8[rbp]
	mov r10, 111
	cmp r8, r10
	jl .L10
	mov r8, 4
	mov -32[rbp], r8
.L10:
.L5:
	mov r8, -8[rbp]
	mov r10, 100
	cmp r8, r10
	jl .L6
.L7:                               # the next statements after the while's block
	mov r8, -32[rbp]
	mov r10, 2
	cmp r8, r10
	jne .L11
	mov r8, 10
	mov -40[rbp], r8
	jmp .L13
.L11:
	mov r8, -32[rbp]
	mov r10, 3
	cmp r8, r10
	jne .L12
	mov r8, 11
	mov -40[rbp], r8
	jmp .L13
.L12:
	mov r8, 12
	mov -40[rbp], r8
.L13:
	mov r10, -40[rbp]
	mov rdi, r10
	call	fib                               # called function "fib"
	mov r8, rax
	mov rax, r8                               # place the return into rax
	mov	esi, eax
	lea	rdi, .LC0[rip]
	mov	eax, 0
	call	printf@PLT
	mov	rsp, rbp
	pop	rbp
	ret
	.ident	"TC: Linux #46~18.04.1-Ubuntu SMP Fri Jul 10 07:21:24 UTC 2020"
```
