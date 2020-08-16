CC=gcc
IN=test-compiler.c 
OUT=test-compiler
TEST_FILE=test.c
FLAGS= -Wall -Werror -lm 
DEBUG_FLAGS = -g -lm

compile:
	$(CC) -o $(OUT) $(IN) $(FLAGS)

run:
	./$(OUT) $(TEST_FILE)

debug: 
	$(CC) -o $(OUT) $(IN) $(DEBUG_FLAGS)

debug_run:
	gdb --args $(OUT) $(TEST_FILE) core

assemble_run: 
	gcc -c test.s -o test.o 
	gcc test.o -o test
	chmod a+x test

re_intel_assemble:
	gcc -S -O3 -fno-asynchronous-unwind-tables -O0 -masm=intel asmtest.c -o asmtest.s

.PHONY: clean
clean:
	rm $(OUT)