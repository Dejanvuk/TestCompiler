CC=gcc
IN=test-compiler.c 
OUT=tc
TEST_FILE=test.c
FLAGS= -Wall -Werror 
DEBUG_FLAGS = -g 

compile:
	$(CC) -o $(OUT) $(IN) $(FLAGS)

run:
	./$(OUT) $(TEST_FILE)

debug: 
	$(CC) -o $(OUT) $(IN) $(DEBUG_LAGS)

debug_run:
	gdb --args $(OUT) $(TEST_FILE) core


.PHONY: clean
clean:
	rm $(OUT)