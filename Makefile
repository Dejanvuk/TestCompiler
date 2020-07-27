CC=gcc
IN=test-compiler.c 
OUT=tc
TEST_FILE=test.c
FLAGS=-Wall -Werror

compile:
	$(CC) -o $(OUT) $(IN) $(FLAGS)

run:
	./$(OUT) $(TEST_FILE)

.PHONY: clean
clean:
	rm $(OUT)