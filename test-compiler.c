#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>

#define isNumber(X) ((X) >= '0' && (X) <= '9' ? true : false)
#define isAlphanumeric(X) (((X) >= '0' && (X) <= '9') || (((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z'))) ? true : false)
#define isLetter(X) ((((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z')) ? true : false)
#define isSpace(X) ((X) == ' ' ? true : false)
#define isEos(X) ((X) == EOF ? true : false) // end of input stream
#define SRC_DATA 256*256 

char* sourceFileName;

char currChar;  // current char read
char* src;  // pointer to source code string; 

char* tokenName;
uint16_t tokenValue;

FILE *fptr;

/*

=======PRODUCTION RULES=======
Extended BNF rules
... -> range
() -> inline production rule ex. A -> "c" (B | D) "e" becomes A -> "c" BD "e",  BD -> B | D
[] ->  " | () ex. A -> [+ | -] C becomes A -> " | (+ | -) becomes A -> " | B , B -> + | -
{} -> right recursive production rule ",A,AA,AAA etc 

PROGRAM: STATEMENT

STATEMENT:
	     EXPRESSION_STATEMENT
	     | COMPOUND_STATEMENT
         | SELECTION_STATEMENT
         | FUNCTION CALL

EXPRESSION_STATEMENT: ["+" | "-"] IDENTIFIER {("+" | "-" | "*" | "/") IDENTIFIER} ";"

BOOLEAN_EXPRESSION: (IDENTIFIER | NUMBER) ("==" | "<=" | "<" | ">=" | ">" | "!=") (IDENTIFIER | NUMBER)

COMPOUND_STATEMENT: "{" {EXPRESSION_STATEMENT} "}"

SELECTION_STATEMENT: "if" '(' BOOLEAN_EXPRESSION ')' STATEMENT [{"else if" '(' BOOLEAN_EXPRESSION ')' STATEMENT}]  ["else" STATEMENT]

FUNCTION_DEFINITION:

FUNCTION CALL: IDENTIFIER "(" [IDENTIFIER | NUMBER | STRING | ARRAY] { IDENTIFIER | NUMBER | STRING | ARRAY} ")" 

ARRAY: "[" ["+" | "-"] NUMBER {"," ["+" | "-"] NUMBER } "]"

IDENTIFIER: {LETTER | NUMBER};

STRING: "'" ({LETTER | NUMBER}) "'"

BOOLEAN: "true" | "false"

POINTER: "*" {IDENTIFIER}

LETTER: 
      "a" | ... | "z" 
      | "A" | ... | "Z"

NUMBER: "0" | ... | "9" 

=======TERMINAL SYMBOLS=======
[a-zA-Z]
[0-9]
"if" "else" "else if" 
"int" "char"
"main"
"="
"+" "-" "*" "/" "%" 
"++" "--" "+=" "-=""*=" "/=" 
">" "<" "<=" ">=" "!=" "==""||" "&&" "?"
"<<" ">>" "|" "&" "^"
"[" "]"
"(" ")"
";"
"return"


=======NON-TERMINAL SYMBOLS=======






*/


enum Token {
  tok_eof = EOF,
  tok_identifier, // [a-zA-Z]\w* ex. x, color, UP
  tok_keyword, // int, if, while, return, function
  tok_separator, // }, (, ;
  tok_operator, // +, <, =
  tok_literal, // string literal true, 6.02e23, "music" or integer literal [0-9]+
  tok_comment // /* Retrieves user data */ , // must be negative
};



/**
 * ====================LEXER====================
 * LL(1) Linear scan in a stream of characters, 
 * identifies the lexemes in the stream, and categorizes them into tokens
 */

/**
 * returns the next char from the stream
 */ 
void getNextChar() {
    //fread(&currChar, sizeof(char), 1, fptr);
    currChar = fgetc(fptr);
}

/**
 * returns the next lexeme from the stream
 */ 
void getNextToken() {

}

/**
 * ====================/LEXER====================
 */


/**
 * ====================PARSER====================
 */
void parser() {
    
}
/**
 * ====================/PARSER====================
 */

void cleanUp() {
    free(src);
    src = NULL;
    pclose(fptr);
}

int main(int argc, char **argv) {

    if(argc < 2) {
        printf("error : no input file specified");
        return -1;
    }

    sourceFileName = argv[1];
    fptr = fopen(sourceFileName, "r");

    if (!fptr) {
        printf("failed to open (%s) source file", sourceFileName);
        return -1;
    }

    if (!(src = malloc(sizeof(char) * SRC_DATA))) {
        printf("could not allocate %d for source data\n", SRC_DATA);
        return -1;
    }

    do {
        getNextChar();
        if(!isEos(currChar))
            printf("%c", currChar);
        else break;
    }
    while(true);

    cleanUp();
}