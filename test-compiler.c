#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>
#include <math.h>

#define isDigit(X) ((X) >= '0' && (X) <= '9' ? true : false)
#define isAlphanumeric(X) (((X) >= '0' && (X) <= '9') || (((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z'))) ? true : false)
#define isLetter(X) ((((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z')) ? true : false)
#define isSpace(X) ((X) == ' ' ? true : false)
#define isEos(X) ((X) == EOF ? true : false) // end of input stream
#define SRC_DATA 256*256 
#define MAX_STRING_SIZE 32
#define MAX_NUMBER_SIZE 32

char* sourceFileName;
char* destFileName;
char* src;  // pointer to source code string; 
FILE *fptr; // pointer to source code file
FILE *ofptr; // pointer to output file

/*

=======PRODUCTION RULES=======
Extended BNF rules
... -> range
() -> inline production rule ex. A -> "c" (B | D) "e" becomes A -> "c" BD "e",  BD -> B | D
[] ->  " | () ex. A -> [+ | -] C becomes A -> " | (+ | -) becomes A -> " | B , B -> + | -
{} -> right recursive production rule ",A,AA,AAA etc 

PROGRAM: STATEMENT ['-1']

STATEMENT:
	     EXPRESSION_STATEMENT
	     | COMPOUND_STATEMENT
         | SELECTION_STATEMENT
         | FUNCTION CALL

PRIMARY_EXPRESSION: 
                  TOK_IDENTIFIER 
                  | NUMBER

EXPRESSION_STATEMENT: (PRIMARY_EXPRESSION | PRIMARY_EXPRESSION (TOK_MULT | TOK_DIV | TOK_PLUS | TOK_MINUS) EXPRESSION_STATEMENT) TOK_SMCL

BOOLEAN_EXPRESSION: PRIMARY_EXPRESSION ('==' | '<=' | '<' | '>=' | '>' | '!=') PRIMARY_EXPRESSION

COMPOUND_STATEMENT: '{' {EXPRESSION_STATEMENT} '}'

SELECTION_STATEMENT: 'if' '(' BOOLEAN_EXPRESSION ')' STATEMENT {'else if' '(' BOOLEAN_EXPRESSION ')' STATEMENT}  ['else' STATEMENT]

FUNCTION_DECLARATION:

FUNCTION_DEFINITION:

FUNCTION_CALL: TOK_IDENTIFIER '(' [TOK_IDENTIFIER | NUMBER | STRING | ARRAY] { TOK_COMMA (TOK_IDENTIFIER | NUMBER | STRING | ARRAY)} ')' TOK_SMCL

RETURN: 'return' (TOK_IDENTIFIER | DIGIT | BOOLEAN | POINTER) TOK_SMCL

ARRAY: '[' [TOK_PLUS | TOK_MINUS] NUMBER {TOK_COMMA [TOK_PLUS | TOK_MINUS] NUMBER } ']'

TOK_IDENTIFIER: {LETTER | DIGIT};

STRING: '"' ({LETTER | DIGIT}) '"'

BOOLEAN: 'true' | 'false'

POINTER: TOK_MULT {TOK_IDENTIFIER}

NUMBER: 
      TOK_INT
      | TOK_DBL
      | LONG

TOK_INT: DIGIT {DIGIT}

TOK_DBL: TOK_INT TOK_COMMA TOK_INT 'd';

TOK_LONG: TOK_INT TOK_COMMA TOK_INT 'l';

LETTER: 
      'a' | ... | 'z'
      | 'A' | ... | 'Z'

DIGIT: '0' | ... | '9'

TOK_MULT: '*'

TOK_DIV: '/'

TOK_PLUS: '+'

TOK_MINUS: '-'

TOK_COMMA: ','

TOK_SMCL: ';'

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
"main"


=======NON-TERMINAL SYMBOLS=======

*/

typedef struct token {
  int token;
  int value;
} Token;

Token currToken = {0,0};

void error(int token) {
    printf("error: expected %d token ", token);
    exit(1);
}

bool accept(int t) {
    return currToken.token == t;
}

bool expect(Token t) {
    if(accept(t.token)) {
        return true;
    }
    else {
        return false;
    }
}

enum { // terminal symbols
  TOK_EOF = EOF, // -1 
  TOK_MOD,
  TOK_MULT, // *
  TOK_DIV, // /
  TOK_PLUS, // +
  TOK_MINUS, // -
  TOK_ROUND_BRACKET_OPEN, // '('
  TOK_ROUND_BRACKET_CLOSE, // ')'
  TOK_IDENTIFIER, // variable
  TOK_INT, // int
  TOK_CURLY_BRACKET_OPEN, // '{'
  TOK_CURLY_BRACKET_CLOSE, // '{'
  TOK_SQUARE_BRACKET_CLOSE, // '['
  TOK_SQUARE_BRACKET_OPEN, // ']'
  TOK_IF, // 'if'
  TOK_ELSE_IF, // 'else if'
  TOK_ELSE, // 'else
  TOK_SMCL // ';'
};

int precedenceTable[] = { 
    2, // MOD
    2, // MULT
    2, // DIV
    3, // PLUS
    3, // MINUS
    4, // ROUND BRACKET OPEN
};
enum { 
    op_PROGRAM,
    op_STATEMENT,
    op_MOD,
    op_DIV,
    op_MULT,
    op_PLUS,
    op_MINUS,
    op_IDENTIFIER,
    op_INT
};

typedef struct ast {
    int op;
    int value;
    struct ast* left;
    struct ast* right;
} AST;

AST* makeAST(int op, int value, AST* left, AST* right) {
    AST* e = (AST*) malloc(sizeof(AST));

    if(e == NULL) {
        printf("could not allocate %zu for AST node\n", sizeof(AST));
        exit(1);
    }

    e->op = op;
    e->value = value;
    e->left = left;
    e->right = right;
    return e;
}

AST* makeChildlessAST(int op, int value) {
    return makeAST(op,value, NULL, NULL);
}

AST* makeOneChildAST(int op, int value, AST* left) {
    return makeAST(op,value, left, NULL);
}

AST* makeProgramAST (AST* left, AST* right) {
  return makeAST(op_PROGRAM, 0, left, right);
};

AST* makeStatementAST (AST* left) {
  return makeOneChildAST(op_STATEMENT,0, left);
};

AST* makeArithmeticExpressionAST (int op,AST* left, AST* right) {
  return makeAST(op, 0, left, right);
};

AST* makePrimaryExpressionAST () {
    switch(currToken.token) {
        case TOK_INT:
            return makeOneChildAST(op_INT,currToken.value, NULL);
            break;
        case TOK_IDENTIFIER:
            return makeOneChildAST(op_IDENTIFIER,0, NULL);
            break;
        default:
            printf("error: invalid token: expected %s %s", "int", "<identifier-name>");
            exit(1);
    }
};

/**
 * ====================LEXER====================
 * LL(1) Linear scan in a stream of characters, 
 * identifies the lexemes in the stream, and categorizes them into tokens
 */

/**
 * returns the next lexeme from the stream
 */ 
void getNextToken() {
    int currChar = fgetc(fptr);

    while (isSpace(currChar) || currChar == '\n' || currChar == '\t' ) // skip white space and newline
        currChar = fgetc(fptr);

    /*
    if(currChar == '_' || isAlphanumeric(currChar)) {
        char str[MAX_STRING_SIZE + 1];
        // read the whole TOK_IDENTIFIER until space
        int i = 0;
        do {
            str[i++] = currChar;
            currChar = fgetc(fptr);
        }
        while(isAlphanumeric(currChar))

        str[i] = '\0';
    }
    */

    if(isDigit(currChar)) { 
        // read the whole number
        char intStr[MAX_NUMBER_SIZE];

        int i = 0;
        do {
            intStr[i++] = currChar;
            currChar = fgetc(fptr);
        }
        while(isDigit(currChar));

        intStr[i] = '\0';

        ungetc(currChar, fptr); // unread the non-decimal back into the stream

        currToken.token = TOK_INT;
        currToken.value = atoi(intStr);
    }
    else {
        switch(currChar) {
        case '+':
            currToken.token = TOK_PLUS;
            break;
        case '-':
            currToken.token = TOK_MINUS;
            break;
        case '*':
            currToken.token = TOK_MULT;
            break;
        case '/':
            currToken.token = TOK_DIV;
            break;
        case '%':
            currToken.token = TOK_MOD;
            break;
        case TOK_EOF:
            currToken.token = TOK_EOF;
            break;
        case ';':
            currToken.token = TOK_SMCL;
            break;
        case '(':
            currToken.token = TOK_ROUND_BRACKET_OPEN;
            break;
        case ')':
            currToken.token = TOK_ROUND_BRACKET_CLOSE;
            break;
        default: // error: unrecognised char
            printf("error: unrecognised character");
            exit(1);

        }
    }
}

/**
 * ====================/LEXER====================
 */


/**
 * ====================PARSER====================
 */


int getArithmeticOp(int t) {
    int arithmeticOp;
    if(t == TOK_MOD) {
        arithmeticOp = op_MOD;
    }
    else if(t == TOK_DIV) {
        arithmeticOp = op_DIV;
    }
    else if(t == TOK_MULT) {
        arithmeticOp = op_MULT;
    }
    else if(t == TOK_PLUS) {
        arithmeticOp = op_PLUS;
    }
    else if(t == TOK_MINUS) {
        arithmeticOp = op_MINUS;
    }
    else {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }

    return arithmeticOp;
}

AST* expressionStatement(int previousTokenPrecedence) {
    AST* left,*right;

    int localToken;

    left = makePrimaryExpressionAST();

    getNextToken();

    if(accept(TOK_ROUND_BRACKET_CLOSE)) {
        return left;
    }

    localToken = currToken.token;
    

    if(localToken == TOK_SMCL) {
        return left;
    }

    // build the right tree accordingly
    while(precedenceTable[localToken] < previousTokenPrecedence) {
        getNextToken(); // either ( or int/identifier

        if(accept(TOK_ROUND_BRACKET_OPEN)) {
            int roundBracketPrecendene = precedenceTable[TOK_ROUND_BRACKET_OPEN];
            getNextToken(); // get the int/identifier
            right = expressionStatement(roundBracketPrecendene);
            if(currToken.token != TOK_ROUND_BRACKET_CLOSE) { // we're expeting a closing ) token
                error(TOK_ROUND_BRACKET_CLOSE);
            }
            getNextToken();
        }
        else 
            right = expressionStatement(precedenceTable[localToken]);

        left = makeArithmeticExpressionAST(getArithmeticOp(localToken), left, right);

        if(accept(TOK_ROUND_BRACKET_CLOSE)) {
            return left; // if ) return the tree 
        }

        localToken = currToken.token;

        if(localToken == TOK_SMCL) 
            return left;
    }

    return left;
}

AST* compoundStatement() {
    return NULL;
}

AST* selectionStatement() {
    return NULL;
}


AST* statement() {
    AST* ast = NULL;

    switch(currToken.token) {
        case TOK_INT:
        case TOK_IDENTIFIER:
            // check the next token, if its not ( it's an expression, otherwise its function call
            ast = makeStatementAST(expressionStatement(4));
            // commented as it will be implemented later 
            //ast = functionCallStatement();
            break;
        case TOK_CURLY_BRACKET_OPEN:
            ast = makeStatementAST(compoundStatement());
            break;
        case TOK_IF:
            ast = makeStatementAST(selectionStatement());
            break;
        default:
            printf("error: invalid token; expected %s or %s or %c or %s", "int", "<identifier-name>", '{', "if");
            exit(1);
    }

    return ast;
}

AST* program() {

    AST* stmt;
    while(currToken.token != TOK_EOF && currToken.token != TOK_SMCL) {
        AST* stmt = statement();
        getNextToken();
    }

    return makeProgramAST(stmt, NULL);
}

int parseArithmeticTree(AST* ast) {
    if(ast->left == NULL) {
        // return either the value of the identifier OR the number
        switch(ast->op) {
            case op_INT:
                return ast->value;
            case op_IDENTIFIER:
                //return getIdentifierValue(); not yet implemented
                break;
        }
    }

    int leftVal = parseArithmeticTree(ast->left);
    int rightVal = parseArithmeticTree(ast->right);

    if(ast->op == op_MOD) {
        return leftVal % rightVal;
    }
    else if(ast->op == op_MULT) {
        return leftVal * rightVal;
    }
    else if(ast->op == op_DIV) {
        return leftVal / rightVal;
    }
    else if(ast->op == op_PLUS) {
        return leftVal + rightVal;
    }
    else if(ast->op == op_MINUS) {
        return leftVal - rightVal;
    }
    else {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }
}

AST* parser() {
   getNextToken();
   return program();
}

/**
 * ====================/PARSER====================
 */

void cleanUp() {
    free(src);
    src = NULL;
    fclose(fptr);
}

void writePreamble() {
    destFileName = (char*) malloc(strlen(sourceFileName));
    strcpy(destFileName, sourceFileName);
    destFileName[strlen(sourceFileName) - 1] = 's';

    ofptr = fopen(destFileName, "wt");

    if (!ofptr) {
        printf("failed to create (%s) dest file", destFileName);
        exit(1);
    }

    char* fileNamePreamble = "\t.file\t\"";
    fwrite(fileNamePreamble, sizeof(fileNamePreamble), 1, ofptr);
    fwrite(sourceFileName, strlen(sourceFileName), 1 , ofptr);
    char* no_prefix_intel = "\"\r\n\t.intel_syntax noprefix"; // forces the GNU Assembler to not require % for registers
    fwrite(no_prefix_intel, strlen(no_prefix_intel), 1, ofptr);
    char* preamble = "\r\n\t.text\r\n\t.globl\tmain\r\n\t.type\tmain, @function\r\nmain:";
    fwrite(preamble, strlen(preamble), 1 , ofptr);
}

void writeExpression(AST* ast) {
    int result = parseArithmeticTree(ast->left->left);
    printf("%d\n", result);
    char strResult[(int)floor(log10(abs(result))) + 1];
    sprintf(strResult, "%d", result);
    char* movlCommand = "\n\tmov eax, ";
    fwrite(movlCommand, strlen(movlCommand), 1, ofptr);
    fwrite(strResult, sizeof(strResult), 1, ofptr);
}

void writePostamble() {
    char* postamble = "\r\n\tret\n";
    fwrite(postamble, strlen(postamble), 1, ofptr);
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

    writePreamble();
    writeExpression(parser());
    writePostamble();

    cleanUp();
}