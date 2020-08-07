#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>
#include <math.h>

#define isDigit(X) ((X) >= '0' && (X) <= '9' ? true : false)

#define isLetter(X) ((((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z')) ? true : false)
#define isSpace(X) ((X) == ' ' ? true : false)
#define isEos(X) ((X) == EOF ? true : false) // end of input stream
#define SRC_DATA 256*256 
#define MAX_STRING_SIZE 32
#define MAX_NUMBER_SIZE 32
#define MAX_SYMBOL_TABLE_SIZE 512

void intToStr(int number, char** result) {
	*result = malloc(((int)floor(log10(abs(number))) + 1));
	sprintf(*result, "%d", number);
}

bool isAlphanumeric(int currChar) {
    return (((currChar) >= '0' && (currChar) <= '9') || (((currChar) >= 'a' && (currChar) <= 'z') || ((currChar) >= 'A' && (currChar) <= 'Z')));
}

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
	     {(ASSIGNMENT_STATEMENT
         | DECLARE_STATEMENT
         | SELECTION_STATEMENT
         | FUNCTION_CALL)}

PRIMARY_EXPRESSION: 
                  TOK_currStringIFIER 
                  | NUMBER

EXPRESSION_STATEMENT: (PRIMARY_EXPRESSION | PRIMARY_EXPRESSION (TOK_MULT | TOK_DIV | TOK_PLUS | TOK_MINUS) EXPRESSION_STATEMENT) TOK_SMCL

ASSIGNMENT_STATEMENT: TOK_currStringIFIER TOK_ASSIGN (EXPRESSION_STATEMENT | FUNCTION_CALL) TOK_SMCL

DECLARE_STATEMENT: TYPE_SPECIFIER TOK_currStringIFIER (ASSIGNMENT_STATEMENT | TOK_SMCL)

BOOLEAN_EXPRESSION: PRIMARY_EXPRESSION ('==' | '<=' | '<' | '>=' | '>' | '!=') PRIMARY_EXPRESSION

SELECTION_STATEMENT: 'if' '(' BOOLEAN_EXPRESSION ')' STATEMENT {'else if' '(' BOOLEAN_EXPRESSION ')' STATEMENT}  ['else' STATEMENT]

FUNCTION_DECLARATION:

FUNCTION_DEFINITION:

FUNCTION_CALL: TOK_currStringIFIER '(' [TOK_currStringIFIER | NUMBER | STRING | ARRAY] { TOK_COMMA (TOK_currStringIFIER | NUMBER | STRING | ARRAY)} ')' TOK_SMCL

RETURN: 'return' (TOK_currStringIFIER | DIGIT | BOOLEAN | POINTER) TOK_SMCL

ARRAY: '[' [TOK_PLUS | TOK_MINUS] NUMBER {TOK_COMMA [TOK_PLUS | TOK_MINUS] NUMBER } ']'

TOK_currStringIFIER: {LETTER | DIGIT};

STRING: '"' ({LETTER | DIGIT}) '"'

BOOLEAN: 'true' | 'false'

POINTER: TOK_MULT {TOK_currStringIFIER}

TYPE_SPECIFIER: 'int' 
              | 'double'

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

TOK_ASSIGN: '='

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

char* currString = NULL;

Token currToken = {0,0, NULL};

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
  TOK_TYPE_SPECIFIER, // 'int', 'double' 
  TOK_ASSIGN, // =
  TOK_currStringIFIER, // variable
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
    op_TYPE,
    op_DECLARE,
    OP_ASSIGN,
    op_currStringIFIER,
    op_INT,
};

int getTypeSpecifier(char* type) {
	if(!strcmp(type, "int"))
		return 0;
    else if (!strcmp(type, "double"))
        return 1;
    else // float
        return 2;
}

typedef struct entry {
    int index;
    char* name;
    int type; // 0-int 1-double
    int scope; // 0- global 1-local/function
} ENTRY;

ENTRY SymbolTable[MAX_SYMBOL_TABLE_SIZE];
int SymbolTableIndex = 0;

/*
return: the index in the tabel of the new symbol
*/
int addSymbolEntry(char* name, char* type) {
    ENTRY* e = (ENTRY*) malloc(sizeof(ENTRY));

    if(e == NULL) {
        printf("could not allocate %zu for ENTRY node\n", sizeof(ENTRY));
        exit(1);
    }

    e->index = SymbolTableIndex++;
    e->name = currString;
    e->type = getTypeSpecifier(type);

    // determine scope based on ast parent
    // program parent -> global
    // function parent -> local
}

/*
return: true if symbol exists in the table, false otherwise
*/
bool isSymbol(ENTRY e) {
    return true;
}

typedef struct ast {
    int op;
    int value;
    struct ast* left;
    struct ast* mid;
    struct ast* right;
} AST;

AST* makeAST(int op, int value, AST* left, AST* mid, AST* right) {
    AST* e = (AST*) malloc(sizeof(AST));

    if(e == NULL) {
        printf("could not allocate %zu for AST node\n", sizeof(AST));
        exit(1);
    }

    e->op = op;
    e->value = value;
    e->left = left;
    e->mid = mid;
    e->right = right;
    return e;
}

AST* makeChildlessAST(int op, int value) {
    return makeAST(op,value, NULL, NULL, NULL);
}

AST* makeOneChildAST(int op, int value, AST* left) {
    return makeAST(op,value, left, NULL, NULL);
}

AST* makeProgramAST (AST* left) {
  return makeAST(op_PROGRAM, 0, left, NULL, NULL);
};

AST* makeStatementAST (AST* left) {
  return makeOneChildAST(op_STATEMENT,0, left);
};

AST* makeArithmeticExpressionAST (int op,AST* left, AST* right) {
  return makeAST(op, 0, left, NULL, right);
};

AST* makeDeclareAST (int op,AST* left,AST* mid, AST* right) {
  return makeAST(op, 0, left, mid, right);
};

AST* makeAssignmentAST (int op,AST* left,AST* mid, AST* right) {
  return makeAST(op, 0, left, mid, right);
};

AST* makePrimaryExpressionAST () {
    switch(currToken.token) {
        case TOK_INT:
            return makeOneChildAST(op_INT,currToken.value, NULL);
            break;
        case TOK_currStringIFIER:
            return makeOneChildAST(op_currStringIFIER,0, NULL);
            break;
        default:
            printf("error: invalid token: expected %s %s", "int", "<currStringifier-name>");
            exit(1);
    }
};

/**
 * ====================LEXER====================
 * LL(1) Linear scan in a stream of characters, 
 * currStringifies the lexemes in the stream, and categorizes them into tokens
 */

/*
* unreads the latest token read back in the stream
*/
void putTokenBack() {

}

/**
 * returns the next lexeme from the stream
 */ 
void getNextToken() {
    int currChar = fgetc(fptr);

    while (isSpace(currChar) || currChar == '\n' || currChar == '\t' ) // skip white space and newline
        currChar = fgetc(fptr);
    
    if(currChar == '_' || isAlphanumeric(currChar)) {
        // read the whole TOK_currStringIFIER until space
        currString = (char*) malloc(MAX_STRING_SIZE + 1);
        int i = 0;
        do {
            currString[i++] = currChar;
            currChar = fgetc(fptr);
        }
        while(isAlphanumeric(currChar));

        ungetc(currChar, fptr); // unread the non-alpha back into the stream

        currString[i] = '\0';

        // check if it's a reserved keyword first
        if(!strcmp(currString, "int")) {
            currToken.token = TOK_TYPE_SPECIFIER;
        }
        else { //it's an currStringifier
            currToken.token = TOK_currStringIFIER;
        }

        return;
    }
    

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
        case '=':
            currToken.token = TOK_ASSIGN;
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

    // first check if its TOK_ROUND_BRACKET_OPEN
    if(!accept(TOK_ROUND_BRACKET_OPEN)) { // skip
        left = makePrimaryExpressionAST();

        getNextToken();

        if(accept(TOK_ROUND_BRACKET_CLOSE)) {
            return left;
        }

        localToken = currToken.token;
    

        if(localToken == TOK_SMCL) {
            return left;
        }
    }
    else { // 
        /*
        left = expressionStatement(precedenceTable[TOK_ROUND_BRACKET_OPEN]);
        getNextToken();
        if(accept(TOK_ROUND_BRACKET_CLOSE)) {
            getNextToken();
            return left;
        }
        else {
            error(TOK_ROUND_BRACKET_CLOSE);
        }
        */
    }

    // build the right tree accordingly
    while(precedenceTable[localToken] < previousTokenPrecedence) {
        getNextToken(); // either ( or int/currStringifier

        if(accept(TOK_ROUND_BRACKET_OPEN)) {
            int roundBracketPrecendene = precedenceTable[TOK_ROUND_BRACKET_OPEN];
            getNextToken(); // get the int/currStringifier or could be one or many (
            right = expressionStatement(roundBracketPrecendene);
            if(currToken.token != TOK_ROUND_BRACKET_CLOSE) { // we're expeting a closing ) token
                error(TOK_ROUND_BRACKET_CLOSE);
            }
            getNextToken();
            /*
            fresh out of ), check if current token has higher precedence ex 1 + (2 * 3) / 2 * 5 - 9 , / has higher precedence than +
            1 + (2 * 3) / (2 - 5) * 9
            */
            while(currToken.token != TOK_SMCL && currToken.token != TOK_ROUND_BRACKET_CLOSE && precedenceTable[currToken.token] < localToken) {
                //run again the loop and return the correct right, a tree where the currToken.token has lower or equal precedence to localToken
                int newLocalToken = currToken.token;
                getNextToken(); // int/currStringifier or could be followed by one or many (
                AST* newRight = expressionStatement(precedenceTable[localToken]);
                right = makeArithmeticExpressionAST(getArithmeticOp(newLocalToken), right, newRight);
            } 
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

AST* declareStatement() {
    
}

AST* assignmentStatement() {
    return NULL;
}

AST* selectionStatement() {
    return NULL;
}


AST* statement() {
    AST* ast = NULL;

    switch(currToken.token) {
        case TOK_INT:
            ast = expressionStatement(4);
            break;
        case TOK_IF:
            ast = selectionStatement();
            break;
        case TOK_TYPE_SPECIFIER:
            ast = declareStatement();
            break;
        default:
            printf("error: invalid token; expected %s or %s or %c or %s", "int", "<currStringifier-name>", '{', "if");
            exit(1);
    }

    return ast;
}

AST* program() {
    AST* firstStatement = NULL;
    AST* prevStatement = NULL;
    AST* stmt = NULL;
    while(currToken.token != TOK_EOF) {
        stmt = statement();
        if(firstStatement == NULL) {
            firstStatement = stmt;
            prevStatement = stmt;
        }
        else {
            prevStatement->right = stmt;
            prevStatement = stmt;
        }
        getNextToken();
    }

    return makeProgramAST(firstStatement);
}

AST* parser() {
   getNextToken();
   AST* ret = program();
   return ret;
}

/**
 * ====================/PARSER====================
 */

/** 
 * 
 * ====================CODE GENERATION====================
 * 
 */

char* const generalPurposeRegisters[]         = {"rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"};
char* const addedPurposeRegisters[]           = {"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};

int availableGeneralPurposeRegisters[] = {0,0,0,0,0,0,0,0};
int availableAddedPurposeRegisters[] = {0,0,0,0,0,0,0,0};

char* const dataMovementInstructions[]        = {"\n\tmov ", "\n\tpush ", "\n\tpov ", "\n\tlea "};
char* const arithmeticAndLogicInstructions[]  = {"\n\tadd ", "\n\tsub ", "\n\tinc ", "\n\tdec ", "\n\timul ", "\n\tidiv ",
                                                     "\n\tand ", "\n\tor ", "\n\txor ", "\n\tnot ", "\n\tneg ","\n\tshl ", "\n\tshr "};
char* const convertInstructions[] = {"\n\tcwd ", "\n\tcdq", "\n\tcqo "};  
char* const controlFlowInstructions[] = {""};

/* 
return: the first available purpose register
*/
int getAvailableRegister() {
    int i = 0;
    for(; i < 8; i++) {
        if(availableAddedPurposeRegisters[i] == 0) {
            availableAddedPurposeRegisters[i] = 1;
            return i;
        }
    }
    return -1; // no available register, use stack
}

void write_register(char* name) {
    fwrite((char*)name, strlen((char*)name), 1, ofptr);
}

void write_number(int intNumber) { 
    char* number = NULL;
    intToStr(intNumber, &number);
    fwrite(number, strlen(number), 1, ofptr);
}

void write_memory(char* name) { //[name]
    fwrite(" [", 1, 1, ofptr);
    fwrite(name, strlen(name), 1, ofptr);
    fwrite("]", 1, 1, ofptr);
}

/*
Move

Syntax
mov <reg>,<reg>
mov <reg>,<mem>
mov <mem>,<reg>
mov <reg>,<const>
mov <mem>,<const>

destName: name of left side reg/mem
fromName: name of right side reg/mem or empty for const
flag: 0-reg 1-mem 2-const
*/

void asm_mov_write(char* destName,char* fromName,int flagDest,int flagFrom, int con) { 

    if((flagDest == 1) && (flagFrom == 1)) {
        printf("error: memory-to-memory move is not supported in asm_mov!");
        exit(1);
    }

    fwrite(dataMovementInstructions[0], strlen(dataMovementInstructions[0]), 1, ofptr);

    if(flagDest == 0) { // register
        write_register(destName);
    }
    else if(flagDest == 1) { // memory
        write_memory(destName);
    }
    else {
        printf("error: invalid dest flag in asm_mov!");
        exit(1);
    }

    fwrite(", ", 2, 1, ofptr);

    if(flagFrom == 0) { // register
        write_register(fromName);
    }
    else if(flagFrom == 1) { // memory
        write_memory(fromName);
    }
    else if(flagFrom == 2) { // constant value
        write_number(con);
        
    }
    else {
        printf("error: invalid from flag in asm_mov!");
        exit(1);
    }
}

/*
Push stack

Syntax
push <reg32>
push <mem>
push <con32>

name: name of reg,mem or con32
flag: 0-reg 1-mem 2-con
*/

void asm_push_write(char* name,int con,int flag) {
    fwrite(dataMovementInstructions[1], strlen(dataMovementInstructions[1]), 1, ofptr);

    if(flag == 0) // reg
        write_register(name);
    else if(flag == 1)  // mem
        write_memory(name);
    else if(flag == 2) { // con32
        write_number(con);
    }
    else {
        printf("error: invalid from flag in asm_push!");
        exit(1);
    }
}

/*
Pop stack

Syntax
pop <reg32>
pop <mem>

name: name of reg or mem
flag: 0-reg 1-mem
*/
void asm_pop_write(char* name,int flag) {
    fwrite(dataMovementInstructions[2], strlen(dataMovementInstructions[2]), 1, ofptr);

    if(flag == 0) 
        write_register(name);
    else if(flag == 1) 
        write_memory((char*)name);
    else {
        printf("error: invalid from flag in asm_pop!");
        exit(1);
    }
}

/* 
Load effective address

Syntax
lea <reg32>,<mem>

name: name of reg or mem
*/
void asm_lea_write(char* name) {
    fwrite(dataMovementInstructions[3], strlen(dataMovementInstructions[3]), 1, ofptr);

    write_memory(name);
}

/* Arithmetic and Logic Instructions */

/* 
Integer Addition or Substraction

Syntax
add/sub <reg>,<reg>
add/sub <reg>,<mem>
add/sub <mem>,<reg>
add/sub <reg>,<con>
add/sub <mem>,<con>

inst: add-0 sub-1
destName: name of left side reg/mem
fromName: name of right side reg/mem or empty for const
flag: 0-reg 1-mem 2-const

*/
void asm_add_or_sub_write(int inst, char* destName, char* fromName,int con, int flagDest, int flagFrom) {
    if((flagDest == 1) && (flagFrom == 1)) {
        printf("error: memory-to-memory is not supported in asm_add_or_sub!");
        exit(1);
    }

    if(inst < 0 || inst > 1) {
        printf("error: invalid inst flag in asm_add_or_sub!");
        exit(1);
    }

    fwrite(arithmeticAndLogicInstructions[inst], strlen(arithmeticAndLogicInstructions[inst]), 1, ofptr);

    if(flagDest == 0) { // register
        write_register(destName);
    }
    else if(flagDest == 1) { // memory
        write_memory(destName);
    }
    else {
        printf("error: invalid dest flag in asm_add_or_sub!");
        exit(1);
    }

    fwrite(", ", 2, 1, ofptr);

    if(flagFrom == 0) { // register
        write_register(fromName);
    }
    else if(flagFrom == 1) { // memory
        write_memory(fromName);
    }
    else if(flagFrom == 2) { // constant value
        write_number(con);
        
    }
    else {
        printf("error: invalid from flag in asm_add_or_sub!");
        exit(1);
    }

}

/* 
Integer Multiplication

Syntax
imul <reg32>,<reg32>
imul <reg32>,<mem>
imul <reg32>,<reg32>,<con>
imul <reg32>,<mem>,<con>

destReg: name of left side reg
fromName: name of right side reg/mem 
flag: 0-reg 1-mem 
con: con for 3rd argument
isCon: if the 3 operand imul should be used
*/
void asm_imul_write(char* destReg, char* fromName,int flagDest, int flagFrom, int con, bool isCon) {
    fwrite(arithmeticAndLogicInstructions[4], strlen(arithmeticAndLogicInstructions[4]), 1, ofptr);

    if(flagDest == 0) { // register
        write_register(destReg);
    }
    else {
        printf("error: invalid from flag in asm_imul!");
        exit(1);
    }

    fwrite(", ", 2, 1, ofptr);

    if(flagFrom == 0) { // register
        write_register(fromName);
    }
    else if(flagFrom == 1) { // memory
        write_memory(fromName);
    }
    else {
        printf("error: invalid from flag in asm_imul!");
        exit(1);
    }

    if(isCon) {
        fwrite(", ", 2, 1, ofptr);
        write_number(con);
    }
}

/* 
Integer Division

Syntax
idiv <reg32>
idiv <mem>

flag: 0-reg32 1-mem
*/
void asm_idiv_write(char* name, int flag) {
    // asm_mov_write(generalPurposeRegisters[2], NULL, 0, 2, 0); for positive rax
    // asm_mov_write(generalPurposeRegisters[2], NULL, 0, 2, -1); for negative rax
    // zeroing rdx with negative dividend leads to large positive Floating point exception so just sign extend rdx
    fwrite(convertInstructions[2], strlen(convertInstructions[2]), 1, ofptr);

    fwrite(arithmeticAndLogicInstructions[5], strlen(arithmeticAndLogicInstructions[5]), 1, ofptr);

    if(flag == 0) { // register
        write_register(name);
    }
    else if(flag == 1) { // memory
        write_memory(name);
    }
    else {
        printf("error: invalid from flag in asm_idiv!");
        exit(1);
    }

}

int parseArithmeticTree(AST* ast) {
    if(ast->left == NULL) {
        int op = ast->op;
        if(op == op_INT) {
            int reg = getAvailableRegister();
            asm_mov_write(addedPurposeRegisters[reg],NULL,0, 2, ast->value);
            return reg;
        }
        else if(op == op_currStringIFIER) {
            //return getcurrStringifierValue(); not yet implemented
            return -1;
        }
    }

    int leftReg = parseArithmeticTree(ast->left);
    int rightReg = parseArithmeticTree(ast->right);

    if(ast->op == op_MOD) {
        return leftReg;
    }
    else if(ast->op == op_MULT) {
        asm_imul_write(addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg], 0, 0, 0, false);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if(ast->op == op_DIV) {
        // 64 bit integer RDX:RAX, RDX has the most significant four bytes and RAX has the least significant four bytes
        asm_mov_write(generalPurposeRegisters[0],addedPurposeRegisters[leftReg],0, 0, 0); // copy dividend argument into RAX
        asm_idiv_write(addedPurposeRegisters[rightReg], 0); // divide by right reg divisor argument
        asm_mov_write(addedPurposeRegisters[leftReg],generalPurposeRegisters[0],0, 0, 0); // move the result from RAX to left reg
        availableGeneralPurposeRegisters[0] = 0;
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if(ast->op == op_PLUS) {
        asm_add_or_sub_write(0, addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg], 0, 0, 0);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if(ast->op == op_MINUS) {
        asm_add_or_sub_write(1, addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg], 0, 0, 0);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }
}

int optimized_parseArithmeticTree(AST* ast) {
    if(ast->left == NULL) {
        // return either the value of the currStringifier OR the number
        switch(ast->op) {
            case op_INT:
                return ast->value;
            case op_currStringIFIER:
                //return getcurrStringifierValue(); not yet implemented
                break;
        }
    }

    int leftVal = optimized_parseArithmeticTree(ast->left);
    int rightVal = optimized_parseArithmeticTree(ast->right);

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
    char* preamble = "\r\n\t.text\r\n\t.globl\tmain\r\n\t.type\tmain, @function\r\nmain:\r\n\tpush\trbp\r\n\tmov\trbp, rsp";
    fwrite(preamble, strlen(preamble), 1 , ofptr);
}

void writeExpression(AST* ast) {
    printf("expr value: %d\n", optimized_parseArithmeticTree(ast->left->left));
    int resultReg = parseArithmeticTree(ast->left->left);
    printf("register that has the expr val:%s\n", addedPurposeRegisters[resultReg]);

    asm_mov_write(generalPurposeRegisters[0],addedPurposeRegisters[resultReg],0, 0, 0);
}

void writePostamble() {
    char* postamble = "\r\n\tpop\trbp\r\n\tret\n";
    fwrite(postamble, strlen(postamble), 1, ofptr);
}

/** 
 * 
 * ====================/CODE GENERATION====================
 */

void cleanUp() {
    free(src);
    src = NULL;
    fclose(fptr);
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