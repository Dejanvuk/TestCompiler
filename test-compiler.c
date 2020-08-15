#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>
#include <math.h>

#include "parser.h"
#include "codegen.h"

#define isDigit(X) ((X) >= '0' && (X) <= '9' ? true : false)

#define isSpace(X) ((X) == ' ' ? true : false)
#define isEos(X) ((X) == EOF ? true : false) // end of input stream
#define SRC_DATA 256 * 256
#define MAX_STRING_SIZE 32
#define MAX_NUMBER_SIZE 32

void intToStr(int number, char **result)
{
    *result = malloc(((int)floor(log10(abs(number))) + 1));
    sprintf(*result, "%d", number);
}

bool isLetter(int currChar)
{
    return ((currChar) >= 'a' && (currChar) <= 'z') || ((currChar) >= 'A' && (currChar) <= 'Z');
}

bool isAlphanumeric(int currChar)
{
    return (((currChar) >= '0' && (currChar) <= '9') || (((currChar) >= 'a' && (currChar) <= 'z') || ((currChar) >= 'A' && (currChar) <= 'Z')));
}

char *sourceFileName;
char *destFileName;
char *src;   // pointer to source code string;
FILE *fptr;  // pointer to source code file
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
                  TOK_IDENTIFIER 
                  | NUMBER

EXPRESSION_STATEMENT: (PRIMARY_EXPRESSION | PRIMARY_EXPRESSION (TOK_MULT | TOK_DIV | TOK_PLUS | TOK_MINUS) EXPRESSION_STATEMENT) TOK_SMCL

ASSIGNMENT_STATEMENT: TOK_IDENTIFIER TOK_ASSIGN (EXPRESSION_STATEMENT | FUNCTION_CALL) TOK_SMCL

DECLARE_STATEMENT: TYPE_SPECIFIER TOK_IDENTIFIER (ASSIGNMENT_STATEMENT | TOK_SMCL)

BOOLEAN_EXPRESSION: PRIMARY_EXPRESSION ('==' | '<=' | '<' | '>=' | '>' | '!=') PRIMARY_EXPRESSION

SELECTION_STATEMENT: 'if' '(' BOOLEAN_EXPRESSION ')' STATEMENT {'else if' '(' BOOLEAN_EXPRESSION ')' STATEMENT}  ['else' STATEMENT]

FUNCTION_DECLARATION:

FUNCTION_DEFINITION:

FUNCTION_CALL: TOK_IDENTIFIER '(' [TOK_IDENTIFIER | NUMBER | STRING | ARRAY] { TOK_COMMA (TOK_IDENTIFIER | NUMBER | STRING | ARRAY)} ')' TOK_SMCL

RETURN: 'return' (TOK_IDENTIFIER | DIGIT | BOOLEAN | POINTER) TOK_SMCL

ARRAY: '[' [TOK_PLUS | TOK_MINUS] NUMBER {TOK_COMMA [TOK_PLUS | TOK_MINUS] NUMBER } ']'

TOK_currStringIFIER: {LETTER | DIGIT};

STRING: '"' ({LETTER | DIGIT}) '"'

BOOLEAN: 'true' | 'false'

POINTER: TOK_MULT {TOK_IDENTIFIER}

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

char *const specialPurposeRegisters[] = {"rsp", "rbp", "rip", "rflags"};
char *const generalPurposeRegisters[] = {"rax", "rcx", "rdx", "rbx", "rsi", "rdi"};
char *const addedPurposeRegisters[] = {"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};

int availableGeneralPurposeRegisters[] = {0, 0, 0, 0, 0, 0, 0, 0};
int availableAddedPurposeRegisters[] = {0, 0, 0, 0, 0, 0, 0, 0};

char *const dataMovementInstructions[] = {"mov", "push", "pov", "lea"};
char *const arithmeticAndLogicInstructions[] = {"add", "sub", "inc", "dec", "imul", "idiv",
                                                "and", "or", "xor", "not", "neg", "shl", "shr"};
char *const convertInstructions[] = {"cwd", "cdq", "cqo"};
char *const controlFlowInstructions[] = {"jmp", "je", "jne", "jz", "jg", "jge", "jl", "jle", "cmp", "call", "ret"};
char *const assemblerDirectives[] = {".align", ".bss", ".byte", ".8byte", ".comm", ".data",
                                     ".double", ".file", ".float", ".globl", ".ident", ".lcomm",
                                     ".long", ".quad", ".rel", ".section", ".set", ".size", ".string",
                                     ".text", ".type", ".value", ".zero"};

char *lastAssemblerDirective = NULL;

typedef struct token
{
    int token;
    int value;
} Token;

char *currString = NULL; // holds the current string read from the stream

Token currToken = {0, 0};

void error(int token)
{
    printf("error: expected %d token ", token);
    exit(1);
}

bool accept(int t)
{
    return currToken.token == t;
}

bool expect(Token t)
{
    if (accept(t.token))
    {
        return true;
    }
    else
    {
        return false;
    }
}

enum
{                  // terminal symbols
    TOK_EOF = EOF, // -1
    TOK_MOD,
    TOK_MULT,                 // *
    TOK_DIV,                  // /
    TOK_PLUS,                 // +
    TOK_MINUS,                // -
    TOK_ROUND_BRACKET_OPEN,   // '('
    TOK_ROUND_BRACKET_CLOSE,  // ')'
    TOK_TYPE_SPECIFIER,       // 'int', 'double'
    TOK_ASSIGN,               // =
    TOK_IDENTIFIER,           // variable
    TOK_INT,                  // int
    TOK_CURLY_BRACKET_OPEN,   // '{'
    TOK_CURLY_BRACKET_CLOSE,  // '{'
    TOK_SQUARE_BRACKET_CLOSE, // '['
    TOK_SQUARE_BRACKET_OPEN,  // ']'
    TOK_IF,                   // 'if'
    TOK_ELSE_IF,              // 'else if'
    TOK_ELSE,                 // 'else
    TOK_SMCL,                 // ';'
    TOK_COMMA,                // ,
    TOK_RETURN                // 'return'
};

int precedenceTable[] = {
    2, // MOD
    2, // MULT
    2, // DIV
    3, // PLUS
    3, // MINUS
    4, // ROUND BRACKET OPEN
};
enum
{
    op_PROGRAM,
    op_MOD,
    op_DIV,
    op_MULT,
    op_PLUS,
    op_MINUS,
    op_DECLARE,
    OP_ASSIGN,
    op_IDENTIFIER,
    op_INT,
    op_RETURN,
    op_FCALL
};

int getTypeSpecifier(char *type)
{
    if (!strcmp(type, "int"))
        return 0;
    else if (!strcmp(type, "double"))
        return 1;
    else // float
        return 2;
}

int getTypeSpecifierSize(int type)
{
    switch (type)
    {
    case 0:
        return 8;
    case 1:
        return 16;
    default:
        printf("error: undefined type specifier");
        exit(1);
    }
}

char *typeSpecifierToStr(int typeSpecifier)
{
    switch (typeSpecifier)
    {
    case 0:
        return "int";
    case 1:
        return "double";
    default:
        printf("error: undefined type specifier");
        exit(1);
    }
}

ENTRY *SymbolTable[MAX_SYMBOL_TABLE_SIZE];
int symbolTableIndex = 0;

int getSymbolScope(int owner)
{
    return (owner == 0) ? 0 : 1;
}

/*
return: the index in the tabel of the new symbol
*/
int addSymbolEntry(ENTRY **table, int *size, char *name, char *type_specifier, int type, int scope, int owner)
{
    ENTRY *e = (ENTRY *)malloc(sizeof(ENTRY));

    if (e == NULL)
    {
        printf("could not allocate %zu for ENTRY node\n", sizeof(ENTRY));
        exit(1);
    }

    int oldSize = *size;

    e->index = oldSize;
    e->name = currString;
    e->type_specifier = getTypeSpecifier(type_specifier);
    e->type = type;
    e->scope = scope;
    e->owner = owner;
    e->localSymbolTableIndex = 0;

    table[oldSize] = e;

    *size += 1;

    return oldSize;
}

/* 
Finds the specific table of the owner and adds the symbol in it
*/
int addSymbolToTheOwnersTable(int owner, char *identifierName, char *typeSpecifier, int type, int scope)
{
    if (owner == 0)
    {
        // only add the symbol if it wasnt declared already
        if (getSymbolIndex(SymbolTable, symbolTableIndex, identifierName) != -1)
        {
            printf("error: duplicate global symbol %s ", identifierName);
            exit(1);
        }

        int symbolIndex = addSymbolEntry(SymbolTable, &symbolTableIndex, identifierName, typeSpecifier, type, scope, owner);
        return symbolIndex;
    }
    else
    {
        ENTRY *functionEntry = lookupSymbol(SymbolTable, owner);
        ENTRY **table = functionEntry->localSymbolTable;
        int *size = &(functionEntry->localSymbolTableIndex);
        // in a function we might use a global variable which needs to be copied over the local table of the function
        // so we skip the check as we already determined the symbol exists globally
        int localSymbolIndex = getSymbolIndex(table, *size, identifierName);
        if (localSymbolIndex != -1 && scope != 0)
        {
            ENTRY *duplicateSymbol = lookupSymbol(table, localSymbolIndex);
            // if the duplicate symbol is global make it local
            if (duplicateSymbol->scope == 0)
            {
                // we assume both the global and the local have the same signature
                // later we will completely overwrite the old table entry with the new one
                duplicateSymbol->scope = 1;
                return duplicateSymbol->index;
            }
            else
            { // else it's duplicated local,throw error
                printf("error: duplicate local symbol %s in function %s ", identifierName, functionEntry->name);
                exit(1);
            }
        }

        int symbolIndex = addSymbolEntry(table, size, identifierName, typeSpecifier, type, scope, owner);
        return symbolIndex;
    }
}

/* 
Basic syntax analysis 
Checks where the current string read was declared previously, local table has precedence over global
if the local string is a global variable, we copy the global's variable entry table over the local function table
return : the index in the table of the declared identifier
*/

int identifierWasDeclared(int owner)
{
    if (owner == 0)
    { // should be removed as only constant values are allowed as rvalue in global variables
        int globalIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString);
        if (globalIndex == -1)
        {
            printf("error: symbol %s is not defined!", currString);
            exit(1);
        }
        else
        {
            return globalIndex;
        }
    }
    else
    { // owner is a function
        // then search in the local owner table
        ENTRY *functionEntry = lookupSymbol(SymbolTable, owner);
        ENTRY **table = functionEntry->localSymbolTable;
        int size = functionEntry->localSymbolTableIndex;
        int localIndex = getSymbolIndex(table, size, currString);
        if (localIndex == -1)
        { // // search maybe the variable is global
            int globalIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString);
            if (globalIndex == -1)
            {
                printf("error: symbol %s is not defined!", currString);
                exit(1);
            }
            else
            {
                /*either:
                add a new symbol entry in the local table, with owner 0 and index poting to the index of the program's table
                or just copy the symbol entry from the global to the local
                here we use the latter */
                ENTRY *pEntry = lookupSymbol(SymbolTable, globalIndex);
                int pIndex = addSymbolToTheOwnersTable(owner, pEntry->name, typeSpecifierToStr(pEntry->type_specifier), pEntry->type, 0);
                return pIndex;
            }
        }
        else
            return localIndex;
    }
}

/*
return: the ENTRY if the symbol exists in the table or null otherwise
*/
ENTRY *lookupSymbol(ENTRY **table, int index)
{
    if (index >= MAX_SYMBOL_TABLE_SIZE)
    {
        printf("invalid symbol table index!");
        exit(1);
    }
    return table[index];
}

/* Get the ENTRY from the owner's table */

ENTRY *getEntryFromOwner(int owner, int index)
{
    if (owner == 0)
    {
        return lookupSymbol(SymbolTable, index);
    }
    else
    {
        // first get the owner's ENTRY and then get it's symbol table
        ENTRY **ownersTable = lookupSymbol(SymbolTable, owner)->localSymbolTable;
        return lookupSymbol(ownersTable, index);
    }
}

/*
return: the index of the symbol
*/
int getSymbolIndex(ENTRY **table, int size, char *name)
{
    int index = -1;
    for (int i = 0; i < size; i++)
    {
        if (!strcmp(name, table[i]->name))
        {
            index = table[i]->index;
            break;
        }
    }

    return index;
}

/* return the number of parameters the function has */
int getNrOfParameters(ENTRY **table)
{
    int nr = 0;
    ENTRY *tempE = lookupSymbol(table, nr);
    while (tempE->scope == 2)
    {
        tempE = lookupSymbol(table, ++nr);
    }
    return nr;
}

int getLocalStackSize(ENTRY **table, int tableSize)
{
    int start = getNrOfParameters(table);
    int stackSize = 0;
    for (int i = start; i < tableSize; i++)
    {
        ENTRY *e = lookupSymbol(table, i);
        if (e->scope == 0)
        { // skip global symbols
            continue;
        }
        stackSize += getTypeSpecifierSize(e->type_specifier);
    }
    return stackSize;
}

/* return the offset of the function argument 
index starts from 1
*/
int getArgOffset(ENTRY **table, int index, int start)
{
    int offset = 0;
    for (int i = start; i <= index; i++)
    {
        ENTRY *e = lookupSymbol(table, i);
        if (e->scope != 2)
        {
            printf("error: invalid argument index");
            exit(1);
        }
        // later check if they are pointer variables
        offset += getTypeSpecifierSize(e->type_specifier);
    }

    return offset;
}

/* */
void getRegisterParameter(int paramIndex, char **srcOperand)
{
    switch (paramIndex)
    {
    case 0: // rdi
        *srcOperand = generalPurposeRegisters[5];
        break;
    case 1: // rsi
        *srcOperand = generalPurposeRegisters[4];
        break;
    case 2: // rdx
        *srcOperand = generalPurposeRegisters[2];
        break;
    case 3: // rcx
        *srcOperand = generalPurposeRegisters[1];
        break;
    case 4: // r8
        *srcOperand = addedPurposeRegisters[0];
        break;
    case 5: // r9
        *srcOperand = addedPurposeRegisters[1];
        break;
    }
}

/* return the offset of the local variable
index starts from 1
*/
int getLocalOffset(ENTRY **table, int index)
{
    int start = 0;

    // first get the first local index
    start = getNrOfParameters(table);

    int offset = 0;
    for (int i = start; i <= index; i++)
    {
        ENTRY *e = lookupSymbol(table, i);
        if (e->scope == 0)
        { // skip global symbols
            continue;
        }
        // later check if they are pointer variables
        offset += getTypeSpecifierSize(e->type_specifier);
    }

    return offset;
}

AST *makeAST(int op, int value, AST *left, AST *mid, AST *right)
{
    AST *e = (AST *)malloc(sizeof(AST));

    if (e == NULL)
    {
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

AST *makeChildlessAST(int op, int value)
{
    return makeAST(op, value, NULL, NULL, NULL);
}

AST *makeOneChildAST(int op, int value, AST *left)
{
    return makeAST(op, value, left, NULL, NULL);
}

AST *makeProgramAST(AST *left)
{
    return makeAST(op_PROGRAM, 0, left, NULL, NULL);
};

AST *makeArithmeticExpressionAST(int op, AST *left, AST *right)
{
    return makeAST(op, 0, left, NULL, right);
};

/*
for variables: left -> expression
for functions: left -> arguments
                mid -> statements
*/
AST *makeDeclareAST(int op, int symbolIndex, AST *left, AST *mid, AST *right)
{
    return makeAST(op, symbolIndex, left, mid, right);
};

AST *makeAssignmentAST(int op, int symbolIndex, AST *left)
{
    return makeAST(op, symbolIndex, left, NULL, NULL);
};

/* 
value: either a constant or a symbol index; in case of name collision local symbol has precedence over global symbol
left -> expressionAst
*/
AST *makeReturnAST(AST *left)
{
    return makeAST(op_RETURN, 0, left, NULL, NULL);
};

/* 
symbolIndex: function index in the global table
mid -> arguments
*/
AST *makeFunctionCallAST(int symbolIndex, AST *mid)
{
    return makeAST(op_FCALL, symbolIndex, NULL, mid, NULL);
};

/* 
owner: the index in the symbol table of the owner of this expression, 0-program , X-function
*/
AST* makePrimaryExpressionAST(int owner)
{
    AST* ast = NULL;

    if (currToken.token == TOK_INT)
    {
        ast = makeOneChildAST(op_INT, currToken.value, NULL);
    }
    else if (currToken.token == TOK_IDENTIFIER)
    {
        int identIndex = identifierWasDeclared(owner);

        // EITHER function call or variable, check the next symbol
        int currChar = fgetc(fptr);

        if(currChar == '(') { // it's a function call
            ast = functionCall(owner);
        }
        else {
            // put back the arithmetic op
            ungetc(currChar, fptr);
            ast = makeOneChildAST(op_IDENTIFIER, identIndex, NULL);
        }
    }
    else
    {
        printf("error: invalid token: expected %s %s", "int", "<currStringifier-name>");
        exit(1);
    }

    return ast;
};

/**
 * ====================LEXER====================
 * LL(1) Linear scan in a stream of characters, 
 * currStringifies the lexemes in the stream, and categorizes them into tokens
 */

/**
 * returns the next lexeme from the stream
 */
void getNextToken()
{
    int currChar = fgetc(fptr);

    while (isSpace(currChar) || currChar == '\n' || currChar == '\t') // skip white space and newline
        currChar = fgetc(fptr);

    if (currChar == '_' || isLetter(currChar))
    {
        // read the whole TOK_IDENTIFIER until space
        currString = (char *)malloc(MAX_STRING_SIZE + 1);
        int i = 0;
        do
        {
            currString[i++] = currChar;
            currChar = fgetc(fptr);
        } while (isAlphanumeric(currChar));

        ungetc(currChar, fptr); // unread the non-alpha back into the stream

        currString[i] = '\0';

        // check if it's a reserved keyword first
        if (!strcmp(currString, "int"))
        {
            currToken.token = TOK_TYPE_SPECIFIER;
        }
        else if (!strcmp(currString, "return"))
        {
            currToken.token = TOK_RETURN;
        }
        else
        { //it's an identifier
            currToken.token = TOK_IDENTIFIER;
        }

        return;
    }

    if (isDigit(currChar))
    {
        // read the whole number
        char intStr[MAX_NUMBER_SIZE];

        int i = 0;
        do
        {
            intStr[i++] = currChar;
            currChar = fgetc(fptr);
        } while (isDigit(currChar));

        intStr[i] = '\0';

        ungetc(currChar, fptr); // unread the non-decimal back into the stream

        currToken.token = TOK_INT;
        currToken.value = atoi(intStr);
    }
    else
    {
        switch (currChar)
        {
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
        case '{':
            currToken.token = TOK_CURLY_BRACKET_OPEN;
            break;
        case '}':
            currToken.token = TOK_CURLY_BRACKET_CLOSE;
            break;
        case ',':
            currToken.token = TOK_COMMA;
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

int getArithmeticOp(int t)
{
    int arithmeticOp;
    if (t == TOK_MOD)
    {
        arithmeticOp = op_MOD;
    }
    else if (t == TOK_DIV)
    {
        arithmeticOp = op_DIV;
    }
    else if (t == TOK_MULT)
    {
        arithmeticOp = op_MULT;
    }
    else if (t == TOK_PLUS)
    {
        arithmeticOp = op_PLUS;
    }
    else if (t == TOK_MINUS)
    {
        arithmeticOp = op_MINUS;
    }
    else
    {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }

    return arithmeticOp;
}

AST *expressionStatement(int previousTokenPrecedence, int owner)
{
    AST *left, *right;

    int localToken;

    // first check if its TOK_ROUND_BRACKET_OPEN
    if (!accept(TOK_ROUND_BRACKET_OPEN))
    { // skip
        left = makePrimaryExpressionAST(owner);

        getNextToken();

        if (accept(TOK_ROUND_BRACKET_CLOSE))
        {
            return left;
        }

        localToken = currToken.token;

        if (localToken == TOK_SMCL)
        {
            return left;
        }
    }
    else
    { //
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
    while (precedenceTable[localToken] < previousTokenPrecedence)
    {
        getNextToken(); // either ( or int/identifier

        if (accept(TOK_ROUND_BRACKET_OPEN))
        {
            int roundBracketPrecendene = precedenceTable[TOK_ROUND_BRACKET_OPEN];
            getNextToken(); // get the int/currStringifier or could be one or many (
            right = expressionStatement(roundBracketPrecendene, owner);
            if (currToken.token != TOK_ROUND_BRACKET_CLOSE)
            { // we're expeting a closing ) token
                error(TOK_ROUND_BRACKET_CLOSE);
            }
            getNextToken();
            /*
            fresh out of ), check if current token has higher precedence ex 1 + (2 * 3) / 2 * 5 - 9 , / has higher precedence than +
            1 + (2 * 3) / (2 - 5) * 9
            */
            while (currToken.token != TOK_SMCL && currToken.token != TOK_ROUND_BRACKET_CLOSE && precedenceTable[currToken.token] < localToken)
            {
                //run again the loop and return the correct right, a tree where the currToken.token has lower or equal precedence to localToken
                int newLocalToken = currToken.token;
                getNextToken(); // int/identifier or could be followed by one or many (
                AST *newRight = expressionStatement(precedenceTable[localToken], owner);
                right = makeArithmeticExpressionAST(getArithmeticOp(newLocalToken), right, newRight);
            }
        }
        else
            right = expressionStatement(precedenceTable[localToken], owner);

        left = makeArithmeticExpressionAST(getArithmeticOp(localToken), left, right);

        if (accept(TOK_ROUND_BRACKET_CLOSE))
        {
            return left; // if ) return the tree
        }

        localToken = currToken.token;

        if (localToken == TOK_SMCL)
            return left;
    }

    return left;
}

/* 
        (op_DECLARE)
    for variable 
        left: expressionAST 
    for functions
        left: array of arguments op_IDENT that contain only the type specifier of each argument 
        mid: statements with the right linking to next statement
*/
AST *declareStatement(int owner)
{
    char *typeSpecifier = currString; // save the type specifier
    char *identifierName = NULL;
    int symbolIndex = -1;

    getNextToken(); // get the identifier name

    if (accept(TOK_IDENTIFIER))
    {
        identifierName = currString;
    }
    else
        error(TOK_IDENTIFIER);

    getNextToken(); // either ';' or '=' or '('

    if (accept(TOK_SMCL))
    {
        // add the symbol to the owner's table and return a declare AST back
        int symbolIndex = addSymbolToTheOwnersTable(owner, identifierName, typeSpecifier, 0, getSymbolScope(owner));
        return makeDeclareAST(op_DECLARE, symbolIndex, NULL, NULL, NULL);
    }
    else if (accept(TOK_ASSIGN))
    {
        symbolIndex = addSymbolToTheOwnersTable(owner, identifierName, typeSpecifier, 0, getSymbolScope(owner));
        getNextToken(); // expression follows
        if (accept(TOK_INT) || accept(TOK_IDENTIFIER))
        {
            AST *expr = expressionStatement(4, owner);
            return makeDeclareAST(op_DECLARE, symbolIndex, expr, NULL, NULL);
        }
        else
        {
            printf("error: invalid token: expected %s or %s", "number", "identifier");
            exit(1);
        }
    }
    else if (accept(TOK_ROUND_BRACKET_OPEN))
    { // function declaration
        AST *firstArgAst = NULL;
        AST *prevArgAst = NULL;

        AST *firstStatementAst = NULL;
        AST *prevStatementAst = NULL;

        if (owner != 0)
        {
            printf("error: functions can only be declared global!");
            exit(1);
        }

        // register the function symbol in the table
        symbolIndex = addSymbolToTheOwnersTable(owner, identifierName, typeSpecifier, 1, getSymbolScope(owner));

        getNextToken(); // get the first argument type specifier
        // create the chain of arguments
        while (currToken.token != TOK_ROUND_BRACKET_CLOSE)
        {
            if (currToken.token == TOK_COMMA)
            {
                getNextToken();
            }

            if (accept(TOK_TYPE_SPECIFIER))
            {
                typeSpecifier = currString; // save the type specifier of the argument
            }
            else
                error(TOK_TYPE_SPECIFIER);

            getNextToken(); // get the identifier name of the argument

            if (accept(TOK_IDENTIFIER))
            {
                identifierName = currString;
            }
            else
                error(TOK_IDENTIFIER);

            int argIndex = addSymbolToTheOwnersTable(symbolIndex, identifierName, typeSpecifier, 0, 2);
            AST *currArgAST = makeDeclareAST(op_DECLARE, argIndex, NULL, NULL, NULL);

            if (firstArgAst == NULL)
            {
                firstArgAst = currArgAST;
                prevArgAst = currArgAST;
            }
            else
            {
                prevArgAst->left = currArgAST;
                prevArgAst = currArgAST;
            }

            getNextToken();
        }

        getNextToken(); // '{' follows

        if (!accept(TOK_CURLY_BRACKET_OPEN))
            error(TOK_CURLY_BRACKET_OPEN);

        getNextToken();
        // create the chain of statements contained in the function untill '}'
        while (currToken.token != TOK_CURLY_BRACKET_CLOSE)
        {
            AST *currStmtAst = statement(symbolIndex); // owner is the function
            if (firstStatementAst == NULL)
            {
                firstStatementAst = currStmtAst;
                prevStatementAst = currStmtAst;
            }
            else
            {
                prevStatementAst->right = currStmtAst;
                prevStatementAst = currStmtAst;
            }
            getNextToken();
        }

        return makeDeclareAST(op_DECLARE, symbolIndex, firstArgAst, firstStatementAst, NULL);
    }
    else
    {
        printf("error: invalid token: expected %c or %c", ';', '=');
        exit(1);
    }
}

AST *assignmentStatement(int owner)
{
    // get back the identifier index
    int identIndex = -1;
    if (owner == 0)
    {
        identIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString);
    }
    else
    {
        // first get the owner's ENTRY and then get it's symbol table
        ENTRY *ownersEntry = lookupSymbol(SymbolTable, owner);
        ENTRY **ownersTable = ownersEntry->localSymbolTable;
        int size = ownersEntry->localSymbolTableIndex;
        identIndex = getSymbolIndex(ownersTable, size, currString);
    }
    getNextToken();
    AST *expr = expressionStatement(4, owner);
    return makeAssignmentAST(OP_ASSIGN, identIndex, expr);
}

AST *selectionStatement()
{
    return NULL;
}

AST *returnStatement(int owner)
{
    if (owner == 0)
    { // program return is in main
        printf("error: expected a declaration but got return!");
        exit(1);
    }

    getNextToken(); // either a number or a identifier; null/undefined will be implemented later

    ENTRY *functionEntry = lookupSymbol(SymbolTable, owner);

    AST *expr = expressionStatement(4, owner);

    // the return type of the function must match the functions type specifier; currently all functions return int
    if (expr->op == op_INT || expr->op == op_MOD || expr->op == op_DIV || expr->op == op_MULT || expr->op == op_PLUS || expr->op == op_MINUS)
    { // return a number constant
        if (functionEntry->type_specifier != 0)
        {
            printf("error: incompatible return type specifier!"); // later we will check the return for overflow, imcompatible casts, pointer from literal etc
            exit(1);
        }
    }
    else if (expr->op == op_IDENTIFIER)
    { // return a variable
        if (functionEntry->type_specifier != 0)
        {
            printf("error: incompatible return type specifier!"); // later we will check the return for overflow, imcompatible casts, pointer from literal etc
            exit(1);
        }
    }

    return makeReturnAST(expr);
}

AST *functionCall(int owner)
{
    // get back the identifier index
    int identIndex = -1;
    identIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString);

    // to be replaced with separate listMethod
    AST *firstArgAst = NULL;
    AST *prevArgAst = NULL;

    getNextToken(); // get the first argument 
        // create the chain of arguments
    while (currToken.token != TOK_ROUND_BRACKET_CLOSE)
    {
        if (currToken.token == TOK_COMMA)
        {
            getNextToken();
        }

        AST *currArgAST = makePrimaryExpressionAST(owner);

        if (firstArgAst == NULL)
        {
            firstArgAst = currArgAST;
            prevArgAst = currArgAST;
        }
        else
        {
            prevArgAst->left = currArgAST;
            prevArgAst = currArgAST;
        }

        getNextToken();
    }

    return makeFunctionCallAST(identIndex, firstArgAst);
}

AST *assignmentOrFunctionCall(int owner)
{
    // basic syntax analysis
    identifierWasDeclared(owner);

    getNextToken(); // either '=' or '('

    if (accept(TOK_ASSIGN))
    {
        return assignmentStatement(owner);
    }
    else if (accept(TOK_ROUND_BRACKET_OPEN))
    {
        return functionCall(owner);
    }
    else
    {
        printf("error: invalid statement, expected assignment or function call!");
        exit(1);
    }
}

AST *statement(int owner)
{
    if (currToken.token == TOK_IF)
    {
        // not yet implemeneted
        // basic syntax analysis: if the owner is 0 aka program throw error
        //ast = selectionStatement();
        return NULL;
    }
    else if (currToken.token == TOK_TYPE_SPECIFIER)
    {
        // variable or function declaration
        return declareStatement(owner);
    }
    else if (currToken.token == TOK_IDENTIFIER)
    {
        // either an assignment or function call
        return assignmentOrFunctionCall(owner);
    }
    else if (currToken.token == TOK_RETURN)
    { // return statement
        return returnStatement(owner);
    }
    else
    {
        printf("error: invalid token; expected a statement");
        exit(1);
    }
}

AST *program()
{
    AST *firstStatement = NULL;
    AST *prevStatement = NULL;
    while (currToken.token != TOK_EOF)
    {
        AST *stmt = declareStatement(0); // program owns a series declarations
        if (firstStatement == NULL)
        {
            firstStatement = stmt;
            prevStatement = stmt;
        }
        else
        {
            prevStatement->right = stmt;
            prevStatement = stmt;
        }
        getNextToken();
    }

    return makeProgramAST(firstStatement);
}

AST *parser()
{
    getNextToken();
    AST *ret = program();
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

/* 
return: the first available purpose register
*/
int getAvailableRegister()
{
    int i = 0;
    for (; i < 8; i++)
    {
        if (availableAddedPurposeRegisters[i] == 0)
        {
            availableAddedPurposeRegisters[i] = 1;
            return i;
        }
    }
    return -1; // no available register, use stack
}

/*
Move

Syntax
mov <reg>,<reg>
mov <reg>,<mem>
mov <mem>,<reg>
mov <reg>,<const>
mov <mem>,<const>
*/

void asm_mov_write(char *destName, char *fromName)
{
    fprintf(ofptr, "\n\t%s %s, %s", dataMovementInstructions[0], destName, fromName);
}

/*
Push stack

Syntax
push <reg32>
push <mem>
push <con32>
*/

void asm_push_write(char *name)
{
    fprintf(ofptr, "\n\t%s %s", dataMovementInstructions[1], name);
}

/*
Pop stack

Syntax
pop <reg32>
pop <mem>
*/
void asm_pop_write(char *name)
{
    fprintf(ofptr, "\n\t%s %s", dataMovementInstructions[2], name);
}

/* 
Load effective address

Syntax
lea <reg32>,<mem>

*/
void asm_lea_write(char *destName, char *fromName)
{
    fprintf(ofptr, "\n\t%s %s, %s", dataMovementInstructions[3], destName, fromName);
}

/* Arithmetic and Logic Instructions */

/* 
Integer Addition 

Syntax
add/sub <reg>,<reg>
add/sub <reg>,<mem>
add/sub <mem>,<reg>
add/sub <reg>,<con>
add/sub <mem>,<con>
*/
void asm_add_write(char *destName, char *fromName)
{
    fprintf(ofptr, "\n\t%s %s, %s", arithmeticAndLogicInstructions[0], destName, fromName);
}

/* 
Integer Substraction

Syntax
add/sub <reg>,<reg>
add/sub <reg>,<mem>
add/sub <mem>,<reg>
add/sub <reg>,<con>
add/sub <mem>,<con>
*/
void asm_sub_write(char *destName, char *fromName)
{
    fprintf(ofptr, "\n\t%s %s, %s", arithmeticAndLogicInstructions[1], destName, fromName);
}

/* 
Integer Multiplication

Syntax
imul <reg32>,<reg32>
imul <reg32>,<mem>
imul <reg32>,<reg32>,<con>
imul <reg32>,<mem>,<con>
*/
void asm_imul_write(char *destReg, char *fromName, char *con)
{
    if (con)
        fprintf(ofptr, "\n\t%s %s, %s, %s", arithmeticAndLogicInstructions[4], destReg, fromName, con);
    else
        fprintf(ofptr, "\n\t%s %s, %s", arithmeticAndLogicInstructions[4], destReg, fromName);
}

/* 
Integer Division

Syntax
idiv <reg32>
idiv <mem>
*/
void asm_idiv_write(char *name)
{
    // asm_mov_write(generalPurposeRegisters[2], "0"); for positive rax
    // asm_mov_write(generalPurposeRegisters[2], "-1"); for negative rax
    // zeroing rdx with negative dividend leads to large positive Floating point exception so just sign extend rdx
    fprintf(ofptr, "\n\t%s", convertInstructions[2]);
    fprintf(ofptr, "\n\t%s %s", arithmeticAndLogicInstructions[5], name);
}

void asm_comm_Write(char *name, char *size, char *alignment)
{
    fprintf(ofptr, "\n\t%s %s,%s,%s", assemblerDirectives[4], name, size, alignment);
}

/* Helper functions for code generation */
void asm_functionPreamble(char *functionName)
{
    fprintf(ofptr, "\n\t.globl\t%s\r\n\t.type\t%s, @function\r\n%s:", functionName, functionName, functionName);
    fprintf(ofptr, "\n\tpush\trbp\r\n\tmov\trbp, rsp");
}

void asm_allocateStackSpace(int offset)
{
    // allocate space on the stack if needed
    char *spaceNeeded = NULL;

    intToStr(offset, &spaceNeeded);
    asm_sub_write(specialPurposeRegisters[0], spaceNeeded);
}

void asm_cleanStack(int bytes)
{
    // add rsp, bytes
    char *result;
    intToStr(bytes, &result);
    asm_mov_write(specialPurposeRegisters[0], result);
    fprintf(ofptr, "                               # clean the stack");
}

void asm_functionPostamble()
{
    fprintf(ofptr, "\r\n\tmov\trsp, rbp\r\n\tpop\trbp\r\n\tret");
}

/* Helper functions to parse different AST's */

/*
parses op_INT, op_IDENTIFIER(variables and functions)
return: the register that contains the value
*/
int parsePrimaryAst(AST *ast, int owner)
{
    int op = ast->op;
    int reg = -1;
    if (op == op_INT)
    {
        reg = getAvailableRegister();
        char *destOperand = addedPurposeRegisters[reg];
        char *srcOperand = NULL;
        intToStr(ast->value, &srcOperand);
        asm_mov_write(destOperand, srcOperand);
    }
    else if (op == op_IDENTIFIER)
    {
        // Program can't have arithmetic statements with identifiers as rvalues
        if (owner == 0)
        {
            printf("Program can't have arithmetic statements with identifiers as rvalues!");
            exit(1);
        }

        reg = getAvailableRegister();
        // get the owner's table
        ENTRY *e = getEntryFromOwner(owner, ast->value);
        char *destOperand = addedPurposeRegisters[reg];
        char *srcOperand = malloc(32 * sizeof(char));
        // check to see if the variable is either global,local or local argument
        if (e->scope == 0)
        { // global variable
            sprintf(srcOperand, "%s[%s]", e->name, specialPurposeRegisters[2]);
        }
        else if (e->scope == 1)
        { // local
            // determine the offset to sub from rbp
            char *offset = NULL;
            intToStr(getLocalOffset(lookupSymbol(SymbolTable, owner)->localSymbolTable, e->index), &offset);
            sprintf(srcOperand, "-%s[%s]", offset, specialPurposeRegisters[1]);
        }
        else
        { //argument
            // if more than 6 arguments check the stack for the rest
            if (e->index > 5)
            {
                // determine the offset to add from rbp
                char *offset = NULL;
                intToStr(getArgOffset(lookupSymbol(SymbolTable, owner)->localSymbolTable, e->index, 6), &offset);
                sprintf(srcOperand, "%s[%s]", offset, specialPurposeRegisters[1]);
            }
            else
            {
                // determine the register the parameter value was stored into
                getRegisterParameter(e->index, &srcOperand);
            }
        }
        asm_mov_write(destOperand, srcOperand);
    }
    else if(op_FCALL) {
        reg = getAvailableRegister();

        parseFunctionCallAst(ast, owner);
         // move the rax value into a register and return it
        asm_mov_write(addedPurposeRegisters[reg], generalPurposeRegisters[0]);
    }

    return reg;
}

int parseArithmeticTree(AST *ast, int owner)
{
    if (ast->left == NULL)
    {
       return parsePrimaryAst(ast,owner);
    }

    int leftReg = parseArithmeticTree(ast->left, owner);
    int rightReg = parseArithmeticTree(ast->right, owner);

    if (ast->op == op_MOD)
    { // not yet implemented
        return leftReg;
    }
    else if (ast->op == op_MULT)
    {
        asm_imul_write(addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg], NULL);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if (ast->op == op_DIV)
    {
        // 64 bit integer RDX:RAX, RDX has the most significant four bytes and RAX has the least significant four bytes
        asm_mov_write(generalPurposeRegisters[0], addedPurposeRegisters[leftReg]); // copy dividend argument into RAX
        asm_idiv_write(addedPurposeRegisters[rightReg]);                           // divide by right reg divisor argument
        asm_mov_write(addedPurposeRegisters[leftReg], generalPurposeRegisters[0]); // move the result from RAX to left reg
        availableGeneralPurposeRegisters[0] = 0;
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if (ast->op == op_PLUS)
    {
        asm_add_write(addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg]);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else if (ast->op == op_MINUS)
    {
        asm_sub_write(addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg]);
        availableAddedPurposeRegisters[rightReg] = 0;
        return leftReg;
    }
    else
    {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }
}

int optimized_parseArithmeticTree(AST *ast)
{
    if (ast->left == NULL)
    {
        // return either the value of the currStringifier OR the number
        switch (ast->op)
        {
        case op_INT:
            return ast->value;
        case op_IDENTIFIER:
            printf("identifiers aren't allowed in constant expressions!");
            exit(1);
        }
    }

    int leftVal = optimized_parseArithmeticTree(ast->left);
    int rightVal = optimized_parseArithmeticTree(ast->right);

    if (ast->op == op_MOD)
    {
        return leftVal % rightVal;
    }
    else if (ast->op == op_MULT)
    {
        return leftVal * rightVal;
    }
    else if (ast->op == op_DIV)
    {
        return leftVal / rightVal;
    }
    else if (ast->op == op_PLUS)
    {
        return leftVal + rightVal;
    }
    else if (ast->op == op_MINUS)
    {
        return leftVal - rightVal;
    }
    else
    {
        printf("error: invalid token: expected %c or %c or %c or %c or %c or %c", '%', '*', '/', '+', '-', ';');
        exit(1);
    }
}

void parseDeclarationAst(AST *ast, int owner)
{
    ENTRY *e = getEntryFromOwner(owner, ast->value);
    int scope = e->scope;
    int type = e->type;
    //int typeSpecifier = e->type_specifier; commented for the moment to remove unused compiler error

    if (scope == 0)
    { // global declaration
        if (type == 0)
        { // variable
            // check if its initialized or not
            if (ast->left)
            { // initialized add it on .data segment
                int rValue = optimized_parseArithmeticTree(ast->left);
                printf("global initialized symbol: constant rvalue %d ", rValue);
                char *strValue = NULL;
                intToStr(rValue, &strValue);

                fprintf(ofptr, "\t.globl\t%s", e->name);

                if (strcmp(lastAssemblerDirective, assemblerDirectives[5]) != 0)
                {
                    fprintf(ofptr, "\n\t%s", assemblerDirectives[5]);
                    lastAssemblerDirective = assemblerDirectives[5];
                }

                // determine offset and allign based on type specifier - not yet implemented, assume int
                fprintf(ofptr, "\t.align 8\r\n\t.type\t%s, @object\r\n\t.size\t%s, 8\r\n%s:\r\n\t.quad\t%s", e->name, e->name, e->name, strValue);
            }
            else
            { // unnitialized add it on .bss segment
                // first check the type specifier for the size - not yet implemented, assume int
                asm_comm_Write(e->name, "8", "8");
            }
        }
        else if (type == 1)
        { // function
            if (strcmp(lastAssemblerDirective, assemblerDirectives[19]) != 0)
            {
                fprintf(ofptr, "\n\t%s", assemblerDirectives[19]);
                lastAssemblerDirective = assemblerDirectives[19];
            }
            asm_functionPreamble(e->name);
            // get the total size in bytes of local variables
            int offset = getLocalStackSize(e->localSymbolTable, e->localSymbolTableIndex);
            if (offset > 0)
                asm_allocateStackSpace(offset);

            // get the number of arguments and restricts r8 & r9 if they contain arguments
            int nrOfArguments = getNrOfParameters(e->localSymbolTable);
            if(nrOfArguments == 5) {
                availableAddedPurposeRegisters[0] = 1;
            }
            else if(nrOfArguments >= 6) {
                availableAddedPurposeRegisters[0] = 1;
                availableAddedPurposeRegisters[1] = 1;
            }

            // parse each statement inside
            AST *currStatement = ast->mid;
            parseStatements(currStatement, e->index);

            if (offset > 0)
                asm_cleanStack(offset);
            asm_functionPostamble();

            // unrestrict r* r9 if they were used to pass arguments
            if(nrOfArguments == 5) {
                availableAddedPurposeRegisters[0] = 0;
            }
            else if(nrOfArguments >= 6) {
                availableAddedPurposeRegisters[0] = 0;
                availableAddedPurposeRegisters[1] = 0;
            }
        }
        else
        {
            printf("error: invalid global declaration type! expected variable or function");
            exit(1);
        }
    }
    else if (scope == 1)
    { // local declaration
        if (type == 0)
        { // variable
            if (ast->left)
            { // initialized
                int resultReg = parseArithmeticTree(ast->left, owner);
                printf("\nlocal: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
                char destOperand[32];
                char *srcOperand = addedPurposeRegisters[resultReg];
                ;
                char *offset = NULL;
                intToStr(getLocalOffset(lookupSymbol(SymbolTable, owner)->localSymbolTable, e->index), &offset);
                sprintf(destOperand, "-%s[%s]", offset, specialPurposeRegisters[1]);
                asm_mov_write(destOperand, srcOperand);
                availableAddedPurposeRegisters[resultReg] = 0;
            }
            else
            { // unitialized, nothing to be done
                return;
            }
        }
        else if (type == 1)
        { // function
            printf("error: functions cant be declared locally!");
            exit(1);
        }
        else
        {
            printf("error: invalid local declaration type! expected variable!");
            exit(1);
        }
    }
    else if (type == 2)
    { // local function argument
        // local function argument declarations are not parsed on their own
        return;
    }
    else
    {
        printf("error: invalid declaration scope");
        exit(1);
    }
}

void parseAssignAst(AST *ast, int owner)
{
    ENTRY *e = getEntryFromOwner(owner, ast->value);
    char* destOperand = malloc(32 * sizeof(char));
    char *srcOperand = NULL;

    // check whether the symbol is global or local
    if (e->scope == 0)
    { // global
        int res = optimized_parseArithmeticTree(ast->left);
        printf("\nassignment: constant rvalue %d ", res);

        intToStr(res, &srcOperand);
        sprintf(destOperand, "%s[%s]", e->name, specialPurposeRegisters[2]);
        asm_mov_write(destOperand, srcOperand);
    }
    else if (e->scope == 1)
    { // local
        int resultReg = parseArithmeticTree(ast->left, owner);
        printf("\nassignment: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
        srcOperand = addedPurposeRegisters[resultReg];

        char *offset = NULL;
        intToStr(getLocalOffset(lookupSymbol(SymbolTable, owner)->localSymbolTable, e->index), &offset);
        sprintf(destOperand, "-%s[%s]", offset, specialPurposeRegisters[1]);
        asm_mov_write(destOperand, srcOperand);
        availableAddedPurposeRegisters[resultReg] = 0;
    }
    else if (e->scope == 2)
    { // parameter

        int resultReg = parseArithmeticTree(ast->left, owner);
        printf("\nassignment: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
        srcOperand = addedPurposeRegisters[resultReg];

        // if more than 6 arguments check the stack for the rest
        if (e->index > 5)
        {
            // determine the offset to add from rbp
            char *offset = NULL;
            intToStr(getArgOffset(lookupSymbol(SymbolTable, owner)->localSymbolTable, e->index, 6), &offset);
            sprintf(destOperand, "%s[%s]", offset, specialPurposeRegisters[1]);
        }
        else
        {
            // determine the register the parameter value was stored into
            getRegisterParameter(e->index, &destOperand);
        }
        asm_mov_write(destOperand, srcOperand);
        availableAddedPurposeRegisters[resultReg] = 0;
    }
    else
    {
        printf("\nerror: invalid lvalue scope!");
        error(1);
    }
}

void parseReturnAst(AST *ast, int owner)
{
    char *destOperand = generalPurposeRegisters[0]; // rax
    char *srcOperand = NULL;

    int resultReg = parseArithmeticTree(ast->left, owner);
    printf("\nreturn: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
    srcOperand = addedPurposeRegisters[resultReg];
    asm_mov_write(destOperand, srcOperand);
    fprintf(ofptr, "                               # place the return into rax");
    availableGeneralPurposeRegisters[0] = 0; // make rax available again
    availableAddedPurposeRegisters[resultReg] = 0;
}

/* 
arg: current argument
argc: number of arguments, start with 0 and when we hit NULL left branch we got the final length 
*/
int passFunctionCallParameters(AST *ast, int argc, int owner)
{
    int totalSize = 0;

    if(ast->left)
        passFunctionCallParameters(ast->left,argc+1,owner);
    else 
        totalSize = argc;

    if(argc > 5) { // push it on the stack 
        if(ast->op == op_INT) {
            char* val = NULL;
            intToStr(ast->value, &val);
            asm_push_write(val);
        }
        else {
            int regValue = parsePrimaryAst(ast, owner); // later will register of expr
            asm_push_write(addedPurposeRegisters[regValue]);
            availableAddedPurposeRegisters[regValue] = 0;
        }
    }
    else { // find the proper register for the value
        char* registerToPush = NULL;
        getRegisterParameter(argc, &registerToPush);
        if(ast->op == op_INT) {
            char* val = NULL;
            intToStr(ast->value, &val);
            asm_mov_write(registerToPush, val);
            if(argc == 4 || argc == 5) // make sure to tick r8 and r9 as unavailable untill the function call
                availableAddedPurposeRegisters[argc] = 1;
        }
        else {
            int regValue = parsePrimaryAst(ast, owner); // later will register of expr
            asm_mov_write(registerToPush, addedPurposeRegisters[regValue]);
            availableAddedPurposeRegisters[regValue] = 0;
        }
    }

    return totalSize;
}

void parseFunctionCallAst(AST *ast, int owner)
{
    ENTRY *e = getEntryFromOwner(0, ast->value); // get the function details first, used later to check for incorrent arg's

    if (ast->mid)
    { // if function has arguments
        AST* firstArg = ast->mid;
        
        int argSize = passFunctionCallParameters(firstArg,0,  owner);

        fprintf(ofptr, "\n\t%s\t%s", controlFlowInstructions[9], e->name);
        fprintf(ofptr, "                               # called function \"%s\"", e->name);

        // after the asm call to function make the registers r8,9 available again if they were used
        if(argSize == 4) {
            // clear r8
            availableAddedPurposeRegisters[0] = 0;

        }
        else if(argSize >= 5) {
            // clear both r8 and r9
            availableAddedPurposeRegisters[0] = 0;
            availableAddedPurposeRegisters[1] = 0;
        }
    }
    else {
        fprintf(ofptr, "\n\t%s\t%s", controlFlowInstructions[9], e->name);
        fprintf(ofptr, "                               # called function \"%s\"", e->name);
    }
}

void parseStatements(AST *ast, int owner)
{
    while (ast != NULL)
    {
        // check ast op code
        switch (ast->op)
        {
        case op_DECLARE:
            parseDeclarationAst(ast, owner);
            break;
        case OP_ASSIGN:
            parseAssignAst(ast, owner);
            break;
        case op_RETURN:
            parseReturnAst(ast, owner);
            break;
        case op_FCALL:
            parseFunctionCallAst(ast, owner);
            break;
        default:
            printf("error: invalid statement AST! expected %s or %s", "op_DECLARE", "op_ASSIGN");
            exit(1);
        }
        ast = ast->right;
    }
}

void parseProgramAst(AST *ast)
{
    AST *currStatementAst = ast->left;
    // parse each statement
    parseStatements(currStatementAst, 0);
}

void writePreamble()
{
    destFileName = (char *)malloc(strlen(sourceFileName));
    strcpy(destFileName, sourceFileName);
    destFileName[strlen(sourceFileName) - 1] = 's';

    ofptr = fopen(destFileName, "wt");

    if (!ofptr)
    {
        printf("failed to create (%s) dest file", destFileName);
        exit(1);
    }
    // 'intel_syntax noprefix' forces the GNU Assembler to not require % for registers
    fprintf(ofptr, "\t.file\t\"%s\"\r\n\t.intel_syntax noprefix\n\t.text", sourceFileName);
    lastAssemblerDirective = assemblerDirectives[19];
}

/** 
 * 
 * ====================/CODE GENERATION====================
 */

void cleanUp()
{
    free(src);
    src = NULL;
    fclose(fptr);
}

int main(int argc, char **argv)
{

    if (argc < 2)
    {
        printf("error : no input file specified");
        return -1;
    }

    sourceFileName = argv[1];
    fptr = fopen(sourceFileName, "r");

    if (!fptr)
    {
        printf("failed to open (%s) source file", sourceFileName);
        return -1;
    }

    if (!(src = malloc(sizeof(char) * SRC_DATA)))
    {
        printf("could not allocate %d for source data\n", SRC_DATA);
        return -1;
    }

    writePreamble();
    parseProgramAst(parser());

    cleanUp();
}