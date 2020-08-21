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
    if(number == 0) {
        *result = malloc(2 *sizeof(char));
         sprintf(*result, "%d", 0);
    }
    else {
        *result = malloc(((int)floor(log10(abs(number))) + 1));
        sprintf(*result, "%d", number);
    }
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
int lineNumber = 1;
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
int jmpLabels = 1;
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
    printf("error on line %d: expected %d token ", lineNumber, token);
    exit(1);
}

bool accept(int t)
{
    return currToken.token == t;
}

enum
{                             // terminal symbols
    TOK_EOF = EOF,            // -1
    TOK_MOD,                  // %
    TOK_MULT,                 // *
    TOK_DIV,                  // /
    TOK_PLUS,                 // +
    TOK_MINUS,                // -
    TOK_ROUND_BRACKET_OPEN,   // '('
    TOK_ROUND_BRACKET_CLOSE,  // ')'
    TOK_EQUAL,                // '=='
    TOK_NEQUAL,               // '!='
    TOK_LE,                   // '<='
    TOK_GE,                   // '>='
    TOK_G,                    // '>'
    TOK_L,                    // '<'
    TOK_LNOT,                 // '!' not yet implemented
    TOK_TYPE_SPECIFIER,       // 'int', 'double'
    TOK_ASSIGN,               // =
    TOK_IDENTIFIER,           // variable 16
    TOK_INT,                  // int
    TOK_CURLY_BRACKET_OPEN,   // '{'
    TOK_CURLY_BRACKET_CLOSE,  // '{'
    TOK_SQUARE_BRACKET_CLOSE, // '['
    TOK_SQUARE_BRACKET_OPEN,  // ']'
    TOK_IF,                   // 'if' 22
    TOK_ELSE_IF,              // 'else if' 23
    TOK_ELSE,                 // 'else
    TOK_SMCL,                 // ';'
    TOK_COMMA,                // ,
    TOK_RETURN,               // 'return'
    TOK_WHILE                 // 'while'
};

int precedenceTable[] = {
    2, // MOD
    2, // MULT
    2, // DIV
    3, // PLUS
    3, // MINUS
    5, // ROUND BRACKET OPEN
    5, // ROUND BRACKET CLOSE
    4, // '=='
    4, // '!='
    4, // '<='
    4, // '>='
    4, // '>'
    4  // '<'
};
enum
{
    op_PROGRAM, // 0
    op_MOD,     // 1
    op_DIV,     // 2
    op_MULT,    // 3
    op_PLUS,    // 4
    op_MINUS,   // 5
    op_DECLARE, // 6
    OP_ASSIGN,  // 7
    op_IDENTIFIER,
    op_INT,
    op_RETURN,
    op_FCALL,
    op_EQUAL,
    op_NEQUAL,
    op_LE,
    op_GE,
    op_L,
    op_G,
    op_IF,     // 18
    op_ELSEIF, // 19
    op_ELSE,   // 20
    op_WHILE   // 21
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
        printf("error on line %d: undefined type specifier", lineNumber);
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
        printf("error on line %d: undefined type specifier", lineNumber);
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
            printf("error on line %d: duplicate global symbol %s ", lineNumber, identifierName);
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
                printf("error on line %d: duplicate local symbol %s in function %s ", lineNumber, identifierName, functionEntry->name);
                exit(1);
            }
        }

        int symbolIndex = addSymbolEntry(table, size, identifierName, typeSpecifier, type, scope, owner);
        return symbolIndex;
    }
}

/*
Basic syntax analysis 
Checks where the current string read was declared previously
return: -1 : not declared 0 - declared global 1 - declared local
Checks where the current string read was declared previously, local table has precedence over global
*/

int identifierWasDeclared(int owner)
{
    if (owner == 0)
    {
        int globalIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString);
        if (globalIndex == -1)
            return -1;
        else
            return 0;
    }
    else
    {
        ENTRY *functionEntry = lookupSymbol(SymbolTable, owner);
        ENTRY **table = functionEntry->localSymbolTable;
        int size = functionEntry->localSymbolTableIndex;
        int localIndex = getSymbolIndex(table, size, currString); // check the local functions table first
        if (localIndex == -1)
        {
            int globalIndex = getSymbolIndex(SymbolTable, symbolTableIndex, currString); // then check the global table
            if (globalIndex == -1)
                return -1;
            else
                return 0;
        }
        else
            return 1;
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
int getNrOfParameters(ENTRY **table, int tableSize)
{
    if (tableSize == 0)
        return 0;

    int nr = 0;
    ENTRY *tempE = lookupSymbol(table, nr);
    while (tempE->scope == 2)
    {
        nr++;
        if (nr >= tableSize)
            return nr;
        tempE = lookupSymbol(table, nr);
    }
    return nr;
}

int getLocalStackSize(ENTRY **table, int tableSize)
{
    if (tableSize == 0)
        return 0;

    int start = getNrOfParameters(table, tableSize);
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
    int offset = 8;
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
int getLocalOffset(ENTRY **table, int index, int tableSize)
{
    int start = 0;

    // first get the first local index
    start = getNrOfParameters(table, tableSize);

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

AST *makeAST(int op, int value, AST *left, AST *mid, AST *right, bool isGlobal)
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
    e->isGlobal = isGlobal;
    return e;
}

AST *makeChildlessAST(int op, int value)
{
    return makeAST(op, value, NULL, NULL, NULL, false);
}

AST *makeOneChildAST(int op, int value, AST *left)
{
    return makeAST(op, value, left, NULL, NULL, false);
}

AST *makeIdentifierAST(int value, AST *left, bool isGlobal)
{
    return makeAST(op_IDENTIFIER, value, left, NULL, NULL, isGlobal);
}

AST *makeProgramAST(AST *left)
{
    return makeAST(op_PROGRAM, 0, left, NULL, NULL, false);
};

AST *makeArithmeticExpressionAST(int op, AST *left, AST *right)
{
    return makeAST(op, 0, left, NULL, right, false);
};

/*
for variables: left -> expression
for functions: left -> arguments
                mid -> statements
*/
AST *makeDeclareAST(int symbolIndex, AST *left, AST *mid, AST *right)
{
    return makeAST(op_DECLARE, symbolIndex, left, mid, right, false);
};

AST *makeAssignmentAST(int op, int symbolIndex, AST *left)
{
    return makeAST(op, symbolIndex, left, NULL, NULL, false);
};

/* 
value: either a constant or a symbol index; in case of name collision local symbol has precedence over global symbol
left -> expressionAst
*/
AST *makeReturnAST(AST *left)
{
    return makeAST(op_RETURN, 0, left, NULL, NULL, false);
};

/* 
symbolIndex: function index in the global table
mid -> arguments
*/
AST *makeFunctionCallAST(int symbolIndex, AST *mid)
{
    return makeAST(op_FCALL, symbolIndex, NULL, mid, NULL, false);
};

/*
left -> conditional expression
mid -> statements inside of if()
*/
AST *makeConditionalAST(int op, AST *left, AST *mid)
{
    return makeAST(op, -1, left, mid, NULL, false);
};

/*
left -> conditional expression
mid -> statements inside of while()
*/
AST *makeWhileAST(AST *left, AST *mid)
{
    return makeAST(op_WHILE, -1, left, mid, NULL, false);
};

/* 
owner: the index in the symbol table of the owner of this expression, 0-program , X-function
*/
AST *makePrimaryExpressionAST(int owner)
{
    AST *ast = NULL;

    if (currToken.token == TOK_INT)
    {
        ast = makeOneChildAST(op_INT, currToken.value, NULL);
    }
    else if (currToken.token == TOK_IDENTIFIER)
    {
        // get details about the identifier

        int identIndex = -1;
        bool identifierIsGlobal = false;

        // get details about the identifier
        if (owner == 0)
        { // only constant values are allowed as rvalue in global variables
            printf("error on line %d: global expressions can't containt identifiers!", lineNumber);
            exit(1);
        }

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
                printf("error on line %d: symbol %s is not defined!", lineNumber, currString);
                exit(1);
            }
            else
            {
                // don't add global identifiers to the local symbol table
                identIndex = globalIndex;
                identifierIsGlobal = true;
            }
        }
        else
        {
            identIndex = localIndex;
        }

        // EITHER function call or variable, check the next symbol
        int currChar = fgetc(fptr);

        if (currChar == '(')
        { // it's a function call
            ast = functionCall(owner);
        }
        else
        {
            // put back the arithmetic op
            ungetc(currChar, fptr);
            ast = makeIdentifierAST(identIndex, NULL, identifierIsGlobal);
        }
    }
    else
    {
        printf("error on line %d: invalid token: expected type specifier or identifier!", lineNumber);
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

    while (isSpace(currChar) || currChar == '\n' || currChar == '\t')
    { // skip white space and newline
        if (currChar == '\n')
            ++lineNumber;
        currChar = fgetc(fptr);
    }

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
        else if (!strcmp(currString, "if"))
        {
            currToken.token = TOK_IF;
        }
        else if (!strcmp(currString, "else"))
        {
            currToken.token = TOK_ELSE;
        }
        else if (!strcmp(currString, "while"))
        {
            currToken.token = TOK_WHILE;
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
        if (currChar == '+')
        {
            currToken.token = TOK_PLUS;
        }
        else if (currChar == '-')
        {
            currToken.token = TOK_MINUS;
        }
        else if (currChar == '*')
        {
            currToken.token = TOK_MULT;
        }
        else if (currChar == '/')
        {
            currToken.token = TOK_DIV;
        }
        else if (currChar == '%')
        {
            currToken.token = TOK_MOD;
        }
        else if (currChar == TOK_EOF)
        {
            currToken.token = TOK_EOF;
        }
        else if (currChar == '=')
        {
            // either = or ==
            int currChar = fgetc(fptr);

            if (currChar == '=')
            {
                currToken.token = TOK_EQUAL;
            }
            else
            {
                ungetc(currChar, fptr);
                currToken.token = TOK_ASSIGN;
            }
        }
        else if (currChar == '>')
        {
            // either > or >=
            int currChar = fgetc(fptr);

            if (currChar == '=')
            {
                currToken.token = TOK_GE;
            }
            else
            {
                ungetc(currChar, fptr);
                currToken.token = TOK_G;
            }
        }
        else if (currChar == '<')
        {
            // either < or <=
            int currChar = fgetc(fptr);

            if (currChar == '=')
            {
                currToken.token = TOK_LE;
            }
            else
            {
                ungetc(currChar, fptr);
                currToken.token = TOK_L;
            }
        }
        else if (currChar == '!')
        {
            // either < or <=
            int currChar = fgetc(fptr);

            if (currChar == '=')
            {
                currToken.token = TOK_NEQUAL;
            }
            else
            {
                ungetc(currChar, fptr);
                currToken.token = TOK_LNOT;
            }
        }
        else if (currChar == ';')
        {
            currToken.token = TOK_SMCL;
        }
        else if (currChar == '(')
        {
            currToken.token = TOK_ROUND_BRACKET_OPEN;
        }
        else if (currChar == ')')
        {
            currToken.token = TOK_ROUND_BRACKET_CLOSE;
        }
        else if (currChar == '{')
        {
            currToken.token = TOK_CURLY_BRACKET_OPEN;
        }
        else if (currChar == '}')
        {
            currToken.token = TOK_CURLY_BRACKET_CLOSE;
        }
        else if (currChar == ',')
        {
            currToken.token = TOK_COMMA;
        }
        else
        {
            printf("error on line %d:  unrecognised character", lineNumber);
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

int getExpressionOp(int t)
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
    else if (t == TOK_EQUAL)
    {
        arithmeticOp = op_EQUAL;
    }
    else if (t == TOK_LE)
    {
        arithmeticOp = op_LE;
    }
    else if (t == TOK_GE)
    {
        arithmeticOp = op_GE;
    }
    else if (t == TOK_L)
    {
        arithmeticOp = op_L;
    }
    else if (t == TOK_G)
    {
        arithmeticOp = op_G;
    }
    else if (t == TOK_NEQUAL)
    {
        arithmeticOp = op_NEQUAL;
    }
    else
    {
        printf("error on line %d: invalid token: expected %c or %c or %c or %c or %c or %c or %s", lineNumber, '%', '*', '/', '+', '-', ';', "boolean comparator");
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
                right = makeArithmeticExpressionAST(getExpressionOp(newLocalToken), right, newRight);
            }
        }
        else
            right = expressionStatement(precedenceTable[localToken], owner);

        left = makeArithmeticExpressionAST(getExpressionOp(localToken), left, right);

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

    AST *ast = NULL;

    getNextToken(); // get the identifier name

    if (accept(TOK_IDENTIFIER))
    {
        identifierName = currString;
    }
    else
    {
        printf("error on line %d : declaration has no identifier!", lineNumber);
        exit(1);
    }

    getNextToken(); // either ';' or '=' or '('

    if (accept(TOK_SMCL))
    {
        // add the symbol to the owner's table and return a declare AST back
        int symbolIndex = addSymbolToTheOwnersTable(owner, identifierName, typeSpecifier, 0, getSymbolScope(owner));
        ast = makeDeclareAST(symbolIndex, NULL, NULL, NULL);
    }
    else if (accept(TOK_ASSIGN))
    {
        symbolIndex = addSymbolToTheOwnersTable(owner, identifierName, typeSpecifier, 0, getSymbolScope(owner));
        getNextToken(); // expression follows
        if (accept(TOK_INT) || accept(TOK_IDENTIFIER))
        {
            AST *expr = expressionStatement(5, owner);
            ast = makeDeclareAST(symbolIndex, expr, NULL, NULL);
        }
        else
        {
            printf("error on line %d: invalid token: expected %s or %s", lineNumber, "number", "identifier");
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
            printf("error on line %d: functions can only be declared global!", lineNumber);
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
            AST *currArgAST = makeDeclareAST(argIndex, NULL, NULL, NULL);

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
                if (currStmtAst->op == op_ELSE || currStmtAst->op == op_ELSEIF)
                {
                    printf("error on line %d: expected if statement previously!", lineNumber);
                    exit(1);
                }
                firstStatementAst = currStmtAst;
                prevStatementAst = currStmtAst;
            }
            else
            {
                if (currStmtAst->op == op_ELSEIF && prevStatementAst->op != op_IF)
                {
                    if (prevStatementAst->op != op_ELSEIF)
                    {

                        printf("error: expected 'if' / 'else if' statement before 'else if'!");
                        exit(1);
                    }
                }
                if (currStmtAst->op == op_ELSE && prevStatementAst->op != op_ELSEIF)
                {
                    if (prevStatementAst->op != op_IF)
                    {

                        printf("error: expected 'if' / 'else if' statement before 'else'!");
                        exit(1);
                    }
                }
                prevStatementAst->right = currStmtAst;
                prevStatementAst = currStmtAst;
            }
        }

        ast = makeDeclareAST(symbolIndex, firstArgAst, firstStatementAst, NULL);
    }
    else
    {
        printf("error on line %d: invalid token: expected %c or %c", lineNumber, ';', '=');
        exit(1);
    }

    getNextToken();
    return ast;
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
    AST *expr = expressionStatement(5, owner);
    return makeAssignmentAST(OP_ASSIGN, identIndex, expr);
}

AST *conditionalStatement(int owner)
{
    if (owner == 0)
    { // program return is in main
        printf("error on line %d: conditional statements can't be global!", lineNumber);
        exit(1);
    }

    int op = -1;
    if (accept(TOK_IF))
    {
        op = op_IF;
    }
    else if (accept(TOK_ELSE))
    {
        getNextToken();
        if (accept(TOK_CURLY_BRACKET_OPEN))
        {
            op = op_ELSE;
        }
        else if (accept(TOK_IF))
        {
            op = op_ELSEIF;
        }
    }

    AST *expr = NULL;
    AST *firstStatementAst = NULL;
    AST *prevStatementAst = NULL;

    if (op != op_ELSE) // Skip expr in 'else'
    {
        getNextToken(); // expect '('

        if (!accept(TOK_ROUND_BRACKET_OPEN))
        {
            printf("error on line %d: expected '(' !", lineNumber);
            exit(1);
        }

        // read the expression ast
        getNextToken();
        expr = expressionStatement(5, owner);

        getNextToken(); // read the ')'
    }

    if (!accept(TOK_CURLY_BRACKET_OPEN))
    {
        printf("error on line %d: expected '{' !", lineNumber);
        exit(1);
    }

    // read the statements inside the block

    getNextToken();

    while (currToken.token != TOK_CURLY_BRACKET_CLOSE)
    {
        AST *currStmtAst = statement(owner);
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
    }

    getNextToken(); // start next statement

    // we don't have to get the next token as we already read it above if needed
    return makeConditionalAST(op, expr, firstStatementAst);
}

AST *whileStatement(int owner)
{
    if (owner == 0)
    { // program return is in main
        printf("error on line %d: while statements can't be global!", lineNumber);
        exit(1);
    }

    AST *expr = NULL;
    AST *firstStatementAst = NULL;
    AST *prevStatementAst = NULL;

    getNextToken(); // expect '('

    if (!accept(TOK_ROUND_BRACKET_OPEN))
    {
        printf("error on line %d: expected '(' !", lineNumber);
        exit(1);
    }

    // read the expression ast
    getNextToken();
    expr = expressionStatement(5, owner);

    getNextToken(); // read the ')'

    if (!accept(TOK_CURLY_BRACKET_OPEN))
    {
        printf("error on line %d: expected '{' !", lineNumber);
        exit(1);
    }

    // read the statements inside the block

    getNextToken();

    while (currToken.token != TOK_CURLY_BRACKET_CLOSE)
    {
        AST *currStmtAst = statement(owner);
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
    }

    getNextToken(); // start next statement

    return makeWhileAST(expr, firstStatementAst);
}

AST *returnStatement(int owner)
{
    if (owner == 0)
    { // program return is in main
        printf("error on line %d: expected a declaration but got return!", lineNumber);
        exit(1);
    }

    getNextToken(); // either a number or a identifier; null/undefined will be implemented later

    ENTRY *functionEntry = lookupSymbol(SymbolTable, owner);

    AST *expr = expressionStatement(5, owner);

    // the return type of the function must match the functions type specifier; currently all functions return int
    if (expr->op == op_INT || expr->op == op_MOD || expr->op == op_DIV || expr->op == op_MULT || expr->op == op_PLUS || expr->op == op_MINUS)
    { // return a number constant
        if (functionEntry->type_specifier != 0)
        {
            printf("error on line %d: incompatible return type specifier!", lineNumber); // later we will check the return for overflow, imcompatible casts, pointer from literal etc
            exit(1);
        }
    }
    else if (expr->op == op_IDENTIFIER)
    { // return a variable
        if (functionEntry->type_specifier != 0)
        {
            printf("error on line %d: incompatible return type specifier!", lineNumber); // later we will check the return for overflow, imcompatible casts, pointer from literal etc
            exit(1);
        }
    }

    getNextToken(); // read the next token for the next statement

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

    ENTRY *functionEntry = lookupSymbol(SymbolTable, identIndex); // look the details of the functions up in case we need to verify

    int argNr = getNrOfParameters(functionEntry->localSymbolTable, functionEntry->localSymbolTableIndex);

    // create the chain of arguments
    while (currToken.token != TOK_ROUND_BRACKET_CLOSE)
    {
        if (currToken.token == TOK_COMMA)
        {
            getNextToken();
        }

        AST *currArgAST = makePrimaryExpressionAST(owner);
        argNr--;

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

    if (argNr < 0)
    {
        printf("error on line %d: too many arguments in %s function call\n", lineNumber, functionEntry->name);
        exit(1);
    }
    else if (argNr > 0)
    {
        printf("error on line %d: too few arguments in %s function call\n", lineNumber, functionEntry->name);
        exit(1);
    }

    return makeFunctionCallAST(identIndex, firstArgAst);
}

AST *assignmentOrFunctionCall(int owner)
{
    // basic syntax analysis
    if (identifierWasDeclared(owner) == -1)
    {
        printf("error on line %d: symbol %s is not defined!", lineNumber, currString);
        exit(1);
    }

    AST *ast = NULL;

    getNextToken(); // either '=' or '('

    if (accept(TOK_ASSIGN))
    {
        ast = assignmentStatement(owner);
    }
    else if (accept(TOK_ROUND_BRACKET_OPEN))
    {
        ast = functionCall(owner);
    }
    else
    {
        printf("error on line %d: invalid statement, expected assignment or function call!", lineNumber);
        exit(1);
    }

    getNextToken();

    return ast;
}

AST *statement(int owner)
{
    if (currToken.token == TOK_IF || currToken.token == TOK_ELSE || currToken.token == TOK_ELSE_IF)
    {
        return conditionalStatement(owner);
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
    else if (currToken.token == TOK_WHILE)
    {
        return whileStatement(owner);
    }
    else
    {
        printf("error on line %d: invalid token; expected a statement", lineNumber);
        exit(1);
    }
}

AST *program()
{
    // ad the program owner to symbol table 
    addSymbolEntry(SymbolTable, &symbolTableIndex, "program", "program-specifier", -1, -1, -1);

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

int getNewLabel()
{
    jmpLabels++;
    return jmpLabels;
}

char *makeLabel(int label)
{
    char *ret = malloc(32 * sizeof(char));
    sprintf(ret, ".L%d", label);
    return ret;
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

void asm_cmp_write(char *destName, char *fromName)
{
    fprintf(ofptr, "\n\t%s %s, %s", controlFlowInstructions[8], destName, fromName);
}

void asm_jmp_write(char *instruction, char *label)
{
    fprintf(ofptr, "\n\t%s %s", instruction, label);
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

/*
void asm_cleanStack(int bytes)
{
    // add rsp, bytes
    char *result;
    intToStr(bytes, &result);
    asm_add_write(specialPurposeRegisters[0], result);
    fprintf(ofptr, "                               # clean the stack");
}
*/

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
        ENTRY *e = NULL;
        char *destOperand = addedPurposeRegisters[reg];
        char *srcOperand = malloc(32 * sizeof(char));
        // check to see if the variable is either global,local or local argument
        if (ast->isGlobal == true)
        { // global variable
            e = getEntryFromOwner(0, ast->value);
            sprintf(srcOperand, "%s[%s]", e->name, specialPurposeRegisters[2]);
        }
        else if (ast->isGlobal == false)
        { // local
            e = getEntryFromOwner(owner, ast->value);
            if (e->scope == 1)
            { // local
                // determine the offset to sub from rbp
                char *offset = NULL;
                ENTRY *ownerEntry = lookupSymbol(SymbolTable, owner);
                intToStr(getLocalOffset(ownerEntry->localSymbolTable, e->index, ownerEntry->localSymbolTableIndex), &offset);
                sprintf(srcOperand, "-%s[%s]", offset, specialPurposeRegisters[1]);
            }
            else if (e->scope == 2)
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
        }
        asm_mov_write(destOperand, srcOperand);
    }
    else if (op_FCALL)
    {
        reg = getAvailableRegister();

        parseFunctionCallAst(ast, owner);
        // move the rax value into a register and return it
        asm_mov_write(addedPurposeRegisters[reg], generalPurposeRegisters[0]);
    }

    return reg;
}

/*
cond: 0 for if/else-if/else 1 for while/for -1 any other expr that doesnt contain booleans
cLabel: custom label we will jump to , used for for/while loops, -1 for any other non-loop expr
*/
int parseArithmeticTree(AST *ast, int owner, int cond, int cLabel)
{
    if (ast->left == NULL)
    {
        return parsePrimaryAst(ast, owner);
    }

    int leftReg = parseArithmeticTree(ast->left, owner, cond, -1);
    int rightReg = parseArithmeticTree(ast->right, owner, cond, -1);

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
    else if (ast->op == op_LE || ast->op == op_L || ast->op == op_GE || ast->op == op_G || ast->op == op_EQUAL || ast->op == op_NEQUAL)
    {
        asm_cmp_write(addedPurposeRegisters[leftReg], addedPurposeRegisters[rightReg]);
        int instruction = -1;

        if (cond == 0) { // if/else-if/else
            int label = getNewLabel(); // the label where the else/else if occur
            switch (ast->op)
            {
            case op_LE:
                instruction = 4;
                break;
            case op_L:
                instruction = 5;
                break;
            case op_GE:
                instruction = 6;
                break;
            case op_G:
                instruction = 7;
                break;
            case op_EQUAL:
                instruction = 2;
                break;
            case op_NEQUAL:
                instruction = 1;
                break;
            }

            asm_jmp_write(controlFlowInstructions[instruction], makeLabel(label));
            availableAddedPurposeRegisters[leftReg] = 0;
            availableAddedPurposeRegisters[rightReg] = 0;
            return label;
        }
        else if (cond == 1){ // for/while
            switch (ast->op)
            {
            case op_LE:
                instruction = 7;
                break;
            case op_L:
                instruction = 6;
                break;
            case op_GE:
                instruction = 5;
                break;
            case op_G:
                instruction = 4;
                break;
            case op_EQUAL:
                instruction = 1;
                break;
            case op_NEQUAL:
                instruction = 2;
                break;
            }

            asm_jmp_write(controlFlowInstructions[instruction], makeLabel(cLabel));
            availableAddedPurposeRegisters[leftReg] = 0;
            availableAddedPurposeRegisters[rightReg] = 0;
            return cLabel;
        }
        else {
            printf("error: expr can't contain boolean operators!");
            exit(1);
        }
    }

    else
    {
        printf("erroron line %d, invalid token: expected %c or %c or %c or %c or %c or %c", lineNumber, '%', '*', '/', '+', '-', ';');
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

                fprintf(ofptr, "\n\t.globl\t%s", e->name);

                if (strcmp(lastAssemblerDirective, assemblerDirectives[5]) != 0)
                {
                    fprintf(ofptr, "\n\t%s", assemblerDirectives[5]);
                    lastAssemblerDirective = assemblerDirectives[5];
                }

                // determine offset and allign based on type specifier - not yet implemented, assume int
                fprintf(ofptr, "\n\t.align 8\r\n\t.type\t%s, @object\r\n\t.size\t%s, 8\r\n%s:\r\n\t.quad\t%s", e->name, e->name, e->name, strValue);
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
            int nrOfArguments = getNrOfParameters(e->localSymbolTable, e->localSymbolTableIndex);
            if (nrOfArguments == 5)
            {
                availableAddedPurposeRegisters[0] = 1;
            }
            else if (nrOfArguments >= 6)
            {
                availableAddedPurposeRegisters[0] = 1;
                availableAddedPurposeRegisters[1] = 1;
            }

            // parse each statement inside
            AST *currStatement = ast->mid;
            parseStatements(currStatement, e->index);

            // unrestrict r* r9 if they were used to pass arguments
            if (nrOfArguments == 5)
            {
                availableAddedPurposeRegisters[0] = 0;
            }
            else if (nrOfArguments >= 6)
            {
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
                int resultReg = parseArithmeticTree(ast->left, owner, -1, -1);
                printf("\nlocal: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
                char destOperand[32];
                char *srcOperand = addedPurposeRegisters[resultReg];
                ;
                char *offset = NULL;
                ENTRY *ownerEntry = lookupSymbol(SymbolTable, owner);
                intToStr(getLocalOffset(ownerEntry->localSymbolTable, e->index, ownerEntry->localSymbolTableIndex), &offset);
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
    char *destOperand = malloc(32 * sizeof(char));
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
        int resultReg = parseArithmeticTree(ast->left, owner, -1, -1);
        printf("\nassignment: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
        srcOperand = addedPurposeRegisters[resultReg];

        char *offset = NULL;
        ENTRY *ownerEntry = lookupSymbol(SymbolTable, owner);
        intToStr(getLocalOffset(ownerEntry->localSymbolTable, e->index, ownerEntry->localSymbolTableIndex), &offset);
        sprintf(destOperand, "-%s[%s]", offset, specialPurposeRegisters[1]);
        asm_mov_write(destOperand, srcOperand);
        availableAddedPurposeRegisters[resultReg] = 0;
    }
    else if (e->scope == 2)
    { // parameter

        int resultReg = parseArithmeticTree(ast->left, owner, -1, -1);
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

    int resultReg = parseArithmeticTree(ast->left, owner, -1, -1);
    printf("\nreturn: register that has the expr val:%s", addedPurposeRegisters[resultReg]);
    srcOperand = addedPurposeRegisters[resultReg];
    asm_mov_write(destOperand, srcOperand);
    fprintf(ofptr, "                               # place the return into rax");
    availableGeneralPurposeRegisters[0] = 0; // make rax available again
    availableAddedPurposeRegisters[resultReg] = 0;

    asm_functionPostamble();
}

/* 
arg: current argument
argc: number of arguments, start with 0 and when we hit NULL left branch we got the final length 
*/
int passFunctionCallParameters(AST *ast, int argc, int owner)
{
    int totalSize = 0;

    if (ast->left)
        passFunctionCallParameters(ast->left, argc + 1, owner);
    else
        totalSize = argc;

    if (argc > 5)
    { // push it on the stack
        if (ast->op == op_INT)
        {
            char *val = NULL;
            intToStr(ast->value, &val);
            asm_push_write(val);
        }
        else
        {
            int regValue = parsePrimaryAst(ast, owner); // later will register of expr
            asm_push_write(addedPurposeRegisters[regValue]);
            availableAddedPurposeRegisters[regValue] = 0;
        }
    }
    else
    { // find the proper register for the value
        char *registerToPush = NULL;
        getRegisterParameter(argc, &registerToPush);
        if (ast->op == op_INT)
        {
            char *val = NULL;
            intToStr(ast->value, &val);
            asm_mov_write(registerToPush, val);
            // make sure to tick r8 and r9 as unavailable untill the function call
            if (argc == 4)
                availableAddedPurposeRegisters[0] = 1;
            else if (argc == 5)
                availableAddedPurposeRegisters[1] = 1;
        }
        else
        {
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
        AST *firstArg = ast->mid;

        int argSize = passFunctionCallParameters(firstArg, 0, owner);

        fprintf(ofptr, "\n\t%s\t%s", controlFlowInstructions[9], e->name);
        fprintf(ofptr, "                               # called function \"%s\"", e->name);

        // after the asm call to function make the registers r8,9 available again if they were used
        if (argSize == 4)
        {
            // clear r8
            availableAddedPurposeRegisters[0] = 0;
        }
        else if (argSize >= 5)
        {
            // clear both r8 and r9
            availableAddedPurposeRegisters[0] = 0;
            availableAddedPurposeRegisters[1] = 0;
        }
    }
    else
    {
        fprintf(ofptr, "\n\t%s\t%s", controlFlowInstructions[9], e->name);
        fprintf(ofptr, "                               # called function \"%s\"", e->name);
    }
}

int parseConditionalAst(AST *ast, int lastLabelNr, int owner)
{
    int label = -1;
    if (ast->left)                                        // if it's not an 'else' or it's an 'else-if' without any 'else-if' / 'else' conditional following
        label = parseArithmeticTree(ast->left, owner, 0, -1); // returns the label of the next conditional
    else
        label = lastLabelNr;

    // parse each statement inside of the conditional
    AST *currStatement = ast->mid;
    int status = parseStatements(currStatement, owner);

    // add jmp to end of conditionals if it's not an 'else' conditional
    // or conditional didn't contain a return statement inside
    if (ast->left && status != -1)
    {
        //  it's pointless to jmp when the last conditional is an 'else-if'
        if (ast->right && (ast->right->op == op_ELSE || ast->right->op == op_ELSEIF))
            asm_jmp_write(controlFlowInstructions[0], makeLabel(lastLabelNr));
    }

    return label;
}

void parseConditionalsAst(AST **ast, int owner)
{
    // first get the number of 'if''else if' 'else' branches to know how many labels to generate
    // because we will need the last conditional's label to jmp to
    // last label is the one after all conditional statements
    int count = 1;

    AST *currAst = (*ast)->right;
    while (currAst != NULL && (currAst->op == op_ELSEIF || currAst->op == op_ELSE))
    {
        count++;
        currAst = currAst->right;
    }
    printf("we will need %d labels \n", count);
    int lastLabelNr = jmpLabels + count;

    // parse each conditional
    currAst = (*ast);
    do
    {
        int nextLabel = parseConditionalAst(currAst, lastLabelNr, owner);
        fprintf(ofptr, "\n%s:", makeLabel(nextLabel));
        currAst = currAst->right;
    } while (currAst != NULL && (currAst->op == op_ELSEIF || currAst->op == op_ELSE));

    *ast = currAst;
}

void parseWhileAst(AST *ast, int owner)
{
    int condLabel = getNewLabel();
    asm_jmp_write(controlFlowInstructions[0], makeLabel(condLabel));

    // parse the statements inside first
    int label = getNewLabel();
    fprintf(ofptr, "\n%s:", makeLabel(label));

    AST *currStatement = ast->mid;
    parseStatements(currStatement, owner);

    // parse the expression
    fprintf(ofptr, "\n%s:", makeLabel(condLabel));
    parseArithmeticTree(ast->left, owner, 1, label);

}

int parseStatements(AST *ast, int owner)
{
    while (ast != NULL)
    {
        switch (ast->op)
        {
        case op_DECLARE:
            parseDeclarationAst(ast, owner);
            ast = ast->right;
            break;
        case OP_ASSIGN:
            parseAssignAst(ast, owner);
            ast = ast->right;
            break;
        case op_RETURN:
            parseReturnAst(ast, owner);
            ast = NULL;
            return -1;
        case op_FCALL:
            parseFunctionCallAst(ast, owner);
            ast = ast->right;
            break;
        case op_IF:
            parseConditionalsAst(&ast, owner);
            break;
        case op_WHILE:
            parseWhileAst(ast, owner);
            ast = ast->right;
            break;
        default:
            printf("error: invalid statement AST! expected %s or %s", "op_DECLARE", "op_ASSIGN");
            exit(1);
        }
    }

    return 1;
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