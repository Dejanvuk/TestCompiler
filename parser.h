#define MAX_SYMBOL_TABLE_SIZE 512

#include "ast.h"

AST* makeAST(int, int, AST*, AST*, AST*, bool);

AST* makeIdentifierAST(int, AST*, bool);

AST* makeChildlessAST(int, int);

AST* makeOneChildAST(int, int, AST* );

AST* makeProgramAST (AST* );

AST* makeArithmeticExpressionAST (int,AST*, AST*);

/*
left -> expression
*/
AST* makeDeclareAST (int, AST*, AST*, AST*);

AST* makeAssignmentAST (int,int, AST*);

AST *makeConditionalAST(int, AST*, AST*);

AST* makePrimaryExpressionAST (int);

int  getArithmeticOp(int);

AST* expressionStatement(int, int);

AST* declareStatement(int);

AST* assignmentStatement(int);

AST* conditionalStatement(int);

AST* returnStatement(int);

AST* functionCall(int);

AST* assignmentOrFunctionCall(int);

AST* statement(int);

AST* program();

AST* parser();

/* Symbol Table */

typedef struct entry {
    int index;
    char* name;
    int type_specifier; // 0-int 1-double
    int type; // 0-variable 1-function 
    int scope; // 0- global 1-local 2-local function argument
    int owner; // for scope 1 and 2 the owner function symbol id else -1 if owner is program
    struct entry* localSymbolTable[MAX_SYMBOL_TABLE_SIZE]; // for functions only
    int localSymbolTableIndex;
} ENTRY;


int addSymbolEntry(ENTRY**, int*, char*, char*, int, int, int);

int addSymbolToTheOwnersTable(int, char*, char*, int, int);

int checkidentfifi(int);

ENTRY* lookupSymbol(ENTRY**, int);

int getSymbolIndex(ENTRY**, int,char*);

