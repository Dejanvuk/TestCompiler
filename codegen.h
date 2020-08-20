#include "ast.h"
int parseArithmeticTree(AST*, int);
int optimized_parseArithmeticTree(AST*);
void parseDeclarationAst(AST*, int);
void parseAssignAst(AST*, int);
int passFunctionCallParameters(AST*, int, int);
void parseFunctionCallAst(AST*, int);
void parseConditionalsAst(AST**,int);
int parseConditionalAst(AST*,int, int);
void parseStatements(AST*, int);
void parseProgramAst(AST*);