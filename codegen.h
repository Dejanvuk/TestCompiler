#include "ast.h"
int parseArithmeticTree(AST*, int);
int optimized_parseArithmeticTree(AST*);
void parseDeclarationAst(AST*, int);
void parseAssignAst(AST*, int);
void parseStatements(AST*, int);
void parseProgramAst(AST*);