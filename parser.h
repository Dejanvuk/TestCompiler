typedef struct ast {
    int op;
    int value;
    struct ast* left;
    struct ast* mid;
    struct ast* right;
} AST;

AST* makeAST(int, int, AST*, AST*, AST*);

AST* makeChildlessAST(int, int);

AST* makeOneChildAST(int, int, AST* );

AST* makeProgramAST (AST* );

AST* makeArithmeticExpressionAST (int,AST*, AST*);

/*
left -> expression
*/
AST* makeDeclareAST (int,int, AST*, AST*, AST*);

AST* makeAssignmentAST (int,int, AST*);

AST* makePrimaryExpressionAST ();

int  getArithmeticOp(int);

AST* expressionStatement(int);

AST* declareStatement(int);

AST* assignmentStatement();

AST* selectionStatement();

AST* statement();

AST* program();

AST* parser();