#ifndef MYAST_H_
# define MYAST_H_

typedef struct ast {
    int op;
    int value;
    struct ast* left;
    struct ast* mid;
    struct ast* right;
} AST;

#endif