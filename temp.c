int parseArithmeticTree(AST* ast) {
    if(ast->left == NULL) {
        int op = ast->op;
        if(op == op_INT) {
            int reg = getAvailableRegister();
            asm_mov_write(addedPurposeRegisters[reg],NULL,0, 2, ast->value);
            return reg;
        }
        else if(op == op_IDENTIFIER) {
            //return getcurrStringifierValue(); not yet implemented
            int reg = getAvailableRegister();
            ENTRY* e = lookupSymbol(ast->value);
            asm_mov_write(addedPurposeRegisters[reg],e->name,0, 3, ast->value);
            return reg;
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
            case op_IDENTIFIER:
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

void parseDeclarationAst(AST* ast, int owner) {
    ENTRY* e = lookupSymbol(ast->value);
    int scope = e->scope;
    int type = e->type;
    int typeSpecifier = e->type_specifier;
    int owner = e->owner;

    if(scope == 0) { // global declaration
        if(type == 0) { // variable
            // check if its initialized or not
            if(ast->left) { // initialized add it on .data segment

            } 
            else { // unnitialized add it on .bss segment

            }
        }
        else if(type == 1) { // function

        }
        else {
            printf("error: invalid global declaration type! expected variable or function");
            exit(1);
        }
    }
    else if(scope == 1) { // local declaration
        if(type == 0) { // variable

        }
        else if(type == 1) { // function
            printf("error: functions cant be declared locally!");
            exit(1);
        }
        else {
            printf("error: invalid local declaration type! expected variable!");
            exit(1);
        }
    } 
    else if(type == 2) {// local function argument
        // local function argument declarations are not parsed on their own
        return;
    }
    else {
        printf("error: invalid declaration scope");
        exit(1);
    }

    /*
    char* varSize;
    switch(e->type_specifier) {
        case 0: // int
            varSize = ",4,4";
            break;
        case 1: // double
            varSize = ",8,8";
            break;
    }

    asm_comm_Write(e->name, varSize);

    if(ast->left) {
        //printf("expr value: %d\n", optimized_parseArithmeticTree(ast->left));
        int resultReg = parseArithmeticTree(ast->left);
        printf("register that has the expr val:%s\n", addedPurposeRegisters[resultReg]);
        asm_mov_write(e->name,addedPurposeRegisters[resultReg],2, 0, 0);
        availableAddedPurposeRegisters[resultReg] = 0;
    }
    */
}

void parseAssignAst(AST* ast, int owner) {
    ENTRY* e = NULL;
    if(owner == 0) {
        e = lookupSymbol(SymbolTable,ast->value);
    }
    else { // owner isn't program, first get the function's ENTRY and then get the ast's ENTRY
        e = lookupSymbol(SymbolTable,owner);
        ENTRY** ownersTable = e->localSymbolTable;
        int ownersIndex = e->localSymbolTableIndex;
        e = lookupSymbol(SymbolTable,ast->value);
    }

    int resultReg = parseArithmeticTree(ast->left);
    printf("register that has the expr val:%s\n", addedPurposeRegisters[resultReg]);
    // check whether the symbol is global or local 

    //asm_mov_write(e->name,addedPurposeRegisters[resultReg],2, 0, 0);
    availableAddedPurposeRegisters[resultReg] = 0;

}

void parseProgramAst(AST* ast) {
    AST* currStatementAst = ast->left;
    // parse each statement
    while(currStatementAst != NULL) {
        // check ast op code 
        switch(currStatementAst->op) {
            case op_DECLARE:
                parseDeclarationAst(currStatementAst, 0);
                break;
            case OP_ASSIGN:
                parseAssignAst(currStatementAst, 0);
                break;
            default:
                printf("error: invalid statement AST! expected %s or %s", "op_DECLARE", "op_ASSIGN");
                exit(1);
        }
        currStatementAst = currStatementAst->right;
    }
}

void writeExpression(AST* ast) {
    //printf("expr value: %d\n", optimized_parseArithmeticTree(ast->left));
    //int resultReg = parseArithmeticTree(ast->left);
    //printf("register that has the expr val:%s\n", addedPurposeRegisters[resultReg]);

    //asm_mov_write(generalPurposeRegisters[0],addedPurposeRegisters[resultReg],0, 0, 0);
    //addedPurposeRegisters[resultReg] = 0;
}

void writePostamble() {
    char* postamble = "\r\n\tpop\trbp\r\n\tret\n";
    fwrite(postamble, strlen(postamble), 1, ofptr);
}

/** 
 * 
 * ====================/CODE GENERATION====================
 */