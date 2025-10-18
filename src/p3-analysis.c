/*
    Ai Statement: 
        Used ai to make a function
         that checks if a global var and function share a name.
        To help make the check_location func
        And making documentation for functions
*/

/**
 * @file p3-analysis.c
 * @brief Compiler phase 3: static analysis
 */
#include "p3-analysis.h"

/**
 * @brief State/data for static analysis visitor
 */
typedef struct AnalysisData
{
    /**
     * @brief List of errors detected
     */
    ErrorList *errors;

    /* BOILERPLATE: TODO: add any new desired state information (and clean it up in AnalysisData_free) */
    DecafType current_return_type;
    int loopdepth;

} AnalysisData;

/**
 * @brief Allocate memory for analysis data
 *
 * @returns Pointer to allocated structure
 */
AnalysisData *AnalysisData_new()
{
    AnalysisData *data = (AnalysisData *)calloc(1, sizeof(AnalysisData));
    CHECK_MALLOC_PTR(data);
    data->errors = ErrorList_new();
    data->current_return_type = UNKNOWN;
    data->loopdepth = 0;
    return data;
}

/**
 * @brief Deallocate memory for analysis data
 *
 * @param data Pointer to the structure to be deallocated
 */
void AnalysisData_free(AnalysisData *data)
{
    /* free everything in data that is allocated on the heap except the error
     * list; it needs to be returned after the analysis is complete */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the data inside a @ref AnalysisVisitor
 * data structure
 */
#define DATA ((AnalysisData *)visitor->data)

/**
 * @brief Macro for more convenient access to the error list inside a
 * @ref AnalysisVisitor data structure
 */
#define ERROR_LIST (((AnalysisData *)visitor->data)->errors)

/**
 * @brief Wrapper for @ref lookup_symbol that reports an error if the symbol isn't found
 *
 * @param visitor Visitor with the error list for reporting
 * @param node AST node to begin the search at
 * @param name Name of symbol to find
 * @returns The @ref Symbol if found, otherwise @c NULL
 */
Symbol *lookup_symbol_with_reporting(NodeVisitor *visitor, ASTNode *node, const char *name)
{
    Symbol *symbol = lookup_symbol(node, name);
    if (symbol == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Symbol '%s' undefined on line %d", name, node->source_line);
    }
    return symbol;
}

/**
 * @brief Macro for shorter storing of the inferred @c type attribute
 */
#define SET_INFERRED_TYPE(T) ASTNode_set_printable_attribute(node, "type", (void *)(T), \
                                                             type_attr_print, dummy_free)

/**
 * @brief Macro for shorter retrieval of the inferred @c type attribute
 */
#define GET_INFERRED_TYPE(N) (DecafType)(long) ASTNode_get_attribute(N, "type")
/**
 * @brief Infer the type of a literal node
 *
 * @param visitor Visitor structure (not used)
 * @param node Literal AST node
 */
void AnalysisVisitor_infer_literal(NodeVisitor *visitor, ASTNode *node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

/**
 * @brief Infer the type attribute for a variable declaration.
 *
 * Validates illegal declarations (e.g., VOID variables, zero-length arrays, and
 * local array declarations when only globals may be arrays). Emits errors to @ref ERROR_LIST.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref VARDECL
 */

void AnalysisVisitor_infer_vardecl(NodeVisitor *visitor, ASTNode *node)
{
    if (node->vardecl.type == VOID)
    {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);
    }
    if (node->vardecl.is_array && node->vardecl.array_length <= 0)
    {
        // array of length 0
        ErrorList_printf(ERROR_LIST, "Zero length array '%s' on line %d", node->vardecl.name, node->source_line);
    }

    if (node->vardecl.is_array)
    {
        ASTNode *parent = (ASTNode*) ASTNode_get_attribute(node, "parent");

        if(!parent || parent->type != PROGRAM)
        {
            ErrorList_printf(ERROR_LIST,
                "Local Variable %s on line %d cannot be an array", node->vardecl.name, node->source_line);

        }
    }
    
}

/**
 * @brief Type-check and infer a LOCATION node (variable/array element).
 *
 * @param visitor Analysis visitor carrying state and errors
 * @param node    AST node of type @ref LOCATION
 */

void AnalysisVisitor_check_location(NodeVisitor *visitor, ASTNode *node)
{
    // node->type must be LOCATION here
    Symbol *sym = lookup_symbol_with_reporting(visitor, node, node->location.name);
    ASTNode *idx = node->location.index;

    if (!sym) {
        // Already reported: "Symbol 'x' undefined..."
        // Prevent cascading by giving this node a type.
        SET_INFERRED_TYPE(UNKNOWN);
        return;
    }

    if (idx) {
        // Using an index: require array symbol and int index
        DecafType idx_t = GET_INFERRED_TYPE(idx);     // postvisit means child is typed
        if (idx_t != INT) 
        {
            ErrorList_printf(ERROR_LIST,
                "Array index for '%s' must be int (found %s) on line %d",
                sym->name, DecafType_to_string(idx_t), node->source_line);
        }
        if (sym->symbol_type != ARRAY_SYMBOL) 
        {
            ErrorList_printf(ERROR_LIST,
                "Non-array symbol '%s' used with index on line %d",
                sym->name, node->source_line);
            SET_INFERRED_TYPE(UNKNOWN);
            return;
        }
        // Array element type is the symbol's base type
        SET_INFERRED_TYPE(sym->type);
    } else {
        // No index: require scalar symbol
        if (sym->symbol_type == ARRAY_SYMBOL) 
        {
            ErrorList_printf(ERROR_LIST,
                "Array '%s' used without an index on line %d",
                sym->name, node->source_line);
            SET_INFERRED_TYPE(UNKNOWN);
            return;
        }
        SET_INFERRED_TYPE(sym->type);
    }
}



/**
 * @brief Check operand types for a binary operator and set the result type.
 *
 * Enforces INT operands for arithmetic/relational ops, BOOL operands for logical
 * ops, and matching operand types for equality ops. Reports errors to @ref ERROR_LIST
 * and sets this node's inferred type to INT or BOOL depending on the operator.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref BINARYOP
 */
void AnalysisVisitor_check_binaryop(NodeVisitor *visitor, ASTNode *node)
{
    DecafType left_type = GET_INFERRED_TYPE(node->binaryop.left);
    DecafType right_type = GET_INFERRED_TYPE(node->binaryop.right);

    switch (node->binaryop.operator)
    {

    /* arithmetic and relational operators */
    case ADDOP:
    case SUBOP:
    case MULOP:
    case DIVOP:
    case MODOP:
        if (left_type != INT || right_type != INT)
        {
            ErrorList_printf(ERROR_LIST,
                             "Type error: binary operator %s requires integer operands "
                             "(found %s and %s) on line %d",
                             BinaryOpToString(node->binaryop.operator),
                             DecafType_to_string(left_type),
                             DecafType_to_string(right_type),
                             node->source_line);
        }
        SET_INFERRED_TYPE(INT);
        break;
    case LTOP:
    case LEOP:
    case GEOP:
    case GTOP:
        if (left_type != INT || right_type != INT)
        {
            ErrorList_printf(ERROR_LIST,
                             "Type error: binary operator %s requires integer operands "
                             "(found %s and %s) on line %d",
                             BinaryOpToString(node->binaryop.operator),
                             DecafType_to_string(left_type),
                             DecafType_to_string(right_type),
                             node->source_line);
        }
        SET_INFERRED_TYPE(BOOL);
        break;

    /* logical operators */
    case OROP:
    case ANDOP:
        /* TODO: finish */
        if (left_type != BOOL || right_type != BOOL)
        {
            ErrorList_printf(ERROR_LIST,
                             "Type error: binary operator %s requires boolean operands "
                             "(found %s and %s) on line %d",
                             BinaryOpToString(node->binaryop.operator),
                             DecafType_to_string(left_type),
                             DecafType_to_string(right_type),
                             node->source_line);
        }
        SET_INFERRED_TYPE(BOOL);
        break;

    /* equality operators */
    case EQOP:
    case NEQOP:
        if (left_type != right_type)
        {
            ErrorList_printf(ERROR_LIST,
                             "Type error: binary operator %s requires matching operands "
                             "(found %s and %s) on line %d",
                             (node->binaryop.operator == EQOP ? "==" : "!="),
                             DecafType_to_string(left_type),
                             DecafType_to_string(right_type),
                             node->source_line);
        }
        SET_INFERRED_TYPE(BOOL);
        break;
    }
}

/**
 * @brief Verify existence of main
 *  Anduse program symbol table to easily ceck for var/func clashes
 *
 * Ensures exactly one function named @c main exists (current message says "No main")
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref PROGRAM
 */

void AnalysisVisitor_check_main(NodeVisitor *visitor, ASTNode *node)
{
    // Check for existance of exactly one main function in code
    NodeList *funcs = node->program.functions;
    int len = funcs->size;
    if (len < 1)
    {
        ErrorList_printf(ERROR_LIST, "Compilation error: No main function found");
        return;
    }
    ASTNode *current_node = funcs->head;
    int mainCount = 0;
    for (int i = 0; i < len; i++)
    {
        if (current_node != NULL &&
            current_node->type == FUNCDECL)
        {
            if (!strcmp(current_node->funcdecl.name, "main"))
            {
                mainCount++;
            }
        }
        current_node = current_node->next;
    }
    if (mainCount != 1)
    {
        ErrorList_printf(ERROR_LIST, "Compilation error: No main function found");
    }
        SymbolTable *ptab = (SymbolTable*)ASTNode_get_attribute(node, "symbolTable");
    if (!ptab || !ptab->local_symbols) return;

    for (Symbol *s1 = ptab->local_symbols->head; s1; s1 = s1->next) 
    {
        if (s1->symbol_type != FUNCTION_SYMBOL) continue;

        // ensure we report the clash only once per name
        bool already_seen = false;
        for (Symbol *prev = ptab->local_symbols->head; prev != s1; prev = prev->next) 
        {
            if (strcmp(prev->name, s1->name) == 0) 
            { 
                already_seen = true;
                break;
            }
        }
        if (already_seen) continue;

        for (Symbol *s2 = s1->next; s2; s2 = s2->next) 
        {
            if (strcmp(s1->name, s2->name) == 0 && s2->symbol_type != FUNCTION_SYMBOL) 
            {
                ErrorList_printf(ERROR_LIST,
                    "Global name '%s' is used for both a function and a variable/array",
                    s1->name);
                break;
            }
        }
    }
}

/**
 * @brief Helper to compute a DecafType for simple expression forms.
 *
 * Determines the type of @ref BINARYOP (based on operator category), @ref LITERAL
 * (literal's own type), or @ref LOCATION (symbol's declared type). Returns @c UNKNOWN
 * if the symbol lookup fails; callers should avoid cascading errors.
 *
 * @param visitor Analysis visitor
 * @param node    Expression node (BINARYOP, LITERAL, or LOCATION)
 * @return DecafType The best-known type for @p node, or @c UNKNOWN
 */
DecafType type_helper(NodeVisitor *visitor, ASTNode *node)
{
    // return type of node from binary op, literal or location
    NodeType assignmentType = node->type;
    DecafType assignmentDecaf;

    if (assignmentType == BINARYOP)
    {
        BinaryOpType binop = node->binaryop.operator;
        switch (binop)
        {
        // Set as INT for operations returning integers
        case ADDOP:
        case SUBOP:
        case MULOP:
        case DIVOP:
        case MODOP:
            assignmentDecaf = INT;
            break;
        // Set bool for others
        case LTOP:
        case LEOP:
        case GEOP:
        case GTOP:
        case OROP:
        case ANDOP:
        case EQOP:
        case NEQOP:
            assignmentDecaf = BOOL;
            break;
        }
    }
    else if (assignmentType == LITERAL)
    {
        // check literal type
        assignmentDecaf = node->literal.type;
    }
    else if (assignmentType == LOCATION)
    {
        // check type of location
        Symbol *symbol = lookup_symbol(node, node->location.name);
        if (!symbol)
        {
            return UNKNOWN; // error handled in function call
        }
        assignmentDecaf = symbol->type;
    }
    return assignmentDecaf;
}

/**
 * @brief Validate duplicate variable declarations within a block scope.
 *
 * Performs an O(n^2) scan of the block's local symbol list to detect duplicates.
 * Reports "Duplicate variable '<name>'" once per duplicate group.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref BLOCK with an attached symbol table
 */
void AnalysisVisitor_infer_block(NodeVisitor *visitor, ASTNode *node) 
{
    // node is BLOCK here

    SymbolTable *table = (SymbolTable*)ASTNode_get_attribute(node, "symbolTable");
    if (!table || !table->local_symbols) return;

    // O(n^2) duplicate check is fine for project scale
    for (Symbol *s1 = table->local_symbols->head; s1; s1 = s1->next) 
    {
        for (Symbol *s2 = s1->next; s2; s2 = s2->next) 
        {
            //printf(node->);
            if (strcmp(s1->name, s2->name) == 0) 
            {
                ErrorList_printf(ERROR_LIST, "Duplicate variable '%s' on line %d",
                                 s2->name, node->source_line);
                break; // report once per duplicate group
            }
        }
    }
}


/**
 * @brief Enforce that a conditional's condition is boolean-typed.
 *
 * Accepts BOOL-typed expressions and relational/logical/equality binary ops.
 * Reports an error if the condition is an arithmetic-int expression or a
 * non-BOOL literal/location. Uses @ref lookup_symbol_with_reporting for locations.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref CONDITIONAL
 */

void AnalysisVisitor_check_conditional(NodeVisitor *visitor, ASTNode *node)
{
    NodeType type = node->conditional.condition->type;
    if (type == BINARYOP)
    {
        // check binary op returns boolean type
        // Not sure if there is easier way to do this
        BinaryOpType binop = node->conditional.condition->binaryop.operator;
        switch (binop)
        {
        // throw error for operations returning integers
        case ADDOP:
        case SUBOP:
        case MULOP:
        case DIVOP:
        case MODOP:
            ErrorList_printf(ERROR_LIST, "Conditional with non boolean argument at line %d", node->source_line);
            return;
        // Pass for others
        case LTOP:
        case LEOP:
        case GEOP:
        case GTOP:
        case OROP:
        case ANDOP:
        case EQOP:
        case NEQOP:
            break;
        }
    }
    else if (type == LITERAL)
    {
        // check literal has boolean type
        if (GET_INFERRED_TYPE(node->conditional.condition) != BOOL)
        {
            ErrorList_printf(ERROR_LIST, "Conditional with non boolean argument at line %d\n", node->source_line);
            return;
        }
    }
    else if (type == LOCATION)
    {
        // check type of location
        Symbol *symbol = lookup_symbol_with_reporting(visitor, node->conditional.condition, node->conditional.condition->location.name);
        if (!symbol)
        {
            return; // error handled in function call
        }
        if (symbol->type != BOOL)
        {
            ErrorList_printf(ERROR_LIST, "Conditional with non boolean argument at line %d\n", node->source_line);
            return;
        }
    }
}
/**
 * @brief Pre-assign the LHS variable type to the assignment node's inferred type.
 *
 * Looks up the LHS location's symbol and sets the assignment expression's printed
 * type attribute to that symbol's type. Also rejects VOID/UNKNOWN LHS types.
 * (Actual LHS/RHS compatibility is enforced in @ref AnalysisVisitor_check_assignment.)
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref ASSIGNMENT
 */
void AnalysisVisitor_infer_assignment(NodeVisitor *visitor, ASTNode *node)
{
    Symbol *sym = lookup_symbol_with_reporting(visitor, node, node->assignment.location->location.name);
    if (!sym)
    {
        return; // error handled in function call
    }
    DecafType symType = sym->type;
    if(symType == UNKNOWN || symType == VOID)
    {
        ErrorList_printf(ERROR_LIST, "Assignment Type Error: Cant set a variable to Void or UNKOWN.  Type = %s on line %d", DecafType_to_string(symType), node->source_line);

    }
    SET_INFERRED_TYPE(symType);

}


/**
 * @brief Check assignment type compatibility (non-array paths).
 *
 * Compares inferred types of LHS location and RHS expression. Rejects VOID on
 * either side and mismatched types. Skips checks if either side is @c UNKNOWN to
 * prevent cascading diagnostics.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref ASSIGNMENT
 */

void AnalysisVisitor_check_assignment(NodeVisitor *visitor, ASTNode *node)
{
    ASTNode  *lhs = node->assignment.location;
    ASTNode  *rhs = node->assignment.value;

    DecafType lt = GET_INFERRED_TYPE(lhs);  // comes from infer_location
    DecafType rt = GET_INFERRED_TYPE(rhs);  // literal, binop, funccall, etc.

    // Don’t pile on errors if a child already failed to type
    if (lt == UNKNOWN || rt == UNKNOWN) return;

    if (lt == VOID || rt == VOID) 
    {
        ErrorList_printf(ERROR_LIST,
            "Assignment Type Error: 'void' is not a valid value or variable type (line %d)",
            node->source_line);
        return;
    }

    if (lt != rt) 
    {
        ErrorList_printf(ERROR_LIST,
            "Assignment Type Error: assigned type %s does not match variable type %s (line %d)",
            DecafType_to_string(rt), DecafType_to_string(lt), node->source_line);
    }
}


/**
 * @brief Infer the type of a function call expression.
 *
 * Validates that the callee identifier exists and is a function symbol, then
 * assigns the call node's inferred type to the function's return type.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref FUNCCALL
 */
void AnalysisVisitor_infer_funccall(NodeVisitor *visitor, ASTNode *node)
{
    //Check that function is defined 

     Symbol *sym = lookup_symbol_with_reporting(visitor, node, node->funccall.name);
    if (!sym) return;

    if (sym->symbol_type != FUNCTION_SYMBOL) 
    {
        ErrorList_printf(ERROR_LIST,
            "Identifier '%s' is not a function (line %d)",
            node->funccall.name, node->source_line);
        return;
    }

    // Infer the call expression's type = function return type
    DecafType ret = sym->type;         // return type stored here in this codebase
    SET_INFERRED_TYPE(ret);
}

/**
 * @brief Check arity and argument types for a function call.
 *
 * Ensures the callee is a function, the argument count matches the parameter
 * count, and each argument's inferred type equals the corresponding parameter type.
 * Reports descriptive mismatches including argument position.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref FUNCCALL
 */

void AnalysisVisitor_check_funccall(NodeVisitor *visitor, ASTNode *node)
{
    Symbol *symbol = lookup_symbol_with_reporting(visitor, node, node->funccall.name);
    //i dont htink i need to check if symbol is null bc the function already does that
    //get symbol func return type
    if (!symbol) return;

    if (symbol->symbol_type != FUNCTION_SYMBOL)
    {
        // Already reported above, but keep if previsit aint't run
        ErrorList_printf(ERROR_LIST,
            "Identifier '%s' is not a function (line %d)",
            node->funccall.name, node->source_line);
        return;
    }
    int arg_count   = (node->funccall.arguments) ? node->funccall.arguments->size : 0;
    int param_count = (symbol->parameters) ? symbol->parameters->size : 0;

    if(arg_count != param_count)
    {
        ErrorList_printf(ERROR_LIST,
            "Function '%s' expects %d argument but got %d (line %d)",
            symbol->name, param_count, arg_count, node->source_line);
        return;
    }

    //Init the first args the written argument and the required parameter then check that number of arguments match
    ASTNode *argument = node->funccall.arguments ? node->funccall.arguments->head : NULL;
    Parameter *requiredParam = symbol->parameters ? symbol->parameters->head : NULL;

    for (int i = 0; argument && requiredParam; ++i, argument = argument->next, requiredParam = requiredParam->next) 
    {
        DecafType arg_ty = GET_INFERRED_TYPE(argument);  // set by literal/location/binaryop visitors
        DecafType par_ty = requiredParam->type;               // required type from signature



        if (arg_ty != par_ty) 
        {
            ErrorList_printf(ERROR_LIST,
                "Type Mismatch in parameter %d of call to '%s': expected %s but found %s on (line %d)",
                i, symbol->name,
                DecafType_to_string(par_ty),
                DecafType_to_string(arg_ty),
                node->source_line);
                return;
        }
    }
}

/**
 * @brief Validate that a return statement matches the current function's return type.
 *
 * Uses @ref DATA->current_return_type, set in @ref AnalysisVisitor_infer_funcdecl, to
 * check value presence and type. Reports missing values for non-void functions,
 * disallows values in void functions, and flags type mismatches. Skips further
 * checks if the expression's type is @c UNKNOWN.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref RETURNSTMT
 */

void AnalysisVisitor_check_return(NodeVisitor *visitor, ASTNode *node)
{
    DecafType expected = DATA->current_return_type;
    ASTNode *expr = node->funcreturn.value;

    if (expr == NULL) 
    {
        if (expected != VOID) 
        {
            ErrorList_printf(ERROR_LIST,
                "Missing return value (expected %s) on line %d",
                DecafType_to_string(expected), node->source_line);
        }
        return;
    }


    if (expected == VOID) 
    {
        ErrorList_printf(ERROR_LIST,
            "Return with a value in a void function on line %d", node->source_line);
        return;

    }
    DecafType actual = GET_INFERRED_TYPE(expr);    
    if(actual == UNKNOWN){
        //only return bc msg is taken care of already
        return;
    }
    if (actual != expected) 
    {
        ErrorList_printf(ERROR_LIST,
            "Return type mismatch: expected %s, found %s (line %d)",
            DecafType_to_string(expected), DecafType_to_string(actual),
            node->source_line);
    }

}


/**
 * @brief Enter a function declaration and set the expected return type.
 *
 * Stores the function's declared return type in @ref DATA->current_return_type so
 * later @ref RETURNSTMT checks can validate against it.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref FUNCDECL
 */

void AnalysisVisitor_infer_funcdecl(NodeVisitor *visitor, ASTNode *node)
{
    //DATA->current_return_type = node->funcdecl.return_type;
    DATA->current_return_type = node->funcdecl.return_type;
    //printf("%s", DecafType_to_string(node->funcdecl.return_type));
}


static void AnalysisVisitor_enter_while(NodeVisitor *visitor, ASTNode *node) 
{
    DATA->loopdepth++;
}
static void AnalysisVisitor_exit_while(NodeVisitor *visitor, ASTNode *node) 
{
    ASTNode *cond = node->whileloop.condition;

    // Always decrement even if early return
    DecafType t = UNKNOWN;
    if (!cond) 
    {
        ErrorList_printf(ERROR_LIST,
            "While loop missing condition (line %d)", node->source_line);
    } 
    else 
    {
        t = GET_INFERRED_TYPE(cond);  // already set by child visitors
        if (t != UNKNOWN && t != BOOL) 
        {
            ErrorList_printf(ERROR_LIST,
                "Type mismatch: bool expected but found %s on line %d",
                DecafType_to_string(t), node->source_line);
        }
        // If t == UNKNOWN, skip to avoid cascading (came from earlier error)
    }
    DATA->loopdepth--;
}

/**
 * @brief Validate that a @c break statement occurs within a loop.
 *
 * Emits an error if encountered with @ref DATA->loopdepth == 0.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref BREAKSTMT
 */

static void AnalysisVisitor_check_break(NodeVisitor *visitor, ASTNode *node)
{
    if (DATA->loopdepth == 0) 
    {
        ErrorList_printf(ERROR_LIST, "'break' not within a loop (line %d)", node->source_line);
    }
}

/**
 * @brief Validate that a @c continue statement occurs within a loop.
 *
 * Emits an error if encountered with @ref DATA->loopdepth == 0.
 *
 * @param visitor Analysis visitor
 * @param node    AST node of type @ref CONTINUESTMT
 */

static void AnalysisVisitor_check_continue(NodeVisitor *visitor, ASTNode *node) 
{
    if (DATA->loopdepth == 0) 
    {
        ErrorList_printf(ERROR_LIST, "'continue' not within a loop (line %d)", node->source_line);
    }
}

//TODO check for infiintie loop maybe
/*
*/

ErrorList *analyze(ASTNode *tree)
{
    /* allocate analysis structures */

    if(tree == NULL)
    {
        ErrorList *errors = ErrorList_new();
        ErrorList_printf(errors, "NULL AST passed to analyze");
        return errors;
    }

    NodeVisitor *v = NodeVisitor_new();
    v->data = (void *)AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_program = NULL;
    v->postvisit_program = AnalysisVisitor_check_main;
    v->previsit_vardecl = AnalysisVisitor_infer_vardecl;
    v->postvisit_vardecl = NULL;
    v->previsit_funcdecl = AnalysisVisitor_infer_funcdecl;
    v->postvisit_funcdecl = NULL;
    v->previsit_block = AnalysisVisitor_infer_block;
    v->postvisit_block = NULL;
    v->previsit_assignment = NULL;
    v->postvisit_assignment = AnalysisVisitor_check_assignment;
    v->previsit_conditional = AnalysisVisitor_check_conditional; //
    v->postvisit_conditional = NULL;
    v->previsit_whileloop = AnalysisVisitor_enter_while;
    v->postvisit_whileloop = AnalysisVisitor_exit_while;
    v->previsit_return = NULL;
    v->postvisit_return = AnalysisVisitor_check_return;
    v->previsit_break = AnalysisVisitor_check_break;
    v->postvisit_break = NULL;
    v->previsit_continue = AnalysisVisitor_check_continue;
    v->postvisit_continue = NULL;
    v->previsit_binaryop = NULL;
    v->invisit_binaryop = NULL;
    v->postvisit_binaryop = AnalysisVisitor_check_binaryop;
    v->previsit_unaryop = NULL;
    v->postvisit_unaryop = NULL;
    v->previsit_location = NULL;
    v->postvisit_location = AnalysisVisitor_check_location;
    v->previsit_funccall = AnalysisVisitor_infer_funccall;
    v->postvisit_funccall = AnalysisVisitor_check_funccall;
    v->previsit_literal = AnalysisVisitor_infer_literal;
    v->postvisit_literal = NULL;

    // v->postvisit_assignment = AnalysisVisitor_check_assignment;
    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList *errors = ((AnalysisData *)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}
