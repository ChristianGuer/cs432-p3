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

void AnalysisVisitor_check_literal(NodeVisitor *visitor, ASTNode *node)
{
    NULL;
}
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

void AnalysisVisitor_check_vardecl(NodeVisitor *visitor, ASTNode *node)
{
    // DecafType var_type = GET_INFERRED_TYPE(node->vardecl.type);
    if (node->vardecl.type == VOID)
    {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);
    }
    if (node->vardecl.array_length < 1)
    {
        // array of length 0
        ErrorList_printf(ERROR_LIST, "Zero length array '%s' on line %d", node->vardecl.name, node->source_line);
    }
}

void AnalysisVisitor_check_location(NodeVisitor *visitor, ASTNode *node)
{
    // This requires a symbol lookup
    Symbol *symbol = lookup_symbol_with_reporting(visitor, node, node->location.name);
    if (symbol == NULL)
    {
        return; // error already reported
    }
}

// This requires a symbol lookup
/* TODO: infer types of locations (this will require a symbol lookup) */
void AnalysisVisitor_infer_binaryop(NodeVisitor *visitor, ASTNode *node)
{
    lookup_symbol_with_reporting(visitor, node, node->location.name);
}

void AnalysisVisitor_check_binaryop(NodeVisitor *visitor, ASTNode *node)
{
    printf("Node %s at line %d has type %s\n",
       NodeType_to_string(node->type),
       node->source_line,
       DecafType_to_string(GET_INFERRED_TYPE(node)));

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
        SET_INFERRED_TYPE(INT);
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
}

void AnalysisVisitor_check_break_continue(NodeVisitor *visitor, ASTNode *node)
{
    ASTNode *parent = (ASTNode *)ASTNode_get_attribute(node, "parent");
    if (parent == NULL)
    {
        return;
    }
    if (parent->type == WHILELOOP)
    {
        puts("Did break correctly");
        return;
    }

    // Check for break and continue statements outside of loops
    NodeList *stmts = node->block.statements;
    ASTNode *current_node = stmts->head;
    while (current_node != NULL)
    {
        if (current_node->type == BREAKSTMT || current_node->type == CONTINUESTMT)
        {
            ErrorList_printf(ERROR_LIST, "Compilation error: Breakout keyword outside of loop on line %d", current_node->source_line);
        }
        current_node = current_node->next;
    }
}

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
        // TODO check type of location
        Symbol *symbol = lookup_symbol_with_reporting(visitor, node->conditional.condition, node->conditional.condition->location.name);
        if (symbol == NULL)
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

ErrorList *analyze(ASTNode *tree)
{
    /* allocate analysis structures */
    NodeVisitor *v = NodeVisitor_new();
    v->data = (void *)AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_program      = NULL;
    v->postvisit_program     = AnalysisVisitor_check_main;
    v->previsit_vardecl      = NULL;
    v->postvisit_vardecl     = AnalysisVisitor_check_vardecl;
    v->previsit_funcdecl     = NULL;
    v->postvisit_funcdecl    = NULL;
    v->previsit_block        = AnalysisVisitor_check_break_continue;
    v->postvisit_block       = NULL;
    v->previsit_assignment   = NULL;
    v->postvisit_assignment  = NULL;
    v->previsit_conditional  = AnalysisVisitor_check_conditional;
    v->postvisit_conditional = NULL;
    v->previsit_whileloop    = NULL;
    v->postvisit_whileloop   = NULL;
    v->previsit_return       = NULL;
    v->postvisit_return      = NULL;
    v->previsit_break        = NULL;
    v->postvisit_break       = NULL;
    v->previsit_continue     = NULL;
    v->postvisit_continue    = NULL;
    v->previsit_binaryop     = NULL;
    v->invisit_binaryop      = NULL;
    v->postvisit_binaryop    = AnalysisVisitor_check_binaryop;
    v->previsit_unaryop      = NULL;
    v->postvisit_unaryop     = NULL;
    v->previsit_location     = AnalysisVisitor_check_location;
    v->postvisit_location    = NULL;
    v->previsit_funccall     = NULL;
    v->postvisit_funccall    = NULL;
    v->previsit_literal      = AnalysisVisitor_infer_literal;
    v->postvisit_literal     = AnalysisVisitor_check_literal;

    // v->postvisit_assignment = AnalysisVisitor_check_assignment;
    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList *errors = ((AnalysisData *)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}
