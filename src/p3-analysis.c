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
    ErrorList* errors;

    /* BOILERPLATE: TODO: add any new desired state information (and clean it up in AnalysisData_free) */

} AnalysisData;

/**
 * @brief Allocate memory for analysis data
 * 
 * @returns Pointer to allocated structure
 */
AnalysisData* AnalysisData_new ()
{
    AnalysisData* data = (AnalysisData*)calloc(1, sizeof(AnalysisData));
    CHECK_MALLOC_PTR(data);
    data->errors = ErrorList_new();
    return data;
}

/**
 * @brief Deallocate memory for analysis data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void AnalysisData_free (AnalysisData* data)
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
#define DATA ((AnalysisData*)visitor->data)

/**
 * @brief Macro for more convenient access to the error list inside a
 * @ref AnalysisVisitor data structure
 */
#define ERROR_LIST (((AnalysisData*)visitor->data)->errors)

/**
 * @brief Wrapper for @ref lookup_symbol that reports an error if the symbol isn't found
 * 
 * @param visitor Visitor with the error list for reporting
 * @param node AST node to begin the search at
 * @param name Name of symbol to find
 * @returns The @ref Symbol if found, otherwise @c NULL
 */
Symbol* lookup_symbol_with_reporting(NodeVisitor* visitor, ASTNode* node, const char* name)
{
    Symbol* symbol = lookup_symbol(node, name);
    if (symbol == NULL) {
        ErrorList_printf(ERROR_LIST, "Symbol '%s' undefined on line %d", name, node->source_line);
    }
    return symbol;
}

/**
 * @brief Macro for shorter storing of the inferred @c type attribute
 */
#define SET_INFERRED_TYPE(T) ASTNode_set_printable_attribute(node, "type", (void*)(T), \
                                 type_attr_print, dummy_free)

/**
 * @brief Macro for shorter retrieval of the inferred @c type attribute
 */
#define GET_INFERRED_TYPE(N) (DecafType)(long)ASTNode_get_attribute(N, "type")


void AnalysisVisitor_infer_literal (NodeVisitor* visitor, ASTNode* node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

void AnalysisVisitor_check_vardecl ( NodeVisitor *visitor, ASTNode *node)
{
    //DecafType var_type = GET_INFERRED_TYPE(node->vardecl.type);
    if (node->vardecl.type == VOID) {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);

    }
    

}

void AnalysisVisitor_check_location (NodeVisitor* visitor, ASTNode* node)
{
    //This requires a symbol lookup
    Symbol* symbol = lookup_symbol_with_reporting(visitor, node, node->location.name);
    if (symbol == NULL) {
        return; //error already reported
    }


}

//This requires a symbol lookup
/* TODO: infer types of locations (this will require a symbol lookup) */
 
void AnalysisVisitor_check_binaryop (NodeVisitor* visitor, ASTNode* node)
{
    DecafType left_type = GET_INFERRED_TYPE(node->binaryop.left);
    DecafType right_type = GET_INFERRED_TYPE(node->binaryop.right);
 
    switch (node->binaryop.operator) {
 
        /* arithmetic and relational operators */
        case ADDOP: case SUBOP: case MULOP: case DIVOP: case MODOP:
        case LTOP: case LEOP: case GEOP: case GTOP:
            if (left_type != INT || right_type != INT) {
                ErrorList_printf(ERROR_LIST,
                    "Type error: binary operator %s requires integer operands "
                    "(found %s and %s) on line %d",
                    BinaryOpToString(node->binaryop.operator),
                    DecafType_to_string(left_type),
                    DecafType_to_string(right_type),
                    node->source_line);
            }
            break;
 
        /* logical operators */
        case OROP: case ANDOP:
            /* TODO: finish */
            break;
 
        /* equality operators */
        case EQOP: case NEQOP:
            if (left_type != right_type) {
                ErrorList_printf(ERROR_LIST,
                    "Type error: binary operator %s requires matching operands "
                    "(found %s and %s) on line %d",
                    (node->binaryop.operator == EQOP ? "==" : "!="),
                    DecafType_to_string(left_type),
                    DecafType_to_string(right_type),
                    node->source_line);
            }
            break;
    }
}
 

ErrorList* analyze (ASTNode* tree)
{
    /* allocate analysis structures */
    NodeVisitor* v = NodeVisitor_new();
    v->data = (void*)AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_literal = AnalysisVisitor_infer_literal;
    v->previsit_vardecl = AnalysisVisitor_check_vardecl;
    v->postvisit_binaryop = AnalysisVisitor_check_binaryop;
    v->previsit_location = AnalysisVisitor_check_location;

    //v->postvisit_assignment = AnalysisVisitor_check_assignment;
    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList* errors = ((AnalysisData*)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}

