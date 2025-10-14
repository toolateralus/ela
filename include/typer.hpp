#include "ast.hpp"


struct Typisting {
  inline void visit_node(ASTNode *node) {
    switch (node->get_node_type()) {
      case AST_NODE_PROGRAM:
        visit_program((ASTProgram *)node);
        break;
      case AST_NODE_BLOCK:
        visit_block((ASTBlock *)node);
        break;
      case AST_NODE_FUNCTION_DECLARATION:
        visit_function_declaration((ASTFunctionDeclaration *)node);
        break;
      case AST_NODE_NOOP:
        visit_noop((ASTNoop *)node);
        break;
      case AST_NODE_ALIAS:
        visit_alias((ASTAlias *)node);
        break;
      case AST_NODE_IMPL:
        visit_impl((ASTImpl *)node);
        break;
      case AST_NODE_IMPORT:
        visit_import((ASTImport *)node);
        break;
      case AST_NODE_MODULE:
        visit_module((ASTModule *)node);
        break;
      case AST_NODE_RETURN:
        visit_return((ASTReturn *)node);
        break;
      case AST_NODE_CONTINUE:
        visit_continue((ASTContinue *)node);
        break;
      case AST_NODE_BREAK:
        visit_break((ASTBreak *)node);
        break;
      case AST_NODE_FOR:
        visit_for((ASTFor *)node);
        break;
      case AST_NODE_IF:
        visit_if((ASTIf *)node);
        break;
      case AST_NODE_ELSE:
        visit_else((ASTElse *)node);
        break;
      case AST_NODE_WHILE:
        visit_while((ASTWhile *)node);
        break;
      case AST_NODE_STRUCT_DECLARATION:
        visit_struct_declaration((ASTStructDeclaration *)node);
        break;
      case AST_NODE_ENUM_DECLARATION:
        visit_enum_declaration((ASTEnumDeclaration *)node);
        break;
      case AST_NODE_CHOICE_DECLARATION:
        visit_choice_declaration((ASTChoiceDeclaration *)node);
        break;
      case AST_NODE_TRAIT_DECLARATION:
        visit_trait_declaration((ASTTraitDeclaration *)node);
        break;
      case AST_NODE_VARIABLE:
        visit_variable((ASTVariable *)node);
        break;
      case AST_NODE_EXPR_STATEMENT:
        visit_expr_statement((ASTExprStatement *)node);
        break;
      case AST_NODE_BIN_EXPR:
        visit_bin_expr((ASTBinExpr *)node);
        break;
      case AST_NODE_UNARY_EXPR:
        visit_unary_expr((ASTUnaryExpr *)node);
        break;
      case AST_NODE_LITERAL:
        visit_literal((ASTLiteral *)node);
        break;
      case AST_NODE_PATH:
        visit_path((ASTPath *)node);
        break;
      case AST_NODE_TYPE:
        visit_type((ASTType *)node);
        break;
      case AST_NODE_TUPLE:
        visit_tuple((ASTTuple *)node);
        break;
      case AST_NODE_CALL:
        visit_call((ASTCall *)node);
        break;
      case AST_NODE_METHOD_CALL:
        visit_method_call((ASTMethodCall *)node);
        break;
      case AST_NODE_ARGUMENTS:
        visit_arguments((ASTArguments *)node);
        break;
      case AST_NODE_DOT_EXPR:
        visit_dot_expr((ASTDotExpr *)node);
        break;
      case AST_NODE_INDEX:
        visit_index((ASTIndex *)node);
        break;
      case AST_NODE_INITIALIZER_LIST:
        visit_initializer_list((ASTInitializerList *)node);
        break;
      case AST_NODE_SIZE_OF:
        visit_size_of((ASTSize_Of *)node);
        break;
      case AST_NODE_TYPE_OF:
        visit_type_of((ASTType_Of *)node);
        break;
      case AST_NODE_DYN_OF:
        visit_dyn_of((ASTDyn_Of *)node);
        break;
      case AST_NODE_DEFER:
        visit_defer((ASTDefer *)node);
        break;
      case AST_NODE_CAST:
        visit_cast((ASTCast *)node);
        break;
      case AST_NODE_LAMBDA:
        visit_lambda((ASTLambda *)node);
        break;
      case AST_NODE_UNPACK:
        visit_unpack((ASTUnpack *)node);
        break;
      case AST_NODE_UNPACK_ELEMENT:
        visit_unpack_element((ASTUnpackElement *)node);
        break;
      case AST_NODE_RANGE:
        visit_range((ASTRange *)node);
        break;
      case AST_NODE_SWITCH:
        visit_switch((ASTSwitch *)node);
        break;
      case AST_NODE_TUPLE_DECONSTRUCTION:
        visit_destructure((ASTDestructure *)node);
        break;
      case AST_NODE_WHERE:
        visit_where((ASTWhere *)node);
        break;
      case AST_NODE_PATTERN_MATCH:
        visit_pattern_match((ASTPatternMatch *)node);
        break;
      case AST_NODE_STATEMENT_LIST:
        visit_statement_list((ASTStatementList *)node);
        break;
      case AST_NODE_WHERE_STATEMENT:
        visit_where_statement((ASTWhereStatement *)node);
        break;
      case AST_NODE_RUN:
        visit_run((ASTRun *)node);
        break;
      default:
        // Handle unknown node type if necessary
        break;
    }
  }

  void visit_program(ASTProgram *node);
  void visit_block(ASTBlock *node);
  void visit_function_declaration(ASTFunctionDeclaration *node);
  void visit_noop(ASTNoop *node);
  void visit_alias(ASTAlias *node);
  void visit_impl(ASTImpl *node);
  void visit_import(ASTImport *node);
  void visit_module(ASTModule *node);
  void visit_return(ASTReturn *node);
  void visit_continue(ASTContinue *node);
  void visit_break(ASTBreak *node);
  void visit_for(ASTFor *node);
  void visit_if(ASTIf *node);
  void visit_else(ASTElse *node);
  void visit_while(ASTWhile *node);
  void visit_struct_declaration(ASTStructDeclaration *node);
  void visit_enum_declaration(ASTEnumDeclaration *node);
  void visit_choice_declaration(ASTChoiceDeclaration *node);
  void visit_trait_declaration(ASTTraitDeclaration *node);
  void visit_variable(ASTVariable *node);
  void visit_expr_statement(ASTExprStatement *node);
  void visit_bin_expr(ASTBinExpr *node);
  void visit_unary_expr(ASTUnaryExpr *node);
  void visit_literal(ASTLiteral *node);
  void visit_path(ASTPath *node);
  void visit_type(ASTType *node);
  void visit_tuple(ASTTuple *node);
  void visit_call(ASTCall *node);
  void visit_method_call(ASTMethodCall *node);
  void visit_arguments(ASTArguments *node);
  void visit_dot_expr(ASTDotExpr *node);
  void visit_index(ASTIndex *node);
  void visit_initializer_list(ASTInitializerList *node);
  void visit_size_of(ASTSize_Of *node);
  void visit_type_of(ASTType_Of *node);
  void visit_dyn_of(ASTDyn_Of *node);
  void visit_defer(ASTDefer *node);
  void visit_cast(ASTCast *node);
  void visit_lambda(ASTLambda *node);
  void visit_unpack(ASTUnpack *node);
  void visit_unpack_element(ASTUnpackElement *node);
  void visit_range(ASTRange *node);
  void visit_switch(ASTSwitch *node);
  void visit_destructure(ASTDestructure *node);
  void visit_where(ASTWhere *node);
  void visit_pattern_match(ASTPatternMatch *node);
  void visit_statement_list(ASTStatementList *node);
  void visit_where_statement(ASTWhereStatement *node);
  void visit_run(ASTRun *node);
};
