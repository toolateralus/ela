#include "ast.hpp"
#include "thir.hpp"

struct ASTResolver {
  THIRGen &gen;

  void visit(ASTNode *node) {
    switch (node->get_node_type()) {
      case AST_NODE_STATEMENT_LIST: {
        ASTStatementList *list = (ASTStatementList *)node;
        for (const auto &stmt : list->statements) {
          visit(stmt);
        }
        return;
      } break;
      case AST_NODE_PROGRAM:
        return visit_program((ASTProgram *)node);
      case AST_NODE_BLOCK:
        return visit_block((ASTBlock *)node);
      case AST_NODE_FUNCTION_DECLARATION:
        return visit_function_declaration((ASTFunctionDeclaration *)node);
      case AST_NODE_ALIAS:
        return visit_alias((ASTAlias *)node);
      case AST_NODE_IMPL:
        return visit_impl((ASTImpl *)node);
      case AST_NODE_IMPORT:
        return visit_import((ASTImport *)node);
      case AST_NODE_MODULE:
        return visit_module((ASTModule *)node);
      case AST_NODE_RETURN:
        return visit_return((ASTReturn *)node);
      case AST_NODE_CONTINUE:
        return visit_continue((ASTContinue *)node);
      case AST_NODE_BREAK:
        return visit_break((ASTBreak *)node);
      case AST_NODE_FOR:
        return visit_for((ASTFor *)node);
      case AST_NODE_IF:
        return visit_if((ASTIf *)node);
      case AST_NODE_ELSE:
        return visit_else((ASTElse *)node);
      case AST_NODE_WHILE:
        return visit_while((ASTWhile *)node);
      case AST_NODE_STRUCT_DECLARATION:
        return visit_struct((ASTStructDeclaration *)node);
      case AST_NODE_ENUM_DECLARATION:
        return visit_enum((ASTEnumDeclaration *)node);
      case AST_NODE_CHOICE_DECLARATION:
        return visit_choice((ASTChoiceDeclaration *)node);
      case AST_NODE_TRAIT_DECLARATION:
        return visit_trait((ASTTraitDeclaration *)node);
      case AST_NODE_PARAMS_DECL:
        return visit_parameters((ASTParamsDecl *)node);
      case AST_NODE_PARAM_DECL:
        return visit_parameter((ASTParamDecl *)node);
      case AST_NODE_VARIABLE:
        return visit_variable((ASTVariable *)node);
      case AST_NODE_EXPR_STATEMENT:
        return visit_expr_statement((ASTExprStatement *)node);
      case AST_NODE_BIN_EXPR:
        return visit_binary((ASTBinExpr *)node);
      case AST_NODE_UNARY_EXPR:
        return visit_unary((ASTUnaryExpr *)node);
      case AST_NODE_LITERAL:
        return visit_literal((ASTLiteral *)node);
      case AST_NODE_PATH:
        return visit_path((ASTPath *)node);
      case AST_NODE_TYPE:
        return visit_type((ASTType *)node);
      case AST_NODE_TUPLE:
        return visit_tuple((ASTTuple *)node);
      case AST_NODE_CALL:
        return visit_call((ASTCall *)node);
      case AST_NODE_METHOD_CALL:
        return visit_method_call((ASTMethodCall *)node);
      case AST_NODE_ARGUMENTS:
        return visit_arguments((ASTArguments *)node);
      case AST_NODE_DOT_EXPR:
        return visit_dot((ASTDotExpr *)node);
      case AST_NODE_INDEX:
        return visit_index((ASTIndex *)node);
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list((ASTInitializerList *)node);
      case AST_NODE_SIZE_OF:
        return visit_sizeof((ASTSize_Of *)node);
      case AST_NODE_TYPE_OF:
        return visit_typeof((ASTType_Of *)node);
      case AST_NODE_DYN_OF:
        return visit_dynof((ASTDyn_Of *)node);
      case AST_NODE_DEFER:
        return visit_defer((ASTDefer *)node);
      case AST_NODE_CAST:
        return visit_cast((ASTCast *)node);
      case AST_NODE_LAMBDA:
        return visit_lambda((ASTLambda *)node);
      case AST_NODE_UNPACK:
        return visit_unpack_expr((ASTUnpackExpr *)node);
      case AST_NODE_UNPACK_ELEMENT:
        return visit_unpack((ASTUnpackElement *)node);
      case AST_NODE_RANGE:
        return visit_range((ASTRange *)node);
      case AST_NODE_SWITCH:
        return visit_switch((ASTSwitch *)node);
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return visit_destructure((ASTDestructure *)node);
      case AST_NODE_WHERE:
        return visit_where((ASTWhere *)node);
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match((ASTPatternMatch *)node);
      case AST_NODE_WHERE_STATEMENT:
        return visit_where_statement((ASTWhereStatement *)node);
      case AST_NODE_NOOP:
      case AST_NODE_RUN:
        return;
    }
  }

  void visit_expr_statement(ASTExprStatement *node) { return visit(node->expression); }
  void visit_sizeof(ASTSize_Of *node) { return visit(node->target_type); }
  void visit_defer(ASTDefer *node) { return visit(node->statement); }

  void declare_type(Type *type);
  void define_type(Type *type);
  void visit_struct(ASTStructDeclaration *node);
  void visit_program(ASTProgram *node);
  void visit_block(ASTBlock *node);
  void visit_function_declaration(ASTFunctionDeclaration *node);
  void visit_parameters(ASTParamsDecl *node);
  void visit_parameter(ASTParamDecl *node);
  void visit_variable(ASTVariable *node);
  void visit_binary(ASTBinExpr *node);
  void visit_unary(ASTUnaryExpr *node);
  void visit_index(ASTIndex *node);
  void visit_path(ASTPath *node);
  void visit_literal(ASTLiteral *);
  void visit_type(ASTType *node);
  void visit_typeof(ASTType_Of *node);
  void visit_call(ASTCall *node);
  void visit_arguments(ASTArguments *node);
  void visit_return(ASTReturn *node);
  void visit_continue(ASTContinue *);
  void visit_break(ASTBreak *);
  void visit_for(ASTFor *node);
  void visit_if(ASTIf *node);
  void visit_else(ASTElse *node);
  void visit_while(ASTWhile *node);
  void visit_dot(ASTDotExpr *node);
  void visit_initializer_list(ASTInitializerList *node);
  void visit_enum(ASTEnumDeclaration *node);
  void visit_range(ASTRange *node);
  void visit_switch(ASTSwitch *node);
  void visit_tuple(ASTTuple *node);
  void visit_destructure(ASTDestructure *node);
  
  void visit_alias(ASTAlias *);
  void visit_import(ASTImport *);
  void visit_impl(ASTImpl *node);
  void visit_choice(ASTChoiceDeclaration *node);
  void visit_cast(ASTCast *node);
  void visit_trait(ASTTraitDeclaration *);
  void visit_lambda(ASTLambda *node);
  void visit_where(ASTWhere *node);
  void visit_module(ASTModule *);
  void visit_dynof(ASTDyn_Of *node);
  void visit_pattern_match(ASTPatternMatch *node);
  void visit_method_call(ASTMethodCall *node);
  void visit_where_statement(ASTWhereStatement *node);
  void visit_unpack_expr(ASTUnpackExpr *);
  void visit_unpack(ASTUnpackElement *node);
};