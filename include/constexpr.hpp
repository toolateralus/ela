#pragma once
#include <unordered_map>
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "ast.hpp"
#include "value.hpp"

struct Context;
struct ASTExpr;

Value *evaluate_constexpr(ASTExpr *node, Context &ctx);

struct CTInterpreter {
  std::unordered_map<InternedString, ExternFunctionValue> extern_functions{};
  Scope *root_scope;
  Scope *current_scope;
  Context &ctx;

  CTInterpreter(Context &context) : ctx(context) {
    root_scope = create_child(ctx.scope);
    current_scope = root_scope;
  }

  Value *try_link_extern_function(Symbol *name);
  void set_value(InternedString &name, Value *value);
  LValue *get_lvalue(ASTNode *node);
  LValue *get_dot_expr_lvalue(ASTDotExpr *dot);
  LValue *get_index_lvalue(ASTIndex *node);
  LValue *get_unary_lvalue(ASTUnaryExpr *node);
  Value *visit_method_call(ASTMethodCall *node);
  Value *visit_path(ASTPath *node);
  Value *visit_pattern_match(ASTPatternMatch *node);
  Value *visit_dyn_of(ASTDyn_Of *node);
  Value *visit_type_of(ASTType_Of *node);
  Value *visit_block(ASTBlock *node);
  Value *visit_expr_statement(ASTExprStatement *node);
  Value *visit_bin_expr(ASTBinExpr *node);
  Value *visit_unary_expr(ASTUnaryExpr *node);
  Value *visit_literal(ASTLiteral *node);
  Value *visit_call(ASTCall *node);
  Value *visit_return(ASTReturn *node);
  Value *visit_dot_expr(ASTDotExpr *node);
  Value *visit_index(ASTIndex *node);
  Value *visit_initializer_list(ASTInitializerList *node);
  Value *visit_range(ASTRange *node);
  Value *visit_switch(ASTSwitch *node);
  Value *visit_tuple(ASTTuple *node);
  Value *visit_cast(ASTCast *node);
  Value *visit_lambda(ASTLambda *node);
  Value *visit_size_of(ASTSize_Of *node);
  Value *visit_function_declaration(ASTFunctionDeclaration *node);
  Value *visit_variable(ASTVariable *node);
  Value *visit_continue(ASTContinue *node);
  Value *visit_break(ASTBreak *node);
  Value *visit_for(ASTFor *node);
  Value *visit_if(ASTIf *node);
  Value *visit_else(ASTElse *node);
  Value *visit_while(ASTWhile *node);
  Value *visit_tuple_deconstruction(ASTDestructure *node);
  Value *visit_impl(ASTImpl *node);
  Value *visit_defer(ASTDefer *node);

  Value *visit_unpack_element(ASTUnpackElement *node);
  Value *visit_unpack(ASTUnpack *node);

  Value *visit(ASTNode *node) {
    switch (node->get_node_type()) {
      case AST_NODE_BLOCK:
        return visit_block(static_cast<ASTBlock *>(node));
      case AST_NODE_FUNCTION_DECLARATION:
        return visit_function_declaration(static_cast<ASTFunctionDeclaration *>(node));
      case AST_NODE_IMPL:
        return visit_impl(static_cast<ASTImpl *>(node));
      case AST_NODE_RETURN:
        return visit_return(static_cast<ASTReturn *>(node));
      case AST_NODE_CONTINUE:
        return visit_continue(static_cast<ASTContinue *>(node));
      case AST_NODE_BREAK:
        return visit_break(static_cast<ASTBreak *>(node));
      case AST_NODE_FOR:
        return visit_for(static_cast<ASTFor *>(node));
      case AST_NODE_IF:
        return visit_if(static_cast<ASTIf *>(node));
      case AST_NODE_ELSE:
        return visit_else(static_cast<ASTElse *>(node));
      case AST_NODE_WHILE:
        return visit_while(static_cast<ASTWhile *>(node));
      case AST_NODE_VARIABLE:
        return visit_variable(static_cast<ASTVariable *>(node));
      case AST_NODE_EXPR_STATEMENT:
        return visit_expr_statement(static_cast<ASTExprStatement *>(node));
      case AST_NODE_DEFER:
        return visit_defer(static_cast<ASTDefer *>(node));
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return visit_tuple_deconstruction(static_cast<ASTDestructure *>(node));
      case AST_NODE_LAMBDA:
        return visit_lambda(static_cast<ASTLambda *>(node));
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr(static_cast<ASTBinExpr *>(node));
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr(static_cast<ASTUnaryExpr *>(node));
      case AST_NODE_LITERAL:
        return visit_literal(static_cast<ASTLiteral *>(node));
      case AST_NODE_PATH:
        return visit_path(static_cast<ASTPath *>(node));
      case AST_NODE_TUPLE:
        return visit_tuple(static_cast<ASTTuple *>(node));
      case AST_NODE_CALL:
        return visit_call(static_cast<ASTCall *>(node));
      case AST_NODE_METHOD_CALL:
        return visit_method_call(static_cast<ASTMethodCall *>(node));
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr(static_cast<ASTDotExpr *>(node));
      case AST_NODE_INDEX:
        return visit_index(static_cast<ASTIndex *>(node));
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list(static_cast<ASTInitializerList *>(node));
      case AST_NODE_SIZE_OF:
        return visit_size_of(static_cast<ASTSize_Of *>(node));
      case AST_NODE_TYPE_OF:
        return visit_type_of(static_cast<ASTType_Of *>(node));
      case AST_NODE_DYN_OF:
        return visit_dyn_of(static_cast<ASTDyn_Of *>(node));
      case AST_NODE_CAST:
        return visit_cast(static_cast<ASTCast *>(node));
      case AST_NODE_RANGE:
        return visit_range(static_cast<ASTRange *>(node));
      case AST_NODE_SWITCH:
        return visit_switch(static_cast<ASTSwitch *>(node));
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match(static_cast<ASTPatternMatch *>(node));
      case AST_NODE_UNPACK:
        return visit_unpack(static_cast<ASTUnpack *>(node));
      case AST_NODE_UNPACK_ELEMENT:
        return visit_unpack_element(static_cast<ASTUnpackElement *>(node));
      case AST_NODE_RUN:  // We do not process recursive run statements.
      // other nodes are ignored because theyre irrelevant.
      default:
        return null_value();
        break;
    }
  }
};
