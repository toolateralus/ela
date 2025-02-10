#pragma once

#include "ast.hpp"
#include "scope.hpp"

struct ASTCopier {
  Scope *current_scope = nullptr;
  Scope copy_scope(Scope *old);

  AST *copy(AST* node);
  AST *copy_impl(AST* node);
  AST *copy_program(AST *node);
  AST *copy_block(AST *node);
  AST *copy_function_declaration(AST *node);
  AST *copy_params_decl(AST *node);
  AST *copy_param_decl(AST *node);
  AST *copy_declaration(AST *node);
  AST *copy_expr_statement(AST *node);
  AST *copy_bin_expr(AST *node);
  AST *copy_unary_expr(AST *node);
  AST *copy_identifier(AST *node);
  AST *copy_literal(AST *node);
  AST *copy_type(AST *node);
  AST *copy_call(AST *node);
  AST *copy_arguments(AST *node);
  AST *copy_return(AST *node);
  AST *copy_continue(AST *node);
  AST *copy_break(AST *node);
  AST *copy_for(AST *node);
  AST *copy_if(AST *node);
  AST *copy_else(AST *node);
  AST *copy_while(AST *node);
  AST *copy_struct_declaration(AST *node);
  AST *copy_dot_expr(AST *node);
  AST *copy_subscript(AST *node);
  AST *copy_initializer_list(AST *node);
  AST *copy_enum_declaration(AST *node);
  AST *copy_range(AST *node);
  AST *copy_switch(AST *node);
  AST *copy_tuple(AST *node);
  AST *copy_tuple_deconstruction(AST *node);
  AST *copy_interface_declaration(AST *node);
  AST *copy_alias(AST *node);
  AST *copy_sizeof(AST *node);
  AST *copy_defer(AST *node);
  AST *copy_lambda(AST *node);
  AST *copy_statement_list(AST *node);
  AST *copy_scope_resolution(AST *node);
  AST *copy_node(AST *node);
  AST *copy_where(AST *node);
  AST *copy_cast(AST *node);
};

AST *deep_copy_ast(AST *root);