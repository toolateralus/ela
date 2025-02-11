#pragma once

#include "ast.hpp"
#include "scope.hpp"

struct ASTCopier {
  Scope current_scope;
  Scope copy_scope(Scope old);
  
  AST *copy(AST *node, AST *new_parent);
  AST *copy_impl(AST *node, AST *new_parent);
  AST *copy_program(AST *node, AST *new_parent);
  AST *copy_block(AST *node, AST *new_parent);
  AST *copy_function_declaration(AST *node, AST *new_parent);
  AST *copy_params_decl(AST *node, AST *new_parent);
  AST *copy_param_decl(AST *node, AST *new_parent);
  AST *copy_declaration(AST *node, AST *new_parent);
  AST *copy_expr_statement(AST *node, AST *new_parent);
  AST *copy_bin_expr(AST *node, AST *new_parent);
  AST *copy_unary_expr(AST *node, AST *new_parent);
  AST *copy_identifier(AST *node, AST *new_parent);
  AST *copy_literal(AST *node, AST *new_parent);
  AST *copy_type(AST *node, AST *new_parent);
  AST *copy_call(AST *node, AST *new_parent);
  AST *copy_arguments(AST *node, AST *new_parent);
  AST *copy_return(AST *node, AST *new_parent);
  AST *copy_continue(AST *node, AST *new_parent);
  AST *copy_break(AST *node, AST *new_parent);
  AST *copy_for(AST *node, AST *new_parent);
  AST *copy_if(AST *node, AST *new_parent);
  AST *copy_else(AST *node, AST *new_parent);
  AST *copy_while(AST *node, AST *new_parent);
  AST *copy_struct_declaration(AST *node, AST *new_parent);
  AST *copy_dot_expr(AST *node, AST *new_parent);
  AST *copy_subscript(AST *node, AST *new_parent);
  AST *copy_initializer_list(AST *node, AST *new_parent);
  AST *copy_enum_declaration(AST *node, AST *new_parent);
  AST *copy_range(AST *node, AST *new_parent);
  AST *copy_switch(AST *node, AST *new_parent);
  AST *copy_tuple(AST *node, AST *new_parent);
  AST *copy_tuple_deconstruction(AST *node, AST *new_parent);
  AST *copy_interface_declaration(AST *node, AST *new_parent);
  AST *copy_alias(AST *node, AST *new_parent);
  AST *copy_sizeof(AST *node, AST *new_parent);
  AST *copy_defer(AST *node, AST *new_parent);
  AST *copy_lambda(AST *node, AST *new_parent);
  AST *copy_statement_list(AST *node, AST *new_parent);
  AST *copy_scope_resolution(AST *node, AST *new_parent);
  AST *copy_node(AST *node, AST *new_parent);
  AST *copy_where(AST *node, AST *new_parent);
  AST *copy_cast(AST *node, AST *new_parent);
};

AST *deep_copy_ast(AST *root);