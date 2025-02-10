#include "ast_copier.hpp"
#include "ast.hpp"

AST *ASTCopier::copy(AST *node) {
  switch (node->node_type) {
    case AST_NODE_PROGRAM:
    case AST_NODE_BLOCK:
    case AST_NODE_FUNCTION:
    case AST_NODE_DECLARATION:
    case AST_NODE_EXPR_STATEMENT:
    case AST_NODE_BIN_EXPR:
    case AST_NODE_UNARY_EXPR:
    case AST_NODE_IDENTIFIER:
    case AST_NODE_LITERAL:
    case AST_NODE_TYPE:
    case AST_NODE_TUPLE:
    case AST_NODE_CALL:
    case AST_NODE_RETURN:
    case AST_NODE_CONTINUE:
    case AST_NODE_BREAK:
    case AST_NODE_FOR:
    case AST_NODE_IF:
    case AST_NODE_ELSE:
    case AST_NODE_WHILE:
    case AST_NODE_STRUCT:
    case AST_NODE_DOT_EXPR:
    case AST_NODE_SCOPE_RESOLUTION:
    case AST_NODE_SUBSCRIPT:
    case AST_NODE_INITIALIZER_LIST:
    case AST_NODE_ENUM:
    case AST_NODE_NOOP:
    case AST_NODE_ALIAS:
    case AST_NODE_IMPL:
    case AST_NODE_INTERFACE:
    case AST_NODE_SIZE_OF:
    case AST_NODE_DEFER:
    case AST_NODE_CAST:
    case AST_NODE_LAMBDA:
    case AST_NODE_RANGE:
    case AST_NODE_SWITCH:
    case AST_NODE_TUPLE_DECONSTRUCTION:
    case AST_NODE_WHERE:
    case AST_NODE_STATEMENT_LIST:
      break;
  }
}
Scope *ASTCopier::copy_scope(Scope *old) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope(*old);
  if (current_scope) {
    scope->parent = current_scope;
  }
  return scope;
}
AST *ASTCopier::copy_program(AST *node) {
  auto new_node = copy(node);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    if (stmt->node_type == AST_NOOP)
      continue;
    new_node->statements.push_back(copy_node(stmt));
  }
  return new_node;
}
AST *ASTCopier::copy_block(AST *node) {
  auto new_node = copy(node);
  // new_node->statements.clear();
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (auto stmt : node->statements) {
    // new_node->statements.push_back(copy_node(stmt));
  }
  current_scope = old_scope;
  return new_node;
}
AST *ASTCopier::copy_function_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->params = copy_node(node->params);
  new_node->return_type = copy_node(node->return_type);
  new_node->generic_instantiations.clear();

  auto old_scope = current_scope;
  if (node->scope) {
    new_node->scope = copy_scope(node->scope);
    current_scope = new_node->scope;
  }

  if (node->where_clause) {
    new_node->where_clause = (AST *)copy_node(node->where_clause.get());
  }

  if (node->block) {
    new_node->block = copy_node(node->block.get());
    node->block.get()->scope->parent = new_node->scope;
  }
  current_scope = old_scope;
  return new_node;
}
AST *ASTCopier::copy_params_decl(AST *node) {
  auto new_node = copy(node);
  new_node->params.clear();
  for (auto param : node->params) {
    new_node->params.push_back(copy_node(param));
  }
  return new_node;
}
AST *ASTCopier::copy_param_decl(AST *node) {
  auto new_node = copy(node);
  if (new_node->tag == ASTParamDecl::Normal) {
    new_node->normal.type = copy_node(node->normal.type);
  } else {
    new_node->self.is_pointer = node->self.is_pointer;
  }
  return new_node;
}
AST *ASTCopier::copy_declaration(AST *node) {
  auto new_node = copy(node);
  if (node->type)
    new_node->type = copy_node(node->type);
  if (node->value)
    new_node->value = copy_node(node->value.get());
  return new_node;
}
AST *ASTCopier::copy_expr_statement(AST *node) {
  auto new_node = copy(node);
  new_node->expression = copy_node(node->expression);
  return new_node;
}
AST *ASTCopier::copy_bin_expr(AST *node) {
  auto new_node = copy(node);
  new_node->left = copy_node(node->left);
  new_node->right = copy_node(node->right);
  return new_node;
}
AST *ASTCopier::copy_unary_expr(AST *node) {
  auto new_node = copy(node);
  new_node->operand = copy_node(node->operand);
  return new_node;
}
AST *ASTCopier::copy_identifier(AST *node) {
  return new (ast_alloc<ASTIdentifier>()) ASTIdentifier(*node);
}

AST *ASTCopier::copy_literal(AST *node) {
  auto new_node = copy(node);
  // do anything here?
  return new_node;
}
AST *ASTCopier::copy_type(AST *node) {
  auto new_node = copy(node);
  new_node->resolved_type = node->resolved_type;
  if (node->pointing_to)
    new_node->pointing_to = copy_node(node->pointing_to.get());
  switch (new_node->kind) {
    case ASTType::NORMAL:
    case ASTType::SELF:
    case ASTType::REFLECTION:
      new_node->normal.generic_arguments.clear();
      for (auto arg : node->normal.generic_arguments) {
        new_node->normal.generic_arguments.push_back(copy_node(arg));
      }
      break;
    case ASTType::TUPLE:
      new_node->tuple_types.clear();
      for (auto type : node->tuple_types) {
        new_node->tuple_types.push_back(copy_node(type));
      }
      break;
    case ASTType::FUNCTION:
      if (node->function.return_type) {
        new_node->function.return_type = copy_node(node->function.return_type.get());
      }
      node->function.parameter_types.clear();
      for (auto param_ty : node->function.parameter_types) {
        new_node->function.parameter_types.push_back(copy_node(param_ty));
      }
      break;
  }
  return new_node;
}
AST *ASTCopier::copy_call(AST *node) {
  auto new_node = copy(node);
  new_node->function = copy_node(node->function);
  new_node->arguments = copy_node(node->arguments);
  new_node->generic_arguments.clear();
  for (auto arg : node->generic_arguments) {
    new_node->generic_arguments.push_back(copy_node(arg));
  }
  return new_node;
}
AST *ASTCopier::copy_arguments(AST *node) {
  auto new_node = copy(node);
  new_node->arguments.clear();
  for (auto arg : node->arguments) {
    new_node->arguments.push_back(copy_node(arg));
  }
  return new_node;
}
AST *ASTCopier::copy_return(AST *node) {
  auto new_node = copy(node);
  if (node->expression)
    new_node->expression = copy_node(node->expression.get());
  return new_node;
}
AST *ASTCopier::copy_continue(AST *node) { return new (ast_alloc<ASTContinue>()) ASTContinue(*node); }
AST *ASTCopier::copy_break(AST *node) { return new (ast_alloc<ASTBreak>()) ASTBreak(*node); }
AST *ASTCopier::copy_for(AST *node) {
  auto new_node = copy(node);
  new_node->iter_identifier = copy_node(node->iter_identifier);
  new_node->range = copy_node(node->range);
  new_node->block = copy_node(node->block);
  return new_node;
}
AST *ASTCopier::copy_if(AST *node) {
  auto new_node = copy(node);
  new_node->condition = copy_node(node->condition);
  new_node->block = copy_node(node->block);
  if (node->_else)
    new_node->_else = copy_node(node->_else.get());
  return new_node;
}
AST *ASTCopier::copy_else(AST *node) {
  auto new_node = copy(node);
  if (node->_if)
    new_node->_if = copy_node(node->_if.get());
  if (node->block)
    new_node->block = copy_node(node->block.get());
  return new_node;
}
AST *ASTCopier::copy_while(AST *node) {
  auto new_node = copy(node);
  if (node->condition)
    new_node->condition = copy_node(node->condition.get());
  new_node->block = copy_node(node->block);
  return new_node;
}

AST *ASTCopier::copy_dot_expr(AST *node) {
  auto new_node = copy(node);
  new_node->base = copy_node(node->base);
  return new_node;
}
AST *ASTCopier::copy_subscript(AST *node) {
  auto new_node = copy(node);
  new_node->left = copy_node(node->left);
  new_node->subscript = copy_node(node->subscript);
  return new_node;
}

AST *ASTCopier::copy_initializer_list(AST *node) {
  auto new_node = copy(node);
  new_node->key_values.clear();
  if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    for (auto expr : node->values) {
      new_node->values.push_back(copy_node(expr));
    }
  } else {
    for (auto [id, expr] : node->key_values) {
      new_node->key_values.push_back({id, copy_node(expr))};
    }
  }
  return new_node;
}
AST *ASTCopier::copy_enum_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->key_values.clear();
  for (auto &kv : node->key_values) {
    new_node->key_values.push_back({kv.first, (AST *)copy_node(kv.second)});
  }
  return new_node;
}

AST *ASTCopier::copy_struct_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->members.clear();
  if (node->where_clause) {
    new_node->where_clause = (AST *)copy_node(node->where_clause.get());
  }
  for (auto &member : node->members) {
    new_node->members.push_back({.is_bitfield = member.is_bitfield,
                                 .bitsize = member.bitsize,
                                 .name = member.name,
                                 .type = copy_node(member.type))};
  }
  new_node->subtypes.clear();
  for (auto subtype : node->subtypes) {
    new_node->subtypes.push_back(copy_node(subtype));
  }
  current_scope = old_scope;
  return new_node;
}
AST *ASTCopier::copy_interface_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->methods.clear();
  if (node->where_clause) {
    new_node->where_clause = (AST *)copy_node(node->where_clause.get());
  }
  for (auto field : node->methods) {
    new_node->methods.push_back(copy_node(field));
  }
  current_scope = old_scope;
  return new_node;
}
AST *ASTCopier::copy_range(AST *node) {
  auto new_node = copy(node);
  new_node->left = copy_node(node->left);
  new_node->right = copy_node(node->right);
  return new_node;
}
AST *ASTCopier::copy_switch(AST *node) {
  auto new_node = copy(node);
  new_node->target = copy_node(node->target);
  new_node->cases.clear();
  for (auto &case_ : node->cases) {
    new_node->cases.push_back(
        {copy_node(case_.expression)), copy_node(case_.block))};
  }
  return new_node;
}
AST *ASTCopier::copy_tuple(AST *node) {
  auto new_node = copy(node);
  new_node->values.clear();
  for (auto value : node->values) {
    new_node->values.push_back(copy_node(value));
  }
  return new_node;
}
AST *ASTCopier::copy_tuple_deconstruction(AST *node) {
  auto new_node = copy(node);
  new_node->idens.clear();
  for (auto iden : node->idens) {
    new_node->idens.push_back(copy_node(iden));
  }
  new_node->right = copy_node(node->right);
  return new_node;
}
AST *ASTCopier::copy_scope_resolution(AST *node) {
  auto new_node = copy(node);
  new_node->base = copy_node(node->base);
  return new_node;
}
AST *ASTCopier::copy_impl(AST *node) {
  auto new_node = copy(node);
  new_node->target = copy_node(node->target);
  if (new_node->interface) {
    new_node->interface = copy_node(new_node->interface.get());
  }
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->methods.clear();
  if (node->where_clause) {
    new_node->where_clause = (AST *)copy_node(node->where_clause.get());
  }
  for (const auto &method : node->methods) {
    new_node->methods.push_back(copy_node(method));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_cast(AST *node) {
  auto new_node = copy(node);
  new_node->expression = (AST *)copy_node(node->expression);
  new_node->target_type = (AST *)copy_node(node->target_type);
  return new_node;
}

AST *ASTCopier::copy_where(AST *node) {
  auto new_node = copy(node);
  new_node->predicate = copy_node(node->predicate);
  new_node->target_type = copy_node(node->target_type);
  return new_node;
}

AST *ASTCopier::copy_node(AST *node) {
  const auto type = node->node_type;
  switch (type) {
    case AST_NODE_WHERE:
      return copy_where(node);
    case AST_NODE_CAST:
      return copy_cast(node);
    case AST_NODE_IMPL:
      return copy_impl(node);
    case AST_NODE_PROGRAM:
      return copy_program(node);
    case AST_NODE_BLOCK:
      return copy_block(node);
    case AST_NODE_FUNCTION:
      return copy_function_declaration(node);
    case AST_NODE_PARAMS_DECL:
      return copy_params_decl(node);
    case AST_NODE_PARAM_DECL:
      return copy_param_decl(node);
    case AST_NODE_DECLARATION:
      return copy_declaration(node);
    case AST_NODE_EXPR_STATEMENT:
      return copy_expr_statement(node);
    case AST_NODE_BIN_EXPR:
      return copy_bin_expr(node);
    case AST_NODE_UNARY_EXPR:
      return copy_unary_expr(node);
    case AST_NODE_IDENTIFIER:
      return copy_identifier(node);
    case AST_NODE_LITERAL:
      return copy_literal(node);
    case AST_NODE_TYPE:
      return copy_type(node);
    case AST_NODE_TUPLE:
      return copy_tuple(node);
    case AST_NODE_CALL:
      return copy_call(node);
    case AST_NODE_ARGUMENTS:
      return copy_arguments(node);
    case AST_NODE_RETURN:
      return copy_return(node);
    case AST_NODE_CONTINUE:
      return copy_continue(node);
    case AST_NODE_BREAK:
      return copy_break(node);
    case AST_NODE_FOR:
      return copy_for(node);
    case AST_NODE_IF:
      return copy_if(node);
    case AST_NODE_ELSE:
      return copy_else(node);
    case AST_NODE_WHILE:
      return copy_while(node);
    case AST_NODE_STRUCT:
      return copy_struct_declaration(node);
    case AST_NODE_DOT_EXPR:
      return copy_dot_expr(node);
    case AST_NODE_SCOPE_RESOLUTION:
      return copy_scope_resolution(node);
    case AST_NODE_SUBSCRIPT:
      return copy_subscript(node);
    case AST_NODE_INITIALIZER_LIST:
      return copy_initializer_list(node);
    case AST_NODE_ENUM_DECLARATION:
      return copy_enum_declaration(node);
    case AST_NODE_RANGE:
      return copy_range(node);
    case AST_NODE_SWITCH:
      return copy_switch(node);
    case AST_NODE_TUPLE_DECONSTRUCTION:
      return copy_tuple_deconstruction(node);
    case AST_NODE_INTERFACE_DECLARATION:
      return copy_interface_declaration(node);
    case AST_NODE_NOOP:
      return node;
    case AST_NODE_ALIAS:
      return copy_alias(node);
    case AST_NODE_SIZE_OF:
      return copy_sizeof(static_cast<ASTSize_Of *>(node));
    case AST_NODE_DEFER:
      return copy_defer(node);
    case AST_NODE_LAMBDA:
      return copy_lambda(node);
    case AST_NODE_STATEMENT_LIST:
      return copy_statement_list(node);
  }
}

AST *deep_copy_ast(AST *root) {
  ASTCopier copier;
  return copier.copy_node(root);
}

AST *ASTCopier::copy_alias(AST *node) {
  auto new_node = copy(node);
  new_node->type = copy_node(node->type);
  return new_node;
}

ASTSize_Of *ASTCopier::copy_sizeof(ASTSize_Of *node) {
  auto new_node = copy(node);
  new_node->target_type = copy_node(node->target_type);
  return new_node;
}

AST *ASTCopier::copy_defer(AST *node) {
  auto new_node = copy(node);
  new_node->statement = copy_node(node->statement);
  return new_node;
}

AST *ASTCopier::copy_lambda(AST *node) {
  auto new_node = copy(node);
  new_node->params = copy_node(node->params);
  new_node->return_type = copy_node(node->return_type);
  new_node->block = copy_node(node->block);
  return new_node;
}

AST *ASTCopier::copy_statement_list(AST *node) {
  auto new_node = copy(node);
  // new_node->statements.clear();
  for (auto stmt : node->statements) {
    // new_node->statements.push_back(copy_node(stmt));
  }
  return new_node;
}