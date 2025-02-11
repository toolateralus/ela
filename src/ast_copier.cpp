#include "ast_copier.hpp"
#include "ast.hpp"
#include "scope.hpp"

AST *ASTCopier::copy(AST *node) {
  switch (node->node_type) {
    case AST_PROGRAM:
      return copy_program(node);
    case AST_BLOCK:
      return copy_block(node);
    case AST_FUNCTION:
      return copy_function_declaration(node);
    case AST_DECLARATION:
      return copy_declaration(node);
    case AST_BINARY:
      return copy_bin_expr(node);
    case AST_UNARY_EXPR:
      return copy_unary_expr(node);
    case AST_IDENTIFIER:
      return copy_identifier(node);
    case AST_LITERAL:
      return copy_literal(node);
    case AST_TYPE:
      return copy_type(node);
    case AST_TUPLE:
      return copy_tuple(node);
    case AST_CALL:
      return copy_call(node);
    case AST_RETURN:
      return copy_return(node);
    case AST_CONTINUE:
      return copy_continue(node);
    case AST_BREAK:
      return copy_break(node);
    case AST_FOR:
      return copy_for(node);
    case AST_IF:
      return copy_if(node);
    case AST_ELSE:
      return copy_else(node);
    case AST_WHILE:
      return copy_while(node);
    case AST_STRUCT:
      return copy_struct_declaration(node);
    case AST_DOT:
      return copy_dot_expr(node);
    case AST_SCOPE_RESOLUTION:
      return copy_scope_resolution(node);
    case AST_SUBSCRIPT:
      return copy_subscript(node);
    case AST_ENUM:
      return copy_enum_declaration(node);
    case AST_NOOP:
      return node;
    case AST_ALIAS:
      return copy_alias(node);
    case AST_IMPL:
      return copy_impl(node);
    case AST_INTERFACE:
      return copy_interface_declaration(node);
    case AST_SIZE_OF:
      return copy_sizeof(node);
    case AST_DEFER:
      return copy_defer(node);
    case AST_CAST:
      return copy_cast(node);
    case AST_LAMBDA:
      return copy_lambda(node);
    case AST_RANGE:
      return copy_range(node);
    case AST_SWITCH:
      return copy_switch(node);
    case AST_TUPLE_DECONSTRUCTION:
      return copy_tuple_deconstruction(node);
    case AST_WHERE:
      return copy_where(node);
    case AST_STATEMENT_LIST:
      return copy_statement_list(node);
    case AST_INITIALIZER:
      return copy_initializer_list(node);
  }
  return nullptr;
}

Scope ASTCopier::copy_scope(Scope old) {
  Scope new_scope;
  Symbol *current = old.head;
  Symbol **new_sym = &new_scope.head;

  while (current) {
    *new_sym = new (symbol_arena.allocate(sizeof(Symbol))) Symbol(*current);
    new_sym = &(*new_sym)->next;
    current = current->next;
  }

  return new_scope;
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
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (auto stmt : node->statements) {
    new_node->statements.push_back(copy_node(stmt));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_function_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->function.parameters.clear();
  for (auto param : node->function.parameters) {
    new_node->function.parameters.push_back(param);
  }
  new_node->function.return_type = copy_node(node->function.return_type);
  new_node->function.generic_instantiations.clear();

  auto old_scope = current_scope;
  new_node->scope = copy_scope(node->scope);
  current_scope = new_node->scope;
  if (node->function.where_clause) {
    new_node->function.where_clause = copy_node(node->function.where_clause.get());
  }
  if (node->function.block) {
    new_node->function.block = copy_node(node->function.block.get());
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->declaration.type = copy_node(node->declaration.type);
  new_node->declaration.value = copy_node(node->declaration.value.get());
  return new_node;
}

AST *ASTCopier::copy_bin_expr(AST *node) {
  auto new_node = copy(node);
  new_node->binary.left = copy_node(node->binary.left);
  new_node->binary.right = copy_node(node->binary.right);
  return new_node;
}

AST *ASTCopier::copy_unary_expr(AST *node) {
  auto new_node = copy(node);
  new_node->unary.operand = copy_node(node->unary.operand);
  return new_node;
}

AST *ASTCopier::copy_identifier(AST *node) {
  auto new_node = ast_alloc(AST_IDENTIFIER, node->parent);
  new_node->identifier = node->identifier;
  return new_node;
}

AST *ASTCopier::copy_literal(AST *node) {
  auto new_node = copy(node);
  return new_node;
}

AST *ASTCopier::copy_type(AST *node) {
  auto new_node = copy(node);
  new_node->resolved_type = node->resolved_type;
  if (node->type.pointing_to)
    new_node->type.pointing_to = copy_node(node->type.pointing_to.get());
  switch (new_node->type.kind) {
    case AST_TYPE_NORMAL:
    case AST_TYPE_SELF:
    case AST_TYPE_REFLECTION:
      new_node->type.normal.generic_arguments.clear();
      for (auto arg : node->type.normal.generic_arguments) {
        new_node->type.normal.generic_arguments.push_back(copy_node(arg));
      }
      break;
    case AST_TYPE_TUPLE:
      new_node->type.tuple_types.clear();
      for (auto type : node->type.tuple_types) {
        new_node->type.tuple_types.push_back(copy_node(type));
      }
      break;
    case AST_TYPE_FUNCTION:
      if (node->type.function.return_type) {
        new_node->type.function.return_type = copy_node(node->type.function.return_type.get());
      }
      node->type.function.parameter_types.clear();
      for (auto param_ty : node->type.function.parameter_types) {
        new_node->type.function.parameter_types.push_back(copy_node(param_ty));
      }
      break;
  }
  return new_node;
}

AST *ASTCopier::copy_call(AST *node) {
  auto new_node = copy(node);
  new_node->call.callee = copy_node(node->call.callee);
  new_node->call.arguments.clear();
  for (auto arg : node->call.arguments) {
    new_node->call.arguments.push_back(copy_node(arg));
  }
  new_node->call.generic_arguments.clear();
  for (auto arg : node->call.generic_arguments) {
    new_node->call.generic_arguments.push_back(copy_node(arg));
  }
  return new_node;
}

AST *ASTCopier::copy_return(AST *node) {
  auto new_node = copy(node);
  if (node->$return)
    new_node->$return = copy_node(node->$return.get());
  return new_node;
}

AST *ASTCopier::copy_continue(AST *node) {
  return node;
}

AST *ASTCopier::copy_break(AST *node) {
  return node;
}

AST *ASTCopier::copy_for(AST *node) {
  auto new_node = copy(node);
  new_node->$for.iter_identifier = copy_node(node->$for.iter_identifier);
  new_node->$for.range = copy_node(node->$for.range);
  new_node->$for.block = copy_node(node->$for.block);
  return new_node;
}

AST *ASTCopier::copy_if(AST *node) {
  auto new_node = copy(node);
  new_node->$if.condition = copy_node(node->$if.condition);
  new_node->$if.block = copy_node(node->$if.block);
  if (node->$if.$else)
    new_node->$if.$else = copy_node(node->$if.$else.get());
  return new_node;
}

AST *ASTCopier::copy_else(AST *node) {
  auto new_node = copy(node);
  if (node->$else.elseif)
    new_node->$else.elseif = copy_node(node->$else.elseif.get());
  if (node->$else.block)
    new_node->$else.block = copy_node(node->$else.block.get());
  return new_node;
}

AST *ASTCopier::copy_while(AST *node) {
  auto new_node = copy(node);
  if (node->$while.condition)
    new_node->$while.condition = copy_node(node->$while.condition.get());
  new_node->$while.block = copy_node(node->$while.block);
  return new_node;
}

AST *ASTCopier::copy_dot_expr(AST *node) {
  auto new_node = copy(node);
  new_node->dot.base = copy_node(node->dot.base);
  return new_node;
}

AST *ASTCopier::copy_scope_resolution(AST *node) {
  auto new_node = copy(node);
  new_node->scope_resolution.base = copy_node(node->scope_resolution.base);
  return new_node;
}

AST *ASTCopier::copy_subscript(AST *node) {
  auto new_node = copy(node);
  new_node->subscript.left = copy_node(node->subscript.left);
  new_node->subscript.index_expression = copy_node(node->subscript.index_expression);
  return new_node;
}

AST *ASTCopier::copy_initializer_list(AST *node) {
  auto new_node = copy(node);
  new_node->initializer.key_values.clear();
  if (node->initializer.tag == INITIALIZER_COLLECTION) {
    for (auto expr : node->initializer.values) {
      new_node->initializer.values.push_back(copy_node(expr));
    }
  } else {
    for (auto [id, expr] : node->initializer.key_values) {
      new_node->initializer.key_values.push_back({id, copy_node(expr)});
    }
  }
  return new_node;
}

AST *ASTCopier::copy_enum_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->$enum.key_values.clear();
  for (auto &kv : node->$enum.key_values) {
    new_node->$enum.key_values.push_back({kv.first, copy_node(kv.second)});
  }
  return new_node;
}

AST *ASTCopier::copy_struct_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->$struct.members.clear();
  if (node->$struct.where_clause) {
    new_node->$struct.where_clause = copy_node(node->$struct.where_clause.get());
  }
  for (auto &member : node->$struct.members) {
    new_node->$struct.members.push_back({.is_bitfield = member.is_bitfield,
                                             .bitsize = member.bitsize,
                                             .name = member.name,
                                             .type = copy_node(member.type)});
  }
  new_node->$struct.subtypes.clear();
  for (auto subtype : node->$struct.subtypes) {
    new_node->$struct.subtypes.push_back(copy_node(subtype));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_interface_declaration(AST *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->interface.methods.clear();
  if (node->interface.where_clause) {
    new_node->interface.where_clause = copy_node(node->interface.where_clause.get());
  }
  for (auto field : node->interface.methods) {
    new_node->interface.methods.push_back(copy_node(field));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_range(AST *node) {
  auto new_node = copy(node);
  new_node->range.left = copy_node(node->range.left);
  new_node->range.right = copy_node(node->range.right);
  return new_node;
}

AST *ASTCopier::copy_switch(AST *node) {
  auto new_node = copy(node);
  new_node->$switch.target = copy_node(node->$switch.target);
  new_node->$switch.cases.clear();
  for (auto &case_ : node->$switch.cases) {
    new_node->$switch.cases.push_back({copy_node(case_.expression), copy_node(case_.block)});
  }
  return new_node;
}

AST *ASTCopier::copy_tuple(AST *node) {
  auto new_node = copy(node);
  new_node->tuple.clear();
  for (auto value : node->tuple) {
    new_node->tuple.push_back(copy_node(value));
  }
  return new_node;
}

AST *ASTCopier::copy_tuple_deconstruction(AST *node) {
  auto new_node = copy(node);
  new_node->tuple_deconstruction.idens.clear();
  for (auto iden : node->tuple_deconstruction.idens) {
    new_node->tuple_deconstruction.idens.push_back(copy_node(iden));
  }
  new_node->tuple_deconstruction.right = copy_node(node->tuple_deconstruction.right);
  return new_node;
}

AST *ASTCopier::copy_impl(AST *node) {
  auto new_node = copy(node);
  new_node->impl.target = copy_node(node->impl.target);
  if (new_node->impl.interface) {
    new_node->impl.interface = copy_node(new_node->impl.interface.get());
  }
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->impl.methods.clear();
  if (node->impl.where_clause) {
    new_node->impl.where_clause = copy_node(node->impl.where_clause.get());
  }
  for (const auto &method : node->impl.methods) {
    new_node->impl.methods.push_back(copy_node(method));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_cast(AST *node) {
  auto new_node = copy(node);
  new_node->cast.expression = copy_node(node->cast.expression);
  new_node->cast.target_type = copy_node(node->cast.target_type);
  return new_node;
}

AST *ASTCopier::copy_where(AST *node) {
  auto new_node = copy(node);
  new_node->where.predicate = copy_node(node->where.predicate);
  new_node->where.target_type = copy_node(node->where.target_type);
  return new_node;
}

AST *ASTCopier::copy_node(AST *node) {
  const auto type = node->node_type;
  switch (type) {
    case AST_WHERE:
      return copy_where(node);
    case AST_CAST:
      return copy_cast(node);
    case AST_IMPL:
      return copy_impl(node);
    case AST_PROGRAM:
      return copy_program(node);
    case AST_BLOCK:
      return copy_block(node);
    case AST_FUNCTION:
      return copy_function_declaration(node);
    case AST_DECLARATION:
      return copy_declaration(node);
    case AST_BINARY:
      return copy_bin_expr(node);
    case AST_UNARY_EXPR:
      return copy_unary_expr(node);
    case AST_IDENTIFIER:
      return copy_identifier(node);
    case AST_LITERAL:
      return copy_literal(node);
    case AST_TYPE:
      return copy_type(node);
    case AST_TUPLE:
      return copy_tuple(node);
    case AST_CALL:
      return copy_call(node);
    case AST_RETURN:
      return copy_return(node);
    case AST_CONTINUE:
      return copy_continue(node);
    case AST_BREAK:
      return copy_break(node);
    case AST_FOR:
      return copy_for(node);
    case AST_IF:
      return copy_if(node);
    case AST_ELSE:
      return copy_else(node);
    case AST_WHILE:
      return copy_while(node);
    case AST_STRUCT:
      return copy_struct_declaration(node);
    case AST_DOT:
      return copy_dot_expr(node);
    case AST_SCOPE_RESOLUTION:
      return copy_scope_resolution(node);
    case AST_SUBSCRIPT:
      return copy_subscript(node);
    case AST_INITIALIZER:
      return copy_initializer_list(node);
    case AST_ENUM:
      return copy_enum_declaration(node);
    case AST_RANGE:
      return copy_range(node);
    case AST_SWITCH:
      return copy_switch(node);
    case AST_TUPLE_DECONSTRUCTION:
      return copy_tuple_deconstruction(node);
    case AST_INTERFACE:
      return copy_interface_declaration(node);
    case AST_NOOP:
      return node;
    case AST_ALIAS:
      return copy_alias(node);
    case AST_SIZE_OF:
      return copy_sizeof(node);
    case AST_DEFER:
      return copy_defer(node);
    case AST_LAMBDA:
      return copy_lambda(node);
    case AST_STATEMENT_LIST:
      return copy_statement_list(node);
  }
  return nullptr;
}

AST *deep_copy_ast(AST *root) {
  ASTCopier copier;
  return copier.copy_node(root);
}

AST *ASTCopier::copy_alias(AST *node) {
  auto new_node = copy(node);
  new_node->alias.type = copy_node(node->alias.type);
  return new_node;
}

AST *ASTCopier::copy_sizeof(AST *node) {
  auto new_node = copy(node);
  new_node->size_of = copy_node(node->size_of);
  return new_node;
}

AST *ASTCopier::copy_defer(AST *node) {
  auto new_node = copy(node);
  new_node->defer = copy_node(node->defer);
  return new_node;
}

AST *ASTCopier::copy_lambda(AST *node) {
  auto new_node = copy(node);
  new_node->lambda.parameters.clear();
  for (auto param : node->lambda.parameters) {
    new_node->lambda.parameters.push_back(param);
  }
  new_node->lambda.return_type = copy_node(node->lambda.return_type);
  new_node->lambda.block = copy_node(node->lambda.block);
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