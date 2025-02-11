#include "ast_copier.hpp"
#include "ast.hpp"
#include "scope.hpp"

AST *ASTCopier::copy(AST *node, AST *new_parent) {
  auto new_node = (AST*)ast_arena.allocate(sizeof(AST));
  new_node->node_type = node->node_type;
  new_node->parent = new_parent;
  switch (new_node->node_type) {
    case AST_PROGRAM:
      new (&new_node->statements) std::vector<AST *>(node->statements);
      break;
    case AST_BLOCK:
      new (&new_node->block) decltype(new_node->block)(node->block);
      break;
    case AST_FUNCTION:
      new (&new_node->function) decltype(new_node->function)(node->function);
      break;
    case AST_DECLARATION:
      new (&new_node->declaration) decltype(new_node->declaration)(node->declaration);
      break;
    case AST_BINARY:
      new (&new_node->binary) decltype(new_node->binary)(node->binary);
      break;
    case AST_UNARY_EXPR:
      new (&new_node->unary) decltype(new_node->unary)(node->unary);
      break;
    case AST_IDENTIFIER:
      new (&new_node->identifier) decltype(new_node->identifier)(node->identifier);
      break;
    case AST_LITERAL:
      new (&new_node->literal) decltype(new_node->literal)(node->literal);
      break;
    case AST_TYPE:
      memcpy(&new_node->type, &node->type, sizeof(decltype(new_node->type)));
      break;
    case AST_TUPLE:
      new (&new_node->tuple) decltype(new_node->tuple)(node->tuple);
      break;
    case AST_CALL:
      new (&new_node->call) decltype(new_node->call)(node->call);
      break;
    case AST_RETURN:
      new (&new_node->$return) decltype(new_node->$return)(node->$return);
      break;
    case AST_CONTINUE:
    case AST_BREAK:
      break;
    case AST_FOR:
      new (&new_node->$for) decltype(new_node->$for)(node->$for);
      break;
    case AST_IF:
      new (&new_node->$if) decltype(new_node->$if)(node->$if);
      break;
    case AST_ELSE:
      new (&new_node->$else) decltype(new_node->$else)(node->$else);
      break;
    case AST_WHILE:
      new (&new_node->$while) decltype(new_node->$while)(node->$while);
      break;
    case AST_STRUCT:
      new (&new_node->$struct) decltype(new_node->$struct)(node->$struct);
      break;
    case AST_DOT:
      new (&new_node->dot) decltype(new_node->dot)(node->dot);
      break;
    case AST_SCOPE_RESOLUTION:
      new (&new_node->scope_resolution) decltype(new_node->scope_resolution)(node->scope_resolution);
      break;
    case AST_SUBSCRIPT:
      new (&new_node->subscript) decltype(new_node->subscript)(node->subscript);
      break;
    case AST_INITIALIZER:
      memcpy(&new_node->initializer, &node->initializer, sizeof(decltype(new_node->initializer)));
      break;
    case AST_ENUM:
      new (&new_node->$enum) decltype(new_node->$enum)(node->$enum);
      break;
    case AST_NOOP:
      break;
    case AST_ALIAS:
      new (&new_node->alias) decltype(new_node->alias)(node->alias);
      break;
    case AST_IMPL:
      new (&new_node->impl) decltype(new_node->impl)(node->impl);
      break;
    case AST_INTERFACE:
      new (&new_node->interface) decltype(new_node->interface)(node->interface);
      break;
    case AST_SIZE_OF:
      new (&new_node->size_of) decltype(new_node->size_of)(node->size_of);
      break;
    case AST_DEFER:
      new (&new_node->defer) decltype(new_node->defer)(node->defer);
      break;
    case AST_CAST:
      new (&new_node->cast) decltype(new_node->cast)(node->cast);
      break;
    case AST_LAMBDA:
      new (&new_node->lambda) decltype(new_node->lambda)(node->lambda);
      break;
    case AST_RANGE:
      new (&new_node->range) decltype(new_node->range)(node->range);
      break;
    case AST_SWITCH:
      new (&new_node->$switch) decltype(new_node->$switch)(node->$switch);
      break;
    case AST_TUPLE_DECONSTRUCTION:
      new (&new_node->tuple_deconstruction) decltype(new_node->tuple_deconstruction)(node->tuple_deconstruction);
      break;
    case AST_WHERE:
      new (&new_node->where) decltype(new_node->where)(node->where);
      break;
    case AST_STATEMENT_LIST:
      new (&new_node->statements) std::vector<AST *>();
      break;
    default:
      break;
  }
  return new_node;
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

AST *ASTCopier::copy_program(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    if (stmt->node_type == AST_NOOP)
      continue;
    new_node->statements.push_back(copy_node(stmt, new_node));
  }
  return new_node;
}

AST *ASTCopier::copy_block(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (auto stmt : node->statements) {
    new_node->statements.push_back(copy_node(stmt, new_node));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_function_declaration(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->function.parameters.clear();
  for (auto param : node->function.parameters) {
    new_node->function.parameters.push_back(param);
  }
  new_node->function.return_type = copy_node(node->function.return_type, new_node);
  new_node->function.generic_instantiations.clear();

  auto old_scope = current_scope;
  new_node->scope = copy_scope(node->scope);
  current_scope = new_node->scope;
  if (node->function.where_clause) {
    new_node->function.where_clause = copy_node(node->function.where_clause.get(), new_node);
  }
  if (node->function.block) {
    new_node->function.block = copy_node(node->function.block.get(), new_node);
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_declaration(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->declaration.type = copy_node(node->declaration.type, new_node);
  new_node->declaration.value = copy_node(node->declaration.value.get(), new_node);
  return new_node;
}

AST *ASTCopier::copy_bin_expr(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->binary.left = copy_node(node->binary.left, new_node);
  new_node->binary.right = copy_node(node->binary.right, new_node);
  return new_node;
}

AST *ASTCopier::copy_unary_expr(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->unary.operand = copy_node(node->unary.operand, new_node);
  return new_node;
}

AST *ASTCopier::copy_identifier(AST *node, AST *new_parent) {
  auto new_node = ast_alloc(AST_IDENTIFIER, new_parent);
  new_node->identifier = node->identifier;
  return new_node;
}

AST *ASTCopier::copy_literal(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  return new_node;
}

AST *ASTCopier::copy_type(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->resolved_type = node->resolved_type;
  if (node->type.pointing_to)
    new_node->type.pointing_to = copy_node(node->type.pointing_to.get(), new_node);
  switch (new_node->type.kind) {
    case AST_TYPE_NORMAL:
    case AST_TYPE_SELF:
    case AST_TYPE_REFLECTION:
      new_node->type.normal.generic_arguments.clear();
      for (auto arg : node->type.normal.generic_arguments) {
        new_node->type.normal.generic_arguments.push_back(copy_node(arg, new_node));
      }
      break;
    case AST_TYPE_TUPLE:
      new_node->type.tuple_types.clear();
      for (auto type : node->type.tuple_types) {
        new_node->type.tuple_types.push_back(copy_node(type, new_node));
      }
      break;
    case AST_TYPE_FUNCTION:
      if (node->type.function.return_type) {
        new_node->type.function.return_type = copy_node(node->type.function.return_type.get(), new_node);
      }
      node->type.function.parameter_types.clear();
      for (auto param_ty : node->type.function.parameter_types) {
        new_node->type.function.parameter_types.push_back(copy_node(param_ty, new_node));
      }
      break;
  }
  return new_node;
}

AST *ASTCopier::copy_call(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->call.callee = copy_node(node->call.callee, new_node);
  new_node->call.arguments.clear();
  for (auto arg : node->call.arguments) {
    new_node->call.arguments.push_back(copy_node(arg, new_node));
  }
  new_node->call.generic_arguments.clear();
  for (auto arg : node->call.generic_arguments) {
    new_node->call.generic_arguments.push_back(copy_node(arg, new_node));
  }
  return new_node;
}

AST *ASTCopier::copy_return(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  if (node->$return)
    new_node->$return = copy_node(node->$return.get(), new_node);
  return new_node;
}

AST *ASTCopier::copy_continue(AST *node, AST *new_parent) { return node; }

AST *ASTCopier::copy_break(AST *node, AST *new_parent) { return node; }

AST *ASTCopier::copy_for(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->$for.iter_identifier = copy_node(node->$for.iter_identifier, new_node);
  new_node->$for.range = copy_node(node->$for.range, new_node);
  new_node->$for.block = copy_node(node->$for.block, new_node);
  return new_node;
}

AST *ASTCopier::copy_if(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->$if.condition = copy_node(node->$if.condition, new_node);
  new_node->$if.block = copy_node(node->$if.block, new_node);
  if (node->$if.$else)
    new_node->$if.$else = copy_node(node->$if.$else.get(), new_node);
  return new_node;
}

AST *ASTCopier::copy_else(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  if (node->$else.elseif)
    new_node->$else.elseif = copy_node(node->$else.elseif.get(), new_node);
  if (node->$else.block)
    new_node->$else.block = copy_node(node->$else.block.get(), new_node);
  return new_node;
}

AST *ASTCopier::copy_while(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  if (node->$while.condition)
    new_node->$while.condition = copy_node(node->$while.condition.get(), new_node);
  new_node->$while.block = copy_node(node->$while.block, new_node);
  return new_node;
}

AST *ASTCopier::copy_dot_expr(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->dot.base = copy_node(node->dot.base, new_node);
  return new_node;
}

AST *ASTCopier::copy_scope_resolution(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->scope_resolution.base = copy_node(node->scope_resolution.base, new_node);
  return new_node;
}

AST *ASTCopier::copy_subscript(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->subscript.left = copy_node(node->subscript.left, new_node);
  new_node->subscript.index_expression = copy_node(node->subscript.index_expression, new_node);
  return new_node;
}

AST *ASTCopier::copy_initializer_list(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->initializer.key_values.clear();
  if (node->initializer.tag == INITIALIZER_COLLECTION) {
    for (auto expr : node->initializer.values) {
      new_node->initializer.values.push_back(copy_node(expr, new_node));
    }
  } else {
    for (auto [id, expr] : node->initializer.key_values) {
      new_node->initializer.key_values.push_back({id, copy_node(expr, new_node)});
    }
  }
  return new_node;
}

AST *ASTCopier::copy_enum_declaration(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->$enum.key_values.clear();
  for (auto &kv : node->$enum.key_values) {
    new_node->$enum.key_values.push_back({kv.first, copy_node(kv.second, new_node)});
  }
  return new_node;
}

AST *ASTCopier::copy_struct_declaration(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->$struct.members.clear();
  if (node->$struct.where_clause) {
    new_node->$struct.where_clause = copy_node(node->$struct.where_clause.get(), new_node);
  }
  for (auto &member : node->$struct.members) {
    new_node->$struct.members.push_back({.is_bitfield = member.is_bitfield,
                                         .bitsize = member.bitsize,
                                         .name = member.name,
                                         .type = copy_node(member.type, new_node)});
  }
  new_node->$struct.subtypes.clear();
  for (auto subtype : node->$struct.subtypes) {
    new_node->$struct.subtypes.push_back(copy_node(subtype, new_node));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_interface_declaration(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->interface.methods.clear();
  if (node->interface.where_clause) {
    new_node->interface.where_clause = copy_node(node->interface.where_clause.get(), new_node);
  }
  for (auto field : node->interface.methods) {
    new_node->interface.methods.push_back(copy_node(field, new_node));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_range(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->range.left = copy_node(node->range.left, new_node);
  new_node->range.right = copy_node(node->range.right, new_node);
  return new_node;
}

AST *ASTCopier::copy_switch(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->$switch.target = copy_node(node->$switch.target, new_node);
  new_node->$switch.cases.clear();
  for (auto &case_ : node->$switch.cases) {
    new_node->$switch.cases.push_back({copy_node(case_.expression, new_node), copy_node(case_.block, new_node)});
  }
  return new_node;
}

AST *ASTCopier::copy_tuple(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->tuple.clear();
  for (auto value : node->tuple) {
    new_node->tuple.push_back(copy_node(value, new_node));
  }
  return new_node;
}

AST *ASTCopier::copy_tuple_deconstruction(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->tuple_deconstruction.idens.clear();
  for (auto iden : node->tuple_deconstruction.idens) {
    new_node->tuple_deconstruction.idens.push_back(copy_node(iden, new_node));
  }
  new_node->tuple_deconstruction.right = copy_node(node->tuple_deconstruction.right, new_node);
  return new_node;
}

AST *ASTCopier::copy_impl(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->impl.target = copy_node(node->impl.target, new_node);
  if (new_node->impl.interface) {
    new_node->impl.interface = copy_node(new_node->impl.interface.get(), new_node);
  }
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->impl.methods.clear();
  if (node->impl.where_clause) {
    new_node->impl.where_clause = copy_node(node->impl.where_clause.get(), new_node);
  }
  for (const auto &method : node->impl.methods) {
    new_node->impl.methods.push_back(copy_node(method, new_node));
  }
  current_scope = old_scope;
  return new_node;
}

AST *ASTCopier::copy_cast(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->cast.expression = copy_node(node->cast.expression, new_node);
  new_node->cast.target_type = copy_node(node->cast.target_type, new_node);
  return new_node;
}

AST *ASTCopier::copy_where(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->where.predicate = copy_node(node->where.predicate, new_node);
  new_node->where.target_type = copy_node(node->where.target_type, new_node);
  return new_node;
}

AST *ASTCopier::copy_node(AST *node, AST *new_parent) {
  const auto type = node->node_type;
  switch (type) {
    case AST_WHERE:
      return copy_where(node, new_parent);
    case AST_CAST:
      return copy_cast(node, new_parent);
    case AST_IMPL:
      return copy_impl(node, new_parent);
    case AST_PROGRAM:
      return copy_program(node, new_parent);
    case AST_BLOCK:
      return copy_block(node, new_parent);
    case AST_FUNCTION:
      return copy_function_declaration(node, new_parent);
    case AST_DECLARATION:
      return copy_declaration(node, new_parent);
    case AST_BINARY:
      return copy_bin_expr(node, new_parent);
    case AST_UNARY_EXPR:
      return copy_unary_expr(node, new_parent);
    case AST_IDENTIFIER:
      return copy_identifier(node, new_parent);
    case AST_LITERAL:
      return copy_literal(node, new_parent);
    case AST_TYPE:
      return copy_type(node, new_parent);
    case AST_TUPLE:
      return copy_tuple(node, new_parent);
    case AST_CALL:
      return copy_call(node, new_parent);
    case AST_RETURN:
      return copy_return(node, new_parent);
    case AST_CONTINUE:
      return copy_continue(node, new_parent);
    case AST_BREAK:
      return copy_break(node, new_parent);
    case AST_FOR:
      return copy_for(node, new_parent);
    case AST_IF:
      return copy_if(node, new_parent);
    case AST_ELSE:
      return copy_else(node, new_parent);
    case AST_WHILE:
      return copy_while(node, new_parent);
    case AST_STRUCT:
      return copy_struct_declaration(node, new_parent);
    case AST_DOT:
      return copy_dot_expr(node, new_parent);
    case AST_SCOPE_RESOLUTION:
      return copy_scope_resolution(node, new_parent);
    case AST_SUBSCRIPT:
      return copy_subscript(node, new_parent);
    case AST_INITIALIZER:
      return copy_initializer_list(node, new_parent);
    case AST_ENUM:
      return copy_enum_declaration(node, new_parent);
    case AST_RANGE:
      return copy_range(node, new_parent);
    case AST_SWITCH:
      return copy_switch(node, new_parent);
    case AST_TUPLE_DECONSTRUCTION:
      return copy_tuple_deconstruction(node, new_parent);
    case AST_INTERFACE:
      return copy_interface_declaration(node, new_parent);
    case AST_NOOP:
      return node;
    case AST_ALIAS:
      return copy_alias(node, new_parent);
    case AST_SIZE_OF:
      return copy_sizeof(node, new_parent);
    case AST_DEFER:
      return copy_defer(node, new_parent);
    case AST_LAMBDA:
      return copy_lambda(node, new_parent);
    case AST_STATEMENT_LIST:
      return copy_statement_list(node, new_parent);
  }
  return nullptr;
}

AST *deep_copy_ast(AST *root) {
  ASTCopier copier;
  // I don't really know what to do with the parent here.
  return copier.copy_node(root, root->parent);
}

AST *ASTCopier::copy_alias(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->alias.type = copy_node(node->alias.type, new_node);
  return new_node;
}

AST *ASTCopier::copy_sizeof(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->size_of = copy_node(node->size_of, new_node);
  return new_node;
}

AST *ASTCopier::copy_defer(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->defer = copy_node(node->defer, new_node);
  return new_node;
}

AST *ASTCopier::copy_lambda(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->lambda.parameters.clear();
  for (auto param : node->lambda.parameters) {
    new_node->lambda.parameters.push_back(param);
  }
  new_node->lambda.return_type = copy_node(node->lambda.return_type, new_node);
  new_node->lambda.block = copy_node(node->lambda.block, new_node);
  return new_node;
}

AST *ASTCopier::copy_statement_list(AST *node, AST *new_parent) {
  auto new_node = copy(node, new_parent);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    new_node->statements.push_back(copy_node(stmt, new_node));
  }
  return new_node;
}