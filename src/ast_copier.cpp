#include "ast_copier.hpp"
#include "ast.hpp"

Scope *ASTCopier::copy_scope(Scope *old) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope(*old);
  if (current_scope) {
    scope->parent = current_scope;
  }
  return scope;
}
ASTProgram *ASTCopier::copy_program(ASTProgram *node) {
  auto new_node = new (ast_alloc<ASTProgram>()) ASTProgram(*node);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    if (stmt->get_node_type() == AST_NODE_NOOP)
      continue;
    new_node->statements.push_back(static_cast<ASTStatement *>(copy_node(stmt)));
  }
  return new_node;
}
ASTBlock *ASTCopier::copy_block(ASTBlock *node) {
  auto new_node = new (ast_alloc<ASTBlock>()) ASTBlock(*node);
  new_node->statements.clear();
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (auto stmt : node->statements) {
    new_node->statements.push_back(static_cast<ASTStatement *>(copy_node(stmt)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTFunctionDeclaration *ASTCopier::copy_function_declaration(ASTFunctionDeclaration *node) {
  auto new_node = new (ast_alloc<ASTFunctionDeclaration>()) ASTFunctionDeclaration(*node);
  new_node->params = static_cast<ASTParamsDecl *>(copy_node(node->params));
  new_node->return_type = static_cast<ASTType *>(copy_node(node->return_type));
  new_node->generic_instantiations.clear();

  auto old_scope = current_scope;
  if (node->scope) {
    new_node->scope = copy_scope(node->scope);
    current_scope = new_node->scope;
  }

  if (node->where_clause) {
    new_node->where_clause = (ASTWhere*)copy_node(node->where_clause.get());
  }

  if (node->block) {
    new_node->block = static_cast<ASTBlock *>(copy_node(node->block.get()));
    node->block.get()->scope->parent = new_node->scope;
  }
  current_scope = old_scope;
  return new_node;
}
ASTParamsDecl *ASTCopier::copy_params_decl(ASTParamsDecl *node) {
  auto new_node = new (ast_alloc<ASTParamsDecl>()) ASTParamsDecl(*node);
  new_node->params.clear();
  for (auto param : node->params) {
    new_node->params.push_back(static_cast<ASTParamDecl *>(copy_node(param)));
  }
  return new_node;
}
ASTParamDecl *ASTCopier::copy_param_decl(ASTParamDecl *node) {
  auto new_node = new (ast_alloc<ASTParamDecl>()) ASTParamDecl(*node);
  if (new_node->tag == ASTParamDecl::Normal) {
    new_node->normal.type = static_cast<ASTType *>(copy_node(node->normal.type));
    if (node->normal.default_value)
      new_node->normal.default_value = static_cast<ASTExpr *>(copy_node(node->normal.default_value.get()));
  } else {
    new_node->self.is_pointer = node->self.is_pointer;
  }
  return new_node;
}
ASTDeclaration *ASTCopier::copy_declaration(ASTDeclaration *node) {
  auto new_node = new (ast_alloc<ASTDeclaration>()) ASTDeclaration(*node);
  if (node->type)
    new_node->type = static_cast<ASTType *>(copy_node(node->type));
  if (node->value)
    new_node->value = static_cast<ASTExpr *>(copy_node(node->value.get()));
  return new_node;
}
ASTExprStatement *ASTCopier::copy_expr_statement(ASTExprStatement *node) {
  auto new_node = new (ast_alloc<ASTExprStatement>()) ASTExprStatement(*node);
  new_node->expression = static_cast<ASTExpr *>(copy_node(node->expression));
  return new_node;
}
ASTBinExpr *ASTCopier::copy_bin_expr(ASTBinExpr *node) {
  auto new_node = new (ast_alloc<ASTBinExpr>()) ASTBinExpr(*node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTUnaryExpr *ASTCopier::copy_unary_expr(ASTUnaryExpr *node) {
  auto new_node = new (ast_alloc<ASTUnaryExpr>()) ASTUnaryExpr(*node);
  new_node->operand = static_cast<ASTExpr *>(copy_node(node->operand));
  return new_node;
}
ASTIdentifier *ASTCopier::copy_identifier(ASTIdentifier *node) {
  return new (ast_alloc<ASTIdentifier>()) ASTIdentifier(*node);
}

InterpolatedStringSegment *ASTCopier::copy_interp_string_segment(InterpolatedStringSegment *segment) {
  if (!segment) {
    return nullptr;
  }
  auto new_segment = new InterpolatedStringSegment(*segment);
  if (segment->expression) {
    new_segment->expression = static_cast<ASTExpr *>(copy_node(segment->expression));
  }
  new_segment->next = copy_interp_string_segment(segment->next);
  return new_segment;
}

ASTLiteral *ASTCopier::copy_literal(ASTLiteral *node) {
  auto new_node = new (ast_alloc<ASTLiteral>()) ASTLiteral(*node);

  if (node->tag == ASTLiteral::InterpolatedString) {
    new_node->interpolated_string_root = copy_interp_string_segment(node->interpolated_string_root);
  }

  return new_node;
}
ASTType *ASTCopier::copy_type(ASTType *node) {
  auto new_node = new (ast_alloc<ASTType>()) ASTType(*node);
  new_node->resolved_type = Type::invalid_id;
  if (node->pointing_to)
    new_node->pointing_to = static_cast<ASTType *>(copy_node(node->pointing_to.get()));
  switch (new_node->kind) {
    case ASTType::NORMAL:
    case ASTType::SELF:
    case ASTType::REFLECTION:
      new_node->normal.generic_arguments.clear();
      for (auto arg : node->normal.generic_arguments) {
        new_node->normal.generic_arguments.push_back(static_cast<ASTType *>(copy_node(arg)));
      }
      break;
    case ASTType::TUPLE:
      new_node->tuple_types.clear();
      for (auto type : node->tuple_types) {
        new_node->tuple_types.push_back(static_cast<ASTType *>(copy_node(type)));
      }
      break;
    case ASTType::FUNCTION:
      if (node->function.return_type) {
        new_node->function.return_type = static_cast<ASTType *>(copy_node(node->function.return_type.get()));
      }
      for (auto param_ty : node->function.parameter_types) {
        new_node->function.parameter_types.push_back(static_cast<ASTType *>(copy_node(param_ty)));
      }
      break;
  }
  return new_node;
}
ASTCall *ASTCopier::copy_call(ASTCall *node) {
  auto new_node = new (ast_alloc<ASTCall>()) ASTCall(*node);
  new_node->function = static_cast<ASTExpr *>(copy_node(node->function));
  new_node->arguments = static_cast<ASTArguments *>(copy_node(node->arguments));
  new_node->generic_arguments.clear();
  for (auto arg : node->generic_arguments) {
    new_node->generic_arguments.push_back(static_cast<ASTType *>(copy_node(arg)));
  }
  return new_node;
}
ASTArguments *ASTCopier::copy_arguments(ASTArguments *node) {
  auto new_node = new (ast_alloc<ASTArguments>()) ASTArguments(*node);
  new_node->arguments.clear();
  for (auto arg : node->arguments) {
    new_node->arguments.push_back(static_cast<ASTExpr *>(copy_node(arg)));
  }
  return new_node;
}
ASTReturn *ASTCopier::copy_return(ASTReturn *node) {
  auto new_node = new (ast_alloc<ASTReturn>()) ASTReturn(*node);
  if (node->expression)
    new_node->expression = static_cast<ASTExpr *>(copy_node(node->expression.get()));
  return new_node;
}
ASTContinue *ASTCopier::copy_continue(ASTContinue *node) { return new (ast_alloc<ASTContinue>()) ASTContinue(*node); }
ASTBreak *ASTCopier::copy_break(ASTBreak *node) { return new (ast_alloc<ASTBreak>()) ASTBreak(*node); }
ASTFor *ASTCopier::copy_for(ASTFor *node) {
  auto new_node = new (ast_alloc<ASTFor>()) ASTFor(*node);
  new_node->iden = static_cast<ASTIdentifier *>(copy_node(node->iden));
  new_node->range = static_cast<ASTRange *>(copy_node(node->range));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  return new_node;
}
ASTIf *ASTCopier::copy_if(ASTIf *node) {
  auto new_node = new (ast_alloc<ASTIf>()) ASTIf(*node);
  new_node->condition = static_cast<ASTExpr *>(copy_node(node->condition));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  if (node->_else)
    new_node->_else = static_cast<ASTElse *>(copy_node(node->_else.get()));
  return new_node;
}
ASTElse *ASTCopier::copy_else(ASTElse *node) {
  auto new_node = new (ast_alloc<ASTElse>()) ASTElse(*node);
  if (node->_if)
    new_node->_if = static_cast<ASTIf *>(copy_node(node->_if.get()));
  if (node->block)
    new_node->block = static_cast<ASTBlock *>(copy_node(node->block.get()));
  return new_node;
}
ASTWhile *ASTCopier::copy_while(ASTWhile *node) {
  auto new_node = new (ast_alloc<ASTWhile>()) ASTWhile(*node);
  if (node->condition)
    new_node->condition = static_cast<ASTExpr *>(copy_node(node->condition.get()));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  return new_node;
}

ASTDotExpr *ASTCopier::copy_dot_expr(ASTDotExpr *node) {
  auto new_node = new (ast_alloc<ASTDotExpr>()) ASTDotExpr(*node);
  new_node->base = static_cast<ASTExpr *>(copy_node(node->base));
  return new_node;
}
ASTSubscript *ASTCopier::copy_subscript(ASTSubscript *node) {
  auto new_node = new (ast_alloc<ASTSubscript>()) ASTSubscript(*node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->subscript = static_cast<ASTExpr *>(copy_node(node->subscript));
  return new_node;
}

ASTInitializerList *ASTCopier::copy_initializer_list(ASTInitializerList *node) {
  auto new_node = new (ast_alloc<ASTInitializerList>()) ASTInitializerList(*node);
  new_node->key_values.clear();
  if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    for (auto expr : node->values) {
      new_node->values.push_back(static_cast<ASTExpr *>(copy_node(expr)));
    }
  } else {
    for (auto [id, expr] : node->key_values) {
      new_node->key_values.push_back({id, static_cast<ASTExpr *>(copy_node(expr))});
    }
  }
  return new_node;
}
ASTEnumDeclaration *ASTCopier::copy_enum_declaration(ASTEnumDeclaration *node) {
  auto new_node = new (ast_alloc<ASTEnumDeclaration>()) ASTEnumDeclaration(*node);
  new_node->key_values.clear();
  for (auto &kv : node->key_values) {
    new_node->key_values.push_back({kv.first, (ASTExpr *)copy_node(kv.second)});
  }
  return new_node;
}

ASTStructDeclaration *ASTCopier::copy_struct_declaration(ASTStructDeclaration *node) {
  auto new_node = new (ast_alloc<ASTStructDeclaration>()) ASTStructDeclaration(*node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->fields.clear();
  if (node->where_clause) {
    new_node->where_clause = (ASTWhere*)copy_node(node->where_clause.get());
  }
  for (auto field : node->fields) {
    new_node->fields.push_back(static_cast<ASTDeclaration *>(copy_node(field)));
  }
  new_node->subtypes.clear();
  for (auto subtype : node->subtypes) {
    new_node->subtypes.push_back(static_cast<ASTStructDeclaration *>(copy_node(subtype)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTInterfaceDeclaration *ASTCopier::copy_interface_declaration(ASTInterfaceDeclaration *node) {
  auto new_node = new (ast_alloc<ASTInterfaceDeclaration>()) ASTInterfaceDeclaration(*node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->methods.clear();
  if (node->where_clause) {
    new_node->where_clause = (ASTWhere*)copy_node(node->where_clause.get());
  }
  for (auto field : node->methods) {
    new_node->methods.push_back(static_cast<ASTFunctionDeclaration *>(copy_node(field)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTRange *ASTCopier::copy_range(ASTRange *node) {
  auto new_node = new (ast_alloc<ASTRange>()) ASTRange(*node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTSwitch *ASTCopier::copy_switch(ASTSwitch *node) {
  auto new_node = new (ast_alloc<ASTSwitch>()) ASTSwitch(*node);
  new_node->target = static_cast<ASTExpr *>(copy_node(node->target));
  new_node->cases.clear();
  for (auto &case_ : node->cases) {
    new_node->cases.push_back(
        {static_cast<ASTExpr *>(copy_node(case_.expression)), static_cast<ASTBlock *>(copy_node(case_.block))});
  }
  return new_node;
}
ASTTuple *ASTCopier::copy_tuple(ASTTuple *node) {
  auto new_node = new (ast_alloc<ASTTuple>()) ASTTuple(*node);
  new_node->type = static_cast<ASTType *>(copy_node(node->type));
  new_node->values.clear();
  for (auto value : node->values) {
    new_node->values.push_back(static_cast<ASTExpr *>(copy_node(value)));
  }
  return new_node;
}
ASTTupleDeconstruction *ASTCopier::copy_tuple_deconstruction(ASTTupleDeconstruction *node) {
  auto new_node = new (ast_alloc<ASTTupleDeconstruction>()) ASTTupleDeconstruction(*node);
  new_node->idens.clear();
  for (auto iden : node->idens) {
    new_node->idens.push_back(static_cast<ASTIdentifier *>(copy_node(iden)));
  }
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTScopeResolution *ASTCopier::copy_scope_resolution(ASTScopeResolution *node) {
  auto new_node = new (ast_alloc<ASTScopeResolution>()) ASTScopeResolution(*node);
  new_node->base = static_cast<ASTExpr *>(copy_node(node->base));
  return new_node;
}
ASTImpl *ASTCopier::copy_impl(ASTImpl *node) {
  auto new_node = new (ast_alloc<ASTImpl>()) ASTImpl(*node);
  new_node->target = static_cast<ASTType *>(copy_node(node->target));
  if (new_node->interface) {
    new_node->interface = static_cast<ASTType *>(copy_node(new_node->interface.get()));
  }
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->methods.clear();
  if (node->where_clause) {
    new_node->where_clause = (ASTWhere*)copy_node(node->where_clause.get());
  }
  for (const auto &method : node->methods) {
    new_node->methods.push_back(static_cast<ASTFunctionDeclaration *>(copy_node(method)));
  }
  current_scope = old_scope;
  return new_node;
}

ASTCast *ASTCopier::copy_cast(ASTCast *node) {
  auto new_node = new (ast_alloc<ASTCast>()) ASTCast(*node);
  new_node->resolved_type = -1;
  new_node->expression = (ASTExpr *)copy_node(node->expression);
  new_node->target_type = (ASTType *)copy_node(node->target_type);
  return new_node;
}

ASTWhere *ASTCopier::copy_where(ASTWhere *node) {
  auto new_node = new (ast_alloc<ASTWhere>()) ASTWhere(*node);
  new_node->resolved_type = -1;
  new_node->predicate = static_cast<ASTExpr *>(copy_node(node->predicate));
  new_node->target_type = static_cast<ASTType *>(copy_node(node->target_type));
  return new_node;
}

ASTNode *ASTCopier::copy_node(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_WHERE:
      return copy_where(static_cast<ASTWhere *>(node));
    case AST_NODE_CAST:
      return copy_cast(static_cast<ASTCast *>(node));
    case AST_NODE_IMPL:
      return copy_impl(static_cast<ASTImpl *>(node));
    case AST_NODE_PROGRAM:
      return copy_program(static_cast<ASTProgram *>(node));
    case AST_NODE_BLOCK:
      return copy_block(static_cast<ASTBlock *>(node));
    case AST_NODE_FUNCTION_DECLARATION:
      return copy_function_declaration(static_cast<ASTFunctionDeclaration *>(node));
    case AST_NODE_PARAMS_DECL:
      return copy_params_decl(static_cast<ASTParamsDecl *>(node));
    case AST_NODE_PARAM_DECL:
      return copy_param_decl(static_cast<ASTParamDecl *>(node));
    case AST_NODE_DECLARATION:
      return copy_declaration(static_cast<ASTDeclaration *>(node));
    case AST_NODE_EXPR_STATEMENT:
      return copy_expr_statement(static_cast<ASTExprStatement *>(node));
    case AST_NODE_BIN_EXPR:
      return copy_bin_expr(static_cast<ASTBinExpr *>(node));
    case AST_NODE_UNARY_EXPR:
      return copy_unary_expr(static_cast<ASTUnaryExpr *>(node));
    case AST_NODE_IDENTIFIER:
      return copy_identifier(static_cast<ASTIdentifier *>(node));
    case AST_NODE_LITERAL:
      return copy_literal(static_cast<ASTLiteral *>(node));
    case AST_NODE_TYPE:
      return copy_type(static_cast<ASTType *>(node));
    case AST_NODE_TUPLE:
      return copy_tuple(static_cast<ASTTuple *>(node));
    case AST_NODE_CALL:
      return copy_call(static_cast<ASTCall *>(node));
    case AST_NODE_ARGUMENTS:
      return copy_arguments(static_cast<ASTArguments *>(node));
    case AST_NODE_RETURN:
      return copy_return(static_cast<ASTReturn *>(node));
    case AST_NODE_CONTINUE:
      return copy_continue(static_cast<ASTContinue *>(node));
    case AST_NODE_BREAK:
      return copy_break(static_cast<ASTBreak *>(node));
    case AST_NODE_FOR:
      return copy_for(static_cast<ASTFor *>(node));
    case AST_NODE_IF:
      return copy_if(static_cast<ASTIf *>(node));
    case AST_NODE_ELSE:
      return copy_else(static_cast<ASTElse *>(node));
    case AST_NODE_WHILE:
      return copy_while(static_cast<ASTWhile *>(node));
    case AST_NODE_STRUCT_DECLARATION:
      return copy_struct_declaration(static_cast<ASTStructDeclaration *>(node));
    case AST_NODE_DOT_EXPR:
      return copy_dot_expr(static_cast<ASTDotExpr *>(node));
    case AST_NODE_SCOPE_RESOLUTION:
      return copy_scope_resolution(static_cast<ASTScopeResolution *>(node));
    case AST_NODE_SUBSCRIPT:
      return copy_subscript(static_cast<ASTSubscript *>(node));
    case AST_NODE_INITIALIZER_LIST:
      return copy_initializer_list(static_cast<ASTInitializerList *>(node));
    case AST_NODE_ENUM_DECLARATION:
      return copy_enum_declaration(static_cast<ASTEnumDeclaration *>(node));
    case AST_NODE_RANGE:
      return copy_range(static_cast<ASTRange *>(node));
    case AST_NODE_SWITCH:
      return copy_switch(static_cast<ASTSwitch *>(node));
    case AST_NODE_TUPLE_DECONSTRUCTION:
      return copy_tuple_deconstruction(static_cast<ASTTupleDeconstruction *>(node));
    case AST_NODE_INTERFACE_DECLARATION:
      return copy_interface_declaration(static_cast<ASTInterfaceDeclaration *>(node));
    default:
      return nullptr;
  }
}

ASTNode *deep_copy_ast(ASTNode *root) {
  ASTCopier copier;
  return copier.copy_node(root);
}
