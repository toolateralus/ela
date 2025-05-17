#include "copier.hpp"
#include "ast.hpp"
#include "type.hpp"

Scope *ASTCopier::copy_scope(Scope *old) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope(*old);
  if (current_scope) {
    scope->parent = current_scope;
  }
  return scope;
}
ASTProgram *ASTCopier::copy_program(ASTProgram *node) {
  auto new_node = copy(node);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    if (stmt->get_node_type() == AST_NODE_NOOP)
      continue;
    new_node->statements.push_back(static_cast<ASTStatement *>(copy_node(stmt)));
  }
  return new_node;
}
ASTBlock *ASTCopier::copy_block(ASTBlock *node) {
  auto new_node = copy(node);
  new_node->statements.clear();
  new_node->scope = copy_scope(new_node->scope);
  new_node->scope->symbols.clear();
  new_node->scope->ordered_symbols.clear();
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (auto stmt : node->statements) {
    new_node->statements.push_back(static_cast<ASTStatement *>(copy_node(stmt)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTFunctionDeclaration *ASTCopier::copy_function_declaration(ASTFunctionDeclaration *node) {
  auto new_node = copy(node);
  new_node->params = static_cast<ASTParamsDecl *>(copy_node(node->params));
  new_node->return_type = static_cast<ASTType *>(copy_node(node->return_type));
  new_node->generic_instantiations.clear();

  auto old_scope = current_scope;
  if (node->scope) {
    new_node->scope = copy_scope(node->scope);
    current_scope = new_node->scope;
  }

  if (node->where_clause) {
    new_node->where_clause = (ASTWhere *)copy_node(node->where_clause.get());
  }

  if (node->block) {
    new_node->block = static_cast<ASTBlock *>(copy_node(node->block.get()));
    node->block.get()->scope->parent = new_node->scope;
  }
  current_scope = old_scope;
  return new_node;
}
ASTParamsDecl *ASTCopier::copy_params_decl(ASTParamsDecl *node) {
  auto new_node = copy(node);
  new_node->params.clear();
  for (auto param : node->params) {
    new_node->params.push_back(static_cast<ASTParamDecl *>(copy_node(param)));
  }
  return new_node;
}

/* 
  ! When passing an argument to a generic function that has a default parameter in that position, we get junk values out and it crashes.
*/
ASTParamDecl *ASTCopier::copy_param_decl(ASTParamDecl *node) {
  auto new_node = copy(node);
  if (new_node->tag == ASTParamDecl::Normal) {
    if (node->normal.default_value.is_not_null()) {
      new_node->normal.default_value = (ASTType *)copy_node(node->normal.default_value.get());
    }
    new_node->normal.type = (ASTType *)(copy_node(node->normal.type));
  } else {
    new_node->self.is_pointer = node->self.is_pointer;
  }
  return new_node;
}

ASTVariable *ASTCopier::copy_variable(ASTVariable *node) {
  auto new_node = copy(node);
  if (node->type)
    new_node->type = static_cast<ASTType *>(copy_node(node->type));
  if (node->value)
    new_node->value = static_cast<ASTExpr *>(copy_node(node->value.get()));
  return new_node;
}
ASTExprStatement *ASTCopier::copy_expr_statement(ASTExprStatement *node) {
  auto new_node = copy(node);
  new_node->expression = static_cast<ASTExpr *>(copy_node(node->expression));
  return new_node;
}
ASTBinExpr *ASTCopier::copy_bin_expr(ASTBinExpr *node) {
  auto new_node = copy(node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTUnaryExpr *ASTCopier::copy_unary_expr(ASTUnaryExpr *node) {
  auto new_node = copy(node);
  new_node->operand = static_cast<ASTExpr *>(copy_node(node->operand));
  return new_node;
}
ASTLiteral *ASTCopier::copy_literal(ASTLiteral *node) {
  auto new_node = copy(node);
  // do anything here?
  return new_node;
}
ASTType *ASTCopier::copy_type(ASTType *node) {
  auto new_node = copy(node);
  new_node->resolved_type = node->resolved_type;

  switch (new_node->kind) {
    case ASTType::SELF:
      break;
    case ASTType::NORMAL:
      if (node->normal.path) {
        new_node->normal.path = (ASTPath *)copy_node(node->normal.path);
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
      new_node->function.parameter_types.clear();
      for (auto param_ty : node->function.parameter_types) {
        new_node->function.parameter_types.push_back(static_cast<ASTType *>(copy_node(param_ty)));
      }
      break;
  }
  return new_node;
}
ASTCall *ASTCopier::copy_call(ASTCall *node) {
  auto new_node = copy(node);
  new_node->callee = static_cast<ASTExpr *>(copy_node(node->callee));
  new_node->arguments = static_cast<ASTArguments *>(copy_node(node->arguments));
  return new_node;
}
ASTArguments *ASTCopier::copy_arguments(ASTArguments *node) {
  auto new_node = copy(node);
  new_node->arguments.clear();
  for (auto arg : node->arguments) {
    new_node->arguments.push_back(static_cast<ASTExpr *>(copy_node(arg)));
  }
  return new_node;
}
ASTReturn *ASTCopier::copy_return(ASTReturn *node) {
  auto new_node = copy(node);
  if (node->expression)
    new_node->expression = static_cast<ASTExpr *>(copy_node(node->expression.get()));
  return new_node;
}
ASTContinue *ASTCopier::copy_continue(ASTContinue *node) { return new (ast_alloc<ASTContinue>()) ASTContinue(*node); }
ASTBreak *ASTCopier::copy_break(ASTBreak *node) { return new (ast_alloc<ASTBreak>()) ASTBreak(*node); }
ASTFor *ASTCopier::copy_for(ASTFor *node) {
  auto new_node = copy(node);
  switch (node->left_tag) {
    case ASTFor::IDENTIFIER: {
      new_node->left.identifier = node->left.identifier;
    } break;
    case ASTFor::DESTRUCTURE: {
      new_node->left.destructure.clear();
      for (auto destructure : node->left.destructure) {
        new_node->left.destructure.push_back({
            .semantic = destructure.semantic,
            .identifier = destructure.identifier,
        });
      }
    } break;
  }
  new_node->right = static_cast<ASTRange *>(copy_node(node->right));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  return new_node;
}
ASTIf *ASTCopier::copy_if(ASTIf *node) {
  auto new_node = copy(node);
  new_node->condition = static_cast<ASTExpr *>(copy_node(node->condition));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  if (node->_else)
    new_node->_else = static_cast<ASTElse *>(copy_node(node->_else.get()));
  return new_node;
}
ASTElse *ASTCopier::copy_else(ASTElse *node) {
  auto new_node = copy(node);
  if (node->_if)
    new_node->_if = static_cast<ASTIf *>(copy_node(node->_if.get()));
  if (node->block)
    new_node->block = static_cast<ASTBlock *>(copy_node(node->block.get()));
  return new_node;
}
ASTWhile *ASTCopier::copy_while(ASTWhile *node) {
  auto new_node = copy(node);
  if (node->condition)
    new_node->condition = static_cast<ASTExpr *>(copy_node(node->condition.get()));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  return new_node;
}

ASTDotExpr *ASTCopier::copy_dot_expr(ASTDotExpr *node) {
  auto new_node = copy(node);
  new_node->base = static_cast<ASTExpr *>(copy_node(node->base));
  new_node->member.generic_arguments.clear();
  for (const auto &arg : node->member.generic_arguments) {
    new_node->member.generic_arguments.push_back((ASTExpr *)copy_node(arg));
  }
  return new_node;
}

ASTIndex *ASTCopier::copy_subscript(ASTIndex *node) {
  auto new_node = copy(node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->index = static_cast<ASTExpr *>(copy_node(node->index));
  return new_node;
}
ASTInitializerList *ASTCopier::copy_initializer_list(ASTInitializerList *node) {
  auto new_node = copy(node);
  new_node->key_values.clear();
  new_node->values.clear();
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
  auto new_node = copy(node);
  new_node->key_values.clear();
  for (auto &kv : node->key_values) {
    new_node->key_values.push_back({kv.first, (ASTExpr *)copy_node(kv.second)});
  }
  return new_node;
}
ASTStructDeclaration *ASTCopier::copy_struct_declaration(ASTStructDeclaration *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->members.clear();
  if (node->where_clause) {
    new_node->where_clause = (ASTWhere *)copy_node(node->where_clause.get());
  }

  for (auto &member : node->members) {
    auto new_member = ASTStructMember{.is_bitfield = member.is_bitfield,
                                      .bitsize = member.bitsize,
                                      .name = member.name,
                                      .type = static_cast<ASTType *>(copy_node(member.type))};

    if (member.default_value) {
      new_member.default_value = (ASTExpr *)copy_node(member.default_value.get());
    }
    new_node->members.push_back(new_member);
  }
  new_node->subtypes.clear();
  for (auto subtype : node->subtypes) {
    new_node->subtypes.push_back(static_cast<ASTStructDeclaration *>(copy_node(subtype)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTTraitDeclaration *ASTCopier::copy_trait_declaration(ASTTraitDeclaration *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->methods.clear();
  if (node->where_clause) {
    new_node->where_clause = (ASTWhere *)copy_node(node->where_clause.get());
  }
  for (auto field : node->methods) {
    new_node->methods.push_back(static_cast<ASTFunctionDeclaration *>(copy_node(field)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTRange *ASTCopier::copy_range(ASTRange *node) {
  auto new_node = copy(node);
  new_node->left = static_cast<ASTExpr *>(copy_node(node->left));
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTSwitch *ASTCopier::copy_switch(ASTSwitch *node) {
  auto new_node = copy(node);
  new_node->target = static_cast<ASTExpr *>(copy_node(node->target));
  new_node->cases.clear();
  for (auto &case_ : node->cases) {
    new_node->cases.push_back(
        {static_cast<ASTExpr *>(copy_node(case_.expression)), static_cast<ASTBlock *>(copy_node(case_.block))});
  }
  return new_node;
}
ASTTuple *ASTCopier::copy_tuple(ASTTuple *node) {
  auto new_node = copy(node);
  new_node->values.clear();
  for (auto value : node->values) {
    new_node->values.push_back(static_cast<ASTExpr *>(copy_node(value)));
  }
  return new_node;
}
ASTDestructure *ASTCopier::copy_destructure(ASTDestructure *node) {
  auto new_node = copy(node);
  new_node->elements.clear();
  for (const auto &destruct : node->elements) {
    DestructureElement new_destruct;
    new_destruct.semantic = destruct.semantic;
    new_destruct.identifier = destruct.identifier;
    new_node->elements.push_back(new_destruct);
  }
  new_node->right = static_cast<ASTExpr *>(copy_node(node->right));
  return new_node;
}
ASTImpl *ASTCopier::copy_impl(ASTImpl *node) {
  auto new_node = copy(node);
  new_node->target = static_cast<ASTType *>(copy_node(node->target));
  if (new_node->trait) {
    new_node->trait = static_cast<ASTType *>(copy_node(new_node->trait.get()));
  }

  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;

  if (node->where_clause) {
    new_node->where_clause = (ASTWhere *)copy_node(node->where_clause.get());
  }

  new_node->aliases.clear();
  for (const auto &alias : node->aliases) {
    new_node->aliases.push_back(copy_alias(alias));
  }

  new_node->constants.clear();
  for (const auto &constant : node->constants) {
    new_node->constants.push_back(copy_variable(constant));
  }

  new_node->methods.clear();
  for (const auto &method : node->methods) {
    new_node->methods.push_back(static_cast<ASTFunctionDeclaration *>(copy_node(method)));
  }
  current_scope = old_scope;
  return new_node;
}
ASTCast *ASTCopier::copy_cast(ASTCast *node) {
  auto new_node = copy(node);
  new_node->expression = (ASTExpr *)copy_node(node->expression);
  new_node->target_type = (ASTType *)copy_node(node->target_type);
  return new_node;
}
ASTWhere *ASTCopier::copy_where(ASTWhere *node) {
  auto new_node = copy(node);
  new_node->constraints.clear();
  for (const auto &[target, predicate] : node->constraints) {
    new_node->constraints.push_back({(ASTType *)copy_node(target), (ASTExpr *)copy_node(predicate)});
  }
  return new_node;
}
ASTNode *ASTCopier::copy_node(ASTNode *node) {
  const auto type = node->get_node_type();
  switch (type) {
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
    case AST_NODE_VARIABLE:
      return copy_variable(static_cast<ASTVariable *>(node));
    case AST_NODE_EXPR_STATEMENT:
      return copy_expr_statement(static_cast<ASTExprStatement *>(node));
    case AST_NODE_BIN_EXPR:
      return copy_bin_expr(static_cast<ASTBinExpr *>(node));
    case AST_NODE_UNARY_EXPR:
      return copy_unary_expr(static_cast<ASTUnaryExpr *>(node));
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
    case AST_NODE_INDEX:
      return copy_subscript(static_cast<ASTIndex *>(node));
    case AST_NODE_INITIALIZER_LIST:
      return copy_initializer_list(static_cast<ASTInitializerList *>(node));
    case AST_NODE_ENUM_DECLARATION:
      return copy_enum_declaration(static_cast<ASTEnumDeclaration *>(node));
    case AST_NODE_RANGE:
      return copy_range(static_cast<ASTRange *>(node));
    case AST_NODE_SWITCH:
      return copy_switch(static_cast<ASTSwitch *>(node));
    case AST_NODE_TUPLE_DECONSTRUCTION:
      return copy_destructure(static_cast<ASTDestructure *>(node));
    case AST_NODE_TRAIT_DECLARATION:
      return copy_trait_declaration(static_cast<ASTTraitDeclaration *>(node));
    case AST_NODE_CHOICE_DECLARATION:
      return copy_choice_declaration(static_cast<ASTChoiceDeclaration *>(node));
    case AST_NODE_NOOP:
      return node;
    case AST_NODE_ALIAS:
      return copy_alias(static_cast<ASTAlias *>(node));
    case AST_NODE_SIZE_OF:
      return copy_sizeof(static_cast<ASTSize_Of *>(node));
    case AST_NODE_DEFER:
      return copy_defer(static_cast<ASTDefer *>(node));
    case AST_NODE_LAMBDA:
      return copy_lambda(static_cast<ASTLambda *>(node));
    case AST_NODE_STATEMENT_LIST:
      return copy_statement_list(static_cast<ASTStatementList *>(node));
    case AST_NODE_TYPE_OF:
      return copy_type_of(static_cast<ASTType_Of *>(node));
    case AST_NODE_IMPORT:
      return copy_import(static_cast<ASTImport *>(node));
    case AST_NODE_MODULE:
      return copy_module(static_cast<ASTModule *>(node));
    case AST_NODE_DYN_OF:
      return copy_dyn_of(static_cast<ASTDyn_Of *>(node));
    case AST_NODE_PATTERN_MATCH:
      return copy_pattern_match(static_cast<ASTPatternMatch *>(node));
    case AST_NODE_PATH:
      return copy_path(static_cast<ASTPath *>(node));
    case AST_NODE_METHOD_CALL:
      return copy_method_call(static_cast<ASTMethodCall *>(node));
      break;
  }
}
ASTType_Of *ASTCopier::copy_type_of(ASTType_Of *node) {
  auto new_node = copy(node);
  new_node->target = (ASTExpr *)copy_node((ASTNode *)node->target);
  return new_node;
}
ASTNode *deep_copy_ast(ASTNode *root) {
  ASTCopier copier;
  return copier.copy_node(root);
}
ASTAlias *ASTCopier::copy_alias(ASTAlias *node) {
  auto new_node = copy(node);
  new_node->source_node = static_cast<ASTType *>(copy_node(node->source_node));
  return new_node;
}
ASTSize_Of *ASTCopier::copy_sizeof(ASTSize_Of *node) {
  auto new_node = copy(node);
  new_node->target_type = static_cast<ASTType *>(copy_node(node->target_type));
  return new_node;
}
ASTDefer *ASTCopier::copy_defer(ASTDefer *node) {
  auto new_node = copy(node);
  new_node->statement = copy_node(node->statement);
  return new_node;
}
ASTLambda *ASTCopier::copy_lambda(ASTLambda *node) {
  auto new_node = copy(node);
  new_node->resolved_type = nullptr;
  new_node->params = static_cast<ASTParamsDecl *>(copy_node(node->params));
  new_node->return_type = static_cast<ASTType *>(copy_node(node->return_type));
  new_node->block = static_cast<ASTBlock *>(copy_node(node->block));
  return new_node;
}
ASTChoiceDeclaration *ASTCopier::copy_choice_declaration(ASTChoiceDeclaration *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(new_node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  new_node->variants.clear();
  for (const auto &variant : node->variants) {
    ASTChoiceVariant new_variant;
    new_variant.kind = variant.kind;
    new_variant.name = variant.name;
    switch (variant.kind) {
      case ASTChoiceVariant::NORMAL:
        break;
      case ASTChoiceVariant::TUPLE:
        new_variant.tuple = static_cast<ASTType *>(copy_node(variant.tuple));
        break;
      case ASTChoiceVariant::STRUCT:
        new_variant.struct_declarations.clear();
        for (auto field : variant.struct_declarations) {
          new_variant.struct_declarations.push_back(static_cast<ASTVariable *>(copy_node(field)));
        }
        break;
    }
    new_node->variants.push_back(std::move(new_variant));
  }
  current_scope = old_scope;
  return new_node;
}
ASTStatementList *ASTCopier::copy_statement_list(ASTStatementList *node) {
  auto new_node = copy(node);
  new_node->statements.clear();
  for (auto stmt : node->statements) {
    new_node->statements.push_back(copy_node(stmt));
  }
  return new_node;
}
ASTImport *ASTCopier::copy_import(ASTImport *node) {
  auto new_node = copy(node);
  new_node->statements.clear();
  new_node->scope = copy_scope(node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (const auto &statement : node->statements) {
    new_node->statements.push_back((ASTStatement *)copy_node(statement));
  }
  current_scope = old_scope;
  return new_node;
}
ASTModule *ASTCopier::copy_module(ASTModule *node) {
  auto new_node = copy(node);
  new_node->scope = copy_scope(node->scope);
  auto old_scope = current_scope;
  current_scope = new_node->scope;
  for (const auto &statement : node->statements) {
    new_node->statements.push_back((ASTStatement *)copy_node(statement));
  }
  current_scope = old_scope;
  return new_node;
}
ASTDyn_Of *ASTCopier::copy_dyn_of(ASTDyn_Of *node) {
  auto new_node = copy(node);
  new_node->object = (ASTExpr *)copy_node(node->object);
  return new_node;
}
ASTPatternMatch *ASTCopier::copy_pattern_match(ASTPatternMatch *node) {
  auto new_node = copy(node);

  new_node->scope = copy_scope(node->scope);
  auto old_scope = current_scope;
  current_scope = node->scope;

  new_node->object = (ASTExpr *)copy_node(node->object);
  new_node->target_type_path = (ASTPath *)copy_node(node->target_type_path);

  current_scope = old_scope;
  return new_node;
}
ASTPath *ASTCopier::copy_path(ASTPath *node) {
  auto new_node = copy(node);
  new_node->segments.clear();
  for (const auto &part : node->segments) {
    if (!part.generic_arguments.empty()) {
      std::vector<ASTExpr *> args;
      for (auto arg : part.generic_arguments) {
        auto new_arg = (ASTExpr *)copy_node(arg);
        new_arg->resolved_type = Type::INVALID_TYPE;
        args.push_back(new_arg);
      }
      new_node->push_segment(part.identifier, args);
    } else {
      new_node->push_segment(part.identifier, {});
    }
  }
  return new_node;
}
ASTMethodCall *ASTCopier::copy_method_call(ASTMethodCall *node) {
  auto new_node = copy(node);
  new_node->callee = (ASTDotExpr *)copy_node(node->callee);
  new_node->arguments = (ASTArguments *)copy_node(node->arguments);
  return new_node;
}