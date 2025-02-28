#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"

Nullable<Symbol> DependencyEmitter::get_symbol(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_SUBSCRIPT:
      return nullptr;
    case AST_NODE_TYPE: {
      auto type_node = static_cast<ASTType *>(node);
      if (type_node->kind != ASTType::NORMAL) {
        return nullptr;
      }
      type_node->accept(this);
      return get_symbol(type_node->normal.base);
    }
    case AST_NODE_IDENTIFIER:
      return ctx.scope->lookup(static_cast<ASTIdentifier *>(node)->value);
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      dotnode->base->accept(this);
      auto type = global_get_type(dotnode->base->resolved_type);
      auto symbol = type->get_info()->scope->local_lookup(dotnode->member_name);
      // Implicit dereference, we look at the base scope.
      if (!symbol && type->get_ext().is_pointer()) {
        type = global_get_type(type->get_element_type());
        symbol = type->get_info()->scope->local_lookup(dotnode->member_name);
      }
      return symbol;
    } break;
    case AST_NODE_SCOPE_RESOLUTION: {
      auto srnode = static_cast<ASTScopeResolution *>(node);
      srnode->base->accept(this);
      auto type = global_get_type(srnode->base->resolved_type);
      auto scope = type->get_info()->scope;
      return scope->local_lookup(srnode->member_name);
    } break;

    default:
      return nullptr; // TODO: verify this isn't strange.
  }
  return nullptr;
}

void DependencyEmitter::decl_type(int type_id) {
  auto type = global_get_type(type_id);
  auto extensions = type->get_ext().extensions;
  for (auto ext : extensions) {
    if (ext.type == TYPE_EXT_POINTER) {
      emitter->forward_decl_type(type);
      return;
    }
  }
  return define_type(type_id);
}

void DependencyEmitter::define_type(int type_id) {
  auto type = global_get_type(type_id);
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->get_info()->as<FunctionTypeInfo>();
      decl_type(info->return_type);
      for (int index = 0; index < info->params_len; index++) {
        auto param_ty = info->parameter_types[index];
        decl_type(param_ty);
      }
    } break;
    case TYPE_TAGGED_UNION:
    case TYPE_STRUCT: {
      if (type->declaring_node.is_not_null()) {
        type->declaring_node.get()->accept(this);
        type->declaring_node.get()->accept(emitter);
      }
    } break;
    case TYPE_TUPLE: {
      auto info = type->get_info()->as<TupleTypeInfo>();
      for (auto type : info->types) {
        define_type(type);
      }
      emitter->emit_tuple(type_id);
    } break;
    case TYPE_ENUM:
      // TODO: enums should be handled here
    case TYPE_INTERFACE:
    case TYPE_SCALAR:
      break;
  }
}

void DependencyEmitter::visit(ASTStructDeclaration *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  if (!node->generic_parameters.empty()) {
    return;
  }
  for (auto subtype : node->subtypes) {
    subtype->accept(this);
  }
  for (auto member : node->members) {
    decl_type(member.type->resolved_type);
  }
}

void DependencyEmitter::visit(ASTProgram *node) {
  for (auto statement : node->statements) {
    statement->accept(this);
  }
}

void DependencyEmitter::visit(ASTBlock *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  for (auto statement : node->statements) {
    statement->accept(this);
  }
}

void DependencyEmitter::visit(ASTFunctionDeclaration *node) {
  if (visited_functions.contains(node)) {
    return;
  } else {
    visited_functions.insert(node);
  }
  if (!node->generic_parameters.empty()) {
    return;
  }
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  decl_type(node->return_type->resolved_type);
  node->params->accept(this);
  if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTParamsDecl *node) {
  for (auto param : node->params) {
    param->accept(this);
  }
}

void DependencyEmitter::visit(ASTParamDecl *node) {
  decl_type(node->resolved_type);
}

void DependencyEmitter::visit(ASTDeclaration *node) {
  node->type->accept(this);
  if (node->value) {
    node->value.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTExprStatement *node) { node->expression->accept(this); }

void DependencyEmitter::visit(ASTBinExpr *node) {
  if (node->is_operator_overload) {
    auto call = ASTCall{};
    auto dot = ASTDotExpr{};
    dot.base = node->left;
    dot.member_name = get_operator_overload_name(node->op.type, OPERATION_BINARY);
    call.function = &dot;
    auto args = ASTArguments{};
    if (node->right) {
      args.arguments = {node->right};
    }
    call.arguments = &args;
    call.accept(this);
  } else {
    node->left->accept(this);
    node->right->accept(this);
  }
}

void DependencyEmitter::visit(ASTUnaryExpr *node) {
  if (node->is_operator_overload) {
    auto call = ASTCall{};
    auto dot = ASTDotExpr{};
    dot.base = node->operand;
    dot.member_name = get_operator_overload_name(node->op.type, OPERATION_UNARY);
    call.function = &dot;
    auto args = ASTArguments{};
    call.arguments = &args;
    call.accept(this);
  } else {
    if (node->op.type == TType::Mul) {
      define_type(node->operand->resolved_type);
    }
    node->operand->accept(this);
  }
}

void DependencyEmitter::visit(ASTIdentifier *node) {
  // TODO: this should be handled by ASTType
  auto type = global_get_type(node->resolved_type);
  if (type && type->kind == TYPE_ENUM) {
    type->declaring_node.get()->accept(this);
    type->declaring_node.get()->accept(emitter);
  }
  // for global variables
  if (auto symbol = ctx.scope->lookup(node->value)) {
    if (symbol->is_variable() && symbol->variable.declaration) {
      auto decl = symbol->variable.declaration.get();
      if (!decl->declaring_block) {
        symbol->variable.declaration.get()->accept(this);
        symbol->variable.declaration.get()->accept(emitter);
      }
    } else if (symbol->is_function()) {
      // TODO: we should change how template retrival works;
      symbol->function.declaration->accept(this);
      symbol->function.declaration->accept(emitter);
    }
  }
}

void DependencyEmitter::visit(ASTLiteral *node) {}

void DependencyEmitter::visit(ASTType *node) {
  decl_type(node->resolved_type);
}

void DependencyEmitter::visit(ASTType_Of *node) {
  node->target->accept(this);
}

void DependencyEmitter::visit(ASTCall *node) {
  for (auto generic_arg : node->generic_arguments) {
    generic_arg->accept(this);
  }

  node->arguments->accept(this);
  auto symbol_nullable = get_symbol(node->function);

  if (symbol_nullable.is_not_null()) {
    auto decl = symbol_nullable.get()->function.declaration;

    if (!node->generic_arguments.empty()) {
      auto generic_args = emitter->typer.get_generic_arg_types(node->generic_arguments);
      decl = find_generic_instance(decl->generic_instantiations, generic_args);
    }
    // only accept if not a fn ptr
    // else, we do the other way
    if (decl) {
      decl->accept(this);
      decl->accept(emitter);
      return;
    }
  } 
  
  node->function->accept(this);
}

void DependencyEmitter::visit(ASTArguments *node) {
  for (auto arg : node->arguments) {
    arg->accept(this);
  }
}

void DependencyEmitter::visit(ASTReturn *node) {
  if (node->expression.is_not_null()) {
    node->expression.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTContinue *node) {}

void DependencyEmitter::visit(ASTBreak *node) {}

void DependencyEmitter::visit(ASTFor *node) {
  define_type(node->iterable_type);
  define_type(node->range_type);
  define_type(node->identifier_type);

  node->range->accept(this);

  auto range_scope = global_get_type(node->range_type)->get_info()->scope;
  auto iter_scope = global_get_type(node->iterable_type)->get_info()->scope;

  switch (node->iteration_kind) {
    case ASTFor::ITERABLE: {
      auto iter_sym = range_scope->local_lookup("iter");
      iter_sym->function.declaration->accept(this);
      iter_sym->function.declaration->accept(emitter);
    } break;
    case ASTFor::ENUMERABLE: {
      auto enum_sym = range_scope->local_lookup("enumerator");
      enum_sym->function.declaration->accept(this);
      enum_sym->function.declaration->accept(emitter);
    } break;
    case ASTFor::ENUMERATOR:
    case ASTFor::ITERATOR:
      break;
  }

  auto done_sym = iter_scope->local_lookup("done");
  done_sym->function.declaration->accept(this);
  done_sym->function.declaration->accept(emitter);

  auto current_sym = iter_scope->local_lookup("current");
  current_sym->function.declaration->accept(this);
  current_sym->function.declaration->accept(emitter);

  auto next_sym = iter_scope->local_lookup("next");
  next_sym->function.declaration->accept(this);
  next_sym->function.declaration->accept(emitter);

  node->block->accept(this);
}

void DependencyEmitter::visit(ASTIf *node) {
  node->condition->accept(this);
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTElse *node) {
  if (node->_if) {
    node->_if.get()->accept(this);
  }
  if (node->block) {
    node->block.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTWhile *node) {
  if (node->condition) {
    node->condition.get()->accept(this);
  }
  node->block->accept(this);
}

void DependencyEmitter::visit(ASTDotExpr *node) {
  define_type(node->base->resolved_type);
  node->base->accept(this);
}

void DependencyEmitter::visit(ASTSubscript *node) {
  if (node->is_operator_overload) {
    auto call = ASTCall{};
    auto dot = ASTDotExpr{};
    dot.base = node->left;
    dot.member_name = get_operator_overload_name(TType::LBrace, OPERATION_SUBSCRIPT);
    call.function = &dot;
    auto args = ASTArguments{};
    args.arguments = {node->subscript};
    call.arguments = &args;
    call.accept(this);
  } else {
    node->left->accept(this);
    node->subscript->accept(this);
  }
}

void DependencyEmitter::visit(ASTInitializerList *node) {
  if (node->target_type)
    node->target_type.get()->accept(this);
  if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    for (const auto &value : node->values) {
      value->accept(this);
    }
  } else {
    for (const auto [key, value] : node->key_values) {
      value->accept(this);
    }
  }
}

void DependencyEmitter::visit(ASTEnumDeclaration *node) {
  for (const auto &[key, value] : node->key_values) {
    value->accept(this);
  }
}

void DependencyEmitter::visit(ASTRange *node) {
  node->left->accept(this);
  node->right->accept(this);
}

void DependencyEmitter::visit(ASTSwitch *node) {
  node->target->accept(this);
  for (const auto $case : node->cases) {
    $case.block->accept(this);
    $case.expression->accept(this);
  }
}

void DependencyEmitter::visit(ASTTuple *node) {
  for (const auto &value : node->values) {
    value->accept(this);
  }
}

void DependencyEmitter::visit(ASTTupleDeconstruction *node) {
  define_type(node->right->resolved_type);
  node->right->accept(this);
}

void DependencyEmitter::visit(ASTSize_Of *node) { node->target_type->accept(this); }

void DependencyEmitter::visit(ASTScopeResolution *node) { node->base->accept(this); }

void DependencyEmitter::visit(ASTAlias *node) { }

void DependencyEmitter::visit(ASTImpl *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  if (!node->generic_parameters.empty()) {
    return;
  }
  for (const auto &method : node->methods) {
    method->accept(this);
  }
}

void DependencyEmitter::visit(ASTDefer *node) { node->statement->accept(this); }

void DependencyEmitter::visit(ASTTaggedUnionDeclaration *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  for (const auto &variant : node->variants) {
    switch (variant.kind) {
      case ASTTaggedUnionVariant::NORMAL:
        break;
      case ASTTaggedUnionVariant::TUPLE:
        variant.tuple->accept(this);
        break;
      case ASTTaggedUnionVariant::STRUCT:
        for (const auto &decl : variant.struct_declarations) {
          decl->accept(this);
        }
        break;
    }
  }
}

void DependencyEmitter::visit(ASTCast *node) {
  node->expression->accept(this);
  node->target_type->accept(this);
}

void DependencyEmitter::visit(ASTInterfaceDeclaration *node) {}

void DependencyEmitter::visit(ASTLambda *node) {
  node->params->accept(this);
  node->block->accept(this);
  emitter->emit_lambda(node);
}

void DependencyEmitter::visit(ASTWhere *node) {
  node->target_type->accept(this);
  node->predicate->accept(this);
}