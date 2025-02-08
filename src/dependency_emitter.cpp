#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"

void DependencyEmitter::emit_type(int type_id) {
  auto type = global_get_type(type_id);
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->get_info()->as<FunctionTypeInfo>();
      emit_type(info->return_type);
      for (int index = 0; index < info->params_len; index++) {
        auto param_ty = info->parameter_types[index];
        emit_type(param_ty);
      }
    } break;
    case TYPE_TAGGED_UNION:
    case TYPE_STRUCT: {
      if (type->get_ext().is_pointer()) {
        emitter->forward_decl_type(type);
        return;
      }
      if (type->declaring_node.is_not_null()) {
        type->declaring_node.get()->accept(this);
      } else {
        throw_error("internal compiler error: could not locate node for struct or tagged union type", {});
      }
    } break;
    case TYPE_TUPLE: {
      if (type->get_ext().is_pointer()) {
        emitter->forward_decl_type(type);
        return;
      }
      auto info = type->get_info()->as<TupleTypeInfo>();
      for (auto type : info->types) {
        emit_type(type);
      }
    } break;
    case TYPE_INTERFACE:
    case TYPE_SCALAR:
    case TYPE_ENUM:
      break;
  }
}

void DependencyEmitter::visit(ASTStructDeclaration *node) {
  for (auto member : node->members) {
    member.type->accept(this);
  }
}

void DependencyEmitter::visit(ASTProgram *node) {
  for (auto statement : node->statements) {
    statement->accept(this);
  }
}

void DependencyEmitter::visit(ASTBlock *node) {
  for (auto statement : node->statements) {
    statement->accept(this);
  }
}

void DependencyEmitter::visit(ASTFunctionDeclaration *node) {
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
	emit_type(node->resolved_type);
}

void DependencyEmitter::visit(ASTDeclaration *node) {
	node->type->accept(this);
	if (node->value) {
		node->value.get()->accept(this);
	}
}

void DependencyEmitter::visit(ASTExprStatement *node) {
	node->expression->accept(this);
}

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
    node->operand->accept(this);
  }
}

void DependencyEmitter::visit(ASTIdentifier *node) {}

void DependencyEmitter::visit(ASTLiteral *node) {}

void DependencyEmitter::visit(ASTType *node) {
  emit_type(node->resolved_type);
}

void DependencyEmitter::visit(ASTCall *node) {
  for (auto generic_arg : node->generic_arguments) {
    generic_arg->accept(this);
  }
  node->arguments->accept(this);
  auto symbol_nullable = emitter->typer.get_symbol(node->function);
  if (symbol_nullable.is_not_null()) {
    auto symbol = symbol_nullable.get();
    if (symbol->is_function()) {
      symbol->function.declaration->accept(this);
    }
  }
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
  node->range->accept(this);
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
  if (node->target_type) node->target_type.get()->accept(this);
  if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    for (const auto &value: node->values) {
      value->accept(this);
    }
  } else {
    for (const auto [key, value]: node->key_values) {
      value->accept(this);
    }
  }
}

void DependencyEmitter::visit(ASTEnumDeclaration *node) {
  for (const auto &[key, value]: node->key_values) {
    value->accept(this);
  }
}

void DependencyEmitter::visit(ASTRange *node) {
  node->left->accept(this);
  node->right->accept(this);
}

void DependencyEmitter::visit(ASTSwitch *node) {
  node->target->accept(this);
  for (const auto $case: node->cases) {
    $case.block->accept(this);
    $case.expression->accept(this);
  }
}

void DependencyEmitter::visit(ASTTuple *node) {
  for (const auto &value: node->values) {
    value->accept(this);
  }
}

void DependencyEmitter::visit(ASTTupleDeconstruction *node) {
  node->right->accept(this);
}

void DependencyEmitter::visit(ASTSize_Of *node) {
  node->target_type->accept(this);
}

void DependencyEmitter::visit(ASTScopeResolution *node) {
  node->base->accept(this);
}

void DependencyEmitter::visit(ASTAlias *node) {
  node->type->accept(this);
}

void DependencyEmitter::visit(ASTImpl *node) {
  node->target->accept(this);
  for (const auto &method: node->methods) {
    method->accept(this);
  }
}

void DependencyEmitter::visit(ASTDefer *node) {
  node->statement->accept(this);
}
void DependencyEmitter::visit(ASTTaggedUnionDeclaration *node) {
  for (const auto &variant: node->variants) {
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
void DependencyEmitter::visit(ASTInterfaceDeclaration *node) {
  for (auto method : node->methods) {
    method->accept(this);
  }
}

void DependencyEmitter::visit(ASTLambda *node) {
  node->params->accept(this);
  node->block->accept(this);
}

void DependencyEmitter::visit(ASTWhere *node) {
  node->target_type->accept(this);
  node->predicate->accept(this);
}