#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <set>

void DependencyEmitter::declare_type(int type_id) {
  auto type = global_get_type(type_id);
  auto extensions = type->get_ext().extensions;
  for (auto ext : extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      emitter->forward_decl_type(type);
      return;
    }
  }
  define_type(type_id);
}

void DependencyEmitter::define_type(int type_id) {
  auto type = global_get_type(type_id);
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->get_info()->as<FunctionTypeInfo>();
      declare_type(info->return_type);
      for (int index = 0; index < info->params_len; index++) {
        auto param_ty = info->parameter_types[index];
        declare_type(param_ty);
      }
    } break;
    case TYPE_CHOICE:
    case TYPE_ENUM:
    case TYPE_STRUCT: {
      if (auto declaring_node = type->declaring_node.get()) {
        declaring_node->accept(this);
        declaring_node->accept(emitter);
      }
    } break;
    case TYPE_TUPLE: {
      auto info = type->get_info()->as<TupleTypeInfo>();
      for (auto type : info->types) {
        define_type(type);
      }
      emitter->emit_tuple(type_id);
    } break;
    case TYPE_DYN: {
      if (type->dyn_emitted) {
        return;
      }
      type->dyn_emitted = true;
      auto info = type->get_info()->as<DynTypeInfo>();
      auto interface_type = global_get_type(info->interface_type);
      auto interface_info = interface_type->get_info()->as<InterfaceTypeInfo>();
      for (auto [name, sym] : interface_info->scope->symbols) {
        if (sym.is_function() && !sym.is_generic_function()) {
          auto declaration = sym.function.declaration;
          for (auto param : declaration->params->params) {
            if (param->resolved_type != -1) {
              define_type(param->resolved_type);
            }
          }
          auto return_type = declaration->return_type->resolved_type;
          if (return_type != -1) {
            define_type(return_type);
          }
        }
      }
      define_type(info->interface_type);
      emitter->emit_dyn_dispatch_object(info->interface_type, type_id);
    } break;
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
    declare_type(member.type->resolved_type);
  }
}

void emit_dependencies_for_reflection(DependencyEmitter *dep_resolver, int id) {
  static std::set<int> visited_type_ids = {};
  if (visited_type_ids.contains(id)) {
    return;
  } else {
    visited_type_ids.insert(id);
  }
  auto type = global_get_type(id);
  if (type->get_ext().is_pointer() || type->get_ext().is_fixed_sized_array()) {
    type = global_get_type(type->get_element_type());
  }
  auto scope = type->get_info()->scope;

  for (auto &[name, symbol] : scope->symbols) {
    if (symbol.is_function() && !symbol.is_generic_function()) {
      symbol.function.declaration->accept(dep_resolver);
    } else if (symbol.type_id >= 0) {
      emit_dependencies_for_reflection(dep_resolver, symbol.type_id);
    }
  }
}

void DependencyEmitter::visit(ASTProgram *node) {
  ctx.set_scope(ctx.root_scope);
  size_t index = 0;

  if (!compile_command.has_flag("nostdlib")) {
    // We have to do this here because the reflection system depends on this type and it doesn't
    // neccesarily get instantiatied.
    // see the `Emitter:get_type_struct`
    // and `bootstrap/reflection.ela/Type :: struct`
    auto sub_types = std::vector<int>{
        ctx.scope->find_type_id("str", {}),
        ctx.scope->find_type_id("void", {{{TYPE_EXT_POINTER_CONST}}}),
    };
    auto tuple_id = global_find_type_id(sub_types, {});
    define_type(tuple_id);
    declare_type(tuple_id);
  }

  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
    }
    statement->accept(this);
    index++;
  }

  for (auto id : reflected_upon_types) {
    emit_dependencies_for_reflection(this, id);
  }

  ctx.set_scope(ctx.root_scope);
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
    emitter->emit_forward_declaration(node);
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
  declare_type(node->return_type->resolved_type);
  node->params->accept(this);
  if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  node->accept(emitter);
}

void DependencyEmitter::visit(ASTParamsDecl *node) {
  for (auto param : node->params) {
    param->accept(this);
  }
}

void DependencyEmitter::visit(ASTParamDecl *node) { declare_type(node->resolved_type); }

void DependencyEmitter::visit(ASTVariable *node) {
  node->type->accept(this);
  if (node->value) {
    node->value.get()->accept(this);
  }
}

void DependencyEmitter::visit(ASTExprStatement *node) { node->expression->accept(this); }

void DependencyEmitter::visit_operator_overload(ASTExpr *base, const std::string &operator_name, ASTExpr *argument) {
  auto call = ASTMethodCall{};
  auto dot = ASTDotExpr{};
  dot.base = base;
  dot.member = ASTPath::Segment{operator_name};
  call.dot = &dot;
  auto args = ASTArguments{};
  if (argument) {
    args.arguments = {argument};
  }
  call.arguments = &args;
  call.accept(this);
}

void DependencyEmitter::visit(ASTBinExpr *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->left, get_operator_overload_name(node->op.type, OPERATION_BINARY), node->right);
  } else {
    node->left->accept(this);
    node->right->accept(this);
  }
}

void DependencyEmitter::visit(ASTUnaryExpr *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->operand, get_operator_overload_name(node->op.type, OPERATION_UNARY), nullptr);
  } else {
    if (node->op.type == TType::Mul) {
      define_type(node->operand->resolved_type);
    }
    node->operand->accept(this);
  }
}

void DependencyEmitter::visit(ASTSubscript *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->left, get_operator_overload_name(TType::LBrace, OPERATION_SUBSCRIPT),
                            node->subscript);
  } else {
    // make sure type is defined for size
    define_type(node->left->resolved_type);
    node->left->accept(this);
    node->subscript->accept(this);
  }
}

void DependencyEmitter::visit(ASTPath *node) {

  // TODO: this should be handled by ASTType ... update: what does this mean by 'this'? I think this is irrelevant.
  auto type = global_get_type(node->resolved_type);
  if (type && type->kind == TYPE_ENUM) {
    type->declaring_node.get()->accept(this);
    type->declaring_node.get()->accept(emitter);
  }

  if (type && type->kind == TYPE_CHOICE) {
    return; // Do we need to do anything here?
  }

  Scope *scope = ctx.scope;
  auto index = 0;
  for (auto &seg in node->segments) {
    auto &ident = seg.identifier;
    auto symbol = scope->lookup(ident);
    scope = nullptr;
    if (!symbol) {
      throw_error("symbol not found in scope", node->source_range);
    }

    ASTDeclaration *instantiation = nullptr;
    if (!seg.generic_arguments.empty()) {
      auto generic_args = emitter->typer.get_generic_arg_types(seg.generic_arguments);
      if (symbol->is_type()) {
        auto decl = (ASTDeclaration *)symbol->type.declaration.get();
        instantiation = find_generic_instance(decl->generic_instantiations, generic_args);
        auto type = global_get_type(instantiation->resolved_type);
        scope = type->get_info()->scope;
      } else if (symbol->is_function()) {
        instantiation = find_generic_instance(symbol->function.declaration->generic_instantiations, generic_args);
      }
    } else {
      if (symbol->is_module()) {
        scope = symbol->module.declaration->scope;
      } else if (symbol->is_type()) {
        scope = global_get_type(symbol->type_id)->get_info()->scope;
      }
    }

    if (instantiation) {
      instantiation->accept(this);
      instantiation->accept(emitter);
    } else if (symbol->is_variable() && symbol->variable.declaration) {
      // for global variables
      auto decl = symbol->variable.declaration.get();
      if (!decl->declaring_block) {
        symbol->variable.declaration.get()->accept(this);
        symbol->variable.declaration.get()->accept(emitter);
      }
    } else if (symbol->is_function()) {
      // TODO: we should change how template retrival works;
      symbol->function.declaration->accept(this);
    } else if (symbol->is_type()) {
      if (auto decl = symbol->type.declaration.get()) {
        decl->accept(this);
        decl->accept(emitter);
      }
    }

    index++;
  }
}

void DependencyEmitter::visit(ASTLiteral *node) {}

void DependencyEmitter::visit(ASTType *node) { declare_type(node->resolved_type); }

void DependencyEmitter::visit(ASTType_Of *node) {
  node->target->accept(this);
  reflected_upon_types.insert(node->target->resolved_type);
}

void DependencyEmitter::visit(ASTCall *node) {
  node->arguments->accept(this);
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
  define_type(node->iterator_type);
  define_type(node->iterable_type);
  define_type(node->identifier_type);

  node->right->accept(this);

  auto range_scope = global_get_type(node->iterable_type)->get_info()->scope;
  auto iter_scope = global_get_type(node->iterator_type)->get_info()->scope;

  switch (node->iteration_kind) {
    case ASTFor::ITERABLE: {
      auto iter_sym = range_scope->local_lookup("iter");
      iter_sym->function.declaration->accept(this);
    } break;
    case ASTFor::ITERATOR:
      break;
  }

  auto done_sym = iter_scope->local_lookup("next");
  done_sym->function.declaration->accept(this);
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
  if (auto default_case = node->default_case.get()) {
    default_case->accept(this);
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

void DependencyEmitter::visit(ASTAlias *node) {}

void DependencyEmitter::visit(ASTImport *node) {}

void DependencyEmitter::visit(ASTImpl *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  if (!node->generic_parameters.empty()) {
    return;
  }
  for (const auto &constant : node->constants) {
    constant->accept(this);
  }
  for (const auto &alias : node->aliases) {
    alias->accept(this);
  }
  for (const auto &method : node->methods) {
    method->accept(this);
  }
}

void DependencyEmitter::visit(ASTDefer *node) { node->statement->accept(this); }

void DependencyEmitter::visit(ASTChoiceDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    return;
  }
  
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });
  for (const auto &variant : node->variants) {
    switch (variant.kind) {
      case ASTChoiceVariant::NORMAL:
        break;
      case ASTChoiceVariant::TUPLE:
        variant.tuple->accept(this);
        break;
      case ASTChoiceVariant::STRUCT:
        for (const auto &decl : variant.struct_declarations)
          decl->accept(this);
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
  for (const auto &[target, predicate] : node->constraints) {
    target->accept(this);
    predicate->accept(this);
  }
}

void DependencyEmitter::visit(ASTModule *node) {}

void DependencyEmitter::visit(ASTDyn_Of *node) {
  define_type(node->resolved_type);
  node->object->accept(this);
  auto element_type = global_get_type(node->object->resolved_type)->get_element_type();
  auto element_scope = global_get_type(element_type)->get_info()->scope;
  auto interface_scope = global_get_type(node->interface_type->resolved_type)->get_info()->scope;
  for (auto [name, interface_sym] : interface_scope->symbols) {
    if (!interface_sym.is_function() || interface_sym.is_generic_function()) {
      continue;
    }
    auto decl = element_scope->local_lookup(name)->function.declaration;
    decl->accept(this);
  }
}

void DependencyEmitter::visit(ASTPatternMatch *node) {}

void DependencyEmitter::visit(ASTMethodCall *node) {
  node->arguments->accept(this);
  node->dot->accept(this);
  auto symbol_nullable = ctx.get_symbol(node->dot);

  if (symbol_nullable.is_not_null()) {
    auto decl = symbol_nullable.get()->function.declaration;
    auto generic_args = node->dot->member.get_resolved_generics();

    if (!generic_args.empty()) {
      decl = (ASTFunctionDeclaration *)find_generic_instance(decl->generic_instantiations, generic_args);
    }

    if (decl && decl->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      decl->accept(this);
      return;
    }
  }
}