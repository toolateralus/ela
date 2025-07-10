#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <set>

void Resolver::declare_type(Type *type) {
  auto extensions = type->extensions;

  // TODO:
  // ! @Cooper-Pilot
  /*
    function pointers were acting weird so i just did this ish.
  */
  if (type->is_kind(TYPE_FUNCTION)) {
    return define_type(type);
  }

  for (auto ext : extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      emitter->forward_decl_type(type);
      return;
    }
  }
  define_type(type);
}

void Resolver::define_type(Type *type) {
  if (type->base_type != Type::INVALID_TYPE) {
    type = type->base_type;
  }

  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->info->as<FunctionTypeInfo>();
      declare_type(info->return_type);
      for (size_t index = 0; index < info->params_len; index++) {
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
      auto info = type->info->as<TupleTypeInfo>();
      for (auto type : info->types) {
        declare_type(type);
      }
      emitter->emit_tuple(type);
    } break;
    case TYPE_DYN: {
      if (type->dyn_emitted) {
        return;
      }
      type->dyn_emitted = true;
      auto info = type->info->as<DynTypeInfo>();
      auto trait_type = info->trait_type;
      auto trait_info = trait_type->info->as<TraitTypeInfo>();
      for (auto [name, sym] : trait_info->scope->symbols) {
        if (sym.is_function && !sym.is_generic_function()) {
          auto declaration = sym.function.declaration;
          for (auto param : declaration->params->params) {
            if (type_is_valid(param->resolved_type)) {
              declare_type(param->resolved_type);
            }
          }
          auto return_type = declaration->return_type->resolved_type;
          if (type_is_valid(return_type)) {
            declare_type(return_type);
          }
        }
      }
      declare_type(info->trait_type);
      emitter->emit_dyn_dispatch_object(info->trait_type, type);
    } break;
    case TYPE_TRAIT:
    case TYPE_SCALAR:
      break;
  }
}

void Resolver::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    return;
  }
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });

  for (auto subtype : node->subtypes) {
    subtype->accept(this);
  }
  for (auto member : node->members) {
    declare_type(member.type->resolved_type);
    if (member.default_value) {
      member.default_value.get()->accept(this);
    }
  }
}

void emit_dependencies_for_reflection(Resolver *dep_resolver, Type *id) {
  static std::set<Type *> visited = {};
  if (visited.contains(id)) {
    return;
  } else {
    visited.insert(id);
  }

  auto type = id;
  if (type->is_pointer() || type->is_fixed_sized_array()) {
    type = type->get_element_type();
  }
  auto scope = type->info->scope;

  for (auto &[name, symbol] : scope->symbols) {
    if (symbol.is_function && !symbol.is_generic_function()) {
      symbol.function.declaration->accept(dep_resolver);
    } else if (type_is_valid(symbol.resolved_type)) {
      emit_dependencies_for_reflection(dep_resolver, symbol.resolved_type);
    }
  }
}

void Resolver::visit(ASTProgram *node) {
  ctx.set_scope(ctx.root_scope);

  if (auto env_sym = ctx.root_scope->local_lookup("Env")) {
    auto env_scope = env_sym->resolved_type->info->scope;
    if (auto initialize_sym = env_scope->local_lookup("initialize")) {
      initialize_sym->function.declaration->accept(this);
    }
  }

  if (compile_command.has_flag("test")) {
    size_t index = 0;
    for (auto &statement : node->statements) {
      if (index == node->end_of_bootstrap_index) {
        ctx.set_scope(node->scope);
      }
      statement->accept(this);
      index++;
    }
  }

  const auto freestanding = compile_command.has_flag("freestanding");
  auto main_sym = node->scope->local_lookup("main");
  if (main_sym && !freestanding) {
    main_sym->function.declaration->accept(this);
  } else {
    // For non-main (library/freestanding) programs we have to force visit everything.
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
  }

  for (auto type : reflected_upon_types) {
    emit_dependencies_for_reflection(this, type);
  }

  auto emit_symbol = [&](InternedString name) {
    if (auto sym = ctx.root_scope->local_lookup(name)) {
      if (sym->is_variable) {
        auto ast = sym->variable.declaration.get();
        ast->accept(this);
        ast->accept(emitter);
      } else if (sym->is_function) {
        sym->function.declaration->accept(this);
      } else if (sym->is_type) {
        define_type(sym->resolved_type);
      }
    }
  };

  // We only need to emit these when we reflect.
  if (reflected_upon_types.size()) {
    emit_symbol("Type");
    emit_symbol("Field");
    emit_symbol("Method");
  }

  ctx.set_scope(ctx.root_scope);
}

void Resolver::visit(ASTBlock *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer _([&] { ctx.set_scope(old_scope); });

  for (auto statement : node->statements) {
    statement->accept(this);
  }
}

void Resolver::visit(ASTFunctionDeclaration *node) {
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

void Resolver::visit(ASTParamsDecl *node) {
  for (auto param : node->params) {
    param->accept(this);
  }
}

void Resolver::visit(ASTParamDecl *node) {
  declare_type(node->resolved_type);
  if (node->tag == ASTParamDecl::Normal && node->normal.default_value) {
    /*
      TODO: i have no idea why but this is all corrupted when you provide an arg when theres a default value.
    */
    node->normal.default_value.get()->accept(this);
  }
}

void Resolver::visit(ASTVariable *node) {
  node->type->accept(this);
  if (node->value) {
    node->value.get()->accept(this);
  }
}

void Resolver::visit(ASTExprStatement *node) { node->expression->accept(this); }

void Resolver::visit_operator_overload(ASTExpr *base, const std::string &operator_name, ASTExpr *argument) {
  auto call = ASTMethodCall{};
  auto dot = ASTDotExpr{};
  dot.base = base;
  dot.member = ASTPath::Segment{operator_name};
  call.callee = &dot;
  auto args = ASTArguments{};
  if (argument) {
    args.arguments = {argument};
  }
  call.arguments = &args;
  call.accept(this);
}

void Resolver::visit(ASTBinExpr *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->left, get_operator_overload_name(node->op, OPERATION_BINARY), node->right);
  } else {
    node->left->accept(this);
    node->right->accept(this);
  }
}

void Resolver::visit(ASTUnaryExpr *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->operand, get_operator_overload_name(node->op, OPERATION_UNARY), nullptr);
  } else {
    define_type(node->operand->resolved_type);
    node->operand->accept(this);
  }
}

void Resolver::visit(ASTIndex *node) {
  if (node->is_operator_overload) {
    visit_operator_overload(node->base, get_operator_overload_name(TType::LBrace, OPERATION_INDEX), node->index);
  } else {
    // make sure type is defined for size
    define_type(node->base->resolved_type);
    node->base->accept(this);
    node->index->accept(this);
  }
}

void Resolver::visit(ASTPath *node) {
  auto type = node->resolved_type;
  if (type && type->kind == TYPE_ENUM && type->declaring_node) {
    type->declaring_node.get()->accept(this);
    type->declaring_node.get()->accept(emitter);
  }

  if (type && type->kind == TYPE_CHOICE) {
    return;  // Do we need to do anything here?
  }

  Scope *scope = ctx.scope;
  for (auto &seg : node->segments) {
    auto &ident = seg.identifier;
    auto symbol = scope->lookup(ident);
    if (!symbol) {
      throw_error(std::format("symbol {} not found in scope", ident.get_str()), node->source_range);
    }
    scope = nullptr;

    ASTDeclaration *instantiation = nullptr;
    if (!seg.generic_arguments.empty()) {
      auto generic_args = emitter->typer.get_generic_arg_types(seg.generic_arguments);
      if (symbol->is_type) {
        auto decl = (ASTDeclaration *)symbol->type.declaration.get();
        instantiation = find_generic_instance(decl->generic_instantiations, generic_args);
        auto type = instantiation->resolved_type;
        scope = type->info->scope;
      } else if (symbol->is_function) {
        instantiation = find_generic_instance(symbol->function.declaration->generic_instantiations, generic_args);
      }
    } else {
      if (symbol->is_module) {
        scope = symbol->module.declaration->scope;
      } else if (symbol->is_type) {
        scope = symbol->resolved_type->info->scope;
      }
    }

    if (instantiation) {
      instantiation->accept(this);
      instantiation->accept(emitter);
    } else if (symbol->is_variable && symbol->variable.declaration) {
      if (!symbol->is_local) {
        symbol->variable.declaration.get()->accept(this);
        symbol->variable.declaration.get()->accept(emitter);
      }
    } else if (symbol->is_function && symbol->function.declaration) {
      symbol->function.declaration->accept(this);
    } else if (symbol->is_type) {
      if (auto decl = symbol->type.declaration.get()) {
        decl->accept(this);
        decl->accept(emitter);
      }
    }
  }
}

void Resolver::visit(ASTLiteral *) {}

void Resolver::visit(ASTType *node) { define_type(node->resolved_type); }

void Resolver::visit(ASTType_Of *node) {
  node->target->accept(this);
  reflected_upon_types.insert(node->target->resolved_type);
}

void Resolver::visit(ASTCall *node) {
  node->arguments->accept(this);
  node->callee->accept(this);
}

void Resolver::visit(ASTArguments *node) {
  for (auto arg : node->arguments) {
    arg->accept(this);
  }
}

void Resolver::visit(ASTReturn *node) {
  if (node->expression.is_not_null()) {
    node->expression.get()->accept(this);
  }
}

void Resolver::visit(ASTContinue *) {}

void Resolver::visit(ASTBreak *) {}

void Resolver::visit(ASTFor *node) {
  define_type(node->iterator_type);
  define_type(node->iterable_type);
  define_type(node->identifier_type);

  node->right->accept(this);

  auto range_scope = node->iterable_type->info->scope;
  auto iter_scope = node->iterator_type->info->scope;

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

void Resolver::visit(ASTIf *node) {
  node->condition->accept(this);
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
}

void Resolver::visit(ASTElse *node) {
  if (node->_if) {
    node->_if.get()->accept(this);
  }
  if (node->block) {
    node->block.get()->accept(this);
  }
}

void Resolver::visit(ASTWhile *node) {
  if (node->condition) {
    node->condition.get()->accept(this);
  }
  node->block->accept(this);
}

void Resolver::visit(ASTDotExpr *node) {
  define_type(node->base->resolved_type);
  node->base->accept(this);
}

void Resolver::visit(ASTInitializerList *node) {
  if (node->target_type) node->target_type.get()->accept(this);
  if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    for (const auto &value : node->values) {
      value->accept(this);
    }
  } else {
    for (const auto [key, value] : node->key_values) {
      value->accept(this);
    }
  }
  declare_type(node->resolved_type);
}

void Resolver::visit(ASTEnumDeclaration *node) {
  for (const auto &[key, value] : node->key_values) {
    value->accept(this);
  }
}

void Resolver::visit(ASTRange *node) {
  node->left->accept(this);
  node->right->accept(this);
}

void Resolver::visit(ASTSwitch *node) {
  node->expression->accept(this);

  auto type = node->expression->resolved_type;

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->is_pointer() && !node->branches.empty()) {
    visit_operator_overload(node->expression, get_operator_overload_name(TType::EQ, OPERATION_BINARY),
                            node->branches[0].expression);
  }

  for (const auto $case : node->branches) {
    $case.block->accept(this);
    $case.expression->accept(this);
  }
  if (auto default_case = node->default_branch.get()) {
    default_case->accept(this);
  }
}

void Resolver::visit(ASTTuple *node) {
  for (const auto &value : node->values) {
    value->accept(this);
  }
}

void Resolver::visit(ASTDestructure *node) {
  define_type(node->right->resolved_type);
  node->right->accept(this);
}

void Resolver::visit(ASTSize_Of *node) { node->target_type->accept(this); }

void Resolver::visit(ASTAlias *) {}

void Resolver::visit(ASTImport *) {}

void Resolver::visit(ASTImpl *node) {
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

void Resolver::visit(ASTDefer *node) { node->statement->accept(this); }

void Resolver::visit(ASTChoiceDeclaration *node) {
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
        for (const auto &decl : variant.struct_declarations) {
          declare_type(decl->resolved_type);
        }
        break;
    }
  }
}

void Resolver::visit(ASTCast *node) {
  node->expression->accept(this);
  node->target_type->accept(this);
}

void Resolver::visit(ASTTraitDeclaration *) {}

void Resolver::visit(ASTLambda *node) {
  node->params->accept(this);
  node->block->accept(this);
  emitter->emit_lambda(node);
}

void Resolver::visit(ASTWhere *node) {
  for (const auto &[target, predicate] : node->constraints) {
    target->accept(this);
    predicate->accept(this);
  }
}

void Resolver::visit(ASTModule *) {}

void Resolver::visit(ASTDyn_Of *node) {
  declare_type(node->resolved_type);
  define_type(node->resolved_type);

  node->object->accept(this);
  auto element_type = node->object->resolved_type->get_element_type();
  auto element_scope = element_type->info->scope;
  for (auto [name, _] : node->resolved_type->info->as<DynTypeInfo>()->methods) {
    auto sym = element_scope->local_lookup(name);
    if (!sym) {
      throw_error(
          std::format("Internal compiler error: couldn't find method {} in dynof({})", name, element_type->to_string()),
          node->source_range);
    }

    if (!sym->is_function || sym->is_generic_function()) {
      throw_error(std::format("Internal compiler error: {} is not a valid method in dynof", name), node->source_range);
    }
    auto decl = sym->function.declaration;
    decl->accept(this);
  }
}

void Resolver::visit(ASTPatternMatch *node) {
  node->object->accept(this);
  node->target_type_path->accept(this);
}

void Resolver::visit(ASTMethodCall *node) {
  node->arguments->accept(this);
  node->callee->accept(this);
  auto symbol_nullable = ctx.get_symbol(node->callee);

  if (symbol_nullable.is_not_null()) {
    auto decl = symbol_nullable.get()->function.declaration;
    auto generic_args = node->callee->member.get_resolved_generics();

    if (!generic_args.empty()) {
      decl = (ASTFunctionDeclaration *)find_generic_instance(decl->generic_instantiations, generic_args);
    }

    if (decl && decl->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      decl->accept(this);
      return;
    }
  }
}

void Resolver::visit(ASTWhereStatement *node) {
  if (node->should_compile) {
    node->block->accept(this);
    return;
  }
  if (node->branch.is_null()) return;

  auto branch = node->branch.get();
  if (branch->where_stmt.is_not_null()) {
    branch->where_stmt.get()->accept(this);
    return;
  }
  if (branch->block.is_not_null()) {
    branch->block.get()->accept(this);
    return;
  }
}
