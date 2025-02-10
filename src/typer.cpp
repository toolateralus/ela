
#include <cassert>
#include <csetjmp>
#include <format>
#include <string>
#include <vector>

#include "ast.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"



void Typer::visit_interface_declaration(ASTInterfaceDeclaration *node, bool generic_instantiation,
                                        std::vector<int> generic_args) {
  auto id = ctx.scope->find_type_id(node->name, {});
  if (id != -1) {
    auto type = global_get_type(id);
    if (type->is_kind(TYPE_INTERFACE)) {
      if (!generic_instantiation)
        throw_error("re-definition of interface type.", node->source_range);
    } else {
      throw_error("re-definition of a type", node->source_range);
    }
  }

  auto previous = ctx.scope;
  Defer _([&] { ctx.set_scope(previous); });
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  auto type = global_get_type(global_create_interface_type(node->name, ctx.scope, generic_args));
  type->declaring_node = node;
  node->resolved_type = type->id;
}


void Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  auto iden = static_cast<ASTIdentifier *>(node->iter_identifier);
  node->range->accept(this);
  // auto range_node_ty = node->range->node_type;
  int range_type_id = node->range->resolved_type;
  Type *range_type = global_get_type(range_type_id);
  node->range_type = range_type->id;

  if (range_type->meta.has_extensions() && range_type->meta.extensions.back().type == TYPE_EXT_POINTER) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            range_type->to_string()),
                node->source_range);
  }

  int iter_ty = Type::INVALID_TYPE_ID;
  auto scope = range_type->info->scope;

  if (range_type->implements("Iterable")) {
    node->iteration_kind = ASTFor::ITERABLE;
    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(range_type_id, "iter");

    auto symbol = scope->local_lookup("iter");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Iterable' but no 'iter' function was found when "
                  "attempting to iterate, or it was a non-function symbol named 'iter'",
                  node->source_range);
    }

    auto symbol_ty = global_get_type(symbol->type_id);
    auto iter_return_ty = global_get_type(symbol_ty->info->as<Function_Info>()->return_type);
    node->iterable_type = iter_return_ty->id;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(iter_return_ty->id, "current");

    symbol = iter_return_ty->info->scope->local_lookup("current");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Iterable' but no 'current' function was found when "
                  "attempting to iterate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->info->as<Function_Info>()->return_type;
  } else if (range_type->implements("Enumerable")) {
    node->iteration_kind = ASTFor::ENUMERABLE;
    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(range_type_id, "enumerator");

    auto symbol = scope->local_lookup("enumerator");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Enumerable' but no 'enumerator' function was found when "
                  "attempting to enumerate, or it was a non-function symbol named 'enumerator'",
                  node->source_range);
    }
    auto iter_return_ty =
        global_get_type(global_get_type(symbol->type_id)->info->as<Function_Info>()->return_type);
    node->iterable_type = iter_return_ty->id;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(iter_return_ty->id, "current");

    symbol = iter_return_ty->info->scope->local_lookup("current");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Iterable' but no 'current' function was found when "
                  "attempting to iterate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->info->as<Function_Info>()->return_type;
  } else if (range_type->implements("Enumerator")) {
    node->iteration_kind = ASTFor::ENUMERATOR;
    compiler_mock_function_call_visit_impl(range_type_id, "current");
    auto symbol = scope->local_lookup("current");

    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Enumerator' but no 'current' function was found when "
                  "attempting to enumerate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->info->as<Function_Info>()->return_type;
    node->iterable_type = range_type_id;
  } else if (range_type->base.get_str().starts_with("Iter$")) {
    node->iteration_kind = ASTFor::ITERATOR;
    node->iterable_type = range_type_id;
    compiler_mock_function_call_visit_impl(range_type_id, "current");
    iter_ty = global_get_type(range_type->generic_args[0])->take_pointer_to();
  } else {
    throw_error("cannot iterate with for-loop on a type that doesn't implement either the 'Iterable' or the "
                "'Enumerable' interface. "
                "Please implement at least one of these, note that 'Iterable'/'iter()' will be selected before "
                "'Enumerable'/'enumerator()', mainly for performance reasons, but also ambiguity",
                node->source_range);
  }

  auto is_enumerator_or_enumerable =
      node->iteration_kind == ASTFor::ENUMERABLE || node->iteration_kind == ASTFor::ENUMERATOR;

  if (is_enumerator_or_enumerable && node->value_semantic == VALUE_SEMANTIC_POINTER) {
    throw_error("Cannot use the 'for *i in ...' \"pointer semantic\" style for loop with objects that implement "
                "'Enumerable'. This is because the enumerable()'s return type is not restricted to being a pointer, "
                "and we can't guarantee "
                "that taking a reference to it is safe or even valid.",
                node->source_range);

  } else if (!is_enumerator_or_enumerable && node->value_semantic != VALUE_SEMANTIC_POINTER) {
    // * for a type that implements Iter, we always return T*, so if we don't use the semantic, we assume an implicit
    // dereference.
    auto type = global_get_type(iter_ty);
    if (!type->meta.is_pointer()) {
      throw_error("internal compiler error: got `for *.. in ...` but the iter() did not return a pointer type?",
                  node->source_range);
    }
    iter_ty = type->get_element_type();
  }

  node->identifier_type = iter_ty;
  ctx.scope->insert_variable(iden->value, iter_ty, node->iter_identifier);
  node->iter_identifier->accept(this);
  node->range->accept(this);

  ctx.exit_scope();
  node->block->accept(this);

  //? Is this correct???
  auto control_flow = node->block->control_flow;
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  node->control_flow = control_flow;
}

void Typer::visit(ASTIf *node) {
  node->condition->accept(this);
  auto cond_ty = node->condition->resolved_type;
  auto conversion_rule = type_conversion_rule(global_get_type(cond_ty), global_get_type(bool_type()));

  if (conversion_rule == CONVERT_PROHIBITED) {
    throw_error(std::format("cannot convert 'if' condition to a boolean, implicitly nor explicitly. got type \"{}\"",
                            global_get_type(cond_ty)->to_string()),
                node->source_range);
  }
  node->block->accept(this);
  auto control_flow = node->block->control_flow;
  if (node->_else.is_not_null()) {
    auto _else = node->_else.get();
    _else->accept(this);
    auto else_cf = _else->control_flow;
    control_flow.flags |= else_cf.flags;
    if ((else_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      assert_return_type_is_valid(control_flow.type, else_cf.type, node);
    }
  } else {
    control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  }
  node->control_flow = control_flow;
}

void Typer::visit(ASTElse *node) {
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
    node->control_flow = node->_if.get()->control_flow;
  } else {
    node->block.get()->accept(this);
    node->control_flow = node->block.get()->control_flow;
  }
}

void Typer::visit(ASTWhile *node) {
  if (node->condition.is_not_null()) {
    node->condition.get()->accept(this);
  }
  node->block->accept(this);
  auto control_flow = node->block->control_flow;
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  // we add fall through here because we dont know if this will get
  // excecuted since we cant evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  node->control_flow = control_flow;
}

void Typer::visit(ASTCall *node) {
  Type *type = nullptr;
  AST *func_decl = nullptr;

  // Try to find the function via a dot expression, scope resolution, identifier, etc.
  // Otherwise find it via a type resolution, for things like array[10](); or what have you.
  auto symbol = get_symbol(node->function).get();
  if (symbol && symbol->is_function()) {
    if (!type) {
      type = global_get_type(symbol->type_id);
    }

    auto declaring_node = symbol->function.declaration;
    if (declaring_node && declaring_node->node_type == AST_FUNCTION) {
      func_decl = static_cast<AST *>(declaring_node);

      // resolve a generic call.
      if (!node->generic_arguments.empty() || !func_decl->generic_parameters.empty()) {
        // doing this so self will get the right type when we call generic methods
        // TODO: handle this in the function decl itself, maybe insert self into symbol table
        auto old_type = type_context;
        Defer _([&] { type_context = old_type; });
        ASTType func_type_ast;
        if (func_decl->declaring_type != Type::INVALID_TYPE_ID) {
          func_type_ast.resolved_type = func_decl->declaring_type;
          type_context = &func_type_ast;
        }

        GENERIC_PANIC_HANDLER(
            data, 1, { func_decl = resolve_generic_function_call(node, func_decl); }, node->source_range);
      }
      type = global_get_type(func_decl->resolved_type);
    }
  } else {
    node->function->accept(this);
    type = global_get_type(node->function->resolved_type);
  }

  if (!type)
    throw_error("use of undeclared function", node->source_range);

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("unable to call a non-function, got {}", type->to_string()), node->source_range);
  }

  auto info = type->info->as<Function_Info>();

  // If we have the declaring node representing this function, type check it against the parameters in that definition.
  // else, use the type.
  if (func_decl) {
    bool skip_first = false;
    if (node->function->node_type == AST_DOT_EXPR) {
      if (!func_decl->params->has_self) {
        throw_error("Calling static methods with instance not allowed", node->source_range);
      }
      skip_first = true;
    }
    type_check_args_from_params(node->arguments, func_decl->params, skip_first);
  } else {
    type_check_args_from_info(node->arguments, info);
  }
  node->resolved_type = info->return_type;
}

void Typer::type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, bool skip_first) {
  auto old_type = expected_type;
  Defer _([&]() { expected_type = old_type; });
  auto args_ct = node->arguments.size();
  auto params_ct = params->params.size();
  auto largest = args_ct > params_ct ? args_ct : params_ct;
  int param_index = skip_first ? 1 : 0;
  for (int arg_index = 0; arg_index < largest; ++arg_index, ++param_index) {
    if (param_index < params_ct) {
      if (arg_index < args_ct) {
        expected_type = params->params[param_index]->resolved_type;
        node->arguments[arg_index]->accept(this);
        assert_types_can_cast_or_equal(
            node->arguments[arg_index]->resolved_type, params->params[param_index]->resolved_type,
            node->arguments[arg_index]->source_range,
            std::format("unexpected argument type.. parameter #{} of function",
                        arg_index + 1)); // +1 here to make it 1 based indexing for user. more intuitive
      }
    } else {
      if (arg_index < args_ct) {
        expected_type = Type::INVALID_TYPE_ID;
        node->arguments[arg_index]->accept(this);
        if (!params->is_varargs) {
          throw_error("Too many arguments to function", node->source_range);
        }
      }
    }
  }
}

void Typer::type_check_args_from_info(ASTArguments *node, Function_Info *info) {
  auto old_type = expected_type;
  Defer _([&]() { expected_type = old_type; });
  auto args_ct = node->arguments.size();
  // TODO: rewrite this. this is so hard tor read.
  if ((args_ct > info->params_len && !info->is_varargs) || args_ct < info->params_len) {
    throw_error(
        std::format("Function call has incorrect number of arguments. Expected: {}, Found: {}... function type: {}",
                    info->params_len, args_ct, info->to_string()),
        node->source_range);
  }

  for (int i = 0; i < args_ct; ++i) {
    auto arg = node->arguments[i];
    expected_type = info->parameter_types[i];
    arg->accept(this);
    if (i < info->params_len) {
      assert_types_can_cast_or_equal(arg->resolved_type, info->parameter_types[i], arg->source_range,
                                     std::format("invalid argument type for parameter #{}", i + 1));
    }
  }
}

AST *Typer::resolve_generic_function_call(ASTCall *node, AST *func) {
  std::vector<int> generic_args;
  if (node->generic_arguments.empty()) {
    node->arguments->accept(this);
    generic_args = node->arguments->resolved_argument_types;
    auto index = 0;
    for (auto generic_arg : generic_args) {
      auto type = ast_alloc<ASTType>();
      type->source_range = node->source_range;
      auto gen_t = global_get_type(generic_arg);

      /*
        * This is auto dereferencing an inferred generic argument when you have a parameter such as T*
        * We do this because it's strange to pass T as s32* if i do func(&s32);
        * it makes it hard to do certain things, and if you wanted to take T as s32*, you'd just not give it a T* in
        your
        * parameter signature.

        * I tried to mke it safer, not sure if i did.
      */

      if (gen_t->meta.is_pointer() && !func->params->params.empty()) {
        // if it == 1, then we skip zero. works out.
        int param_infer_index = func->params->params[0]->tag == ASTParamDecl::Self;
        if (param_infer_index < func->params->params.size() &&
            func->params->params[param_infer_index]->normal.type != nullptr &&
            !func->params->params[param_infer_index]->normal.type->extensions.empty()) {
          type->resolved_type = gen_t->get_element_type();
        } else {
          type->resolved_type = generic_arg;
        }
      } else {
        type->resolved_type = generic_arg;
      }
      node->generic_arguments.push_back(type);
      index++;
    }
  } else {
    generic_args = get_generic_arg_types(node->generic_arguments);
  }
  auto instantiation = visit_generic(&Typer::visit_function_header, func, generic_args);
  if (!instantiation) {
    throw_error("Template instantiation argument count mismatch", node->source_range);
  }
  instantiation->generic_arguments = generic_args;
  visit_function_body(static_cast<AST *>(instantiation));
  return instantiation;
}

void Typer::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  if (auto _switch = dynamic_cast<ASTSwitch *>(node->expression)) {
    node->control_flow = _switch->control_flow;
    node->resolved_type = _switch->resolved_type;
    node->resolved_type = _switch->resolved_type;
  }
}

template <typename T> T Typer::visit_generic(VisitorMethod<T> visit_method, T definition, std::vector<int> args) {
  if (definition->generic_parameters.size() != args.size()) {
    return nullptr;
  }
  auto instantiation = find_generic_instance(definition->generic_instantiations, args);
  if (!instantiation) {
    instantiation = static_cast<T>(deep_copy_ast(definition));
    definition->generic_instantiations.push_back({args, instantiation});
    (this->*visit_method)(instantiation, true, args);
    instantiation->generic_parameters.clear();
    instantiation->generic_instantiations.clear();
  }
  return instantiation;
}

std::vector<TypeExtension> Typer::accept_extensions(std::vector<AST_Type_Extension> ast_extensions) {
  std::vector<TypeExtension> extensions;
  for (auto &meta : ast_extensions) {
    if (meta.type == TYPE_EXT_ARRAY) {
      auto val = evaluate_constexpr(meta.expression, ctx);
      if (val.tag != Value::INTEGER) {
        throw_error("Fixed array must have integer size.", meta.expression->source_range);
      }
      extensions.push_back({meta.type, (size_t)val.integer});
    } else {
      extensions.push_back({.type = meta.type});
    }
  }
  return extensions;
}

void Typer::visit(ASTType *node) {
  if (node->resolved_type != Type::INVALID_TYPE_ID) {
    return;
  }

  Type_Metadata extensions;
  extensions.extensions = accept_extensions(node->extensions);

  if (node->kind == ASTType::SELF) {
    auto self = get_self_type();
    if (self == Type::INVALID_TYPE_ID) {
      throw_error("Cannot locate #self type.", node->source_range);
    }
    auto self_w_ext = global_find_type_id(self, extensions);
    if (self_w_ext == Type::INVALID_TYPE_ID) {
      throw_error("Cannot locate #self type with extensions", node->source_range);
    }
    node->resolved_type = self_w_ext;
    return;
  }

  // ! I have to check if the base is null because for some reason it's just null sometimes.
  if (node->kind == ASTType::NORMAL) {
    auto &normal_ty = node->normal;
    auto symbol = get_symbol(normal_ty.base).get();

    if (!symbol || !symbol->is_type()) {
      throw_error("use of undeclared type, or cannot use a non-type symbol as a type", node->source_range);
    }

    auto declaring_node = symbol->type.declaration.get();

    if (declaring_node && !normal_ty.generic_arguments.empty()) {
      std::vector<int> generic_args;
      for (auto &arg : normal_ty.generic_arguments) {
        arg->accept(this);
        generic_args.push_back(arg->resolved_type);
      }

      ASTStatement *instantiation = nullptr;
      auto decl_node_type = declaring_node->node_type;

      GENERIC_PANIC_HANDLER(data, 1, {
        switch (decl_node_type) {
          case AST_STRUCT:
            instantiation = visit_generic(&Typer::visit_struct_declaration, (AST *)declaring_node,
                                          generic_args);
            break;
          case AST_FUNCTION:
            instantiation = visit_generic(&Typer::visit_function_header,
                                          (AST *)declaring_node, generic_args);
            break;
          case AST_INTERFACE_DECLARATION:
            instantiation = visit_generic(&Typer::visit_interface_declaration,
                                          (ASTInterfaceDeclaration *)declaring_node, generic_args);
            break;
          default:
            throw_error("Invalid target to generic args", node->source_range);
            break;
        }
      }, node->source_range);

      if (!instantiation) {
        throw_error("Template instantiation argument count mismatch", node->source_range);
      }

      GENERIC_PANIC_HANDLER(other_data, 2, {
        if (decl_node_type == AST_FUNCTION) {
          auto func = static_cast<AST *>(instantiation);
          func->generic_arguments = generic_args;
          visit_function_body(func);
        } else if (decl_node_type == AST_STRUCT) {
          auto struct_decl = static_cast<AST *>(instantiation);
          for (auto impl : struct_decl->impls) {
            if (impl->resolved_type == Type::INVALID_TYPE_ID) {
              // setting target resolved_type so that when target's visited it won't try to
              // instatiate the impls again. otherwise, it visits them in reverse order.
              impl->target->resolved_type = instantiation->resolved_type;
              visit_generic(&Typer::visit_impl_declaration, impl, generic_args);
            }
          }
        }
        node->resolved_type = global_find_type_id(instantiation->resolved_type, extensions);
      }, node->source_range);

    } else {
      normal_ty.base->accept(this);
      auto base_ty = global_get_type(normal_ty.base->resolved_type);
      if (!base_ty) {
        throw_error(std::format("use of undeclared type", normal_ty.base->resolved_type), node->source_range);
      }
      node->resolved_type = global_find_type_id(base_ty->id, extensions);
    }
  } else if (node->kind == ASTType::TUPLE) {
    std::vector<int> types;
    for (const auto &t : node->tuple_types) {
      t->accept(this);
      types.push_back(t->resolved_type);
    }
    node->resolved_type = global_find_type_id(types, extensions);
  } else if (node->kind == ASTType::REFLECTION) {
    auto &normal_ty = node->normal;
    normal_ty.base->accept(this);
    auto base_ty = global_get_type(normal_ty.base->resolved_type);
    if (!base_ty) {
      throw_error("use of undeclared type", node->source_range);
    }
    node->pointing_to.get()->accept(this);
    node->resolved_type = global_find_type_id(base_ty->id, extensions);
  } else if (node->kind == ASTType::FUNCTION) {
    auto &func = node->function;
    Function_Info info;
    // TODO: I don' tthink is is ever null, ever.
    if (func.return_type.is_not_null()) {
      func.return_type.get()->accept(this);
      info.return_type = func.return_type.get()->resolved_type;
    } else {
      info.return_type = void_type();
    }
    for (auto &param_ty : func.parameter_types) {
      param_ty->accept(this);
      info.parameter_types[info.params_len] = param_ty->resolved_type;
      info.params_len++;
    }
    node->resolved_type = global_find_function_type_id(info, extensions);
  } else {
    throw_error("internal compiler error: Invalid type kind", node->source_range);
  }
}

void Typer::visit(ASTDotExpr *node) {
  node->base->accept(this);
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = global_get_type(base_ty_id);

  if (!base_ty) {
    throw_error("internal compiler error: un-typed variable on lhs of dot "
                "expression?",
                node->source_range);
  }

  Scope *base_scope = base_ty->info->scope;

  // Implicit dereference, we look at the base scope.
  if (base_ty->meta.is_pointer()) {
    base_ty = global_get_type(base_ty_id = base_ty->get_element_type());
    base_scope = base_ty->info->scope;
  }

  if (!base_scope) {
    throw_error("internal compiler error: dot expression used on a type that had a null scope", node->source_range);
  }

  if (auto member = base_scope->local_lookup(node->member_name)) {
    node->resolved_type = member->type_id;
  } else {
    for (const auto &[name, _] : base_scope->symbols) {
      std::cout << "symbol: " << name.get_str() << '\n';
    }
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", node->member_name, base_ty->to_string()),
                node->source_range);
  }
}

void Typer::visit(ASTScopeResolution *node) {
  node->base->accept(this);
  auto id = node->base->resolved_type;
  auto base_ty = global_get_type(id);
  Scope *scope = base_ty->info->scope;
  if (!scope) {
    throw_error("internal compiler error: scope is null for scope resolution", node->source_range);
  }
  if (auto member = scope->local_lookup(node->member_name)) {
    node->resolved_type = member->type_id;
    return;
  } else if (auto type = scope->find_type_id(node->member_name, {})) {
    if (type == Type::INVALID_TYPE_ID)
      goto ERROR_CASE;
    node->resolved_type = type;
    return;
  } else {
  ERROR_CASE:
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", node->member_name, base_ty->to_string()),
                node->source_range);
  }
}

void Typer::visit(ASTSubscript *node) {
  node->left->accept(this);
  node->subscript->accept(this);
  auto left_ty = global_get_type(node->left->resolved_type);
  auto subscript_ty = global_get_type(node->subscript->resolved_type);

  auto overload = find_operator_overload(Token_Type::LBrace, left_ty, OPERATION_SUBSCRIPT);
  if (overload != -1) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->info->as<Function_Info>()->return_type;
    return;
  }

  // * Todo: reimplement operator overloads with interfaces.

  auto meta = left_ty->meta;

  if (!meta.is_fixed_sized_array() && !meta.is_pointer()) {
    throw_error(std::format("cannot index into non-array, non-pointer type that doesn't implement 'subscript :: "
                            "fn(self*, idx: u32)' method. {}",
                            left_ty->to_string()),
                node->source_range);
  }

  node->resolved_type = left_ty->get_element_type();
}

void Typer::visit(ASTSwitch *node) {
  node->target->accept(this);
  auto type_id = node->target->resolved_type;
  auto type = global_get_type(type_id);

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->meta.is_pointer()) {
    auto operator_overload = find_operator_overload(Token_Type::EQ, type, OPERATION_BINARY);
    if (operator_overload == -1) {
      throw_error(
          std::format("Can't use a 'switch' statement/expression on a non-scalar, non-enum type that doesn't implement "
                      "Eq (== operator on #self)\ngot type '{}'",
                      type->to_string()),
          node->target->source_range);
    }
  }

  int return_type = void_type();
  int flags = BLOCK_FLAGS_FALL_THROUGH;

  for (const auto &_case : node->cases) {
    _case.expression->accept(this);
    auto expr_type = _case.expression->resolved_type;
    _case.block->accept(this);
    auto block_cf = _case.block->control_flow;
    flags |= block_cf.flags;
    if ((block_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      if (return_type != void_type()) {
        assert_return_type_is_valid(return_type, block_cf.type, node);
      }
      return_type = block_cf.type;
    }

    if (type_is_numerical(type)) {
      continue;
    } else {
      assert_types_can_cast_or_equal(expr_type, type_id, node->source_range, "Invalid switch case.");
    }
  }
  node->resolved_type = node->return_type = return_type;
  if (node->is_statement) {
    node->control_flow = Control_Flow{flags, return_type};
  } else {
    if ((flags & BLOCK_FLAGS_BREAK) != 0) {
      throw_warning(WarningSwitchBreak, "You do not need to break from switch cases.", node->source_range);
    } else if ((flags & BLOCK_FLAGS_CONTINUE) != 0) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->source_range);
    }
  }
}

void Typer::visit(ASTTupleDeconstruction *node) {
  node->right->accept(this);
  node->resolved_type = node->right->resolved_type;

  auto type = global_get_type(node->right->resolved_type);

  if (type->meta.has_extensions()) {
    throw_error("Cannot destructure pointer or array type.", node->source_range);
  }

  if (type->is_kind(TYPE_TUPLE)) {
    auto info = (type->info->as<Tuple_Info>());
    if (node->idens.size() != info->types.size()) {
      throw_error(std::format("Cannot currently partially deconstruct a tuple. "
                              "expected {} identifiers to assign, got {}",
                              info->types.size(), node->idens.size()),
                  node->source_range);
    }
    for (int i = 0; i < node->idens.size(); ++i) {
      auto type = info->types[i];
      auto iden = node->idens[i];
      ctx.scope->insert_variable(iden->value, type, iden);
    }
  } else {
    auto scope = type->info->scope;
    for (const auto &[name, symbol] : scope->symbols) {
      if (symbol.is_function()) {
        continue;
      }
      if (symbol.is_variable()) {
        ctx.scope->insert_variable(name, symbol.type_id, symbol.variable.initial_value.get());
      }
    }
  }

  return;
};

void Typer::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    auto symbol_nullable = get_symbol(node->target);

    if (symbol_nullable.is_null() || !symbol_nullable.get()->is_type()) {
      throw_error("generic `impl![...]` can only be used on types.", node->source_range);
    }

    auto declaring_node = symbol_nullable.get()->type.declaration.get();

    if (declaring_node->node_type != AST_STRUCT) {
      throw_error("generic `impl![...]` can only be used on structs, currently.", node->source_range);
    }

    auto node_as_struct = static_cast<AST *>(declaring_node);
    node_as_struct->impls.push_back(node);
    GENERIC_PANIC_HANDLER(
        data, 1,
        {
          for (auto instantiations : node_as_struct->generic_instantiations) {
            visit_generic(&Typer::visit_impl_declaration, node, instantiations.arguments);
          }
        },
        node->source_range);
  } else {
    visit_impl_declaration(node, false);
  }
  return;
}

void Typer::visit(ASTDefer *node) {
  node->statement->accept(this);
  return;
}

void Typer::visit(ASTCast *node) {
  node->expression->accept(this);
  auto expr_type = global_get_type(node->expression->resolved_type);
  node->target_type->accept(this);
  auto type = global_get_type(node->target_type->resolved_type);
  auto conversion = type_conversion_rule(expr_type, type);
  if (conversion == CONVERT_PROHIBITED) {
    throw_error(std::format("casting {} to {} is strictly prohibited.", expr_type->to_string(), type->to_string()),
                node->source_range);
  }
  node->resolved_type = type->id;
}

int Typer::get_self_type() {
  if (type_context.is_not_null()) {
    type_context.get()->accept(this);
    return type_context.get()->resolved_type;
  }
  return Type::INVALID_TYPE_ID;
}

bool Typer::visit_where_predicate(Type *type, ASTExpr *node) {
  switch (node->node_type) {
    case AST_BINARY: {
      auto bin = static_cast<ASTBinExpr *>(node);
      auto op = bin->op.type;
      if (op == Token_Type::And) {
        return visit_where_predicate(type, bin->left) && visit_where_predicate(type, bin->right);
      } else if (op == Token_Type::Or) {
        return visit_where_predicate(type, bin->left) || visit_where_predicate(type, bin->right);
      } else {
        throw_error("Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
                    "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
                    bin->source_range);
      }
    } break;
    case AST_TYPE: {
      node->accept(this);
      // return whether this type implements this trait or not.
      // also can be used to assert whether it's equal to the type provided or not.
      return std::ranges::find(type->interfaces, node->resolved_type) != type->interfaces.end() ||
             type->id == node->resolved_type;
    } break;
    default:
      throw_error("Invalid node in 'where' clause predicate", node->source_range);
  }
  return false;
}

void Typer::visit(ASTWhere *node) {
  node->target_type->accept(this);
  auto type = global_get_type(node->target_type->resolved_type);
  auto satisfied = visit_where_predicate(type, node->predicate);

  if (!satisfied) {
    throw_error(std::format("'where' clause type constraint not satified for {}", get_unmangled_name(type)),
                node->source_range);
  }
}

int Typer::find_generic_type_of(const Interned_String &base, const std::vector<int> &generic_args,
                                const Source_Range &source_range) {
  ASTStatement *instantiation = nullptr;
  auto symbol = ctx.scope->lookup(base);

  // Probably not a generic type?
  if (!symbol || !symbol->is_type()) {
    return -1;
  }

  auto declaring_node = symbol->type.declaration.get();

  if (!declaring_node) {
    throw_error("internal compiler error: unable to find type's declaring node", source_range);
  }

  auto decl_node_type = declaring_node->node_type;

  GENERIC_PANIC_HANDLER(
      data, 1,
      {
        switch (decl_node_type) {
          case AST_STRUCT:
            instantiation =
                visit_generic(&Typer::visit_struct_declaration, (AST *)declaring_node, generic_args);
            break;
          case AST_FUNCTION:
            instantiation =
                visit_generic(&Typer::visit_function_header, (AST *)declaring_node, generic_args);
            break;
          case AST_INTERFACE_DECLARATION:
            instantiation = visit_generic(&Typer::visit_interface_declaration,
                                          (ASTInterfaceDeclaration *)declaring_node, generic_args);
            break;
          default:
            throw_error("Invalid target to generic args", source_range);
            break;
        }
      },
      source_range);

  if (!instantiation) {
    throw_error("Template instantiation argument count mismatch", source_range);
  }

  GENERIC_PANIC_HANDLER(
      other_data, 2,
      {
        if (decl_node_type == AST_FUNCTION) {
          auto func = static_cast<AST *>(instantiation);
          func->generic_arguments = generic_args;
          visit_function_body(func);
        } else if (decl_node_type == AST_STRUCT) {
          auto struct_decl = static_cast<AST *>(instantiation);
          for (auto impl : struct_decl->impls) {
            if (impl->resolved_type == Type::INVALID_TYPE_ID) {
              visit_generic(&Typer::visit_impl_declaration, impl, generic_args);
            }
          }
        }
      },
      source_range);

  return global_find_type_id(instantiation->resolved_type, {});
}

void Typer::visit(ASTSize_Of *node) {
  node->target_type->accept(this);
  node->resolved_type = u64_type();
}
