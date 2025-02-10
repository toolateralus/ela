#include "ast.hpp"
#include "core.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"

static size_t get_uid() {
  static size_t n = 0;
  return n++;
}

#ifdef USE_GENERIC_PANIC_HANDLER
#define GENERIC_PANIC_HANDLER(data_name, uid, block, source_range)                                                     \
  GenericInstantiationErrorUserData data_name;                                                                         \
  set_panic_handler(generic_instantiation_panic_handler);                                                              \
  set_error_user_data(&data_name);                                                                                     \
  Defer defer_##uid([] { reset_panic_handler(); });                                                                    \
  if (setjmp(data_name.save_state) == 0) {                                                                             \
    /* clang-format off */\
    block                                                                                            \
    /* clang-format on */                                                                                              \
  } else {                                                                                                             \
    handle_generic_error(&data_name, source_range);                                                                    \
  }
#else
#define GENERIC_PANIC_HANDLER(data_name, uid, block, source_range) block
#endif

void handle_generic_error(GenericInstantiationErrorUserData *data, const Source_Range &range) {
  reset_panic_handler();
  throw_error(std::format("Error at definition: {}\nerror: {}",
                          format_source_location(data->definition_range, ERROR_FAILURE, 3), data->message),
              range);
}

auto generic_instantiation_panic_handler(auto msg, auto range, auto void_data) {
  auto data = (GenericInstantiationErrorUserData *)(void_data);
  data->definition_range = range;
  data->message = msg;
  longjmp(data->save_state, 1);
};

// TODO: add a statement in the .accept() function of AST base type
// TODO: where we return the if the resolved type is already calculated?

// TODO: then we'd have to make suer we reset all the resolved types when copying,
// But it would save some double visits possibly.

void assert_types_can_cast_or_equal(const int from, const int to, const Source_Range &source_range,
                                    const std::string &message) {
  auto from_t = global_get_type(from);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t, source_range);
  if (to != from && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' + std::format("expected \"{}\", got \"{}\"", to_t->to_string(), from_t->to_string()),
                source_range);
  }
}

void assert_return_type_is_valid(int &return_type, int new_type, AST *node) {
  if (return_type == Type::INVALID_TYPE_ID) {
    return_type = new_type;
  } else if (new_type != Type::INVALID_TYPE_ID && new_type != return_type) {
    assert_types_can_cast_or_equal(return_type, new_type, node->source_range, "Inconsistent return types in block.");
  }
};

Nullable<Symbol> Typer::get_symbol(AST *node) {
  switch (node->node_type) {
    case AST_SUBSCRIPT:
      return nullptr;
    case AST_TYPE: {
      if (node->type.kind != AST_TYPE_NORMAL) {
        return nullptr;
      }
      return get_symbol(node->type.normal.base);
    }
    case AST_IDENTIFIER:
      return node->parent->lookup(node->identifier);
    case AST_DOT_EXPR: {
      auto &dot = node->dot;
      visit(dot.base);
      auto type = global_get_type(dot.base->resolved_type);
      auto symbol = type->info.scope.lookup(dot.member_name);
      // Implicit dereference, we look at the base scope.
      if (!symbol && type->meta.is_pointer()) {
        type = global_get_type(type->get_element_type());
        symbol = type->info.scope.lookup(dot.member_name);
      }
      return symbol;
    } break;
    case AST_SCOPE_RESOLUTION: {
      auto &srnode = node->scope_resolution;
      visit(srnode.base);
      auto type = global_get_type(srnode.base->resolved_type);
      auto scope = type->info.scope;
      return node->local_lookup(srnode.member_name);
    } break;

    default:
      return nullptr; // TODO: verify this isn't strange.
  }
  return nullptr;
}

std::vector<int> Typer::get_generic_arg_types(const std::vector<AST *> &args) {
  std::vector<int> generic_args;
  for (const auto &arg : args) {
    visit(arg);
    generic_args.push_back(arg->resolved_type);
  }
  return generic_args;
}

void Typer::visit_struct_declaration(AST *node, bool generic_instantiation, std::vector<int> generic_args) {
  auto type_id = node->find_type_id(node->$struct.name, {});

  if (type_id != Type::INVALID_TYPE_ID) {
    auto type = global_get_type(type_id);
    if (type->is_kind(TYPE_STRUCT)) {
      auto info = type->info.$struct;
      if ((info.flags & STRUCT_FLAG_FORWARD_DECLARED) == 0) {
        throw_error("Redefinition of struct", node->source_range);
      }
    } else {
      throw_error("cannot redefine already existing type", node->source_range);
    }
  } else {
    type_id = node->create_struct_type(node->$struct.name, node);
    auto type = global_get_type(type_id);

    // again, we ony do this once.
    for (const auto &param : node->$struct.generic_parameters) {
      // TODO: this is probably wrong. as it's not a scalar or whatever.
      type->info.scope.insert(Symbol::create_type(Type::UNRESOLVED_GENERIC_TYPE_ID, param, TYPE_SCALAR, nullptr));
    }

    if (node->$struct.is_union) {
      // we only do this once so that we can't do funky stuff and rewrite whether this type was a union or not.
      type->info.$struct.flags |= STRUCT_FLAG_IS_UNION;
    }
    if (node->$struct.is_fwd_decl) {
      // We only mark a forward declaration if it's the first occurence of that type.
      // Otherwise if you used a forward declaration in a latter translation unit, you'd like erase
      // the type and make a defined type unusable.
      node->resolved_type = type->id;
      type->info.$struct.flags |= STRUCT_FLAG_FORWARD_DECLARED;
      return;
    }
  }

  auto &$struct = node->$struct;
  auto type = global_get_type(node->resolved_type);
  auto &info = type->info.$struct;

  // if we made it this far, we're definitely not forward declared.
  info.flags &= ~STRUCT_FLAG_FORWARD_DECLARED;

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : $struct.generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      node->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
    type = global_get_type(global_create_struct_type($struct.name, node->scope, generic_args));
  }

  if ($struct.where_clause) {
    visit_where($struct.where_clause.get());
  }

  type->declaring_node = node;

  for (auto subunion : $struct.subtypes) {
    for (const auto &field : subunion->$struct.members) {
      visit_type(field.type);
      node->insert_variable(field.name, field.type->resolved_type, nullptr);
    }
  }
  for (auto alias : $struct.aliases) {
    visit(alias);
  }
  for (auto member : $struct.members) {
    visit_type(member.type);
    node->insert_variable(member.name, member.type->resolved_type, nullptr);
  }
  node->resolved_type = type->id;
  if (type->is_kind(TYPE_SCALAR)) {
    throw_error("struct declaration was a scalar???", node->source_range);
  }
}

void Typer::visit_program(AST *node) {
  for (auto &statement : node->statements) {
    visit(statement);
  }
}

void Typer::visit_block(AST *node) {
  int statement_idx = 0;
  for (auto &statement : node->statements) {
    visit(statement);
    auto &stmnt_cf = statement->control_flow;
    auto &block_cf = node->control_flow;
    block_cf.flags |= stmnt_cf.flags;
    if ((stmnt_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      assert_return_type_is_valid(block_cf.type, stmnt_cf.type, node);
    }
    if ((stmnt_cf.flags & BLOCK_FLAGS_FALL_THROUGH) == 0) {
      block_cf.flags &= ~BLOCK_FLAGS_FALL_THROUGH;
    }
    statement_idx++;
  }
  node->block.flags = node->control_flow.flags;
  node->block.return_type = node->control_flow.type == Type::INVALID_TYPE_ID ? void_type() : node->control_flow.type;
}

void Typer::visit_function_body(AST *node) {
  auto old_ty = expected_type;
  expected_type = node->function.return_type->resolved_type;
  auto block = node->function.block.get();
  if (!block) {
    throw_error("internal compiler error: attempting to visit body of function forward declaration.",
                node->source_range);
  }
  visit(block);
  auto control_flow = block->control_flow;
  if (control_flow.type == Type::INVALID_TYPE_ID)
    control_flow.type = void_type();
  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0)
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0)
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 && node->function.return_type->resolved_type != void_type())
    throw_error("Not all code paths return a value.", node->source_range);
  assert_types_can_cast_or_equal(control_flow.type, node->function.return_type->resolved_type, node->source_range,
                                 std::format("invalid return type", node->function.name.get_str()));
}

void Typer::visit_function_declaration(AST *node) {
  if (!node->function.generic_parameters.empty()) {
    // TODO: actually generate a signature for a generic function so that you can compare them
    node->parent->insert_function(node->function.name, Type::UNRESOLVED_GENERIC_TYPE_ID, node);
    return;
  }

  visit_function_header(node, false);

  if ((node->function.flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
    node->parent->insert_function(node->function.name, node->resolved_type, node,
                                  SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
    return;
  }

  node->parent->insert_function(node->function.name, node->resolved_type, node);

  if ((node->function.flags & FUNCTION_IS_FOREIGN) != 0) {
    return;
  }

  visit_function_body(node);
}

void Typer::visit_impl_declaration(AST *node, bool generic_instantiation, std::vector<int> generic_args) {
  auto old_type = type_context;
  type_context = node->target;
  Defer _([&] { type_context = old_type; });

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->impl.generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      node->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
  }

  if (node->impl.where_clause) {
    visit(node->impl.where_clause.get());
  }

  visit(node->impl.target);

  auto target_ty = global_get_type(node->impl.target->resolved_type);
  if (!target_ty) {
    throw_error("use of undeclared type", node->impl.target->source_range);
  }
  Type *interface_ty = nullptr;

  if (node->impl.interface) {
    visit(node->impl.interface.get());
    auto type_id = node->impl.interface.get()->resolved_type;
    if (type_id == Type::INVALID_TYPE_ID) {
      throw_error("internal compiler error: type of impl interface was invalid", node->source_range);
    }
    interface_ty = global_get_type(type_id);
  }

  auto &type_scope = target_ty->info.scope;
  Scope impl_scope = {};
  for (const auto &method : node->impl.methods) {
    method->function.declaring_type = target_ty->id;
    if (!method->function.generic_parameters.empty()) {

      // TODO: actually generate a signature for a generic function so that you can compare them
      type_scope.insert(Symbol::create_function(method->function.name, method->resolved_type, method, SYMBOL_IS_FUNCTION));
      impl_scope.insert(*type_scope.lookup(method->function.name));
      continue;
    }
    visit_function_header(method, false);
    auto func_ty_id = method->resolved_type;
    if (auto symbol = type_scope.lookup(method->function.name)) {
      if (!(symbol->flags & SYMBOL_IS_FORWARD_DECLARED)) {
        throw_error("Redefinition of method", method->source_range);
      } else {
        symbol->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
      }
    } else {
      if ((method->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        type_scope->insert_function(method->name, method->resolved_type, method, SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
      } else {
        type_scope->insert_function(method->name, method->resolved_type, method);
      }
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      if (method->flags & FUNCTION_IS_FOREIGN || method->flags & FUNCTION_IS_FORWARD_DECLARED) {
        continue;
      }
    }
    visit_function_body(method);
  }

  if (interface_ty) {
    auto declaring_node = interface_ty->declaring_node.get();
    if (!declaring_node || declaring_node->node_type != AST_INTERFACE_DECLARATION) {
      throw_error(
          std::format("\'impl <interface> for <type>\' must implement an interface. got {}", interface_ty->to_string()),
          node->source_range);
    }
    auto interface = static_cast<ASTInterfaceDeclaration *>(declaring_node);
    interface = (ASTInterfaceDeclaration *)deep_copy_ast(interface);
    ctx.set_scope(interface->scope);
    for (auto &decl : interface->methods) {
      decl->accept(this);
    }
    ctx.set_scope(node->scope);
    for (auto &[name, interface_sym] : interface->scope->symbols) {
      if (!interface_sym.is_function()) continue;

      if (auto impl_symbol = impl_scope.local_lookup(name)) {
        if (interface_sym.type_id != impl_symbol->type_id) {
          if (interface_sym.type_id != Type::INVALID_TYPE_ID && impl_symbol->type_id != Type::INVALID_TYPE_ID) {
            throw_error(std::format("method \"{}\" doesn't match interface.\nexpected {}, got {}", name,
                                    global_get_type(interface_sym.type_id)->to_string(),
                                    global_get_type(impl_symbol->type_id)->to_string()),
                        node->source_range);
          } else {
            throw_error("internal compiler error: method.type_id or impl_symbol.type_id was null", node->source_range);
          }
        }
      } else {
        throw_error(std::format("required method \"{}\" (from interface {}) not implemented in impl", name,
                                interface_ty->to_string()),
                    node->source_range);
      }
    }
    for (auto &[name, impl_sym] : impl_scope.symbols) {
      if (!interface->scope->local_lookup(name)) {
        throw_error(std::format("impl method \"{}\" not found in interface", name), node->source_range);
      }
    }
    target_ty->interfaces.push_back(interface_ty->id);
  }

  node->resolved_type = target_ty->id;
}

void Typer::visit_function_header(AST *node, bool generic_instantiation,
                                     std::vector<int> generic_args) {

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->function.generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      node->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
  }

  if (node->function.where_clause) {
    visit(node->function.where_clause.get());
  }

  visit(node->function.return_type);
  visit_parameters(node->source_range, node->function.parameters);

  Function_Info info;
  // Get function type id from header.
  info.return_type = node->function.return_type->resolved_type;
  info.is_varargs = (node->function.flags & FUNCTION_IS_VARARGS) != 0;

  for (const auto &param : node->function.parameters) {
    if (param.tag == AST_PARAM_NORMAL) {
      auto &normal = param.normal;
      node->insert_variable(normal.name, param.resolved_type, nullptr);
      info.parameter_types.push_back(param.resolved_type);
    } else {
      auto type = get_self_type();
      if (param.self.is_pointer) {
        type = global_get_type(type)->take_pointer_to();
      }
      node->insert_variable("self", type, nullptr);
      info.parameter_types.push_back(type);
    }
  }

  if (info.return_type == Type::UNRESOLVED_GENERIC_TYPE_ID) {
    throw_error("internal compiler error: unresolved generic return type.", node->source_range);
  }

  node->resolved_type = global_find_function_type_id(info, {});
}

void Typer::visit_parameters(Source_Range source_range, std::vector<AST_Parameter_Declaration> &params) {
  for (auto &param : params) {
    if (param.tag == AST_PARAM_SELF) {
      if (!type_context) {
        throw_error("No target type for self", source_range);
      }
      param.resolved_type = get_self_type();
      if (param.self.is_pointer) {
        param.resolved_type = global_get_type(param.resolved_type)->take_pointer_to();
      }
      return;
    } else {
      visit_type(param.normal.type);
      int id = param.normal.type->resolved_type;
      param.resolved_type = id;

      if (id == Type::INVALID_TYPE_ID) {
        throw_error("Use of undeclared type.", source_range);
      }

      auto type = global_get_type(id);

      if (type->meta.is_fixed_sized_array()) {
        throw_warning(WarningDownCastFixedArrayParam,
                      "using a fixed array as a function parameter: note, this "
                      "casts the length information off and gets passed as as "
                      "pointer. Consider using a dynamic array",
                      source_range);
        // cast off the fixed size array and add a pointer to it,
        // for s8[] to s8*
        {
          auto element = type->get_element_type();
          param.resolved_type = global_get_type(element)->take_pointer_to();
        }
      }

      auto old_ty = expected_type;
      expected_type = id;
      Defer _defer([&] { expected_type = old_ty; });
    }
  }
}

std::vector<int> Typer::visit_arguments(Source_Range source_range, AST *scope, std::vector<AST *> &arguments) {
  auto type = global_get_type(expected_type);
  std::vector<int> types;
  Function_Info *info = nullptr;
  if (type) {
    info = &type->info.function;
  }
  for (int i = 0; i < arguments.size(); ++i) {
    auto arg = arguments[i];
    if (arg->node_type == AST_SWITCH || arg->node_type == AST_IF) {
      throw_error(
          "cannot use 'switch' or 'if' expressions in binary expressions, only `=`, `:=` and `return` statements",
          source_range);
    }
    if (!info) {
      visit(arg);
      types.push_back(arg->resolved_type);
      continue;
    }
    auto old_ty = expected_type;
    expected_type = info->parameter_types[i];
    Defer _defer([&] { expected_type = old_ty; });
    visit(arg);
    types.push_back(arg->resolved_type);
  }
  return types;
}

void Typer::compiler_mock_function_call_visit_impl(AST *scope, int left_type, const Interned_String &method_name) {
  AST call(AST_CALL);
  AST left(AST_IDENTIFIER);
  static int depth = 0;

  left.identifier = "$$temp$$" + std::to_string(depth++);
  scope->insert_variable(left.identifier, left_type, nullptr);

  Defer erase_temp_symbol([&] {
    depth--;
    scope->scope.erase("$$temp$$");
  });

  call.call.arguments = {&left};

  // .method
  AST dot(AST_DOT_EXPR);
  dot.dot.base = &left;
  dot.dot.member_name = method_name;
  call.call.function = &dot;
  visit_call(&call);
}

bool expr_is_literal(AST *expr) {
  switch (expr->node_type) {
    case AST_BINARY:
      return expr_is_literal(expr->binary.left) && expr_is_literal(expr->binary.right);
    case AST_UNARY_EXPR:
      return expr_is_literal(expr->unary.operand);
    case AST_LITERAL:
      return true;
    default:
      return false;
  }
}

void Typer::visit_noop(AST *node) {
  return; // It's a no-op, do nothing.
}

void Typer::visit_declaration(AST *node) {
  if (node->find_type_id(node->declaration.name, {}) != Type::INVALID_TYPE_ID ||
      keywords.contains(node->declaration.name.get_str())) {
    throw_error("Invalid variable declaration: a type or keyword exists with "
                "that name,",
                node->source_range);
  }

  auto &decl = node->declaration;
  // Inferred declaration.
  if (decl.type == nullptr) {
    if (decl.value.get()->node_type == AST_TYPE && decl.value.get()->type.kind == AST_TYPE_REFLECTION) {
      throw_error("Cannot use a type as a value.", decl.value.get()->source_range);
    }
    visit(decl.value.get());
    auto value_ty = decl.value.get()->resolved_type;
    if (value_ty == void_type()) {
      throw_error("Cannot assign a variable with value type of 'void'", node->source_range);
    }
    auto type = global_get_type(value_ty);

    // CLEANUP: This is nonsense.
    decl.type = ast_alloc(AST_TYPE, node->parent);
    decl.type->source_range = node->source_range;
    decl.type->resolved_type = value_ty;
    node->resolved_type = value_ty;

    // TODO: so, we just don't set the type if it can't be assigned to int??? what?
    // I don't get this and I don't care to read it right now I'm doing a massive refactor.
    if (type->is_kind(TYPE_SCALAR) && type->meta.has_no_extensions() && expr_is_literal(decl.value.get())) {
      auto &info = type->info.scalar;
      auto rule = type_conversion_rule(type, global_get_type(s32_type()), node->source_range);
      if (info.is_integral && rule != CONVERT_PROHIBITED && rule != CONVERT_EXPLICIT) {
        decl.type->resolved_type = s32_type();
      }
    }
  }

  visit_type(decl.type);

  if (decl.type->resolved_type == Type::INVALID_TYPE_ID) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }

  if (decl.value.is_not_null()) {
    if (decl.value.get()->node_type == AST_TYPE && decl.value.get()->type.kind == AST_TYPE_REFLECTION) {
      throw_error("Cannot use a type as a value.", decl.value.get()->source_range);
    }
    auto old_ty = expected_type;
    expected_type = decl.type->resolved_type;
    Defer _defer([&] { expected_type = old_ty; });
    visit(decl.value.get());
    auto expr_type = decl.value.get()->resolved_type;
    assert_types_can_cast_or_equal(expr_type, decl.type->resolved_type, node->source_range,
                                   "invalid type in declaration");
  }

  auto symbol = node->lookup(decl.name);
  symbol->type_id = decl.type->resolved_type;
  auto type = global_get_type(decl.type->resolved_type);
  if (symbol->type_id == void_type() || decl.type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", decl.name.get_str()), node->source_range);
  }
}

void Typer::visit_interface_declaration(AST *node) {
  if (!node->interface.generic_parameters.empty()) {
    node->parent->declare_interface(node->interface.name, node);
  } else {
    visit_interface_declaration(node, false);
    node->parent->create_type_alias(node->interface.name, node->resolved_type, TYPE_INTERFACE, node);
  }
  return;
}

void Typer::visit_struct_declaration(AST *node) {
  if (!node->$struct.generic_parameters.empty()) {
    node->parent->create_type_alias(node->$struct.name, Type::UNRESOLVED_GENERIC_TYPE_ID, TYPE_STRUCT, node);
  } else {
    visit_struct_declaration(node, false);
  }
}

void Typer::visit_enum_declaration(AST *node) {
  auto elem_type = Type::INVALID_TYPE_ID;

  if (node->find_type_id(node->$enum.name, {}) != Type::INVALID_TYPE_ID) {
    throw_error("Redefinition of enum " + node->$enum.name.get_str(), node->source_range);
  }

  Type_Info info{Enum_Info{}};
  for (const auto &[key, value] : node->$enum.key_values) {
    visit(value);
    auto node_ty = value->resolved_type;
    info.scope.insert(Symbol::create_variable(key, node_ty, value, node));
    if (elem_type == Type::INVALID_TYPE_ID) {
      elem_type = node_ty;
    } else {
      assert_types_can_cast_or_equal(node_ty, elem_type, node->source_range, "inconsistent types in enum declaration.");
    }
  }
  if (elem_type == void_type()) {
    throw_error("Invalid enum declaration.. got null or no type.", node->source_range);
  }

  node->$enum.element_type = elem_type;
  info.$enum.element_type = elem_type;
  node->resolved_type = node->parent->create_enum_type(node->$enum.name, node->$enum.is_flags, node);
}

void Typer::visit_return(AST *node) {
  int type;
  if (node->$return.is_not_null()) {
    visit(node->$return.get());
    type = node->$return.get()->resolved_type;
  } else {
    type = void_type();
  }
  node->resolved_type = type;
  node->control_flow = Control_Flow{BLOCK_FLAGS_RETURN, type};
}

void Typer::visit_continue(AST *node) {
  node->control_flow = Control_Flow{BLOCK_FLAGS_CONTINUE, Type::INVALID_TYPE_ID};
}

void Typer::visit_break(AST *node) { node->control_flow = Control_Flow{BLOCK_FLAGS_BREAK, Type::INVALID_TYPE_ID}; }

void Typer::visit_bin_expr(AST *node) {
  auto &binary = node->binary;

  visit(binary.left);
  auto left = binary.left->resolved_type;

  auto old_ty = expected_type;
  Defer _defer([&] { expected_type = old_ty; });

  if (binary.op == Token_Type::Assign) {
    expected_type = left;
  } else if (binary.left->node_type == AST_SWITCH || binary.right->node_type == AST_SWITCH ||
             binary.right->node_type == AST_IF || binary.left->node_type == AST_IF) {
    throw_error("cannot use 'switch' or 'if' expressions in function arguments, they're only valid in `=`, `:=` and "
                "`return` statements",
                node->source_range);
  }

  visit(binary.right);
  auto right = binary.right->resolved_type;

  if (binary.op == Token_Type::Assign) {
    if (binary.left->node_type == AST_IDENTIFIER) {
      node->insert_variable(binary.left->identifier, binary.left->resolved_type, binary.right);
    }
  }

  auto left_ty = global_get_type(left);

  auto operator_overload_ty = find_operator_overload(binary.op, left_ty, OPERATION_BINARY);
  if (operator_overload_ty != -1) {
    binary.is_operator_overload = true;
    node->resolved_type = global_get_type(operator_overload_ty)->info.function.return_type;
    return;
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have
  // their operands type checked, but right now that would involve casting
  // scalars to each other, which makes no  sense.
  if (is_relational(binary.op)) {
    node->resolved_type = bool_type();
  } else {
    auto left_t = global_get_type(left);
    auto right_t = global_get_type(right);
    auto conv_rule_0 = type_conversion_rule(left_t, right_t, binary.left->source_range);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t, binary.right->source_range);

    if (((conv_rule_0 == CONVERT_PROHIBITED) && (conv_rule_1 == CONVERT_PROHIBITED)) ||
        ((conv_rule_0 == CONVERT_EXPLICIT) && (conv_rule_1 == CONVERT_EXPLICIT))) {
      throw_error(std::format("Type error in binary expression: cannot convert between {} and {}", left_t->to_string(),
                              right_t->to_string()),
                  node->source_range);
    }
    // TODO: is this correct??? do we even need to assign that here?
    node->resolved_type = left;
  }
}

void Typer::visit_unary_expr(AST *node) {
  auto &unary = node->unary;

  if (unary.operand->node_type == AST_SWITCH || unary.operand->node_type == AST_IF) {
    throw_error("cannot use 'switch' or 'if' expressions in unary expressions. they're only valid in `=`, `:=` and "
                "`return` statements",
                node->source_range);
  }

  visit(unary.operand);
  auto operand_ty = unary.operand->resolved_type;

  auto type = global_get_type(operand_ty);

  auto overload = find_operator_overload(unary.op, type, OPERATION_UNARY);
  if (overload != -1) {
    unary.is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->info.function.return_type;
    return;
  }

  // Address-Of.
  if (unary.op == Token_Type::And) {
    node->resolved_type = global_get_type(operand_ty)->take_pointer_to();
    return;
  }

  // Dereference.
  if (unary.op == Token_Type::Mul) {
    auto type = global_get_type(operand_ty);
    if (type->meta.is_pointer()) {
      node->resolved_type = type->get_element_type();
      return;
    } else {
      throw_error(std::format("Cannot dereference a non-pointer type, got \"{}\"", type->to_string()),
                  node->source_range);
    }
  }

  // unary operator overload.
  auto left_ty = global_get_type(operand_ty);

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule =
        type_conversion_rule(global_get_type(operand_ty), global_get_type(bool_type()), unary.operand->source_range);
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED && conversion_rule != CONVERT_EXPLICIT);

    if (unary.op == Token_Type::LogicalNot && can_convert) {
      node->resolved_type = bool_type();
      return;
    }
  }

  node->resolved_type = operand_ty;
}

void Typer::visit_identifier(AST *node) {
  node->resolved_type = node->parent->find_type_id(node->identifier, {});
  if (node->resolved_type != Type::INVALID_TYPE_ID) {
    return;
  }
  auto symbol = node->parent->lookup(node->identifier);
  if (symbol) {
    node->resolved_type = symbol->type_id;
  } else {
    throw_error(std::format("Use of undeclared identifier '{}'", node->identifier), node->source_range);
  }
}

void Typer::visit_literal(AST *node) {
  auto &literal = node->literal;
  switch (literal.tag) {
    case LITERAL_INTEGER: {
      int base = 10;
      auto value = literal.value.get_str();
      if (value.starts_with("0x")) {
        base = 0;
      }

      if (value.starts_with("0b")) {
        value = value.substr(2, value.length());
        base = 2;
      }
      auto n = std::strtoll(value.c_str(), nullptr, base);

      if (expected_type != Type::INVALID_TYPE_ID) {
        auto type = global_get_type(expected_type);
        if (type->is_kind(TYPE_SCALAR) && type_is_numerical(type)) {
          if (type->info.scalar.is_integral) {
            node->resolved_type = type->id;
            return;
          }
        }
      }
      node->resolved_type = s32_type();
      return;
    }
    case LITERAL_FLOAT:
      node->resolved_type = f32_type();
      return;
    case LITERAL_STRING: {
      if (literal.is_c_string) {
        static int type = global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER}}});
        node->resolved_type = type;
      } else {
        static size_t uid_idx = 0;
        static int type = node->find_type_id("str", {});
        node->resolved_type = type;
      }
      return;
    }
    case LITERAL_BOOL:
      node->resolved_type = bool_type();
      return;
    case LITERAL_NULL:
      // infer pointer type from decl or assign type, else we just use void*, for like n := null;
      if (expected_type != -1) {
        node->resolved_type = expected_type;
        return;
      }
      node->resolved_type = voidptr_type();
      return;
    case LITERAL_CHAR:
      node->resolved_type = u32_type();
      return;
  }
}

void Typer::visit_type(AST *node) {}

void Typer::visit_tuple(AST *node) {
  auto &tuple = node->tuple;
  std::vector<int> types;
  auto declaring_tuple = global_get_type(expected_type);

  int type_index = 0;
  for (const auto &v : tuple) {
    auto old = expected_type;
    Defer _([&] { expected_type = old; });
    if (declaring_tuple && declaring_tuple->is_kind(TYPE_TUPLE)) {
      auto info = declaring_tuple->info.tuple;
      if (info.types.size() < type_index) {
        throw_error(std::format("too many expressions provided to tuple\ntuple type {}", declaring_tuple->to_string()),
                    v->source_range);
      }
      expected_type = info.types[type_index];
    }
    visit(v);
    types.push_back(v->resolved_type);
    type_index++;
  }
  static constexpr const Type_Metadata extensions;
  node->resolved_type = global_find_type_id(types, extensions);
}

void Typer::visit_call(AST *node) {}

void Typer::visit_for(AST *node) {}
void Typer::visit_if(AST *node) {}
void Typer::visit_else(AST *node) {}
void Typer::visit_while(AST *node) {}
void Typer::visit_dot_expr(AST *node) {}
void Typer::visit_scope_resolution(AST *node) {}
void Typer::visit_subscript(AST *node) {}

void Typer::visit_initializer_list(AST *node) {
  auto &initializer = node->initializer;
  Type *target_type;
  if (initializer.target_type.is_null()) {
    target_type = global_get_type(expected_type);
  } else {
    visit(initializer.target_type.get());
    target_type = global_get_type(initializer.target_type.get()->resolved_type);
  }
  if (!target_type) {
    throw_error("Can't use initializer list, no target type was provided", node->source_range);
  }
  if (target_type->meta.is_pointer() || target_type->is_kind(TYPE_SCALAR) && target_type->meta.has_no_extensions()) {
    throw_error(std::format("Cannot use an initializer list on a pointer, or a scalar type (int/float, etc) that's "
                            "not an array\n\tgot {}",
                            target_type->to_string()),
                node->source_range);
  }

  switch (initializer.tag) {
    case INITIALIZER_NAMED: {
      if (!target_type->is_kind(TYPE_STRUCT)) {
        throw_error(std::format("named initializer lists can only be used for structs & unions, got type {}\nNote, for "
                                "unions, you can only provide one value.",
                                target_type->to_string()),
                    node->source_range);
      }

      // @Cleanup this is useful for returning a default value.
      // we would probably prefer a type::default(),
      // but for now we'll leave it.
      if (initializer.key_values.empty()) {
        node->resolved_type = target_type->id;
        return;
      }

      auto &scope = target_type->info.scope;

      for (const auto &[id, value] : initializer.key_values) {
        auto old = expected_type;
        Defer _([&] { expected_type = old; });
        auto symbol = scope.lookup(id);
        if (!symbol)
          throw_error(std::format("Invalid named initializer list: couldn't find {}", id), node->source_range);

        if (symbol->is_function()) {
          throw_error(std::format("Cannot initialize a function :: ({}) with an initializer list.", id),
                      value->source_range);
        }
        expected_type = symbol->type_id;

        visit(value);
        auto value_ty = value->resolved_type;
        assert_types_can_cast_or_equal(value_ty, symbol->type_id, value->source_range,
                                       "Unable to cast type to target field for named initializer list");
        value->resolved_type = symbol->type_id; // Again, we do this here to avoid annoyances with lowering to c++
      }
    } break;
    case INITIALIZER_COLLECTION: {
      // TODO:
      // We can support these types of initializer lists, by creating something in-language like
      // Init_List :: struct![T] {  ptr: T*; length: u64; } and passing this 'dynamic' array to a special function
      if (target_type->meta.is_fixed_sized_array()) {
        auto &values = initializer.values;
        // Zero init construction. Pretty redundant.
        if (values.empty()) {
          node->resolved_type = target_type->id;
          return;
        }

        auto target_element_type = target_type->get_element_type();
        visit(values[0]);
        auto element_type = values[0]->resolved_type;
        for (int i = 1; i < values.size(); ++i) {
          int type = Type::INVALID_TYPE_ID;
          if (values[i]->node_type == AST_INITIALIZER) {
            auto old = expected_type;
            Defer _([&] { expected_type = old; });
            expected_type = target_element_type;
            visit(values[i]);
            type = values[i]->resolved_type;
          } else {
            visit(values[i]);
            type = values[i]->resolved_type;
          }
          assert_types_can_cast_or_equal(
              type, element_type, values[i]->source_range,
              "Found inconsistent types in a collection-style initializer list. These types must be homogenous");

          values[i]->resolved_type =
              target_element_type; // We do this here to avoid casting problems with C/C++ init lists.
        }

        auto element_ty_ptr = global_get_type(element_type);
        auto target_element_ty_ptr = global_get_type(target_element_type);

        if (element_ty_ptr->is_kind(TYPE_SCALAR) && element_ty_ptr->meta.has_no_extensions() &&
            target_element_ty_ptr->is_kind(TYPE_SCALAR) && target_element_ty_ptr->meta.has_no_extensions()) {
          auto target_info = target_element_ty_ptr->info.scalar;
          auto elem_info = element_ty_ptr->info.scalar;

          // We allow implicit downcasting/ sign casting, just to prevent annoyances.
          if (target_info.is_integral && elem_info.is_integral) {
            node->resolved_type = target_type->id;
            return;
          }
        }

        assert_types_can_cast_or_equal(
            element_type, target_element_type, node->source_range,
            "Failed to assign element type from value passed into collection-style initializer list");
        node->resolved_type = target_type->id;
      } else {
        if (!target_type->implements("Init") && !target_type->base.get_str().contains("Init_List$")) {
          throw_error("Unable to use 'collection style' initalizer lists on non fixed array types that don't implement "
                      "Init![T] interface",
                      node->source_range);
        }
        auto &values = initializer.values;
        // * How on earth will we infer this?
        // * I think we'll have to look at the target type,
        // * search for the init_list() function, check if it's generic,
        // * if it's not, use the concrete type argument for the Init_List![T] argument,
        // * otherwise if it is generic,
        // * we just allow any homogenous collection of values?
        // For now, we just do the latter - allow any collection of values.
        visit(values[0]);
        auto target_element_type = values[0]->resolved_type;
        for (int i = 1; i < values.size(); ++i) {
          int type = Type::INVALID_TYPE_ID;
          if (values[i]->node_type == AST_INITIALIZER) {
            auto old = expected_type;
            Defer _([&] { expected_type = old; });
            expected_type = target_element_type;
            visit(values[i]);
            type = values[i]->resolved_type;
          } else {
            visit(values[i]);
            type = values[i]->resolved_type;
          }
          assert_types_can_cast_or_equal(
              type, target_element_type, values[i]->source_range,
              "Found inconsistent types in a collection-style initializer list. These types must be homogenous");

          values[i]->resolved_type = target_element_type;
        }
        node->resolved_type = find_generic_type_of("Init_List", {target_element_type}, node->source_range);
        return;
      }
    } break;
    case INITIALIZER_EMPTY:
      node->resolved_type = target_type->id;
      return;
  }
  node->resolved_type = target_type->id;
}

void Typer::visit_alias(AST *node) {
  visit_type(node->alias.type);
  if (node->alias.type->resolved_type == Type::INVALID_TYPE_ID) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }
  if (node->local_lookup(node->alias.name)) {
    throw_error("Redeclaration of type", node->source_range);
  }
  auto type = global_get_type(node->alias.type->resolved_type);
  node->create_type_alias(node->alias.name, node->alias.type->resolved_type, type->kind, node);
}

void Typer::visit_impl(AST *node) {}

void Typer::visit_size_of(AST *node) {}

void Typer::visit_defer(AST *node) {}

void Typer::visit_cast(AST *node) {}

void Typer::visit_lambda(AST *node) {
  auto &lambda = node->lambda;
  lambda.unique_identifier = "$lambda$" + std::to_string(LAMBDA_UNIQUE_ID++);
  visit_parameters(node->source_range, lambda.parameters);
  visit(lambda.return_type);
  Function_Info info;
  int parameter_index = 0;
  for (const auto &param : lambda.parameters) {
    info.parameter_types.push_back(param.resolved_type);
    lambda.block->insert_variable(param.normal.name, param.resolved_type, nullptr);
    parameter_index++;
  }
  visit_block(lambda.block);
  info.return_type = lambda.return_type->resolved_type;
  auto type = global_find_function_type_id(info, {});
  node->resolved_type = global_get_type(type)->take_pointer_to();
}

void Typer::visit_range(AST *node) {
  auto &range = node->range;
  visit(range.left);
  visit(range.right);

  auto left = range.left->resolved_type;
  auto right = range.right->resolved_type;

  auto conversion_rule_left_to_right = type_conversion_rule(global_get_type(left), global_get_type(right));
  auto conversion_rule_right_to_left = type_conversion_rule(global_get_type(right), global_get_type(left));

  // Alwyas cast to the left? or should we upcast to the largest number type?
  if (conversion_rule_left_to_right == CONVERT_NONE_NEEDED || conversion_rule_left_to_right == CONVERT_IMPLICIT) {
    right = range.right->resolved_type = left;
  } else if (conversion_rule_right_to_left == CONVERT_NONE_NEEDED ||
             conversion_rule_right_to_left == CONVERT_IMPLICIT) {
    left = range.left->resolved_type = right;
  } else {
    throw_error("Can only use ranges when both types are implicitly castable to each other. Range will always take the "
                "left side's type",
                node->source_range);
  }

  node->resolved_type = find_generic_type_of("Range_Base", {left}, node->source_range);

  if (node->resolved_type == -1) {
    throw_error(std::format("Unable to find range type for `{}..{}`", global_get_type(left)->to_string(),
                            global_get_type(right)->to_string()),
                node->source_range);
  }
}

void Typer::visit_switch(AST *node) {}

void Typer::visit_tuple_deconstruction(AST *node) {}

void Typer::visit_where(AST *node) {};