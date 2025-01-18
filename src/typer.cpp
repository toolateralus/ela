
#include <cassert>
#include <format>
#include <ranges>
#include <string>
#include <vector>

#include "ast.hpp"
#include "ast_copier.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"

// TODO: add a statement in the .accept() function of ASTNode base type
// TODO: where we return the if the resolved type is already calculated?

// TODO: then we'd have to make suer we reset all the resolved types when copying,
// But it would save some double visits possibly.

void assert_types_can_cast_or_equal(const int from, const int to, const SourceRange &source_range,
                                    const std::format_string<std::string, std::string> &format,
                                    const std::string &message) {
  auto from_t = global_get_type(from);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t, source_range);
  if (to != from && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' + std::format(format, to_t->to_string(), from_t->to_string()), source_range);
  }
}

void assert_return_type_is_valid(int &return_type, int new_type, ASTNode *node) {
  if (return_type == -1) {
    return_type = new_type;
  } else if (new_type != -1 && new_type != return_type) {
    assert_types_can_cast_or_equal(new_type, return_type, node->source_range, "Expected: {}, Found: {}",
                                   "Inconsistent return types in block.");
  }
};

Nullable<Symbol> Typer::get_symbol(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_SUBSCRIPT:
      return nullptr;
    case AST_NODE_TYPE: {
      auto type_node = static_cast<ASTType *>(node);
      if (type_node->kind != ASTType::NORMAL) {
        return nullptr;
      }
      return get_symbol(type_node->normal.base);
    }
    case AST_NODE_IDENTIFIER:
      return ctx.scope->lookup(static_cast<ASTIdentifier *>(node)->value);
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      dotnode->base->accept(this);
      auto type = global_get_type(dotnode->base->resolved_type);
      auto scope = type->get_info()->scope;
      return scope->local_lookup(dotnode->member_name);
    } break;
    case AST_NODE_SCOPE_RESOLUTION: {
      auto srnode = static_cast<ASTScopeResolution *>(node);
      srnode->base->accept(this);
      auto type = global_get_type(srnode->resolved_type);
      auto scope = type->get_info()->scope;
      return scope->local_lookup(srnode->member_name);
    } break;

    default:
      throw_error("Get symbol cannot be used on this node type", node->source_range);
  }
  return nullptr;
}

void Typer::visit(ASTProgram *node) {
  for (auto &statement : node->statements)
    statement->accept(this);
  return;
}

std::vector<int> Typer::get_generic_arg_types(const std::vector<ASTType *> &args) {
  std::vector<int> generic_args;
  for (const auto &arg : args) {
    arg->accept(this);
    generic_args.push_back(arg->resolved_type);
  }
  return generic_args;
}

int find_generic_instance(std::vector<GenericInstance> instantiations, const std::vector<int> &gen_args) {
  for (auto &instantiation : instantiations) {
    if (instantiation.arguments == gen_args) {
      return instantiation.type;
    }
  }
  return -1;
}

int Typer::visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                    std::vector<int> generic_args) {
  auto type = global_get_type(node->resolved_type);
  auto info = (type->get_info()->as<StructTypeInfo>());

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0 || node->is_fwd_decl) {
    return -1;
  }

  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      ctx.scope->types[param] = *generic_arg;
      generic_arg++;
    }
    type = global_get_type(global_create_struct_type(node->name, node->scope, generic_args));
  }

  type->declaring_node = node;
  ctx.scope->insert("this", type->take_pointer_to(), node);

  for (auto subunion : node->unions) {
    for (const auto &field : subunion->fields) {
      field->accept(this);
      node->scope->insert(field->name, field->type->resolved_type, field);
    }
  }
  for (auto decl : node->fields) {
    decl->accept(this);
  }

  ctx.set_scope(old_scope);
  return type->id;
}

// TODO: remove union declaration node and UnionTypeInfo and TYPE_UNION,
// we should just use struct & just have a union flag for various semantic differences
// and emit time stuff. It will make things a lot more solid and reduce a ton of code.
int Typer::visit_union_declaration(ASTUnionDeclaration *node, bool generic_instantiation,
                                   std::vector<int> generic_args) {
  if (node->is_fwd_decl) {
    return -1;
  }

  auto type = global_get_type(node->resolved_type);

  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      ctx.scope->types[param] = *generic_arg;
      generic_arg++;
    }
    type = global_get_type(global_create_union_type(node->name, node->scope, UNION_IS_NORMAL));
  }

  ctx.scope->insert("this", type->take_pointer_to(), node);

  for (const auto &_struct : node->structs) {
    for (const auto &field : _struct->fields) {
      field->accept(this);
      node->scope->insert(field->name, field->type->resolved_type, field);
    }
  }

  for (const auto &field : node->fields) {
    field->accept(this);
  }

  ctx.set_scope(old_scope);
  return type->id;
}

void Typer::visit_function_body(ASTFunctionDeclaration *node, int return_type) {
  auto old_ty = declaring_or_assigning_type;
  auto old_scope = ctx.scope;
  auto _defer = Defer([&] {
    ctx.set_scope(old_scope);
    declaring_or_assigning_type = old_ty;
  });
  ctx.set_scope(node->scope);
  declaring_or_assigning_type = return_type;
  auto block = node->block.get();
  if (!block) {
    // TODO: remove this? expression body does not imply there is no body.
    throw_error("Expression bodies not yet supported", node->source_range);
  }
  block->accept(this);
  auto control_flow = block->control_flow;
  if (control_flow.type == -1)
    control_flow.type = void_type();
  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0)
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0)
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 && return_type != void_type())
    throw_error("Not all code paths return a value.", node->source_range);
  assert_types_can_cast_or_equal(control_flow.type, return_type, node->source_range,
                                 "invalid return type.. expected '{}', got '{}'",
                                 std::format("function: '{}'", node->name.get_str()));
}

int Typer::visit_function_signature(ASTFunctionDeclaration *node, bool generic_instantiation,
                                    std::vector<int> generic_args) {
  // Setup context.
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  Defer _([&] { ctx.set_scope(old_scope); });

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      ctx.scope->types[param] = *generic_arg;
      generic_arg++;
    }
  }

  node->return_type->accept(this);
  node->params->accept(this);

  FunctionTypeInfo info;
  // Get function type id from header.
  info.return_type = node->return_type->resolved_type;
  info.is_varargs = (node->flags & FUNCTION_IS_VARARGS) != 0;

  for (const auto &param : node->params->params) {
    if (param->tag == ASTParamDecl::Normal) {
      auto &normal = param->normal;
      if (normal.default_value.is_not_null()) {
        info.default_params++;
      }
      ctx.scope->insert(normal.name, param->resolved_type, param);
      info.parameter_types[info.params_len] = param->resolved_type;
    } else {
      auto type = get_self_type();
      if (param->self.is_pointer) {
        type = global_get_type(type)->take_pointer_to();
      }
      ctx.scope->insert("self", type, param);
      info.parameter_types[info.params_len] = type;
    }

    info.params_len++;
  }

  if (info.return_type == -2) {
    throw_error("Internal Compiler error: unresolved generic return type.", node->source_range);
  }
  return global_find_function_type_id(info, {});
}

int Typer::visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<int> generic_args) {
  auto previous = ctx.scope;
  auto old_type = type_context;
  Defer _([&] {
    ctx.set_scope(previous);
    type_context = old_type;
  });

  type_context = node->target;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      ctx.scope->types[param] = *generic_arg;
      generic_arg++;
    }
  }

  node->target->accept(this);
  auto type = global_get_type(node->target->resolved_type);
  if (!type) {
    throw_error("use of undeclared type", node->target->source_range);
  }

  Type *interface_ty = nullptr;

  if (node->interface) {
    node->interface.get()->accept(this);
    auto type_id = node->interface.get()->resolved_type;
    if (type_id == Type::invalid_id) {
      throw_error("Internal compiler error: type of impl interface was invalid", node->source_range);
    }
    interface_ty = global_get_type(type_id);
  }

  auto type_scope = type->get_info()->scope;
  Scope impl_scope = {};
  for (const auto &method : node->methods) {
    if (!method->generic_parameters.empty()) {
      ctx.scope->insert(method->name, -1, method, SYMBOL_IS_FUNCTION);
      continue;
    }
    auto func_ty_id = visit_function_signature(method, false);
    if (auto symbol = type_scope->local_lookup(method->name)) {
      if (!(symbol->flags & SYMBOL_IS_FORWARD_DECLARED)) {
        throw_error("Redefinition of method", method->source_range);
      } else {
        symbol->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
      }
    } else {
      if ((method->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        type_scope->insert(method->name, func_ty_id, method, SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION);
      } else {
        type_scope->insert(method->name, func_ty_id, method, SYMBOL_IS_FUNCTION);
      }
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      if (method->flags & FUNCTION_IS_FOREIGN || method->flags & FUNCTION_IS_FORWARD_DECLARED) {
        continue;
      }
    }
    auto info = global_get_type(func_ty_id)->get_info()->as<FunctionTypeInfo>();
    visit_function_body(method, info->return_type);
  }

  if (interface_ty) {
    auto declaring_node = interface_ty->declaring_node.get();
    if (!declaring_node || declaring_node->get_node_type() != AST_NODE_INTERFACE_DECLARATION) {
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
      if (auto impl_symbol = impl_scope.local_lookup(name)) {
        if (interface_sym.type_id != impl_symbol->type_id) {
          if (interface_sym.type_id != -1 && impl_symbol->type_id != -1) {
            throw_error(std::format("method \"{}\" doesn't match interface.\nexpected {}, got {}", name,
                                    global_get_type(interface_sym.type_id)->to_string(),
                                    global_get_type(impl_symbol->type_id)->to_string()),
                        node->source_range);
          } else {
            std::cout << "\033[1;90mmmethod.type_id == \033[1;31m" << std::to_string(interface_sym.type_id)
                      << "\033[0m\n";
            std::cout << "\033[1;90mimpl_symbol.type_id == \033[1;31m" << std::to_string(impl_symbol->type_id)
                      << "\033[0m\n";
            throw_error("internal compiler error: method.type_id or impl_symbol.type_id was null",
                        interface_sym.declaring_node ? interface_sym.declaring_node.get()->source_range
                                                     : node->source_range);
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
        throw_error(std::format("impl method \"{}\" not found in interface", name),
                    impl_sym.declaring_node.get()->source_range);
      }
    }
    type->interfaces.push_back(interface_ty->id);
  }

  return type->id;
}

int Typer::visit_interface_declaration(ASTInterfaceDeclaration *node, bool generic_instantiation,
                                       std::vector<int> generic_args) {
  auto previous = ctx.scope;
  Defer _([&] { ctx.set_scope(previous); });
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      ctx.scope->types[param] = *generic_arg;
      generic_arg++;
    }
  }
  auto type = global_get_type(global_create_interface_type(node->name, node->scope, generic_args));
  type->declaring_node = node;
  return type->id;
}

void Typer::visit(ASTTaggedUnionDeclaration *node) {
  auto type = global_get_type(node->resolved_type);
  auto info = type->get_info()->as<TaggedUnionTypeInfo>();
  ctx.set_scope(node->scope);
  for (const auto &member : node->members) {
    member->accept(this);
    auto type = member->resolved_type;
    InternedString name;
    if (member->get_node_type() == AST_NODE_DECLARATION) {
      name = static_cast<ASTDeclaration *>(member)->name;
    } else if (member->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
      name = static_cast<ASTStructDeclaration *>(member)->name;
    }
    info->variants.push_back(TaggedUnionVariant{name, type});
  }
  ctx.exit_scope();
}

void Typer::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1, node, SYMBOL_IS_VARIABLE);
    return;
  }
  // TODO: verify this is correct for generic instantiations?
  node->resolved_type = visit_struct_declaration(node, false);
}

void Typer::visit(ASTUnionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1, node, SYMBOL_IS_VARIABLE);
    return;
  }
  // TODO: verify this is correct for generic instantiations?
  node->resolved_type = visit_union_declaration(node, false);
}

void Typer::visit(ASTEnumDeclaration *node) {
  auto elem_type = -1;
  ctx.scope->create_enum_type(node->name, create_child(ctx.scope), node->is_flags);
  auto enum_type = global_get_type(ctx.scope->find_type_id(node->name, {}));
  auto info = enum_type->get_info()->as<EnumTypeInfo>();

  // TODO: why is value still nullable? we don't even check if it exists before we use it.
  for (const auto &[key, value] : node->key_values) {
    value.get()->accept(this);
    auto node_ty = value.get()->resolved_type;
    info->scope->insert(key, node_ty, node);
    if (elem_type == -1) {
      elem_type = node_ty;
    } else {
      assert_types_can_cast_or_equal(node_ty, elem_type, node->source_range, "expected: {}, got : {}",
                                     "Inconsistent types in enum declaration.");
    }
  }
  if (elem_type == void_type()) {
    throw_error("Invalid enum declaration.. got null or no type.", node->source_range);
  }
  node->element_type = elem_type;
  info->element_type = elem_type;
  node->resolved_type = enum_type->id;
}

void Typer::visit(ASTFunctionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1, node, SYMBOL_IS_FUNCTION);
    return;
  }

  auto type_id = visit_function_signature(node, false);
  node->resolved_type = type_id;

  if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
    ctx.scope->insert(node->name, type_id, node, SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION);
    return;
  }

  ctx.scope->insert(node->name, type_id, node, SYMBOL_IS_FUNCTION);

  if ((node->flags & FUNCTION_IS_FOREIGN) != 0) {
    return;
  }

  // TODO: we should probably stop using info here and just get the declaring function node for everything.
  auto info = global_get_type(type_id)->get_info()->as<FunctionTypeInfo>();
  visit_function_body(node, info->return_type);
}

void Typer::visit(ASTDeclaration *node) {
  // Inferred declaration.
  if (node->type == nullptr) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      auto type = static_cast<ASTType *>(node->value.get());
      if (type->kind != ASTType::REFLECTION) {
        throw_error("Cannot use a type as a value.", node->value.get()->source_range);
      }
    }
    node->value.get()->accept(this);
    auto value_ty = node->value.get()->resolved_type;
    if (value_ty == void_type()) {
      throw_error("Cannot assign a variable with value type of 'void'", node->source_range);
    }
    auto type = global_get_type(value_ty);

    // CLEANUP: This is nonsense.
    node->type = ast_alloc<ASTType>();
    node->type->resolved_type = value_ty;
    node->resolved_type = value_ty;

    // TODO: so, we just don't set the type if it can't be assigned to int??? what?
    if (type->is_kind(TYPE_SCALAR) && type->get_ext().has_no_extensions()) {
      auto info = (type->get_info()->as<ScalarTypeInfo>());
      auto rule = type_conversion_rule(type, global_get_type(int_type()), node->source_range);
      if (info->is_integral && rule != CONVERT_PROHIBITED && rule != CONVERT_EXPLICIT) {
        node->type->resolved_type = int_type();
      }
    }
  }

  node->type->accept(this);

  if (node->type->resolved_type == -1) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }

  if (node->value.is_not_null()) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      auto type = static_cast<ASTType *>(node->value.get());
      if (type->kind != ASTType::REFLECTION) {
        throw_error("Cannot use a type as a value.", node->value.get()->source_range);
      }
    }

    auto old_ty = declaring_or_assigning_type;
    declaring_or_assigning_type = node->type->resolved_type;
    Defer _defer([&] { declaring_or_assigning_type = old_ty; });
    node->value.get()->accept(this);
    auto expr_type = node->value.get()->resolved_type;
    assert_types_can_cast_or_equal(expr_type, node->type->resolved_type, node->source_range,
                                   "invalid declaration types. expected: {}, got {}",
                                   std::format("declaration: {}", node->name.get_str()));
  }

  auto symbol = ctx.scope->lookup(node->name);
  symbol->type_id = node->type->resolved_type;
  auto type = global_get_type(node->type->resolved_type);

  // TODO: is this neccesary?
  // If we declare a c_string, we skip the normal string construction procedure.
  if (node->value.get() && node->value.get()->resolved_type == string_type() && type->id == string_type()) {
    node->value.get()->resolved_type = c_string_type();
  }

  if (symbol->type_id == void_type() || node->type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", node->name.get_str()), node->source_range);
  }

  if (node->is_constexpr) {
    auto type = global_get_type(node->type->resolved_type);
    if ((!type->is_kind(TYPE_SCALAR) || type->get_ext().has_extensions())) {
      throw_error(std::format("Can only use scalar types (integers, floats, "
                              "bools) as constant expressions, got {}",
                              type->to_string()),
                  node->value.get()->source_range);
    }
  }
}

void Typer::visit(ASTBlock *node) {
  ctx.set_scope(node->scope);

  int last_statement_idx = current_block_statement_idx;
  Defer _([&] { current_block_statement_idx = last_statement_idx; });

  int statement_idx = 0;
  for (auto &statement : node->statements) {
    current_block_statement_idx = statement_idx;
    statement->accept(this);
    ASTNodeType node_type = statement->get_node_type();
    // TODO: verify that we want to allow all nodes to partake in this? it might be cheaper to not do
    // a bunch of comparisons, it might be cheaper to not do this for every node.
    // Probably the latter, since theres much less work to be done, and then there's nothing we have to do
    // when we want to add more control flow structures, if we do.
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

  node->flags = node->control_flow.flags;
  node->return_type = node->control_flow.type == -1 ? void_type() : node->control_flow.type;
  ctx.exit_scope();
}
// TODO: Remove ParamDecl, and ArgumentDecl probably. Such unneccesary nodes, a ton of boilerplate visitor logic
// and no real benefit.
// TODO: we can keep an ASTParamsDecl but meh
void Typer::visit(ASTParamsDecl *node) {
  for (auto &param : node->params) {
    param->accept(this);
  }
  return;
}
void Typer::visit(ASTParamDecl *node) {
  if (node->tag == ASTParamDecl::Self) {
    if (!type_context) {
      throw_error("No target type for self", node->source_range);
    }
    node->resolved_type = get_self_type();
    if (node->self.is_pointer) {
      node->resolved_type = global_get_type(node->resolved_type)->take_pointer_to();
    }
    return;
  } else {
    node->normal.type->accept(this);
    int id = node->normal.type->resolved_type;
    node->resolved_type = id;

    if (id == -1) {
      throw_error("Use of undeclared type.", node->source_range);
    }

    auto type = global_get_type(id);

    if (type->get_ext().is_fixed_sized_array()) {
      throw_warning(WarningDownCastFixedArrayParam,
                    "using a fixed array as a function parameter: note, this "
                    "casts the length information off and gets passed as as "
                    "pointer. Consider using a dynamic array",
                    node->source_range);
      if (node->normal.default_value.is_not_null()) {
        throw_error("Cannot currently use default parameters for fixed buffer pointers.", node->source_range);
      }

      // cast off the fixed size array and add a pointer to it,
      // for s8[] to s8*
      {
        auto element = type->get_element_type();
        node->resolved_type = global_get_type(element)->take_pointer_to();
      }
    }

    auto old_ty = declaring_or_assigning_type;
    declaring_or_assigning_type = id;
    Defer _defer([&] { declaring_or_assigning_type = old_ty; });

    if (node->normal.default_value.is_not_null()) {
      node->normal.default_value.get()->accept(this);
      auto expr_type = node->normal.default_value.get()->resolved_type;
      assert_types_can_cast_or_equal(expr_type, node->resolved_type, node->source_range,
                                     "invalid parameter declaration; expected: {} got: {}",
                                     std::format("parameter: {}", node->normal.name));
    }
  }
}

void Typer::visit(ASTReturn *node) {
  int type;
  if (node->expression.is_not_null()) {
    node->expression.get()->accept(this);
    type = node->expression.get()->resolved_type;
  } else {
    type = ctx.scope->find_type_id("void", {});
  }
  node->control_flow = ControlFlow{BLOCK_FLAGS_RETURN, type};
}
void Typer::visit(ASTContinue *node) { node->control_flow = ControlFlow{BLOCK_FLAGS_CONTINUE, -1}; }
void Typer::visit(ASTBreak *node) { node->control_flow = ControlFlow{BLOCK_FLAGS_BREAK, -1}; }

void Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  auto iden = static_cast<ASTIdentifier *>(node->iden);
  node->range->accept(this);
  int range_type_id = node->range->resolved_type;
  Type *range_type = global_get_type(range_type_id);

  if (range_type->get_ext().has_extensions() && range_type->get_ext().extensions.back().type == TYPE_EXT_POINTER) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            range_type->to_string()),
                node->source_range);
  }

  int iter_ty = -1;

  // TODO: implement some kind of interface system that we can use for iterators,
  // no longer can we rely on C++'s crappy 'begin()/end()' since we compile our own free methods.
  if (range_type_id == string_type()) {
    iter_ty = char_type();
  } else if (range_type_id == ::range_type()) {
    iter_ty = int_type();
    if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
      throw_error("Cannot use pointer value semantic with a range. use Range{<start>, "
                  "<end>, <increment>} syntax to increment by a custom value.",
                  node->source_range);
    }
  } else if (range_type->get_ext().is_array() || range_type->get_ext().is_fixed_sized_array()) {
    iter_ty = range_type->get_element_type();
  } else {
    throw_error("Cannot iterate with a range-based for loop over a "
                "non-collection type.",
                node->source_range);
  }

  if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
    iter_ty = global_get_type(iter_ty)->take_pointer_to();
  }

  ctx.scope->insert(iden->value, iter_ty, node);
  node->iden->accept(this);
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
  auto func_node_type = node->function->get_node_type();

  // Try to visit implementation on call if not emitted.
  try_visit_impl_on_call(node, func_node_type);

  // Resolve the type of the function being called
  node->function->accept(this);
  Type *type = global_get_type(node->function->resolved_type);

  auto symbol_nullable = get_symbol(node->function);
  bool method_call = false;

  if (!symbol_nullable && !type) {
    throw_error("use of undeclared function", node->source_range);
  }

  if (auto symbol = symbol_nullable.get()) {
    if (!type) {
      type = global_get_type(symbol->type_id);
    }
    if (symbol->declaring_node.is_not_null() &&
        symbol->declaring_node.get()->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      auto func = static_cast<ASTFunctionDeclaration *>(symbol->declaring_node.get());
      method_call = (func->flags & FUNCTION_IS_METHOD) != 0 || func->params->has_self;
      // Try to resolve generic function call
      if (!node->generic_arguments.empty() || (func && !func->generic_parameters.empty())) {
        resolve_generic_function_call(node, type, func);
      }
    }
  }

  if (!type) {
    throw_error("unable to locate type for function call", node->source_range);
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("unable to call a non-function typed variable, got type {}", type->to_string()),
                node->source_range);
  }

  auto info = (type->get_info()->as<FunctionTypeInfo>());
  type_check_arguments(node, type, method_call, info);
  node->resolved_type = info->return_type;
}

void Typer::type_check_arguments(ASTCall *&node, Type *&type, bool &method_call, FunctionTypeInfo *&info) {
  auto old_type = declaring_or_assigning_type;
  Defer _([&]() { declaring_or_assigning_type = old_type; });
  auto args = node->arguments->arguments;
  auto args_ct = args.size();
  auto params_ct = info->params_len - (method_call ? 1 : 0);
  // TODO: rewrite this. this is so hard tor read.
  if ((args_ct > params_ct && !info->is_varargs) || args_ct < params_ct - info->default_params) {
    throw_error(std::format("Function call has incorrect number of arguments. Expected: {}, Found: {}\n type: {}",
                            params_ct, args_ct, type->to_string()),
                node->source_range);
  }

  int param_index = 0;
  for (int arg_index = 0; arg_index < args_ct; ++arg_index) {
    if (method_call && arg_index == 0) {
      param_index++;
    }
    auto arg = args[arg_index];
    declaring_or_assigning_type = info->parameter_types[param_index];
    arg->accept(this);
    auto arg_ty = arg->resolved_type;
    if (arg_index < params_ct) {
      assert_types_can_cast_or_equal(arg_ty, info->parameter_types[param_index], node->source_range,
                                     "invalid argument types. expected: {}, got: {}",
                                     std::format("parameter: {} of function", arg_index));
    }
    param_index++;
  }
}

void Typer::resolve_generic_function_call(ASTCall *node, Type *&type, ASTFunctionDeclaration *func) {
  std::vector<int> gen_args;
  if (node->generic_arguments.empty()) {
    node->arguments->accept(this);
    gen_args = node->arguments->resolved_argument_types;
  } else {
    gen_args = get_generic_arg_types(node->generic_arguments);
  }

  auto type_id = visit_generic<ASTFunctionDeclaration>(&Typer::visit_function_signature, func, gen_args);

  if (type_id == -2) {
    throw_error("Template instantiation argument count mismatch", node->source_range);
  }

  type = global_get_type(type_id);
  auto info = type->get_info()->as<FunctionTypeInfo>();

  for (auto &instantiation : func->generic_instantiations) {
    if (instantiation.type == type_id) {
      visit_function_body(static_cast<ASTFunctionDeclaration *>(instantiation.node), info->return_type);
      break;
    }
  }
}

void Typer::try_visit_impl_on_call(ASTCall *&node, ASTNodeType &func_node_type) {
  if (func_node_type == AST_NODE_DOT_EXPR || func_node_type == AST_NODE_SCOPE_RESOLUTION) {
    Type *base_ty = nullptr;
    if (func_node_type == AST_NODE_DOT_EXPR) {
      auto base = static_cast<ASTDotExpr *>(node->function)->base;
      base->accept(this);
      base_ty = global_get_type(base->resolved_type);
    } else {
      auto base = static_cast<ASTScopeResolution *>(node->function)->base;
      base->accept(this);
      base_ty = global_get_type(base->resolved_type);
    }
    if (auto declaring_node = base_ty->declaring_node.get()) {
      auto declaring_node_type = declaring_node->get_node_type();
      if (declaring_node_type == AST_NODE_STRUCT_DECLARATION) {
        auto struct_decl = static_cast<ASTStructDeclaration *>(declaring_node);
        for (auto &impl : struct_decl->impls) {
          visit_generic<ASTImpl>(&Typer::visit_impl_declaration, impl, base_ty->generic_args);
        }
      }
    }
  }
}

void Typer::visit(ASTArguments *node) {
  auto type = global_get_type(declaring_or_assigning_type);
  FunctionTypeInfo *info = nullptr;
  if (type) {
    info = dynamic_cast<FunctionTypeInfo *>(type->get_info());
  }
  for (int i = 0; i < node->arguments.size(); ++i) {
    auto arg = node->arguments[i];
    if (!info) {
      arg->accept(this);
      node->resolved_argument_types.push_back(arg->resolved_type);
      continue;
    }
    auto old_ty = declaring_or_assigning_type;
    declaring_or_assigning_type = info->parameter_types[i];
    Defer _defer([&] { declaring_or_assigning_type = old_ty; });
    arg->accept(this);
    node->resolved_argument_types.push_back(arg->resolved_type);
  }
}

void Typer::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  if (auto _switch = dynamic_cast<ASTSwitch *>(node->expression)) {
    node->control_flow = _switch->control_flow;
  }
}

template <typename T>
int Typer::visit_generic(int (Typer::*visit_method)(T *, bool, std::vector<int>), ASTNode *declaring_node,
                         std::vector<int> args) {
  auto definition = static_cast<T *>(declaring_node);
  if (definition->generic_parameters.size() != args.size()) {
    return -2;
  }
  auto type_id = find_generic_instance(definition->generic_instantiations, args);
  if (type_id == -1) {
    auto copy = static_cast<T *>(deep_copy_ast(definition));
    type_id = (this->*visit_method)(copy, true, args);
    copy->generic_parameters.clear();
    copy->generic_instantiations.clear();
    definition->generic_instantiations.push_back({args, copy, type_id});
  }
  return type_id;
}

std::vector<TypeExtension> Typer::accept_extensions(std::vector<ASTTypeExtension> ast_extensions) {
  std::vector<TypeExtension> extensions;
  for (auto &ext : ast_extensions) {
    if (ext.type == TYPE_EXT_FIXED_ARRAY) {
      auto val = evaluate_constexpr(ext.expression, ctx);
      if (val.tag != Value::INTEGER) {
        throw_error("Fixed array must have integer size.", ext.expression->source_range);
      }
      extensions.push_back({ext.type, (size_t)val.integer});
    } else if (ext.type == TYPE_EXT_MAP) {
      ext.expression->accept(this);
      auto type_id = ext.expression->resolved_type;
      extensions.push_back({.type = ext.type, .key_type = type_id});
    } else {
      extensions.push_back({.type = ext.type});
    }
  }
  return extensions;
}

void Typer::visit(ASTType *node) {
  if (node->resolved_type != Type::invalid_id) {
    return;
  }

  TypeExtensions extensions;
  extensions.extensions = accept_extensions(node->extensions);

  if (node->kind == ASTType::SELF) {
    auto self = get_self_type();
    if (self == -1) {
      throw_error("Cannot locate #self type.", node->source_range);
    }
    auto self_w_ext = global_find_type_id(self, extensions);
    if (self_w_ext == -1) {
      throw_error("Cannot locate #self type with extensions", node->source_range);
    }
    node->resolved_type = self_w_ext;
    return;
  }

  if (node->kind == ASTType::NORMAL) {
    auto &normal_ty = node->normal;
    auto symbol = get_symbol(normal_ty.base).get();
    if (symbol && symbol->declaring_node.is_not_null() && !normal_ty.generic_arguments.empty()) {
      auto declaring_node = symbol->declaring_node.get();
      std::vector<int> generic_args;
      for (auto &arg : normal_ty.generic_arguments) {
        arg->accept(this);
        generic_args.push_back(arg->resolved_type);
      }
      int type_id = -1;
      switch (declaring_node->get_node_type()) {
        case AST_NODE_STRUCT_DECLARATION:
          type_id = visit_generic(&Typer::visit_struct_declaration, declaring_node, generic_args);
          break;
        case AST_NODE_UNION_DECLARATION:
          type_id = visit_generic(&Typer::visit_union_declaration, declaring_node, generic_args);
          break;
        case AST_NODE_FUNCTION_DECLARATION:
          type_id = visit_generic(&Typer::visit_function_signature, declaring_node, generic_args);
          break;
        case AST_NODE_INTERFACE_DECLARATION:
          type_id = visit_generic(&Typer::visit_interface_declaration, declaring_node, generic_args);
          break;
        default:
          throw_error("Invalid target to generic args", node->source_range);
          break;
      }
      if (type_id == -2) {
        throw_error("Template instantiation argument count mismatch", node->source_range);
      }
      if (declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
        auto info = global_get_type(type_id)->get_info()->as<FunctionTypeInfo>();
        auto func = static_cast<ASTFunctionDeclaration *>(declaring_node);
        for (auto &instantiation : func->generic_instantiations) {
          if (instantiation.type == type_id) {
            visit_function_body(static_cast<ASTFunctionDeclaration *>(instantiation.node), info->return_type);
            break;
          }
        }
      }
      node->resolved_type = global_find_type_id(type_id, extensions);
    } else {
      normal_ty.base->accept(this);
      auto base_ty = global_get_type(normal_ty.base->resolved_type);
      if (!base_ty) {
        throw_error(std::format("use of undeclared type"), node->source_range);
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
    FunctionTypeInfo info;
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
    throw_error("Internal Compiler Error: Invalid type kind", node->source_range);
  }
}

void Typer::visit(ASTBinExpr *node) {
  node->left->accept(this);
  auto left = node->left->resolved_type;

  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  if (node->op.type == TType::Assign) {
    declaring_or_assigning_type = left;
  }
  if (node->op.type == TType::Concat) {
    auto type = global_get_type(left);
    // TODO: if the array is a pointer to an array, we should probably have an implicit dereference.
    declaring_or_assigning_type = type->get_element_type();
  }

  node->right->accept(this);
  auto right = node->right->resolved_type;

  if (node->op.type == TType::Assign) {
    if (node->left->get_node_type() == AST_NODE_IDENTIFIER) {
      ctx.scope->insert(((ASTIdentifier *)node->left)->value, node->left->resolved_type, node->right);
    }
  }
  auto left_ty = global_get_type(left);

  if (left_ty->id != string_type() && left_ty->is_kind(TYPE_STRUCT) && left_ty->get_ext().has_no_extensions() &&
      node->op.type != TType::Assign && !node->op.is_comp_assign()) {
    node->is_operator_overload = true;
    auto function_type = find_operator_overload(node->op.type, left_ty);
    // TODO: actually type check against the function?
    node->resolved_type = global_get_type(function_type)->get_info()->as<FunctionTypeInfo>()->return_type;
    return;
  }

  // array remove operator.
  if (node->op.type == TType::Erase) {
    if (!left_ty->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array", node->source_range);
    }
    auto element_ty = left_ty->get_element_type();
    assert_types_can_cast_or_equal(right, element_ty, node->source_range, "expected : {}, got {}",
                                   "invalid type in array concatenation expression");
    node->resolved_type = element_ty;
    return;
  }

  // There was operator overloading here. Instead, now we're going to wait
  // until we have traits and a more sensible,
  // non C++ way to do this.
  // For now, it's being removed
  // (edit: you can override operator overloads by implementing functions with the same name as the operator's
  // ttype.to_lower())

  // TODO: clean up this hacky mess.
  if (node->op.type == TType::Concat) {
    if (!left_ty->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array", node->source_range);
    }
    auto element_ty = left_ty->get_element_type();
    assert_types_can_cast_or_equal(right, element_ty, node->source_range, "expected : {}, got {}",
                                   "invalid type in array concatenation expression");
    node->resolved_type = void_type();
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have
  // their operands type checked, but right now that would involve casting
  // scalars to each other, which makes no  sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
  } else {
    auto left_t = global_get_type(left);
    auto right_t = global_get_type(right);
    auto conv_rule_0 = type_conversion_rule(left_t, right_t, node->left->source_range);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t, node->right->source_range);
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

void Typer::visit(ASTUnaryExpr *node) {
  node->operand->accept(this);
  auto operand_ty = node->operand->resolved_type;

  // Address-Of.
  if (node->op.type == TType::And) {
    node->resolved_type = global_get_type(operand_ty)->take_pointer_to();
    return;
  }

  // Dereference.
  if (node->op.type == TType::Mul) {
    auto type = global_get_type(operand_ty);
    if (type->get_ext().is_pointer()) {
      node->resolved_type = type->get_element_type();
      return;
    } else {
      throw_error(std::format("Cannot dereference a non-pointer type, got \"{}\"", type->to_string()),
                  node->source_range);
    }
  }

  // unary operator overload.
  auto left_ty = global_get_type(operand_ty);

  // ~ operator, pop on dynamic arrays.
  if (left_ty->get_ext().is_array() && node->op.type == TType::Not) {
    node->resolved_type = left_ty->get_element_type();
    return;
  }

  // * Again, operator overloading once was here.
  // * We need to wait until we have interfaces to do it properly.

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule =
        type_conversion_rule(global_get_type(operand_ty), global_get_type(bool_type()), node->operand->source_range);
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED && conversion_rule != CONVERT_EXPLICIT);

    if (node->op.type == TType::LogicalNot && can_convert) {
      node->resolved_type = bool_type();
      return;
    }
  }

  // TODO: is this correct?
  node->resolved_type = operand_ty;
  return;
}

void Typer::visit(ASTIdentifier *node) {
  auto str = node->value.get_str();

  node->resolved_type = ctx.scope->find_type_id(node->value, {});
  if (node->resolved_type != -1) {
    return;
  }

  auto symbol = ctx.scope->lookup(node->value);
  if (symbol) {
    node->resolved_type = symbol->type_id;
  } else {
    throw_error(std::format("Use of undeclared identifier '{}'", node->value), node->source_range);
  }
}

void Typer::visit(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer: {
      int base = 10;
      auto value = node->value.get_str();
      if (value.starts_with("0x")) {
        base = 0;
      }

      if (value.starts_with("0b")) {
        value = value.substr(2, value.length());
        base = 2;
      }
      auto n = std::strtoll(value.c_str(), nullptr, base);

      if (declaring_or_assigning_type != -1) {
        auto type = global_get_type(declaring_or_assigning_type);
        if (type->is_kind(TYPE_SCALAR) && type_is_numerical(type)) {
          auto info = (type->get_info()->as<ScalarTypeInfo>());
          if (info->is_integral) {
            node->resolved_type = type->id;
            return;
          }
        }
      }
      node->resolved_type = s32_type();
      return;
    }
    case ASTLiteral::Float:
      node->resolved_type = float32_type();
      return;
    case ASTLiteral::RawString:
    case ASTLiteral::String: {
      static auto freestanding = compile_command.has_flag("freestanding");
      if (node->is_c_string || freestanding) {
        node->resolved_type = c_string_type();
      } else {
        node->resolved_type = string_type();
      }
      return;
    }
    case ASTLiteral::Bool:
      node->resolved_type = bool_type();
      return;
    case ASTLiteral::Null:
      node->resolved_type = voidptr_type();
      return;
    case ASTLiteral::InterpolatedString: {
      auto current = node->interpolated_string_root;
      while (current) {
        if (current->expression)
          current->expression->accept(this);
        current = current->next;
      }
      node->resolved_type = string_type();
      return;
    }
    case ASTLiteral::Char:
      node->resolved_type = char_type();
      return;
  }
}

void Typer::visit(ASTDotExpr *node) {
  node->base->accept(this);
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = global_get_type(base_ty_id);

  if (!base_ty) {
    throw_error("Internal Compiler Error: un-typed variable on lhs of dot "
                "expression?",
                node->source_range);
  }

  // TODO: remove this hack to get array length
  if (base_ty->get_ext().is_array()) {
    if (node->member_name == "length") {
      node->resolved_type = u32_type();
      return;
    }
    if (node->member_name == "data") {
      node->resolved_type = global_get_type(base_ty->get_element_type())->take_pointer_to();
      return;
    }
  }

  // TODO: remove this hack as well
  if (base_ty->get_ext().is_map()) {
    if (node->member_name == "contains") {
      static auto contains_ty = [] {
        auto func = FunctionTypeInfo{};
        func.is_varargs = true;
        func.return_type = bool_type();
        return global_find_function_type_id(func, {});
      }();
      node->resolved_type = contains_ty;
      return;
    }
  }

  Scope *base_scope = base_ty->get_info()->scope;

  if (!base_scope) {
    throw_error("Internal compiler error: dot expression used on a type that had a null scope", node->source_range);
  }

  if (auto member = base_scope->local_lookup(node->member_name)) {
    node->resolved_type = member->type_id;
  } else {
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", node->member_name, base_ty->to_string()),
                node->source_range);
  }
}

void Typer::visit(ASTScopeResolution *node) {
  node->base->accept(this);
  auto id = node->base->resolved_type;
  auto base_ty = global_get_type(id);
  Scope *scope = base_ty->get_info()->scope;
  if (!scope) {
    throw_error("Internal Compiler Error: scope is null for scope resolution", node->source_range);
  }
  if (auto member = scope->local_lookup(node->member_name)) {
    node->resolved_type = member->type_id;
    return;
  } else if (auto type = scope->find_type_id(node->member_name, {})) {
    if (type < 0)
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

  /*
  !HACK FIX STRING SLICING THIS IS TERRIBLE
 */
  if (left_ty->id == string_type()) {
    if (subscript_ty->id == range_type()) {
      node->resolved_type = left_ty->id;
      return;
    }
    node->resolved_type = char_type();
    return;
  }

  // * Todo: reimplement operator overloads with interfaces.

  auto ext = left_ty->get_ext();

  if (ext.is_map()) {
    assert_types_can_cast_or_equal(node->subscript->resolved_type, ext.extensions.back().key_type, node->source_range,
                                   "expected : {}, got {}", "Invalid type when subscripting map");
    node->resolved_type = left_ty->get_element_type();
    return;
  }

  if (!left_ty->get_ext().is_array() && !left_ty->get_ext().is_fixed_sized_array() &&
      !left_ty->get_ext().is_pointer()) {
    throw_error(std::format("cannot index into non array type. {}", left_ty->to_string()), node->source_range);
  }

  if (left_ty->get_ext().is_array()) {
    if (subscript_ty->id == range_type()) {
      node->resolved_type = left_ty->id;
      return;
    }
    node->resolved_type = left_ty->get_element_type();
    return;
  }
  node->resolved_type = left_ty->get_element_type();
}

void Typer::visit(ASTInitializerList *node) {
  Type *target_type;
  if (node->target_type.is_null()) {
    target_type = global_get_type(declaring_or_assigning_type);
  } else {
    node->target_type.get()->accept(this);
    target_type = global_get_type(node->target_type.get()->resolved_type);
  }
  if (!target_type) {
    throw_error("Can't use initializer list, no target type was provided", node->source_range);
  }

  if (target_type->get_ext().is_pointer() ||
      target_type->is_kind(TYPE_SCALAR) && target_type->get_ext().has_no_extensions()) {
    throw_error(std::format("Cannot use an initializer list on a pointer, or a scalar type (int/float, etc) that's "
                            "not an array\n\tgot {}",
                            target_type->to_string()),
                node->source_range);
  }

  auto scope = target_type->get_info()->scope;

  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_NAMED: {
      if (!target_type->is_kind(TYPE_STRUCT) && !target_type->is_kind(TYPE_UNION)) {
        throw_error(std::format("named initializer lists can only be used for structs & unions, got type {}\nNote, for "
                                "unions, you can only provide one value.",
                                target_type->to_string()),
                    node->source_range);
      }

      // @Cleanup this is useful for returning a default value.
      // we would probably prefer a type::default(),
      // but for now we'll leave it.
      if (node->key_values.empty()) {
        node->resolved_type = target_type->id;
        return;
      }

      for (const auto &[id, value] : node->key_values) {
        auto old = declaring_or_assigning_type;
        Defer _([&] { declaring_or_assigning_type = old; });
        auto symbol = scope->local_lookup(id);
        if (!symbol)
          throw_error(std::format("Invalid named initializer list: couldn't find {}", id), node->source_range);

        if (symbol->is_function()) {
          throw_error(std::format("Cannot initialize a function :: ({}) with an initializer list.", id),
                      value->source_range);
        }
        declaring_or_assigning_type = symbol->type_id;

        value->accept(this);
        auto value_ty = value->resolved_type;
        assert_types_can_cast_or_equal(value_ty, symbol->type_id, value->source_range, "from {}, to {}",
                                       "Unable to cast type to target field for named initializer list");
      }
    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      if (!target_type->get_ext().is_array() && !target_type->get_ext().is_fixed_sized_array() &&
          !target_type->get_ext().is_map()) {
        throw_error(std::format("Collection-style initializer lists like '{{0, 1, 2, ..}} or {{{{key, value}}, {{key, "
                                "value}}}}' can only be used with "
                                "arrays, fixed arrays, and maps. Got {}",
                                target_type->to_string()),
                    node->source_range);
      }
      auto &values = node->values;

      if (values.empty()) {
        node->resolved_type = target_type->id;
        return;
      }

      auto target_element_type = target_type->get_element_type();
      values[0]->accept(this);
      auto element_type = values[0]->resolved_type;
      for (int i = 1; i < values.size(); ++i) {
        int type = -1;
        if (values[i]->get_node_type() == AST_NODE_INITIALIZER_LIST) {
          auto old = declaring_or_assigning_type;
          Defer _([&] { declaring_or_assigning_type = old; });
          declaring_or_assigning_type = target_element_type;
          values[i]->accept(this);
          type = values[i]->resolved_type;
        } else {
          values[i]->accept(this);
          type = values[i]->resolved_type;
        }
        assert_types_can_cast_or_equal(
            type, element_type, values[i]->source_range, "to {} from {}",
            "Found inconsistent types in a collection-style initializer list. These types must be homogenous");
      }

      auto element_ty_ptr = global_get_type(element_type);
      auto target_element_ty_ptr = global_get_type(target_element_type);

      if (element_ty_ptr->is_kind(TYPE_SCALAR) && element_ty_ptr->get_ext().has_no_extensions() &&
          target_element_ty_ptr->is_kind(TYPE_SCALAR) && target_element_ty_ptr->get_ext().has_no_extensions()) {
        auto target_info = target_element_ty_ptr->get_info()->as<ScalarTypeInfo>();
        auto elem_info = element_ty_ptr->get_info()->as<ScalarTypeInfo>();

        // We allow implicit downcasting/ sign casting, just to prevent annoyances.
        if (target_info->is_integral && elem_info->is_integral) {
          node->resolved_type = target_type->id;
          return;
        }
      }

      assert_types_can_cast_or_equal(
          element_type, target_element_type, node->source_range, "to {} from {}",
          "Failed to assign element type from value passed into collection-style initializer list");

      node->resolved_type = target_type->id;
      return;
    } break;
    case ASTInitializerList::INIT_LIST_EMPTY:
      node->resolved_type = target_type->id;
      return;
  }
  node->resolved_type = target_type->id;
}
void Typer::visit(ASTRange *node) {
  node->left->accept(this);
  node->right->accept(this);
  auto left = node->left->resolved_type;
  auto right = node->right->resolved_type;
  if (!type_is_numerical(global_get_type(left)) || !type_is_numerical(global_get_type(right))) {
    throw_error("cannot use a non-numerical type in a range expression", node->source_range);
  }

  auto l_ty = global_get_type(left);
  auto r_ty = global_get_type(right);

  if (!l_ty->is_kind(TYPE_SCALAR) || !r_ty->is_kind(TYPE_SCALAR)) {
    throw_error("Cannot use non-scalar or integral types in a range expression", node->source_range);
  }

  auto l_info = (l_ty->get_info()->as<ScalarTypeInfo>());
  auto r_info = (r_ty->get_info()->as<ScalarTypeInfo>());

  if (!l_info->is_integral || !r_info->is_integral) {
    throw_error("Cannot use non-scalar or integral types in a range expression", node->source_range);
  }

  node->resolved_type = range_type();
}
void Typer::visit(ASTSwitch *node) {
  node->target->accept(this);
  auto type_id = node->target->resolved_type;
  auto type = global_get_type(type_id);

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

    if (expr_type == range_type() && type_is_numerical(type)) {
      continue;
    } else {
      assert_types_can_cast_or_equal(expr_type, type_id, node->source_range, "got {}, expected {}",
                                     "Invalid switch case.");
    }
  }
  node->resolved_type = return_type;
  if (node->is_statement) {
    node->control_flow = ControlFlow{flags, return_type};
  } else {
    if ((flags & BLOCK_FLAGS_BREAK) != 0) {
      throw_warning(WarningSwitchBreak, "You do not need to break from switch cases.", node->source_range);
    } else if ((flags & BLOCK_FLAGS_CONTINUE) != 0) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->source_range);
    }
  }
}

void Typer::visit(ASTTuple *node) {
  std::vector<int> types;
  auto declaring_tuple = global_get_type(declaring_or_assigning_type);

  int type_index = 0;
  for (const auto &v : node->values) {
    auto old = declaring_or_assigning_type;
    Defer _([&] { declaring_or_assigning_type = old; });
    if (declaring_tuple && declaring_tuple->is_kind(TYPE_TUPLE)) {
      auto info = declaring_tuple->get_info()->as<TupleTypeInfo>();
      if (info->types.size() < type_index) {
        throw_error(std::format("too many expressions provided to tuple\ntuple type {}", declaring_tuple->to_string()),
                    v->source_range);
      }
      declaring_or_assigning_type = info->types[type_index];
    }
    v->accept(this);
    types.push_back(v->resolved_type);
    type_index++;
  }
  TypeExtensions extensions;
  extensions.extensions = accept_extensions(node->type->extensions);
  node->type->resolved_type = global_find_type_id(types, extensions);
}
void Typer::visit(ASTAlias *node) {
  node->type->accept(this);

  if (node->type->resolved_type == -1) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }

  if (ctx.scope->types.contains(node->name)) {
    throw_error("Redeclaration of type", node->source_range);
  }
  ctx.scope->types[node->name] = node->type->resolved_type;

  return;
}

void Typer::visit(ASTTupleDeconstruction *node) {
  node->right->accept(this);
  auto type = global_get_type(node->right->resolved_type);

  if (!type->is_kind(TYPE_TUPLE)) {
    throw_error("Cannot currently destruct a non-tuple. Coming soon for structs.", node->source_range);
  }

  auto info = (type->get_info()->as<TupleTypeInfo>());

  if (node->idens.size() != info->types.size()) {
    throw_error(std::format("Cannot currently partially deconstruct a tuple. "
                            "expected {} identifiers to assign, got {}",
                            info->types.size(), node->idens.size()),
                node->source_range);
  }

  for (int i = 0; i < node->idens.size(); ++i) {
    auto type = info->types[i];
    auto iden = node->idens[i];
    // TODO: fix repro 41, this is helpful for that.
    // std::cout << "tuple[" << std::to_string(i) << "] = { \"" << iden->value.get_str() << "\", " <<
    // global_get_type(type)->to_string() << "}\n";
    ctx.scope->insert(iden->value, type, node);
  }

  return;
};

void Typer::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    auto symbol_nullable = get_symbol(node->target);
    if (symbol_nullable.is_null() || symbol_nullable.get()->declaring_node.is_null()) {
      throw_error("Generic impls only for generic types for now.", node->source_range);
    }
    auto declaring_node = symbol_nullable.get()->declaring_node.get();
    if (declaring_node->get_node_type() != AST_NODE_STRUCT_DECLARATION) {
      throw_error("Generic impls are only for struct types for now.", node->source_range);
    }
    auto node_as_struct = static_cast<ASTStructDeclaration *>(declaring_node);
    node_as_struct->impls.push_back(node);
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

void Typer::visit(ASTInterfaceDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->declare_interface(node->name, node);
  } else {
    ctx.scope->insert(node->name, visit_interface_declaration(node, false), node);
  }
  return;
}

int Typer::get_self_type() {
  if (type_context.is_not_null()) {
    type_context.get()->accept(this);
    return type_context.get()->resolved_type;
  }
  return -1;
}