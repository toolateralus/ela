
#include <cassert>
#include <csetjmp>
#include <format>
#include <iostream>
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

void handle_generic_error(GenericInstantiationErrorUserData *data, const SourceRange &range) {
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

// TODO: add a statement in the .accept() function of ASTNode base type
// TODO: where we return the if the resolved type is already calculated?

// TODO: then we'd have to make suer we reset all the resolved types when copying,
// But it would save some double visits possibly.

void assert_types_can_cast_or_equal(const int from, const int to, const SourceRange &source_range,
                                    const std::string &message) {
  auto from_t = global_get_type(from);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t, source_range);
  if (to != from && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' + std::format("expected \"{}\", got \"{}\"", to_t->to_string(), from_t->to_string()),
                source_range);
  }
}

void assert_return_type_is_valid(int &return_type, int new_type, ASTNode *node) {
  if (return_type == Type::INVALID_TYPE_ID) {
    return_type = new_type;
  } else if (new_type != Type::INVALID_TYPE_ID && new_type != return_type) {
    assert_types_can_cast_or_equal(return_type, new_type, node->source_range, "Inconsistent return types in block.");
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

void Typer::visit(ASTTaggedUnionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->create_type_alias(node->name, Type::INVALID_TYPE_ID, TypeKind(0));
    return;
  }
  visit_tagged_union_declaration(node, false);
}

void Typer::visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                     std::vector<int> generic_args) {
  auto type = global_get_type(node->resolved_type);

  auto info = (type->get_info()->as<StructTypeInfo>());

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0 || node->is_fwd_decl) {
    node->resolved_type = type->id;
    return;
  }

  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind);
      generic_arg++;
    }
    type = global_get_type(global_create_struct_type(node->name, node->scope, generic_args));
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  type->declaring_node = node;

  for (auto subunion : node->subtypes) {
    for (const auto &field : subunion->members) {
      field.type->accept(this);
      node->scope->insert_variable(field.name, field.type->resolved_type, nullptr);
    }
  }
  for (auto alias : node->aliases) {
    alias->accept(this);
  }
  for (auto decl : node->members) {
    decl.type->accept(this);
    ctx.scope->insert_variable(decl.name, decl.type->resolved_type, nullptr);
  }

  ctx.set_scope(old_scope);
  node->resolved_type = type->id;

  if (type->is_kind(TYPE_SCALAR)) {
    throw_error("struct declaration was a scalar???", node->source_range);
  }
}

void Typer::visit_tagged_union_declaration(ASTTaggedUnionDeclaration *node, bool generic_instantiation,
                                           std::vector<int> generic_args) {
  auto type = global_get_type(node->resolved_type);

  auto old_scope = ctx.scope;
  Defer _defer([&] { ctx.scope = old_scope; });
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind);
      generic_arg++;
    }
    type = global_get_type(global_create_tagged_union_type(node->name, node->scope, generic_args));
  }

  type->declaring_node = node;

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  auto info = type->get_info()->as<TaggedUnionTypeInfo>();

  for (const auto &variant : node->variants) {
    switch (variant.kind) {
      case ASTTaggedUnionVariant::NORMAL: {
        // TODO: is this how we want to do this?
        info->variants.push_back({variant.name, void_type()});
      } break;
      case ASTTaggedUnionVariant::TUPLE: {
        variant.tuple->accept(this);
        auto type = variant.tuple->resolved_type;
        info->variants.push_back({variant.name, type});
      } break;
      case ASTTaggedUnionVariant::STRUCT: {
        auto scope = create_child(ctx.scope);
        ctx.set_scope(scope);
        for (const auto &field : variant.struct_declarations) {
          field->accept(this);
        }
        ctx.exit_scope();
        auto type = global_create_struct_type(variant.name, scope);
        info->variants.push_back({variant.name, type});
      } break;
    }
  }
  ctx.exit_scope();
}

void Typer::visit_function_body(ASTFunctionDeclaration *node) {
  auto old_ty = declaring_or_assigning_type;
  auto old_scope = ctx.scope;
  auto _defer = Defer([&] {
    ctx.set_scope(old_scope);
    declaring_or_assigning_type = old_ty;
  });
  ctx.set_scope(node->scope);
  declaring_or_assigning_type = node->return_type->resolved_type;
  auto block = node->block.get();
  if (!block) {
    throw_error("internal compiler error: attempting to visit body of function forward declaration.",
                node->source_range);
  }
  block->accept(this);
  auto control_flow = block->control_flow;
  if (control_flow.type == Type::INVALID_TYPE_ID)
    control_flow.type = void_type();
  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0)
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0)
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 && node->return_type->resolved_type != void_type())
    throw_error("Not all code paths return a value.", node->source_range);
  assert_types_can_cast_or_equal(control_flow.type, node->return_type->resolved_type, node->source_range,
                                 std::format("invalid return type", node->name.get_str()));
}

void Typer::visit(ASTLambda *node) {
  node->unique_identifier = "$lambda$" + std::to_string(LAMBDA_UNIQUE_ID++);
  node->params->accept(this);
  node->return_type->accept(this);

  // ! for debugging repro 70.ela
  // ! std::cout << "lambda return type " << node->return_type->resolved_type << '\n';

  std::vector<int> param_types;
  FunctionTypeInfo info;
  // TODO:
  // We need to make it so the lambda block's parent is the root scope, so that it doesn't give the impression that it
  // can do closures.
  int parameter_index = 0;
  for (const auto &param : node->params->params) {
    info.parameter_types[parameter_index] = param->resolved_type;
    info.params_len++;
    node->block->scope->insert_variable(param->normal.name, param->resolved_type, nullptr);
    parameter_index++;
  }

  node->block->accept(this);
  info.return_type = node->return_type->resolved_type;
  auto type = global_find_function_type_id(info, {});
  node->resolved_type = global_get_type(type)->take_pointer_to();

  // w????
  // std::cout << global_get_type(node->resolved_type)->to_string() << '\n';
}

void Typer::visit_function_signature(ASTFunctionDeclaration *node, bool generic_instantiation,
                                     std::vector<int> generic_args) {
  // Setup context.
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  Defer _([&] { ctx.set_scope(old_scope); });

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind);
      generic_arg++;
    }
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
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
      ctx.scope->insert_variable(normal.name, param->resolved_type, nullptr);
      info.parameter_types[info.params_len] = param->resolved_type;
    } else {
      auto type = get_self_type();
      if (param->self.is_pointer) {
        type = global_get_type(type)->take_pointer_to();
      }
      ctx.scope->insert_variable("self", type, nullptr);
      info.parameter_types[info.params_len] = type;
    }

    info.params_len++;
  }

  if (info.return_type == Type::UNRESOLVED_GENERIC_TYPE_ID) {
    throw_error("internal compiler error: unresolved generic return type.", node->source_range);
  }
  node->resolved_type = global_find_function_type_id(info, {});
}

void Typer::visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<int> generic_args) {
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
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind);
      generic_arg++;
    }
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  node->target->accept(this);
  auto target_ty = global_get_type(node->target->resolved_type);
  if (!target_ty) {
    throw_error("use of undeclared type", node->target->source_range);
  }

  Type *interface_ty = nullptr;

  if (node->interface) {
    node->interface.get()->accept(this);
    auto type_id = node->interface.get()->resolved_type;
    if (type_id == Type::INVALID_TYPE_ID) {
      throw_error("internal compiler error: type of impl interface was invalid", node->source_range);
    }
    interface_ty = global_get_type(type_id);
  }

  auto type_scope = target_ty->get_info()->scope;
  Scope impl_scope = {};
  for (const auto &method : node->methods) {
    if (!method->generic_parameters.empty()) {
      type_scope->insert_function(method->name, method);
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      continue;
    }
    visit_function_signature(method, false);
    auto func_ty_id = method->resolved_type;
    if (auto symbol = type_scope->local_lookup(method->name)) {
      if (!(symbol->flags & SYMBOL_IS_FORWARD_DECLARED)) {
        throw_error("Redefinition of method", method->source_range);
      } else {
        symbol->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
      }
    } else {
      if ((method->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        type_scope->insert_function(method->name, method, SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
      } else {
        type_scope->insert_function(method->name, method);
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
      ctx.scope->create_type_alias(param, *generic_arg, kind);
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

void Typer::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->create_type_alias(node->name, Type::UNRESOLVED_GENERIC_TYPE_ID, TYPE_STRUCT);
  } else {
    visit_struct_declaration(node, false);
  }
}

void Typer::visit(ASTEnumDeclaration *node) {
  auto elem_type = Type::INVALID_TYPE_ID;
  ctx.scope->create_enum_type(node->name, create_child(ctx.scope), node->is_flags, node);
  auto enum_type = global_get_type(ctx.scope->find_type_id(node->name, {}));
  auto info = enum_type->get_info()->as<EnumTypeInfo>();

  for (const auto &[key, value] : node->key_values) {
    value->accept(this);
    auto node_ty = value->resolved_type;
    info->scope->insert_variable(key, node_ty, value);
    if (elem_type == Type::INVALID_TYPE_ID) {
      elem_type = node_ty;
    } else {
      assert_types_can_cast_or_equal(node_ty, elem_type, node->source_range, "inconsistent types in enum declaration.");
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
    ctx.scope->insert_function(node->name, node);
    return;
  }

  visit_function_signature(node, false);

  if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
    ctx.scope->insert_function(node->name, node, SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
    return;
  }

  ctx.scope->insert_function(node->name, node);

  if ((node->flags & FUNCTION_IS_FOREIGN) != 0) {
    return;
  }
  visit_function_body(node);
}

bool expr_is_literal(ASTExpr *expr) {
  switch (expr->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin_expr = static_cast<ASTBinExpr *>(expr);
      return expr_is_literal(bin_expr->left) && expr_is_literal(bin_expr->right);
    }
    case AST_NODE_UNARY_EXPR:
      return expr_is_literal(static_cast<ASTUnaryExpr *>(expr)->operand);
    case AST_NODE_LITERAL:
      return true;
    default:
      return false;
  }
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
    node->type->source_range = node->source_range;
    node->type->resolved_type = value_ty;
    node->resolved_type = value_ty;

    // TODO: so, we just don't set the type if it can't be assigned to int??? what?
    if (type->is_kind(TYPE_SCALAR) && type->get_ext().has_no_extensions() && expr_is_literal(node->value.get())) {
      auto info = (type->get_info()->as<ScalarTypeInfo>());
      auto rule = type_conversion_rule(type, global_get_type(s32_type()), node->source_range);
      if (info->is_integral && rule != CONVERT_PROHIBITED && rule != CONVERT_EXPLICIT) {
        node->type->resolved_type = s32_type();
      }
    }
  }

  node->type->accept(this);

  if (node->type->resolved_type == Type::INVALID_TYPE_ID) {
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
                                   "invalid type in declaration");
  }

  auto symbol = ctx.scope->lookup(node->name);
  symbol->type_id = node->type->resolved_type;
  auto type = global_get_type(node->type->resolved_type);

  if (symbol->type_id == void_type() || node->type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", node->name.get_str()), node->source_range);
  }

  if (node->is_constexpr) {
    // TODO: we should probably improve this.
    // Our interpreter can't handle structs, but we want structs.

    // auto type = global_get_type(node->type->resolved_type);
    // if ((!type->is_kind(TYPE_SCALAR) || type->get_ext().has_extensions())) {
    //   throw_error(std::format("Can only use scalar types (integers, floats, "
    //                           "bools) as constant expressions, got {}",
    //                           type->to_string()),
    //               node->value.get()->source_range);
    // }
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
  node->return_type = node->control_flow.type == Type::INVALID_TYPE_ID ? void_type() : node->control_flow.type;
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

    if (id == Type::INVALID_TYPE_ID) {
      throw_error("Use of undeclared type.", node->source_range);
    }

    auto type = global_get_type(id);

    if (type->get_ext().is_fixed_sized_array()) {
      throw_warning(WarningDownCastFixedArrayParam,
                    "using a fixed array as a function parameter: note, this "
                    "casts the length information off and gets passed as as "
                    "pointer. Consider using a dynamic array",
                    node->source_range);
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
  node->resolved_type = type;
  node->control_flow = ControlFlow{BLOCK_FLAGS_RETURN, type};
}

void Typer::visit(ASTContinue *node) { node->control_flow = ControlFlow{BLOCK_FLAGS_CONTINUE, Type::INVALID_TYPE_ID}; }

void Typer::visit(ASTBreak *node) { node->control_flow = ControlFlow{BLOCK_FLAGS_BREAK, Type::INVALID_TYPE_ID}; }

void Typer::compiler_mock_function_call_visit_impl(int left_type, const InternedString &method_name) {
  ASTCall call;
  ASTArguments arguments;
  call.arguments = &arguments;

  // Type.
  ASTIdentifier left;
  static int depth = 0;

  left.value = "$$temp$$" + std::to_string(depth++);
  ctx.scope->insert_variable(left.value, left_type, nullptr);

  Defer erase_temp_symbol([&] {
    depth--;
    ctx.scope->erase("$$temp$$");
  });

  // .method
  ASTDotExpr dot;
  dot.base = &left;
  dot.member_name = method_name;

  call.function = &dot;
  call.accept(this);
}

void Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  auto iden = static_cast<ASTIdentifier *>(node->iter_identifier);
  node->range->accept(this);
  // auto range_node_ty = node->range->get_node_type();
  int range_type_id = node->range->resolved_type;
  Type *range_type = global_get_type(range_type_id);
  node->range_type = range_type->id;

  if (range_type->get_ext().has_extensions() && range_type->get_ext().extensions.back().type == TYPE_EXT_POINTER) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            range_type->to_string()),
                node->source_range);
  }

  int iter_ty = Type::INVALID_TYPE_ID;
  auto scope = range_type->get_info()->scope;

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
    auto iter_return_ty = global_get_type(symbol_ty->get_info()->as<FunctionTypeInfo>()->return_type);
    node->iterable_type = iter_return_ty->id;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(iter_return_ty->id, "current");

    symbol = iter_return_ty->get_info()->scope->local_lookup("current");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Iterable' but no 'current' function was found when "
                  "attempting to iterate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type;
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
        global_get_type(global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type);
    node->iterable_type = iter_return_ty->id;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_function_call_visit_impl(iter_return_ty->id, "current");

    symbol = iter_return_ty->get_info()->scope->local_lookup("current");
    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Iterable' but no 'current' function was found when "
                  "attempting to iterate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type;
  } else if (range_type->implements("Enumerator")) {
    node->iteration_kind = ASTFor::ENUMERATOR;
    compiler_mock_function_call_visit_impl(range_type_id, "current");
    auto symbol = scope->local_lookup("current");

    if (!symbol || !symbol->is_function()) {
      throw_error("internal compiler error: type implements 'Enumerator' but no 'current' function was found when "
                  "attempting to enumerate, or it was a non-function symbol named 'current'",
                  node->source_range);
    }
    iter_ty = global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type;
    node->iterable_type = range_type_id;
  } else if (range_type->get_base().get_str().starts_with("Iter$")) {
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
    if (!type->get_ext().is_pointer()) {
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
  ASTFunctionDeclaration *func_decl = nullptr;

  // Try to find the function via a dot expression, scope resolution, identifier, etc.
  // Otherwise find it via a type resolution, for things like array[10](); or what have you.
  if (auto symbol = get_symbol(node->function).get()) {
    if (!type) {
      type = global_get_type(symbol->type_id);
    }

    if (!symbol->is_function()) {
      throw_error("unable to call `()` a non-callable, non function symbol", node->source_range);
    }

    auto declaring_node = symbol->function.declaration;
    if (declaring_node && declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      func_decl = static_cast<ASTFunctionDeclaration *>(declaring_node);

      // resolve a generic call.
      if (!node->generic_arguments.empty() || !func_decl->generic_parameters.empty()) {
        // doing this so self will get the right type when we call generic methods
        auto old_type = type_context;
        Defer _([&] { type_context = old_type; });
        auto func = node->function;
        if (func->get_node_type() == AST_NODE_TYPE) {
          auto ast_type = static_cast<ASTType *>(func);
          if (ast_type->kind == ASTType::NORMAL) {
            if (!ast_type->normal.generic_arguments.empty()) {
              throw_error("Internal compiler error: generic args atached to wrong ast", func->source_range);
            }
            func = ast_type->normal.base;
          } else {
            throw_error("Cannot call method of function or tuple type", func->source_range);
          }
        }
        auto func_node_ty = func->get_node_type();
        ASTType type_ast;
        if (func_node_ty == AST_NODE_DOT_EXPR) {
          auto func_as_dot = static_cast<ASTDotExpr *>(func);
          func_as_dot->base->accept(this);
          auto type = global_get_type(func_as_dot->base->resolved_type);
          if (!type->get_info()->scope->local_lookup(func_as_dot->member_name) && type->get_ext().is_pointer()) {
            type = global_get_type(type->get_element_type());
          }
          type_ast.resolved_type = type->id;
          type_context = &type_ast;
        } else if (func_node_ty == AST_NODE_SCOPE_RESOLUTION) {
          auto func_as_scope_res = static_cast<ASTScopeResolution *>(func);
          func_as_scope_res->base->accept(this);
          type_ast.resolved_type = func_as_scope_res->base->resolved_type;
          type_context = &type_ast;
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

  auto info = type->get_info()->as<FunctionTypeInfo>();

  // If we have the declaring node representing this function, type check it against the parameters in that definition.
  // else, use the type.
  if (func_decl) {
    bool skip_first = false;
    if (node->function->get_node_type() == AST_NODE_DOT_EXPR) {
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
  auto old_type = declaring_or_assigning_type;
  Defer _([&]() { declaring_or_assigning_type = old_type; });
  auto args_ct = node->arguments.size();
  auto params_ct = params->params.size();
  auto largest = args_ct > params_ct ? args_ct : params_ct;
  int param_index = skip_first ? 1 : 0;
  for (int arg_index = 0; arg_index < largest; ++arg_index, ++param_index) {
    if (param_index < params_ct) {
      if (arg_index < args_ct) {
        declaring_or_assigning_type = params->params[param_index]->resolved_type;
        node->arguments[arg_index]->accept(this);
        assert_types_can_cast_or_equal(
            node->arguments[arg_index]->resolved_type, params->params[param_index]->resolved_type,
            node->arguments[arg_index]->source_range,
            std::format("unexpected argument type.. parameter #{} of function",
                        arg_index + 1)); // +1 here to make it 1 based indexing for user. more intuitive
      }
    } else {
      if (arg_index < args_ct) {
        declaring_or_assigning_type = Type::INVALID_TYPE_ID;
        node->arguments[arg_index]->accept(this);
        if (!params->is_varargs) {
          throw_error("Too many arguments to function", node->source_range);
        }
      }
    }
  }
}

void Typer::type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info) {
  auto old_type = declaring_or_assigning_type;
  Defer _([&]() { declaring_or_assigning_type = old_type; });
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
    declaring_or_assigning_type = info->parameter_types[i];
    arg->accept(this);
    if (i < info->params_len) {
      assert_types_can_cast_or_equal(arg->resolved_type, info->parameter_types[i], arg->source_range,
                                     std::format("invalid argument type for parameter #{}", i + 1));
    }
  }
}

ASTFunctionDeclaration *Typer::resolve_generic_function_call(ASTCall *node, ASTFunctionDeclaration *func) {
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

      if (gen_t->get_ext().is_pointer() && !func->params->params.empty()) {
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
  auto instantiation = visit_generic(&Typer::visit_function_signature, func, generic_args);
  if (!instantiation) {
    throw_error("Template instantiation argument count mismatch", node->source_range);
  }
  instantiation->generic_arguments = generic_args;
  visit_function_body(static_cast<ASTFunctionDeclaration *>(instantiation));
  return instantiation;
}

void Typer::visit(ASTArguments *node) {
  auto type = global_get_type(declaring_or_assigning_type);
  FunctionTypeInfo *info = nullptr;
  if (type) {
    info = dynamic_cast<FunctionTypeInfo *>(type->get_info());
  }
  for (int i = 0; i < node->arguments.size(); ++i) {
    auto arg = node->arguments[i];
    if (arg->get_node_type() == AST_NODE_SWITCH || arg->get_node_type() == AST_NODE_IF) {
      throw_error(
          "cannot use 'switch' or 'if' expressions in binary expressions, only `=`, `:=` and `return` statements",
          node->source_range);
    }
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

std::vector<TypeExtension> Typer::accept_extensions(std::vector<ASTTypeExtension> ast_extensions) {
  std::vector<TypeExtension> extensions;
  for (auto &ext : ast_extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      auto val = evaluate_constexpr(ext.expression, ctx);
      if (val.tag != Value::INTEGER) {
        throw_error("Fixed array must have integer size.", ext.expression->source_range);
      }
      extensions.push_back({ext.type, (size_t)val.integer});
    } else {
      extensions.push_back({.type = ext.type});
    }
  }
  return extensions;
}

void Typer::visit(ASTType *node) {
  if (node->resolved_type != Type::INVALID_TYPE_ID) {
    return;
  }

  TypeExtensions extensions;
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
      auto decl_node_type = declaring_node->get_node_type();

      GENERIC_PANIC_HANDLER(data, 1, {
        switch (decl_node_type) {
          case AST_NODE_STRUCT_DECLARATION:
            instantiation = visit_generic(&Typer::visit_struct_declaration, (ASTStructDeclaration *)declaring_node,
                                          generic_args);
            break;
          case AST_NODE_FUNCTION_DECLARATION:
            instantiation = visit_generic(&Typer::visit_function_signature,
                                          (ASTFunctionDeclaration *)declaring_node, generic_args);
            break;
          case AST_NODE_INTERFACE_DECLARATION:
            instantiation = visit_generic(&Typer::visit_interface_declaration,
                                          (ASTInterfaceDeclaration *)declaring_node, generic_args);
            break;
          case AST_NODE_TAGGED_UNION_DECLARATION: {
            instantiation = visit_generic(&Typer::visit_tagged_union_declaration,
                                          (ASTTaggedUnionDeclaration *)declaring_node, generic_args);
          } break;
          default:
            throw_error("Invalid target to generic args", node->source_range);
            break;
        }
      }, node->source_range);

      if (!instantiation) {
        throw_error("Template instantiation argument count mismatch", node->source_range);
      }

      GENERIC_PANIC_HANDLER(other_data, 2, {
        if (decl_node_type == AST_NODE_FUNCTION_DECLARATION) {
          auto func = static_cast<ASTFunctionDeclaration *>(instantiation);
          func->generic_arguments = generic_args;
          visit_function_body(func);
        } else if (decl_node_type == AST_NODE_STRUCT_DECLARATION) {
          auto struct_decl = static_cast<ASTStructDeclaration *>(instantiation);
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
    throw_error("internal compiler error: Invalid type kind", node->source_range);
  }
}

void Typer::visit(ASTBinExpr *node) {
  node->left->accept(this);
  auto left = node->left->resolved_type;

  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  if (node->op.type == TType::Assign) {
    declaring_or_assigning_type = left;
  } else if (node->left->get_node_type() == AST_NODE_SWITCH || node->right->get_node_type() == AST_NODE_SWITCH ||
             node->right->get_node_type() == AST_NODE_IF || node->left->get_node_type() == AST_NODE_IF) {
    throw_error("cannot use 'switch' or 'if' expressions in function arguments, they're only valid in `=`, `:=` and "
                "`return` statements",
                node->source_range);
  }

  node->right->accept(this);
  auto right = node->right->resolved_type;

  if (node->op.type == TType::Assign) {
    if (node->left->get_node_type() == AST_NODE_IDENTIFIER) {
      ctx.scope->insert_variable(((ASTIdentifier *)node->left)->value, node->left->resolved_type, node->right);
    }
  }

  auto left_ty = global_get_type(left);

  auto operator_overload_ty = find_operator_overload(node->op.type, left_ty, OPERATION_BINARY);
  if (operator_overload_ty != -1) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(operator_overload_ty)->get_info()->as<FunctionTypeInfo>()->return_type;
    return;
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
  if (node->operand->get_node_type() == AST_NODE_SWITCH || node->operand->get_node_type() == AST_NODE_IF) {
    throw_error("cannot use 'switch' or 'if' expressions in unary expressions. they're only valid in `=`, `:=` and "
                "`return` statements",
                node->source_range);
  }

  node->operand->accept(this);
  auto operand_ty = node->operand->resolved_type;

  auto type = global_get_type(operand_ty);

  auto overload = find_operator_overload(node->op.type, type, OPERATION_UNARY);
  if (overload != -1) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->get_info()->as<FunctionTypeInfo>()->return_type;
    return;
  }

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
  node->resolved_type = ctx.scope->find_type_id(node->value, {});
  if (node->resolved_type != Type::INVALID_TYPE_ID) {
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

      if (declaring_or_assigning_type != Type::INVALID_TYPE_ID) {
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
      node->resolved_type = f32_type();
      return;
    case ASTLiteral::String: {
      if (node->is_c_string) {
        static int type = global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER}}});
        ;
        node->resolved_type = type;
      } else {
        static size_t uid_idx = 0;
        static int type = ctx.scope->find_type_id("str", {});
        node->resolved_type = type;
      }
      return;
    }
    case ASTLiteral::Bool:
      node->resolved_type = bool_type();
      return;
    case ASTLiteral::Null:
      // infer pointer type from decl or assign type, else we just use void*, for like n := null;
      if (declaring_or_assigning_type != -1) {
        node->resolved_type = declaring_or_assigning_type;
        return;
      }
      node->resolved_type = voidptr_type();
      return;
    case ASTLiteral::Char:
      node->resolved_type = u32_type();
      return;
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

  Scope *base_scope = base_ty->get_info()->scope;

  // Implicit dereference, we look at the base scope.
  if (base_ty->get_ext().is_pointer()) {
    base_ty = global_get_type(base_ty_id = base_ty->get_element_type());
    base_scope = base_ty->get_info()->scope;
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
  Scope *scope = base_ty->get_info()->scope;
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

  auto overload = find_operator_overload(TType::LBrace, left_ty, OPERATION_SUBSCRIPT);
  if (overload != -1) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->get_info()->as<FunctionTypeInfo>()->return_type;
    return;
  }

  // * Todo: reimplement operator overloads with interfaces.

  auto ext = left_ty->get_ext();

  if (!ext.is_fixed_sized_array() && !ext.is_pointer()) {
    throw_error(std::format("cannot index into non-array, non-pointer type that doesn't implement 'subscript :: "
                            "fn(self*, idx: u32)' method. {}",
                            left_ty->to_string()),
                node->source_range);
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
      if (!target_type->is_kind(TYPE_STRUCT)) {
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
        assert_types_can_cast_or_equal(value_ty, symbol->type_id, value->source_range,
                                       "Unable to cast type to target field for named initializer list");
        value->resolved_type = symbol->type_id; // Again, we do this here to avoid annoyances with lowering to c++
      }
    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      // TODO:
      // We can support these types of initializer lists, by creating something in-language like
      // Init_List :: struct![T] {  ptr: T*; length: u64; } and passing this 'dynamic' array to a special function
      if (target_type->get_ext().is_fixed_sized_array()) {
        auto &values = node->values;
        // Zero init construction. Pretty redundant.
        if (values.empty()) {
          node->resolved_type = target_type->id;
          return;
        }

        auto target_element_type = target_type->get_element_type();
        values[0]->accept(this);
        auto element_type = values[0]->resolved_type;
        for (int i = 1; i < values.size(); ++i) {
          int type = Type::INVALID_TYPE_ID;
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
              type, element_type, values[i]->source_range,
              "Found inconsistent types in a collection-style initializer list. These types must be homogenous");

          values[i]->resolved_type =
              target_element_type; // We do this here to avoid casting problems with C/C++ init lists.
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
            element_type, target_element_type, node->source_range,
            "Failed to assign element type from value passed into collection-style initializer list");
        node->resolved_type = target_type->id;
      } else {
        if (!target_type->implements("Init") && !target_type->get_base().get_str().contains("Init_List$")) {
          throw_error("Unable to use 'collection style' initalizer lists on non fixed array types that don't implement "
                      "Init![T] interface",
                      node->source_range);
        }

        auto &values = node->values;
        // * How on earth will we infer this?
        // * I think we'll have to look at the target type,
        // * search for the init_list() function, check if it's generic,
        // * if it's not, use the concrete type argument for the Init_List![T] argument,
        // * otherwise if it is generic,
        // * we just allow any homogenous collection of values?
        // For now, we just do the latter - allow any collection of values.
        values[0]->accept(this);
        auto target_element_type = values[0]->resolved_type;
        for (int i = 1; i < values.size(); ++i) {
          int type = Type::INVALID_TYPE_ID;
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
              type, target_element_type, values[i]->source_range,
              "Found inconsistent types in a collection-style initializer list. These types must be homogenous");

          values[i]->resolved_type = target_element_type;
        }
        node->resolved_type = find_generic_type_of("Init_List", {target_element_type}, node->source_range);
        return;
      }
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

  auto conversion_rule_left_to_right = type_conversion_rule(global_get_type(left), global_get_type(right));
  auto conversion_rule_right_to_left = type_conversion_rule(global_get_type(right), global_get_type(left));

  // Alwyas cast to the left? or should we upcast to the largest number type?
  if (conversion_rule_left_to_right == CONVERT_NONE_NEEDED || conversion_rule_left_to_right == CONVERT_IMPLICIT) {
    right = node->right->resolved_type = left;
  } else if (conversion_rule_right_to_left == CONVERT_NONE_NEEDED ||
             conversion_rule_right_to_left == CONVERT_IMPLICIT) {
    left = node->left->resolved_type = right;
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
void Typer::visit(ASTSwitch *node) {
  node->target->accept(this);
  auto type_id = node->target->resolved_type;
  auto type = global_get_type(type_id);

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->get_ext().is_pointer()) {
    auto operator_overload = find_operator_overload(TType::EQ, type, OPERATION_BINARY);
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
  node->resolved_type = global_find_type_id(types, extensions);
}
void Typer::visit(ASTAlias *node) {
  node->type->accept(this);

  if (node->type->resolved_type == Type::INVALID_TYPE_ID) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }

  if (ctx.scope->symbols.contains(node->name)) {
    throw_error("Redeclaration of type", node->source_range);
  }

  auto type = global_get_type(node->type->resolved_type);

  ctx.scope->create_type_alias(node->name, node->type->resolved_type, type->kind);

  return;
}

void Typer::visit(ASTTupleDeconstruction *node) {
  node->right->accept(this);
  node->resolved_type = node->right->resolved_type;

  auto type = global_get_type(node->right->resolved_type);

  if (type->get_ext().has_extensions()) {
    throw_error("Cannot destructure pointer or array type.", node->source_range);
  }

  if (type->is_kind(TYPE_TUPLE)) {
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
      ctx.scope->insert_variable(iden->value, type, iden);
    }
  } else {
    auto scope = type->get_info()->scope;
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

    if (declaring_node->get_node_type() != AST_NODE_STRUCT_DECLARATION) {
      throw_error("generic `impl![...]` can only be used on structs, currently.", node->source_range);
    }

    auto node_as_struct = static_cast<ASTStructDeclaration *>(declaring_node);
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

void Typer::visit(ASTInterfaceDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->declare_interface(node->name, node);
  } else {
    visit_interface_declaration(node, false);
    ctx.scope->create_type_alias(node->name, node->resolved_type, TYPE_INTERFACE);
  }
  return;
}

int Typer::get_self_type() {
  if (type_context.is_not_null()) {
    type_context.get()->accept(this);
    return type_context.get()->resolved_type;
  }
  return Type::INVALID_TYPE_ID;
}

bool Typer::visit_where_predicate(Type *type, ASTExpr *node) {
  switch (node->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin = static_cast<ASTBinExpr *>(node);
      auto op = bin->op.type;
      if (op == TType::And) {
        return visit_where_predicate(type, bin->left) && visit_where_predicate(type, bin->right);
      } else if (op == TType::Or) {
        return visit_where_predicate(type, bin->left) || visit_where_predicate(type, bin->right);
      } else {
        throw_error("Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
                    "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
                    bin->source_range);
      }
    } break;
    case AST_NODE_TYPE: {
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

int Typer::find_generic_type_of(const InternedString &base, const std::vector<int> &generic_args,
                                const SourceRange &source_range) {
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

  auto decl_node_type = declaring_node->get_node_type();

  GENERIC_PANIC_HANDLER(
      data, 1,
      {
        switch (decl_node_type) {
          case AST_NODE_STRUCT_DECLARATION:
            instantiation =
                visit_generic(&Typer::visit_struct_declaration, (ASTStructDeclaration *)declaring_node, generic_args);
            break;
          case AST_NODE_FUNCTION_DECLARATION:
            instantiation =
                visit_generic(&Typer::visit_function_signature, (ASTFunctionDeclaration *)declaring_node, generic_args);
            break;
          case AST_NODE_INTERFACE_DECLARATION:
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
        if (decl_node_type == AST_NODE_FUNCTION_DECLARATION) {
          auto func = static_cast<ASTFunctionDeclaration *>(instantiation);
          func->generic_arguments = generic_args;
          visit_function_body(func);
        } else if (decl_node_type == AST_NODE_STRUCT_DECLARATION) {
          auto struct_decl = static_cast<ASTStructDeclaration *>(instantiation);
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
