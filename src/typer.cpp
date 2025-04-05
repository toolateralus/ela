
#include <algorithm>
#include <cassert>
#include <csetjmp>
#include <format>
#include <iostream>
#include <ostream>
#include <ranges>
#include <sstream>
#include <string>
#include <vector>

#include "ast.hpp"
#include "copier.hpp"
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

#define USE_GENERIC_PANIC_HANDLER

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

void assert_types_can_cast_or_equal(ASTExpr *expr, const int to, const SourceRange &source_range,
                                    const std::string &message) {
  auto from_t = global_get_type(expr->resolved_type);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t, source_range);
  if (to != expr->resolved_type && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' + std::format("expected \"{}\", got \"{}\"", to_t->to_string(), from_t->to_string()),
                source_range);
  }

  if (conv_rule == CONVERT_IMPLICIT) {
    if (to_t->is_kind(TYPE_TAGGED_UNION)) {
      expr->conversion = std::optional(ImplicitConversion{
          .to = to, .from = expr->resolved_type, .kind = ImplicitConversion::VARIANT_TO_TAGGED_UNION});
    } else if (to_t->get_base() == "any") {
      expr->conversion =
          std::optional(ImplicitConversion{.to = to, .from = expr->resolved_type, .kind = ImplicitConversion::TO_ANY});
    }
    expr->resolved_type = to;
  }
}

bool expr_is_literal(const ASTExpr *expr) {
  switch (expr->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin_expr = static_cast<const ASTBinExpr *>(expr);
      return expr_is_literal(bin_expr->left) && expr_is_literal(bin_expr->right);
    }
    case AST_NODE_UNARY_EXPR:
      return expr_is_literal(static_cast<const ASTUnaryExpr *>(expr)->operand);
    case AST_NODE_LITERAL:
      return true;
    default:
      return false;
  }
}

void Typer::visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                     std::vector<int> generic_args) {
  auto type = global_get_type(node->resolved_type);

  auto info = (type->get_info()->as<StructTypeInfo>());

  if (HAS_FLAG(info->flags, STRUCT_FLAG_FORWARD_DECLARED) || node->is_fwd_decl) {
    node->resolved_type = type->id;
    return;
  }

  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
    type = global_get_type(global_create_struct_type(node->name, node->scope, generic_args));
  }

  type->declaring_node = node;
  node->resolved_type = type->id;

  auto old_type_context = type_context;
  ASTType ast_type;
  ast_type.resolved_type = type->id;
  type_context = &ast_type;
  Defer _([&] { type_context = old_type_context; });

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  for (auto subunion : node->subtypes) {
    for (const auto &field : subunion->members) {
      field.type->accept(this);
      node->scope->insert_variable(field.name, field.type->resolved_type, nullptr, MUT);
    }
  }

  for (auto decl : node->members) {
    decl.type->accept(this);
    ctx.scope->insert_variable(decl.name, decl.type->resolved_type, nullptr, MUT);
    auto sym = ctx.scope->local_lookup(decl.name);
    if (sym->is_variable()) {
      sym->flags |= SYMBOL_IS_LOCAL;
    }
  }

  ctx.set_scope(old_scope);
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
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
    type = global_get_type(global_create_tagged_union_type(node->name.get_str(), node->scope, generic_args));
  }

  type->declaring_node = node;

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  auto info = type->get_info()->as<TaggedUnionTypeInfo>();
  info->scope = node->scope;

  for (const auto &variant : node->variants) {
    switch (variant.kind) {
      case ASTTaggedUnionVariant::NORMAL: {
        info->variants.push_back({variant.name, void_type()});
      } break;
      case ASTTaggedUnionVariant::TUPLE: {
        variant.tuple->accept(this);
        auto type = variant.tuple->resolved_type;
        info->variants.push_back({variant.name, type});
        info->scope->create_type_alias(variant.name, type, TYPE_TUPLE, variant.tuple);
      } break;
      case ASTTaggedUnionVariant::STRUCT: {
        auto scope = create_child(ctx.scope);
        ctx.set_scope(scope);
        for (const auto &field : variant.struct_declarations) {
          field->accept(this);
        }
        ctx.exit_scope();
        auto type = info->scope->create_struct_type(variant.name, scope, nullptr);
        info->variants.push_back({variant.name, type});
      } break;
    }
  }
  ctx.exit_scope();
}

void Typer::visit_function_body(ASTFunctionDeclaration *node) {
  auto old_ty = expected_type;
  auto old_scope = ctx.scope;
  auto _defer = Defer([&] {
    ctx.set_scope(old_scope);
    expected_type = old_ty;
  });
  ctx.set_scope(node->scope);
  expected_type = node->return_type->resolved_type;
  auto block = node->block.get();
  if (!block) {
    throw_error("internal compiler error: attempting to visit body of function forward declaration.",
                node->source_range);
  }
  block->accept(this);
  auto control_flow = block->control_flow;
  if (control_flow.type == Type::INVALID_TYPE_ID)
    control_flow.type = void_type();
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_CONTINUE))
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_BREAK))
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_FALL_THROUGH) && node->return_type->resolved_type != void_type())
    throw_error("Not all code paths return a value.", node->source_range);
}

int Typer::get_self_type() {
  if (type_context.is_not_null()) {
    type_context.get()->accept(this);
    return type_context.get()->resolved_type;
  }
  return Type::INVALID_TYPE_ID;
}

std::string print_where_predicate(ASTExpr *predicate) {
  switch (predicate->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin = static_cast<ASTBinExpr *>(predicate);
      auto op = bin->op.type;
      if (op == TType::And) {
        return print_where_predicate(bin->left) + " & " + print_where_predicate(bin->right);
      } else if (op == TType::Or) {
        return print_where_predicate(bin->left) + " | " + print_where_predicate(bin->right);
      } else {
        throw_error("Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
                    "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
                    bin->source_range);
      }
    } break;
    case AST_NODE_TYPE: {
      return global_get_type(predicate->resolved_type)->to_string();
    } break;
    default:
      throw_error("Invalid node in 'where' clause predicate", predicate->source_range);
  }
  return "";
}

bool Typer::visit_where_predicate(Type *target_type, ASTExpr *predicate) {
  switch (predicate->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin = static_cast<ASTBinExpr *>(predicate);
      auto op = bin->op.type;
      if (op == TType::And) {
        return visit_where_predicate(target_type, bin->left) && visit_where_predicate(target_type, bin->right);
      } else if (op == TType::Or) {
        return visit_where_predicate(target_type, bin->left) || visit_where_predicate(target_type, bin->right);
        return true;
      } else {
        throw_error("Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
                    "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
                    bin->source_range);
      }
    } break;
    case AST_NODE_TYPE: {
      // make sure the type is fixed up.
      predicate->accept(this);
      // return whether this type implements this trait or not.
      // also can be used to assert whether it's equal to the type provided or not.
      return std::ranges::find(target_type->interfaces, predicate->resolved_type) != target_type->interfaces.end() ||
             target_type->id == predicate->resolved_type;
    } break;
    default:
      throw_error("Invalid node in 'where' clause predicate", predicate->source_range);
  }
  return false;
}

void Typer::visit(ASTWhere *node) {
  for (auto &constraint : node->constraints) {
    auto [target_type, predicate] = constraint;
    target_type->accept(this);
    auto type = global_get_type(target_type->resolved_type);
    auto satisfied = visit_where_predicate(type, predicate);

    if (!satisfied) {
      throw_error(std::format("constraint \"{}\" not satisfied for {}", print_where_predicate(predicate),
                              get_unmangled_name(type)),
                  node->source_range);
    }
  }
}

int Typer::find_generic_type_of(const InternedString &base, const std::vector<int> &generic_args,
                                const SourceRange &source_range) {
  ASTStatement *instantiation = nullptr;
  auto symbol = ctx.scope->lookup(base);

  // Probably not a generic type?
  if (!symbol || !symbol->is_type()) {
    return Type::INVALID_TYPE_ID;
  }

  auto declaring_node = symbol->type.declaration.get();

  if (!declaring_node) {
    throw_error("internal compiler error: unable to find type's declaring node", source_range);
  }

  switch (declaring_node->get_node_type()) {
    case AST_NODE_STRUCT_DECLARATION:
    case AST_NODE_FUNCTION_DECLARATION:
    case AST_NODE_INTERFACE_DECLARATION:
      break;
    default:
      throw_error("Invalid target to generic args", source_range);
      break;
  }
  instantiation = visit_generic((ASTDeclaration *)declaring_node, generic_args, source_range);
  return global_find_type_id(instantiation->resolved_type, {});
}

void Typer::visit_function_header(ASTFunctionDeclaration *node, bool generic_instantiation,
                                  std::vector<int> generic_args) {
  // Setup context.
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  Defer _([&] { ctx.set_scope(old_scope); });

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
  }

  if ((node->flags & (FUNCTION_IS_FORWARD_DECLARED | FUNCTION_IS_FOREIGN)) == 0) {
    node->scope->name = node->name.get_str() + mangled_type_args(generic_args);
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
      ctx.scope->insert_variable(normal.name, param->resolved_type, nullptr, param->mutability);
      ctx.scope->local_lookup(normal.name)->flags |= SYMBOL_IS_LOCAL;
      info.parameter_types[info.params_len] = param->resolved_type;
    } else {
      auto type = get_self_type();
      if (param->self.is_pointer) {
        type = global_get_type(type)->take_pointer_to(param->mutability);
      }

      if (param->tag == ASTParamDecl::Себя) {
        ctx.scope->insert_variable("себя", type, nullptr, param->mutability);
        ctx.scope->local_lookup("себя")->flags |= SYMBOL_IS_LOCAL;
      } else {
        ctx.scope->insert_variable("self", type, nullptr, param->mutability);
        ctx.scope->local_lookup("self")->flags |= SYMBOL_IS_LOCAL;
      }
      info.parameter_types[info.params_len] = type;
    }

    info.params_len++;
  }

  if (info.return_type == Type::UNRESOLVED_GENERIC_TYPE_ID) {
    throw_error("internal compiler error: unresolved generic return type.", node->source_range);
  }
  node->resolved_type = global_find_function_type_id(info, {});
}

bool impl_method_matches_interface(int interface_method, int impl_method) {
  auto interface = global_get_type(interface_method)->get_info()->as<FunctionTypeInfo>();
  auto impl = global_get_type(impl_method)->get_info()->as<FunctionTypeInfo>();

  if (interface->params_len != impl->params_len) {
    return false;
  }

  for (int i = 0; i < interface->params_len; ++i) {
    auto interface_param = global_get_type(interface->parameter_types[i]);
    auto impl_param = global_get_type(impl->parameter_types[i]);
    if (interface_param->is_kind(TYPE_INTERFACE)) {
      if (!impl_param->implements(interface_param->id)) {
        return false;
      }
      if (interface_param->generic_args != impl_param->generic_args) {
        return false;
      }
    } else if (interface_param != impl_param) {
      return false;
    }
  }

  {
    auto interface_return = global_get_type(interface->return_type);
    auto impl_return = global_get_type(impl->return_type);
    if (interface_return->is_kind(TYPE_INTERFACE)) {
      if (interface_return->generic_base_id != Type::INVALID_TYPE_ID) {
        if (!impl_return->implements(interface_return->generic_base_id)) {
          return false;
        }
        if (interface_return->generic_args != impl_return->generic_args) {
          return false;
        }
      } else {
        if (!impl_return->implements(interface_return->id)) {
          return false;
        }
      }
    } else if (interface_return != impl_return) {
      return false;
    }
  }

  return true;
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
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  node->target->accept(this);
  node->scope->name = std::to_string(node->target->resolved_type) + "impl";

  auto target_ty = global_get_type(node->target->resolved_type);
  if (!target_ty) {
    if (node->target->resolved_type == Type::INVALID_TYPE_ID) {
      throw_error("use of undeclared type", node->target->source_range);
    } else if (node->target->resolved_type == Type::UNRESOLVED_GENERIC_TYPE_ID) {
      throw_error("use of unresolved generic type", node->target->source_range);
    }
  }
  Type *interface_ty = nullptr;

  if (node->interface) {
    node->interface.get()->accept(this);
    auto interface_id = node->interface.get()->resolved_type;
    if (interface_id == Type::INVALID_TYPE_ID) {
      throw_error("internal compiler error: type of impl interface was invalid", node->source_range);
    }
    interface_ty = global_get_type(interface_id);
    node->scope->name = node->scope->name.get_str() + "_of" + std::to_string(interface_id);
  } else {
  }

  auto type_scope = target_ty->get_info()->scope;
  Scope impl_scope = {};

  for (const auto &constant : node->constants) {
    auto old = ctx.scope;
    ctx.scope = type_scope;
    constant->accept(this);
    ctx.scope = old;
  }

  for (const auto &alias : node->aliases) {
    // auto old_scope = ctx.scope;
    // Defer _([&]{
    //   ctx.scope = old_scope;
    // });
    // ctx.set_scope(type_scope); // is this correct?
    alias->accept(this);
  }

  // We forward declare all the methods so they can refer to each other without obnoxious crud crap.
  // just like C-- (owned)
  for (const auto &method : node->methods) {
    method->declaring_type = target_ty->id;

    if (!method->generic_parameters.empty()) {
      type_scope->insert_function(method->name, Type::UNRESOLVED_GENERIC_TYPE_ID, method);
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      continue;
    }

    visit_function_header(method, false);
    type_scope->insert_function(method->name, method->resolved_type, method,
                                SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
    impl_scope.symbols[method->name] = type_scope->symbols[method->name];
  }

  for (const auto &method : node->methods) {
    //// TODO: handle more attributes
    for (auto attr : method->attributes) {
      if (attr.tag == ATTRIBUTE_INLINE) {
        method->flags |= FUNCTION_IS_INLINE;
      }
    }

    method->declaring_type = target_ty->id;
    if (!method->generic_parameters.empty()) {
      // TODO: actually generate a signature for a generic function so that you can compare them
      type_scope->insert_function(method->name, Type::UNRESOLVED_GENERIC_TYPE_ID, method);
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      continue;
    }

    auto func_ty_id = method->resolved_type;

    if (auto symbol = type_scope->local_lookup(method->name)) {
      if (!(symbol->flags & SYMBOL_IS_FORWARD_DECLARED)) {
        throw_error("Redefinition of method", method->source_range);
      } else {
        symbol->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
      }
    } else {
      if (HAS_FLAG(method->flags, FUNCTION_IS_FORWARD_DECLARED)) {
        type_scope->insert_function(method->name, method->resolved_type, method,
                                    SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
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
      if (!interface_sym.is_function())
        continue;

      if (auto impl_symbol = impl_scope.local_lookup(name)) {
        if (!impl_method_matches_interface(interface_sym.type_id, impl_symbol->type_id)) {
          if (interface_sym.type_id != Type::INVALID_TYPE_ID && impl_symbol->type_id != Type::INVALID_TYPE_ID) {
            throw_error(std::format("method \"{}\" doesn't match interface.\nexpected {},\ngot {}", name,
                                    global_get_type(interface_sym.type_id)->to_string(),
                                    global_get_type(impl_symbol->type_id)->to_string()),
                        node->source_range);
          } else {
            throw_error("internal compiler error: method.type_id or impl_symbol.type_id was null", node->source_range);
          }
        }
      } else if (DOESNT_HAVE_FLAG(interface_sym.function.declaration->flags, FUNCTION_IS_FORWARD_DECLARED)) {
        auto method = (ASTFunctionDeclaration *)deep_copy_ast(interface_sym.function.declaration);
        method->declaring_type = target_ty->id;

        if (!method->generic_parameters.empty()) {
          // TODO: actually generate a signature for a generic function so that you can compare them
          type_scope->insert_function(method->name, Type::UNRESOLVED_GENERIC_TYPE_ID, method);
          impl_scope.symbols[method->name] = type_scope->symbols[method->name];
          continue;
        }

        visit_function_header(method, false);
        auto func_ty_id = method->resolved_type;

        if (auto symbol = type_scope->local_lookup(method->name)) {
          if (!(symbol->flags & SYMBOL_IS_FORWARD_DECLARED)) {
            throw_error("Redefinition of method", method->source_range);
          } else {
            symbol->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
          }
        } else {
          if (HAS_FLAG(method->flags, FUNCTION_IS_FORWARD_DECLARED)) {
            type_scope->insert_function(method->name, method->resolved_type, method,
                                        SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
          } else {
            type_scope->insert_function(method->name, method->resolved_type, method);
          }
          impl_scope.symbols[method->name] = type_scope->symbols[method->name];
          if (method->flags & FUNCTION_IS_FOREIGN || method->flags & FUNCTION_IS_FORWARD_DECLARED) {
            continue;
          }
        }
        visit_function_body(method);
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
  if (id != Type::INVALID_TYPE_ID) {
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

  auto type = global_get_type(global_create_interface_type(node->name.get_str(), ctx.scope, generic_args));

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto kind = global_get_type(*generic_arg)->kind;
      ctx.scope->create_type_alias(param, *generic_arg, kind, node);
      generic_arg++;
    }
    type->generic_base_id = ctx.scope->lookup(node->name)->type_id;
  }

  node->scope->name = node->name.get_str() + mangled_type_args(generic_args);

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  type->declaring_node = node;
  node->resolved_type = type->id;
}

void Typer::compiler_mock_associated_function_call_visit_impl(int left_type, const InternedString &method_name) {
  auto old_call_state = in_call;
  in_call = true;
  Defer _([&] { in_call = old_call_state; });
  ASTCall call;
  ASTArguments arguments;
  call.arguments = &arguments;

  // Type.
  ASTIdentifier left;
  left.value = global_get_type(left_type)->get_base();

  ASTScopeResolution sr;
  sr.base = &left;
  sr.member_name = method_name;

  call.function = &sr;
  call.accept(this);
}

void Typer::compiler_mock_method_call_visit_impl(int left_type, const InternedString &method_name) {
  auto old_call_state = in_call;
  in_call = true;
  Defer _([&] { in_call = old_call_state; });
  ASTCall call;
  ASTArguments arguments;
  call.arguments = &arguments;

  // Type.
  ASTIdentifier left;
  static int depth = 0;

  left.value = "$$temp$$" + std::to_string(depth++);
  ctx.scope->insert_variable(left.value, left_type, nullptr, MUT);
  ctx.scope->local_lookup(left.value)->flags |= SYMBOL_IS_LOCAL;

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

bool is_const_pointer(ASTNode *node) {
  if (node == nullptr)
    return false;

  if (auto subscript = dynamic_cast<ASTSubscript *>(node)) {
    return is_const_pointer(subscript->left);
  } else if (auto dot = dynamic_cast<ASTDotExpr *>(node)) {
    return is_const_pointer(dot->base);
  }

  auto type = global_get_type(node->resolved_type);
  if (type->get_ext().is_const_pointer()) {
    return true;
  }

  return false;
}

void Typer::type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, Nullable<ASTExpr> self_nullable) {
  auto old_type = expected_type;
  Defer _([&]() { expected_type = old_type; });
  auto args_ct = node->arguments.size();
  auto params_ct = params->params.size();
  int param_index = self_nullable.is_not_null() ? 1 : 0;

  { // Check the other parameters, besides self.
    for (int arg_index = 0; arg_index < args_ct || param_index < params_ct; ++arg_index, ++param_index) {
      if (param_index < params_ct) {
        auto &param = params->params[param_index];
        if (arg_index < args_ct) {
          expected_type = param->resolved_type;
          node->arguments[arg_index]->accept(this);

          assert_types_can_cast_or_equal(
              node->arguments[arg_index], param->resolved_type, node->arguments[arg_index]->source_range,
              std::format("unexpected argument type.. parameter #{} of function",
                          arg_index + 1)); // +1 here to make it 1 based indexing for user. more intuitive

        } else {
          std::stringstream ss;
          ss << "Too few arguments to function. Expected:\n  fn(";
          for (auto param : params->params) {
            if (param->tag == ASTParamDecl::Normal) {
              ss << param->normal.name.get_str() << ": "
                 << global_get_type(param->normal.type->resolved_type)->to_string() << ", ";
            } else {
              ss << (param->self.is_pointer ? "*" : "") << "self, ";
            }
          }
          ss << ")\nbut got:\n";
          ss << "  fn(";

          for (auto arg : node->arguments) {
            ss << global_get_type(arg->resolved_type)->to_string() << ", ";
          }
          ss << ")\n";
          throw_error(ss.str(), node->source_range);
        }
      } else {
        if (!params->is_varargs) {
          std::stringstream ss;
          ss << "Too many arguments to function. Expected:\n  fn(";
          for (auto param : params->params) {
            if (param->tag == ASTParamDecl::Normal) {
              ss << param->normal.name.get_str() << ": "
                 << global_get_type(param->normal.type->resolved_type)->to_string() << ", ";
            } else {
              ss << (param->self.is_pointer ? "*" : "") << "self, ";
            }
          }
          ss << ")\nbut got:\n  fn(";

          for (auto arg : node->arguments) {
            ss << global_get_type(arg->resolved_type)->to_string() << ", ";
          }
          ss << ")\n";
          throw_error(ss.str(), node->source_range);
        }
        expected_type = Type::INVALID_TYPE_ID;
        node->arguments[arg_index]->accept(this);
      }
    }
  }

  if (self_nullable.is_not_null()) {
    auto self = self_nullable.get();
    auto first = global_get_type(params->params[0]->resolved_type);
    auto self_symbol = ctx.get_symbol(self);
    auto self_type = global_get_type(self->resolved_type);

    if (first->get_ext().is_mut_pointer()) {
      if (!self_type->get_ext().is_pointer() && self_symbol && self_symbol.get()->is_const()) {
        throw_error("cannot call a '*mut self' method with a const variable, consider adding 'mut' to the declaration.",
                    node->source_range);
      }
      if (is_const_pointer(self)) {
        throw_error("cannot call a '*mut self' method with a const pointer, consider taking it as '&mut' (or however "
                    "you obtained this pointer)",
                    node->source_range);
      }
    }
  }
}

void Typer::type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info) {
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
      assert_types_can_cast_or_equal(arg, info->parameter_types[i], arg->source_range,
                                     std::format("invalid argument type for parameter #{}", i + 1));
    }
  }
}

/*
  ! When you provide the wrong number of arguments (or at least too few) to a generic function that is inferring it's
  genericsd, ! the compiler just crashes and doesn't report an error REPRO: 103
*/
ASTFunctionDeclaration *Typer::resolve_generic_function_call(ASTCall *node, ASTFunctionDeclaration *func) {
  std::vector<int> generic_args;

  if (node->generic_arguments.empty()) {
    // infer generic parameter (return type only) from expected type
    if (node->arguments->arguments.empty() && func->generic_parameters.size() == 1) {
      if (func->return_type->kind == ASTType::NORMAL) {
        auto identifier = dynamic_cast<ASTIdentifier *>(func->return_type->normal.base);
        if (identifier && func->generic_parameters[0] == identifier->value) {
          auto type = ast_alloc<ASTType>();
          type->resolved_type = expected_type;
          type->source_range = type->source_range;
          node->generic_arguments.push_back(type);
        }
      }
    } else { // Infer generic parameter(S) from arguments.

      /*
        ! This is the cause of repro 106.
        ! I don't quite know  how to fix it, but somehow we need to
        ! at least unset the expected type here? that will be a temporary fix.
      */
      {
        auto old_expected = expected_type;
        expected_type = Type::INVALID_TYPE_ID;
        node->arguments->accept(this);
        expected_type = old_expected;
      }

      auto arguments = node->arguments->resolved_argument_types;
      auto parameters = func->params->params;
      auto generics = func->generic_parameters;

      std::vector<std::pair<bool, int>> arg_to_generic_map(parameters.size());

      for (size_t i = 0; i < parameters.size(); ++i) {
        auto &param = parameters[i];
        switch (param->tag) {
          case ASTParamDecl::Self:
          case ASTParamDecl::Себя: {
            arg_to_generic_map[i] = {false, Type::INVALID_TYPE_ID};
          } break;
          case ASTParamDecl::Normal: {
            bool is_generic = false;
            int generic_index = 0;

            for (const auto &generic : generics) {
              auto identifier = dynamic_cast<ASTIdentifier *>(param->normal.type->normal.base);
              if (identifier && generic == identifier->value) {
                is_generic = true;
                break;
              }
              ++generic_index;
            }

            arg_to_generic_map[i] = {is_generic, generic_index};
          } break;
        }
      }

      std::vector<int> inferred_generics(generics.size(), Type::INVALID_TYPE_ID);

      auto start_index = (parameters[0]->tag == ASTParamDecl::Self || parameters[0]->tag == ASTParamDecl::Себя) ? 1 : 0;

      for (size_t i = 0; i < arguments.size(); ++i) {
        auto arg_ty_id = arguments[i];
        auto [is_generic, generic_index] = arg_to_generic_map[i + start_index];

        if (is_generic) {
          auto extensions = parameters[i + start_index]->normal.type->extensions;

          int pointer_levels = 0;

          while (!extensions.empty() &&
                 (extensions.back().type == TYPE_EXT_POINTER_CONST || extensions.back().type == TYPE_EXT_POINTER_MUT)) {
            pointer_levels++;
            extensions.pop_back();
          }

          auto type = global_get_type(arg_ty_id);
          while (pointer_levels > 0 && type->get_ext().is_pointer()) {
            arg_ty_id = type->get_element_type();
            type = global_get_type(arg_ty_id);
            pointer_levels--;
          }

          inferred_generics[generic_index] = arg_ty_id;
        }
      }

      for (auto i = 0; i < generics.size(); ++i) {
        auto type = ast_alloc<ASTType>();
        type->source_range = node->source_range;
        type->resolved_type = inferred_generics[i];
        node->generic_arguments.push_back(type);
      }
    }
  }

  generic_args = get_generic_arg_types(node->generic_arguments);

  return (ASTFunctionDeclaration *)visit_generic(func, generic_args, node->source_range);
}

ASTDeclaration *Typer::visit_generic(ASTDeclaration *definition, std::vector<int> args, SourceRange source_range) {
  GenericInstantiationErrorUserData data;
  set_panic_handler(generic_instantiation_panic_handler);
  set_error_user_data(&data);
  Defer defer_1([] { reset_panic_handler(); });
  if (_setjmp(data.save_state) == 0) {
    if (definition->generic_parameters.size() != args.size()) {
      if (definition->generic_parameters.size() > args.size()) {
        throw_error(std::format("too few generic arguments. expected {}, got {}", definition->generic_parameters.size(),
                                args.size()),
                    definition->source_range);
      } else {
        throw_error(std::format("too many generic arguments. expected {}, got {}",
                                definition->generic_parameters.size(), args.size()),
                    definition->source_range);
      }
    }
    auto instantiation = find_generic_instance(definition->generic_instantiations, args);
    if (!instantiation) {
      instantiation = static_cast<ASTDeclaration *>(deep_copy_ast(definition));
      definition->generic_instantiations.push_back({args, instantiation});
      switch (definition->get_node_type()) {
        case AST_NODE_STRUCT_DECLARATION: {
          visit_struct_declaration((ASTStructDeclaration *)instantiation, true, args);
        } break;
        case AST_NODE_FUNCTION_DECLARATION: {
          visit_function_header((ASTFunctionDeclaration *)instantiation, true, args);
          auto func = static_cast<ASTFunctionDeclaration *>(instantiation);
          func->generic_arguments = args;
          visit_function_body(func);
        } break;
        case AST_NODE_INTERFACE_DECLARATION:
          visit_interface_declaration((ASTInterfaceDeclaration *)instantiation, true, args);
          break;
        case AST_NODE_TAGGED_UNION_DECLARATION: {
          visit_tagged_union_declaration((ASTTaggedUnionDeclaration *)instantiation, true, args);
        } break;
        case AST_NODE_IMPL: {
          visit_impl_declaration((ASTImpl *)instantiation, true, args);
        } break;
        default:
          throw_error("Invalid target to generic args", source_range);
          break;
      }
      for (auto impl : instantiation->impls) {
        if (impl->resolved_type == Type ::INVALID_TYPE_ID) {
          visit_generic(impl, args, source_range);
        }
      }
      instantiation->generic_parameters.clear();
      instantiation->generic_instantiations.clear();
    }
    return instantiation;
  } else {
    handle_generic_error(&data, source_range);
    return nullptr;
  }
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

std::vector<int> Typer::get_generic_arg_types(const std::vector<ASTType *> &args) {
  std::vector<int> generic_args;
  for (const auto &arg : args) {
    arg->accept(this);
    generic_args.push_back(arg->resolved_type);
  }
  return generic_args;
}

void Typer::visit(ASTProgram *node) {
  ctx.set_scope(ctx.root_scope);
  size_t index = 0;
  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
    }
    statement->accept(this);
    index++;
  }
  ctx.set_scope(ctx.root_scope);
}

void Typer::visit(ASTTaggedUnionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->create_type_alias(node->name, Type::INVALID_TYPE_ID, TypeKind(0), node);
    return;
  }
  visit_tagged_union_declaration(node, false);
}

void Typer::visit(ASTLambda *node) {
  node->unique_identifier = "$lambda$" + std::to_string(LAMBDA_UNIQUE_ID++);
  node->params->accept(this);
  node->return_type->accept(this);

  auto old_expected = expected_type;

  Defer _([&] { expected_type = old_expected; });

  expected_type = node->return_type->resolved_type;

  std::vector<int> param_types;
  FunctionTypeInfo info;

  int parameter_index = 0;
  for (const auto &param : node->params->params) {
    info.parameter_types[parameter_index] = param->resolved_type;
    info.params_len++;
    node->block->scope->insert_variable(param->normal.name, param->resolved_type, nullptr, param->mutability);
    node->block->scope->local_lookup(param->normal.name)->flags |= SYMBOL_IS_LOCAL;
    parameter_index++;
  }

  node->block->accept(this);
  info.return_type = node->return_type->resolved_type;
  auto type = global_find_function_type_id(info, {});
  node->resolved_type = global_get_type(type)->take_pointer_to(MUT);

  // w????
  // std::cout << global_get_type(node->resolved_type)->to_string() << '\n';
}

void Typer::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->create_type_alias(node->name, Type::UNRESOLVED_GENERIC_TYPE_ID, TYPE_STRUCT, node);
  } else {
    visit_struct_declaration(node, false);
  }
}

void Typer::visit(ASTEnumDeclaration *node) {
  auto elem_type = Type::INVALID_TYPE_ID;
  auto enum_ty_id = ctx.scope->create_enum_type(node->name, create_child(ctx.scope), node->is_flags, node);
  global_get_type(enum_ty_id)->declaring_node = node;
  auto enum_type = global_get_type(ctx.scope->find_type_id(node->name, {}));
  auto info = enum_type->get_info()->as<EnumTypeInfo>();

  for (const auto &[key, value] : node->key_values) {
    value->accept(this);
    auto node_ty = value->resolved_type;
    info->scope->insert_variable(key, node_ty, value, CONST);
    if (elem_type == Type::INVALID_TYPE_ID) {
      elem_type = node_ty;
    } else {
      assert_types_can_cast_or_equal(value, elem_type, node->source_range, "inconsistent types in enum declaration.");
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
  for (auto attr : node->attributes) {
    if (attr.tag == ATTRIBUTE_INLINE) {
      node->flags |= FUNCTION_IS_INLINE;
    }
  }

  if (!node->generic_parameters.empty()) {
    // TODO: actually generate a signature for a generic function so that you can compare them
    ctx.scope->insert_function(node->name, Type::UNRESOLVED_GENERIC_TYPE_ID, node);
    return;
  }

  visit_function_header(node, false);

  if (HAS_FLAG(node->flags, FUNCTION_IS_FORWARD_DECLARED)) {
    ctx.scope->insert_function(node->name, node->resolved_type, node,
                               SymbolFlags(SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION));
    return;
  }

  ctx.scope->insert_function(node->name, node->resolved_type, node);

  if (HAS_FLAG(node->flags, FUNCTION_IS_FOREIGN)) {
    return;
  }

  visit_function_body(node);
}

void Typer::visit(ASTVariable *node) {
  // Inferred declaration.
  if (node->type == nullptr) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      throw_error("Cannot use a type as a value.", node->value.get()->source_range);
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
      throw_error("Cannot use a type as a value.", node->value.get()->source_range);
    }

    auto old_ty = expected_type;
    expected_type = node->type->resolved_type;
    Defer _defer([&] { expected_type = old_ty; });
    node->value.get()->accept(this);
    assert_types_can_cast_or_equal(node->value.get(), node->type->resolved_type, node->source_range,
                                   "invalid type in declaration");
  }

  if (!node->is_constexpr && ctx.scope->local_lookup(node->name)) {
    throw_error(std::format("re-definition of '{}'", node->name), node->source_range);
  }

  auto variable_type = node->type->resolved_type;
  ctx.scope->insert_variable(node->name, variable_type, node->value.get(), node->mutability, node);
  if (node->is_local) {
    ctx.scope->local_lookup(node->name)->flags |= SYMBOL_IS_LOCAL;
  }

  if (variable_type == void_type()) {
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

  int return_type = Type::INVALID_TYPE_ID;
  node->control_flow.type = Type::INVALID_TYPE_ID;
  for (auto &statement : node->statements) {
    statement->accept(this);
    auto &stmnt_cf = statement->control_flow;
    auto &block_cf = node->control_flow;
    block_cf.flags |= stmnt_cf.flags;

    if (DOESNT_HAVE_FLAG(stmnt_cf.flags, BLOCK_FLAGS_FALL_THROUGH)) {
      block_cf.flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      block_cf.type = stmnt_cf.type;
    }
  }

  node->flags = node->control_flow.flags;
  node->resolved_type = node->control_flow.type;
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
  if (node->tag == ASTParamDecl::Self || node->tag == ASTParamDecl::Себя) {
    if (!type_context) {
      throw_error("No target type for self", node->source_range);
    }
    node->resolved_type = get_self_type();
    if (node->self.is_pointer) {
      node->resolved_type = global_get_type(node->resolved_type)->take_pointer_to(node->mutability);
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
        node->resolved_type = global_get_type(element)->take_pointer_to(CONST);
      }
    }

    auto old_ty = expected_type;
    expected_type = id;
    Defer _defer([&] { expected_type = old_ty; });
  }
}

void Typer::visit(ASTReturn *node) {
  int type;
  if (node->expression.is_not_null()) {
    node->expression.get()->accept(this);
    type = node->expression.get()->resolved_type;

    if (expected_type != Type::INVALID_TYPE_ID && expected_type != void_type()) {
      assert_types_can_cast_or_equal(node->expression.get(), expected_type, node->source_range, "invalid return type");
    }
  } else {
    type = ctx.scope->find_type_id("void", {});
  }
  node->resolved_type = type;
  node->control_flow = {BLOCK_FLAGS_RETURN, type};
}

void Typer::visit(ASTContinue *node) { node->control_flow = {BLOCK_FLAGS_CONTINUE, Type::INVALID_TYPE_ID}; }

void Typer::visit(ASTBreak *node) { node->control_flow = {BLOCK_FLAGS_BREAK, Type::INVALID_TYPE_ID}; }

void Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  node->right->accept(this);
  int iterable_type_id = node->right->resolved_type;
  Type *iterable_type = global_get_type(iterable_type_id);
  node->iterable_type = iterable_type->id;

  if (iterable_type->get_ext().is_pointer()) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            iterable_type->to_string()),
                node->source_range);
  }

  int iter_ty = Type::INVALID_TYPE_ID;
  auto scope = iterable_type->get_info()->scope;

  if (iterable_type->implements(iterable_interface())) { // can return an iterator.
    node->iteration_kind = ASTFor::ITERABLE;

    compiler_mock_method_call_visit_impl(iterable_type_id, "iter");
    auto symbol = scope->local_lookup("iter");
    auto symbol_ty = global_get_type(symbol->type_id);
    auto iter_return_ty = global_get_type(symbol_ty->get_info()->as<FunctionTypeInfo>()->return_type);
    node->iterator_type = iter_return_ty->id;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_method_call_visit_impl(iter_return_ty->id, "next");
    symbol = iter_return_ty->get_info()->scope->local_lookup("next");
    iter_ty = global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type;
    auto option = global_get_type(iter_ty);
    iter_ty = option->generic_args[0];
  } else if (iterable_type->implements(iterator_interface())) { // directly an iterator.
    node->iteration_kind = ASTFor::ITERATOR;
    node->iterator_type = iterable_type_id;

    // make sure the impl is actually emitted if this is generic.
    auto iterable_type = global_get_type(iterable_type_id);
    compiler_mock_method_call_visit_impl(iterable_type_id, "next");
    auto symbol = iterable_type->get_info()->scope->local_lookup("next");
    iter_ty = global_get_type(symbol->type_id)->get_info()->as<FunctionTypeInfo>()->return_type;
    auto option = global_get_type(iter_ty);
    iter_ty = option->generic_args[0];
  } else {
    throw_error("cannot iterate with for-loop on a type that doesn't implement either the 'Iterable!<T>' or the "
                "'Iterator!<T>' interface. ",
                node->source_range);
  }

  node->identifier_type = iter_ty;

  if (node->left_tag == ASTFor::IDENTIFIER) {
    auto iden = node->left.identifier;
    ctx.scope->insert_variable(iden->value, iter_ty, iden, MUT);
    ctx.scope->local_lookup(iden->value)->flags |= SYMBOL_IS_LOCAL;
    node->left.identifier->accept(this);
  } else {
    auto type = global_get_type(iter_ty);

    // if our iterable type returns a pointer, we "dereference" here because of the way destructuring needs to be done
    // it doesn't actually get generated as a dereference but we need to analyze the scope of the base type.
    if (type->get_ext().is_pointer()) {
      type = global_get_type(type->get_element_type());
    }

    auto scope = type->get_info()->scope;
    if (node->left.destructure.size() != scope->fields_count()) {
      throw_error(std::format("Cannot currently partially deconstruct a struct. "
                              "expected {} identifiers to assign, got {}, for type {}",
                              scope->symbols.size(), node->left.destructure.size(), type->to_string()),
                  node->source_range);
    }
    int i = 0;
    for (auto name : scope->ordered_symbols) {
      auto symbol = scope->local_lookup(name);
      if (symbol->is_function() || symbol->is_type()) {
        continue;
      }

      auto &destructure = node->left.destructure[i];
      auto iden = destructure.identifier;
      auto type_id = symbol->type_id;
      if (destructure.semantic == VALUE_SEMANTIC_POINTER) {
        type_id = global_get_type(type_id)->take_pointer_to(destructure.mutability);
      }
      ctx.scope->insert_variable(iden->value, type_id, iden, MUT);
      ctx.scope->local_lookup(iden->value)->flags |= SYMBOL_IS_LOCAL;
      i++;
    }
  }

  node->right->accept(this);

  ctx.exit_scope();
  node->block->accept(this);

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
  node->function->accept(this);
  auto symbol = ctx.get_symbol(node->function).get();

  if (symbol && symbol->is_function()) {
    if (!type) {
      type = global_get_type(symbol->type_id);
    }
    auto declaring_node = symbol->function.declaration;
    if (declaring_node && declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      func_decl = static_cast<ASTFunctionDeclaration *>(declaring_node);

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

        func_decl = resolve_generic_function_call(node, func_decl);
      }

      type = global_get_type(func_decl->resolved_type);
    }
  } else {
    node->function->accept(this);

    // Implicitly pass the 'dyn.instance' when calling the function pointers
    // that the dyn thingy sets up.
    if (node->function->get_node_type() == AST_NODE_DOT_EXPR) {
      auto base = ((ASTDotExpr *)node->function)->base;
      auto base_type = global_get_type(base->resolved_type);
      if (base_type->is_kind(TYPE_DYN)) {
        auto &args = node->arguments->arguments;
        auto dot = ast_alloc<ASTDotExpr>();
        dot->base = base;
        dot->member_name = "instance";
        dot->resolved_type = global_find_type_id(void_type(), {{{TYPE_EXT_POINTER_MUT}}});
        args.insert(args.begin(), dot);
      }
    }

    type = global_get_type(node->function->resolved_type);
  }

  if (!type) {
    throw_error("use of undeclared function", node->source_range);
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("unable to call a non-function, got {}", type->to_string()), node->source_range);
  }

  auto info = type->get_info()->as<FunctionTypeInfo>();

  // If we have the declaring node representing this function, type check it against the parameters in that definition.
  // else, use the type.
  if (func_decl) {
    if (node->function->get_node_type() == AST_NODE_DOT_EXPR) {
      if (!func_decl->params->has_self) {
        throw_error("Calling static methods with instance not allowed", node->source_range);
      }
      type_check_args_from_params(node->arguments, func_decl->params, ((ASTDotExpr *)node->function)->base);
    } else {
      type_check_args_from_params(node->arguments, func_decl->params, nullptr);
    }
  } else {
    type_check_args_from_info(node->arguments, info);
  }

  node->resolved_type = info->return_type;
}

void Typer::visit(ASTImport *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  node->scope->name = node->module_name;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  ctx.set_scope(old_scope);
  switch (node->tag) {
    case ASTImport::IMPORT_NORMAL:
      // do nothing
      break;
    case ASTImport::IMPORT_ALL: {
      for (const auto &symbol : node->scope->symbols) {
        ctx.scope->symbols.insert(symbol);
      }
    } break;
    case ASTImport::IMPORT_NAMED: {
      for (const auto &symbol : node->symbols) {
        if (node->scope->local_lookup(symbol)) {
          ctx.scope->symbols[symbol] = *node->scope->local_lookup(symbol);
        } else {
          throw_error(std::format("unable to import \"{}\".. not found in module \"{}\"", symbol.get_str(),
                                  node->module_name.get_str()),
                      node->source_range);
        }
      }
    } break;
  }
}

void Typer::visit(ASTArguments *node) {
  auto type = global_get_type(expected_type);
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
    auto old_ty = expected_type;
    expected_type = info->parameter_types[i];
    Defer _defer([&] { expected_type = old_ty; });
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

void Typer::visit(ASTType_Of *node) {
  static auto type_ptr = ctx.scope->find_type_id("Type", {{{TYPE_EXT_POINTER_CONST}}});
  node->target->accept(this);
  node->resolved_type = type_ptr;
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

  if (node->kind == ASTType::NORMAL) {
    auto &normal_ty = node->normal;
    normal_ty.base->accept(this);
    auto symbol = ctx.get_symbol(normal_ty.base).get();

    if (!symbol) {
      throw_error("use of undeclared type", node->source_range);
    } else if (!symbol->is_type()) {
      throw_error("cannot use a non-type symbol as a type", node->source_range);
    }

    auto declaring_node = symbol->type.declaration.get();

    if (declaring_node && !normal_ty.generic_arguments.empty()) {
      std::vector<int> generic_args;
      for (auto &arg : normal_ty.generic_arguments) {
        arg->accept(this);
        generic_args.push_back(arg->resolved_type);
      }

      switch (declaring_node->get_node_type()) {
        case AST_NODE_STRUCT_DECLARATION:
        case AST_NODE_FUNCTION_DECLARATION:
        case AST_NODE_INTERFACE_DECLARATION:
        case AST_NODE_TAGGED_UNION_DECLARATION:
          break;
        default:
          throw_error("Invalid target to generic args", node->source_range);
          break;
      }
      auto instantiation = visit_generic((ASTDeclaration *)declaring_node, generic_args, node->source_range);
      node->resolved_type = global_find_type_id(instantiation->resolved_type, extensions);
    } else {
      normal_ty.base->accept(this);
      auto base_ty = global_get_type(normal_ty.base->resolved_type);
      if (!base_ty) {
        if (normal_ty.base->resolved_type == Type::INVALID_TYPE_ID) {
          throw_error("use of undeclared type", node->source_range);
        } else if (normal_ty.base->resolved_type == Type::UNRESOLVED_GENERIC_TYPE_ID) {
          throw_error("use of unresolved generic type", node->source_range);
        }
      }
      node->resolved_type = global_find_type_id(base_ty->id, extensions);
    }

    if (node->normal.is_dyn) {
      auto type = global_get_type(node->resolved_type);
      auto extension = type->get_ext();
      auto ty = ctx.scope->find_or_create_dyn_type_of(type->base_id == -1 ? type->id : type->base_id,
                                                      node->source_range, this);
      if (extensions.has_extensions()) {
        node->resolved_type = global_find_type_id(ty, extensions);
      } else {
        node->resolved_type = ty;
      }
    }

  } else if (node->kind == ASTType::TUPLE) {
    std::vector<int> types;
    for (const auto &t : node->tuple_types) {
      t->accept(this);
      types.push_back(t->resolved_type);
    }
    node->resolved_type = global_find_type_id(types, extensions);
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

  auto old_ty = expected_type;
  Defer _defer([&] { expected_type = old_ty; });

  if (node->op.type == TType::Assign) {
    expected_type = left;
  } else if (node->left->get_node_type() == AST_NODE_SWITCH || node->right->get_node_type() == AST_NODE_SWITCH ||
             node->right->get_node_type() == AST_NODE_IF || node->left->get_node_type() == AST_NODE_IF) {
    throw_error("cannot use 'switch' or 'if' expressions in function arguments, they're only valid in `=`, `:=` and "
                "`return` statements",
                node->source_range);
  }

  node->right->accept(this);
  auto right = node->right->resolved_type;

  if (node->op.type == TType::Assign || node->op.is_comp_assign()) {
    if (node->left->get_node_type() == AST_NODE_IDENTIFIER) {
      auto name = ((ASTIdentifier *)node->left)->value;
      if (auto symbol = ctx.scope->lookup(name)) {
        if (symbol->mutability == CONST) {
          throw_error("cannot assign to a constant variable. consider adding 'mut' to the parameter or variable.",
                      node->source_range);
        }
      } else {
        throw_error("can't assign a non-existent variable (TODO verify this error is correct)", node->source_range);
      }
      // we assume this is mutable since we made it past that?
      auto symbol = ctx.scope->lookup(name);
      if (symbol->is_variable()) {
        symbol->variable.initial_value = node->right;
      } else {
        throw_error("Cannot assign to non-variable symbol", node->source_range);
      }
    }
  }

  auto left_ty = global_get_type(left);

  // Do checks for constness.
  if (node->op.type == TType::Assign) {
    if (node->left->get_node_type() == AST_NODE_UNARY_EXPR) {
      auto unary = (ASTUnaryExpr *)node->left;
      auto unary_operand_ty = global_get_type(unary->operand->resolved_type);
      if (unary->op.type == TType::Mul && unary_operand_ty->get_ext().is_const_pointer()) {
        throw_error("cannot dereference into a const pointer!", node->source_range);
      }

      auto left_ty = global_get_type(unary->operand->resolved_type);
      auto symbol = ctx.get_symbol(unary->operand);
      if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->get_ext().is_mut_pointer()) {
        throw_error("cannot assign into a const variable!", node->source_range);
      }

    } else if (node->left->get_node_type() == AST_NODE_SUBSCRIPT) {
      auto subscript = (ASTSubscript *)node->left;

      auto subscript_left_ty = global_get_type(subscript->left->resolved_type);
      if (subscript_left_ty->get_ext().is_const_pointer()) {
        throw_error("cannot subscript-assign into a const pointer!", node->source_range);
      }

      auto symbol = ctx.get_symbol(subscript->left);
      if (symbol.is_not_null() && symbol.get()->is_const() && !subscript_left_ty->get_ext().is_mut_pointer()) {
        throw_error("cannot subscript-assign into a const variable!", node->source_range);
      }

    } else if (node->left->get_node_type() == AST_NODE_DOT_EXPR) {
      auto dot = (ASTDotExpr *)node->left;

      auto symbol = ctx.get_symbol(dot->base);
      auto left_ty = global_get_type(dot->base->resolved_type);

      if (left_ty->get_ext().is_const_pointer()) {
        throw_error("cannot dot-assign into a const pointer!", dot->base->source_range);
      }

      if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->get_ext().is_mut_pointer()) {
        throw_error("cannot dot-assign into a const variable!", node->source_range);
      }

      /*
        We have to check all the way down the left of the dot expression.
      */
      while (dot->base->get_node_type() == AST_NODE_DOT_EXPR) {
        dot = (ASTDotExpr *)dot->base;
        auto left_ty = global_get_type(dot->base->resolved_type);
        if (left_ty->get_ext().is_const_pointer()) {
          throw_error("cannot dot-assign into a const pointer!", dot->base->source_range);
        }
        auto symbol = ctx.get_symbol(dot->base);
        if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->get_ext().is_mut_pointer()) {
          throw_error("cannot dot-assign into a const variable!", dot->base->source_range);
        }
      }
    }
  }

  auto operator_overload_ty = find_operator_overload(CONST, left_ty, node->op.type, OPERATION_BINARY);
  if (operator_overload_ty != Type::INVALID_TYPE_ID) {
    node->is_operator_overload = true;
    auto ty = global_get_type(operator_overload_ty);
    node->resolved_type = ty->get_info()->as<FunctionTypeInfo>()->return_type;
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

  auto overload = find_operator_overload(CONST, type, node->op.type, OPERATION_UNARY);
  if (overload != Type::INVALID_TYPE_ID) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->get_info()->as<FunctionTypeInfo>()->return_type;
    auto name = get_operator_overload_name(node->op.type, OPERATION_UNARY);

    if (name == "deref") {
      auto type = global_get_type(node->resolved_type);
      if (!type->get_ext().is_pointer()) {
        throw_error("'deref' operator overload must return a pointer, the compiler will auto dereference this when "
                    "it's used. it allows us to assign via this function",
                    node->source_range);
      }
      node->resolved_type = type->get_element_type();
    }

    return;
  }

  // Address-Of.
  if (node->op.type == TType::And) {
    auto symbol = ctx.get_symbol(node->operand);
    auto op_ty = global_get_type(operand_ty);

    if (symbol) {
      auto sym = symbol.get();
      if (sym->is_const() && node->mutability == MUT && !op_ty->get_ext().is_mut_pointer() &&
          !op_ty->is_kind(TYPE_FUNCTION)) {
        throw_error("cannot take a mutable pointer to a non-mutable variable", node->source_range);
      }
    }

    node->resolved_type = op_ty->take_pointer_to(node->mutability);
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
      auto value = node->value.get_str();
      if (value.starts_with("0x")) {
        if (value.length() > 18) {
          throw_error("Hexidecimal literal is too large to be represented by a 64 bit integer.", node->source_range);
        }
      } else if (value.starts_with("0b")) {
        if (value.length() > 64 + 2) {
          throw_error("Binary literal is too large to be represented by a 64 bit integer", node->source_range);
        }
      } else {
        // errno = 0;
        // auto parsed_unsigned = strtoull(value.c_str(), nullptr, 10);
        // if (errno == ERANGE && parsed_unsigned == ULLONG_MAX) {
        //   throw_error("Unsigned integer literal is too large to be represented by a 64 bit integer.",
        //               node->source_range);
        // }

        // errno = 0;
        // auto parsed_signed = strtoll(value.c_str(), nullptr, 10);
        // if (errno == ERANGE && (parsed_signed == LLONG_MAX || parsed_signed == LLONG_MIN)) {
        //   throw_error("Signed integer literal is too large to be represented by a 64 bit integer.",
        //   node->source_range);
        // }
      }

      if (expected_type != Type::INVALID_TYPE_ID && type_is_numerical(global_get_type(expected_type))) {
        node->resolved_type = expected_type;
        return;
      }
      node->resolved_type = s32_type();
      return;
    }
    case ASTLiteral::Float:
      if (expected_type == f64_type()) {
        node->resolved_type = f64_type();
      } else {
        node->resolved_type = f32_type();
      }
      return;
    case ASTLiteral::String: {
      if (node->is_c_string) {
        static int type = global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER_CONST}}});
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
      if (expected_type != Type::INVALID_TYPE_ID) {
        node->resolved_type = expected_type;
        return;
      }
      node->resolved_type = ctx.scope->find_type_id("void", {{{TYPE_EXT_POINTER_CONST}}});
      return;
    case ASTLiteral::Char:
      if (expected_type == u8_type()) {
        node->resolved_type = u8_type();
      } else {
        node->resolved_type = u32_type();
      }
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
  auto scope_nullable = ctx.get_scope(node->base);
  auto scope = scope_nullable.get();

  if (auto member = scope->local_lookup(node->member_name)) {
    node->resolved_type = member->type_id;
  } else if (auto type = scope->find_type_id(node->member_name, {})) {
    if (type == Type::INVALID_TYPE_ID) {
      throw_error(std::format("Member \"{}\" not found in base", node->member_name), node->source_range);
    }
    node->resolved_type = type;
  } else {
    throw_error(std::format("Member \"{}\" not found in base", node->member_name), node->source_range);
  }
}

void Typer::visit(ASTSubscript *node) {
  node->left->accept(this);
  node->subscript->accept(this);
  auto left_ty = global_get_type(node->left->resolved_type);
  auto subscript_ty = global_get_type(node->subscript->resolved_type);

  auto symbol = ctx.get_symbol(node->left);

  Mutability mutability = symbol ? symbol.get()->mutability : CONST;
  auto overload = find_operator_overload(mutability, left_ty, TType::LBrace, OPERATION_SUBSCRIPT);

  if (overload != Type::INVALID_TYPE_ID) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->get_info()->as<FunctionTypeInfo>()->return_type;

    auto type = global_get_type(node->resolved_type);
    if (!type->get_ext().is_pointer()) {
      throw_error("subscript methods MUST return a pointer!\nthis is because we have to be able to assign though it, "
                  "so `*$13_subscript$1(obj, index) = 10` has to be possible\n"
                  "example: subscript :: fn(self*, index: u32) -> s32* { return &self.data[index]; }\n"
                  "obviously this is somewhat limiting. we have yet to find a better solution to this.",
                  node->source_range);
    }

    node->resolved_type = type->get_element_type();

    return;
  }

  auto ext = left_ty->get_ext();
  if (!ext.is_fixed_sized_array() && !ext.is_pointer()) {
    throw_error(
        std::format(
            "cannot index into non-array, non-pointer type that doesn't implement the `Subscript` interface. {}",
            left_ty->to_string()),
        node->source_range);
  }

  node->resolved_type = left_ty->get_element_type();
}

void Typer::visit(ASTInitializerList *node) {
  Type *target_type;
  if (node->target_type.is_null()) {
    target_type = global_get_type(expected_type);
  } else {
    node->target_type.get()->accept(this);
    target_type = global_get_type(node->target_type.get()->resolved_type);

    if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
      auto expected = global_get_type(expected_type);
      if (expected && expected->get_ext().is_fixed_sized_array()) {
        auto elem = expected->get_element_type();
        auto rule = type_conversion_rule(target_type, global_get_type(elem));
        if (rule == CONVERT_PROHIBITED) {
          throw_error("invalid initializer list element type", node->source_range);
        }
        target_type = expected;
      } else {
        target_type = global_get_type(find_generic_type_of("InitList", {target_type->id}, node->source_range));
      }
    }
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

  /*
    for collection style initializer lists.
  */
  int target_element_type = Type::INVALID_TYPE_ID;
  if (target_type->get_base().get_str().starts_with("InitList$")) {
    target_element_type = target_type->generic_args[0];
  } else if (target_type->get_ext().is_fixed_sized_array()) {
    target_element_type = target_type->get_element_type();
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
        auto old = expected_type;
        Defer _([&] { expected_type = old; });
        auto symbol = scope->local_lookup(id);
        if (!symbol)
          throw_error(std::format("Invalid named initializer list: couldn't find {}", id), node->source_range);

        if (symbol->is_function()) {
          throw_error(std::format("Cannot initialize a function :: ({}) with an initializer list.", id),
                      value->source_range);
        }
        expected_type = symbol->type_id;

        value->accept(this);

        assert_types_can_cast_or_equal(value, symbol->type_id, value->source_range,
                                       "Unable to cast type to target field for named initializer list");
      }
    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      auto &values = node->values;

      if (values.empty()) {
        node->resolved_type = target_type->id;
        return;
      }

      for (int i = 0; i < values.size(); ++i) {
        {
          auto old = expected_type;
          Defer _([&] { expected_type = old; });
          expected_type = target_element_type;
          values[i]->accept(this);
        }
        assert_types_can_cast_or_equal(
            values[i], target_element_type, values[i]->source_range,
            "Found inconsistent types in a collection-style initializer list. These types must be homogenous");
      }
      node->resolved_type = target_type->id;
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

  node->resolved_type = find_generic_type_of("RangeBase", {left}, node->source_range);

  if (node->resolved_type == Type::INVALID_TYPE_ID) {
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
    auto operator_overload = find_operator_overload(CONST, type, TType::EQ, OPERATION_BINARY);
    if (operator_overload == Type::INVALID_TYPE_ID) {
      throw_error(
          std::format("Can't use a 'switch' statement/expression on a non-scalar, non-enum type that doesn't implement "
                      "Eq (== operator on #self)\ngot type '{}'",
                      type->to_string()),
          node->target->source_range);
    }
  }

  auto old_expected_type = expected_type;
  if (!node->is_statement) {
    expected_type = Type::INVALID_TYPE_ID;
  }
  Defer _([&] { expected_type = old_expected_type; });

  int return_type = void_type();
  int flags = BLOCK_FLAGS_FALL_THROUGH;

  for (const auto &_case : node->cases) {
    _case.expression->accept(this);
    auto expr_type = _case.expression->resolved_type;
    _case.block->accept(this);
    auto &block_cf = _case.block->control_flow;
    flags |= block_cf.flags;

    if (HAS_FLAG(block_cf.flags, BLOCK_FLAGS_RETURN)) {
      return_type = block_cf.type;
    }

    if (type_is_numerical(type)) {
      continue;
    } else {
      assert_types_can_cast_or_equal(_case.expression, type_id, node->source_range, "Invalid switch case.");
    }
  }

  if (node->default_case) {
    auto _case = node->default_case.get();
    _case->accept(this);
  }

  node->resolved_type = node->return_type = return_type;
  if (node->is_statement) {
    node->control_flow = ControlFlow{flags, return_type};
  } else {
    if (HAS_FLAG(flags, BLOCK_FLAGS_BREAK)) {
      throw_warning(WarningSwitchBreak, "You do not need to break from switch cases.", node->source_range);
    } else if (HAS_FLAG(flags, BLOCK_FLAGS_CONTINUE)) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->source_range);
    }
  }
}

void Typer::visit(ASTTuple *node) {
  std::vector<int> types;
  auto declaring_tuple = global_get_type(expected_type);

  int type_index = 0;
  for (const auto &v : node->values) {
    auto old = expected_type;
    Defer _([&] { expected_type = old; });

    bool declaring_type_set = false;
    if (declaring_tuple && declaring_tuple->is_kind(TYPE_TUPLE)) {
      auto info = declaring_tuple->get_info()->as<TupleTypeInfo>();
      if (info->types.size() < type_index) {
        throw_error(std::format("too many expressions provided to tuple\ntuple type {}", declaring_tuple->to_string()),
                    v->source_range);
      }
      expected_type = info->types[type_index];
      declaring_type_set = true;
    }

    v->accept(this);

    if (declaring_type_set) {
      assert_types_can_cast_or_equal(v, expected_type, v->source_range,
                                     "tuple value was incapable of casting to expected tuple element type");
      v->resolved_type = expected_type;
    }

    types.push_back(v->resolved_type);
    type_index++;
  }
  TypeExtensions extensions;
  node->resolved_type = global_find_type_id(types, extensions);
}

void Typer::visit(ASTAlias *node) {
  node->source_node->accept(this);

  auto symbol = ctx.get_symbol(node->source_node);

  if (ctx.scope->symbols.contains(node->name)) {
    throw_error("redefinition in alias", node->source_range);
  }

  if (symbol && node->source_node->get_node_type() != AST_NODE_TYPE) {
    ctx.scope->symbols[node->name] = *symbol.get();
  } else {
    auto type = global_get_type(node->source_node->resolved_type);
    if (type == nullptr) {
      throw_error("cannot alias a non-existent type or symbol", node->source_range);
    }
    ctx.scope->create_type_alias(node->name, type->id, type->kind, node);
  }
  return;
}

void Typer::visit(ASTTupleDeconstruction *node) {
  node->right->accept(this);
  node->resolved_type = node->right->resolved_type;
  auto type = global_get_type(node->right->resolved_type);

  if (type->get_ext().has_extensions())
    throw_error("Cannot destructure pointer or array type.", node->source_range);

  auto scope = type->get_info()->scope;
  int i = 0;
  for (const auto name : scope->ordered_symbols) {
    auto symbol = scope->local_lookup(name);

    if (symbol->is_function() || symbol->is_type())
      continue;

    if (i > node->elements.size())
      break;

    auto destructure = node->elements[i];
    auto type = symbol->type_id;

    if (destructure.semantic == VALUE_SEMANTIC_POINTER) {
      type = global_get_type(symbol->type_id)->take_pointer_to(MUT);
    }
    ctx.scope->insert_variable(destructure.identifier->value, type, symbol->variable.initial_value.get(),
                               destructure.mutability);
    ctx.scope->local_lookup(destructure.identifier->value)->flags |= SYMBOL_IS_LOCAL;
    ++i;
  }
};

void Typer::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    auto symbol_nullable = ctx.get_symbol(node->target);

    if (symbol_nullable.is_null() || !symbol_nullable.get()->is_type()) {
      throw_error("generic `impl![...]` can only be used on types.", node->source_range);
    }

    auto declaring_node = symbol_nullable.get()->type.declaration.get();
    auto node_as_decl = static_cast<ASTDeclaration *>(declaring_node);
    node_as_decl->impls.push_back(node);
    for (auto instantiations : node_as_decl->generic_instantiations) {
      visit_generic(node, instantiations.arguments, instantiations.declaration->source_range);
    }
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
    std::vector<int> args;
    for (auto &param : node->generic_parameters) {
      args.push_back(Type::UNRESOLVED_GENERIC_TYPE_ID);
    }
    ctx.scope->create_interface_type(node->name, node->scope, args, node);
  } else {
    visit_interface_declaration(node, false);
    ctx.scope->create_type_alias(node->name, node->resolved_type, TYPE_INTERFACE, node);
  }
  return;
}

void Typer::visit(ASTSize_Of *node) {
  node->target_type->accept(this);
  node->resolved_type = u64_type();
}

void Typer::visit(ASTModule *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  node->scope->name = node->module_name;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  ctx.set_scope(old_scope);
  ctx.scope->create_module(node->module_name, node);
}

void Typer::visit(ASTDyn_Of *node) {
  if (!node->interface_type) {
    auto type = global_get_type(expected_type);
    if (type && type->is_kind(TYPE_DYN)) {
      node->interface_type = ast_alloc<ASTType>();
      node->interface_type->resolved_type = type->get_info()->as<DynTypeInfo>()->interface_type;
    } else {
      throw_error("if a dyn type isn't already expected (via an argument, or an explicitly typed variable declaration, "
                  "etc), you must pass the _interface_ type as the second parameter to 'dynof'\nSo, if you wanted a "
                  "'dyn Format', youd use 'dynof(my_instance, Format)'",
                  node->source_range);
    }
  } else {
    node->interface_type->accept(this);
  }

  node->object->accept(this);

  auto object_type = global_get_type(node->object->resolved_type);

  if (!object_type->get_ext().is_mut_pointer()) {
    throw_error("'dynof' requires the second argument, the instance to create a dyn dispatch object for, must be a "
                "mutable pointer. eventually we'll have const dyn's",
                node->source_range);
  }

  auto type = global_get_type(node->interface_type->resolved_type);
  if (!type->is_kind(TYPE_INTERFACE)) {
    throw_error("cannot use 'dynof(Type, $expr)' on types that aren't interfaces.", node->source_range);
  }

  auto element_type = global_get_type(object_type->get_element_type());
  if (!element_type->implements(node->interface_type->resolved_type)) {
    throw_error(
        std::format("cannot create 'dyn {}' from object of type '{}' because it does not implement the interface.",
                    type->to_string(), element_type->to_string()),
        node->source_range);
  }

  auto ty =
      ctx.scope->find_or_create_dyn_type_of(type->base_id == -1 ? type->id : type->base_id, node->source_range, this);
  node->resolved_type = ty;
}

void Typer::visit(ASTPatternMatch *node) {}

int Scope::find_or_create_dyn_type_of(int interface_type, SourceRange range, Typer *typer) {
  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->is_kind(TYPE_DYN) &&
        type_table[i]->get_info()->as<DynTypeInfo>()->interface_type == interface_type) {
      return type_table[i]->id;
    }
  }
  auto iface_type = global_get_type(interface_type);
  auto interface_name = "dyn$" + iface_type->to_string();
  auto dyn_info = new (type_info_alloc<DynTypeInfo>()) DynTypeInfo();
  dyn_info->interface_type = interface_type;

  // TODO: * determine whether 'dyn' should actually be in the type name itself. *
  auto ty = global_get_type(global_create_type(TYPE_DYN, interface_name, dyn_info, {}, -1));

  dyn_info->scope->insert_variable("instance", global_find_type_id(void_type(), {{{TYPE_EXT_POINTER_MUT}}}), nullptr,
                                   MUT);

  ty->get_info()->as<DynTypeInfo>()->interface_type = interface_type;

  auto interface_info = iface_type->get_info()->as<InterfaceTypeInfo>();

  for (auto [name, sym] : interface_info->scope->symbols) {
    if (sym.is_function() && !sym.is_generic_function()) {
      auto declaration = sym.function.declaration;

      std::vector<int> parameters;

      bool has_self = false;
      for (auto param : declaration->params->params) {
        if (param->tag == ASTParamDecl::Self || param->tag == ASTParamDecl::Себя) {
          if (param->self.is_pointer) {
            parameters.push_back(global_find_type_id(void_type(), {{{TYPE_EXT_POINTER_CONST}}}));
          } else {
            throw_error("cannot use 'dyn' on interfaces that take 'self' by value because that would be a zero-sized "
                        "parameter, as we don't know the type of the 'self' at compile time definitively.",
                        range);
          }
          has_self = true;
        } else {
          if (!has_self) {
            throw_error(
                "'dyn' can only be used with interfaces that do not have any associated functions, e.g functions "
                "that\n"
                "do not take a '*mut self', nor a '*const self' (in the case of 'dyn' self must always be a pointer)",
                range);
          }

          param->accept(typer);
          // There's an exception here for interface typed parameters.
          auto parameter_type = global_get_type(param->resolved_type);
          if (parameter_type->is_kind(TYPE_INTERFACE)) {
            throw_error("you cannot take a 'dyn' of an interface that uses other interfaces as parameter constraints.\n"
                        "the parameters all must be concrete types, with the exception of '*const/mut self' params.",
                        range);
          }

          parameters.push_back(param->resolved_type);
        }
      }

      if (declaration->return_type->kind == ASTType::SELF) {
        throw_error(
            "just as we can't take 'self' by value in a 'dyn' interface, you can't return '#self', even by pointer, "
            "because we would have to return it as a type erased *const void. return the concrete type.",
            range);
      }

      declaration->return_type->accept(typer);
      auto return_type = declaration->return_type->resolved_type;

      FunctionTypeInfo type_info;
      memcpy(type_info.parameter_types, parameters.data(), parameters.size() * sizeof(int));
      type_info.params_len = parameters.size();
      type_info.return_type = return_type;

      // ! TODO: @Cooper-Pilot Why do i have to call back into the dependency emitter here, even though the dependency
      // emitter ! Tries to resolve each of the parameter and return types of every method in the freaking dang
      // Interface??? ! This is a last ditch effort hack so I can just continue writing the rest of the stuff like
      // calling dyn's.
      auto function_type = global_get_type(global_find_function_type_id(type_info, {{{TYPE_EXT_POINTER_MUT}}}));
      dyn_info->methods.push_back({name.get_str(), function_type});
      dyn_info->scope->insert_variable(name.get_str(), function_type->id, nullptr, MUT, nullptr);
    }
  }

  auto sym = Symbol::create_type(ty->id, interface_name, TYPE_DYN, nullptr);
  sym.scope = this; // TODO: we have to fit this in modules or some stuff.

  symbols.insert_or_assign(interface_name, sym);
  return ty->id;
}

