#include <any>
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
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"

static inline int int_from_any(const std::any &any) { return std::any_cast<int>(any); }

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

void Typer::find_function_overload(ASTCall *&node, Symbol *&symbol, std::vector<int> &arg_tys, Type *&type) {
  if ((symbol->flags & SYMBOL_HAS_OVERLOADS) != 0) {
    bool found_exact_match = false;
    int exact_match_idx = -1;

    bool found_implicit_match = false;
    int implicit_match_idx = -1;

// Define the helper macro
#define NON_VARARGS_NO_DEFAULT_PARAMS(info) (!info->is_varargs && info->default_params == 0)

    for (const auto &[i, overload] : symbol->function_overload_types | std::ranges::views::enumerate) {
      auto ovrld_ty = global_get_type(overload);
      auto info = (ovrld_ty->get_info()->as<FunctionTypeInfo>());

      bool match = true;
      int required_params = info->params_len - info->default_params;
      if (arg_tys.size() < required_params || (!info->is_varargs && arg_tys.size() > info->params_len)) {
        match = false;
      } else {
        for (int j = 0; j < arg_tys.size(); ++j) {
          if (j >= info->params_len) {
            if (!info->is_varargs) {
              match = false;
              break;
            }
          } else {
            auto conversion_rule = type_conversion_rule(global_get_type(arg_tys[j]),
                                                        global_get_type(info->parameter_types[j]), node->source_range);
            if (conversion_rule == CONVERT_EXPLICIT && NON_VARARGS_NO_DEFAULT_PARAMS(info)) {
              match = false;
              break;
            }
            if (conversion_rule == CONVERT_IMPLICIT && NON_VARARGS_NO_DEFAULT_PARAMS(info)) {
              found_implicit_match = true;
              implicit_match_idx = i;
            } else if (conversion_rule != CONVERT_NONE_NEEDED) {
              match = false;
              break;
            }
          }
        }
      }

      if (match) {
        found_exact_match = true;
        exact_match_idx = i;
        break;
      }
    }

    if (!found_exact_match && !found_implicit_match) {
      std::vector<std::string> names;
      for (auto n : arg_tys) {
        names.push_back(global_get_type(n)->to_string());
      }
      throw_error(std::format("No function overload for provided argument "
                              "signature found.. got : {}",
                              names),
                  node->source_range);
    }

    if (found_exact_match) {
      type = global_get_type(symbol->function_overload_types[exact_match_idx]);
      assert(type != nullptr);
    } else {
      type = global_get_type(symbol->function_overload_types[implicit_match_idx]);
      assert(type != nullptr);
    }

#undef NON_VARARGS_NO_DEFAULT_PARAMS
  }
}

// CLEANUP(Josh) 10/4/2024, 1:39:21 PM Wow this is an eyesore. There has to be a
// way to clean this dang thing up either returns the correct type that this
// init list will get casted to, or throws an error. node->types_are_homogenous
// is pretty loose: it states that these types are either the same, or
// implicitly convertible to each other. that may not be enough to satisfy the
// C++ type system when it's strangely strict in some places more than others
int assert_type_can_be_assigned_from_init_list(ASTInitializerList *node, int declaring_type) {
  auto type = global_get_type(declaring_type);
  //! BUG This fails to accurately type check sub initializers. Also, we may
  //! want to implement sub initializers for struct types
  // and check those too
  if (node->types_are_homogenous && (type->get_ext().is_array() || type->get_ext().is_fixed_sized_array())) {
    for (const auto [i, expr] : node->expressions | std::ranges::views::enumerate) {
      if (expr->get_node_type() == AST_NODE_INITIALIZER_LIST) {
        assert_type_can_be_assigned_from_init_list(static_cast<ASTInitializerList *>(expr), node->types[i]);
      }
    }
    return declaring_type;
  }
  if (type->get_ext().has_extensions()) {
    throw_error("Unable to construct type from initializer list", node->source_range);
  }
  if (type->is_kind(TYPE_SCALAR)) {
    // this is just a plain scalar type, such as an int.
  } else if (type->is_kind(TYPE_STRUCT)) {
    auto info = (type->get_info()->as<StructTypeInfo>());

    for (const auto &[name, symbol] : info->scope->symbols) {
      if (name == "this")
        continue;

      // constructors use anonymous symbol names.
      if ((symbol.flags & SYMBOL_IS_FUNCTION) == 0 || !name.get_str().contains("__anon_D"))
        continue;
      auto type = global_get_type(symbol.type_id);

      if (!type)
        continue;

      auto info = (type->get_info()->as<FunctionTypeInfo>());
      auto &params = info->parameter_types;

      if (info->params_len != node->expressions.size()) {
        continue;
      }

      for (int i = 0; i < info->params_len; ++i) {
        auto type = global_get_type(params[i]);
        auto rule = type_conversion_rule(type, global_get_type(node->types[i]), node->source_range);
        if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
          continue;
        }
      }

      return declaring_type;
    }

    // !HACK i used node->expressions.size() to bypass a bug with the types of
    // initlist exceeding the number of expressions.
    // * REMOVE ME *
    if (info->scope->fields_count() < node->expressions.size()) {
      throw_error("excess elements provided in initializer list.", node->source_range);
    }
    // search for fields within the range of the types provided.
    int i = 0;
    for (const auto &name : info->scope->ordered_symbols) {
      if (name == "this")
        continue;
      auto sym = info->scope->symbols[name];
      if (i >= node->types.size()) {
        break;
      }
      if (!sym.is_function() && !global_get_type(sym.type_id)->is_kind(TYPE_FUNCTION)) {
        assert_types_can_cast_or_equal(node->types[i], sym.type_id, node->source_range, "expected: {}, got: {}",
                                       "Invalid types in initializer list for struct");
      }
      i++;
    }
  } else if (type->is_kind(TYPE_UNION)) {
    auto info = (type->get_info()->as<UnionTypeInfo>());
    if (node->types.size() > 1) {
      throw_error("You can only initialize one field of a union with an "
                  "initializer list",
                  node->source_range);
    }
    // search for the first field member and type check against it.
    for (const auto &[name, sym] : info->scope->symbols) {
      if (!sym.is_function() && !global_get_type(sym.type_id)->is_kind(TYPE_FUNCTION)) {
        assert_types_can_cast_or_equal(node->types[0], sym.type_id, node->source_range, "{}, {}",
                                       "Invalid types in initializer list for union");
        break;
      }
    }
  } else {
    throw_error("Unable to construct type from initializer list", node->source_range);
  }
  return declaring_type;
}

Nullable<Symbol> Typer::get_symbol(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_IDENTIFIER:
      return ctx.scope->lookup(static_cast<ASTIdentifier *>(node)->value);
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      auto type = global_get_type(int_from_any(dotnode->base->accept(this)));

      // TODO:
      // * We can't do this, since there's no symbol that represents these
      // * compiler intrinsics. So instead, we just roll with it as we wouldve -- this
      // * Is only for overloading anyway.
      if (type->get_ext().is_array())
        return nullptr;

      if (type->get_ext().is_map())
        return nullptr;

      if (!type->is_kind(TYPE_STRUCT) && !type->is_kind(TYPE_UNION))
        throw_error("cannot use . on a non-struct, non-union type", node->source_range);

      auto scope = type->is_kind(TYPE_STRUCT) ? type->get_info()->as<StructTypeInfo>()->scope
                                              : type->get_info()->as<UnionTypeInfo>()->scope;
      return scope->local_lookup(dotnode->member_name);
    } break;
    case AST_NODE_SCOPE_RESOLUTION: {
      auto srnode = static_cast<ASTScopeResolution *>(node);
      auto type = global_get_type(int_from_any(srnode->base->accept(this)));
      if (!type->is_kind(TYPE_STRUCT) && !type->is_kind(TYPE_UNION))
        throw_error("cannot use :: on a non-struct, non-union type", node->source_range);

      auto scope = type->is_kind(TYPE_STRUCT) ? type->get_info()->as<StructTypeInfo>()->scope
                                              : type->get_info()->as<UnionTypeInfo>()->scope;
      return scope->local_lookup(srnode->member_name);
    } break;
    case AST_NODE_SUBSCRIPT: {
      return nullptr;
    }
    default:
      throw_error("Get symbol cannot be used on this node type", node->source_range);
  }
  return nullptr;
}

std::any Typer::visit(ASTProgram *node) {
  for (auto &statement : node->statements)
    statement->accept(this);
  return {};
}

int Typer::visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                    std::vector<int> generic_args) {
  auto last_decl = current_struct_decl;
  current_struct_decl = node;
  Defer _([&] { current_struct_decl = last_decl; });

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
    type = global_get_type(global_create_struct_type(node->name, node->scope));
  }

  ctx.scope->insert("this", type->take_pointer_to());

  for (auto subunion: node->unions) {
    for (const auto &field : subunion->fields) {
      field->accept(this);
      node->scope->insert(field->name, field->type->resolved_type);
    }
  }
  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method : node->methods) {
    method->accept(this);
  }
  

  ctx.set_scope(old_scope);
  return type->id;
}

int Typer::visit_union_declaration(ASTUnionDeclaration *node, bool generic_instantiation,
                                   std::vector<int> generic_args) {
  if (node->is_fwd_decl) {
    return {};
  }

  auto last_decl = current_union_decl;

  current_union_decl = node;
  Defer _([&] { current_union_decl = last_decl; });

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

  ctx.scope->insert("this", type->take_pointer_to());

  for (const auto &_struct : node->structs) {
    for (const auto &field : _struct->fields) {
      field->accept(this);
      node->scope->insert(field->name, field->type->resolved_type);
    }
  }

  for (const auto &field : node->fields) {
    field->accept(this);
  }
  for (const auto &method : node->methods) {
    method->accept(this);
  }

  ctx.set_scope(old_scope);
  return type->id;
}

int Typer::visit_function_declaration(ASTFunctionDeclaration *node, bool generic_instantiation,
                                      std::vector<int> generic_args) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  auto last_decl = current_func_decl;
  current_func_decl = node;
  Defer _([&] {
    current_func_decl = last_decl;
    ctx.set_scope(old_scope);
  });

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
  info.return_type = node->return_type->resolved_type;

  if (info.return_type == -1) {
    throw_error("Use of undeclared type", node->return_type->source_range);
  }

  info.params_len = 0;
  info.default_params = 0;
  info.meta_type = node->meta_type;

  auto name = node->name;

  info.is_varargs = (node->flags & FUNCTION_IS_VARARGS) != 0;

  auto params = node->params->params;

  for (const auto &param : params) {
    if (param->default_value.is_not_null())
      info.default_params++;
    ctx.scope->insert(param->name, param->type->resolved_type);
    info.parameter_types[info.params_len] = param->type->resolved_type;
    info.params_len++;
  }

  auto type_id = global_find_function_type_id(info, {});

  // TODO: we need to support fwd decls of overloaded functions
  if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
    ctx.scope->parent->insert(node->name, type_id, SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION);
    return {};
  }

  auto sym = ctx.scope->parent->lookup(node->name);

  if (sym && (sym->flags & SYMBOL_IS_FORWARD_DECLARED) != 0) {
    sym->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
  }

  if (!generic_instantiation) {
    // CLEANUP(Josh) 10/7/2024, 8:07:00 AM
    // This is ugly. It's for function overloading
    if (sym && ((node->flags & FUNCTION_IS_CTOR) == 0) && (node->flags & FUNCTION_IS_DTOR) == 0) {
      if (sym->function_overload_types.size() >= 1)
        sym->flags |= SYMBOL_HAS_OVERLOADS;
      for (const auto overload_type_id : sym->function_overload_types) {
        auto type = global_get_type(overload_type_id);
        auto this_type = global_get_type(type_id);
        if (type->equals(this_type->base_id, this_type->get_ext()) &&
            type->type_info_equals(this_type->get_info(), this_type->kind))
          throw_error(std::format("re-definition of function '{}'", node->name.get_str()), node->source_range);
      }
      sym->function_overload_types.push_back(type_id);
      sym->type_id = type_id;
    } else {
      // always insert the first function declarations as the 0th overloaded type,
      // because we can tell when a fucntion has been overloaded when this array's
      // size is > 1
      ctx.scope->parent->insert(node->name, type_id, SYMBOL_IS_FUNCTION);
      auto sym = ctx.scope->parent->lookup(node->name);
      sym->function_overload_types.push_back(type_id);
      sym->declaring_node = node;
    }
  }
  if (info.meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN)
    return {};

  auto old_ty = declaring_or_assigning_type;
  auto _defer = Defer([&] { declaring_or_assigning_type = old_ty; });
  declaring_or_assigning_type = info.return_type;

  auto control_flow = std::any_cast<ControlFlow>(node->block.get()->accept(this));
  if (control_flow.type == -1)
    control_flow.type = void_type();
  const auto is_ctor = (node->flags & FUNCTION_IS_CTOR) != 0, is_dtor = (node->flags & FUNCTION_IS_DTOR) != 0;
  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0)
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0)
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 && info.return_type != void_type() && !(is_ctor || is_dtor))
    throw_error("Not all code paths return a value.", node->source_range);
  assert_types_can_cast_or_equal(control_flow.type, info.return_type, node->source_range,
                                 "invalid return type.. expected '{}', got '{}'",
                                 std::format("function: '{}'", node->name.get_str()));
  return type_id;
}

std::any Typer::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1);
    auto sym = ctx.scope->lookup(node->name);
    sym->declaring_node = node;
    return {};
  }
  return visit_struct_declaration(node, false);
}

std::any Typer::visit(ASTUnionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1);
    auto sym = ctx.scope->lookup(node->name);
    sym->declaring_node = node;
    return {};
  }
  return visit_union_declaration(node, false);
}

std::any Typer::visit(ASTEnumDeclaration *node) {
  auto elem_type = -1;

  std::vector<InternedString> fields;
  for (const auto &[key, value] : node->key_values) {
    fields.push_back(key);
    if (value.is_null())
      continue;

    if (node->is_flags) {
      throw_error("You shouldn't use a #flags enum to generate auto "
                  "flags, and also use non-default values.",
                  node->source_range);
    }

    auto expr = value.get();
    auto id = int_from_any(value.get()->accept(this));
    auto type = global_get_type(id);

    if (elem_type == -1) {
      elem_type = id;
    }

    assert_types_can_cast_or_equal(id, elem_type, node->source_range, "expected: {}, got : {}",
                                   "Inconsistent types in enum declaration.");
  }

  if (elem_type == void_type())
    throw_error("Invalid enum declaration.. got null or no type.", node->source_range);

  if (elem_type == -1) {
    elem_type = s32_type();
  }

  ctx.scope->create_enum_type(node->name, fields, node->is_flags);
  auto enum_type = global_get_type(ctx.scope->find_type_id(node->name, {}));
  auto info = (enum_type->get_info()->as<EnumTypeInfo>());
  node->element_type = elem_type;
  info->element_type = elem_type;

  return {};
}

// For generic types.
// TODO: this has a lot of duplicated code and can be cleaned up for sure.
int Typer::get_function_type(ASTFunctionDeclaration *node) {
  node->return_type->accept(this);
  node->params->accept(this);

  FunctionTypeInfo info;
  info.return_type = node->return_type->resolved_type;

  if (info.return_type == -1) {
    throw_error("Use of undeclared type", node->return_type->source_range);
  }

  info.params_len = 0;
  info.default_params = 0;
  info.meta_type = node->meta_type;
  info.is_varargs = (node->flags & FUNCTION_IS_VARARGS) != 0;
  auto params = node->params->params;

  for (const auto &param : params) {
    if (param->default_value.is_not_null())
      info.default_params++;
    if (node->block.is_not_null())
      node->block.get()->scope->insert(param->name, param->type->resolved_type);
    info.parameter_types[info.params_len] = param->type->resolved_type;
    info.params_len++;
  }

  return global_find_function_type_id(info, {});
}

std::any Typer::visit(ASTFunctionDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    ctx.scope->insert(node->name, -1, SYMBOL_IS_FUNCTION);
    auto sym = ctx.scope->lookup(node->name);
    sym->declaring_node = node;
    return {};
  }
  return visit_function_declaration(node, false);
}

std::any Typer::visit(ASTDeclaration *node) {
  // Inferred declaration.
  if (node->type == nullptr) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      auto type = static_cast<ASTType *>(node->value.get());
      if (type->kind != ASTType::REFLECTION) {
        throw_error("Cannot use a type as a value.", node->value.get()->source_range);
      }
    }

    auto value_ty = int_from_any(node->value.get()->accept(this));
    if (value_ty == void_type()) {
      throw_error("Cannot assign a variable of type 'void'", node->source_range);
    }
    auto type = global_get_type(value_ty);

    node->type = ast_alloc<ASTType>();
    node->type->resolved_type = value_ty;

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
    auto expr_type = int_from_any(node->value.get()->accept(this));
    assert_types_can_cast_or_equal(expr_type, node->type->resolved_type, node->source_range,
                                   "invalid declaration types. expected: {}, got {}",
                                   std::format("declaration: {}", node->name.get_str()));
  }

  auto symbol = ctx.scope->lookup(node->name);
  symbol->type_id = node->type->resolved_type;

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

  return {};
}

std::any Typer::visit(ASTBlock *node) {
  ctx.set_scope(node->scope);
  ControlFlow block_cf = {BLOCK_FLAGS_FALL_THROUGH, -1};

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    ASTNodeType node_type = statement->get_node_type();
    if (node_type == AST_NODE_BLOCK || node_type == AST_NODE_IF || node_type == AST_NODE_FOR ||
        node_type == AST_NODE_WHILE || node_type == AST_NODE_RETURN || node_type == AST_NODE_CONTINUE ||
        node_type == AST_NODE_BREAK || node_type == AST_NODE_EXPR_STATEMENT) {
      auto stmnt_cf = std::any_cast<ControlFlow>(result);
      block_cf.flags |= stmnt_cf.flags;
      if ((stmnt_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
        assert_return_type_is_valid(block_cf.type, stmnt_cf.type, node);
      }
      if ((stmnt_cf.flags & BLOCK_FLAGS_FALL_THROUGH) == 0) {
        block_cf.flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      }
    }
  }

  node->flags = block_cf.flags;
  node->return_type = block_cf.type;
  ctx.exit_scope();
  return block_cf;
}
std::any Typer::visit(ASTParamsDecl *node) {
  for (auto &param : node->params) {
    param->accept(this);
  }
  return {};
}
std::any Typer::visit(ASTParamDecl *node) {
  auto id = int_from_any(node->type->accept(this));

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
    if (node->default_value.is_not_null()) {
      throw_error("Cannot currently use default parameters for fixed buffer pointers.", node->source_range);
    }

    // cast off the fixed size array and add a pointer to it,
    // for s8[] to s8*
    {
      auto element = type->get_element_type();
      node->type->resolved_type = global_get_type(element)->take_pointer_to();
    }
  }

  auto old_ty = declaring_or_assigning_type;
  declaring_or_assigning_type = id;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    assert_types_can_cast_or_equal(expr_type, node->type->resolved_type, node->source_range,
                                   "invalid parameter declaration; expected: {} got: {}",
                                   std::format("parameter: {}", node->name));
  }
  return id;
}

std::any Typer::visit(ASTReturn *node) {
  int type;
  if (node->expression.is_not_null()) {
    type = int_from_any(node->expression.get()->accept(this));
  } else {
    type = ctx.scope->find_type_id("void", {});
  }
  return ControlFlow{BLOCK_FLAGS_RETURN, type};
}
std::any Typer::visit(ASTContinue *node) { return ControlFlow{BLOCK_FLAGS_CONTINUE, -1}; }
std::any Typer::visit(ASTBreak *node) { return ControlFlow{BLOCK_FLAGS_BREAK, -1}; }

std::any Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  auto iden = static_cast<ASTIdentifier *>(node->iden);
  int range_type_id = int_from_any(node->range->accept(this));
  Type *range_type = global_get_type(range_type_id);

  if (range_type->get_ext().has_extensions() && range_type->get_ext().extensions.back().type == TYPE_EXT_POINTER) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            range_type->to_string()),
                node->source_range);
  }

  int iter_ty = -1;

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
  } else if (range_type->is_kind(TYPE_STRUCT)) {
    auto info = dynamic_cast<StructTypeInfo *>(range_type->get_info());
    Symbol *begin = info->scope->lookup("begin");
    Symbol *end = info->scope->lookup("end");
    auto begin_ty = global_get_type(begin->type_id);
    // TODO: assert begin&endty are functions && return the same type.
    if (begin && end && begin->type_id == end->type_id) {
      iter_ty = begin_ty->get_info()->as<FunctionTypeInfo>()->return_type;
      if (node->value_semantic != VALUE_SEMANTIC_POINTER) {
        iter_ty = global_get_type(iter_ty)->get_element_type();
      }
    } else {
      throw_error(std::format("Can only iterate over structs you define 'begin' and "
                              "'end' on. They must both be defined, and must both "
                              "return the same type. type in question {}",
                              range_type->to_string()),
                  node->source_range);
    }
  } else {
    throw_error("Cannot iterate with a range-based for loop over a "
                "non-collection type.",
                node->source_range);
  }

  if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
    iter_ty = global_get_type(iter_ty)->take_pointer_to();
  }

  ctx.scope->insert(iden->value, iter_ty);
  node->iden->accept(this);
  node->range->accept(this);

  ctx.exit_scope();
  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}
std::any Typer::visit(ASTIf *node) {
  auto cond_ty = int_from_any(node->condition->accept(this));
  assert_types_can_cast_or_equal(cond_ty, bool_type(), node->source_range, "expected: {}, got {}",
                                 "if statement condition was not convertible to boolean");

  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  if (node->_else.is_not_null()) {
    auto _else = node->_else.get();
    auto else_cf = std::any_cast<ControlFlow>(_else->accept(this));
    control_flow.flags |= else_cf.flags;
    if ((else_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      assert_return_type_is_valid(control_flow.type, else_cf.type, node);
    }
  } else {
    control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  }
  return control_flow;
}
std::any Typer::visit(ASTElse *node) {
  if (node->_if.is_not_null()) {
    return node->_if.get()->accept(this);
  } else {
    return node->block.get()->accept(this);
  }
  return {};
}
std::any Typer::visit(ASTWhile *node) {
  if (node->condition.is_not_null()) {
    node->condition.get()->accept(this);
  }
  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  // we add fall through here because we dont know if this will get
  // excecuted since we cant evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}

std::vector<int> Typer::get_generic_arg_types(const std::vector<ASTType *> &args) {
  std::vector<int> generic_args;
  for (const auto &arg : args) {
    generic_args.push_back(int_from_any(arg->accept(this)));
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

// FEATURE(Josh) 10/1/2024, 8:46:53 AM We should be able to call constructors
// with this function syntax, using #make(Type, ...) is really clunky
// and annoying;
std::any Typer::visit(ASTCall *node) {
  auto type = global_get_type(int_from_any(node->function->accept(this)));

  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  auto symbol_nullable = get_symbol(node->function);

  if (!symbol_nullable && !type) {
    throw_error("Use of undeclared function", node->source_range);
  }

  std::vector<int> arg_tys;
  if (symbol_nullable) {
    auto symbol = symbol_nullable.get();

    type = symbol->type_id != -1 ? global_get_type(symbol->type_id) : nullptr;
    if (type) {
      declaring_or_assigning_type = type->id;
    }
    arg_tys = std::any_cast<std::vector<int>>(node->arguments->accept(this));

    if (!node->generic_arguments.empty() ||
        (symbol->declaring_node.is_not_null() &&
         symbol->declaring_node.get()->get_node_type() == AST_NODE_FUNCTION_DECLARATION &&
         static_cast<ASTFunctionDeclaration *>(symbol->declaring_node.get())->generic_parameters.size() != 0)) {
      // TODO: make the generic argument inference actually make sense. This is just kind of a hack so we can omit it on some basic calls
      // like println etc.
      if (node->generic_arguments.empty()) {
        auto gen_args = std::any_cast<std::vector<int>>(node->arguments->accept(this));
        auto type_id = visit_generic<ASTFunctionDeclaration>(&Typer::visit_function_declaration,
                                                            symbol->declaring_node.get(), gen_args);
        if (type_id == -2) {
          throw_error("Template instantiation argument count mismatch", node->source_range);
        }
        type = global_get_type(type_id);
        symbol_nullable = nullptr;
      } else {
        auto gen_args = get_generic_arg_types(node->generic_arguments);
        auto type_id = visit_generic<ASTFunctionDeclaration>(&Typer::visit_function_declaration,
                                                            symbol->declaring_node.get(), gen_args);
        if (type_id == -2) {
          throw_error("Template instantiation argument count mismatch", node->source_range);
        }
        type = global_get_type(type_id);
        symbol_nullable = nullptr;
      }
    } else {
      find_function_overload(node, symbol, arg_tys, type);
    }
  } else {
    declaring_or_assigning_type = type->id;
    arg_tys = std::any_cast<std::vector<int>>(node->arguments->accept(this));
  }

  if (!type) {
    throw_error("Unable to locate type for function call", node->source_range);
  }

  auto info = (type->get_info()->as<FunctionTypeInfo>());

  if (!info->is_varargs &&
      (arg_tys.size() > info->params_len || arg_tys.size() < info->params_len - info->default_params)) {
    throw_error(std::format("Function call has incorrect number of arguments. Expected: {}, Found: {}\n type: {}",
                            info->params_len, arg_tys.size(), type->to_string()),
                node->source_range);
  }

  for (int i = 0; i < info->params_len; ++i) {
    // !BUG: default parameters evade type checking
    if (arg_tys.size() <= i) {
      continue;
    }

    assert_types_can_cast_or_equal(arg_tys[i], info->parameter_types[i], node->source_range,
                                   "invalid argument types. expected: {}, got: {}",
                                   std::format("parameter: {} of function", i));
  }

  node->type = info->return_type;
  return info->return_type;
}
std::any Typer::visit(ASTArguments *node) {
  auto type = global_get_type(declaring_or_assigning_type);

  FunctionTypeInfo *info = nullptr;
  if (type) {
    info = dynamic_cast<FunctionTypeInfo *>(type->get_info());
  }

  std::vector<int> argument_types;
  for (int i = 0; i < node->arguments.size(); ++i) {
    auto arg = node->arguments[i];

    // TODO: make sure this never happens, we should always have the type of
    // the thing. However args are sometime used for non-functions.
    if (!info) {
      auto arg_ty = int_from_any(arg->accept(this));
      argument_types.push_back(arg_ty);
      continue;
    }
    auto old_ty = declaring_or_assigning_type;
    declaring_or_assigning_type = info->parameter_types[i];
    Defer _defer([&] { declaring_or_assigning_type = old_ty; });
    argument_types.push_back(int_from_any(arg->accept(this)));
  }
  return argument_types;
}

std::any Typer::visit(ASTExprStatement *node) {
  auto result = node->expression->accept(this);
  if (auto _switch = dynamic_cast<ASTSwitch *>(node->expression)) {
    return result;
  }
  return ControlFlow{.flags = BLOCK_FLAGS_FALL_THROUGH, .type = void_type()};
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
      auto type_id = int_from_any(ext.expression->accept(this));
      extensions.push_back({.type = ext.type, .key_type = type_id});
    } else {
      extensions.push_back({.type = ext.type});
    }
  }
  return extensions;
}

std::any Typer::visit(ASTType *node) {
  if (node->resolved_type != Type::invalid_id) {
    return node->resolved_type;
  }

  TypeExtensions extensions;
  extensions.extensions = accept_extensions(node->extensions);

  if (node->kind == ASTType::NORMAL) {
    auto &normal_ty = node->normal;
    auto symbol = ctx.scope->lookup(normal_ty.base);
    if (symbol && symbol->declaring_node.is_not_null() && !normal_ty.generic_arguments.empty()) {
      auto declaring_node = symbol->declaring_node.get();
      std::vector<int> generic_args;
      for (auto &arg : normal_ty.generic_arguments) {
        generic_args.push_back(int_from_any(arg->accept(this)));
      }
      int type_id = -1;
      if (declaring_node->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
        type_id = visit_generic<ASTStructDeclaration>(&Typer::visit_struct_declaration, declaring_node, generic_args);
      } else if (declaring_node->get_node_type() == AST_NODE_UNION_DECLARATION) {
        type_id = visit_generic<ASTUnionDeclaration>(&Typer::visit_union_declaration, declaring_node, generic_args);
      } else if (declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
        type_id =
            visit_generic<ASTFunctionDeclaration>(&Typer::visit_function_declaration, declaring_node, generic_args);
      }
      if (type_id == -1) {
        throw_error("Invalid target to generic args", node->source_range);
      } else if (type_id == -2) {
        throw_error("Template instantiation argument count mismatch", node->source_range);
      }
      node->resolved_type = global_find_type_id(type_id, extensions);
    } else {
      node->resolved_type = ctx.scope->find_type_id(node->normal.base, extensions);
    }
  } else if (node->kind == ASTType::TUPLE) {
    std::vector<int> types;
    for (const auto &t : node->tuple_types) {
      types.push_back(int_from_any(t->accept(this)));
    }
    node->resolved_type = global_find_type_id(types, extensions);
  } else if (node->kind == ASTType::REFLECTION) {
    node->pointing_to.get()->accept(this);
    node->resolved_type = ctx.scope->find_type_id(node->normal.base, extensions);
  } else if (node->kind == ASTType::FUNCTION) {
    auto &func = node->function;
    FunctionTypeInfo info;
    if (func.return_type.is_not_null()) {
      info.return_type = int_from_any(func.return_type.get()->accept(this));
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
  return node->resolved_type;
}
std::any Typer::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));

  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });
  if (node->op.type == TType::Assign || node->op.type == TType::ColonEquals) {
    declaring_or_assigning_type = left;
  }

  if (node->op.type == TType::Concat) {
    auto type = global_get_type(left);
    // TODO: if the array is a pointer to an array, we should probably have an
    // implicit dereference.
    declaring_or_assigning_type = type->get_element_type();
  }

  auto right = int_from_any(node->right->accept(this));
  auto type = global_get_type(left);

  // array remove operator.
  if (node->op.type == TType::Erase) {
    if (!type->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array", node->source_range);
    }
    auto element_ty = type->get_element_type();
    assert_types_can_cast_or_equal(right, element_ty, node->source_range, "expected : {}, got {}",
                                   "invalid type in array concatenation expression");
    return element_ty;
  }

  // CLEANUP(Josh) 10/4/2024, 2:00:49 PM
  // We copy pasted this code like in 5 places, and a lot of the stuff is just
  // identical.
  {
    if (type && type->is_kind(TYPE_STRUCT) && type->get_ext().has_no_extensions()) {
      auto info = (type->get_info()->as<StructTypeInfo>());
      if (auto sym = info->scope->lookup(node->op.value)) {
        auto enclosing_scope = ctx.scope;
        ctx.set_scope(info->scope);
        Defer _([&]() { ctx.set_scope(enclosing_scope); });
        if (sym->is_function()) {
          // TODO: fix this. we have ambiguity with how we do this
          int t = -1;
          if (sym->function_overload_types[0] == -1) {
            t = sym->type_id;
          } else {
            t = sym->function_overload_types[0];
          }
          auto fun_ty = global_get_type(t);
          auto fun_info = (fun_ty->get_info()->as<FunctionTypeInfo>());
          auto param_0 = fun_info->parameter_types[0];
          assert_types_can_cast_or_equal(right, param_0, node->source_range, "expected, {}, got {}",
                                         "invalid call to operator overload");
          return fun_info->return_type;
        }
      }
    }
  }

  // TODO: clean up this hacky mess.
  if (node->op.type == TType::Concat) {
    if (!type->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array", node->source_range);
    }
    auto element_ty = type->get_element_type();
    assert_types_can_cast_or_equal(right, element_ty, node->source_range, "expected : {}, got {}",
                                   "invalid type in array concatenation expression");
    return void_type();
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have
  // their operands type checked, but right now that would involve casting
  // scalars to each other, which makes no  sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
    return bool_type();
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
  }

  node->resolved_type = left;
  return left;
}
std::any Typer::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));

  if (node->op.type == TType::Increment || node->op.type == TType::Decrement || node->op.type == TType::And ||
      node->op.type == TType::Mul || node->op.type == TType::Not) {
  }

  if (node->op.type == TType::And) {
    return global_get_type(operand_ty)->take_pointer_to();
  }

  if (node->op.type == TType::Mul) {
    return global_get_type(operand_ty)->get_element_type();
  }

  // unary operator overload.
  auto left_ty = global_get_type(operand_ty);

  if (left_ty->get_ext().is_array() && node->op.type == TType::Not) {
    return left_ty->get_element_type();
  }

  if (left_ty && left_ty->is_kind(TYPE_STRUCT) && left_ty->get_ext().has_no_extensions()) {
    auto info = (left_ty->get_info()->as<StructTypeInfo>());
    if (auto sym = info->scope->lookup(node->op.value)) {
      auto enclosing_scope = ctx.scope;
      ctx.set_scope(info->scope);
      Defer _([&]() { ctx.set_scope(enclosing_scope); });
      if (sym->is_function()) {
        // TODO: fix this. we have ambiguitty with how we do this
        int t = -1;
        if (sym->function_overload_types[0] == -1) {
          t = sym->type_id;
        } else {
          t = sym->function_overload_types[0];
        }
        auto fun_ty = global_get_type(t);
        auto fun_info = (fun_ty->get_info()->as<FunctionTypeInfo>());
        return fun_info->return_type;
      }
    } else
      throw_error(std::format("couldn't find {} overload for struct type", node->op.value), node->source_range);
  }

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule =
        type_conversion_rule(global_get_type(operand_ty), global_get_type(bool_type()), node->operand->source_range);
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED && conversion_rule != CONVERT_EXPLICIT);

    if (node->op.type == TType::LogicalNot && can_convert) {
      return bool_type();
    }
  }

  return operand_ty;
}
std::any Typer::visit(ASTIdentifier *node) {
  auto str = node->value.get_str();

  auto type_id = ctx.scope->find_type_id(node->value, {});
  if (type_id != -1) {
    return type_id;
  }

  auto symbol = ctx.scope->lookup(node->value);
  if (symbol) {
    return symbol->type_id;
  } else {
    throw_error(std::format("Use of undeclared identifier '{}'", node->value), node->source_range);
  }
}
std::any Typer::visit(ASTLiteral *node) {
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
          if (info->is_integral)
            return type->id;
        }
      }
      return s32_type();
    }
    case ASTLiteral::Float:
      return float32_type();
    case ASTLiteral::RawString:
    case ASTLiteral::String:
      return c_string_type();
      break;
    case ASTLiteral::Bool:
      return bool_type();
    case ASTLiteral::Null:
      return voidptr_type();
    case ASTLiteral::InterpolatedString: {
      for (const auto &arg : node->interpolated_values) {
        arg->accept(this);
      }
      return ctx.scope->find_type_id("string", {});
    }
    case ASTLiteral::Char:
      return char_type();
      break;
  }
}
std::any Typer::visit(ASTDotExpr *node) {
  auto base_ty_id = int_from_any(node->base->accept(this));
  auto base_ty = global_get_type(base_ty_id);

  if (!base_ty) {
    throw_error("Internal Compiler Error: un-typed variable on lhs of dot "
                "expression?",
                node->source_range);
  }

  // TODO: remove this hack to get array length
  if (base_ty->get_ext().is_array()) {
    if (node->member_name == "length") {
      return s32_type();
    }
    if (node->member_name == "data") {
      return global_get_type(base_ty->get_element_type())->take_pointer_to();
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
      return contains_ty;
    }
  }

  Scope *base_scope = nullptr;
  if (auto info = dynamic_cast<StructTypeInfo *>(base_ty->get_info())) {
    base_scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo *>(base_ty->get_info())) {
    base_scope = info->scope;
  } else {
    throw_error("Dot expressions can only be used on structs, unions, and enums.", node->source_range);
  }

  auto member = base_scope->lookup(node->member_name);
  if (auto member = base_scope->lookup(node->member_name)) {
    return member->type_id;
  } else {
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", node->member_name, base_ty->to_string()),
                node->source_range);
  }
}
std::any Typer::visit(ASTScopeResolution *node) {
  // .EnumVariant fix ups.
  if (node->base == nullptr) {
    bool found = false;
    for (auto i = 0; i < type_table.size(); ++i) {
      auto type = global_get_type(i);
      if (type && type->is_kind(TYPE_ENUM)) {
        auto info = (type->get_info()->as<EnumTypeInfo>());
        for (const auto &key : info->keys) {
          if (key == node->member_name) {
            if (found) {
              throw_warning(WarningAmbigousVariants,
                            std::format("Found multiple enum types with variant '{}'.. "
                                        "using the `.{}` syntax will choose the first "
                                        "defined one. (Note: ignored candidate `{}`)",
                                        key.get_str(), key.get_str(), type->get_base().get_str()),
                            node->source_range);
            } else {
              auto ast_type = ast_alloc<ASTType>();
              ast_type->kind = ASTType::NORMAL;
              ast_type->normal.base = type->get_base();
              node->base = ast_type;
              found = true;
            }
          }
        }
      }
    }

    if (!found)
      throw_error(std::format("Unable to find enum variant {}", node->member_name), node->source_range);
  }

  auto id = int_from_any(node->base->accept(this));
  auto base_ty = global_get_type(id);

  Scope *scope = nullptr;
  switch (base_ty->kind) {
    case TYPE_STRUCT: {
      auto info = (base_ty->get_info()->as<StructTypeInfo>());
      scope = info->scope;
    } break;
    case TYPE_UNION: {
      auto info = (base_ty->get_info()->as<UnionTypeInfo>());
      scope = info->scope;
    } break;
    case TYPE_ENUM: {
      auto info = (base_ty->get_info()->as<EnumTypeInfo>());
      if (std::ranges::find(info->keys, node->member_name) != info->keys.end()) {
        return base_ty->id;
      }
      throw_error("failed to find key in enum type.", node->source_range);
    } break;
    default:
      throw_error("Unsupported type for scope resolution (:: operator)", node->source_range);
  }

  if (!scope) {
    throw_error("Internal Compiler Error: scope is null for scope resolution", node->source_range);
  }

  auto member = scope->lookup(node->member_name);
  if (auto member = scope->lookup(node->member_name)) {
    return member->type_id;
  } else {
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", node->member_name, base_ty->to_string()),
                node->source_range);
  }
}
std::any Typer::visit(ASTSubscript *node) {
  auto left = int_from_any(node->left->accept(this));
  auto subscript = int_from_any(node->subscript->accept(this));
  auto left_ty = global_get_type(left);

  /*
  !HACK FIX STRING SLICING THIS IS TERRIBLE
 */
  if (left_ty->id == string_type()) {
    if (subscript == range_type()) {
      return left_ty->id;
    }
    auto element_id = char_type();
    return element_id;
  }

  /// ? CLEANUP(Josh) 10/4/2024, 2:18:42 PM  Remove unwanted operator
  /// overloads.
  // delete the subscript operator, call operator, and various other operators
  // we may not want in the languaeg. We want to keep it simple, and having
  // 100-200 lines of code dedicated to things that are never used is not
  // conducive to that prospect.
  {
    if (left_ty && left_ty->is_kind(TYPE_STRUCT) && left_ty->get_ext().has_no_extensions()) {
      auto info = (left_ty->get_info()->as<StructTypeInfo>());
      if (auto sym = info->scope->lookup("[")) {
        auto enclosing_scope = ctx.scope;
        ctx.set_scope(info->scope);
        Defer _([&]() { ctx.set_scope(enclosing_scope); });
        if (sym->is_function()) {
          // TODO: fix this. we have ambiguity with how we do this
          int t = -1;
          if (sym->function_overload_types[0] == -1) {
            t = sym->type_id;
          } else {
            t = sym->function_overload_types[0];
          }
          auto fun_ty = global_get_type(t);
          auto fun_info = (fun_ty->get_info()->as<FunctionTypeInfo>());
          auto param_0 = fun_info->parameter_types[0];
          assert_types_can_cast_or_equal(subscript, fun_info->parameter_types[0], node->source_range,
                                         "expected: {}, got: {}",
                                         "invalid parameter type in subscript operator overload");
          return fun_info->return_type;
        }
      } else {
        throw_error("couldn't find [] overload for struct type", node->source_range);
      }
    }
  }
  auto ext = left_ty->get_ext();

  if (ext.is_map()) {
    assert_types_can_cast_or_equal(subscript, ext.extensions.back().key_type, node->source_range,
                                   "expected : {}, got {}", "Invalid type when subscripting map");
    return left_ty->get_element_type();
  }

  if (!left_ty->get_ext().is_array() && !left_ty->get_ext().is_fixed_sized_array() &&
      !left_ty->get_ext().is_pointer()) {
    throw_error(std::format("cannot index into non array type. {}", left_ty->to_string()), node->source_range);
  }

  if (left_ty->get_ext().is_array()) {
    if (subscript == range_type()) {
      return left_ty->id;
    }
    auto element_id = left_ty->get_element_type();
    return element_id;
  }
  return left_ty->get_element_type();
}
std::any Typer::visit(ASTMake *node) {
  auto type = int_from_any(node->type_arg->accept(this));
  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });
  declaring_or_assigning_type = type;
  if (!node->arguments->arguments.empty()) {
    node->arguments->accept(this);
  }
  if (type == -1) {
    throw_error("Cannot make non existent type", node->source_range);
  }
  return type;
}
std::any Typer::visit(ASTInitializerList *node) {
  // * TODO: Make it so we can have this more complex, up front initialization.
  // * This will allow for better sub-initializer lists.
  // * Type *type;
  // * if (declaring_or_assigning_type != -1) {
  // *   type = global_get_type(declaring_or_assigning_type);
  //   Scope *scope;
  //   switch (type->kind) {
  //     case TYPE_SCALAR: goto regular_init;
  //     case TYPE_STRUCT: {
  //       auto info = type->get_info()->as<StructTypeInfo>();
  //       scope = info->scope;
  //     } break;
  //     case TYPE_UNION: {
  //       auto info = type->get_info()->as<UnionTypeInfo>();
  //       scope = info->scope;
  //     } break;
  //     case TYPE_TUPLE: {
  //       throw_error("Cannot use an initializer list to initialize a tuple, use <value, value...> syntax.",
  //       node->source_range);
  //     } break;
  //     case TYPE_FUNCTION: {
  //       throw_error("Cannot use an initializer list to initialize a function type.", node->source_range);
  //     } break;
  //     case TYPE_ENUM: {
  //       throw_error("Cannot use an initializer list to initialize an enum type, just use the variant.",
  //       node->source_range);
  //     } break;
  //   }
  //   int i {};
  //   for (const auto& iden : scope->ordered_symbols) {
  //     auto &sym = scope->symbols[iden];
  //     if (sym.is_function()) continue;
  //     auto old_decl_ty = declaring_or_assigning_type;
  //     declaring_or_assigning_type = sym.type_id;
  //     if (node->expressions.size() <= i) {
  //       break;
  //     }
  //     auto type = int_from_any(node->expressions[i]->accept(this));
  //     declaring_or_assigning_type = old_decl_ty;
  //     // TODO: assert type compatiblity
  //   }
  // }
  // regular_init:
  int last_type = -1;
  for (const auto &expr : node->expressions) {
    int type = int_from_any(expr->accept(this));
    if (last_type == -1) {
      last_type = type;
    } else if (last_type != type) {
      auto rule = type_conversion_rule(global_get_type(type), global_get_type(last_type), expr->source_range);
      if (rule == CONVERT_PROHIBITED || rule == CONVERT_EXPLICIT) {
        node->types_are_homogenous = false;
      }
    }
    // !BUG: somehow for 2 expressions, sometimes this will end up with 4
    // ! types. I have no idea how atha's happening. I put a hack in somewhere
    // ! that checks the length of the expressions instead of the types
    node->types.push_back(type);
  }

  return assert_type_can_be_assigned_from_init_list(node, declaring_or_assigning_type);
}
std::any Typer::visit(ASTAllocate *node) {
  if (node->kind == ASTAllocate::Delete) {
    if (node->arguments.is_null() || node->arguments.get()->arguments.size() < 1)
      throw_error("invalid delete statement: you need at least one argument", node->source_range);
    for (auto &arg : node->arguments.get()->arguments) {
      arg->accept(this);
    }
    return void_type();
  }
  // just type check them, no need to return
  // we should probably type check parameters for a constructor
  // but we need a seperate system for that
  auto type = int_from_any(node->type.get()->accept(this));
  if (type == -1) {
    throw_error("Use of undeclared type", node->source_range);
  }
  if (node->arguments) {
    auto declaring_type = global_get_type(declaring_or_assigning_type);
    // Make sure that an initializer list won't try to construct a pointer which is prohibited.
    if (declaring_type->get_ext().is_pointer()) {
      declaring_or_assigning_type =
          global_find_type_id(declaring_type->base_id, declaring_type->get_ext().without_back());
    }

    node->arguments.get()->accept(this);
  }

  auto t = global_get_type(type);
  return node->type.get()->resolved_type = t->id;
}
std::any Typer::visit(ASTRange *node) {
  auto left = int_from_any(node->left->accept(this));
  auto right = int_from_any(node->right->accept(this));
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

  return range_type();
}
std::any Typer::visit(ASTSwitch *node) {
  auto type_id = int_from_any(node->target->accept(this));
  auto type = global_get_type(type_id);

  int return_type = void_type();
  int flags = BLOCK_FLAGS_FALL_THROUGH;

  for (const auto &_case : node->cases) {
    auto expr_type = int_from_any(_case.expression->accept(this));
    auto block_cf = std::any_cast<ControlFlow>(_case.block->accept(this));
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
  node->return_type = return_type;
  if (node->is_statement) {
    return ControlFlow{flags, return_type};
  } else {
    if ((flags & BLOCK_FLAGS_BREAK) != 0) {
      throw_warning(WarningSwitchBreak, "You do not need to break from switch cases.", node->source_range);
    } else if ((flags & BLOCK_FLAGS_CONTINUE) != 0) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->source_range);
    }
    return return_type;
  }
}
std::any Typer::visit(ASTTuple *node) {
  std::vector<int> types;
  for (const auto &v : node->values) {
    types.push_back(int_from_any(v->accept(this)));
  }
  TypeExtensions extensions;
  extensions.extensions = accept_extensions(node->type->extensions);
  return node->type->resolved_type = global_find_type_id(types, extensions);
}
std::any Typer::visit(ASTAlias *node) {
  node->type->accept(this);

  if (node->type->resolved_type == -1) {
    throw_error("Declaration of a variable with a non-existent type.", node->source_range);
  }

  if (ctx.scope->types.contains(node->name)) {
    throw_error("Redeclaration of type", node->source_range);
  }
  ctx.scope->types[node->name] = node->type->resolved_type;

  return {};
}
std::any Typer::visit(ASTTupleDeconstruction *node) {
  auto type = global_get_type(int_from_any(node->right->accept(this)));

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
    if (ctx.scope->local_lookup(iden->value)) {
      throw_error(std::format("Redefinition of a variable is not allowed in a tuple "
                              "deconstruction yet.\nOffending variable {}",
                              iden->value),
                  node->source_range);
    }

    ctx.scope->insert(iden->value, type);
  }

  return {};
};
