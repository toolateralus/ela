#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <any>
#include <cassert>
#include <format>

#include <ranges>
#include <string>
#include <vector>

static inline int int_from_any(const std::any &any) {
  return std::any_cast<int>(any);
}

void assert_types_can_cast_or_equal(
    const int from, const int to, const SourceRange &source_range,
    const std::format_string<std::string, std::string> &format,
    const std::string &message) {
  auto from_t = global_get_type(from);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t);
  if (to != from &&
      (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' +
                    std::format(format, to_t->to_string(), from_t->to_string()),
                source_range);
  }
}

void assert_return_type_is_valid(int &return_type, int new_type,
                                 ASTNode *node) {
  if (return_type == -1) {
    return_type = new_type;
  } else if (new_type != -1 && new_type != return_type) {
    assert_types_can_cast_or_equal(new_type, return_type, node->source_range,
                                   "Expected: {}, Found: {}",
                                   "Inconsistent return types in block.");
  }
};
void Typer::find_function_overload(ASTCall *&node, Symbol *&symbol,
                                   std::vector<int> &arg_tys, Type *&type) {
  if ((symbol->flags & SYMBOL_HAS_OVERLOADS) != 0) {
    bool found_exact_match = false;
    int exact_match_idx = -1;

    bool found_implicit_match = false;
    int implicit_match_idx = -1;

// Define the helper macro
#define NON_VARARGS_NO_DEFAULT_PARAMS(info)                                    \
  (!info->is_varargs && info->default_params == 0)

    for (const auto &[i, overload] :
         symbol->function_overload_types | std::ranges::views::enumerate) {

      auto ovrld_ty = ctx.scope->get_type(overload);
      auto info = static_cast<FunctionTypeInfo *>(ovrld_ty->get_info());

      bool match = true;
      int required_params = info->params_len - info->default_params;
      if (arg_tys.size() < required_params ||
          (!info->is_varargs && arg_tys.size() > info->params_len)) {
        match = false;
      } else {
        for (int j = 0; j < arg_tys.size(); ++j) {
          if (j >= info->params_len) {
            if (!info->is_varargs) {
              match = false;
              break;
            }
          } else {
            auto conversion_rule = type_conversion_rule(
                ctx.scope->get_type(arg_tys[j]),
                ctx.scope->get_type(info->parameter_types[j]));
            if (conversion_rule == CONVERT_EXPLICIT &&
                NON_VARARGS_NO_DEFAULT_PARAMS(info)) {
              match = false;
              break;
            }
            if (conversion_rule == CONVERT_IMPLICIT &&
                NON_VARARGS_NO_DEFAULT_PARAMS(info)) {
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
        names.push_back(ctx.scope->get_type(n)->to_string());
      }
      throw_error(std::format("No function overload for provided argument "
                              "signature found.. got : {}",
                              names),
                  node->source_range);
    }

    if (found_exact_match) {
      type =
          ctx.scope->get_type(symbol->function_overload_types[exact_match_idx]);
      assert(type != nullptr);
    } else {
      type = ctx.scope->get_type(
          symbol->function_overload_types[implicit_match_idx]);
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
int assert_type_can_be_assigned_from_init_list(ASTInitializerList *node,
                                               int declaring_type) {
  auto type = global_get_type(declaring_type);
  //! BUG This fails to accurately type check sub initializers. Also, we may
  //! want to implement sub initializers for struct types
  // and check those too
  if (node->types_are_homogenous &&
      (type->get_ext().is_array() || type->get_ext().is_fixed_sized_array())) {
    for (const auto [i, expr] :
         node->expressions | std::ranges::views::enumerate) {
      if (expr->get_node_type() == AST_NODE_INITIALIZER_LIST) {
        assert_type_can_be_assigned_from_init_list(
            static_cast<ASTInitializerList *>(expr), node->types[i]);
      }
    }
    return declaring_type;
  }
  if (type->get_ext().has_extensions()) {
    throw_error("Unable to construct type from initializer list",
                node->source_range);
  }
  if (type->is_kind(TYPE_SCALAR)) {
    // this is just a plain scalar type, such as an int.
  } else if (type->is_kind(TYPE_STRUCT)) {
    auto info = static_cast<StructTypeInfo *>(type->get_info());

    // TODO: re enable this once we can find constructors
    for (const auto &[name, symbol] : info->scope->symbols) {
      if (name == "this")
        continue;

      // constructors use anonymous symbol names.
      if ((symbol.flags & SYMBOL_IS_FUNCTION) == 0 ||
          !name.get_str().contains("__anon_D"))
        continue;
      auto type = global_get_type(symbol.type_id);

      if (!type)
        continue;

      auto info = static_cast<FunctionTypeInfo *>(type->get_info());
      auto &params = info->parameter_types;

      if (info->params_len != node->expressions.size()) {
        continue;
      }

      for (int i = 0; i < info->params_len; ++i) {
        auto type = global_get_type(params[i]);
        auto rule = type_conversion_rule(type, global_get_type(node->types[i]));
        if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
          continue;
        }
      }

      // TODO: fix this. We need to know if a symbol is a constructor or not,
      // right now we are just assuming if there's
      // TODO: a function that matches the type signature of the init list
      // within the struct, that it's a valid constructor.
      return declaring_type;
    }

    // !HACK i used node->expressions.size() to bypass a bug with the types of
    // initlist exceeding the number of expressions.
    // * REMOVE ME *
    if (info->scope->fields_count() < node->expressions.size()) {
      throw_error("excess elements provided in initializer list.",
                  node->source_range);
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
      if (!sym.is_function() &&
          !global_get_type(sym.type_id)->is_kind(TYPE_FUNCTION)) {
        assert_types_can_cast_or_equal(
            node->types[i], sym.type_id, node->source_range,
            "expected: {}, got: {}",
            "Invalid types in initializer list for struct");
      }
      i++;
    }
  } else if (type->is_kind(TYPE_UNION)) {
    auto info = static_cast<UnionTypeInfo *>(type->get_info());
    if (node->types.size() > 1) {
      throw_error("You can only initialize one field of a union with an "
                  "initializer list",
                  node->source_range);
    }
    // search for the first field member and type check against it.
    for (const auto &[name, sym] : info->scope->symbols) {
      if (!sym.is_function() &&
          !global_get_type(sym.type_id)->is_kind(TYPE_FUNCTION)) {
        assert_types_can_cast_or_equal(
            node->types[0], sym.type_id, node->source_range, "{}, {}",
            "Invalid types in initializer list for union");
        break;
      }
    }
  } else {
    throw_error("Unable to construct type from initializer list",
                node->source_range);
  }
  return declaring_type;
}

std::any Typer::visit(ASTProgram *node) {
  for (auto &statement : node->statements)
    statement->accept(this);
  return {};
}

std::any Typer::visit(ASTStructDeclaration *node) {
  auto last_decl = current_struct_decl;
  current_struct_decl = node;
  Defer _([&] { current_struct_decl = last_decl; });

  auto type = ctx.scope->get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->get_info());

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0 || node->is_fwd_decl) {
    return {};
  }

  if (info->scope == nullptr) {
    info->scope = node->scope;
  }
  ctx.set_scope(info->scope);
  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method : node->methods) {
    method->accept(this);
  }
  ctx.exit_scope();
  return {};
}
std::any Typer::visit(ASTUnionDeclaration *node) {

  if (node->is_fwd_decl) {
    return {};
  }

  auto last_decl = current_union_decl;

  current_union_decl = node;
  Defer _([&] { current_union_decl = last_decl; });
  // we store this ast just to type check the stuff.
  ctx.set_scope(node->scope);

  // do this first.
  for (const auto &_struct : node->structs) {
    for (const auto &field : _struct->fields) {
      field->accept(this);
      node->scope->insert(field->name.value, field->type->resolved_type);
    }
  }

  for (const auto &field : node->fields) {
    field->accept(this);
  }
  for (const auto &method : node->methods) {
    method->accept(this);
  }

  ctx.exit_scope();
  return {};
}
std::any Typer::visit(ASTEnumDeclaration *node) {

  auto elem_type = -1;

  for (const auto &[key, value] : node->key_values) {
    if (value.is_null())
      continue;

    if (node->is_flags) {
      throw_error("You shouldn't use a #flags enum to generate auto "
                  "flags, and also use non-default values.",
                  node->source_range);
    }

    auto expr = value.get();
    auto id = int_from_any(value.get()->accept(this));
    auto type = ctx.scope->get_type(id);

    if (elem_type == -1) {
      elem_type = id;
    }

    assert_types_can_cast_or_equal(id, elem_type, node->source_range,
                                   "expected: {}, got : {}",
                                   "Inconsistent types in enum declaration.");
  }

  if (elem_type == void_type())
    throw_error("Invalid enum declaration.. got null or no type.",
                node->source_range);

  if (elem_type == -1) {
    elem_type = s32_type();
  }

  node->element_type = elem_type;

  auto enum_type =
      global_get_type(global_find_type_id(node->type->base, {}));
  auto info = static_cast<EnumTypeInfo *>(enum_type->get_info());
  info->element_type = elem_type;

  return {};
}
std::any Typer::visit(ASTFunctionDeclaration *node) {
  auto last_decl = current_func_decl;
  current_func_decl = node;
  Defer _([&] { current_func_decl = last_decl; });

  if (ctx.scope->is_struct_or_union_scope) {
    node->flags |= FUNCTION_IS_METHOD;

    if (current_struct_decl) {
      auto ty = current_struct_decl.get()->type->resolved_type;
      if (ty == -1) {
        throw_error(
            "Internal compiler error: Failed to get type of 'this' pointer",
            node->source_range);
      }

      ctx.scope->insert("this", ctx.scope->get_pointer_to_type(ty));
    } else if (current_union_decl) {
      ctx.scope->insert("this",
                        ctx.scope->get_pointer_to_type(
                            current_union_decl.get()->type->resolved_type));
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

  auto name = node->name.value;

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

  auto type_id =
      ctx.scope->find_function_type_id(get_function_typename(node), info, {});

  // TODO: we need to support fwd decls of overloaded functions
  if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
    ctx.scope->insert(node->name.value, type_id);
    auto sym = ctx.scope->lookup(node->name.value);
    sym->flags |= SYMBOL_IS_FORWARD_DECLARED | SYMBOL_IS_FUNCTION;
    return {};
  }

  auto sym = ctx.scope->lookup(node->name.value);

  if (sym && (sym->flags & SYMBOL_IS_FORWARD_DECLARED) != 0) {
    sym->flags &= ~SYMBOL_IS_FORWARD_DECLARED;
  }

  // CLEANUP(Josh) 10/7/2024, 8:07:00 AM
  // This is ugly. It's for function overloading
  if (sym && ((node->flags & FUNCTION_IS_CTOR) == 0) &&
      (node->flags & FUNCTION_IS_DTOR) == 0) {
    if (sym->function_overload_types.size() >= 1)
      sym->flags |= SYMBOL_HAS_OVERLOADS;
    for (const auto overload_type_id : sym->function_overload_types) {
      auto type = ctx.scope->get_type(overload_type_id);
      auto this_type = ctx.scope->get_type(type_id);
      if (type->equals(this_type->get_base(), this_type->get_ext()))
        throw_error(std::format("re-definition of function '{}'",
                                node->name.value.get_str()),
                    {});
    }
    sym->function_overload_types.push_back(type_id);
    sym->type_id = type_id;
  } else {
    // always insert the first function declarations as the 0th overloaded type,
    // because we can tell when a fucntion has been overloaded when this array's
    // size is > 1
    ctx.scope->insert(node->name.value, type_id, SYMBOL_IS_FUNCTION);
    auto sym = ctx.scope->lookup(node->name.value);
    sym->function_overload_types.push_back(type_id);
    sym->declaring_node = node;
  }
  if (info.meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN)
    return {};

  auto old_ty = declaring_or_assigning_type;
  auto _defer = Defer([&] { declaring_or_assigning_type = old_ty; });
  declaring_or_assigning_type = info.return_type;

  auto control_flow =
      std::any_cast<ControlFlow>(node->block.get()->accept(this));
  if (control_flow.type == -1)
    control_flow.type = void_type();
  const auto is_ctor = (node->flags & FUNCTION_IS_CTOR) != 0,
             is_dtor = (node->flags & FUNCTION_IS_DTOR) != 0;
  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0)
    throw_error("Keyword \"continue\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0)
    throw_error("Keyword \"break\" must be in a loop.", node->source_range);
  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 &&
      info.return_type != void_type() && !(is_ctor || is_dtor))
    throw_error("Not all code paths return a value.", node->source_range);
  assert_types_can_cast_or_equal(
      control_flow.type, info.return_type, node->source_range,
      "invalid return type.. expected '{}', got '{}'",
      std::format("function: '{}'", node->name.value.get_str()));
  return {};
}
std::any Typer::visit(ASTDeclaration *node) {

  // Inferred declaration.
  if (node->type == nullptr) {
    auto value_ty = int_from_any(node->value.get()->accept(this));
    if (value_ty == void_type()) {
      throw_error("Cannot assign a variable of type 'void'",
                  node->source_range);
    }
    auto type = global_get_type(value_ty);

    { // TODO: make sure we even need to do this. Perhaps we just assign the
      // resolved type.
      node->type = ast_alloc<ASTType>();
      node->type->resolved_type = value_ty;
      node->type->base = type->get_base();
      node->type->extension_info = type->get_ext();
    }

    if (type->is_kind(TYPE_SCALAR) && type->get_ext().has_no_extensions()) {
      auto info = static_cast<ScalarTypeInfo *>(type->get_info());
      auto rule = type_conversion_rule(type, ctx.scope->get_type(int_type()));
      if (info->is_integral && rule != CONVERT_PROHIBITED &&
          rule != CONVERT_EXPLICIT) {
        // CLEANUP: again, make sure we need to mock this up to this extent. I
        // can't see whyw e'd look it up again.
        node->type->resolved_type = int_type();
        node->type->base = "int";
        node->type->extension_info = {};
      }
    }
  }

  node->type->accept(this);

  if (node->type->resolved_type == -1) {
    throw_error("Declaration of a variable with a non-existent type.",
                node->source_range);
  }

  if (node->value.is_not_null()) {
    auto old_ty = declaring_or_assigning_type;
    declaring_or_assigning_type = node->type->resolved_type;
    Defer _defer([&] { declaring_or_assigning_type = old_ty; });
    auto expr_type = int_from_any(node->value.get()->accept(this));
    assert_types_can_cast_or_equal(
        expr_type, node->type->resolved_type, node->source_range,
        "invalid declaration types. expected: {}, got {}",
        std::format("declaration: {}", node->name.value.get_str()));
  }

  auto symbol = ctx.scope->lookup(node->name.value);
  symbol->type_id = node->type->resolved_type;

  if (symbol->type_id == void_type() ||
      node->type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}",
                            node->name.value.get_str()),
                node->source_range);
  }
  
  return {};
}

std::any Typer::visit(ASTBlock *node) {
  ctx.set_scope(node->scope);
  ControlFlow block_cf = {BLOCK_FLAGS_FALL_THROUGH, -1};

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    ASTNodeType node_type = statement->get_node_type();
    if (node_type == AST_NODE_BLOCK || node_type == AST_NODE_IF ||
        node_type == AST_NODE_FOR || node_type == AST_NODE_WHILE ||
        node_type == AST_NODE_RETURN || node_type == AST_NODE_CONTINUE ||
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

  auto type = ctx.scope->get_type(id);

  if (type->get_ext().is_fixed_sized_array()) {
    throw_warning("using a fixed array as a function parameter: note, this "
                  "casts the length information off and gets passed as as "
                  "pointer. Consider using a dynamic array",
                  node->source_range);
    if (node->default_value.is_not_null()) {
      throw_warning("Cannot currently use default parameters for fixed buffer "
                    "pointers. Also, length information gets casted off. "
                    "consider using a dynamic array",
                    node->source_range);
    }

    // cast off the fixed size array and add a pointer to it,
    // for s8[] to s8*
    {
      auto element = type->get_element_type();
      auto element_t = ctx.scope->get_type(element);
      auto extensions = element_t->get_ext();
      extensions.extensions.push_back(TYPE_EXT_POINTER);
      node->type->resolved_type =
          ctx.scope->find_type_id(element_t->get_base(), extensions);
    }
  }

  auto old_ty = declaring_or_assigning_type;
  declaring_or_assigning_type = id;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    assert_types_can_cast_or_equal(
        expr_type, node->type->resolved_type, node->source_range,
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
std::any Typer::visit(ASTContinue *node) {
  return ControlFlow{BLOCK_FLAGS_CONTINUE, -1};
}
std::any Typer::visit(ASTBreak *node) {
  return ControlFlow{BLOCK_FLAGS_BREAK, -1};
}

std::any Typer::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);

  auto iden = static_cast<ASTIdentifier *>(node->iden);
  int range_type_id = int_from_any(node->range->accept(this));
  Type *range_type = ctx.scope->get_type(range_type_id);

  if (range_type->get_ext().has_extensions() &&
      range_type->get_ext().extensions.back() == TYPE_EXT_POINTER) {
    throw_error(
        std::format(
            "Cannot iterate over a pointer. Did you mean to dereference a "
            "pointer to an array, range or struct? got type {}",
            range_type->to_string()),
        node->source_range);
  }

  int iter_ty = -1;

  if (range_type_id == global_find_type_id("string", {})) {
    iter_ty = char_type();
  } else if (range_type_id == global_find_type_id("Range", {})) {
    iter_ty =
        int_type(); // ! THIS SHOULD BE S64 BUT IT CAUSES ANNOY BALLS ISSUES.
    if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
      throw_error(
          "Cannot use pointer value semantic with a range. use Range{<start>, "
          "<end>, <increment>} syntax to increment by a custom value.",
          node->source_range);
    }
  } else if (range_type->get_ext().is_array() ||
             range_type->get_ext().is_fixed_sized_array()) {
    iter_ty = range_type->get_element_type();
  } else if (range_type->is_kind(TYPE_STRUCT)) {
    auto info = dynamic_cast<StructTypeInfo *>(range_type->get_info());
    Symbol *begin = info->scope->lookup("begin");
    Symbol *end = info->scope->lookup("end");
    if (begin && end && begin->type_id == end->type_id) {
      iter_ty = begin->type_id;
    } else {
      throw_error(
          std::format("Can only iterate over structs you define 'begin' and "
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
    auto type = ctx.scope->get_type(iter_ty);
    auto ext = type->get_ext();
    ext.extensions.push_back(TYPE_EXT_POINTER);
    iter_ty = ctx.scope->find_type_id(type->get_base(), ext);
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
  assert_types_can_cast_or_equal(
      cond_ty, bool_type(), node->source_range, "expected: {}, got {}",
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

// FEATURE(Josh) 10/1/2024, 8:46:53 AM We should be able to call constructors
// with this function syntax, using #make(Type, ...) is really clunky
// and annoying;
std::any Typer::visit(ASTCall *node) {
  auto type = global_get_type(int_from_any(node->function->accept(this)));

  auto old_ty = declaring_or_assigning_type;
  Defer _defer([&] { declaring_or_assigning_type = old_ty; });

  if (type)
    declaring_or_assigning_type = type->id;

  std::vector<int> arg_tys =
      std::any_cast<std::vector<int>>(node->arguments->accept(this));

  // the type may be null for a generic function but the if statement above
  // should always take care of that if that was the case.
  if (type == nullptr) {
    throw_error("Use of undeclared function", node->source_range);
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("Unable to call function... target did not refer "
                            "to a function typed variable. Constructors "
                            "currently use #make(Type, ...) syntax."),
                node->source_range);
  }

  if (node->function->get_node_type() == AST_NODE_IDENTIFIER) {
    auto identifier = static_cast<ASTIdentifier *>(node->function);
    auto symbol = ctx.scope->lookup(identifier->value);
    find_function_overload(node, symbol, arg_tys, type);
  }

  auto info = static_cast<FunctionTypeInfo *>(type->get_info());

  if (!info->is_varargs &&
      (arg_tys.size() > info->params_len ||
       arg_tys.size() < info->params_len - info->default_params)) {
    throw_error(std::format("Function call has incorrect number of arguments. "
                            "Expected: {}, Found: {}",
                            info->params_len, arg_tys.size()),
                node->source_range);
  }

  for (int i = 0; i < info->params_len; ++i) {
    // !BUG: default parameters evade type checking
    if (arg_tys.size() <= i) {
      continue;
    }

    assert_types_can_cast_or_equal(
        arg_tys[i], info->parameter_types[i], node->source_range,
        "invalid argument types. expected: {}, got: {}",
        std::format("parameter: {} of function", i));
  }

  node->type = info->return_type;
  return info->return_type;
}
std::any Typer::visit(ASTArguments *node) {

  auto type = ctx.scope->get_type(declaring_or_assigning_type);

  FunctionTypeInfo *info = nullptr;
  if (type) {
    info = dynamic_cast<FunctionTypeInfo *>(type->get_info());
  }

  std::vector<int> argument_types;
  for (int i = 0; i < node->arguments.size(); ++i) {
    // TODO: make sure this never happens, we should always have the type of
    // the thing. However args are sometime used for non-functions.
    auto arg = node->arguments[i];
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

std::any Typer::visit(ASTType *node) {
  if (!node->tuple_types.empty()) {
    std::vector<int> types;
    for (const auto &t : node->tuple_types)
      types.push_back(int_from_any(t->accept(this)));
    node->resolved_type = ctx.scope->find_type_id(types, node->extension_info);
    // node->base = get_tuple_types).get_str);

  } else if (node->flags == ASTTYPE_EMIT_OBJECT) {
    node->pointing_to.get()->accept(this);
    node->resolved_type =
        ctx.scope->find_type_id(node->base, node->extension_info);
  } else {
    node->resolved_type =
        ctx.scope->find_type_id(node->base, node->extension_info);
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
  auto type = ctx.scope->get_type(left);

  // array remove operator.
  if (node->op.type == TType::Erase) {
    if (!type->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array",
                  node->source_range);
    }
    auto element_ty = type->get_element_type();
    assert_types_can_cast_or_equal(
        right, element_ty, node->source_range, "expected : {}, got {}",
        "invalid type in array concatenation expression");
    return element_ty;
  }

  // CLEANUP(Josh) 10/4/2024, 2:00:49 PM
  // We copy pasted this code like in 5 places, and a lot of the stuff is just
  // identical.
  {
    if (type && type->is_kind(TYPE_STRUCT) &&
        type->get_ext().has_no_extensions()) {
      auto info = static_cast<StructTypeInfo *>(type->get_info());
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
          auto fun_ty = ctx.scope->get_type(t);
          auto fun_info = static_cast<FunctionTypeInfo *>(fun_ty->get_info());
          auto param_0 = fun_info->parameter_types[0];
          assert_types_can_cast_or_equal(right, param_0, node->source_range,
                                         "expected, {}, got {}",
                                         "invalid call to operator overload");
          return fun_info->return_type;
        }
      }
    }
  }

  // TODO: clean up this hacky mess.
  if (node->op.type == TType::Concat) {
    if (!type->get_ext().is_array()) {
      throw_error("Cannot use concat operator on a non-array",
                  node->source_range);
    }
    auto element_ty = type->get_element_type();
    assert_types_can_cast_or_equal(
        right, element_ty, node->source_range, "expected : {}, got {}",
        "invalid type in array concatenation expression");
    return void_type();
  }

  if (node->op.type == TType::Assign || node->op.is_comp_assign()) {
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have
  // their operands type checked, but right now that would involve casting
  // scalars to each other, which makes no  sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
    return bool_type();
  } else {
    auto left_t = ctx.scope->get_type(left);
    auto right_t = ctx.scope->get_type(right);
    auto conv_rule_0 = type_conversion_rule(left_t, right_t);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t);

    if (((conv_rule_0 == CONVERT_PROHIBITED) &&
         (conv_rule_1 == CONVERT_PROHIBITED)) ||
        ((conv_rule_0 == CONVERT_EXPLICIT) &&
         (conv_rule_1 == CONVERT_EXPLICIT))) {
      throw_error(std::format("Type error in binary expression: cannot convert "
                              "between {} and {}",
                              left_t->to_string(), right_t->to_string()),
                  node->source_range);
    }
  }

  node->resolved_type = left;
  return left;
}
std::any Typer::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));

  if (node->op.type == TType::Increment || node->op.type == TType::Decrement ||
      node->op.type == TType::And || node->op.type == TType::Mul ||
      node->op.type == TType::BitwiseNot) {
  }

  if (node->op.type == TType::And) {
    return ctx.scope->get_pointer_to_type(operand_ty);
  }

  if (node->op.type == TType::Mul) {
    return remove_one_pointer_ext(operand_ty, node->source_range);
  }

  // unary operator overload.
  auto left_ty = ctx.scope->get_type(operand_ty);

  if (left_ty->get_ext().is_array() && node->op.type == TType::BitwiseNot) {
    return left_ty->get_element_type();
  }

  if (left_ty && left_ty->is_kind(TYPE_STRUCT) &&
      left_ty->get_ext().has_no_extensions()) {
    auto info = static_cast<StructTypeInfo *>(left_ty->get_info());
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
        auto fun_ty = ctx.scope->get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo *>(fun_ty->get_info());
        return fun_info->return_type;
      }
    } else
      throw_error(std::format("couldn't find {} overload for struct type",
                              node->op.value),
                  node->source_range);
  }

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule = type_conversion_rule(
        ctx.scope->get_type(operand_ty), ctx.scope->get_type(bool_type()));
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED &&
                        conversion_rule != CONVERT_EXPLICIT);

    if (node->op.type == TType::Not && can_convert) {
      return bool_type();
    }
  }

  return operand_ty;
}
std::any Typer::visit(ASTIdentifier *node) {

  auto str = node->value;

  auto type_id = ctx.scope->find_type_id(node->value, {});
  if (type_id != -1) {
    return type_id;
  }

  auto symbol = ctx.scope->lookup(node->value);
  if (symbol) {
    return symbol->type_id;
  } else {
    throw_error(std::format("Use of undeclared identifier '{}'", node->value),
                node->source_range);
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
      auto type = ctx.scope->get_type(declaring_or_assigning_type);
      if (type->is_kind(TYPE_SCALAR) && type_is_numerical(type)) {
        auto info = static_cast<ScalarTypeInfo *>(type->get_info());
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
  // .EnumVariant fix ups.
  if (node->base == nullptr) {
    bool found = false;
    for (auto i = 0; i < type_table.size(); ++i) {
      auto type = ctx.scope->get_type(i);
      if (type && type->is_kind(TYPE_ENUM)) {
        auto info = static_cast<EnumTypeInfo *>(type->get_info());
        for (const auto &key : info->keys) {
          if (key == node->member_name) {
            if (found) {
              throw_warning(
                  std::format("Found multiple enum types with variant '{}'.. "
                              "using the `.{}` syntax will choose the first "
                              "defined one. (Note: ignored candidate `{}`)",
                              key.get_str(), key.get_str(),
                              type->get_base().get_str()),
                  node->source_range);
            } else {
              auto ast_type = ast_alloc<ASTType>();
              ast_type->base = type->get_base();
              node->base = ast_type;
              found = true;
            }
          }
        }
      }
    }

    if (!found)
      throw_error(
          std::format("Unable to find enum variant {}", node->member_name),
          node->source_range);
  }

  auto base_ty_id = int_from_any(node->base->accept(this));
  auto base_ty = ctx.scope->get_type(base_ty_id);

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
      return ctx.scope->get_pointer_to_type(base_ty->get_element_type());
    }
  }

  // TODO: remove this hack as well
  if (base_ty->get_ext().is_map()) {
    if (node->member_name == "contains") {
      static auto contains_ty = []{
        auto func = FunctionTypeInfo{};
        func.is_varargs = true;
        func.return_type = global_find_type_id("bool", {});
        return global_find_function_type_id("bool(...)", func, {});
      }();
      return contains_ty;
    }
  }

  if (base_ty->is_kind(TYPE_ENUM)) {
    auto info = static_cast<EnumTypeInfo *>(base_ty->get_info());
    if (std::ranges::find(info->keys, node->member_name) != info->keys.end()) {
      return info->element_type;
    }
    throw_error("failed to find key in enum type.", node->source_range);
  }

  Scope *base_scope = nullptr;
  if (auto info = dynamic_cast<StructTypeInfo *>(base_ty->get_info())) {
    base_scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo *>(base_ty->get_info())) {
    base_scope = info->scope;
  } else {
    throw_error("Dot expressions can only be used on structs, unions, and enums.",
                node->source_range);
  }

  auto member = base_scope->lookup(node->member_name);
  return member->type_id;
}

std::any Typer::visit(ASTScopeResolution *node) {
  auto t = global_get_type(int_from_any(node->left->accept(this)));
  auto left_ty = global_get_type(t->get_true_type());

  Scope *scope = nullptr;
  switch (left_ty->kind) {
  case TYPE_STRUCT: {
    auto info = static_cast<StructTypeInfo *>(left_ty->get_info());
    scope = info->scope;
  } break;
  case TYPE_UNION: {
    auto info = static_cast<UnionTypeInfo *>(left_ty->get_info());
    scope = info->scope;
  } break;
  default:
    throw_error("Unsupported type for scope resolution (:: operator)",
                node->source_range);
  }

  if (!scope) {
    throw_error("Internal Compiler Error: scope is null for scope resolution",
                node->source_range);
  }

  auto calling_scope = ctx.scope;
  Scope *dot_parent = scope->parent;

  if (dot_parent && calling_scope != scope && dot_parent != calling_scope) {
    scope->parent = calling_scope;
  }

  ctx.set_scope(scope);
  int type = int_from_any(node->right->accept(this));
  ctx.set_scope(calling_scope);

  if (dot_parent && calling_scope != scope && dot_parent != calling_scope) {
    scope->parent = dot_parent;
  }
  return type;
}

std::any Typer::visit(ASTSubscript *node) {

  auto left = int_from_any(node->left->accept(this));
  auto subscript = int_from_any(node->subscript->accept(this));
  // TODO: adding get true type fixed a bug here. This shouldn't really be
  // neccesary, The alias system is still crappy.
  auto left_ty = global_get_type(ctx.scope->get_type(left)->get_true_type());

  /*
  !HACK FIX STRING SLICING THIS IS TERRIBLE
 */
  if (left_ty->id == global_find_type_id("string", {})) {
    if (subscript == global_find_type_id("Range", {})) {
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
    if (left_ty && left_ty->is_kind(TYPE_STRUCT) &&
        left_ty->get_ext().has_no_extensions()) {
      auto info = static_cast<StructTypeInfo *>(left_ty->get_info());
      if (auto sym = info->scope->lookup("[")) {
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
          auto fun_ty = ctx.scope->get_type(t);
          auto fun_info = static_cast<FunctionTypeInfo *>(fun_ty->get_info());
          auto param_0 = fun_info->parameter_types[0];
          assert_types_can_cast_or_equal(
              subscript, fun_info->parameter_types[0], node->source_range,
              "expected: {}, got: {}",
              "invalid parameter type in subscript operator overload");
          return fun_info->return_type;
        }
      } else {
        throw_error("couldn't find [] overload for struct type",
                    node->source_range);
      }
    }
  }
  auto ext = left_ty->get_ext();

  if (ext.is_map()) {
    assert_types_can_cast_or_equal(subscript, ext.key_type, node->source_range,
                                   "expected : {}, got {}",
                                   "Invalid type when subscripting map");
    return get_map_value_type(left_ty);
  }

  if (!left_ty->get_ext().is_array() && !left_ty->get_ext().is_pointer()) {
    throw_error(std::format("cannot index into non array type. {}",
                            left_ty->to_string()),
                node->source_range);
  }

  if (left_ty->get_ext().is_array()) {
    if (subscript == global_find_type_id("Range", {})) {
      return left_ty->id;
    }
    auto element_id = left_ty->get_element_type();
    return element_id;
  }
  return remove_one_pointer_ext(left_ty->id, node->source_range);
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
  int last_type = -1;
  for (const auto &expr : node->expressions) {
    int type = int_from_any(expr->accept(this));
    if (last_type == -1) {
      last_type = type;
    } else if (last_type != type) {
      auto rule = type_conversion_rule(ctx.scope->get_type(type),
                                       ctx.scope->get_type(last_type));
      if (rule == CONVERT_PROHIBITED || rule == CONVERT_EXPLICIT) {
        node->types_are_homogenous = false;
      }
    }
    // !BUG: somehow for 2 expressions, sometimes this will end up with 4
    // types. I have no idea how atha's happening. I put a hack in somewhere
    // that checks the length of the expressions instead of the types. paste
    // this into the terminal and click the link ::  echo
    // type_visitor.cpp:249:1
    node->types.push_back(type);
  }

  return assert_type_can_be_assigned_from_init_list(
      node, declaring_or_assigning_type);
}
std::any Typer::visit(ASTAllocate *node) {
  if (node->kind == ASTAllocate::Delete) {
    if (node->arguments.is_null() ||
        node->arguments.get()->arguments.size() < 1)
      throw_error("invalid delete statement: you need at least one argument",
                  node->source_range);
    return void_type();
  }
  // just type check them, no need to return
  // we should probably type check parameters for a constructor
  // but we need a seperate system for that
  auto type = int_from_any(node->type.get()->accept(this));
  if (type == -1) {
    throw_error("Use of undeclared type", node->source_range);
  }
  if (node->arguments)
    node->arguments.get()->accept(this);

  auto t = ctx.scope->get_type(type);
  return node->type.get()->resolved_type = t->id;
}
std::any Typer::visit(ASTRange *node) {
  auto left = int_from_any(node->left->accept(this));
  auto right = int_from_any(node->right->accept(this));
  if (!type_is_numerical(ctx.scope->get_type(left)) ||
      !type_is_numerical(ctx.scope->get_type(right))) {
    throw_error("cannot use a non-numerical type in a range expression",
                node->source_range);
  }

  auto l_ty = ctx.scope->get_type(left);
  auto r_ty = ctx.scope->get_type(right);

  if (!l_ty->is_kind(TYPE_SCALAR) || !r_ty->is_kind(TYPE_SCALAR)) {
    throw_error("Cannot use non-scalar or integral types in a range expression",
                node->source_range);
  }

  auto l_info = static_cast<ScalarTypeInfo *>(l_ty->get_info());
  auto r_info = static_cast<ScalarTypeInfo *>(r_ty->get_info());

  if (!l_info->is_integral || !r_info->is_integral) {
    throw_error("Cannot use non-scalar or integral types in a range expression",
                node->source_range);
  }

  return global_find_type_id("Range", {});
}
std::any Typer::visit(ASTSwitch *node) {
  auto type_id = int_from_any(node->target->accept(this));
  auto type = ctx.scope->get_type(type_id);

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

    if (expr_type == global_find_type_id("Range", {}) &&
        type_is_numerical(type)) {
      continue;
    } else {
      assert_types_can_cast_or_equal(expr_type, type_id, node->source_range,
                                     "got {}, expected {}",
                                     "Invalid switch case.");
    }
  }
  node->return_type = return_type;
  if (node->is_statement) {
    return ControlFlow{flags, return_type};
  } else {
    if ((flags & BLOCK_FLAGS_BREAK) != 0) {
      throw_warning("You do not need to break from switch cases.",
                    node->source_range);
    } else if ((flags & BLOCK_FLAGS_CONTINUE) != 0) {
      throw_error("Cannot continue from a switch case: it is not a loop.",
                  node->source_range);
    }
    return return_type;
  }
}

std::any Typer::visit(ASTTuple *node) {
  std::vector<int> types;
  for (const auto &v : node->values) {
    types.push_back(int_from_any(v->accept(this)));
  }

  return node->type->resolved_type =
             ctx.scope->find_type_id(types, node->type->extension_info);
}

std::any Typer::visit(ASTTupleDeconstruction *node) {
  auto type = ctx.scope->get_type(int_from_any(node->right->accept(this)));

  if (!type->is_kind(TYPE_TUPLE)) {
    throw_error(
        "Cannot currently destruct a non-tuple. Coming soon for structs.",
        node->source_range);
  }

  auto info = static_cast<TupleTypeInfo *>(type->get_info());

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
      throw_error(
          std::format("Redefinition of a variable is not allowed in a tuple "
                      "deconstruction yet.\nOffending variable {}",
                      iden->value),
          node->source_range);
    }

    ctx.scope->insert(iden->value, type);
  }

  return {};
};
