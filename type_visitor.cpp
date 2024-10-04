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
#include <jstl/containers/vector.hpp>
#include <limits>
#include <ranges>
#include <string>
#include <vector>

void assert_types_can_cast_or_equal(
    const int from, const int to, const SourceRange &source_range,
    std::format_string<std::string, std::string> format, std::string message) {
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

const auto assert_return_type_is_valid(int &return_type, int new_type,
                                         ASTNode *node) {
  if (return_type == -1) {
    return_type = new_type;
  } else if (new_type != -1 && new_type != return_type) {
    assert_types_can_cast_or_equal(new_type, return_type, node->source_range,
                                "Expected: {}, Found: {}",
                                "Inconsistent return types in block.");
  }
};

static inline int int_from_any(const std::any &any) {
  return std::any_cast<int>(any);
}

// CLEANUP(Josh) 10/4/2024, 1:39:21 PM Wow this is an eyesore. There has to be a way to clean this dang thing up
// either returns the correct type that this init list will get casted to, or 
// throws an error.
// node->types_are_homogenous is pretty loose: it states that these types are either the same, or implicitly convertible to each other.
// that may not be enough to satisfy the C++ type system when it's strangely strict in some places more than others
int assert_type_can_be_assigned_from_init_list(ASTInitializerList *node, int declaring_type) {
  auto type = global_get_type(declaring_type);
  //!BUG This fails to accurately type check sub initializers. Also, we may want to implement sub initializers for struct types
  // and check those too
  if (node->types_are_homogenous && (type->get_ext().is_array() || type->get_ext().is_fixed_sized_array())) {
    for (const auto [i, expr] : node->expressions | std::ranges::views::enumerate) {
      if (auto sub_init = dynamic_cast<ASTInitializerList*>(expr)) {
        assert_type_can_be_assigned_from_init_list(sub_init, node->types[i]);
      }
    }
    return declaring_type;
  }
  if (!type->get_ext().has_no_extensions()) {
    throw_error("Unable to construct type from initializer list", node->source_range);
  }
  if (type->is_kind(TYPE_SCALAR)) {
    // this is just a plain scalar type, such as an int.
  } else if (type->is_kind(TYPE_STRUCT)) {
    auto info = static_cast<StructTypeInfo*>(type->get_info());
    if (info->scope->fields_count() < node->types.size()) {
      throw_error("excess elements provided in initializer list.", node->source_range);
    }
    // search for fields within the range of the types provided.
    int i = 0;
    for (const auto &[name, sym] : info->scope->symbols) {
      if (i >= node->types.size()) {
        break;
      }
      if (!sym.is_function()) {
        assert_types_can_cast_or_equal(node->types[i], sym.type_id, node->source_range, "{}, {}", "Invalid types in initializer list for struct");
      }
      i++;
    }
  } else if (type->is_kind(TYPE_UNION)) {
    auto info = static_cast<UnionTypeInfo*>(type->get_info());
    if (node->types.size() > 1) {
      throw_error("You can only initialize one field of a union with an initializer list", node->source_range);
    }
    // search for the first field member and type check against it.
    for (const auto &[name, sym] : info->scope->symbols) {
      if (!sym.is_function()) {
        assert_types_can_cast_or_equal(node->types[0], sym.type_id, node->source_range, "{}, {}", "Invalid types in initializer list for union");
        break;
      }
    }
  } else {
    throw_error("Unable to construct type from initializer list", node->source_range);
  }
  return declaring_type;
}

std::any TypeVisitor::visit(ASTType *node) {
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    node->pointing_to.get()->accept(this);
  }
  return node->resolved_type = global_find_type_id(node->base, node->extension_info);
}
std::any TypeVisitor::visit(ASTProgram *node) {
  for (auto &statement : node->statements) {
    statement->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTFunctionDeclaration *node) {

  node->return_type->accept(this);
  node->params->accept(this);

  FunctionTypeInfo info;

  info.return_type = node->return_type->resolved_type;
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

  auto type_id = global_find_function_type_id(
      global_get_function_typename(node), info, {});

  auto sym = ctx.scope->lookup(node->name.value);
  
  if (sym && ((node->flags & FUNCTION_IS_CTOR) == 0) && (node->flags & FUNCTION_IS_DTOR) == 0) {
    if (sym->function_overload_types.size() >= 1) sym->flags |= SYMBOL_HAS_OVERLOADS;
    for (const auto overload_type_id : sym->function_overload_types) {
      auto type = global_get_type(overload_type_id);
      
      auto this_type = global_get_type(type_id);
      auto visited = std::unordered_set<const Type*>();
      if (type->equals(this_type->get_base(), this_type->get_ext(), visited)) { 
        throw_error(std::format("re-definition of function '{}'", node->name.value),
                 {});
      }
    }
    sym->function_overload_types.push_back(type_id);
    sym->type_id = type_id;
  } else  {
    // always insert the first function declarations as the 0th overloaded type,
    // because we can tell when a fucntion has been overloaded when this array's size is > 1
    ctx.scope->insert(node->name.value, type_id, SYMBOL_IS_FUNCTION);
    auto sym = ctx.scope->lookup(node->name.value);
    sym->function_overload_types.push_back(type_id);
  }

  if (info.meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN) {
    return {};
  }

  auto control_flow =
      std::any_cast<ControlFlow>(node->block.get()->accept(this));
  if (control_flow.type == -1) {
    control_flow.type = void_type();
  }

  const auto is_ctor = (node->flags & FUNCTION_IS_CTOR) != 0,
             is_dtor = (node->flags & FUNCTION_IS_DTOR) != 0;

  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0) {
    throw_error("Keyword \"continue\" must be in a loop.",
                node->source_range);
  }

  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0) {
    throw_error("Keyword \"break\" must be in a loop.",
                node->source_range);
  }

  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 &&
      info.return_type != void_type() && !(is_ctor || is_dtor)) {
    throw_error("Not all code paths return a value.",
                node->source_range);
  }

  assert_types_can_cast_or_equal(control_flow.type, info.return_type,
                              node->source_range,
                              "invalid function return type: {} {}",
                              std::format("function: {}", node->name.value));
  return {};
}
std::any TypeVisitor::visit(ASTBlock *node) {
  ctx.set_scope(node->scope);
  ControlFlow block_cf = {BLOCK_FLAGS_FALL_THROUGH, -1};

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    if (dynamic_cast<ASTBlock *>(statement) ||
        dynamic_cast<ASTIf *>(statement) || dynamic_cast<ASTFor *>(statement) ||
        dynamic_cast<ASTWhile *>(statement) ||
        dynamic_cast<ASTReturn *>(statement) ||
        dynamic_cast<ASTContinue *>(statement) ||
        dynamic_cast<ASTBreak *>(statement)) {
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
std::any TypeVisitor::visit(ASTParamsDecl *node) {
  for (auto &param : node->params) {
    param->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTParamDecl *node) {
  auto id = int_from_any(node->type->accept(this));
  auto type = global_get_type(id);
  
  if (type->get_ext().is_fixed_sized_array()) {
    throw_warning("using a fixed array as a function parameter: note, this "
                  "casts the length information off and gets passed as as "
                  "pointer. Consider using a dynamic array",
                  node->source_range);
    if (node->default_value.is_not_null()) {
      throw_warning("Cannot currently use default parameters for fixed buffer "
                  "pointers. Also, length information gets casted off. "
                  "consider using a dynamic array", node->source_range);
    }

    // cast the fixed sized array to a base*.
    {
      auto extensions = type->get_ext();
      auto it = std::find(extensions.extensions.begin(),
                          extensions.extensions.end(), TYPE_EXT_ARRAY);
      extensions.extensions.erase(it);
      extensions.array_sizes.erase(extensions.array_sizes.begin() + 1);
      extensions.extensions.insert(extensions.extensions.begin(),
                                   TYPE_EXT_POINTER);
      node->type->resolved_type =
          global_find_type_id(type->get_base(), extensions);
    }
  }

  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    assert_types_can_cast_or_equal(
        expr_type, node->type->resolved_type, node->source_range,
        "invalid parameter declaration; expected: {} got: {}",
        std::format("parameter: {}", node->name));
  }
  return {};
}
std::any TypeVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);
  if (node->value.is_not_null()) {
    declaring_or_assigning_type = node->type->resolved_type;
    auto expr_type = int_from_any(node->value.get()->accept(this));
    assert_types_can_cast_or_equal(
        expr_type, node->type->resolved_type, node->source_range,
        "invalid declaration types. expected: {}, got {}",
        std::format("declaration: {}", node->name.value));
  }

  auto symbol = ctx.scope->lookup(node->name.value);
  symbol->type_id = node->type->resolved_type;
  
  if (symbol->type_id == void_type() || node->type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", node->name.value), node->source_range);
  }
  return {};
}
std::any TypeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  
  if (node->op.type == TType::Assign || node->op.type == TType::ColonEquals) 
    declaring_or_assigning_type = left;
  
  auto right = int_from_any(node->right->accept(this));
  auto type = global_get_type(left);
  
  // CLEANUP(Josh) 10/4/2024, 2:00:49 PM
  // We copy pasted this code like in 5 places, and a lot of the stuff is just identical.
  // TODO: this needs a really big rework.
  // TODO: we gotta do type checking on parameters.
  if (type && type->is_kind(TYPE_STRUCT) && type->get_ext().has_no_extensions()) {
    auto info = static_cast<StructTypeInfo*>(type->get_info());
    if (auto sym = info->scope->lookup(node->op.value)) {
      auto enclosing_scope = ctx.scope;
      ctx.set_scope(info->scope);
      Defer _([&](){
        ctx.set_scope(enclosing_scope);
      });
      if (sym->is_function()) {
        // TODO: fix this. we have ambiguitty with how we do this
        int t = -1;
        if (sym->function_overload_types[0] == -1) {
          t = sym->type_id;
        } else {
          t = sym->function_overload_types[0];
        }
        auto fun_ty = global_get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->get_info());
        auto param_0 = fun_info->parameter_types[0];
        assert_types_can_cast_or_equal(right, param_0, node->source_range, "expected, {}, got {}", "invalid call to operator overload");
        return fun_info->return_type;
      }
    }
  }

  // CLEANUP(Josh) 10/4/2024, 1:59:20 PM
  // These really shouldn't be a part of the expression hierarchy,
  // Same with assignment. Not only will this refuse to compile to C++,
  // it also makes 0 sense.
  
  // special case for type inferred declarations
  if (node->op.type == TType::ColonEquals) {
    left = right;
    
    if (right == void_type()) {
      throw_error("Cannot assign a variable of type 'void'", node->source_range);
    }
    
    if (auto iden = dynamic_cast<ASTIdentifier *>(node->left)) {
      ctx.scope->insert(iden->value.value, left);
    }
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have their
  // operands type checked, but right now that would involve casting scalars to
  // each other, which makes no  sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
    return bool_type();
  } else {
    auto left_t = global_get_type(left);
    auto right_t = global_get_type(right);
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
std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));

  if (node->op.type == TType::And) {
    auto ty = global_get_type(operand_ty);
    auto extensions = ty->get_ext();
    extensions.extensions.push_back(TYPE_EXT_POINTER);
    return global_find_type_id(ty->get_base(), extensions);
  }

  if (node->op.type == TType::Mul) {
    return remove_one_pointer_ext(operand_ty, node->source_range);
  }


  auto left_ty = global_get_type(operand_ty);
  if (left_ty && left_ty->is_kind(TYPE_STRUCT) && left_ty->get_ext().has_no_extensions()) {
    auto info = static_cast<StructTypeInfo*>(left_ty->get_info());
    if (auto sym = info->scope->lookup(node->op.value)) {
      auto enclosing_scope = ctx.scope;
      ctx.set_scope(info->scope);
      Defer _([&](){
        ctx.set_scope(enclosing_scope);
      });
      if (sym->is_function()) {
        // TODO: fix this. we have ambiguitty with how we do this
        int t = -1;
        if (sym->function_overload_types[0] == -1) {
          t = sym->type_id;
        } else {
          t = sym->function_overload_types[0];
        }
        auto fun_ty = global_get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->get_info());
        return fun_info->return_type;
      }
    } else throw_error(std::format("couldn't find {} overload for struct type", node->op.value), node->source_range);
  }

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule =
        type_conversion_rule(global_get_type(operand_ty),
                             global_get_type(bool_type()));
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED &&
                        conversion_rule != CONVERT_EXPLICIT);

    if (node->op.type == TType::Not && can_convert) {
      return bool_type();
    }
  }

  return operand_ty;
}
std::any TypeVisitor::visit(ASTIdentifier *node) {
  auto symbol = ctx.scope->lookup(node->value.value);
  if (symbol) {
    // if ((symbol->flags & SYMBOL_HAS_OVERLOADS) != 0) {
    //   throw_warning()
    // }
    return symbol->type_id;
  }
  else {
    throw_error(
        std::format("Use of undeclared identifier '{}'", node->value.value),
         node->source_range);
  }
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
  case ASTLiteral::Integer: {
    // !BUG: this still seems to not always return the correct values.
    // Although almost always it is correct, and maybe it is always correct.
    int base = 10;
    if (node->value.starts_with("0x")) {
      base = 0;
    } 
    if (node->value.starts_with("0b")) {
      node->value = node->value.substr(2, node->value.length());
      base = 2;
    }
    auto n = std::strtoll(node->value.c_str(), nullptr, base);
    if (n > std::numeric_limits<int32_t>::max() ||
        n < std::numeric_limits<int32_t>::min()) {
      return s64_type();
    }
    if (n > std::numeric_limits<int16_t>::max() ||
        n < std::numeric_limits<int16_t>::min()) {
      return s32_type();
    }
    if (n > std::numeric_limits<int8_t>::max() ||
        n < std::numeric_limits<int8_t>::min()) {
      return s16_type();
    }
    return s8_type();
  }
  case ASTLiteral::Float:
    return float32_type();
  case ASTLiteral::RawString:
  case ASTLiteral::String:
    return charptr_type();
    break;
  case ASTLiteral::Bool:
    return bool_type();
  case ASTLiteral::Null:
    return voidptr_type();
  case ASTLiteral::InterpolatedString: {
    return global_find_type_id("string", {});
  }
  case ASTLiteral::Char:
    return char_type();
    break;
  }
}
std::any TypeVisitor::visit(ASTCall *node) {
  auto symbol = ctx.scope->lookup(node->name.value);

  if (!symbol) {
    throw_error(std::format("Use of undeclared symbol '{}'", node->name.value),
                 node->source_range);
  }

  std::vector<int> arg_tys =
      std::any_cast<std::vector<int>>(node->arguments->accept(this));
  
  Type *type = global_get_type(symbol->type_id);
  
  // TODO: again, we probably don't need functor objects, theyre fancy and unneccesary.
  // Perform call to operator overload for ();
  if (type->is_kind(TYPE_STRUCT)) {
    auto info = static_cast<StructTypeInfo*>(type->get_info());
    if (auto sym = info->scope->lookup("(")) {
      auto enclosing_scope = ctx.scope;
      ctx.set_scope(info->scope);
      Defer _([&](){
        ctx.set_scope(enclosing_scope);
      });
      if (sym->is_function()) {
        // TODO: fix this. we have ambiguitty with how we do this
        int t = -1;
        if (sym->function_overload_types[0] == -1) {
          t = sym->type_id;
        } else {
          t = sym->function_overload_types[0];
        }
        auto fun_ty = global_get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->get_info());
        if (fun_info->params_len != arg_tys.size())
          throw_error("Invalid number of arguments for call operator overload", node->source_range);
        for (int i = 0; i < fun_info->params_len; ++i) {
          assert_types_can_cast_or_equal(arg_tys[i], fun_info->parameter_types[i], node->source_range, "expected: {}, got: {}", "invalid parameter type in call operator overload");
        }
        return fun_info->return_type;
      }
    } else throw_error("couldn't find call '()' overload for struct type", node->source_range);
  }
  
  // Find a suitable function overload to call.
  if ((symbol->flags & SYMBOL_HAS_OVERLOADS) != 0) {
    bool found_exact_match = false;
    int exact_match_idx = -1;
    
    bool found_implicit_match = false;
    int implicit_match_idx = -1;
    
    // CLEANUP: fix this, enumerate is slow as balls.
    for (const auto &[i, overload]: symbol->function_overload_types | std::ranges::views::enumerate) {
      auto name = node->name.value;
      auto ovrld_ty = global_get_type(overload);
      auto info = static_cast<FunctionTypeInfo*>(ovrld_ty->get_info());
      for (int j = 0; j < info->params_len; ++j) {
        if (j >= arg_tys.size() && !(info->is_varargs || info->default_params > 0)) goto didnt_match;
        auto conversion_rule = type_conversion_rule(global_get_type(arg_tys[j]), global_get_type(info->parameter_types[j]));
        if (conversion_rule == CONVERT_EXPLICIT && !(info->is_varargs || info->default_params > 0)) goto didnt_match;
        if (conversion_rule == CONVERT_IMPLICIT && !(info->is_varargs || info->default_params > 0)) {
          found_implicit_match = true;
          implicit_match_idx = i;
        } else if (conversion_rule != CONVERT_NONE_NEEDED) goto didnt_match;
      }
      found_exact_match = true;
      exact_match_idx = i;
      break;
      didnt_match:
    }
    if (!found_exact_match && !found_implicit_match) {
      std::vector<std::string> names;
      for (auto n : arg_tys) {
        // Here we use global get type just because we're dumping an error and it doesn't matter.
        names.push_back(global_get_type(n)->to_string());
      }
      throw_error(std::format("No function overload for provided argument signature found.. got : {}", names), node->source_range);
    }
    if (found_exact_match) {
      type = global_get_type(symbol->function_overload_types[exact_match_idx]);
      assert(type != nullptr);
    } else {
      type = global_get_type(symbol->function_overload_types[implicit_match_idx]);
      assert(type != nullptr);
    }
  } else {
    type = global_get_type(symbol->type_id);
  }
  
  
  auto fn_ty_info = dynamic_cast<FunctionTypeInfo *>(type->get_info());

  // FEATURE(Josh) 10/1/2024, 8:46:53 AM We should be able to call constructors
  // without this function syntax, using #make(Type, ...) is really clunky
  // and annoying;

  if (!type || !fn_ty_info) {
    throw_error("Unable to call function: {} did not refer to a function typed "
                "variable. Constructors currently use #make(Type, ...) syntax.",
                 node->source_range);
  }

  auto info = dynamic_cast<const FunctionTypeInfo *>(fn_ty_info);

  if (!info->is_varargs &&
      (arg_tys.size() > info->params_len ||
       arg_tys.size() < info->params_len - info->default_params)) {
    throw_error(
        std::format("Function call '{}' has incorrect number of arguments. "
                    "Expected: {}, Found: {}",
                    node->name.value, info->params_len, arg_tys.size()),
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
        std::format("parameter: {} of function: {}", i, node->name.value));
  }

  node->type = info->return_type;
  return info->return_type;
}
std::any TypeVisitor::visit(ASTArguments *node) {
  std::vector<int> argument_types;
  for (auto arg : node->arguments) {
    // TODO: We need to have a way to get the expected types of the parameters of the function we're visiting for here 
    // so that we can implicitly convert initializer lists to the correct type here
    argument_types.push_back(int_from_any(arg->accept(this)));
  }
  return argument_types;
}
std::any TypeVisitor::visit(ASTReturn *node) {
  int type;
  if (node->expression.is_not_null()) {
    // TODO: we should know the return type of the function we're visiting here so we can coerce initializer lists
    type = int_from_any(node->expression.get()->accept(this));
  } else {
    type = global_find_type_id("void", {});
  }
  return ControlFlow{BLOCK_FLAGS_RETURN, type};
}
std::any TypeVisitor::visit(ASTContinue *node) {
  return ControlFlow{BLOCK_FLAGS_CONTINUE, -1};
}
std::any TypeVisitor::visit(ASTBreak *node) {
  return ControlFlow{BLOCK_FLAGS_BREAK, -1};
}
std::any TypeVisitor::visit(ASTFor *node) {
  ctx.set_scope(node->block->scope);
  switch (node->tag) {
  case ASTFor::RangeBased: {
    auto v = node->value.range_based;
    auto type = int_from_any(v.collection->accept(this));

    auto t = global_get_type(type);
    
    auto iden = static_cast<ASTIdentifier *>(v.target);

    if (!t) {
      throw_error("Internal compiler error: element type was null in range based for loop", node->source_range);
    }

    int iter_ty = -1;
    
    if (t->is_kind(TYPE_STRUCT) &&
        (!t->get_ext().is_array() && !t->get_ext().is_fixed_sized_array())) {
        auto info = dynamic_cast<StructTypeInfo *>(t->get_info());
      // TODO: add a way to use the value_semantic thing with custom
      // iterators.
      Symbol *begin = info->scope->lookup("begin");
      Symbol *end = info->scope->lookup("end");
      if (begin && end && begin->type_id == end->type_id) {
        iter_ty = begin->type_id;
      } else {
        throw_error("Can only iterate over structs you define 'begin' and "
                    "'end' on. They must both be defined, and must both "
                    "return the same type.",
                     node->source_range);
      }
    } else if (!t->get_ext().is_array() &&
               !t->get_ext().is_fixed_sized_array()) {
      throw_error("cannot iterate with a range based for loop over a non "
                  "collection type.",
                   node->source_range);
    } else {
      iter_ty = t->get_element_type();
    }

    // Take a pointer to the type.
    // This probably won't work well with custom iterators.
    if (v.value_semantic == VALUE_SEMANTIC_POINTER) {
      auto type = global_get_type(iter_ty);
      auto ext = type->get_ext();
      ext.extensions.push_back(TYPE_EXT_POINTER);
      iter_ty = global_find_type_id(type->get_base(), ext);
    }

    ctx.scope->insert(iden->value.value, iter_ty);
    v.target->accept(this);
  } break;
  case ASTFor::CStyle: {
    auto v = node->value.c_style;
    v.decl->accept(this);
    v.condition->accept(this);
    v.increment->accept(this);
  } break;
  }
  ctx.exit_scope();
  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  // we add fall through here because we dont know if this will get
  // excecuted since we cant evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}
std::any TypeVisitor::visit(ASTIf *node) {
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
std::any TypeVisitor::visit(ASTElse *node) {
  if (node->_if.is_not_null()) {
    return node->_if.get()->accept(this);
  } else {
    return node->block.get()->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTWhile *node) {

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
std::any TypeVisitor::visit(ASTStructDeclaration *node) {
  auto type = global_get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->get_info());
  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0) {
    return {};
  }
  ctx.set_scope(node->scope);
  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method : node->methods) {
    method->accept(this);
  }

  ctx.exit_scope();
  return {};
}
std::any TypeVisitor::visit(ASTUnionDeclaration *node) {
  
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
std::any TypeVisitor::visit(ASTDotExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto left_ty = global_get_type(left);

  if (!left_ty) {
    throw_error("Internal Compiler Error: un-typed variable on lhs of dot "
                "expression?",
                 node->source_range);
  }

  // TODO: remove this hack to get array length
  if (left_ty->get_ext().is_array()) {
    auto right = dynamic_cast<ASTIdentifier *>(node->right);
    if (right && right->value.value == "length") {
      return s32_type();
    }
    if (right && right->value.value == "data") {
      return get_pointer_to_type(left_ty->get_element_type());
    }
  }
  
  // Get enum variant
  if (left_ty->is_kind(TYPE_ENUM)) {
    auto info = static_cast<EnumTypeInfo *>(left_ty->get_info());
    auto iden = dynamic_cast<ASTIdentifier *>(node->right);
    if (!iden) {
      throw_error("cannot use a dot expression with a non identifer on the "
                  "right hand side when referring to a enum.",
                   node->source_range);
    }
    auto name = iden->value.value;
    bool found = false;
    for (const auto &key : info->keys) {
      if (name == key) {
        found = true;
        break;
      }
    }
    if (!found) {
      throw_error("failed to find key in enum type.",
                  node->source_range);
    }
    
    // TODO: put the element_type from the ASTEnumDeclaration into the
    // type info so that we can return that instead of assuming its s32.
    // that would help us be safer about typing.
    return s32_type();
  }

  if (left_ty->kind != TYPE_STRUCT && left_ty->kind != TYPE_UNION) {
    throw_error(
        std::format("cannot use dot expr on non-struct currently, got {}",
                    left_ty->to_string()),
         node->source_range);
  }

  Scope *scope;
  if (auto info = dynamic_cast<StructTypeInfo *>(left_ty->get_info())) {
    scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo *>(left_ty->get_info())) {
    scope = info->scope;
  }

  auto previous_scope = ctx.scope;
  auto prev_parent = scope->parent;
  if (prev_parent && !previous_scope->is_struct_or_union_scope) {
    scope->parent = previous_scope;
  }

  // TODO: see above.
  ctx.set_scope(scope);
  int type = int_from_any(node->right->accept(this));
  ctx.set_scope(previous_scope);

  if (prev_parent && !previous_scope->is_struct_or_union_scope) {
    scope->parent = prev_parent;
  }
  return type;
  throw_error("unable to resolve dot expression type.",
              node->source_range);
}
std::any TypeVisitor::visit(ASTSubscript *node) {
  auto left = int_from_any(node->left->accept(this));
  auto subscript = int_from_any(node->subscript->accept(this));
  auto left_ty = global_get_type(left);

  /// CLEANUP(Josh) 10/4/2024, 2:18:42 PM
  // delete the subscript operator, call operator, and various other operators we may not want in the languaeg.
  // We want to keep it simple, and having 100-200 lines of code dedicated to things that are never used is not conducive to that prospect.
  if (left_ty && left_ty->is_kind(TYPE_STRUCT) &&
      left_ty->get_ext().has_no_extensions()) {
    auto info = static_cast<StructTypeInfo *>(left_ty->get_info());
    if (auto sym = info->scope->lookup("[")) {
      auto enclosing_scope = ctx.scope;
      ctx.set_scope(info->scope);
      Defer _([&](){
        ctx.set_scope(enclosing_scope);
      });
      if (sym->is_function()) {
        // TODO: fix this. we have ambiguitty with how we do this
        int t = -1;
        if (sym->function_overload_types[0] == -1) {
          t = sym->type_id;
        } else {
          t = sym->function_overload_types[0];
        }
        auto fun_ty = global_get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo *>(fun_ty->get_info());
        auto param_0 = fun_info->parameter_types[0];
        assert_types_can_cast_or_equal(
            subscript, fun_info->parameter_types[0], node->source_range,
            "expected: {}, got: {}",
            "invalid parameter type in subscript operator overload");
        return fun_info->return_type;
      }
    } else
      throw_error("couldn't find [] overload for struct type",
                  node->source_range);
  } 

  if (!left_ty->get_ext().is_array() && !left_ty->get_ext().is_pointer()) {
    throw_error(std::format("cannot index into non array type. {}",
                            left_ty->to_string()),
                 node->source_range);
  }

  if (left_ty->get_ext().is_array()) {
    auto element_id = left_ty->get_element_type();
    return element_id;
  } else {
    return remove_one_pointer_ext(left_ty->id, node->source_range);
  }
}
std::any TypeVisitor::visit(ASTMake *node) {
  auto type = int_from_any(node->type_arg->accept(this));
  if (!node->arguments->arguments.empty()) {
    node->arguments->accept(this);
  }
  if (type == -1) {
    throw_error("Cannot make non existent type",
                node->source_range);
  }
  return type;
}
std::any TypeVisitor::visit(ASTInitializerList *node) {
  
  int last_type = -1;
  for (const auto &expr : node->expressions)  {
    int type = int_from_any(expr->accept(this));
    
    if (last_type == -1) {
      last_type = type;
    } else if (last_type != type) {
      auto rule =  type_conversion_rule(global_get_type(type), global_get_type(last_type));
      if (rule == CONVERT_PROHIBITED || rule == CONVERT_EXPLICIT) {
        node->types_are_homogenous = false;
      }
    }
    node->types.push_back(type);
  }
  
  // TODO: assert that type can be default constructed.
  // for now this is just an error
  if (node->types.empty()) {
    throw_error("Cannot have an empty initializer list currently. to be "
                "implemented.",
                 node->source_range);
  }
  return assert_type_can_be_assigned_from_init_list(node, declaring_or_assigning_type);
}
std::any TypeVisitor::visit(ASTEnumDeclaration *node) {
  int largest_type = s32_type();
  int largest_type_size = 1;
  for (const auto &[key, value] : node->key_values) {
    if (value.is_not_null()) {
      if (node->is_flags) {
        throw_error("You shouldn't use a #flags enum to generate auto "
                    "flags, and also use non-default values.",
                     node->source_range);
      }
      auto expr = value.get();
      auto id = int_from_any(value.get()->accept(this));
      
      auto type = global_get_type(id);
      
      if (!type->is_kind(TYPE_SCALAR) || !type->get_ext().has_no_extensions()) {
        throw_error("Cannot have non integral types in enums got: " + type->to_string(), node->source_range);
      }
      
      auto info = static_cast<ScalarTypeInfo*>(type->get_info());
      
      if (info->size > largest_type_size) {
        largest_type = id;
        largest_type_size = info->size;
      }
      
      assert_types_can_cast_or_equal(id, s64_type(), node->source_range,
                                  "expected: {}, got : {}",
                                  "Cannot have non-integral types in enums");
    }
  }
  node->element_type = largest_type;
  return {};
}
std::any TypeVisitor::visit(ASTAllocate *node) {
  // TODO(Josh) 10/1/2024, 3:27:53 PM
  // Do something here. This is probably bad,
  // but this shouldn't be used as an expression really.
  if (node->kind == ASTAllocate::Delete) {
    if (node->arguments.is_null() ||
        node->arguments.get()->arguments.size() < 1) {
      throw_error("invalid delete statement: you need at least one argument",
                   node->source_range);
    }
    // TODO(Josh) 10/1/2024, 4:40:04 PM This won't quite work, as the
    // allocations aren't directly tied to the delete. We try to delete the
    // delete node, not the alloc node. we need to somehow lookup the source
    // scope and variable, and then erase that. erase_allocation(node);
    return void_type();
  }

  auto type = int_from_any(node->type.get()->accept(this));
  // just type check them, no need to return
  // we should probably type check parameters for a constructor
  // but we need a seperate system for that
  if (node->arguments)
    node->arguments.get()->accept(this);

  auto t = global_get_type(type);
  return node->type.get()->resolved_type = t->id;
}