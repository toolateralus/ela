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

void validate_type_compatability(
    const int from, const int to, const SourceRange &source_range,
    std::format_string<std::string, std::string> format, std::string message) {
  auto from_t = global_get_type(from);
  auto to_t = global_get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t);
  if (to != from &&
      (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' +
                    std::format(format, to_t->to_string(), from_t->to_string()),
                ERROR_FAILURE, source_range);
  }
}
const auto check_return_type_consistency(int &return_type, int new_type,
                                         ASTNode *node) {
  if (return_type == -1) {
    return_type = new_type;
  } else if (new_type != -1 && new_type != return_type) {
    validate_type_compatability(new_type, return_type, node->source_range,
                                "Expected: {}, Found: {}",
                                "Inconsistent return types in block.");
  }
};

static inline int int_from_any(const std::any &any) {
  return std::any_cast<int>(any);
}

std::any TypeVisitor::visit(ASTType *node) {
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    node->pointing_to.get()->accept(this);
  }
  return node->resolved_type = ctx.scope->find_type_id(
             node->base, node->extension_info);
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

  auto type_id = ctx.scope->find_function_type_id(
      ctx.scope->get_function_typename(node), info, {});

  auto sym = ctx.scope->lookup(node->name.value);
  
  // TODO: don't ignore constructors and destructors.
  if (sym && ((node->flags & FUNCTION_IS_CTOR) == 0) && (node->flags & FUNCTION_IS_DTOR) == 0) {
    if (sym->function_overload_types.size() >= 1) sym->flags |= SYMBOL_HAS_OVERLOADS;
    for (const auto overload_type_id : sym->function_overload_types) {
      auto type = ctx.scope->get_type(overload_type_id);
      
      auto this_type = ctx.scope->get_type(type_id);
      // TODO: verify that we even want to use this function.
      // It might not do the fine grained equality check that we need on the parameter and return types.
      // Maybe it will sinec that info is encoded into function type name.s
      if (type->equals(this_type->base, this_type->extensions)) { 
        throw_error(std::format("re-definition of function '{}'", node->name.value),
                ERROR_FAILURE, {});
      }
    }
    sym->function_overload_types.push_back(type_id);
    sym->type_id = type_id;
  } else  {
    // TODO(Josh) 10/3/2024, 10:36:56 AM
    // Fix this jank
    
    // insert function
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
    throw_error("Keyword \"continue\" must be in a loop.", ERROR_FAILURE,
                node->source_range);
  }

  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0) {
    throw_error("Keyword \"break\" must be in a loop.", ERROR_FAILURE,
                node->source_range);
  }

  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 &&
      info.return_type != void_type() && !(is_ctor || is_dtor)) {
    throw_error("Not all code paths return a value.", ERROR_FAILURE,
                node->source_range);
  }

  validate_type_compatability(control_flow.type, info.return_type,
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
        check_return_type_consistency(block_cf.type, stmnt_cf.type, node);
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
  auto type = ctx.scope->get_type(id);
  
  if (type->extensions.is_fixed_sized_array()) {
    throw_warning("using a fixed array as a function parameter: note, this "
                  "casts the length information off and gets passed as as "
                  "pointer. Consider using a dynamic array",
                  node->source_range);
    if (node->default_value.is_not_null()) {
      throw_error("Cannot currently use default parameters for fixed buffer "
                  "pointers. Also, length information gets casted off. "
                  "consider using a dynamic array",
                  ERROR_WARNING, node->source_range);
    }

    // cast the fixed sized array to a base*.
    {
      auto extensions = type->extensions;
      auto it = std::find(extensions.extensions.begin(),
                          extensions.extensions.end(), TYPE_EXT_ARRAY);
      extensions.extensions.erase(it);
      extensions.array_sizes.erase(extensions.array_sizes.begin() + 1);
      extensions.extensions.insert(extensions.extensions.begin(),
                                   TYPE_EXT_POINTER);
      node->type->resolved_type =
          ctx.scope->find_type_id(type->base, extensions);
    }
  }

  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    validate_type_compatability(
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
    validate_type_compatability(
        expr_type, node->type->resolved_type, node->source_range,
        "invalid declaration types. expected: {}, got {}",
        std::format("declaration: {}", node->name.value));
  }

  auto symbol = ctx.scope->lookup(node->name.value);
  symbol->type_id = node->type->resolved_type;
  
  if (symbol->type_id == void_type() || node->type->resolved_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", node->name.value), ERROR_FAILURE, node->source_range);
  }
  
  return {};
}
std::any TypeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  
  if (node->op.type == TType::Assign || node->op.type == TType::ColonEquals) declaring_or_assigning_type = left;
  
  auto right = int_from_any(node->right->accept(this));
  auto type = ctx.scope->get_type(left);
  
  // TODO: this needs a really big rework.
  // TODO: we gotta do type checking on parameters.
  if (type && type->is_kind(TYPE_STRUCT) && type->extensions.has_no_extensions()) {
    auto info = static_cast<StructTypeInfo*>(type->info);
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
        auto fun_ty = ctx.scope->get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->info);
        auto param_0 = fun_info->parameter_types[0];
        validate_type_compatability(right, param_0, node->source_range, "expected, {}, got {}", "invalid call to operator overload");
        return fun_info->return_type;
      }
    }
  }

  // special case for type inferred declarations
  if (node->op.type == TType::ColonEquals) {
    left = right;
    
    if (right == void_type()) {
      throw_error("Cannot assign a variable of type 'void'", ERROR_FAILURE, node->source_range);
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
    auto left_t = ctx.scope->get_type(left);
    auto right_t = ctx.scope->get_type(right);
    auto conv_rule_0 = type_conversion_rule(left_t, right_t);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t);
    // TODO(Josh) 10/1/2024, 3:07:47 PM
    // validate that this is what we want. Before, it was too strict, now it
    // feels like its too loose. also, we should probably do specific type
    // checking based on the operator, we still need some sort of table.
    if (((conv_rule_0 == CONVERT_PROHIBITED) &&
         (conv_rule_1 == CONVERT_PROHIBITED)) ||
        ((conv_rule_0 == CONVERT_EXPLICIT) &&
         (conv_rule_1 == CONVERT_EXPLICIT))) {
      throw_error(std::format("Type error in binary expression: cannot convert "
                              "between {} and {}",
                              left_t->to_string(), right_t->to_string()),
                  ERROR_FAILURE, node->source_range);
    }
  }

  node->resolved_type = left;
  return left;
}
std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));

  if (node->op.type == TType::And) {
    auto ty = ctx.scope->get_type(operand_ty);
    auto extensions = ty->extensions;
    extensions.extensions.push_back(TYPE_EXT_POINTER);
    return ctx.scope->find_type_id(ty->base, extensions);
  }

  if (node->op.type == TType::Mul) {
    return remove_one_pointer_ext(operand_ty, node->source_range);
  }


  auto left_ty = ctx.scope->get_type(operand_ty);
  if (left_ty && left_ty->is_kind(TYPE_STRUCT) && left_ty->extensions.has_no_extensions()) {
    auto info = static_cast<StructTypeInfo*>(left_ty->info);
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
        auto fun_ty = ctx.scope->get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->info);
        return fun_info->return_type;
      }
    } else throw_error(std::format("couldn't find {} overload for struct type", node->op.value), ERROR_FAILURE, node->source_range);
  }

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule =
        type_conversion_rule(ctx.scope->get_type(operand_ty),
                             ctx.scope->get_type(bool_type()));
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
        ERROR_FAILURE, node->source_range);
  }
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
  case ASTLiteral::Integer: {
    // TODO: this still seems to not always return the correct values.
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
  }
}
std::any TypeVisitor::visit(ASTCall *node) {
  auto symbol = ctx.scope->lookup(node->name.value);

  if (!symbol) {
    throw_error(std::format("Use of undeclared symbol '{}'", node->name.value),
                ERROR_FAILURE, node->source_range);
  }

  std::vector<int> arg_tys =
      std::any_cast<std::vector<int>>(node->arguments->accept(this));
  
  Type *type = ctx.scope->get_type(symbol->type_id);
  
  // TODO: again, we probably don't need functor objects, theyre fancy and unneccesary.
  // Perform call to operator overload for ();
  if (type->is_kind(TYPE_STRUCT)) {
    auto info = static_cast<StructTypeInfo*>(type->info);
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
        auto fun_ty = ctx.scope->get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo*>(fun_ty->info);
        if (fun_info->params_len != arg_tys.size())
          throw_error("Invalid number of arguments for call operator overload", ERROR_FAILURE, node->source_range);
        for (int i = 0; i < fun_info->params_len; ++i) {
          validate_type_compatability(arg_tys[i], fun_info->parameter_types[i], node->source_range, "expected: {}, got: {}", "invalid parameter type in call operator overload");
        }
        return fun_info->return_type;
      }
    } else throw_error("couldn't find call '()' overload for struct type", ERROR_FAILURE, node->source_range);
  }
  
  // Find a suitable function overload to call.
  if ((symbol->flags & SYMBOL_HAS_OVERLOADS) != 0) {
    bool found_exact_match = false;
    int exact_match_idx = -1;
    
    bool found_implicit_match = false;
    int implicit_match_idx = -1;
    
    // todo: fix this, enumerate is slow as balls.
    for (const auto &[i, overload]: symbol->function_overload_types | std::ranges::views::enumerate) {
      auto name = node->name.value;
      auto ovrld_ty = ctx.scope->get_type(overload);
      auto info = static_cast<FunctionTypeInfo*>(ovrld_ty->info);
      for (int j = 0; j < info->params_len; ++j) {
        // TODO: probably could match these better than just comparing typeids.
        // such as allowing implicit conversions to still take place.
        if (j >= arg_tys.size() && !(info->is_varargs || info->default_params > 0)) goto didnt_match;
        auto conversion_rule = type_conversion_rule(ctx.scope->get_type(arg_tys[j]), ctx.scope->get_type(info->parameter_types[j]));
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
      throw_error(std::format("No function overload for provided argument signature found.. got : {}", names), ERROR_FAILURE, node->source_range);
    }
    if (found_exact_match) {
      type = ctx.scope->get_type(symbol->function_overload_types[exact_match_idx]);
      assert(type != nullptr);
    } else {
      type = ctx.scope->get_type(symbol->function_overload_types[implicit_match_idx]);
      assert(type != nullptr);
    }
  } else {
    type = ctx.scope->get_type(symbol->type_id);
  }
  
  
  auto fn_ty_info = dynamic_cast<FunctionTypeInfo *>(type->info);

  // TODO(Josh) 10/1/2024, 8:46:53 AM We should be able to call constructors
  // without this function syntax, using #make(Type, ...) is really clunky
  // and annoying;

  if (!type || !fn_ty_info) {
    throw_error("Unable to call function: {} did not refer to a function typed "
                "variable. Constructors currently use #make(Type, ...) syntax.",
                ERROR_FAILURE, node->source_range);
  }

  auto info = dynamic_cast<const FunctionTypeInfo *>(fn_ty_info);

  if (!info->is_varargs &&
      (arg_tys.size() > info->params_len ||
       arg_tys.size() < info->params_len - info->default_params)) {
    throw_error(
        std::format("Function call '{}' has incorrect number of arguments. "
                    "Expected: {}, Found: {}",
                    node->name.value, info->params_len, arg_tys.size()),
        ERROR_FAILURE, node->source_range);
  }

  for (int i = 0; i < info->params_len; ++i) {

    // !BUG: default parameters evade type checking
    if (arg_tys.size() <= i) {
      continue;
    }

    validate_type_compatability(
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
    // TODO: We need to have a way to get the expected types of the parameters of the function we're visiting for here  so that we can implicitly convert initializer lists to the correct type here
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
    type = ctx.scope->find_type_id("void", {});
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
    
    // !BUG  For some reason this is returning null for float *;
    // if we just added extensions to a type, and it already exists in the root scope,
    // that newly created type should get propogated to the root scope
    
    // NOTE: i did that, and i STILL cannot get the type from the enclosing scope aka ctx.scope.
    // Right now im just using global_get_type, but this should absolutely not have to be the case
    // and this must be fixed.
    
    // !BUG REMOVE THIS
    auto t = global_get_type(type);
    
    // !BUG THIS MUST BE UNCOMMENTED ONCE WE FIND THE SOURCE OF THE BUG
    // auto t = ctx.scope->get_type(type);
    
    auto iden = static_cast<ASTIdentifier *>(v.target);

    if (!t) {
      throw_error("Internal compiler error: element type was null in range based for loop", ERROR_CRITICAL, node->source_range);
    }

    int iter_ty = -1;
    
    if (t->is_kind(TYPE_STRUCT) &&
        (!t->extensions.is_array() && !t->extensions.is_fixed_sized_array())) {
        auto info = dynamic_cast<StructTypeInfo *>(t->info);
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
                    ERROR_FAILURE, node->source_range);
      }
    } else if (!t->extensions.is_array() &&
               !t->extensions.is_fixed_sized_array()) {
      throw_error("cannot iterate with a range based for loop over a non "
                  "collection type.",
                  ERROR_FAILURE, node->source_range);
    } else {
      iter_ty = t->get_element_type();
    }

    // Take a pointer to the type.
    // This probably won't work well with custom iterators.
    if (v.value_semantic == VALUE_SEMANTIC_POINTER) {
      //! ALONG WITH ALL THE OTHER GLOBAL TYPE BUGS, THIS ONE WILL BE SOLVED. ONCE WE FIND A SOLUTION TO THER ROOT OF THE PROBLEM FOR WHICH IS UNKNOWN
      auto type = global_get_type(iter_ty);
      auto ext = type->extensions;
      ext.extensions.push_back(TYPE_EXT_POINTER);
      iter_ty = ctx.scope->find_type_id(type->base, ext);
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
  validate_type_compatability(
      cond_ty, bool_type(), node->source_range, "expected: {}, got {}",
      "if statement condition was not convertible to boolean");

  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  if (node->_else.is_not_null()) {
    auto _else = node->_else.get();
    auto else_cf = std::any_cast<ControlFlow>(_else->accept(this));
    control_flow.flags |= else_cf.flags;
    if ((else_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      check_return_type_consistency(control_flow.type, else_cf.type, node);
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
  auto type = ctx.scope->get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->info);
  info->scope = node->scope;
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
std::any TypeVisitor::visit(ASTDotExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto left_ty = ctx.scope->get_type(left);

  if (!left_ty) {
    throw_error("Internal Compiler Error: un-typed variable on lhs of dot "
                "expression?",
                ERROR_FAILURE, node->source_range);
  }

  // TODO: remove this hack to get array length
  if (left_ty->extensions.is_array()) {
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
    auto info = static_cast<EnumTypeInfo *>(left_ty->info);
    auto iden = dynamic_cast<ASTIdentifier *>(node->right);
    if (!iden) {
      throw_error("cannot use a dot expression with a non identifer on the "
                  "right hand side when referring to a enum.",
                  ERROR_FAILURE, node->source_range);
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
      throw_error("failed to find key in enum type.", ERROR_FAILURE,
                  node->source_range);
    }
    // TODO(Josh) Add a way to support more than just s32 types from enums.
    // Ideally, we could even use const char* etc. 9/30/2024, 11:53:45 AM
    return s32_type();
  }

  if (left_ty->kind != TYPE_STRUCT && left_ty->kind != TYPE_UNION) {
    throw_error(
        std::format("cannot use dot expr on non-struct currently, got {}",
                    left_ty->to_string()),
        ERROR_FAILURE, node->source_range);
  }

  Scope *scope;
  if (auto info = dynamic_cast<StructTypeInfo *>(left_ty->info)) {
    scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo *>(left_ty->info)) {
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
  throw_error("unable to resolve dot expression type.", ERROR_FAILURE,
              node->source_range);
}
std::any TypeVisitor::visit(ASTSubscript *node) {
  auto left = int_from_any(node->left->accept(this));
  auto subscript = int_from_any(node->subscript->accept(this));
  auto left_ty = ctx.scope->get_type(left);

  // TODO: determine if we even want operator overloading for subscript.
  // It seems to have highlighted type system issues, though so we can at least use it to debug why a dot expression
  // seems to propogate the root type to the right

  // TODO: this should be improved to handle [0,1,2,3] and [0..10];
  // Perform call to operator overload for [];
  if (left_ty && left_ty->is_kind(TYPE_STRUCT) &&
      left_ty->extensions.has_no_extensions()) {
    auto info = static_cast<StructTypeInfo *>(left_ty->info);
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
        auto fun_ty = ctx.scope->get_type(t);
        auto fun_info = static_cast<FunctionTypeInfo *>(fun_ty->info);
        auto param_0 = fun_info->parameter_types[0];
        validate_type_compatability(
            subscript, fun_info->parameter_types[0], node->source_range,
            "expected: {}, got: {}",
            "invalid parameter type in subscript operator overload");
        return fun_info->return_type;
      }
    } else
      throw_error("couldn't find [] overload for struct type", ERROR_FAILURE,
                  node->source_range);
  } 

  if (!left_ty->extensions.is_array() && !left_ty->extensions.is_pointer()) {
    throw_error(std::format("cannot index into non array type. {}",
                            left_ty->to_string()),
                ERROR_FAILURE, node->source_range);
  }

  if (left_ty->extensions.is_array()) {
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
    throw_error("Cannot make non existent type", ERROR_FAILURE,
                node->source_range);
  }
  return type;
}

// TODO: make it so initializer lists can contain more than one type,
// so we can use it for complex initialization of structs without constructors.
// also maybe we want {.member = value, .member1 = value1} kind of syntax. 
std::any TypeVisitor::visit(ASTInitializerList *node) {
  int type = -1;
  for (const auto &expr : node->expressions) {
    auto t = int_from_any(expr->accept(this));
    if (type == -1)
      type = t;
    else
      validate_type_compatability(t, type, node->source_range,
                                  "expected: {}, got {}",
                                  "initializer list had different types in "
                                  "one or many expressions");
  }
  if (type == -1) {
    throw_error("Cannot have an empty initializer list currently. to be "
                "implemented.",
                ERROR_FAILURE, node->source_range);
  }

  auto base = ctx.scope->get_type(type);
  
  auto dynamic_arr_ty = ctx.scope->find_type_id(
      base->base, {.extensions = {TYPE_EXT_ARRAY}, .array_sizes = {-1}});
      
  // TODO: make this more robust to struct initializer lists etc.
  if (declaring_or_assigning_type != dynamic_arr_ty) {
    auto type = ctx.scope->get_type(declaring_or_assigning_type);
    if (type->is_kind(TYPE_STRUCT)) {
      // TODO: do some type checking to make sure that the initializer list fits in the struct.
      // This may contain nested initializers, and we should probably make a seperate system just to
      // deduce whether this is possible or not so we don't have to clunky up a ton of this visitor.
      // For now I'll just assert that the number of members and their types match the initializer list.
    }
    return declaring_or_assigning_type;
  }
  return dynamic_arr_ty;
}

std::any TypeVisitor::visit(ASTEnumDeclaration *node) {
  int largest_type = s32_type();
  int largest_type_size = 1;
  for (const auto &[key, value] : node->key_values) {
    if (value.is_not_null()) {
      if (node->is_flags) {
        throw_error("You shouldn't use a #flags enum to generate auto "
                    "flags, and also use non-default values.",
                    ERROR_FAILURE, node->source_range);
      }
      auto expr = value.get();
      auto id = int_from_any(value.get()->accept(this));
      
      auto type = ctx.scope->get_type(id);
      
      if (!type->is_kind(TYPE_SCALAR) || !type->extensions.has_no_extensions()) {
        throw_error("Cannot have non integral types in enums got: " + type->to_string(), ERROR_FAILURE, node->source_range);
      }
      
      auto info = static_cast<ScalarTypeInfo*>(type->info);
      
      if (info->size > largest_type_size) {
        largest_type = id;
        largest_type_size = info->size;
      }
      
      validate_type_compatability(id, s64_type(), node->source_range,
                                  "expected: {}, got : {}",
                                  "Cannot have non-integral types in enums");
    }
  }
  node->element_type = largest_type;
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
std::any TypeVisitor::visit(ASTAllocate *node) {
  // TODO(Josh) 10/1/2024, 3:27:53 PM
  // Do something here. This is probably bad,
  // but this shouldn't be used as an expression really.
  if (node->kind == ASTAllocate::Delete) {
    if (node->arguments.is_null() ||
        node->arguments.get()->arguments.size() < 1) {
      throw_error("invalid delete statement: you need at least one argument",
                  ERROR_FAILURE, node->source_range);
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

  auto t = ctx.scope->get_type(type);
  return node->type.get()->resolved_type = t->id;
}