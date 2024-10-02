#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <any>
#include <format>
#include <jstl/containers/vector.hpp>
#include <limits>
#include <string>
#include <vector>

// these are called from and to because in the event of an implicit cast this
// should be the behaviour.

void validate_type_compatability(
    const int from, const int to, const SourceRange &source_range,
    std::format_string<std::string, std::string> format, std::string message) {
  auto from_t = get_type(from);
  auto to_t = get_type(to);
  auto conv_rule = type_conversion_rule(from_t, to_t);
  if (to != from &&
      (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' +
                    std::format(format, to_t->to_string(), from_t->to_string()),
                ERROR_FAILURE, source_range);
  }
}

/*
  ######################
  #### TYPE VISITOR ####
  ######################
*/

// Use this only to cast the result type of an expression.
// visiting the ASTType resolves itself.
static inline int int_from_any(const std::any &any) {
  return std::any_cast<int>(any);
}

std::any TypeVisitor::visit(ASTType *node) {
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    node->pointing_to.get()->accept(this);
  }
  return node->resolved_type = find_type_id(node->base, node->extension_info);
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
  
  auto type_id = find_type_id(get_function_type_name(node), info, {});

  // insert function
  context.current_scope->insert(node->name.value, type_id);

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

std::any TypeVisitor::visit(ASTBlock *node) {
  context.set_scope(node->scope);
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
  context.exit_scope();
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
  auto type = get_type(id);
  
  if (type->extensions.is_fixed_sized_array()) {
    throw_warning("using a fixed array as a function parameter: note, this casts the length information off and gets passed as as pointer. Consider using a dynamic array", node->source_range);
    if (node->default_value.is_not_null()) {
      throw_error("Cannot currently use default parameters for fixed buffer pointers. Also, length information gets casted off. consider using a dynamic array", ERROR_WARNING, node->source_range);
    }
    
    // cast the fixed sized array to a base*.
    {
      auto extensions = type->extensions;
      auto it = std::find(extensions.extensions.begin(), extensions.extensions.end(), TYPE_EXT_ARRAY);
      extensions.extensions.erase(it);
      extensions.array_sizes.erase(extensions.array_sizes.begin() + 1);
      extensions.extensions.insert(extensions.extensions.begin(), TYPE_EXT_POINTER);
      node->type->resolved_type = find_type_id(type->base, extensions);
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

// throws if inequal and unassignable.
std::any TypeVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);
  if (node->value.is_not_null()) {
    auto expr_type = int_from_any(node->value.get()->accept(this));
    validate_type_compatability(
        expr_type, node->type->resolved_type, node->source_range,
        "invalid declaration types. expected: {}, got {}",
        std::format("declaration: {}", node->name.value));
  }

  auto symbol = context.current_scope->lookup(node->name.value);
  symbol->type_id = node->type->resolved_type;
  return {};
}
std::any TypeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto right = int_from_any(node->right->accept(this));
  
  if (node->op.type == TType::ColonEquals) {
    left = right; 
    if (auto iden = dynamic_cast<ASTIdentifier*>(node->left)) {
      context.current_scope->insert(iden->value.value, left); 
    }
  }
  
  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have their operands type checked, but right now that would involve casting scalars to each other, which makes no  sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
    return bool_type();
  } else {
    auto left_t = get_type(left);
    auto right_t = get_type(right);
    auto conv_rule_0 = type_conversion_rule(left_t, right_t);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t);
    // TODO(Josh) 10/1/2024, 3:07:47 PM
    // validate that this is what we want. Before, it was too strict, now it feels like its too loose.
    // also, we should probably do specific type checking based on the operator, we still need some sort of table.
    if (((conv_rule_0 == CONVERT_PROHIBITED) && (conv_rule_1 == CONVERT_PROHIBITED)) || ((conv_rule_0 == CONVERT_EXPLICIT) && (conv_rule_1 == CONVERT_EXPLICIT))) {
      throw_error(std::format("Type error in binary expression: cannot convert between {} and {}",
              left_t->to_string(), right_t->to_string()),
          ERROR_FAILURE, node->source_range);
    }
  }
  
  node->resolved_type = left;
  return left;
}
std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));
  auto conversion_rule =
      type_conversion_rule(get_type(operand_ty), get_type(bool_type()));
  auto can_convert = (conversion_rule != CONVERT_PROHIBITED &&
                      conversion_rule != CONVERT_EXPLICIT);

  if (node->op.type == TType::Not && can_convert) {
    return bool_type();
  }

  if (node->op.type == TType::And) {
    auto ty = get_type(operand_ty);
    auto extensions = ty->extensions;
    extensions.extensions.push_back(TYPE_EXT_POINTER);
    return find_type_id(ty->base, extensions);
  }

  if (node->op.type == TType::Mul) {
    return remove_one_pointer_ext(operand_ty, node->source_range);
  }

  return operand_ty;
}
std::any TypeVisitor::visit(ASTIdentifier *node) {
  auto symbol = context.current_scope->lookup(node->value.value);
  if (symbol)
    return symbol->type_id;
  else {
    throw_error(
        std::format("Use of undeclared identifier '{}'", node->value.value),
        ERROR_FAILURE, node->source_range);
  }
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
  case ASTLiteral::Integer: {
    
    auto n = std::stoll(node->value);
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
  }
}
std::any TypeVisitor::visit(ASTCall *node) {
  auto symbol = context.current_scope->lookup(node->name.value);

  if (!symbol) {
    throw_error(std::format("Use of undeclared symbol '{}'", node->name.value),
                ERROR_FAILURE, node->source_range);
  }

  std::vector<int> arg_tys =
      std::any_cast<std::vector<int>>(node->arguments->accept(this));

  auto type = get_type(symbol->type_id);
  auto fn_ty_info = dynamic_cast<FunctionTypeInfo*>(type->info);
  
  // TODO(Josh) 10/1/2024, 8:46:53 AM We should be able to call constructors without
  // this function syntax, using #make(Type, ...) is really clunky and annoying;
  
  if (!type || !fn_ty_info) {
    throw_error("Unable to call function: {} did not refer to a function typed variable. Constructors currently use #make(Type, ...) syntax.", ERROR_FAILURE, node->source_range);
  }

  auto info = dynamic_cast<const FunctionTypeInfo *>(fn_ty_info);

  if (!info->is_varargs && (arg_tys.size() > info->params_len ||
       arg_tys.size() < info->params_len - info->default_params)) {
    throw_error(
        std::format("Function call '{}' has incorrect number of arguments. "
                    "Expected: {}, Found: {}",
                    node->name.value, info->params_len, arg_tys.size()),
        ERROR_FAILURE, node->source_range);
  }

  for (int i = 0; i < info->params_len; ++i) {
    
    // BUG: default parameters evade type checking
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
    argument_types.push_back(int_from_any(arg->accept(this)));
  }
  return argument_types;
}
std::any TypeVisitor::visit(ASTReturn *node) {
  int type;
  if (node->expression.is_not_null()) {
    type = int_from_any(node->expression.get()->accept(this));
  } else {
    type = find_type_id("void", {});
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
  context.set_scope(node->block->scope);
  switch (node->tag) {
  case ASTFor::RangeBased: {
    auto v = node->value.range_based;
    auto type = int_from_any(v.collection->accept(this));
    auto t = get_type(type);
    auto iden = static_cast<ASTIdentifier *>(v.target);
    
    // Todo: check for begin and end or some related iterator functions in structs before we allow that.
    // Right now we'll just allow it.
    
    int iter_ty = -1;
    if (auto info = dynamic_cast<StructTypeInfo*>(t->info)) {
      // TODO: add a way to use the value_semantic thing with custom iterators.
      Symbol* begin = info->scope->lookup("begin");
      Symbol* end = info->scope->lookup("end");
      if (begin && end && begin->type_id == end->type_id) {
        iter_ty = begin->type_id;
      } else {
        throw_error("Can only iterate over structs you define 'begin' and 'end' on. They must both be defined, and must both return the same type.", ERROR_FAILURE, node->source_range);
      }
    } else if (!t->extensions.is_array() && !t->extensions.is_fixed_sized_array()) {
      throw_error("cannot iterate with a range based for loop over a non collection type.", ERROR_FAILURE, node->source_range);
    } else {
      iter_ty = t->get_element_type();
    }
    
    // Take a pointer to the type.
    // This probably won't work well with custom iterators.
    if (v.value_semantic == VALUE_SEMANTIC_POINTER) {
      auto type = get_type(iter_ty);
      auto ext = type->extensions;
      ext.extensions.push_back(TYPE_EXT_POINTER);
      iter_ty = find_type_id(type->base, ext);
    }
    
    context.current_scope->insert(iden->value.value, iter_ty);
    v.target->accept(this);
  } break;
  case ASTFor::CStyle: {
    auto v = node->value.c_style;
    v.decl->accept(this);
    v.condition->accept(this);
    v.increment->accept(this);
  } break;
  }
  context.exit_scope();
  auto control_flow = std::any_cast<ControlFlow>(node->block->accept(this));
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  // we add fall through here because we dont know if this will get excecuted
  // since we cant evaluate the condition to know
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
  // we add fall through here because we dont know if this will get excecuted
  // since we cant evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}

std::any TypeVisitor::visit(ASTStructDeclaration *node) {
  auto type = get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->info);
  info->scope = node->scope;
  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0) {
    return {};
  }

  context.set_scope(node->scope);

  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method : node->methods) {
    method->accept(this);
  }

  
  context.exit_scope();
  return {};
}
std::any TypeVisitor::visit(ASTDotExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto left_ty = get_type(left);
  
  // TODO: remove this hack to get array length
  if (left_ty->extensions.is_array()) {
    auto right = dynamic_cast<ASTIdentifier*>(node->right);
    if (right && right->value.value == "length") {
      return s32_type();
    }
  }
  
  // Get enum variant
  if (left_ty->is_kind(TYPE_ENUM)) {
    // TODO: make type->info not nullable. it should never be null.
    auto info = static_cast<EnumTypeInfo*>(left_ty->info);
    auto iden = dynamic_cast<ASTIdentifier*>(node->right);
    
    if (!iden) {
      throw_error("cannot use a dot expression with a non identifer on the right hand side when referring to a enum.", ERROR_FAILURE, node->source_range);
    }
    
    auto name = iden->value.value;
    
    bool found = false;
    for (const auto &key: info->keys) {
      if (name == key) {
        found = true;
        break;
      }
    }
    
    if (!found) {
      throw_error("failed to find key in enum type.", ERROR_FAILURE, node->source_range);
    }
    
    // TODO(Josh) Add a way to support more than just s32 types from enums. Ideally, we could even use const char* etc. 9/30/2024, 11:53:45 AM
    return s32_type();
  }

  if (left_ty->kind != TYPE_STRUCT && left_ty->kind != TYPE_UNION) {
    throw_error(std::format("cannot use dot expr on non-struct currently, got {}", left_ty->to_string()), ERROR_FAILURE,
                node->source_range);
  }
  
  Scope *scope;
  if (auto info = dynamic_cast<StructTypeInfo *>(left_ty->info)) {
    scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo*>(left_ty->info)) {
    scope = info->scope;
  }

  auto previous_scope = context.current_scope;
  auto prev_parent = scope->parent;
  if (prev_parent && !previous_scope->is_struct_or_union_scope) {
    scope->parent = previous_scope;
  }
  
  // TODO: see above.
  context.set_scope(scope);
  int type = int_from_any(node->right->accept(this));
  context.set_scope(previous_scope);
  
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
  auto left_ty = get_type(left);

  
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
std::any TypeVisitor::visit(ASTInitializerList *node) {
  int type = -1;
  for (const auto &expr : node->expressions) {
    auto t = int_from_any(expr->accept(this));
    if (type == -1) type = t;
    else validate_type_compatability(t, type, node->source_range, "expected: {}, got {}", "initializer list had different types in one or many expressions");
  }
  if (type == -1) {
    throw_error("Cannot have an empty initializer list currently. to be implemented.", ERROR_FAILURE, node->source_range);
  }
  
  auto base = get_type(type);
  // (int)node->expressions.size();
  return find_type_id(base->base, {.extensions = {TYPE_EXT_ARRAY}, .array_sizes = { -1 }});
}
std::any TypeVisitor::visit(ASTEnumDeclaration *node) {
  for (const auto &[key, value]: node->key_values) {
    if (value.is_not_null()) {
      if (node->is_flags) {
        throw_error("You shouldn't use a #flags enum to generate auto flags, and also use non-default values.", ERROR_FAILURE, node->source_range);
      }
      auto expr = value.get();
      auto type = int_from_any(value.get()->accept(this));
      validate_type_compatability(type, s32_type(), node->source_range, "expected: {}, got : {}", "Cannot have non-integral types in enums");
    }
  }
  return {};
}

std::any TypeVisitor::visit(ASTUnionDeclaration *node) {
  // we store this ast just to type check the stuff.
  context.set_scope(node->scope);
  
  // do this first.
  for (const auto &_struct : node->structs) {
    for (const auto &field: _struct->fields) {
      field->accept(this);
      node->scope->insert(field->name.value, field->type->resolved_type);
    }
  }
  for (const auto &field: node->fields) {
    field->accept(this);  
  }
  for (const auto &method: node->methods) {
    method->accept(this);
  }
  context.exit_scope();
  return {};
}

std::any TypeVisitor::visit(ASTAllocate *node) {
  // TODO(Josh) 10/1/2024, 3:27:53 PM
  // Do something here. This is probably bad,
  // but this shouldn't be used as an expression really.
  if (node->kind == ASTAllocate::Delete) {
    if (node->arguments.is_null() || node->arguments.get()->arguments.size() < 1) {
      throw_error("invalid delete statement: you need at least one argument", ERROR_FAILURE, node->source_range);
    }
    // TODO(Josh) 10/1/2024, 4:40:04 PM This won't quite work, as the allocations aren't directly tied to the 
    // delete. We try to delete the delete node, not the alloc node.
    // we need to somehow lookup the source scope and variable, and then erase that.
    // erase_allocation(node);
    return void_type();
  }
  
  auto type = int_from_any(node->type.get()->accept(this));
  // just type check them, no need to return
  // we should probably type check parameters for a constructor 
  // but we need a seperate system for that
  if (node->arguments)
    node->arguments.get()->accept(this);
  
  auto t = get_type(type);
  return node->type.get()->resolved_type = t->id;
}