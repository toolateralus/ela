#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <any>
#include <cstdlib>
#include <format>
#include <jstl/containers/vector.hpp>
#include <limits>
#include <string>

// these are called from and to because in the event of an implicit cast this should be the behaviour.
void validate_type_compatability(const int from, const int to, const std::vector<Token> &source_tokens, std::format_string<std::string, std::string> format, std::string message) {
  auto from_t = get_type(from);
  auto to_t = get_type(to);
  
  auto conv_rule = type_conversion_rule(from_t, to_t);
  
  if (to != from && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(
        message + '\n' +
            std::format(format, from_t->to_string(), to_t->to_string()),
        ERROR_FAILURE, source_tokens);
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
  return node->resolved_type = find_type_id(node->base, node->extension_info);
}
std::any TypeVisitor::visit(ASTProgram *node) {
  for (auto &statement : node->statements) {
    statement->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTFuncDecl *node) {
  
  
  node->return_type->accept(this);
  node->params->accept(this);

  FunctionTypeInfo info;

  info.return_type = node->return_type->resolved_type;
  info.params_len = 0;
  info.default_params = 0;
  info.meta_type = node->meta_type;

  auto params = node->params->params;
  for (const auto &param : params) {
    if (param->default_value.is_not_null())
      info.default_params++;

    if (node->block.is_not_null())
      node->block.get()->scope->insert(param->name, param->type->resolved_type);

    info.parameter_types[info.params_len] = param->type->resolved_type;
    info.params_len++;
  }

  auto type_id = find_type_id("", info, {});

  // insert function
  context.current_scope->insert(node->name.value, type_id);

  if (info.meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN) {
    return {};
  }

  auto control_flow = std::any_cast<ControlFlow>(node->block.get()->accept(this));
  if (control_flow.type == -1) {
    control_flow.type = void_type();
  }

  if ((control_flow.flags & BLOCK_FLAGS_CONTINUE) != 0) {
    throw_error("Keyword \"continue\" must be in a loop.", ERROR_FAILURE,
                node->source_tokens);
  }

  if ((control_flow.flags & BLOCK_FLAGS_BREAK) != 0) {
    throw_error("Keyword \"break\" must be in a loop.", ERROR_FAILURE,
                node->source_tokens);
  }

  if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) != 0 &&
      info.return_type != void_type()) {
    throw_error("Not all code paths return a value.", ERROR_FAILURE,
                node->source_tokens);
  }

  validate_type_compatability(control_flow.type, info.return_type,
                              node->source_tokens,
                              "invalid function return type: {} {}",
                              std::format("function: {}", node->name.value));
  return {};
}
const auto check_return_type_consistency(int &return_type, int new_type,
                                         ASTNode *node) {
  if (return_type == -1) {
    return_type = new_type;
  } else if (new_type != -1 && new_type != return_type) {
    validate_type_compatability(return_type, new_type,
                                node->source_tokens,
                                "Expected: {}, Found: {}",
                                "Inconsistent return types in block.");
  }
};

std::any TypeVisitor::visit(ASTBlock *node) {
  context.enter_scope(node->scope);
  ControlFlow block_cf = {BLOCK_FLAGS_FALL_THROUGH, -1};

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    if (dynamic_cast<ASTBlock *>(statement) ||
        dynamic_cast<ASTIf *>(statement) ||
        dynamic_cast<ASTFor *>(statement) ||
        dynamic_cast<ASTWhile *>(statement) ||
        dynamic_cast<ASTReturn *>(statement) ||
        dynamic_cast<ASTContinue *>(statement) ||
        dynamic_cast<ASTBreak *>(statement)
    ) {
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
  node->type->accept(this);
  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    validate_type_compatability(
        node->type->resolved_type, expr_type, node->source_tokens,
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
    validate_type_compatability(expr_type, node->type->resolved_type, node->source_tokens, "invalid declaration types. expected: {}, got {}", std::format("declaration: {}", node->name.value));
  }

  // TODO: probably want something a bit nicer than this.
  context.current_scope->lookup(node->name.value)->type_id =
      node->type->resolved_type;
  return {};
}
std::any TypeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto right = int_from_any(node->right->accept(this));
  // TODO: relational expressions need to have their operands type checked, but right now that would involve casting scalars to each other, which makes no sense.
  if (node->op.is_relational()) {
    node->resolved_type = bool_type();
    return bool_type();    
  } else {
    validate_type_compatability(right, left, node->source_tokens, "invalid types in binary expression. expected: {}, got {}", "");
  }
  node->resolved_type = left;
  return left;
}

std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));
  auto conversion_rule = type_conversion_rule(
      get_type(operand_ty), get_type(bool_type()));
  auto can_convert = (conversion_rule != CONVERT_PROHIBITED &&
                      conversion_rule != CONVERT_EXPLICIT);

  if (node->op.type == TType::Not && can_convert) {
    return bool_type();
  }

  if (node->op.type == TType::And) {
    auto ty = get_type(operand_ty);
    return find_type_id(ty->base, TypeExt{.extensions = {TYPE_EXT_POINTER}});
  }

  if (node->op.type == TType::Mul) {
    return remove_one_pointer_ext(operand_ty, node->source_tokens);
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
        ERROR_FAILURE, node->source_tokens);
  }
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
  case ASTLiteral::Integer: {
    // TODO:
    // this will fail if the number is greater than max int64_t or less
    // than min int64_t
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
    return f32_type();
  case ASTLiteral::RawString:
  case ASTLiteral::String:
    return string_type();
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
                ERROR_FAILURE, node->source_tokens);
  }

  jstl::Vector<int> arg_tys =
      std::any_cast<jstl::Vector<int>>(node->arguments->accept(this));

  auto fn_ty_info = get_type(symbol->type_id)->info;

  if (fn_ty_info.is_null()) {
    throw_error(
        std::format("Function call '{}' does not refer to a function type.",
                    node->name.value),
        ERROR_FAILURE, node->source_tokens);
  }

  auto info = dynamic_cast<const FunctionTypeInfo *>(fn_ty_info.get());

  if ((arg_tys.size() > info->params_len ||
       arg_tys.size() < info->params_len - info->default_params) &&
      !info->is_varargs) {
    throw_error(
        std::format("Function call '{}' has incorrect number of arguments. "
                    "Expected: {}, Found: {}",
                    node->name.value, info->params_len, arg_tys.size()),
        ERROR_FAILURE, node->source_tokens);
  }

  for (int i = 0; i < info->params_len; ++i) {
    // TODO: default parameters evade type checking
    if (arg_tys.size() <= i) {
      continue;
    }
    validate_type_compatability(arg_tys[i], info->parameter_types[i], node->source_tokens, "invalid argument types. expected: {}, got: {}", std::format("parameter: {} of function: {}", i, node->name.value));
  }

  node->type = info->return_type;
  return info->return_type;
}
std::any TypeVisitor::visit(ASTArguments *node) {
  jstl::Vector<int> argument_types;
  for (auto arg : node->arguments) {
    argument_types.push(int_from_any(arg->accept(this)));
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
std::any TypeVisitor::visit(ASTContinue *node) { return ControlFlow{BLOCK_FLAGS_CONTINUE, -1}; }
std::any TypeVisitor::visit(ASTBreak *node) { return ControlFlow{BLOCK_FLAGS_BREAK, -1}; }

std::any TypeVisitor::visit(ASTFor *node) {
  context.enter_scope(node->block->scope);
  switch (node->tag) {
  case ASTFor::RangeBased: {
    auto v = node->value.range_based;
    auto type = int_from_any(v.collection->accept(this));
    auto iden = static_cast<ASTIdentifier *>(v.target);
    context.current_scope->insert(iden->value.value, type);
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
  // we add fall through here because we dont know if this will get excecuted since we cant
  // evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}
std::any TypeVisitor::visit(ASTIf *node) {
  auto cond_ty = int_from_any(node->condition->accept(this));
  validate_type_compatability(
      bool_type(), cond_ty, node->source_tokens,
      "expected: {}, got {}",
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
  // we add fall through here because we dont know if this will get excecuted since we cant
  // evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  return control_flow;
}

std::any TypeVisitor::visit(ASTCompAssign *node) {
  auto symbol = context.current_scope->lookup(node->name.value);
  auto expr_ty = int_from_any(node->expr->accept(this));
  validate_type_compatability(
      symbol->type_id, expr_ty, node->source_tokens,
      "invalid types in compound assignment. expected: {}, got {}", "");
  return {};
}

std::any TypeVisitor::visit(ASTStructDeclaration *node) {
  auto type = get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo*>(type->info.get());
  
  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0) {
    return {};
  }
  
  // TODO: we can improve this to not rely so heavily on a Scope object.
  // It seems wrong to store a scope in a Type *.
  context.enter_scope(node->scope);
  jstl::Vector<ASTDeclaration*> fields;
  for (auto decl : node->declarations) {
    decl->accept(this);
    fields.push(decl);
  }
  info->fields = fields;
  info->scope = node->scope;
  context.exit_scope();
  return {};
}

std::any TypeVisitor::visit(ASTDotExpr *node) {
  // TODO: this needs serious improvement. Check whether the left hand side variable even exists, etc.
  
  auto left = int_from_any(node->left->accept(this));
  auto left_ty = get_type(left);
  if (left_ty->kind != TYPE_STRUCT) {
    throw_error("cannot use dot expr on non-struct currently.", ERROR_FAILURE, node->source_tokens);
  }
  auto info = static_cast<StructTypeInfo*>(left_ty->info.get());
  
  auto old_parent = info->scope->parent;
  auto above = context.current_scope;
  
  if (above->is_struct_scope) {
    info->scope->parent = above;
  }
  context.enter_scope(info->scope);
  
  const auto exit_scope = [&]  {
    if (above->is_struct_scope) {
      info->scope->parent = old_parent;
    }
    context.current_scope = above;
  };
  
  if (auto iden = dynamic_cast<ASTIdentifier*>(node->right)) {
    if (!context.current_scope->lookup(iden->value.value)) {
      throw_error(std::format("use of undeclared identifier: {}", iden->value.value), ERROR_FAILURE, node->source_tokens);
    }
    for (const auto &field: info->fields) {
      if (field->name.value == iden->value.value) {
        auto value =  node->type->resolved_type = field->type->resolved_type;
        exit_scope();
        return value;
      }
    }
  }
  if (auto dot = dynamic_cast<ASTDotExpr*>(node->right)) {
    auto value = node->type->resolved_type = int_from_any(dot->accept(this));
    exit_scope();
    return value;
  }
  throw_error("unable to resolve dot expression type.", ERROR_FAILURE, node->source_tokens);
}