#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <any>
#include <format>
#include <jstl/containers/vector.hpp>

// these are called from and to because in the event of an implicit cast this should be the behaviour.
void validate_type_compatability(const int from, const int to, const std::vector<Token> &source_tokens, std::format_string<std::string, std::string> format, std::string message) {
  auto tleft = get_type(from),
        tright = get_type(to);
        
  auto conv_rule = type_conversion_rule(tleft, tright);
  
  if (to != from && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(
        message + '\n' +
            std::format(format, tleft->to_string(), tright->to_string()),
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
  node->resolved_type = find_type_id(node->base, node->extension_info);
  return {};
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
  info.flags = node->flags;

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

  if (info.flags & FUNCTION_FOREIGN) {
    return {};
  }

  auto return_type = find_type_id("void", {});
  visitor_flags |= VisitorBase::FLAG_FUNCTION_ROOT_LEVEL_BLOCK;
  auto control_flow = std::any_cast<ControlFlow>(node->block.get()->accept(this));
  if (control_flow.type != -1) {
    return_type = control_flow.type;
  }
  validate_type_compatability(return_type, info.return_type,
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
    auto expected_type = get_type(return_type);
    auto found_type = get_type(new_type);
    throw_error(
        std::format(
            "Inconsistent return types in block. Expected: {}, Found: {}",
            expected_type->base, found_type->base),
        ERROR_FAILURE, node->source_tokens);
  }
};

std::any TypeVisitor::visit(ASTBlock *node) {
  bool fn_root_level = visitor_flags & FLAG_FUNCTION_ROOT_LEVEL_BLOCK;

  if (fn_root_level) {
    visitor_flags &= ~FLAG_FUNCTION_ROOT_LEVEL_BLOCK;
  }

  context.enter_scope(node->scope);
  int flags = BLOCK_FLAGS_FALL_THROUGH;
  int return_type = -1;

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    auto control_flow = ControlFlow{BLOCK_FLAGS_FALL_THROUGH, -1};
    if (dynamic_cast<ASTBlock *>(statement) ||
        dynamic_cast<ASTIf *>(statement) ||
        dynamic_cast<ASTFor *>(statement) ||
        dynamic_cast<ASTWhile *>(statement)
    ) {
      control_flow = std::any_cast<ControlFlow>(result);
    } else if (dynamic_cast<ASTContinue *>(statement)) {
      control_flow.flags = BLOCK_FLAGS_CONTINUE;
    } else if (dynamic_cast<ASTBreak *>(statement)) {
      control_flow.flags = BLOCK_FLAGS_BREAK;
    } else if (auto ret = dynamic_cast<ASTReturn *>(statement)) {
      control_flow.flags = BLOCK_FLAGS_RETURN;
      if (ret->expression.is_not_null()) {
        control_flow.type = int_from_any(ret->expression.get()->accept(this));
      } else {
        control_flow.type = find_type_id("void", {});
      }
    }
    flags |= control_flow.flags;
    if ((control_flow.flags & BLOCK_FLAGS_RETURN) != 0) {
      check_return_type_consistency(return_type, control_flow.type, node);
    }
    if ((control_flow.flags & BLOCK_FLAGS_FALL_THROUGH) == 0) {
      flags &= ~BLOCK_FLAGS_FALL_THROUGH;
    }
  }

  if (return_type == -1) {
    return_type = find_type_id("void", {});
  }

  if (fn_root_level && return_type != find_type_id("void", {}) &&
      flags != BLOCK_FLAGS_RETURN) {
    throw_error("Not all code paths return a value.", ERROR_FAILURE,
                node->source_tokens);
  }

  node->flags = flags;
  node->return_type = return_type;
  context.exit_scope();
  return ControlFlow{flags, return_type};
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
    return find_type_id("bool", {});    
  } else {
    validate_type_compatability(left, right, node->source_tokens, "invalid types in binary expression. expected: {}, got {}", "");
  }
  return left;
}

std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  auto operand_ty = int_from_any(node->operand->accept(this));
  auto conversion_rule = type_conversion_rule(
      get_type(operand_ty), get_type(find_type_id("bool", {})));
  auto can_convert = (conversion_rule != CONVERT_PROHIBITED &&
                      conversion_rule != CONVERT_EXPLICIT);

  if (node->op.type == TType::Not && can_convert) {
    return find_type_id("bool", {});
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
  case ASTLiteral::Integer:
    return find_type_id("s32", {});
  case ASTLiteral::Float:
    return find_type_id("f32", {});
  case ASTLiteral::RawString:
  case ASTLiteral::String:
    return find_type_id("u8", {.extensions = { TYPE_EXT_POINTER }});
    break;
  case ASTLiteral::Bool:
    return find_type_id("bool", {});
  case ASTLiteral::Null:
    return find_type_id(
        "void", TypeExt{.extensions = {TYPE_EXT_POINTER}, .array_sizes = {}});
    break;
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
  if (node->expression.is_not_null())
    node->expression.get()->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTContinue *node) { return {}; }
std::any TypeVisitor::visit(ASTBreak *node) { return {}; }

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
  return node->block->accept(this);
}
std::any TypeVisitor::visit(ASTIf *node) {

  auto cond_ty = int_from_any(node->condition->accept(this));
  validate_type_compatability(
      find_type_id("bool", {}), cond_ty, node->source_tokens,
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
  return node->block->accept(this);
}
std::any TypeVisitor::visit(ASTCompAssign *node) {
  auto symbol = context.current_scope->lookup(node->name.value);
  auto expr_ty = int_from_any(node->expr->accept(this));
  validate_type_compatability(
      symbol->type_id, expr_ty, node->source_tokens,
      "invalid types in compound assignment. expected: {}, got {}", "");
  return {};
}
