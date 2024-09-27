#include "visitor.hpp"
#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include <format>
#include <jstl/containers/vector.hpp>

void validate_type_compatability(const int node_ty, const int expr_type, const std::vector<Token> &source_tokens, std::format_string<std::string, std::string> format, std::string message) {
  auto tleft = get_type(node_ty),
        tright = get_type(expr_type);
        
  auto conv_rule = type_conversion_rule(tleft, tright);
  
  if (expr_type != node_ty && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(
        message + '\n' + std::format(format, tleft->to_string(), tright->to_string()),
        ERROR_FAILURE, source_tokens);
  }
}

std::any SerializeVisitor::visit(ASTProgram *node) {
  ss << indent() << "Program {\n";
  indentLevel++;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  indentLevel--;
  ss << indent() << "}\n";
  return ss.str();
}
std::any SerializeVisitor::visit(ASTBlock *node) {
  ss << indent() << "Block {\n";

  ss << indent() << "flags: " << block_flags_to_string(node->flags) << '\n';
  auto type = get_type(node->return_type);
  if (type)
    ss << indent() << "type: " << type->to_string() << '\n';

  indentLevel++;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTFuncDecl *node) {
  ss << indent() << "Function " << node->name.value << " {\n";
  indentLevel++;
  auto sym = context.current_scope->lookup(node->name.value);
  ss << indent() << "type: " << get_type(sym->type_id)->to_string() << '\n';
  visit(node->params);
  visit(node->block);
  ss << indent() << "returns: ";
  visit(node->return_type);
  ss << '\n';

  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTParamsDecl *node) {
  ss << indent() << "Parameters {\n";
  indentLevel++;
  for (auto param : node->params) {
    visit(param);
  }
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTParamDecl *node) {
  ss << indent() << "Parameter " << node->name << " : ";
  node->type->accept(this);
  ss << "\n";
  return {};
}
std::any SerializeVisitor::visit(ASTDeclaration *node) {
  ss << indent() << "Declaration " << node->name.value << " : ";
  node->type->accept(this);
  if (node->value.is_not_null()) {
    ss << " = ";
    node->value.get()->accept(this);
  }
  ss << '\n';
  return {};
}
std::any SerializeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  ss << "\n";
  return {};
}
std::any SerializeVisitor::visit(ASTBinExpr *node) {
  ss << "(";
  node->left->accept(this);
  ss << " " << node->op.value << " ";
  node->right->accept(this);
  ss << ")";
  return {};
}
std::any SerializeVisitor::visit(ASTUnaryExpr *node) {
  ss << indent() << "unary: ";
  ss << node->op.value;
  node->operand->accept(this);
  return {};
}
std::any SerializeVisitor::visit(ASTIdentifier *node) {
  ss << node->value.value;
  return {};
}
std::any SerializeVisitor::visit(ASTLiteral *node) {
  if (node->tag == ASTLiteral::String) {
    ss << '\"' << node->value << '\"';
  } else
    ss << node->value;
  return {};
}
std::any SerializeVisitor::visit(ASTType *node) {
  if (node->resolved_type != -1) {
    auto type = get_type(node->resolved_type);
    ss << "type: " << node->resolved_type << ", " << type->base
       << type->extensions.to_string();
    return {};
  }
  ss << node->base;
  ss << node->extension_info.to_string();
  return {};
}
std::any SerializeVisitor::visit(ASTArguments *node) {
  ss << indent() << "Arguments {";
  indentLevel++;
  bool first = true;
  for (auto arg : node->arguments) {
    if (!first)
      ss << ", ";
    first = false;
    arg->accept(this);
  }
  indentLevel--;
  ss << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTCall *node) {
  ss << indent() << "Call " << node->name.value
     << " type: " << get_type(node->type)->to_string() << " {\n";
  indentLevel++;
  visit(node->arguments);
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::string SerializeVisitor::indent() {
  return std::string(indentLevel * 2, ' ');
}

std::any SerializeVisitor::visit(ASTReturn *node) {
  ss << indent() << "Return: ";
  if (node->expression.is_not_null())
    node->expression.get()->accept(this);
  ss << indent() << '\n';
  return {};
}
std::any SerializeVisitor::visit(ASTContinue *node) {
  ss << indent() << "Continue\n";
  return {};
}
std::any SerializeVisitor::visit(ASTBreak *node) {
  ss << indent() << "Break\n";
  return {};
}
std::any SerializeVisitor::visit(ASTFor *node) {
  ss << indent() << "For {\n";
  indentLevel++;

  switch (node->tag) {
  case ASTFor::RangeBased: {
    auto v = node->value.range_based;
    ss << indent() << "RangeBased:\n";
    ss << indent() << "target: ";
    v.target->accept(this);
    ss << " -> ";
    v.collection->accept(this);
    ss << '\n';
  } break;
  case ASTFor::CStyle: {
    auto v = node->value.c_style;
    ss << indent() << "CStyle:";
    v.decl->accept(this);
    ss << indent() << "condition: ";
    v.condition->accept(this);
    ss << '\n' << indent() << "increment: ";
    v.increment->accept(this);
    ss << '\n';
  } break;
  }

  node->block->accept(this);

  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTIf *node) {
  ss << indent() << "If {\n";
  indentLevel++;
  ss << indent() << "condition: ";
  node->condition->accept(this);
  ss << '\n';
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  indentLevel--;
  ss << indent() << "}\n";

  return {};
}
std::any SerializeVisitor::visit(ASTElse *node) {
  ss << indent() << "Else {\n";
  indentLevel++;
  if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  } else if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  }
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTWhile *node) {
  ss << indent() << "While {\n";
  indentLevel++;
  ss << indent() << "condition: ";
  if (node->condition.is_not_null())
    node->condition.get()->accept(this);
  ss << '\n';

  node->block->accept(this);
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTCompAssign *node) {
  ss << indent() << "Compound Assignment {\n";
  indentLevel++;
  ss << indent() << "left: " << node->name.value;
  ss << '\n' << indent() << "operator: " << node->op.value << '\n';
  ss << indent() << "right: ";
  node->expr->accept(this);
  ss << '\n';
  indentLevel--;
  ss << indent() << "}\n";
  return {};
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
  
  auto params = node->params->params;
  for (const auto &param : params) {
    if (param->default_value.is_not_null()) info.default_params++;
    node->block->scope->insert(param->name, param->type->resolved_type);
    info.parameter_types[info.params_len] = param->type->resolved_type;
    info.params_len++;
  }

  auto type_id = find_type_id("", info, {});

  // insert function
  context.current_scope->insert(node->name.value, type_id);

  auto return_type = find_type_id("void", {});

  visitor_flags |= VisitorBase::FLAG_FUNCTION_ROOT_LEVEL_BLOCK;
  visitor_flags |= VisitorBase::FLAG_VISITING_FUNCTION;
  return_type = int_from_any(node->block->accept(this));
  visitor_flags &= ~VisitorBase::FLAG_VISITING_FUNCTION;
  
  validate_type_compatability(return_type, info.return_type, node->source_tokens, "invalid function return type: {} {}", std::format("function: {}", node->name.value));
  return {};
}

// TODO: wrangle this absolute mess of a function. I have no fucking idea how we
// will ever fix this.
std::any TypeVisitor::visit(ASTBlock *node) {
  const auto update_flags = [](int &flags, int block_flags) {
    flags |= block_flags;
    if ((block_flags & BLOCK_FLAGS_FALL_THROUGH) == 0) {
      flags &= ~BLOCK_FLAGS_FALL_THROUGH;
    }
  };
  const auto check_return_type_consistency = [&](int &return_type,
                                                int new_type) {
    if (return_type == -1) {
      return_type = new_type;
    } else if (new_type != -1 && new_type != return_type) {
      auto expected_type = get_type(return_type);
      auto found_type = get_type(new_type);
      throw_error(
          std::format("Inconsistent return types in block. Expected: {}, Found: {}", expected_type->base, found_type->base),
          ERROR_FAILURE,
          node->source_tokens
      );
    }
  };

  bool fn_root_level = visitor_flags & FLAG_VISITING_FUNCTION &&
                       visitor_flags & FLAG_FUNCTION_ROOT_LEVEL_BLOCK;

  if (fn_root_level) {
    visitor_flags &= ~FLAG_FUNCTION_ROOT_LEVEL_BLOCK;
  }

  context.enter_scope(node->scope);
  int flags = BLOCK_FLAGS_FALL_THROUGH;
  int return_type = -1;

  for (auto &statement : node->statements) {
    auto result = statement->accept(this);
    int stmt_flags = BLOCK_FLAGS_FALL_THROUGH;
    int stmt_ret_ty = -1;

    {
      // TODO: this needs a lot of work. @Cooper-Pilot for gods sake I need help.
      // ASTBlock *block = nullptr;

      // if (auto block_stmt = dynamic_cast<ASTBlock *>(statement)) {
      //   block = block_stmt;
      // }
      // if (auto if_stmt = dynamic_cast<ASTIf *>(statement)) {
      //   block = if_stmt->block;
      //   stmt_ret_ty = int_from_any(result);
      //   if (if_stmt->_else.is_not_null()) {
      //     auto else_result = if_stmt->_else.get()->accept(this);
      //     int else_ret_ty = int_from_any(else_result);
      //     check_return_type_consistency(stmt_ret_ty, else_ret_ty);
      //   }
      // } else if (auto for_stmt = dynamic_cast<ASTFor *>(statement)) {
      //   block = for_stmt->block;
      // } else if (auto while_stmt = dynamic_cast<ASTWhile *>(statement)) {
      //   block = while_stmt->block;
      // }

      // if (block) {
      //   stmt_flags = block->flags;
      //   stmt_ret_ty = int_from_any(result);
      // } else
    }
     if (auto cont = dynamic_cast<ASTContinue *>(statement)) {
      flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      flags |= BLOCK_FLAGS_CONTINUE;
      continue;
    } else if (auto ret = dynamic_cast<ASTReturn *>(statement)) {
      flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      flags |= BLOCK_FLAGS_RETURN;
      stmt_ret_ty = ret->expression.is_not_null()
                        ? int_from_any(ret->expression.get()->accept(this))
                        : find_type_id("void", {});
      update_flags(flags, stmt_flags);
      check_return_type_consistency(return_type, stmt_ret_ty);
      break;
      
    } else if (auto brk = dynamic_cast<ASTBreak *>(statement)) {
      flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      flags |= BLOCK_FLAGS_BREAK;
      continue;
    }

    update_flags(flags, stmt_flags);
    check_return_type_consistency(return_type, stmt_ret_ty);
  }

  if (return_type == -1) {
    return_type = find_type_id("void", {});
  }

  if (fn_root_level && return_type != find_type_id("void", {}) &&
      (flags & BLOCK_FLAGS_RETURN) == 0) {
    throw_error(
        "Not all code paths return a value.",
        ERROR_FAILURE,
        node->source_tokens
    );
  }

  node->flags = flags;
  node->return_type = return_type;
  context.exit_scope();
  return return_type;
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
    validate_type_compatability(node->type->resolved_type, expr_type, node->source_tokens, "invalid parameter declaration; expected: {} got: {}", std::format("parameter: {}", node->name));
  }
  return {};
}
// throws if inequal and unassignable.

std::any TypeVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);

  if (node->value.is_not_null()) {
    auto expr_type = int_from_any(node->value.get()->accept(this));
    validate_type_compatability(node->type->resolved_type, expr_type, node->source_tokens, "invalid declaration types. expected: {}, got {}", std::format("declaration: {}", node->name.value));
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
  validate_type_compatability(left, right, node->source_tokens, "invalid types in binary expression. expected: {}, got {}", "");
  // for now we just return the lhs.
  return left;
}

std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  // TODO: type convert certain operations where neccesary, like ! returns a
  // boolean regardless of it's operands types.
  return node->operand->accept(this);
}
std::any TypeVisitor::visit(ASTIdentifier *node) {
  auto symbol = context.current_scope->lookup(node->value.value);
  if (symbol)
    return symbol->type_id;
  else {
    throw_error(
        std::format("Use of undeclared identifier '{}'", node->value.value),
        ERROR_FAILURE,
        node->source_tokens
    );
  }
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
  case ASTLiteral::Integer:
    return find_type_id("s32", {});
  case ASTLiteral::Float:
    return find_type_id("f32", {});
  case ASTLiteral::String:
    return find_type_id("string", {});
    break;
  case ASTLiteral::Bool:
    return find_type_id("bool", {});
  case ASTLiteral::Null:
    return find_type_id("void", TypeExtensionInfo { .extensions = {TYPE_EXT_POINTER}, .array_sizes = {} });
    break;
  }
}
std::any TypeVisitor::visit(ASTCall *node) {
  auto symbol = context.current_scope->lookup(node->name.value);

  if (!symbol) {
    throw_error(
        std::format("Use of undeclared symbol '{}'", node->name.value),
        ERROR_FAILURE,
        node->source_tokens
    );
  }

  jstl::Vector<int> arg_tys =
      std::any_cast<jstl::Vector<int>>(node->arguments->accept(this));

  auto fn_ty_info = get_type(symbol->type_id)->info;

  if (fn_ty_info.is_null()) {
    throw_error(
        std::format("Function call '{}' does not refer to a function type.",
                        node->name.value),
        ERROR_FAILURE,
        node->source_tokens
    );
  }

  auto info = dynamic_cast<const FunctionTypeInfo *>(fn_ty_info.get());

  if ((arg_tys.size() > info->params_len || arg_tys.size() < info->params_len - info->default_params) && !info->is_varargs) {
    throw_error(
        std::format("Function call '{}' has incorrect number of arguments. "
                        "Expected: {}, Found: {}",
                        node->name.value, info->params_len, arg_tys.size()),
        ERROR_FAILURE,
        node->source_tokens
    );
  }
  
  for (int i = 0; i < info->params_len; ++i) {
    // TODO: default parameters evade type checking
    if (arg_tys.size() <= i) {
      continue;
    }
    validate_type_compatability(info->parameter_types[i], arg_tys[i], node->source_tokens, "invalid argument types. expected: {}, got: {}", std::format("parameter: {} of function: {}", i, node->name.value));
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
    auto iden = static_cast<ASTIdentifier*>(v.target);
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
  // TODO: type check to confirm this is convertible to, or is a bool.
  //auto bool_type = find_type_id("bool", {});
  node->condition->accept(this);

  if (node->_else.is_not_null()) {
    return node->_else.get()->accept(this);
  } else {
    return node->block->accept(this);
  }
  return {};
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
  validate_type_compatability(symbol->type_id, expr_ty, node->source_tokens, "invalid types in compound assignment. expected: {}, got {}", "");
  return {};
}
