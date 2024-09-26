#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <jstl/containers/vector.hpp>

std ::any EmitVisitor::visit(ASTCompAssign *node){
  ss << node->name.value << " ";
  ss << node->op.value << " ";
  node->expr->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTWhile *node) {
  ss << "while ";
  if (node->condition.is_not_null()) {
    ss << "(";
    node->condition.get()->accept(this);
    ss << ")";
  } else {
    ss << "(true)";
  }
  node->block->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTElse *node) {
  ss << " else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return {};
}
std ::any EmitVisitor::visit(ASTIf *node) {
  ss << indent() << "if (";
  node->condition->accept(this);
  ss << ')';
  node->block->accept(this);
  
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  
  return {};
}
std ::any EmitVisitor::visit(ASTFor *node) {
  // TODO: 
  return {};
}
std ::any EmitVisitor::visit(ASTBreak *node) {
  ss << "break";
  return {};  
}
std ::any EmitVisitor::visit(ASTContinue *node) {
  ss << "continue";
  return {};
}
std ::any EmitVisitor::visit(ASTReturn *node) {
  ss << "return";
  if (node->expression.is_not_null()) {
    ss << ' ';
    node->expression.get()->accept(this);
  }
  return {};
}
std ::any EmitVisitor::visit(ASTArguments *node) {
  ss << "(";
  for (const auto &arg: node->arguments) {
    arg->accept(this);
  }
  ss << ")";
  return {};
}
std ::any EmitVisitor::visit(ASTType *node) {
  ss << get_type(node->resolved_type)->to_string();
  return {};
}
std ::any EmitVisitor::visit(ASTCall *node) {
  ss << node->name.value;
  node->arguments->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTLiteral *node) {
  ss << node->value;
  return {};
}
std ::any EmitVisitor::visit(ASTIdentifier *node) {
  ss << node->value.value;
  return {};
}
std ::any EmitVisitor::visit(ASTUnaryExpr *node) {
  ss << node->op.value;
  node->operand->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTBinExpr *node) {
  auto left = node->left->accept(this);
  ss << ' ' << node->op.value << ' '; 
  auto right = node->right->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std ::any EmitVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);
  ss << indent() << ' ';
  ss << indent() << node->name.value << " = ";
  if (node->value.is_not_null()) {
    node->value.get()->accept(this);
  } else {
    ss << indent() << "{}";
  }
  return {};
}
std ::any EmitVisitor::visit(ASTParamDecl *node) {
  node->type->accept(this);
  ss << ' ' << node->name;
  if (node->default_value.is_not_null()) {
    ss << " = ";
    node->default_value.get()->accept(this);
  }
  return {};
}
std ::any EmitVisitor::visit(ASTParamsDecl *node) {
  ss << " (";
  int i = 0;
  for (const auto &param: node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      ss << ", ";
    }
    ++i;
  }
  ss << ")";
  return {};
}
std ::any EmitVisitor::visit(ASTFuncDecl *node) {
  auto symbol = context.current_scope->lookup(node->name.value);
  node->return_type->accept(this);
  ss << ' ';
  ss << node->name.value << ' ';
  node->params->accept(this);  
  node->block->accept(this);
  return {};
}

std ::any EmitVisitor::visit(ASTBlock *node) {
  ss << indent() << "{\n";
  context.enter_scope(node->scope);
  indentLevel++;
  for (const auto &statement: node->statements) {
    statement->accept(this);
    semicolon();
  }
  ss << indent() << "\n}";
  context.exit_scope();
  indentLevel--;
  return {};
}

std ::any EmitVisitor::visit(ASTProgram *node) {
  ss << R"_(#include "boilerplate.hpp")_" << '\n';
  for (const auto &statement: node->statements) {
    statement->accept(this);
    semicolon();
  }
  return {};
}

