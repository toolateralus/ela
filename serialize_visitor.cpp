#include "ast.hpp"
#include "visitor.hpp"

std::any SerializeVisitor::visit(ASTProgram *node) {
  printf("%s\n", typeid(node).name());
  
  ss << getIndent() << "Program {\n";
  indentLevel++;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  indentLevel--;
  ss << getIndent() << "}\n";
  return ss.str();
}
std::any SerializeVisitor::visit(ASTBlock *node) {
  ss << getIndent() << "Block {\n";
  indentLevel++;
  for (auto statement : node->statements) {
    statement->accept(this);
  }
  indentLevel--;
  ss << getIndent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTFuncDecl *node) {
  ss << getIndent() << "Function " << node->name.value << " {\n";
  indentLevel++;
  visit(node->params);
  visit(node->block);
  indentLevel--;
  ss << getIndent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTParamsDecl *node) {
  ss << getIndent() << "Parameters {\n";
  indentLevel++;
  for (auto param : node->params) {
    visit(param);
  }
  indentLevel--;
  ss << getIndent() << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTParamDecl *node) {
  ss << getIndent() << "Parameter " << node->name << " : " << node->type->base
     << "\n";
  return {};
}
std::any SerializeVisitor::visit(ASTDeclaration *node) {
  ss << getIndent() << "Declaration " << node->name.value << " : "
     << node->type->base << "\n";
  return {};
}
std::any SerializeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  ss << ";\n";
  return {};
}
std::any SerializeVisitor::visit(ASTBinExpr *node) {
  node->left->accept(this);
  ss << " " << node->op.value << " ";
  node->right->accept(this);
  return {};
}

std::any SerializeVisitor::visit(ASTUnaryExpr *node) {
  ss << node->op.value;
  node->operand->accept(this);
  return {};
}
std::any SerializeVisitor::visit(ASTIdentifier *node) {
  ss << node->value;
  return {};
}
std::any SerializeVisitor::visit(ASTLiteral *node) {
  ss << node->value;
  return {};
}
std::any SerializeVisitor::visit(ASTType *node) {
  ss << node->base;
  return {};
}
std::string SerializeVisitor::getIndent() {
  return std::string(indentLevel * 2, ' ');
}
