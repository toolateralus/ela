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
  ss << getIndent() << "Declaration " << node->name.value << " : ";
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
  ss << node->op.value;
  node->operand->accept(this);
  return {};
}
std::any SerializeVisitor::visit(ASTIdentifier *node) {
  ss << node->value;
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
  ss << node->base;
  ss << std::string(node->ptr_depth, '*');
  for (int i = 0; i < node->array_dims.size(); ++i) {
    if (node->array_dims[i] == -1) {
      ss << "[]";
    } else {
      ss << "[" << node->array_dims[i] << "]";
    }
  }
  return {};
}
std::any SerializeVisitor::visit(ASTArguments *node) {
  ss << getIndent() << "Arguments {";
  indentLevel++;
  bool first = true;
  for (auto arg : node->arguments) {
    if (!first) ss << ", ";
    first = false;
    arg->accept(this);
  }
  indentLevel--;
  ss << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTCall *node) {
  ss << getIndent() << "Call " << node->name.value << " {\n";
  indentLevel++;
  visit(node->arguments);
  indentLevel--;
  ss << getIndent() << "}\n";
  return {};
}
std::string SerializeVisitor::getIndent() {
  return std::string(indentLevel * 2, ' ');
}