#include "visitor.hpp"
#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include <any>
#include <jstl/containers/vector.hpp>

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
  
  if (node->block.is_not_null()) {
    context.enter_scope(node->block.get()->scope);
    visit(node->block.get());
    context.exit_scope();
  }
  
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

std::any SerializeVisitor::visit(ASTStructDeclaration *node) {
  ss << indent() << "Struct " << node->type->base << " {\n";
  indentLevel++;
  for (auto decl : node->declarations) {
    decl->accept(this);
  }
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}


std::any SerializeVisitor::visit(ASTDotExpr *node) {
  // todo:
  ss << "dot expr. todo\n";
  return {};
}
