#include "visitor.hpp"
#include "ast.hpp"
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
  
  if ((node->flags & FUNCTION_IS_CTOR) != 0 || (node->flags & FUNCTION_IS_DTOR) != 0) {
    ss << indent() << "constructor: ";
    node->params->accept(this);
    return {};
  }
  
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
  
  context.enter_scope(node->scope);
  
  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method: node->methods) {
    method->accept(this);
  }
  
  context.exit_scope();
  
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}

std::any SerializeVisitor::visit(ASTDotExpr *node) {
  ss << indent() << "DotExpr {\n";
  indentLevel++;
  ss << indent() << "object: ";
  node->left->accept(this);
  ss << '\n' << indent() << "member: ";
  node->right->accept(this);
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}



/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
// {

// clang-format off
std::any ASTProgram::accept(VisitorBase *visitor) {return visitor->visit(this); }
std::any ASTBlock::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTType::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTExprStatement::accept(VisitorBase *visitor) {return visitor->visit(this); }
std::any ASTDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTBinExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTUnaryExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIdentifier::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTLiteral::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTParamDecl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTParamsDecl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTFuncDecl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCall::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTArguments::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTReturn::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTBreak::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTContinue::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTFor::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIf::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTElse::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTWhile::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCompAssign::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTStructDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTDotExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTSubscript::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTMake::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTInitializerList::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}

// clang-format on
// }
/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/

std::any SerializeVisitor::visit(ASTSubscript *node) {
  node->left->accept(this);
  ss << '[';
  node->subscript->accept(this);
  ss << ']';
  return {};
}

std::any SerializeVisitor::visit(ASTMake *node) {
  ss << "make: \n";
  indentLevel++;
  ss << "type: \n " << indent();
  node->type_arg->accept(this);
  
  ss << "args: \n" << indent();
  node->arguments->accept(this);
  indentLevel--;
  
  return {};
}
std::any SerializeVisitor::visit(ASTInitializerList *node) {
  ss << "init list: {";
  for(const auto &expr: node->expressions)  {
    expr->accept(this);
    ss << ", ";
  }
  ss << "}\n";
  return {};
}
