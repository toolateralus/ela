#include "visitor.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "type.hpp"
#include <any>

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
  auto type = global_get_type(node->return_type);
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
std::any SerializeVisitor::visit(ASTFunctionDeclaration *node) {

  if ((node->flags & FUNCTION_IS_CTOR) != 0 ||
      (node->flags & FUNCTION_IS_DTOR) != 0) {
    ss << indent() << "constructor: ";
    node->params->accept(this);
    return {};
  }

  ss << indent() << "Function " << node->name.value.get_str() << " {\n";
  indentLevel++;
  auto sym = context.scope->lookup(node->name.value);
  // ss << indent() << "type: " << global_get_type(sym->type_id)->to_string() <<
  // '\n';
  visit(node->params);

  if (node->block.is_not_null()) {
    context.set_scope(node->block.get()->scope);
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
  ss << indent() << "Parameter " << node->name.get_str() << " : ";
  node->type->accept(this);
  ss << "\n";
  return {};
}
std::any SerializeVisitor::visit(ASTDeclaration *node) {
  ss << indent() << "Declaration " << node->name.value.get_str() << " : ";
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
  ss << " " << node->op.value.get_str() << " ";
  node->right->accept(this);
  ss << ")";
  return {};
}
std::any SerializeVisitor::visit(ASTUnaryExpr *node) {
  ss << indent() << "unary: ";
  ss << node->op.value.get_str();
  node->operand->accept(this);
  return {};
}
std::any SerializeVisitor::visit(ASTIdentifier *node) {
  ss << node->value.get_str();
  return {};
}
std::any SerializeVisitor::visit(ASTLiteral *node) {
  if (node->tag == ASTLiteral::String) {
    ss << '\"' << node->value.get_str() << '\"';
  } else
    ss << node->value.get_str();
  return {};
}
std::any SerializeVisitor::visit(ASTType *node) {
  if (node->resolved_type != -1) {
    auto type = global_get_type(node->resolved_type);
    ss << "type: " << node->resolved_type << ", " << type->get_base().get_str()
       << " " << type->get_ext().to_string();
    return {};
  }
  ss << node->base.get_str();
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
  ss << indent() << "Call " << node->function
     << " type: " << global_get_type(node->type)->to_string() << " {\n";
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
  node->iden->accept(this);
  ss << " in ";
  node->range->accept(this);
  ss << '\n';
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
std::any SerializeVisitor::visit(ASTStructDeclaration *node) {
  auto t = global_get_type(node->type->resolved_type);
  auto info = (t->get_info()->as<StructTypeInfo>());
  const auto is_anonymous = (info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0;

  if (!is_anonymous) {
    ss << indent() << "Struct ";
    ss << node->type->base.get_str();
    ss << " {\n";
  } else {
    ss << indent() << "anonymous struct" << '\n';
  }
  indentLevel++;

  context.set_scope(node->scope);

  for (auto decl : node->fields) {
    decl->accept(this);
  }
  for (auto method : node->methods) {
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
  ss << indent() << "base: ";
  node->base->accept(this);
  ss << '\n' << indent() << "name: ";
  ss << node->member_name.get_str() << '\n';
  indentLevel--;
  ss << indent() << "}\n";
  return {};
}
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
  for (const auto &expr : node->expressions) {
    expr->accept(this);
    ss << ", ";
  }
  ss << "}\n";
  return {};
}

std::any SerializeVisitor::visit(ASTEnumDeclaration *node) {
  ss << "enum : ";
  ss << node->type->base.get_str();
  for (const auto &[key, value] : node->key_values) {
    ss << "\nkey: " << key.get_str();
    ss << "value: ";
    if (value.is_not_null())
      value.get()->accept(this);
  }
  return {};
}

/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
// {

// clang-format off
std::any ASTSwitch::accept(VisitorBase *visitor) { return visitor->visit(this); }
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
std::any ASTFunctionDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTTuple::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCall::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTArguments::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTReturn::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTBreak::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTContinue::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTFor::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIf::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTUnionDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTNoop::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTElse::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTWhile::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTStructDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTDotExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTScopeResolution::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTSubscript::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTMake::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTEnumDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTInitializerList::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTAllocate::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTRange::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTTupleDeconstruction::accept(VisitorBase *visitor) { return visitor->visit(this); }

// clang-format on
// }
/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/

std::any SerializeVisitor::visit(ASTUnionDeclaration *node) {
  ss << "union : ";
  context.set_scope(node->scope);
  for (const auto &field : node->fields) {
    field->accept(this);
  }
  for (const auto &method : node->methods) {
    method->accept(this);
  }
  context.exit_scope();
  return {};
}

std::any SerializeVisitor::visit(ASTAllocate *node) {
  ss << "allocation: ";
  if (node->type)
    node->type.get()->accept(this);
  if (node->arguments)
    node->arguments.get()->accept(this);
  return {};
}

std::any SerializeVisitor::visit(ASTScopeResolution *node) {
  node->base->accept(this);
  ss << "::" << node->member_name.get_str();
  return {};
}


// TODO: implement me. Im lazy and this takes a while and uses up my hands!
std::any SerializeVisitor::visit(ASTTuple *node) { return {}; }
std::any ASTStatementList::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
ASTNodeType ASTStatementList::get_node_type() const {
  return AST_NODE_STATEMENT_LIST;
}
