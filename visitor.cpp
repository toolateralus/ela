#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"

std::any SerializeVisitor::visit(ASTProgram *node) {
  printf("%s\n", typeid(node).name());
  
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
  if (node->resolved_type != -1) {
    auto type = get_type(node->resolved_type);
    ss << "type: " << node->resolved_type << ", " << type->name << type->type_extensions.to_string();
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
    if (!first) ss << ", ";
    first = false;
    arg->accept(this);
  }
  indentLevel--;
  ss << "}\n";
  return {};
}
std::any SerializeVisitor::visit(ASTCall *node) {
  ss << indent() << "Call " << node->name.value << " {\n";
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
  for (auto &statement: node->statements) {
    statement->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTFuncDecl *node) {
  // TODO: we need to store this in the symbol table.
  node->return_type->accept(this);
  node->params->accept(this);
  node->block->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBlock *node) {
  context.enter_scope(node->scope);
  for (auto &statement: node->statements) {
    statement->accept(this);
  }
  context.exit_scope();
  return {};
}
std::any TypeVisitor::visit(ASTParamsDecl *node) {
  for (auto &param: node->params) {
    param->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTParamDecl *node) {
  node->type->accept(this);
  
  if (node->default_value.is_not_null()) {
    auto expr_type = int_from_any(node->default_value.get()->accept(this));
    if (expr_type != node->type->resolved_type) {
      throw_error({
        .message = std::format("Incompatible types in expression. declaring: {}  provided {}", get_type(node->type->resolved_type)->name, get_type(expr_type)->name),
        .severity = ERROR_FAILURE,
      });
    }
  }
  return {};
}
std::any TypeVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);
  
  if (node->value.is_not_null()) {
    auto expr_type = int_from_any(node->value.get()->accept(this));
    
    if (expr_type != node->type->resolved_type) {
      auto tleft = get_type(node->type->resolved_type), tright = get_type(expr_type);
      throw_error({
        .message = std::format("Incompatible types in expression. declaring: {}  provided {}", tleft->name, tright->name),
        .severity = ERROR_FAILURE,
      });
    }
  }
  
  // TODO: probably want something a bit nicer than this.
  context.current_scope->lookup(node->name.value)->type_id = node->type->resolved_type;
  return {};
} 
std::any TypeVisitor::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTBinExpr *node) {
  auto left = int_from_any(node->left->accept(this));
  auto right = int_from_any(node->left->accept(this));
  // TODO: type check in accordance to which operators are permitted to be used on this type;
  // TODO: type convert certain operations where neccesary, like == returns a boolean regardless of it's operands types.  
  auto tleft = get_type(left), tright = get_type(right);
  
  if (left != right && type_conversion_rule(tleft, tright) == CONVERT_PROHIBITED) {
    throw_error({
      .message = std::format("binary expression {} with left: {}, right {}, is invalid due to their types.", node->op.value, tleft->name, tright->name),
      .severity = ERROR_FAILURE,
    });
  }
  
  // for now we just return the lhs.
  return tleft;
}
std::any TypeVisitor::visit(ASTUnaryExpr *node) {
  // TODO: type convert certain operations where neccesary, like ! returns a boolean regardless of it's operands types.
  return node->operand->accept(this);
}
std::any TypeVisitor::visit(ASTIdentifier *node) {
  auto symbol = context.current_scope->lookup(node->value);
  return symbol->type_id;
}
std::any TypeVisitor::visit(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer:
      return find_type_id("i32", {});
    case ASTLiteral::Float:
      return find_type_id("f32", {});
    case ASTLiteral::String:
      return find_type_id("string", {});
    break;
  }
}
std::any TypeVisitor::visit(ASTCall *node) {
  // TODO: add functions to symbol table, perform a lookup here and return the return type.
  return {};
}
std::any TypeVisitor::visit(ASTArguments *node) {
  // TODO: what do we do here?
  for (auto arg: node->arguments) {
    arg->accept(this);
  }
  return {};
}
std::any TypeVisitor::visit(ASTReturn *node) {
  if (node->expression.is_not_null())
    node->expression.get()->accept(this);
  return {};
}
std::any TypeVisitor::visit(ASTContinue *node) {
  return {};
}
std::any TypeVisitor::visit(ASTBreak *node) {
  return {};
}

