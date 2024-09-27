#include "core.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <jstl/containers/vector.hpp>

std::any EmitVisitor::visit(ASTCompAssign *node) {
  (*ss) <<indent() << node->name.value << " ";
  (*ss) <<node->op.value << " ";
  node->expr->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTWhile *node) {
  (*ss) <<indent() << "while ";
  if (node->condition.is_not_null()) {
    (*ss) <<"(";
    node->condition.get()->accept(this);
    (*ss) <<")";
  } else {
    (*ss) <<"(true)";
  }
  node->block->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTElse *node) {
  (*ss) <<" else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTIf *node) {
  (*ss) <<indent() << "if (";
  node->condition->accept(this);
  (*ss) <<")";
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  
  return {};
}

std::any EmitVisitor::visit(ASTFor *node) {
  const auto emit_range_based = [&] {
    auto v = node->value.range_based;
    (*ss) <<"auto ";
    v.target->accept(this);
    (*ss) <<" : ";
    v.collection->accept(this);
  };
  
  const auto emit_c_style = [&] {
    auto v = node->value.c_style;
    v.decl->accept(this);
    semicolon();
    space();
    v.condition->accept(this);
    semicolon();
    space();
    v.increment->accept(this);
  };
  
  (*ss) <<indent() << "for (";
  switch (node->tag) {
  case ASTFor::RangeBased:
    emit_range_based();
    break;
  case ASTFor::CStyle:
    emit_c_style();
    break;
  }
  (*ss) <<")";
  node->block->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTBreak *node) {
  indented("break");
  return {};  
}

std::any EmitVisitor::visit(ASTContinue *node) {
  indented("continue");
  return {};
}

std::any EmitVisitor::visit(ASTReturn *node) {
  indented("return");
  if (node->expression.is_not_null()) {
    space();
    node->expression.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTArguments *node) {
  (*ss) <<"(";
  for (int i = 0; i < node->arguments.size(); ++i) {
    node->arguments[i]->accept(this);
    if (i != node->arguments.size() - 1) {
      (*ss) <<", ";
    }
  }
  (*ss) <<")";
  return {};
}

std::any EmitVisitor::visit(ASTType *node) {
  (*ss) <<get_type(node->resolved_type)->to_cpp_string();
  return {};
}

std::any EmitVisitor::visit(ASTCall *node) {
  (*ss) <<node->name.value;
  node->arguments->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTLiteral *node) {
  if (node->tag == ASTLiteral::Null) {
    (*ss) <<"nullptr";
  } else if (node->tag == ASTLiteral::String) {
    (*ss) <<std::format("\"{}\"", node->value);
  } else if (node->tag == ASTLiteral::RawString) {
    // TODO: search for a pattern '__()__' for example, that doesn't exist at all in the string.
    (*ss) <<std::format("R\"__({})__\"", node->value);
  } else {
    (*ss) <<node->value;
  }
  return {};
}

std::any EmitVisitor::visit(ASTIdentifier *node) {
  (*ss) <<node->value.value;
  return {};
}

std::any EmitVisitor::visit(ASTUnaryExpr *node) {
  (*ss) <<node->op.value;
  node->operand->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTBinExpr *node) {
  // TODO: Figure out how we want to control custom precedence. Right now,
  // TODO(cont): We'll just parenthesize every single sub-expression;
  (*ss) <<"(";
  auto left = node->left->accept(this);
  space();
  (*ss) <<node->op.value;; 
  space();
  auto right = node->right->accept(this);
  (*ss) <<")";
  return {};
}

std::any EmitVisitor::visit(ASTExprStatement *node) {
  (*ss) <<indent();
  node->expression->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTDeclaration *node) {
  node->type->accept(this);
  space();
  (*ss) <<node->name.value;
  space();
  if (node->value.is_not_null()) {
    (*ss) <<" = ";
    node->value.get()->accept(this);
  } else {
    (*ss) <<"{}";
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamDecl *node) {
  node->type->accept(this);
  (*ss) <<' ' << node->name;
  if (node->default_value.is_not_null()) {
    (*ss) <<" = ";
    node->default_value.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamsDecl *node) {
  (*ss) <<" (";
  int i = 0;
  for (const auto &param : node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      (*ss) <<", ";
    }
    ++i;
  }
  (*ss) <<")";
  return {};
}

std::any EmitVisitor::visit(ASTFuncDecl *node) {
  
  auto test_flag = get_compilation_flag("test");
  
  if (!test_flag && node->flags == FUNCTION_TEST) {
    return {};
  } else if (test_flag && node->flags == FUNCTION_TEST) {
    test_functions << "_test(\"" << node->name.value << "\", " << node->name.value << "),";
  }
  
  if (test_flag && node->name.value == "main") {
    return {};
  }
  
  auto symbol = context.current_scope->lookup(node->name.value);
  
  // we override main's return value to allow compilation without explicitly returning int from main.
  if (node->name.value == "main") {
    (*ss) <<"int";
  } else {
    node->return_type->accept(this);
  }
  
  space();
  
  (*ss) <<node->name.value;
  node->params->accept(this);  
  node->block->accept(this);
  
  if (node->name.value != "main") {
    use_header();
    node->return_type->accept(this);
    (*ss) << ' ' <<node->name.value << ' ';
    node->params->accept(this);
    (*ss) << ";\n";
    use_code();
  }
  
  
  return {};
}

std::any EmitVisitor::visit(ASTBlock *node) {
  (*ss) <<(" {\n");
  indentLevel++;
  context.enter_scope(node->scope);
  for (const auto &statement : node->statements) {
    if (dynamic_cast<ASTDeclaration*>(statement)){
      indented("");
    }
    
    statement->accept(this);
    semicolon();
    newline();
  }
  indentLevel--;
  indented("}");
  context.exit_scope();
  return {};
}

std::any EmitVisitor::visit(ASTProgram *node) {
  const auto testing = get_compilation_flag("test");
  
  header <<R"_(#include "boilerplate.hpp")_" << '\n';
  
  
  for (const auto &statement : node->statements) {
    statement->accept(this);
    semicolon();
    newline();
  }
  
  if (testing) {
    auto test_init = test_functions.str();
    if (test_init.ends_with(',')) {
      test_init.pop_back();
    }
    code << "const jstl::Vector<_test> tests" << "{" << test_init << "};\n";
    code << "__TEST_RUNNER_MAIN;";
  }
  
  return {};
}