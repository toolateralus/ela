#pragma once
#include "ast.hpp"
#include "scope.hpp"
#include <any>
#include <sstream>

struct VisitorBase {
  virtual ~VisitorBase() = default;
  DECLARE_VISIT_BASE_METHODS()
};


struct SerializeVisitor : VisitorBase {
  std::stringstream ss {};
  int indentLevel = 0;
  std::string indent();
  std::any visit(ASTProgram *node) override;
  std::any visit(ASTFuncDecl *node) override;
  std::any visit(ASTBlock *node) override;
  std::any visit(ASTParamsDecl *node) override;
  std::any visit(ASTParamDecl *node) override;
  std::any visit(ASTDeclaration *node) override;
  std::any visit(ASTExprStatement *node) override;
  std::any visit(ASTBinExpr *node) override;
  std::any visit(ASTUnaryExpr *node) override;
  std::any visit(ASTIdentifier *node) override;
  std::any visit(ASTLiteral *node) override;
  std::any visit(ASTType *node) override;
  std::any visit(ASTCall *node) override;
  std::any visit(ASTArguments *node) override;
  std::any visit(ASTReturn *node) override;
  std::any visit(ASTContinue *node) override;
  std::any visit(ASTBreak *node) override;
  std::any visit(ASTFor *node) override;
  std::any visit(ASTIf *node) override;
  std::any visit(ASTElse *node) override;
  std::any visit(ASTWhile *node) override;
};


struct TypeVisitor : VisitorBase {
  TypeVisitor(Context &context) : context(context){}
  Context &context;
  std::string getIndent();
  std::any visit(ASTProgram *node) override;
  std::any visit(ASTFuncDecl *node) override;
  std::any visit(ASTBlock *node) override;
  std::any visit(ASTParamsDecl *node) override;
  std::any visit(ASTParamDecl *node) override;
  std::any visit(ASTDeclaration *node) override;
  std::any visit(ASTExprStatement *node) override;
  std::any visit(ASTBinExpr *node) override;
  std::any visit(ASTUnaryExpr *node) override;
  std::any visit(ASTIdentifier *node) override;
  std::any visit(ASTLiteral *node) override;
  std::any visit(ASTType *node) override;
  std::any visit(ASTCall *node) override;
  std::any visit(ASTArguments *node) override;

  std::any visit(ASTReturn *node) override;
  std::any visit(ASTContinue *node) override;
  std::any visit(ASTBreak *node) override;

  std::any visit(ASTFor *node) override;
  std::any visit(ASTIf *node) override;
  std::any visit(ASTElse *node) override;
  std::any visit(ASTWhile *node) override;
};