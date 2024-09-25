#pragma once
#include "ast.hpp"
#include <any>
#include <sstream>

struct VisitorBase {
  virtual ~VisitorBase() = default;
  virtual std ::any visit(ASTProgram *node) = 0;
  virtual std ::any visit(ASTBlock *node) = 0;
  virtual std ::any visit(ASTFuncDecl *node) = 0;
  virtual std ::any visit(ASTParamsDecl *node) = 0;
  virtual std ::any visit(ASTParamDecl *node) = 0;
  virtual std ::any visit(ASTDeclaration *node) = 0;
  virtual std ::any visit(ASTExprStatement *node) = 0;
  virtual std ::any visit(ASTBinExpr *node) = 0;
  virtual std ::any visit(ASTUnaryExpr *node) = 0;
  virtual std ::any visit(ASTIdentifier *node) = 0;
  virtual std ::any visit(ASTLiteral *node) = 0;
  virtual std ::any visit(ASTType *node) = 0;
};


// Defined in serialize_visitor.cpp
struct SerializeVisitor : VisitorBase {
  std::stringstream ss {};
  int indentLevel = 0;
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
};