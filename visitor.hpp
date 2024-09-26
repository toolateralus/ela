#pragma once
#include "ast.hpp"
#include "scope.hpp"
#include <any>
#include <sstream>

struct VisitorBase {
  
  enum VisitorFlags {
    FLAG_NO_STATE = 0,
    FLAG_VISITING_FUNCTION = 1 << 1,
    FLAG_FUNCTION_ROOT_LEVEL_BLOCK = 1 << 2,
  };
  
  int visitor_flags = FLAG_NO_STATE;
  
  virtual ~VisitorBase() = default;
  DECLARE_VISIT_BASE_METHODS()
};


struct SerializeVisitor : VisitorBase {
  SerializeVisitor(Context &context) : context(context) {}
  std::stringstream ss {};
  int indentLevel = 0;
  Context &context;
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
  std::any visit(ASTCompAssign *node) override;
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
  std::any visit(ASTCompAssign *node) override;
};


struct EmitVisitor : VisitorBase {
  EmitVisitor(Context &context) : context(context) {}
  
  inline void semicolon() {
    ss << ";\n";
  }
  
  std::stringstream ss {};
  int indentLevel = 0;
  Context &context;
  std::string indent() {
    return std::string(indentLevel * 2, ' ');
  }
  
  std ::any visit(ASTProgram *node) override;
  std ::any visit(ASTBlock *node) override;
  std ::any visit(ASTFuncDecl *node) override;
  std ::any visit(ASTParamsDecl *node) override;
  std ::any visit(ASTParamDecl *node) override;
  std ::any visit(ASTDeclaration *node) override;
  std ::any visit(ASTExprStatement *node) override;
  std ::any visit(ASTBinExpr *node) override;
  std ::any visit(ASTUnaryExpr *node) override;
  std ::any visit(ASTIdentifier *node) override;
  std ::any visit(ASTLiteral *node) override;
  std ::any visit(ASTType *node) override;
  std ::any visit(ASTCall *node) override;
  std ::any visit(ASTArguments *node) override;
  std ::any visit(ASTReturn *node) override;
  std ::any visit(ASTContinue *node) override;
  std ::any visit(ASTBreak *node) override;
  std ::any visit(ASTFor *node) override;
  std ::any visit(ASTIf *node) override;
  std ::any visit(ASTElse *node) override;
  std ::any visit(ASTWhile *node) override;
  std ::any visit(ASTCompAssign *node) override;
};