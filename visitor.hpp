#pragma once
#include "ast.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include <any>
#include <sstream>

struct VisitorBase {
  enum VisitorFlags {
    FLAG_NO_STATE = 0,
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
  std::any visit(ASTStructDeclaration *node) override;
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
};

struct TypeVisitor : VisitorBase {
  TypeVisitor(Context &context) : context(context){}
  Context &context;
  std::string getIndent();
  std::any visit(ASTStructDeclaration *node) override;
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
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
  
};


struct EmitVisitor : VisitorBase {
  bool emit_default_args = false;
  int num_tests = 0;

  Nullable<ASTStructDeclaration> current_struct_decl;
  
  TypeVisitor &type_visitor;
  
  std::stringstream header {};
  std::stringstream code {};
  std::stringstream *ss {};
  std::stringstream test_functions {};
  
  int indentLevel = 0;
  Context &context;

  std::string get_code() const {
    return code.str();
  }
  std::string get_header() const {
    return header.str();
  }
  
  void use_code() {
    ss = &code;
  }
  void use_header() {
    ss = &header;
  }
  
  EmitVisitor(Context &context, TypeVisitor &type_visitor) : context(context), type_visitor(type_visitor) {
    ss = &code;
  }
  std::string indent() {
    return std::string(indentLevel * 2, ' ');
  }  
  inline void indented(const std::string &s) {
    (*ss) <<indent() << s;
  }
  inline void indentedln(const std::string &s) {
    (*ss) <<indent() << s << '\n';
  }
  inline void newline() {
    (*ss) <<'\n';
  }
  inline void newline_indented() {
    (*ss) <<'\n' << indent();
  }
  inline void semicolon() {
    (*ss) <<";";
  }
  inline void space() {
    (*ss) <<' ';
  }

  void emit_local_function(ASTFuncDecl *node);
  void emit_forward_declaration(ASTFuncDecl *node);
  void emit_foreign_function(ASTFuncDecl * node);
  std::any visit(ASTStructDeclaration *node) override;
  std ::any visit(ASTProgram *node) override;
  std ::any visit(ASTBlock *node) override;
  std ::any visit(ASTFuncDecl *node) override;
  std ::any visit(ASTParamsDecl *node) override;
  std ::any visit(ASTParamDecl *node) override;
  void cast_pointers_implicit(ASTDeclaration *&node);
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
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
};