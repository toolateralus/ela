#pragma once
#include "ast.hpp"
#include "core.hpp"
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
  std::stringstream ss{};
  int indentLevel = 0;
  Context &context;
  std::string indent();
  std::any visit(ASTProgram *node) override;
  std::any visit(ASTFunctionDeclaration *node) override;
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
  std::any visit(ASTStructDeclaration *node) override;
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
  std::any visit(ASTInitializerList *node) override;
  std::any visit(ASTEnumDeclaration *node) override;
  std::any visit(ASTUnionDeclaration *node) override;
  std::any visit(ASTAllocate *node) override;
};

struct TypeVisitor : VisitorBase {
  bool ignore_polymorphic_functions = true;
  bool within_dot_expression = false;
  int declaring_or_assigning_type = -1;
  
  void report_mutated_if_iden(ASTExpr *node);
  
  TypeVisitor(Context &context) : ctx(context) {}
  Context &ctx;
  std::string getIndent();
  std::any visit(ASTStructDeclaration *node) override;
  std::any visit(ASTProgram *node) override;
  std::any visit(ASTFunctionDeclaration *node) override;
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
  void find_function_overload(ASTCall *&node, Symbol *&symbol,
                              std::vector<int> &arg_tys, Type *&type);
  std::any visit(ASTCall *node) override;
  std::any visit(ASTArguments *node) override;
  std::any visit(ASTReturn *node) override;
  std::any visit(ASTContinue *node) override;
  std::any visit(ASTBreak *node) override;
  std::any visit(ASTFor *node) override;
  std::any visit(ASTIf *node) override;
  std::any visit(ASTElse *node) override;
  std::any visit(ASTWhile *node) override;
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
  std::any visit(ASTInitializerList *node) override;
  std::any visit(ASTEnumDeclaration *node) override;
  std::any visit(ASTUnionDeclaration *node) override;
  std::any visit(ASTAllocate *node) override;
  int generate_polymorphic_function(ASTCall *node,
                                    ASTFunctionDeclaration *func_decl,
                                    std::vector<int> arg_tys);
};

struct EmitVisitor : VisitorBase {
  bool within_dot_expression = false;
  bool emit_default_init = true;
  bool emit_default_args = false;
  int num_tests = 0;
  
  
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTUnionDeclaration> current_union_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;

  TypeVisitor &type_visitor;

  std::stringstream header{};
  std::stringstream code{};
  std::stringstream *ss{};
  std::stringstream test_functions{};

  int indentLevel = 0;
  Context &ctx;

  inline std::string get_code() const { return code.str(); }
  inline std::string get_header() const { return header.str(); }
  // TODO(Josh) 10/1/2024, 10:10:17 AM
  // This causes a lot of empty lines. It would be nice to have a way to neatly
  // do this.
  inline void emit_line_directive(ASTNode *node) {
    static int last_loc = -1;
    static bool is_debugging = get_compilation_flag("debug");
    if (!is_debugging) {
      return;
    }
    auto loc = node->source_range.begin_loc;
    if (loc != last_loc) {
      auto filename = get_source_filename(node->source_range);

      // !BUG: figure out why this is sometimes empty.
      if (filename.empty()) {
        return;
      }

      (*ss) << "\n#line " << std::to_string(loc) << " \"" << filename << "\"\n";
      last_loc = loc;
    }
  }
  inline void use_code() { ss = &code; }
  inline void use_header() { ss = &header; }
  inline EmitVisitor(Context &context, TypeVisitor &type_visitor)
      : ctx(context), type_visitor(type_visitor) {
    ss = &code;
  }
  inline std::string indent() { return std::string(indentLevel * 2, ' '); }
  inline void indented(const std::string &s) { (*ss) << indent() << s; }
  inline void indentedln(const std::string &s) {
    (*ss) << indent() << s << '\n';
  }
  inline void newline() { (*ss) << '\n'; }
  inline void newline_indented() { (*ss) << '\n' << indent(); }
  inline void semicolon() { (*ss) << ";"; }
  inline void space() { (*ss) << ' '; }
  void interpolate_string(ASTLiteral *node);
  void emit_local_function(ASTFunctionDeclaration *node);
  void emit_forward_declaration(ASTFunctionDeclaration *node);
  void emit_foreign_function(ASTFunctionDeclaration *node);
  void cast_pointers_implicit(ASTDeclaration *&node);

  bool should_emit_function(EmitVisitor *visitor, ASTFunctionDeclaration *node,
                            bool test_flag);

  std::string to_cpp_string(const TypeExt &ext, const std::string &base);

  // CLEANUP(Josh) 10/5/2024, 9:57:02 AM
  // This should be in the emit visitor not here.
  std::string to_cpp_string(Type *type);

  std::string get_cpp_scalar_type(int id);

  std::any visit(ASTStructDeclaration *node) override;
  std ::any visit(ASTProgram *node) override;
  std ::any visit(ASTBlock *node) override;
  std ::any visit(ASTFunctionDeclaration *node) override;
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
  std::any visit(ASTDotExpr *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
  std::any visit(ASTInitializerList *node) override;
  std::any visit(ASTEnumDeclaration *node) override;
  std::any visit(ASTUnionDeclaration *node) override;
  std::any visit(ASTAllocate *node) override;
};
