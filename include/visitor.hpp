#pragma once
#include <any>
#include <sstream>

#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "string_builder.hpp"

struct VisitorBase {
  enum VisitorFlags {
    FLAG_NO_STATE = 0,
    FLAG_FUNCTION_ROOT_LEVEL_BLOCK = 1 << 2,
  };
  int visitor_flags = FLAG_NO_STATE;
  virtual ~VisitorBase() = default;
  DECLARE_VISIT_BASE_METHODS()

  virtual std::any visit(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return {};
  };
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
  std::any visit(ASTScopeResolution *node) override;
  std::any visit(ASTSubscript *node) override;
  std::any visit(ASTMake *node) override;
  std::any visit(ASTInitializerList *node) override;
  std::any visit(ASTEnumDeclaration *node) override;
  std::any visit(ASTUnionDeclaration *node) override;
  std::any visit(ASTAllocate *node) override;
  std::any visit(ASTTuple *node) override;


  // TODO: implement me.
  
  std::any visit(ASTRange *node) override { return {}; }
  std::any visit(ASTSwitch *node) override { return {}; };
  std::any visit(ASTTupleDeconstruction *node) override { return {}; };
};

struct Typer : VisitorBase {
  Nullable<Symbol> get_symbol(ASTNode *);

  int declaring_or_assigning_type = -1;

  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTUnionDeclaration> current_union_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;

  Typer(Context &context) : ctx(context) {}
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
  std::any visit(ASTScopeResolution *node) override;
  void find_function_overload(ASTCall *&node, Symbol *&symbol, std::vector<int> &arg_tys, Type *&type);
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
  std::any visit(ASTRange *node) override;
  std::any visit(ASTSwitch *node) override;
  std::any visit(ASTTuple *node) override;
  std::any visit(ASTTupleDeconstruction *node) override;
  InternedString type_name(ASTExpr *node);
};

struct Emitter : VisitorBase {
  bool has_user_defined_main = false;
  bool emit_default_init = true;
  bool emit_default_args = false;
  int num_tests = 0;

  std::vector<std::function<void()>> pending_statements;

  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTUnionDeclaration> current_union_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;

  Typer &type_visitor;

  StringBuilder code{};
  StringBuilder *ss{};
  StringBuilder test_functions{};

  int indentLevel = 0;
  Context &ctx;

  const bool is_freestanding = compile_command.compilation_flags.contains("-ffreestanding") ||
                               compile_command.compilation_flags.contains("-nostdlib");
                               
  // TODO(Josh) 10/1/2024, 10:10:17 AM
  // This causes a lot of empty lines. It would be nice to have a way to neatly
  // do this.
  inline void emit_line_directive(ASTNode *node) {
    static int last_loc = -1;
    static bool is_debugging = !compile_command.has_flag("release");
    if (!is_debugging) {
      return;
    }
    auto loc = node->source_range.begin_loc;
    if (loc != last_loc) {
      auto filename = get_source_filename(node->source_range);

      // !BUG: figure out why this is sometimes empty.
      if (filename.empty()) {
        // printf("Empty filename for line directive.\n");
        return;
      }

      (*ss) << std::string{"\n#line "} << std::to_string(loc) << std::string{" \""} << filename << std::string{"\"\n"};
      last_loc = loc;
    }
  }

  std::string to_type_struct(Type *type, Context &context);
  inline Emitter(Context &context, Typer &type_visitor) : type_visitor(type_visitor), ctx(context) { ss = &code; }
  inline std::string indent() { return std::string(indentLevel * 2, ' '); }
  inline void indented(const std::string &s) { (*ss) << indent() << s; }
  inline void indentedln(const std::string &s) { (*ss) << indent() << s + '\n'; }
  inline void newline() { (*ss) << '\n'; }
  inline void newline_indented() { (*ss) << '\n' << indent(); }
  inline void semicolon() { (*ss) << ";"; }
  inline void space() { (*ss) << ' '; }
  void interpolate_string(ASTLiteral *node);
  void emit_local_function(ASTFunctionDeclaration *node);
  void emit_forward_declaration(ASTFunctionDeclaration *node);
  void emit_foreign_function(ASTFunctionDeclaration *node);
  void cast_pointers_implicit(ASTDeclaration *&node);

  bool should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag);

  void emit_function_pointer_type_string(Type *type, Nullable<std::string> identifier = nullptr);
  std::string to_cpp_string(const TypeExt &ext, const std::string &base);
  std::string to_cpp_string(Type *type);
  std::string get_cpp_scalar_type(int id);

  std::string get_type_struct(Type *type, int id, Context &context, const std::string &fields);
  std::string get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context);
  std::string get_elements_function(Type *type);
  void emit_condition_block(ASTNode *node, const std::string &keyword, Nullable<ASTExpr> condition,
                            Nullable<ASTBlock> block);
  void emit_function_pointer_dynamic_array_declaration(const std::string &type_string, const std::string &name,
                                                       Type *type);
  void get_declaration_type_signature_and_identifier(const std::string &name, Type *type);

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
  std::any visit(ASTRange *node) override;
  std::any visit(ASTSwitch *node) override;
  std::any visit(ASTTuple *node) override;
  std::any visit(ASTTupleDeconstruction *node) override;
  std::any visit(ASTScopeResolution *node) override;

  std::any visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
      (*ss) << ";";
    }
    return {};
  };

};