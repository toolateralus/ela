#pragma once

#include <deque>

#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "string_builder.hpp"

struct VisitorBase {
  virtual ~VisitorBase() = default;
  DECLARE_VISIT_BASE_METHODS()
  virtual void visit(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return;
  };
};

struct Typer : VisitorBase {

  Nullable<ASTType> type_context = nullptr;
  int current_block_statement_idx;
  int declaring_or_assigning_type = -1;

  template <typename T>
  int visit_generic(int (Typer::*visit_method)(T *, bool, std::vector<int>), ASTNode *declaring_node,
                    std::vector<int> args);

  Typer(Context &context) : ctx(context) {}
  Context &ctx;
  Nullable<Symbol> get_symbol(ASTNode *);
  std::vector<TypeExtension> accept_extensions(std::vector<ASTTypeExtension> ast_extensions);
  std::string getIndent();
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;
  void visit(ASTDeclaration *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
  void visit(ASTIdentifier *node) override;
  void visit(ASTLiteral *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTType *node) override;
  void visit(ASTScopeResolution *node) override;
  void visit(ASTInterfaceDeclaration *node) override;

  std::vector<int> get_generic_arg_types(const std::vector<ASTType *> &args);
  // For generics.
  int visit_function_signature(ASTFunctionDeclaration *node, bool generic_instantiation,
                                 std::vector<int> generic_args = {});
  int visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                               std::vector<int> generic_args = {});
  int visit_impl_declaration(ASTImpl *node, bool generic_instantiation,
                              std::vector<int> generic_args = {});
  int visit_interface_declaration(ASTInterfaceDeclaration *node, bool generic_instantiation,
                                   std::vector<int> generic_args = {});
  void visit_function_body(ASTFunctionDeclaration *node, int return_type);

  int get_self_type();

  void type_check_arguments(ASTCall *&node, Type *&type, bool &method_call, FunctionTypeInfo *&info);
  void resolve_generic_function_call(ASTCall *node, Type *&type, ASTFunctionDeclaration *func);
  void try_visit_impl_on_call(ASTCall *&node, ASTNodeType &func_node_type);

  void visit(ASTCall *node) override;
  void visit(ASTArguments *node) override;
  void visit(ASTReturn *node) override;
  void visit(ASTContinue *node) override;
  void visit(ASTBreak *node) override;
  void visit(ASTFor *node) override;
  void visit(ASTIf *node) override;
  void visit(ASTElse *node) override;
  void visit(ASTWhile *node) override;
  void visit(ASTDotExpr *node) override;
  void visit(ASTSubscript *node) override;
  void visit(ASTInitializerList *node) override;
  void visit(ASTEnumDeclaration *node) override;
  void visit(ASTRange *node) override;
  void visit(ASTSwitch *node) override;
  void visit(ASTTuple *node) override;
  void visit(ASTTupleDeconstruction *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTTaggedUnionDeclaration *node) override;

  InternedString type_name(ASTExpr *node);
};

struct Emitter : VisitorBase {

  void emit_deferred_statements(ASTBlock *parent, bool is_return);
  static constexpr const char * defer_return_value_key = "$defer$return$value";
  bool has_user_defined_main = false;
  bool emit_default_init = true;
  bool emit_default_args = false;
  std::vector<int> generic_arguments;
  int num_tests = 0;

  std::vector<std::function<void()>> pending_statements;

  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;
  Nullable<ASTImpl> current_impl = nullptr;

  Typer &typer;

  // used to cache up defers.
  bool emitting_function_with_defer = false;

  // the one at the top was the last one that was placed. we do this because you need to hit all the outer ones,
  // which will be done witha  fall thruogh on the labels,but you may want to skip some defers, say you never branched into that block.
  std::deque<std::stringstream> defer_blocks {};


  std::stringstream code{};
  std::stringstream *ss{};
  std::stringstream test_functions{};

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
  inline Emitter(Context &context, Typer &type_visitor) : typer(type_visitor), ctx(context) { ss = &code; }
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
  std::string to_cpp_string(const TypeExtensions &ext, const std::string &base);
  std::string to_cpp_string(Type *type);
  std::string get_cpp_scalar_type(int id);

  std::string get_type_struct(Type *type, int id, Context &context, const std::string &fields);
  std::string get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context);
  std::string get_elements_function(Type *type);

  void emit_condition_block(ASTNode *node, const std::string &keyword, Nullable<ASTExpr> condition,
                            Nullable<ASTBlock> block);

  std::string get_function_pointer_type_string(Type *type, Nullable<std::string> identifier = nullptr);
  std::string get_function_pointer_dynamic_array_declaration(const std::string &type_string, const std::string &name,
                                                             Type *type);
  std::string get_declaration_type_signature_and_identifier(const std::string &name, Type *type);
  int get_expr_left_type_sr_dot(ASTNode *node);
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;

  void visit(ASTDeclaration *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
  void visit(ASTIdentifier *node) override;
  void visit(ASTLiteral *node) override;
  void visit(ASTType *node) override;
  void visit(ASTCall *node) override;
  void visit(ASTArguments *node) override;
  void visit(ASTReturn *node) override;
  void visit(ASTContinue *node) override;
  void visit(ASTBreak *node) override;
  void visit(ASTFor *node) override;
  void visit(ASTIf *node) override;
  void visit(ASTElse *node) override;
  void visit(ASTWhile *node) override;
  void visit(ASTDotExpr *node) override;
  void visit(ASTSubscript *node) override;
  void visit(ASTInitializerList *node) override;
  void visit(ASTEnumDeclaration *node) override;
  void visit(ASTRange *node) override;
  void visit(ASTSwitch *node) override;
  void visit(ASTTuple *node) override;
  void visit(ASTTupleDeconstruction *node) override;
  void visit(ASTScopeResolution *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTTaggedUnionDeclaration *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTInterfaceDeclaration *node) override;

  void visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      emit_line_directive(stmt);
      stmt->accept(this);
      (*ss) << ";";
    }
    return;
  };
};