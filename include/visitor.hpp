#pragma once

#include <csetjmp>
#include <deque>
#include <vector>

#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"

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

  template <typename T> using VisitorMethod = void (Typer::*)(T, bool, std::vector<int>);
  template <typename T> T visit_generic(VisitorMethod<T> visit_method, T declaring_node, std::vector<int> args);

  Typer(Context &context) : ctx(context) {}
  Context &ctx;
  Nullable<Symbol> get_symbol(ASTNode *);
  std::vector<TypeExtension> accept_extensions(std::vector<ASTTypeExtension> ast_extensions);
  std::string getIndent();

  int find_generic_type_of(const InternedString &base, const std::vector<int> &generic_args,
                           const SourceRange &source_range);

 
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
  void visit(ASTSize_Of *node) override;

  std::vector<int> get_generic_arg_types(const std::vector<ASTType *> &args);
  // For generics.
  void visit_function_header(ASTFunctionDeclaration *node, bool generic_instantiation,
                                std::vector<int> generic_args = {});
  void visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                std::vector<int> generic_args = {});
  void visit_tagged_union_declaration(ASTTaggedUnionDeclaration *node, bool generic_instantiation,
                                      std::vector<int> generic_args = {});
  void visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<int> generic_args = {});
  void visit_interface_declaration(ASTInterfaceDeclaration *node, bool generic_instantiation,
                                   std::vector<int> generic_args = {});
  void visit_function_body(ASTFunctionDeclaration *node);

  int get_self_type();

  void type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, bool skip_first);
  void type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info);
  ASTFunctionDeclaration *resolve_generic_function_call(ASTCall *node, ASTFunctionDeclaration *func);

  void compiler_mock_function_call_visit_impl(int type, const InternedString &method_name);

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
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;
  bool visit_where_predicate(Type *type, ASTExpr *node);

  InternedString type_name(ASTExpr *node);
};

enum DeferBlockType {
  DEFER_BLOCK_TYPE_OTHER,
  DEFER_BLOCK_TYPE_FUNC,
  DEFER_BLOCK_TYPE_LOOP,
};

struct DeferBlock {
  std::vector<ASTDefer *> defers;
  DeferBlockType type;
};

struct DependencyEmitter;

struct Emitter : VisitorBase {
  DependencyEmitter *dependencyEmitter;
  static constexpr const char *defer_return_value_key = "$defer$return$value";
  bool has_user_defined_main = false;
  bool emit_default_init = true;
  bool emit_default_value = true;
  bool emit_default_args = false;
  int num_tests = 0;

  void emit_tuple_dependants(std::vector<int> &types);

  int cf_expr_return_id = 0;
  Nullable<std::string> cf_expr_return_register;

  Nullable<ASTType> type_context;

  Typer &typer;

  // used to cache up defers.
  bool emitting_function_with_defer = false;

  // the one at the top was the last one that was placed. we do this because you need to hit all the outer ones,
  // which will be done witha  fall thruogh on the labels,but you may want to skip some defers, say you never branched
  // into that block.
  std::deque<DeferBlock> defer_blocks{};

  std::stringstream code{};
  std::stringstream *ss{};
  std::stringstream test_functions{};

  int indent_level = 0;
  Context &ctx;

  int type_list_id = -1;
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
    auto loc = node->source_range.begin_location.line;
    if (loc != last_loc) {
      auto filename = get_source_filename(node->source_range);
      if (filename.empty()) {
        printf("Empty filename for line directive.\n");
        return;
      }
      (*ss) << std::string{"\n#line "} << std::to_string(loc) << std::string{" \""} << filename << std::string{"\"\n"};
      last_loc = loc;
    }
  }

  void call_operator_overload(const SourceRange& range, Type *left_ty, OperationKind operation, TType op, ASTExpr *left,
                              ASTExpr *right = nullptr);

  void emit_type_or_fwd_decl(Type* type);
  void forward_decl_type(Type* type);
  template <typename T> void emit_generic_instantiations(std::vector<GenericInstance<T>> instantiations);
  void emit_deferred_statements(DeferBlockType type);

  std::string to_type_struct(Type *type, Context &context);
  Emitter(Context &context, Typer &type_visitor);
  ~Emitter();
  inline std::string indent() { return std::string(indent_level * 2, ' '); }
  inline void indented(const std::string &s) { (*ss) << indent() << s; }
  inline void indentedln(const std::string &s) { (*ss) << indent() << s + '\n'; }
  inline void newline() { (*ss) << '\n'; }
  inline void newline_indented() { (*ss) << '\n' << indent(); }
  inline void semicolon() { (*ss) << ";"; }
  inline void space() { (*ss) << ' '; }
  
  void emit_forward_declaration(ASTFunctionDeclaration *node);
  void emit_foreign_function(ASTFunctionDeclaration *node);

  bool should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag);
  std::string to_cpp_string(const TypeExtensions &ext, const std::string &base);
  std::string to_cpp_string(Type *type);
  std::string get_cpp_scalar_type(int id);

  std::string get_type_struct(Type *type, int id, Context &context, const std::string &fields);
  std::string get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context);
  std::string get_elements_function(Type *type);

  std::string get_function_pointer_type_string(Type *type, Nullable<std::string> identifier = nullptr);
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
  void visit(ASTSize_Of *node) override;
  void visit(ASTScopeResolution *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTTaggedUnionDeclaration *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTInterfaceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;

  void visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      emit_line_directive(stmt);
      stmt->accept(this);
      (*ss) << ";";
    }
    return;
  };
}; 

struct DependencyEmitter : VisitorBase {
  Context &ctx;
  Emitter *emitter;
  inline DependencyEmitter(Context &context, Emitter *emitter) : ctx(context), emitter(emitter) {}

  void emit_type(int type_id);

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
  void visit(ASTSize_Of *node) override;
  void visit(ASTScopeResolution *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTTaggedUnionDeclaration *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTInterfaceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;
  void visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
  };
}; 

struct GenericInstantiationErrorUserData {
  std::string message = "";
  SourceRange definition_range = {};
  jmp_buf save_state;
};