#pragma once

#include <csetjmp>
#include <deque>
#include <set>
#include <vector>

#include "ast.hpp"
#include "builder.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"

struct VisitorBase {
  virtual ~VisitorBase() = default;
  void visit(ASTNoop *noop) { return; }

  virtual void visit(ASTPath *node) = 0;
  virtual void visit(ASTMethodCall *node) = 0;
  virtual void visit(ASTDyn_Of *node) = 0;
  virtual void visit(ASTPatternMatch *node) = 0;
  virtual void visit(ASTSize_Of *node) = 0;
  virtual void visit(ASTImport *node) = 0;
  virtual void visit(ASTCast *node) = 0;
  virtual void visit(ASTWhere *node) = 0;
  virtual void visit(ASTLambda *node) = 0;
  virtual void visit(ASTProgram *node) = 0;
  virtual void visit(ASTBlock *node) = 0;
  virtual void visit(ASTFunctionDeclaration *node) = 0;
  virtual void visit(ASTParamsDecl *node) = 0;
  virtual void visit(ASTParamDecl *node) = 0;
  virtual void visit(ASTVariable *node) = 0;
  virtual void visit(ASTExprStatement *node) = 0;
  virtual void visit(ASTBinExpr *node) = 0;
  virtual void visit(ASTUnaryExpr *node) = 0;
  virtual void visit(ASTLiteral *node) = 0;
  virtual void visit(ASTType *node) = 0;
  virtual void visit(ASTCall *node) = 0;
  virtual void visit(ASTArguments *node) = 0;
  virtual void visit(ASTReturn *node) = 0;
  virtual void visit(ASTContinue *node) = 0;
  virtual void visit(ASTBreak *node) = 0;
  virtual void visit(ASTFor *node) = 0;
  virtual void visit(ASTIf *node) = 0;
  virtual void visit(ASTElse *node) = 0;
  virtual void visit(ASTWhile *node) = 0;
  virtual void visit(ASTStructDeclaration *node) = 0;
  virtual void visit(ASTDotExpr *node) = 0;
  virtual void visit(ASTSubscript *node) = 0;
  virtual void visit(ASTInitializerList *node) = 0;
  virtual void visit(ASTEnumDeclaration *node) = 0;
  virtual void visit(ASTRange *node) = 0;
  virtual void visit(ASTSwitch *node) = 0;
  virtual void visit(ASTTuple *node) = 0;
  virtual void visit(ASTAlias *node) = 0;
  virtual void visit(ASTImpl *node) = 0;
  virtual void visit(ASTTupleDeconstruction *node) = 0;
  virtual void visit(ASTDefer *node) = 0;
  virtual void visit(ASTInterfaceDeclaration *node) = 0;
  virtual void visit(ASTChoiceDeclaration *node) = 0;
  virtual void visit(ASTModule *node) = 0;
  virtual void visit(ASTType_Of *node) = 0;
  virtual void visit(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return;
  };
};

struct Typer : VisitorBase {
  Nullable<ASTType> type_context = nullptr;
  bool in_call = false;
  int expected_type = -1;

  ASTDeclaration *visit_generic(ASTDeclaration *definition, std::vector<int> args, SourceRange source_range);

  Typer(Context &context) : ctx(context) {}
  Context &ctx;
  std::vector<TypeExtension> accept_extensions(std::vector<ASTTypeExtension> ast_extensions);
  std::string getIndent();

  int iterator_interface() {
    static int iter_id = ctx.scope->lookup("Iterator")->type_id;
    return iter_id;
  }
  int iterable_interface() {
    static int iterable_id = ctx.scope->lookup("Iterable")->type_id;
    return iterable_id;
  }
  int init_interface() {
    static int init_id = ctx.scope->lookup("Init")->type_id;
    return init_id;
  }

  int find_generic_type_of(const InternedString &base, const std::vector<int> &generic_args,
                           const SourceRange &source_range);

  void visit(ASTMethodCall *node) override;
  void visit(ASTPatternMatch *node) override;
  void visit(ASTPath *node) override;
  void visit(ASTDyn_Of *node) override;
  void visit(ASTModule *node) override;
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTImport *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;
  void visit(ASTVariable *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
  void visit(ASTLiteral *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTType *node) override;
  void visit(ASTInterfaceDeclaration *node) override;
  void visit(ASTSize_Of *node) override;
  void visit(ASTType_Of *node) override;

  std::vector<int> get_generic_arg_types(const std::vector<ASTExpr *> &args);
  // For generics.
  void visit_function_header(ASTFunctionDeclaration *node, bool generic_instantiation,
                             std::vector<int> generic_args = {});
  void visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                std::vector<int> generic_args = {});
  void visit_choice_declaration(ASTChoiceDeclaration *node, bool generic_instantiation,
                                      std::vector<int> generic_args = {});
  void visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<int> generic_args = {});
  void visit_interface_declaration(ASTInterfaceDeclaration *node, bool generic_instantiation,
                                   std::vector<int> generic_args = {});
  void visit_function_body(ASTFunctionDeclaration *node);

  int get_self_type();

  void type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, Nullable<ASTExpr> self);
  void type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info);
  ASTFunctionDeclaration *resolve_generic_function_call(ASTFunctionDeclaration *func, std::vector<ASTExpr*> *generic_args, ASTArguments *arguments, SourceRange range);

  void compiler_mock_method_call_visit_impl(int type, const InternedString &method_name);

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
  void visit(ASTChoiceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;
  bool visit_where_predicate(Type *type, ASTExpr *node);

  void compiler_mock_associated_function_call_visit_impl(int left_type, const InternedString &method_name);
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
  std::vector<InternedString> type_info_strings {};
  std::unordered_set<int> reflected_upon_types;
  DependencyEmitter *dep_emitter;
  
  static constexpr const char *defer_return_value_key = "$defer$return$value";
  bool has_user_defined_main = false;

  InternedString user_defined_entry_point;

  bool emit_default_init = true;
  bool emit_default_value = true;
  int num_tests = 0;

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

  StringBuilder code{};
  StringBuilder test_functions{};

  int indent_level = 0;
  Context &ctx;

  int type_list_id = -1;
  const bool is_freestanding =
      compile_command.c_flags.contains("-ffreestanding") || compile_command.c_flags.contains("-nostdlib");

  // TODO(Josh) 10/1/2024, 10:10:17 AM
  // This causes a lot of empty lines. It would be nice to have a way to neatly
  // do this.
  inline void emit_line_directive(ASTNode *node) {
    static int last_loc = -1;
    static bool is_release_or_omitting_line_info = compile_command.has_flag("release") || compile_command.has_flag("nl");
    if (is_release_or_omitting_line_info) {
      return;
    }
    auto loc = node->source_range.begin_location.line;
    // if (loc != last_loc) {
    auto filename = get_source_filename(node->source_range);
    if (filename.empty()) {
      printf("Empty filename for line directive.\n");
      return;
    }

    auto line = std::format("#line {} \"{}\"\n", loc, filename);
    code << line;
    last_loc = loc;
    // }
  }

  void emit_dyn_dispatch_object(int interface_type, int dyn_type);
  void emit_tuple(int type);
  std::string emit_symbol(Symbol *symbol);
  void emit_lambda(ASTLambda *node);
  void call_operator_overload(const SourceRange &range, Type *left_ty, OperationKind operation, TType op, ASTExpr *left,
                              ASTExpr *right = nullptr);

  void forward_decl_type(Type *type);
  void emit_deferred_statements(DeferBlockType type);

  std::string to_type_struct(Type *type, Context &context);
  Emitter(Context &context, Typer &type_visitor);
  inline std::string indent() { return std::string(indent_level * 2, ' '); }
  inline void indented(const std::string &s) { code << indent() << s; }
  inline void indentedln(const std::string &s) { code << indent() << s + '\n'; }
  inline void newline() { code << '\n'; }
  inline void newline_indented() { code << '\n' << indent(); }
  inline void semicolon() { code << ";"; }
  inline void space() { code << ' '; }

  void emit_forward_declaration(ASTFunctionDeclaration *node);
  void emit_foreign_function(ASTFunctionDeclaration *node);

  bool should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag);
  std::string to_cpp_string(const TypeExtensions &ext, const std::string &base);
  std::string to_cpp_string(Type *type);
  std::string get_cpp_scalar_type(int id);

  std::string get_type_struct(Type *type, int id, Context &context, const std::string &fields);
  std::string get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context);
  std::string get_elements_function(Type *type);

  std::string get_function_pointer_type_string(Type *type, Nullable<std::string> identifier = nullptr, bool type_erase_self = false);
  std::string get_declaration_type_signature_and_identifier(const std::string &name, Type *type);

  int get_expr_left_type_sr_dot(ASTNode *node);

  void visit(ASTMethodCall *node) override;
  void visit(ASTPatternMatch *node) override;
  void visit(ASTPath *node) override;
  void visit(ASTDyn_Of *node) override;
  void visit(ASTModule *node) override;
  void visit(ASTImport *node) override;
  void visit(ASTType_Of *node) override;
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;

  void visit(ASTVariable *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
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
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTChoiceDeclaration *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTInterfaceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;
  void visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return;
  };
  void emit_marker_choice_variant_instantiation(Type *type, const ASTPath *value);
  void emit_pattern_match_for_if(ASTIf *the_if, ASTPatternMatch *path);
  void emit_pattern_match_for_while(ASTWhile *the_while, ASTPatternMatch *path);
  void emit_pattern_match_destructure(ASTExpr *object, const std::string &variant_name, ASTPatternMatch *pattern, Type *variant_type);

  void emit_choice_tuple_variant_instantiation(ASTPath *path, ASTArguments *arguments);
  void emit_choice_struct_variant_instantation(ASTPath *path, ASTInitializerList *initializer);
};

struct DependencyEmitter : VisitorBase {
  Context &ctx;
  Emitter *emitter;
  std::set<ASTFunctionDeclaration *> visited_functions = {};
  std::unordered_set<int> reflected_upon_types;
  inline DependencyEmitter(Context &context, Emitter *emitter) : ctx(context), emitter(emitter) {}
  void visit_operator_overload(ASTExpr *base, const std::string &operator_name, ASTExpr *argument);

  void define_type(int type_id);
  void declare_type(int type_id);
  void visit(ASTMethodCall *node) override;
  void visit(ASTPath *node) override;
  void visit(ASTPatternMatch *node) override;
  void visit(ASTDyn_Of *node) override;
  void visit(ASTModule *node) override;
  void visit(ASTImport *node) override;
  void visit(ASTType_Of *node) override;
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;
  void visit(ASTVariable *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
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
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTChoiceDeclaration *node) override;
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