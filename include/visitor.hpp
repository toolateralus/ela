#pragma once

#include <csetjmp>
#include <vector>
#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

struct VisitorBase {
  ASTNode *parent_node = nullptr;
  ASTNode *parent_prev = nullptr;

  virtual ~VisitorBase() = default;
  void visit(ASTNoop *) { return; }

  virtual void visit(ASTRun *) = 0;
  virtual void visit(ASTWhereStatement *) = 0;
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
  virtual void visit(ASTForCStyle *node) = 0;
  virtual void visit(ASTIf *node) = 0;
  virtual void visit(ASTElse *node) = 0;
  virtual void visit(ASTWhile *node) = 0;
  virtual void visit(ASTStructDeclaration *node) = 0;
  virtual void visit(ASTDotExpr *node) = 0;
  virtual void visit(ASTIndex *node) = 0;
  virtual void visit(ASTInitializerList *node) = 0;
  virtual void visit(ASTEnumDeclaration *node) = 0;
  virtual void visit(ASTRange *node) = 0;
  virtual void visit(ASTSwitch *node) = 0;
  virtual void visit(ASTTuple *node) = 0;
  virtual void visit(ASTAlias *node) = 0;
  virtual void visit(ASTImpl *node) = 0;
  virtual void visit(ASTDestructure *node) = 0;
  virtual void visit(ASTDefer *node) = 0;
  virtual void visit(ASTTraitDeclaration *node) = 0;
  virtual void visit(ASTChoiceDeclaration *node) = 0;
  virtual void visit(ASTModule *node) = 0;
  virtual void visit(ASTType_Of *node) = 0;
  virtual void visit(ASTUnpack *node) = 0;
  virtual void visit(ASTUnpackElement *node) = 0;
  virtual void visit(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return;
  };
};

extern ASTVariable *g_testing_tests_declaration;
extern ASTFunctionDeclaration *g_testing_runner_declaration;
extern Type *g_testing_Test_type;

extern ASTStructDeclaration *g_InitList_declaration;
extern ASTStructDeclaration *g_Slice_declaration;
extern ASTStructDeclaration *g_SliceMut_declaration;
extern ASTChoiceDeclaration *g_Option_type;

extern Type *g_refl_Method_type, *g_refl_Field_type, *g_refl_Type_type;
extern Type *g_str_type, *g_String_type;
extern Type *g_Init_trait_type, *g_Iterable_trait_type, *g_Iterator_trait_type, *g_Destroy_trait_type;

struct Typer : VisitorBase {
  Nullable<ASTType> type_context = nullptr;
  Type *expected_type = Type::INVALID_TYPE;
  ASTDeclaration *visit_generic(ASTDeclaration *definition, std::vector<Type *> &args, Span span);
  Typer(Context &context) : ctx(context) {}
  Context &ctx;

  // This is strictly for unpack expressions right now,
  // collection initializers  '.[...tuple] '
  // other tuples             '(...tuple) '
  // arguments                'fn_call(...tuple)'
  Nullable<std::vector<ASTExpr *>> current_expression_list;

  std::vector<TypeExtension> accept_extensions(std::vector<ASTTypeExtension> ast_extensions);

  void implement_destroy_glue_for_choice_type(ASTChoiceDeclaration *choice, const bool generic_instantiation,
                                              const std::vector<Type *> generic_args = {});

  void visit_import_group(const ASTImport::Group &group, Scope *module_scope, Scope *import_scope,
                          const Span &range);

  Type *find_generic_type_of(const InternedString &base, std::vector<Type *> generic_args,
                             const Span &span);

  void visit_path(ASTPath *node, bool from_call = false);

  void visit(ASTWhereStatement *node) override;
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
  void visit(ASTForCStyle *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTType *node) override;
  void visit(ASTTraitDeclaration *node) override;
  void visit(ASTSize_Of *node) override;
  void visit(ASTType_Of *node) override;
  void visit(ASTUnpackElement *node) override;
  void visit(ASTRun *) override;

  std::vector<Type *> get_generic_arg_types(const std::vector<ASTExpr *> &args);

  // For generics.
  void visit_function_header(ASTFunctionDeclaration *node, bool visit_where_clause, bool generic_instantiation,
                             std::vector<Type *> generic_args);

  void visit_structural_type_declaration(ASTStructDeclaration *node);

  void visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                std::vector<Type *> generic_args = {});
  void visit_choice_declaration(ASTChoiceDeclaration *node, bool generic_instantiation,
                                std::vector<Type *> generic_args = {});
  void visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<Type *> generic_args = {});
  void visit_trait_declaration(ASTTraitDeclaration *node, bool generic_instantiation,
                               std::vector<Type *> generic_args = {});
  void visit_function_body(ASTFunctionDeclaration *node, bool macro_expansion);

  void expand_macro(ASTFunctionDeclaration *macro, ASTCall *originator);

  Type *get_self_type();

  void type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, ASTFunctionDeclaration *function,
                                   Nullable<ASTExpr> self, bool is_deinit_call);
  void type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info);
  ASTFunctionDeclaration *resolve_generic_function_call(ASTFunctionDeclaration *func,
                                                        std::vector<ASTExpr *> *generic_args, ASTArguments *arguments,
                                                        Span range);

  void compiler_mock_method_call_visit_impl(Type *type, const InternedString &method_name);
  void compiler_mock_associated_function_call_visit_impl(Type *left_type, const InternedString &method_name);

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
  void visit(ASTIndex *node) override;
  void visit(ASTInitializerList *node) override;
  void visit(ASTEnumDeclaration *node) override;
  void visit(ASTRange *node) override;
  void visit(ASTSwitch *node) override;
  void visit(ASTTuple *node) override;
  void visit(ASTDestructure *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTChoiceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;
  void visit(ASTUnpack *node) override;

  bool visit_where_predicate(Type *type, ASTExpr *node);
  bool visit_where_predicate_throws(Type *type, ASTExpr *node);

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

struct GenericInstantiationErrorUserData {
  std::string message = "";
  Span definition_range = {};
  jmp_buf save_state;
};

std::string get_temporary_variable();