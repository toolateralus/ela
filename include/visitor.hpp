#pragma once

#include <csetjmp>
#include <vector>
#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"

struct VisitorBase {
  virtual ~VisitorBase() = default;
  void visit(ASTNoop *) { return; }

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
  virtual void visit(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
    return;
  };
};

struct Typer : VisitorBase {
  Nullable<ASTType> type_context = nullptr;
  Type *expected_type = Type::INVALID_TYPE;
  ASTDeclaration *visit_generic(ASTDeclaration *definition, std::vector<Type *> &args, SourceRange source_range);
  Typer(Context &context) : ctx(context) {}
  Context &ctx;
  std::vector<TypeExtension> accept_extensions(std::vector<ASTTypeExtension> ast_extensions);
  std::string getIndent();
  void visit_path_for_call(ASTPath *node);

  Type *type_ptr_list = nullptr;
  Type *method_list = nullptr;
  Type *field_list = nullptr;


  /*
    TODO:
    Why are these in the typer and not just globally available?
    they're static and lazily evaluated, and these things are all
    from the stdlib, which is in Context::root_scope, which is also
    statically available I think.
  */
  Type *iterator_trait() {
    static Type *iter_id = ctx.scope->lookup("Iterator")->resolved_type;
    return iter_id;
  }

  Type *iterable_trait() {
    static Type *iterable_id = ctx.scope->lookup("Iterable")->resolved_type;
    return iterable_id;
  }

  Type *init_trait() {
    static Type *init_id = ctx.scope->lookup("Init")->resolved_type;
    return init_id;
  }

  Type *find_generic_type_of(const InternedString &base, std::vector<Type *> generic_args,
                             const SourceRange &source_range);

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
  void visit(ASTCast *node) override;
  void visit(ASTType *node) override;
  void visit(ASTTraitDeclaration *node) override;
  void visit(ASTSize_Of *node) override;
  void visit(ASTType_Of *node) override;

  std::vector<Type *> get_generic_arg_types(const std::vector<ASTExpr *> &args);

  // For generics.
  void visit_function_header(ASTFunctionDeclaration *node, bool generic_instantiation,
                             std::vector<Type *> generic_args = {});
  void visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation,
                                std::vector<Type *> generic_args = {});
  void visit_choice_declaration(ASTChoiceDeclaration *node, bool generic_instantiation,
                                std::vector<Type *> generic_args = {});
  void visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<Type *> generic_args = {});
  void visit_trait_declaration(ASTTraitDeclaration *node, bool generic_instantiation,
                               std::vector<Type *> generic_args = {});
  void visit_function_body(ASTFunctionDeclaration *node);

  Type *get_self_type();

  void type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, ASTFunctionDeclaration *function,
                                   Nullable<ASTExpr> self, bool is_deinit_call);
  void type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info);
  ASTFunctionDeclaration *resolve_generic_function_call(ASTFunctionDeclaration *func,
                                                        std::vector<ASTExpr *> *generic_args, ASTArguments *arguments,
                                                        SourceRange range);

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
  SourceRange definition_range = {};
  jmp_buf save_state;
};
