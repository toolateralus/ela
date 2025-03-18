#pragma once

#include "ast.hpp"
#include "scope.hpp"

struct ASTCopier {

  template<class T>
  T *copy(T *node) requires std::is_base_of_v<ASTNode, T> {
    auto new_node = new (ast_alloc<T>()) T(*node);
    new_node->control_flow.type = -1;

    if constexpr (std::is_base_of<ASTStatement, T>::value) {
      new_node->attributes = node->attributes;
    }
    return new_node;
  }

  Scope *current_scope = nullptr;
  ASTImpl *copy_impl(ASTImpl* node);
  Scope *copy_scope(Scope *old);
  ASTProgram *copy_program(ASTProgram *node);
  ASTBlock *copy_block(ASTBlock *node);
  ASTFunctionDeclaration *copy_function_declaration(ASTFunctionDeclaration *node);
  ASTParamsDecl *copy_params_decl(ASTParamsDecl *node);
  ASTParamDecl *copy_param_decl(ASTParamDecl *node);
  ASTVariable *copy_variable(ASTVariable *node);
  ASTExprStatement *copy_expr_statement(ASTExprStatement *node);
  ASTBinExpr *copy_bin_expr(ASTBinExpr *node);
  ASTUnaryExpr *copy_unary_expr(ASTUnaryExpr *node);
  ASTIdentifier *copy_identifier(ASTIdentifier *node);
  ASTLiteral *copy_literal(ASTLiteral *node);
  ASTType *copy_type(ASTType *node);
  ASTCall *copy_call(ASTCall *node);
  ASTArguments *copy_arguments(ASTArguments *node);
  ASTReturn *copy_return(ASTReturn *node);
  ASTContinue *copy_continue(ASTContinue *node);
  ASTBreak *copy_break(ASTBreak *node);
  ASTFor *copy_for(ASTFor *node);
  ASTIf *copy_if(ASTIf *node);
  ASTElse *copy_else(ASTElse *node);
  ASTWhile *copy_while(ASTWhile *node);
  ASTStructDeclaration *copy_struct_declaration(ASTStructDeclaration *node);
  ASTDotExpr *copy_dot_expr(ASTDotExpr *node);
  ASTSubscript *copy_subscript(ASTSubscript *node);
  ASTInitializerList *copy_initializer_list(ASTInitializerList *node);
  ASTEnumDeclaration *copy_enum_declaration(ASTEnumDeclaration *node);
  ASTRange *copy_range(ASTRange *node);
  ASTSwitch *copy_switch(ASTSwitch *node);
  ASTTuple *copy_tuple(ASTTuple *node);
  ASTTupleDeconstruction *copy_tuple_deconstruction(ASTTupleDeconstruction *node);
  ASTInterfaceDeclaration *copy_interface_declaration(ASTInterfaceDeclaration *node);
  ASTAlias *copy_alias(ASTAlias *node);
  ASTSize_Of *copy_sizeof(ASTSize_Of *node);
  ASTDefer *copy_defer(ASTDefer *node);
  ASTLambda *copy_lambda(ASTLambda *node);
  ASTTaggedUnionDeclaration *copy_tagged_union_declaration(ASTTaggedUnionDeclaration *node);
  ASTStatementList *copy_statement_list(ASTStatementList *node);
  ASTScopeResolution *copy_scope_resolution(ASTScopeResolution *node);
  ASTNode *copy_node(ASTNode *node);
  ASTWhere *copy_where(ASTWhere *node);
  ASTCast *copy_cast(ASTCast *node);
  ASTType_Of *copy_type_of(ASTType_Of *node);
  ASTImport *copy_import(ASTImport *node);
  ASTModule *copy_module(ASTModule *node);
};

ASTNode *deep_copy_ast(ASTNode *root);