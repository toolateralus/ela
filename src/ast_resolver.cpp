#include "ast_resolver.hpp"

void ASTResolver::declare_type(Type *) {}
void ASTResolver::define_type(Type *) {}

void ASTResolver::visit_program(ASTProgram *) {}

void ASTResolver::visit_struct(ASTStructDeclaration *) {}
void ASTResolver::visit_function_declaration(ASTFunctionDeclaration *) {}
void ASTResolver::visit_enum(ASTEnumDeclaration *) {}
void ASTResolver::visit_alias(ASTAlias *) {}
void ASTResolver::visit_import(ASTImport *) {}
void ASTResolver::visit_impl(ASTImpl *) {}
void ASTResolver::visit_choice(ASTChoiceDeclaration *) {}
void ASTResolver::visit_trait(ASTTraitDeclaration *) {}
void ASTResolver::visit_module(ASTModule *) {}


void ASTResolver::visit_arguments(ASTArguments *) {}
void ASTResolver::visit_parameters(ASTParamsDecl *) {}
void ASTResolver::visit_parameter(ASTParamDecl *) {}
void ASTResolver::visit_block(ASTBlock *) {}
void ASTResolver::visit_variable(ASTVariable *) {}

void ASTResolver::visit_type(ASTType *) {}
void ASTResolver::visit_typeof(ASTType_Of *) {}

void ASTResolver::visit_return(ASTReturn *) {}
void ASTResolver::visit_continue(ASTContinue *) {}
void ASTResolver::visit_break(ASTBreak *) {}
void ASTResolver::visit_for(ASTFor *) {}
void ASTResolver::visit_if(ASTIf *) {}
void ASTResolver::visit_while(ASTWhile *) {}

void ASTResolver::visit_binary(ASTBinExpr *) {}
void ASTResolver::visit_unary(ASTUnaryExpr *) {}
void ASTResolver::visit_index(ASTIndex *) {}
void ASTResolver::visit_path(ASTPath *) {}
void ASTResolver::visit_literal(ASTLiteral *) {}
void ASTResolver::visit_call(ASTCall *) {}
void ASTResolver::visit_else(ASTElse *) {}
void ASTResolver::visit_dot(ASTDotExpr *) {}
void ASTResolver::visit_cast(ASTCast *) {}
void ASTResolver::visit_lambda(ASTLambda *) {}
void ASTResolver::visit_range(ASTRange *) {}
void ASTResolver::visit_switch(ASTSwitch *) {}
void ASTResolver::visit_tuple(ASTTuple *) {}
void ASTResolver::visit_initializer_list(ASTInitializerList *) {}

void ASTResolver::visit_destructure(ASTDestructure *) {}

void ASTResolver::visit_where(ASTWhere *) {}
void ASTResolver::visit_dynof(ASTDyn_Of *) {}
void ASTResolver::visit_pattern_match(ASTPatternMatch *) {}
void ASTResolver::visit_method_call(ASTMethodCall *) {}
void ASTResolver::visit_where_statement(ASTWhereStatement *) {}
void ASTResolver::visit_unpack_expr(ASTUnpackExpr *) {}
void ASTResolver::visit_unpack(ASTUnpackElement *) {}