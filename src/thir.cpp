#include "thir.hpp"
#include "ast.hpp"

THIR *THIRVisitor::visit_method_call(ASTMethodCall *ast) {
  auto thir = THIR_ALLOC(THIRCall, ast);
  auto base = ast->callee->base;
  auto member = ast->callee->member;
  auto type_scope = base->resolved_type->info->scope;
  auto symbol = type_scope->local_lookup(member.identifier);
  if (symbol->is_variable()) {
    thir->callee = visit_dot_expr(ast->callee);
  }
  for (const auto &argument : ast->arguments->arguments) {
    thir->arguments.push_back(visit_expr(argument));
  }
  thir->type = ast->resolved_type;
  thir->source_range = ast->source_range;
  return thir;
}

THIR *THIRVisitor::visit_path(ASTPath *ast) { return nullptr; }
THIR *THIRVisitor::visit_pattern_match(ASTPatternMatch *ast) { return nullptr; }
THIR *THIRVisitor::visit_dyn_of(ASTDyn_Of *ast) { return nullptr; }
THIR *THIRVisitor::visit_type_of(ASTType_Of *ast) { return nullptr; }
THIR *THIRVisitor::visit_block(ASTBlock *ast) { return nullptr; }
THIR *THIRVisitor::visit_expr_statement(ASTExprStatement *ast) { return nullptr; }
THIR *THIRVisitor::visit_bin_expr(ASTBinExpr *ast) { return nullptr; }
THIR *THIRVisitor::visit_unary_expr(ASTUnaryExpr *ast) { return nullptr; }
THIR *THIRVisitor::visit_literal(ASTLiteral *ast) { return nullptr; }
THIR *THIRVisitor::visit_type(ASTType *ast) { return nullptr; }
THIR *THIRVisitor::visit_call(ASTCall *ast) { return nullptr; }
THIR *THIRVisitor::visit_return(ASTReturn *ast) { return nullptr; }
THIR *THIRVisitor::visit_dot_expr(ASTDotExpr *ast) { return nullptr; }
THIR *THIRVisitor::visit_subscript(ASTIndex *ast) { return nullptr; }
THIR *THIRVisitor::visit_initializer_list(ASTInitializerList *ast) { return nullptr; }
THIR *THIRVisitor::visit_range(ASTRange *ast) { return nullptr; }
THIR *THIRVisitor::visit_switch(ASTSwitch *ast) { return nullptr; }
THIR *THIRVisitor::visit_tuple(ASTTuple *ast) { return nullptr; }
THIR *THIRVisitor::load_value(ASTNode *ast, THIR *expr) { return nullptr; }
THIR *THIRVisitor::visit_cast(ASTCast *ast) { return nullptr; }
THIR *THIRVisitor::visit_lambda(ASTLambda *ast) { return nullptr; }
THIR *THIRVisitor::visit_size_of(ASTSize_Of *ast) { return nullptr; }
THIR *THIRVisitor::visit_struct_declaration(ASTStructDeclaration *ast) { return nullptr; }
THIR *THIRVisitor::visit_module(ASTModule *ast) { return nullptr; }
THIR *THIRVisitor::visit_import(ASTImport *ast) { return nullptr; }
THIR *THIRVisitor::visit_program(ASTProgram *ast) { return nullptr; }
THIR *THIRVisitor::visit_function_declaration(ASTFunctionDeclaration *ast) { return nullptr; }
THIR *THIRVisitor::visit_variable(ASTVariable *ast) { return nullptr; }
THIR *THIRVisitor::visit_continue(ASTContinue *ast) { return nullptr; }
THIR *THIRVisitor::visit_break(ASTBreak *ast) { return nullptr; }
THIR *THIRVisitor::visit_for(ASTFor *ast) { return nullptr; }
THIR *THIRVisitor::visit_if(ASTIf *ast) { return nullptr; }
THIR *THIRVisitor::visit_else(ASTElse *ast) { return nullptr; }
THIR *THIRVisitor::visit_while(ASTWhile *ast) { return nullptr; }
THIR *THIRVisitor::visit_enum_declaration(ASTEnumDeclaration *ast) { return nullptr; }
THIR *THIRVisitor::visit_tuple_deconstruction(ASTDestructure *ast) { return nullptr; }
THIR *THIRVisitor::visit_impl(ASTImpl *ast) { return nullptr; }
THIR *THIRVisitor::visit_defer(ASTDefer *ast) { return nullptr; }
THIR *THIRVisitor::visit_choice_declaration(ASTChoiceDeclaration *) { return nullptr; }