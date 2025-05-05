#include "thir.hpp"

THIR *THIRVisitor::visit_method_call(ASTMethodCall *node) {
  THIRCall call = {};
  auto base = node->callee->base;
  auto member = node->callee->member;
  auto type_scope = base->resolved_type->info->scope;
  auto symbol = type_scope->local_lookup(member.identifier);
  if (symbol->is_variable()) {
    call.callee = visit_dot_expr(node->callee);
  }
  return nullptr;
}
THIR *THIRVisitor::visit_path(ASTPath *node) { return nullptr; }
THIR *THIRVisitor::visit_pattern_match(ASTPatternMatch *node) { return nullptr; }
THIR *THIRVisitor::visit_dyn_of(ASTDyn_Of *node) { return nullptr; }
THIR *THIRVisitor::visit_type_of(ASTType_Of *node) { return nullptr; }
THIR *THIRVisitor::visit_block(ASTBlock *node) { return nullptr; }
THIR *THIRVisitor::visit_expr_statement(ASTExprStatement *node) { return nullptr; }
THIR *THIRVisitor::visit_bin_expr(ASTBinExpr *node) { return nullptr; }
THIR *THIRVisitor::visit_unary_expr(ASTUnaryExpr *node) { return nullptr; }
THIR *THIRVisitor::visit_literal(ASTLiteral *node) { return nullptr; }
THIR *THIRVisitor::visit_type(ASTType *node) { return nullptr; }
THIR *THIRVisitor::visit_call(ASTCall *node) { return nullptr; }
THIR *THIRVisitor::visit_return(ASTReturn *node) { return nullptr; }
THIR *THIRVisitor::visit_dot_expr(ASTDotExpr *node) { return nullptr; }
THIR *THIRVisitor::visit_subscript(ASTIndex *node) { return nullptr; }
THIR *THIRVisitor::visit_initializer_list(ASTInitializerList *node) { return nullptr; }
THIR *THIRVisitor::visit_range(ASTRange *node) { return nullptr; }
THIR *THIRVisitor::visit_switch(ASTSwitch *node) { return nullptr; }
THIR *THIRVisitor::visit_tuple(ASTTuple *node) { return nullptr; }
THIR *THIRVisitor::load_value(ASTNode *node, THIR *expr) { return nullptr; }
THIR *THIRVisitor::visit_cast(ASTCast *node) { return nullptr; }
THIR *THIRVisitor::visit_lambda(ASTLambda *node) { return nullptr; }
THIR *THIRVisitor::visit_size_of(ASTSize_Of *node) { return nullptr; }
THIR *THIRVisitor::visit_struct_declaration(ASTStructDeclaration *node) { return nullptr; }
THIR *THIRVisitor::visit_module(ASTModule *node) { return nullptr; }
THIR *THIRVisitor::visit_import(ASTImport *node) { return nullptr; }
THIR *THIRVisitor::visit_program(ASTProgram *node) { return nullptr; }
THIR *THIRVisitor::visit_function_declaration(ASTFunctionDeclaration *node) { return nullptr; }
THIR *THIRVisitor::visit_variable(ASTVariable *node) { return nullptr; }
THIR *THIRVisitor::visit_continue(ASTContinue *node) { return nullptr; }
THIR *THIRVisitor::visit_break(ASTBreak *node) { return nullptr; }
THIR *THIRVisitor::visit_for(ASTFor *node) { return nullptr; }
THIR *THIRVisitor::visit_if(ASTIf *node) { return nullptr; }
THIR *THIRVisitor::visit_else(ASTElse *node) { return nullptr; }
THIR *THIRVisitor::visit_while(ASTWhile *node) { return nullptr; }
THIR *THIRVisitor::visit_enum_declaration(ASTEnumDeclaration *node) { return nullptr; }
THIR *THIRVisitor::visit_tuple_deconstruction(ASTTupleDeconstruction *node) { return nullptr; }
THIR *THIRVisitor::visit_impl(ASTImpl *node) { return nullptr; }
THIR *THIRVisitor::visit_defer(ASTDefer *node) { return nullptr; }
THIR *THIRVisitor::visit_choice_declaration(ASTChoiceDeclaration *node) { return nullptr; }