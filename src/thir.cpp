#include "thir.hpp"

THIR *THIRVisitor::visit_method_call(ASTMethodCall *node) {
	THIRCall call = {};
	auto base = node->dot->base;
	auto member = node->dot->member;
	auto type_scope = base->resolved_type->info->scope;
	auto symbol = type_scope->local_lookup(member.identifier);
	if (symbol->is_variable()) {
		call.callee = visit_dot_expr(node->dot);
	}
}
THIR *THIRVisitor::visit_path(ASTPath *node) {

}
THIR *THIRVisitor::visit_pattern_match(ASTPatternMatch *node) {

}
THIR *THIRVisitor::visit_dyn_of(ASTDyn_Of *node) {

}
THIR *THIRVisitor::visit_type_of(ASTType_Of *node) {

}
THIR *THIRVisitor::visit_block(ASTBlock *node) {

}
THIR *THIRVisitor::visit_expr_statement(ASTExprStatement *node) {

}
THIR *THIRVisitor::visit_bin_expr(ASTBinExpr *node) {

}
THIR *THIRVisitor::visit_unary_expr(ASTUnaryExpr *node) {

}
THIR *THIRVisitor::visit_literal(ASTLiteral *node) {

}
THIR *THIRVisitor::visit_type(ASTType *node) {

}
THIR *THIRVisitor::visit_call(ASTCall *node) {

}
THIR *THIRVisitor::visit_return(ASTReturn *node) {

}
THIR *THIRVisitor::visit_dot_expr(ASTDotExpr *node) {

}
THIR *THIRVisitor::visit_subscript(ASTIndex *node) {

}
THIR *THIRVisitor::visit_initializer_list(ASTInitializerList *node) {

}
THIR *THIRVisitor::visit_range(ASTRange *node) {

}
THIR *THIRVisitor::visit_switch(ASTSwitch *node) {

}
THIR *THIRVisitor::visit_tuple(ASTTuple *node) {

}
THIR *THIRVisitor::load_value(ASTNode *node, THIR *expr) {

}
THIR *THIRVisitor::visit_cast(ASTCast *node) {

}
THIR *THIRVisitor::visit_lambda(ASTLambda *node) {

}
THIR *THIRVisitor::visit_size_of(ASTSize_Of *node) {

}
THIR *THIRVisitor::visit_struct_declaration(ASTStructDeclaration *node) {

}
THIR *THIRVisitor::visit_module(ASTModule *node) {

}
THIR *THIRVisitor::visit_import(ASTImport *node) {

}
THIR *THIRVisitor::visit_program(ASTProgram *node) {

}
THIR *THIRVisitor::visit_function_declaration(ASTFunctionDeclaration *node) {

}
THIR *THIRVisitor::visit_variable(ASTVariable *node) {

}
THIR *THIRVisitor::visit_continue(ASTContinue *node) {

}
THIR *THIRVisitor::visit_break(ASTBreak *node) {

}
THIR *THIRVisitor::visit_for(ASTFor *node) {

}
THIR *THIRVisitor::visit_if(ASTIf *node) {

}
THIR *THIRVisitor::visit_else(ASTElse *node) {

}
THIR *THIRVisitor::visit_while(ASTWhile *node) {

}
THIR *THIRVisitor::visit_enum_declaration(ASTEnumDeclaration *node) {

}
THIR *THIRVisitor::visit_tuple_deconstruction(ASTTupleDeconstruction *node) {

}
THIR *THIRVisitor::visit_impl(ASTImpl *node) {

}
THIR *THIRVisitor::visit_defer(ASTDefer *node) {

}
THIR *THIRVisitor::visit_choice_declaration(ASTChoiceDeclaration *node) {

}