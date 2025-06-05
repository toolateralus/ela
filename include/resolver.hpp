#pragma once
#include <set>
#include "thir.hpp"

struct Emitter;
struct Resolver {
  std::set<const Type *> forward_declared_types;
  std::set<const Type *> emitted_types;
  std::set<const THIRFunction *> emitted_functions;
  std::set<const THIRVariable *> emitted_global_variables;

  Resolver(Emitter &emitter): emitter(emitter) {}
  Emitter &emitter;

  void declare_or_define_type(Type *type);
  void emit_type_definition(Type *type);

  void visit_node(const THIR *thir);
  void visit_program(const THIRProgram *thir);
  void visit_bin_expr(const THIRBinExpr *thir);
  void visit_unary_expr(const THIRUnaryExpr *thir);
  void visit_literal(const THIRLiteral *thir);
  void visit_call(const THIRCall *thir);
  void visit_member_access(const THIRMemberAccess *thir);
  void visit_cast(const THIRCast *thir);
  void visit_index(const THIRIndex *thir);
  void visit_aggregate_initializer(const THIRAggregateInitializer *thir);
  void visit_collection_initializer(const THIRCollectionInitializer *thir);
  void visit_empty_initializer(const THIREmptyInitializer *thir);
  void visit_size_of(const THIRSizeOf *thir);
  void visit_return(const THIRReturn *thir);
  void visit_break(const THIRBreak *thir);
  void visit_continue(const THIRContinue *thir);
  void visit_for(const THIRFor *thir);
  void visit_if(const THIRIf *thir);
  void visit_while(const THIRWhile *thir);
  void visit_switch(const THIRSwitch *thir);
  void visit_variable(const THIRVariable *thir);
  void visit_type(const THIRType *thir);
  void visit_function(const THIRFunction *thir);
  void visit_block(const THIRBlock *thir);
  void visit_expr_block(const THIRExprBlock *thir);
};