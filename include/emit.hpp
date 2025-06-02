#pragma once
#include "builder.hpp"
#include "thir.hpp"

struct Emitter {
  StringBuilder code{};
  int indent_level = 0;

  inline void indented(const std::string &string) {
    // 2 space indenting is our standard.
    code << std::string(indent_level * 2, ' ');
    code << string;
  }

  inline void indented_terminated(const std::string &string) {
    indented(string);
    code << ";\n";
  }

  void emit_node(const THIR *thir);
  void emit_program(const THIRProgram *thir);
  void emit_bin_expr(const THIRBinExpr *thir);
  void emit_unary_expr(const THIRUnaryExpr *thir);
  void emit_literal(const THIRLiteral *thir);
  void emit_call(const THIRCall *thir);
  void emit_member_access(const THIRMemberAccess *thir);
  void emit_cast(const THIRCast *thir);
  void emit_index(const THIRIndex *thir);
  void emit_aggregate_initializer(const THIRAggregateInitializer *thir);
  void emit_collection_initializer(const THIRCollectionInitializer *thir);
  void emit_empty_initializer(const THIREmptyInitializer *thir);
  void emit_size_of(const THIRSizeOf *thir);
  void emit_return(const THIRReturn *thir);
  void emit_break(const THIRBreak *thir);
  void emit_continue(const THIRContinue *thir);
  void emit_for(const THIRFor *thir);
  void emit_if(const THIRIf *thir);
  void emit_while(const THIRWhile *thir);
  void emit_switch(const THIRSwitch *thir);
  void emit_variable(const THIRVariable *thir);
  void emit_struct(const THIRStruct *thir);
  void emit_function(const THIRFunction *thir);
  void emit_block(const THIRBlock *thir);
};