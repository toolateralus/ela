#pragma once
#include <cstdarg>
#include "builder.hpp"
#include "core.hpp"
#include "thir.hpp"

struct Emitter {
  bool emitting_global_initializer = false;
  StringBuilder code{};
  int indent_level = 0;
  const THIRFunction *entry_point;
  
  inline void indented(const std::string &string = {}) {
    // 2 space indenting is our standard.
    code << std::string(indent_level * 2, ' ');
    code << string;
  }

  inline void indentedf(const char *fmt, ...) {
    code << std::string(indent_level * 2, ' ');
    va_list args;
    va_start(args, fmt);
    char *result;
    vasprintf(&result, fmt, args);
    va_end(args);
    code << result;
  }

  inline void indented_terminated(const std::string &string) {
    indented(string);
    code << ";\n";
  }

  // similar to 'emit_symbol' except, when passed say a THIRFunction*, this will emit the identifier that points to that
  // function we just compute paths/references instead of storing them in the THIR, which aligns more with LLVM in the
  // long run.

  // this will call emit_node if it doesn't require extra intercepted support to emit correctly.
  void emit_expr(const THIR *thir);

  inline void emit_line_directive(const THIR *thir) {
    // const static bool is_debugging = compile_command.has_flag("debug");
    const static bool no_line = compile_command.has_flag("nl");

    if (no_line) {
      return;
    }

    code << "\n#line " << std::to_string(thir->span.line) << " \""
         << thir->span.files()[thir->span.file] << "\";\n";
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
  void emit_return(const THIRReturn *thir);
  void emit_break(const THIRBreak *thir);
  void emit_continue(const THIRContinue *thir);
  void emit_for(const THIRFor *thir);
  void emit_if(const THIRIf *thir);
  void emit_while(const THIRWhile *thir);
  void emit_variable(const THIRVariable *thir);

  void emit_struct(Type *type);
  void emit_tuple(Type *type);
  void emit_choice(Type *type);
  void emit_enum(Type *type);
  void emit_anonymous_struct(Type *thir);
  void emit_struct_body(Type *thir);
  void emit_type(Type *type);
  void forward_declare_type(const Type *type);
  void emit_dyn_dispatch_object_struct(Type *type);

  void emit_function(const THIRFunction *thir, const bool forward_declaration = false);
  void emit_block(const THIRBlock *thir);
  void emit_expression_block(const THIRExprBlock *thir);

  std::string reflection_prelude(const std::unordered_map<const Type *, ReflectionInfo>&);
};