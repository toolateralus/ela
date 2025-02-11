#pragma once

#include <csetjmp>
#include <deque>
#include <vector>

#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"

#define DEFINE_GENERIC_VISITOR()                                                                                       \
  void visit(AST *node) {                                                                                              \
    switch (node->node_type) {                                                                                         \
      case AST_PROGRAM:                                                                                                \
        return visit_program(node);                                                                                    \
      case AST_BLOCK:                                                                                                  \
        return visit_block(node);                                                                                      \
      case AST_FUNCTION:                                                                                               \
        return visit_function_declaration(node);                                                                       \
      case AST_DECLARATION:                                                                                            \
        return visit_declaration(node);                                                                                \
      case AST_BINARY:                                                                                                 \
        return visit_bin_expr(node);                                                                                   \
      case AST_UNARY_EXPR:                                                                                             \
        return visit_unary_expr(node);                                                                                 \
      case AST_IDENTIFIER:                                                                                             \
        return visit_identifier(node);                                                                                 \
      case AST_LITERAL:                                                                                                \
        return visit_literal(node);                                                                                    \
      case AST_TYPE:                                                                                                   \
        return visit_type(node);                                                                                       \
      case AST_TUPLE:                                                                                                  \
        return visit_tuple(node);                                                                                      \
      case AST_CALL:                                                                                                   \
        return visit_call(node);                                                                                       \
      case AST_RETURN:                                                                                                 \
        return visit_return(node);                                                                                     \
      case AST_CONTINUE:                                                                                               \
        return visit_continue(node);                                                                                   \
      case AST_BREAK:                                                                                                  \
        return visit_break(node);                                                                                      \
      case AST_FOR:                                                                                                    \
        return visit_for(node);                                                                                        \
      case AST_IF:                                                                                                     \
        return visit_if(node);                                                                                         \
      case AST_ELSE:                                                                                                   \
        return visit_else(node);                                                                                       \
      case AST_WHILE:                                                                                                  \
        return visit_while(node);                                                                                      \
      case AST_STRUCT:                                                                                                 \
        return visit_struct_declaration(node);                                                                         \
      case AST_DOT_EXPR:                                                                                               \
        return visit_dot_expr(node);                                                                                   \
      case AST_SCOPE_RESOLUTION:                                                                                       \
        return visit_scope_resolution(node);                                                                           \
      case AST_SUBSCRIPT:                                                                                              \
        return visit_subscript(node);                                                                                  \
      case AST_INITIALIZER:                                                                                            \
        return visit_initializer_list(node);                                                                           \
      case AST_ENUM:                                                                                                   \
        return visit_enum_declaration(node);                                                                           \
      case AST_NOOP:                                                                                                   \
        return visit_noop(node);                                                                                       \
      case AST_ALIAS:                                                                                                  \
        return visit_alias(node);                                                                                      \
      case AST_IMPL:                                                                                                   \
        return visit_impl(node);                                                                                       \
      case AST_INTERFACE:                                                                                              \
        return visit_interface_declaration(node);                                                                      \
      case AST_SIZE_OF:                                                                                                \
        return visit_size_of(node);                                                                                    \
      case AST_DEFER:                                                                                                  \
        return visit_defer(node);                                                                                      \
      case AST_CAST:                                                                                                   \
        return visit_cast(node);                                                                                       \
      case AST_LAMBDA:                                                                                                 \
        return visit_lambda(node);                                                                                     \
      case AST_RANGE:                                                                                                  \
        return visit_range(node);                                                                                      \
      case AST_SWITCH:                                                                                                 \
        return visit_switch(node);                                                                                     \
      case AST_TUPLE_DECONSTRUCTION:                                                                                   \
        return visit_tuple_deconstruction(node);                                                                       \
      case AST_WHERE:                                                                                                  \
        return visit_where(node);                                                                                      \
      case AST_STATEMENT_LIST:                                                                                         \
        return visit_statement_list(node);                                                                             \
    }                                                                                                                  \
  }

struct Typer {
  // parent type of impl's or whatever struct/union/enum etc that's currently being processed.
  Nullable<AST> type_context = nullptr;
  // either the current argument type,
  // the assigning type, declaring type,
  // used for type inference in more complex scenarios.
  int expected_type = -1;

  using VisitorMethod = void (Typer::*)(AST *, bool, std::vector<int>);
  AST *visit_generic(VisitorMethod visit_method, AST *declaring_node, auto &(*subtype)(AST*), std::vector<int> args);
  void compiler_mock_function_call_visit_impl(Scope scope, int left_type, const Interned_String &method_name);
  void visit_impl_declaration(AST *node, bool generic_instantiation, std::vector<int> generic_args = {});
  void visit_interface_declaration(AST *node, bool generic_instantiation, std::vector<int> generic_args = {});
  void visit_struct_declaration(AST *node, bool generic_instantiation, std::vector<int> generic_args = {});
  
  void visit_parameters(Source_Range source_range, std::vector<AST_Parameter_Declaration> &params);
  std::vector<int> visit_arguments(Source_Range source_range, AST *scope, std::vector<AST *> &arguments);

  void visit_function_body(AST *node);
  void visit_function_header(AST *node, bool generic_instantiation, std::vector<int> generic_args = {});

  AST *resolve_generic_function_call(AST *node, AST *function);
  int find_generic_type_of(const Interned_String &base, const std::vector<int> &generic_args,
                           const Source_Range &source_range);
  std::vector<int> get_generic_arg_types(const std::vector<AST *> &args);

  void type_check_args_from_params(AST *call, AST *function, bool skip_first);
  void type_check_args_from_info(AST *call, Function_Info *info);

  bool visit_where_predicate(Type *type, AST *node);
  void compiler_mock_function_call_visit_impl(int type, const Interned_String &method_name);

  Nullable<Symbol> get_symbol(AST *node);
  Interned_String type_name(AST *node);
  int get_self_type();
  std::vector<TypeExtension> accept_extensions(std::vector<AST_Type_Extension> ast_extensions);
  void visit_statement_list(AST *node) {
    for (auto &stmt : node->statements) {
      visit(stmt);
    }
  };
  DEFINE_VISITORS()
  DEFINE_GENERIC_VISITOR()
};

enum DeferBlockType: unsigned char {
  DEFER_BLOCK_TYPE_OTHER,
  DEFER_BLOCK_TYPE_FUNC,
  DEFER_BLOCK_TYPE_LOOP,
};

struct DeferBlock {
  std::vector<AST *> defers;
  DeferBlockType type;
};

struct DependencyEmitter;

struct Emitter {
  static constexpr const char *defer_return_value_key = "$defer$return$value";
  bool has_user_defined_main = false;
  bool emit_default_init = true;
  bool emit_default_value = true;
  bool emit_default_args = false;
  int num_tests = 0;
  int cf_expr_return_id = 0;

  Nullable<std::string> cf_expr_return_register;
  Nullable<AST> type_context;

  Typer &typer;

  // used to cache up defers.
  bool emitting_function_with_defer = false;

  // the one at the top was the last one that was placed. we do this because you need to hit all the outer ones,
  // which will be done witha  fall thruogh on the labels,but you may want to skip some defers, say you never branched
  // into that block.
  std::deque<DeferBlock> defer_blocks{};

  std::stringstream code{};
  std::stringstream *ss{};
  std::stringstream test_functions{};

  int indent_level = 0;

  int type_list_id = -1;
  const bool is_freestanding = compile_command.compilation_flags.contains("-ffreestanding") ||
                               compile_command.compilation_flags.contains("-nostdlib");

  // TODO(Josh) 10/1/2024, 10:10:17 AM
  // This causes a lot of empty lines. It would be nice to have a way to neatly
  // do this.
  inline void emit_line_directive(AST *node) {
    static int last_loc = -1;
    static bool is_debugging = !compile_command.has_flag("release");
    if (!is_debugging) {
      return;
    }
    auto loc = node->source_range.begin_location.line;
    if (loc != last_loc) {
      auto filename = get_source_filename(node->source_range);
      if (filename.empty()) {
        printf("Empty filename for line directive.\n");
        return;
      }
      (*ss) << std::string{"\n#line "} << std::to_string(loc) << std::string{" \""} << filename << std::string{"\"\n"};
      last_loc = loc;
    }
  }
  void emit_tuple(int type);
  void emit_lambda(AST *node);
  void call_operator_overload(const Source_Range &range, Type *left_ty, OperationKind operation, Token_Type op,
                              AST *left, AST *right = nullptr);

  void forward_decl_type(Type *type);
  void emit_deferred_statements(DeferBlockType type);

  std::string to_type_struct(Type *type);
  Emitter(Typer &type_visitor);
  inline std::string indent() { return std::string(indent_level * 2, ' '); }
  inline void indented(const std::string &s) { (*ss) << indent() << s; }
  inline void indentedln(const std::string &s) { (*ss) << indent() << s + '\n'; }
  inline void newline() { (*ss) << '\n'; }
  inline void newline_indented() { (*ss) << '\n' << indent(); }
  inline void semicolon() { (*ss) << ";"; }
  inline void space() { (*ss) << ' '; }

  void emit_forward_declaration(AST *node);
  void emit_foreign_function(AST *node);

  bool should_emit_function(Emitter *visitor, AST *node, bool test_flag);
  std::string to_cpp_string(const Type_Metadata &meta, const std::string &base);
  std::string to_cpp_string(Type *type);
  std::string get_cpp_scalar_type(int id);

  std::string get_type_struct(Type *type, int id, const std::string &fields);
  std::string get_field_struct(const std::string &name, Type *type, Type *parent_type);
  std::string get_elements_function(Type *type);

  std::string get_function_pointer_type_string(Type *type, Nullable<std::string> identifier = nullptr);
  std::string get_declaration_type_signature_and_identifier(const std::string &name, Type *type);

  int get_expr_left_type_sr_dot(AST *node);

  void visit_statement_list(AST *node) {
    for (auto stmt : node->statements) {
      emit_line_directive(stmt);
      visit(stmt);
      semicolon();
      newline();
    }
    return;
  };

  DEFINE_VISITORS();
  DEFINE_GENERIC_VISITOR()
};

struct DependencyEmitter {
  Emitter *emitter;
  inline DependencyEmitter(Emitter *emitter) : emitter(emitter) {}
  std::string define_type(int type_id);
  std::string decl_type(int type_id);
  DEFINE_VISITORS()
  DEFINE_GENERIC_VISITOR()
  void visit_statement_list(AST *node) {
    for (auto &stmt : node->statements) {
      visit(stmt);
    }
  };
};

struct GenericInstantiationErrorUserData {
  std::string message = "";
  Source_Range definition_range = {};
  jmp_buf save_state;
};