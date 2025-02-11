#include <ostream>
#include <sstream>
#include <string>

#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "visitor.hpp"

constexpr auto TYPE_FLAGS_INTEGER = 1 << 0;
constexpr auto TYPE_FLAGS_FLOAT = 1 << 1;
constexpr auto TYPE_FLAGS_BOOL = 1 << 2;
constexpr auto TYPE_FLAGS_STRING = 1 << 3;
constexpr auto TYPE_FLAGS_STRUCT = 1 << 4;
constexpr auto TYPE_FLAGS_ENUM = 1 << 5;
constexpr auto TYPE_FLAGS_TUPLE = 1 << 6;

constexpr auto TYPE_FLAGS_ARRAY = 1 << 7;
constexpr auto TYPE_FLAGS_FUNCTION = 1 << 8;
constexpr auto TYPE_FLAGS_POINTER = 1 << 9;

constexpr auto TYPE_FLAGS_SIGNED = 1 << 10;
constexpr auto TYPE_FLAGS_UNSIGNED = 1 << 11;

void Emitter::initialize_reflection_system() {
  // Emit runtime reflection type info for requested types, only when we have
  // actually requested runtime type information.
  if (!type_info_strings.empty() && !is_freestanding) {
    std::stringstream type_info{};
    for (const auto &str : type_info_strings) {
      type_info << str.get_str() << ";\n";
    }

    code << "void $initialize_reflection_system() {\n";
    {
      // we don't bother doing pushes into type info, it's easier for us to do it this way.
      code << std::format("_type_info.length = _type_info.capacity = {};\n", type_info_strings.size());
      code << std::format("_type_info.data = realloc(_type_info.data, sizeof(Type*) * {});", type_table.size());
      code << type_info.str() << ";\n";
    }
    code << "}\n";
  }
}
void Emitter::emit_runtime_main(AST *&node) {
  if (is_testing) {
    auto test_init = test_functions.str();
    if (test_init.ends_with(',')) {
      test_init.pop_back();
    }
    code << TESTING_MAIN_BOILERPLATE_AAAAGHH << '\n';
    // deploy the array of test struct wrappers.
    code << std::format("__COMPILER_GENERATED_TEST tests[{}] = {}\n", num_tests, "{ " + test_init + " };");
    // use the test runner main macro.
    code << "__TEST_RUNNER_MAIN;";
  } else {
    if (has_user_defined_main && !is_freestanding) {
      code << std::format("int main (int argc, char** argv) {{\n${}_initialize(argc, argv);\n{}\n__ela_main_();\n}}\n",
                          node->find_type_id("Env", {}),
                          type_info_strings.size() != 0 ? "$initialize_reflection_system();"
                                                        : "{/* no reflection present in module */};");
    } // C calls main() for freestanding
  }
}

void Emitter ::visit_program(AST *node) {
  emit_line_directive(node);
  for (const auto &statement : node->statements) {
    visit(statement);
  }
  if (is_freestanding && !type_info_strings.empty()) {
    throw_error("You cannot use runtime type reflection in a freestanding or "
                "nostdlib environment, due to a lack of allocators. To compare "
                "types, use #typeid (temporarily unavailable).",
                {});
  }
  initialize_reflection_system();
  emit_runtime_main(node);
}

void Emitter ::visit_block(AST *node) {
  emit_line_directive(node);
  indented("{\n");
  indent_level++;
  defer_blocks.emplace_back();
  for (const auto &statement : node->statements) {
    emit_line_directive(node);
    indent();
    visit(statement);
  }
  emit_deferred_statements(DEFER_BLOCK_TYPE_OTHER);
  defer_blocks.pop_back();
  indent_level--;
  indented("}");
}

void Emitter::visit_arguments(const std::vector<AST *> &arguments) {
  (*ss) << "(";
  for (const auto arg : arguments) {
    visit(arg);
    if (arg != arguments.back()) {
      (*ss) << ", ";
    }
  }
  (*ss) << ")";
}

void Emitter::visit_parameters(const std::vector<AST_Parameter_Declaration> &parameters) {
  (*ss) << "(";
  for (const auto &parameter : parameters) {
    auto type = global_get_type(parameter.resolved_type);
    if (parameter.tag == AST_PARAM_NORMAL) {
      if (type->is_kind(TYPE_FUNCTION)) {
        (*ss) << get_declaration_type_signature_and_identifier(parameter.normal.name.get_str(), type);
      } else {
        visit(parameter.normal.type);
        (*ss) << ' ' << parameter.normal.name.get_str();
      }
    } else {
      (*ss) << ' ' << to_cpp_string(type) << " self";
    }
  }
  (*ss) << ")";
}

void Emitter ::visit_function_declaration(AST *node) {
  auto emit_function_signature_and_body = [&](const std::string &name) {
    visit(node->function.return_type);
    (*ss) << " " + name;
    emit_default_args = true;
    visit_parameters(node->function.parameters);
    emit_default_args = false;
    defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_FUNC});

    if (node->function.block.is_not_null()) {
      auto block = node->function.block.get();
      visit(block);
    }
    emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
    defer_blocks.pop_back();
  };

  auto emit_various_function_declarations = [&] {
    if (!node->function.generic_parameters.empty()) {
      return;
    }

    if (node->function.name != "main") {
      if ((node->function.flags & FUNCTION_IS_STATIC) != 0) {
        (*ss) << "static ";
      }
      if ((node->function.flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        // This should've already been emitted, by the dependency emitter.
        return;
      }
    }

    if ((node->function.flags & FUNCTION_IS_EXPORTED) != 0) {
      (*ss) << "extern  ";
    }

    std::string name;
    if (node->function.declaring_type != Type::INVALID_TYPE_ID) {
      name += "$" + std::to_string(node->function.declaring_type) + "_";
    }
    name += node->function.name.get_str();
    if (!node->function.generic_arguments.empty()) {
      name += mangled_type_args(node->function.generic_arguments);
    }

    if (node->function.name == "main" && !is_freestanding) {
      has_user_defined_main = true;
      visit(node->function.return_type);
      // TODO: we should clean this up.
      // TODO it's got some strange behavior in a lot of cases.
      (*ss) << " __ela_main_()";
      visit(node->function.block.get());
    } else {
      emit_function_signature_and_body(name);
    }
  };

  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }

  emit_line_directive(node);
  auto test_flag = compile_command.has_flag("test");

  // this also happens to emit the test boilerplate that bootstraps it into the
  // test runner, if applicable.
  if (!should_emit_function(this, node, test_flag)) {
    return;
  }

  if ((node->function.flags & FUNCTION_IS_FOREIGN) != 0) {
    emit_foreign_function(node);
    return;
  }

  emit_various_function_declarations();
}

void Emitter ::visit_declaration(AST *node) {}
void Emitter ::visit_bin_expr(AST *node) {}
void Emitter ::visit_unary_expr(AST *node) {}
void Emitter ::visit_identifier(AST *node) {}
void Emitter ::visit_literal(AST *node) {}

void Emitter ::visit_type(AST *node) {
  auto type = global_get_type(node->resolved_type);
  if (!type) {
    throw_error("internal compiler error: ASTType* resolved to null in emitter.", node->source_range);
  }

  // For reflection
  if (node->type.kind == AST_TYPE_REFLECTION) {
    auto id = node->type.pointing_to.get()->resolved_type;
    if (id == -1)
      throw_error("Invalid type in #type() node", node->source_range);
    auto type = global_get_type(id);
    (*ss) << to_type_struct(type);
    return;
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    (*ss) << get_function_pointer_type_string(type);
    return;
  }

  if (type->is_kind(TYPE_ENUM)) {
    auto enum_info = type->info.$enum;
    auto elem_ty = global_get_type(enum_info.element_type);
    (*ss) << to_cpp_string(elem_ty);
    return;
  }

  auto type_string = to_cpp_string(type);

  (*ss) << type_string;
}

void Emitter ::visit_tuple(AST *node) {}
void Emitter ::visit_call(AST *node) {
  auto base_symbol = typer.get_symbol(node->call.callee);

  std::vector<int> generic_args;
  for (const auto arg : node->call.generic_arguments) {
    generic_args.push_back(arg->resolved_type);
  }

  auto symbol = base_symbol.get();
  if (node->call.callee->node_type == AST_DOT_EXPR) {
    if (!base_symbol || !base_symbol.get()->is_function()) {
      throw_error("can't call a non-function", node->source_range);
    }

    auto func = symbol->function.declaration;

    auto method_call = (func->function.flags & FUNCTION_IS_METHOD) != 0;
    auto static_method = (func->function.flags & FUNCTION_IS_STATIC) != 0;

    if (!method_call || static_method) {
      throw_error("cannot call a static method from an instance", node->source_range);
    }

    auto base_type = global_get_type(get_expr_left_type_sr_dot(node->call.callee));
    if (!base_type) {
      throw_error("internal compiler error: unable to find method call", node->source_range);
    }

    (*ss) << "$" << std::to_string(base_type->base_id == -1 ? base_type->id : base_type->base_id) << "_"
          << symbol->name.get_str();
    (*ss) << mangled_type_args(generic_args);
    (*ss) << "(";

    Type *function_type = global_get_type(symbol->type_id);
    // if generic function
    if (!function_type) {
      auto instance = find_generic_instance(func->function.generic_instantiations,
                                            typer.get_generic_arg_types(node->call.generic_arguments));
      function_type = global_get_type(instance->resolved_type);
    }

    auto param_0_ty = global_get_type(function_type->info.function.parameter_types[0]);

    // TODO: use a more TAC style emitter, so we don't have a problem with R-Values.
    // Right now, chained function calls are quite messed up.
    if (param_0_ty->meta.is_pointer() && !base_type->meta.is_pointer()) {
      (*ss) << "&";
    }

    // eww!
    AST *base = node->call.callee->dot.base;

    visit(base);
    if (node->call.arguments.size() > 0) {
      (*ss) << ", ";
    }

    for (auto &arg : node->call.arguments) {
      visit(arg);
      if (arg != node->call.arguments.back()) {
        (*ss) << ", ";
      }
    }
    (*ss) << ")";
  } else {
    auto func = node->call.callee;
    if (func->node_type == AST_TYPE) {
      auto &ast_type = func->call.callee->type;
      if (ast_type.kind != AST_TYPE_NORMAL) {
        throw_error("Cannot call a tuple or function type", node->source_range);
      }
      if (!ast_type.normal.generic_arguments.empty()) {
        throw_error("internal compiler error: generic args to call put on base", node->source_range);
      }
      func = ast_type.normal.base;
    }
    // normal function call, or a static method.
    visit(func);
    (*ss) << mangled_type_args(generic_args);
    visit_arguments(node->call.arguments);
  }
}
void Emitter ::visit_return(AST *node) {}
void Emitter ::visit_continue(AST *node) {}
void Emitter ::visit_break(AST *node) {}

void Emitter ::visit_for(AST *node) {
  emit_line_directive(node);
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});

  static int depth = 0;
  std::string range_unique_id = "$_range_id" + std::to_string(depth);
  std::string unique_id = "$_loop_id" + std::to_string(depth);
  depth++;

  (*ss) << indent() << "{\n";
  indent_level++;

  std::string range_type_str = to_cpp_string(global_get_type(node->$for.range_type));
  std::string iterable_type_str = to_cpp_string(global_get_type(node->$for.iterable_type));
  std::string identifier_type_str = to_cpp_string(global_get_type(node->$for.identifier_type));
  auto iterable_method_str = "$" + std::to_string(node->$for.iterable_type);

  switch (node->$for.iteration_kind) {
    case ITERABLE:
      indented(range_type_str + " " + range_unique_id + " = ");
      visit(node->$for.range);
      end_line();
      indented(iterable_type_str + " " + unique_id + " = $" + std::to_string(node->$for.range_type) + "_iter(&" +
               range_unique_id + ");\n");
      break;

    case ENUMERABLE:
      indented(range_type_str + " " + range_unique_id + " = ");
      visit(node->$for.range);
      end_line();
      indented(iterable_type_str + " " + unique_id + " = $" + std::to_string(node->$for.range_type) + "_enumerator(&" +
               range_unique_id + ");\n");
      break;

    case ENUMERATOR:
      indented(iterable_type_str + " " + unique_id + " = ");
      visit(node->$for.range);
      end_line();
      break;

    case ITERATOR:
      indented(iterable_type_str + " " + unique_id + " = ");
      visit(node->$for.range);
      end_line();
      break;
  }

  indented("while (!$" + std::to_string(node->$for.iterable_type) + "_done(" + unique_id + ")) {\n");
  indent_level++;

  indented(identifier_type_str + " ");
  (*ss) << node->$for.iter_identifier->identifier.get_str();

  (*ss) << " = ";
  if (node->$for.value_semantic == VALUE_SEMANTIC_POINTER) {
    (*ss) << iterable_method_str << "_current(" << unique_id << ");\n";
  } else if (node->$for.iteration_kind == ENUMERABLE || node->$for.iteration_kind == ENUMERATOR) {
    (*ss) << iterable_method_str << "_current(" << unique_id << ");\n";
  } else {
    (*ss) << "*" << iterable_method_str << "_current(" << unique_id << ");\n";
  }

  // this MUST happen before the block or continue will cause a permanent hangup!!!
  (*ss) << indent() << iterable_method_str << "_next(&" << unique_id << ");\n";

  visit(node->$for.block);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);

  indent_level--;
  indented("}\n");
  indent_level--;
  indented("}\n");
  defer_blocks.pop_back();
}

void Emitter ::visit_if(AST *node) {
  emit_line_directive(node);
  (*ss) << indent() << "if (";
  visit(node->$if.condition);
  (*ss) << ")";
  visit(node->$if.block);
  if (node->$if.$else.is_not_null()) {
    visit(node->$if.$else.get());
  }
}

void Emitter ::visit_else(AST *node) {
  emit_line_directive(node);
  (*ss) << " else ";
  if (node->$else.elseif.is_not_null()) {
    visit(node->$else.elseif.get());
  } else if (node->$else.block.is_not_null()) {
    visit(node->$else.block.get());
  }
}

void Emitter ::visit_while(AST *node) {
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  emit_line_directive(node);
  (*ss) << indent() << "while (";
  if (node->$while.condition.is_not_null()) {
    visit(node->$while.condition.get());
  } else {
    (*ss) << "true";
  }
  (*ss) << ") ";
  visit(node->$while.block);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  defer_blocks.pop_back();
}

void Emitter ::visit_struct_declaration(AST *node) {}
void Emitter ::visit_dot_expr(AST *node) {}
void Emitter ::visit_scope_resolution(AST *node) {}
void Emitter ::visit_subscript(AST *node) {}
void Emitter ::visit_initializer_list(AST *node) {}
void Emitter ::visit_enum_declaration(AST *node) {}
void Emitter ::visit_noop(AST *node) {}
void Emitter ::visit_alias(AST *node) {}
void Emitter ::visit_impl(AST *node) {}
void Emitter ::visit_interface_declaration(AST *node) {}
void Emitter ::visit_size_of(AST *node) {}
void Emitter ::visit_defer(AST *node) {}
void Emitter ::visit_cast(AST *node) {}
void Emitter ::visit_lambda(AST *node) {}
void Emitter ::visit_range(AST *node) {}
void Emitter ::visit_switch(AST *node) {}
void Emitter ::visit_tuple_deconstruction(AST *node) {}
void Emitter ::visit_where(AST *node) {};