#include <ostream>
#include <sstream>
#include <string>

#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
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

bool Emitter::should_emit_function(AST *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->function.flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->function.flags & FUNCTION_IS_TEST) {
    test_functions << "(__COMPILER_GENERATED_TEST){.name = \"" << node->function.name.get_str() << "\", .function = &"
                   << node->function.name.get_str() << "},";
    num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->function.name == "main") {
    return false;
  }
  return true;
}

std::string get_operator_value_string(Token_Type type) {
  switch (type) {
    case Token_Type::Assign:
      return "=";
    case Token_Type::Add:
      return "+";
    case Token_Type::Sub:
      return "-";
    case Token_Type::Mul:
      return "*";
    case Token_Type::Div:
      return "/";
    case Token_Type::Modulo:
      return "%";
    case Token_Type::Range:
      return "..";
    case Token_Type::Arrow:
      return "->";
    case Token_Type::Comma:
      return ",";
    case Token_Type::Semi:
      return ";";
    case Token_Type::Not:
      return "!";
    case Token_Type::LogicalNot:
      return "!";
    case Token_Type::Or:
      return "|";
    case Token_Type::And:
      return "&";
    case Token_Type::SHL:
      return "<<";
    case Token_Type::SHR:
      return ">>";
    case Token_Type::Xor:
      return "^";
    case Token_Type::LogicalOr:
      return "||";
    case Token_Type::LogicalAnd:
      return "&&";
    case Token_Type::LT:
      return "<";
    case Token_Type::GT:
      return ">";
    case Token_Type::EQ:
      return "==";
    case Token_Type::NEQ:
      return "!=";
    case Token_Type::LE:
      return "<=";
    case Token_Type::GE:
      return ">=";
    case Token_Type::LParen:
      return "(";
    case Token_Type::RParen:
      return ")";
    case Token_Type::LBrace:
      return "[";
    case Token_Type::RBrace:
      return "]";
    case Token_Type::DoubleColon:
      return "::";
    case Token_Type::Dot:
      return ".";
    case Token_Type::Increment:
      return "++";
    case Token_Type::Decrement:
      return "--";
    case Token_Type::CompAdd:
      return "+=";
    case Token_Type::CompSub:
      return "-=";
    case Token_Type::CompMul:
      return "*=";
    case Token_Type::CompDiv:
      return "/=";
    case Token_Type::CompMod:
      return "%=";
    case Token_Type::CompAnd:
      return "&=";
    case Token_Type::CompOr:
      return "|=";
    case Token_Type::CompXor:
      return "^=";
    case Token_Type::CompSHL:
      return "<<=";
    case Token_Type::CompSHR:
      return ">>=";
    default:
      break;
  }
  throw_error("Failed to get operator string", {});
  exit(1); // [[noreturn]]
}

size_t calculate_actual_length(const std::string_view &str_view) {
  size_t length = 0;
  for (size_t i = 0; i < str_view.size(); ++i) {
    if (str_view[i] == '\\' && i + 1 < str_view.size()) {
      switch (str_view[i + 1]) {
        case 'n':
        case 't':
        case 'r':
        case '\\':
        case '"':
          ++i; // Skip the escape character
          break;
        case 'x': // Hexadecimal escape sequence
          i += 2; // Skip \x and the next two hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'u': // Unicode escape sequence
          i += 4; // Skip \u and the next four hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'U': // Unicode escape sequence
          i += 8; // Skip \U and the next eight hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        default:
          if (str_view[i + 1] >= '0' && str_view[i + 1] <= '7') { // Octal escape sequence
            ++i;                                                  // Skip the first digit
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i; // Skip the second digit
            }
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i; // Skip the third digit
            }
          }
          break;
      }
    }
    ++length;
  }
  return length;
}

std::string Emitter::get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;
  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    auto &meta = type->meta;

    if (meta.is_array()) {
      identifier += meta.to_string();
    }

    return get_function_pointer_type_string(type, &identifier);
  }
  auto base = type->base.get_str();
  ;
  tss << to_cpp_string(global_get_type(type->get_element_type()));
  if (!type->meta.is_array()) {
    tss << name << ' ';
  }
  bool emitted_iden = false;
  for (const auto meta : type->meta.extensions) {
    if (meta.type == TYPE_EXT_POINTER) {
      tss << "*";
    } else if (meta.type == TYPE_EXT_ARRAY) {
      if (!emitted_iden) {
        emitted_iden = true;
        tss << ' ' << name;
      }
      tss << "[" << std::to_string(meta.array_size) << "]";
    }
  }
  return tss.str();
}

std::string Emitter::get_function_pointer_type_string(Type *type, Nullable<std::string> identifier) {
  auto type_prefix = std::string{"*"};

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error("internal compiler error: tried to get a function pointer from "
                "a non-function type",
                {});
  }

  std::stringstream ss;

  auto info = type->info.function;
  auto return_type = global_get_type(info.return_type);

  ss << to_cpp_string(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

  ss << ")(";

  for (int i = 0; i < info.parameter_types.size(); ++i) {
    auto &param = info.parameter_types[i];
    auto type = global_get_type(param);
    ss << to_cpp_string(type);
    if (i != info.parameter_types.size() - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

std::string Emitter::get_field_struct(const std::string &name, Type *type, Type *parent_type) {
  std::stringstream ss;
  ss << "(Field) { " << std::format(".name = \"{}\", ", name) << std::format(".type = {}, ", to_type_struct(type));

  if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    ss << std::format(".offset = offsetof({}, {})", parent_type->base.get_str(), name);
  }

  if (parent_type->is_kind(TYPE_ENUM)) {
    auto symbol = parent_type->info.scope.lookup(name);
    // We don't check the nullable here because it's an absolute guarantee that enum variables all have
    // a value always.
    auto value = evaluate_constexpr(symbol->variable.initial_value.get());
    ss << std::format(".enum_value = {}", value.integer);
  }

  ss << " }";
  return ss.str();
}

std::string Emitter::get_elements_function(Type *type) {
  //! We have to remove these lambdas so we can compile down to C.
  auto element_type = global_get_type(type->get_element_type());
  if (!type->meta.is_array()) {
    return std::format(".elements = +[](char * array) -> _array<Element> {{\n"
                       "  auto arr = (_array<{}>*)(array);\n"
                       "  _array<Element> elements;\n"
                       "  for (int i = 0; i < arr->length; ++i) {{\n"
                       "    elements.push({{\n"
                       "      .data = (char*)&(*arr)[i],\n"
                       "      .type = {},\n"
                       "    }});\n"
                       "  }}\n"
                       "  return elements;\n"
                       "}}\n",
                       to_cpp_string(element_type), to_type_struct(element_type));
  } else {
    auto size = type->meta.extensions.back().array_size;
    return std::format(".elements = +[](char * array) -> _array<Element> {{\n"
                       "  auto arr = ({}*)(array);\n"
                       "  _array<Element> elements;\n"
                       "  for (int i = 0; i < {}; ++i) {{\n"
                       "    elements.push({{\n"
                       "      .data = (char*)&arr[i],\n"
                       "      .type = {},\n"
                       "    }});\n"
                       "  }}\n"
                       "  return elements;\n"
                       "}}\n",
                       to_cpp_string(element_type), size, to_type_struct(element_type));
  }
}

std::string get_type_flags(Type *type) {
  int kind_flags = 0;
  // TODO: refactor this for new String/str types?
  // And C string.
  // For now we'll just say it's u8* only.
  if (type->id == global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER}}})) {
    return std::format(".flags = {}\n", TYPE_FLAGS_STRING);
  }
  switch (type->kind) {
    case TYPE_SCALAR: {
      auto sint = type->id == s32_type() || type->id == s8_type() || type->id == s16_type() || type->id == s32_type() ||
                  type->id == s64_type();

      auto uint = type->id == u8_type() || type->id == u16_type() || type->id == u32_type() || type->id == u64_type();

      auto floating_pt = type->id == f32_type() || type->id == f64_type();
      if (sint) {
        kind_flags |= TYPE_FLAGS_SIGNED;
      } else if (uint) {
        kind_flags |= TYPE_FLAGS_UNSIGNED;
      }

      if (sint || uint) {
        kind_flags |= TYPE_FLAGS_INTEGER;
      } else if (floating_pt) {
        kind_flags |= TYPE_FLAGS_FLOAT;
      } else if (type->id == bool_type()) {
        kind_flags |= TYPE_FLAGS_BOOL;
      }
      break;
    }
    case TYPE_FUNCTION:
      kind_flags = TYPE_FLAGS_FUNCTION;
      break;
    case TYPE_STRUCT:
      kind_flags = TYPE_FLAGS_STRUCT;
      break;
    case TYPE_ENUM:
      kind_flags = TYPE_FLAGS_ENUM;
      break;
    // TODO: We need to let struct types know that they're a union when they are.
    // case TYPE_UNION:
    //   kind_flags = TYPE_FLAGS_UNION;
    //   break;
    case TYPE_TUPLE:
      kind_flags = TYPE_FLAGS_TUPLE;
      break;
    case TYPE_INTERFACE:
      kind_flags = 0;
      break;
  }
  for (const auto &meta : type->meta.extensions) {
    switch (meta.type) {
      case TYPE_EXT_POINTER:
        kind_flags |= TYPE_FLAGS_POINTER;
        break;
      case TYPE_EXT_ARRAY:
        kind_flags |= TYPE_FLAGS_ARRAY;
        break;
      case TYPE_EXT_INVALID:
        throw_error("internal compiler error: Extension type not set.", {});
        break;
    }
  }
  return ".flags = " + std::to_string(kind_flags) + "\n";
}

std::string Emitter::get_type_struct(Type *type, int id, const std::string &fields) {
  std::stringstream ss;

  if (!type) {
    throw_error("internal compiler error: type was null in 'get_type_struct()' reflection emitter", {});
  }

  auto kind = 0;

  ss << "_type_info.data[" << id << "]" << "= malloc(sizeof(Type));\n";
  ss << "*_type_info.data[" << id << "] = (Type) {" << ".id = " << id << ", "
     << ".name = \"" << type->to_string() << "\", ";

  if (!type->is_kind(TYPE_ENUM))
    ss << ".size = sizeof(" << to_cpp_string(type) << "), ";

  ss << get_type_flags(type) << ",\n";

  // ! We can't use this either: it uses a lambda.
  //   if (type->meta.is_fixed_sized_array()) {
  //     ss << get_elements_function(type) << ",\n";
  //   }

  if (type->meta.is_pointer() || type->meta.is_array()) {
    ss << ".element_type = " << to_type_struct(global_get_type(type->get_element_type())) << ",\n";
  } else {
    ss << ".element_type = NULL,\n";
  }

  ss << " };";

  auto get_fields_init_statements = [&] {
    std::stringstream fields_ss;
    if (type->kind == TYPE_STRUCT) {
      auto info = type->info;
      if (info.scope.empty()) {
        return std::string("{}");
      }

      int count = info.scope.fields_count();
      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &sym : info.scope) {
        if (sym.name == "this")
          continue;

        auto t = global_get_type(sym.type_id);
        // TODO: handle methods separately
        if (t->is_kind(TYPE_FUNCTION) || (sym.flags & SYMBOL_IS_FUNCTION))
          continue;

        if (!t)
          throw_error("internal compiler error: Type was null in reflection 'to_type_struct()'", {});

        fields_ss << "_type_info.data[" << id << "]->fields.data[" << it << "] = ";
        fields_ss << get_field_struct(sym.name.get_str(), t, type) << ";\n";
        ++it;
      }
    } else if (type->kind == TYPE_ENUM) {
      // TODO: we have to fix this!.
      auto info = type->info;
      if (info.scope.empty()) {
        return std::string("{}");
      }

      int count = info.scope.fields_count();

      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &symbol : info.scope) {
        if (symbol.is_function())
          continue;

        auto t = global_get_type(s32_type());

        if (!t) {
          throw_error("internal compiler error: Type was null in reflection 'to_type_struct()'", {});
        }

        fields_ss << "_type_info.data[" << id << "]->fields.data[" << it << "] = ";
        fields_ss << get_field_struct(symbol.name.get_str(), t, type) << ";\n";
        ++it;
      }
    }
    return fields_ss.str();
  };

  ss << get_fields_init_statements();
  type_info_strings.push_back(ss.str());
  return std::format("_type_info.data[{}]", id);
}

std::string Emitter::to_type_struct(Type *type) {
  if (!type) {
    throw_error("internal compiler error: Reflection system got a null type", {});
  }

  auto id = type->id;

  static bool *type_cache = [] {
    auto arr = new bool[type_table.size()];
    memset(arr, 0, type_table.size());
    return arr;
  }();

  if (type_cache[id]) {
    return std::format("_type_info.data[{}]", id);
  }

  type_cache[id] = true;

  return get_type_struct(type, id, "{}");
}

void Emitter::emit_foreign_function(AST *node) {
  if (node->function.name == "main") {
    throw_error("main function cannot be foreign", node->source_range);
  }
  (*ss) << "extern ";
  (*ss) << get_cpp_scalar_type(node->function.return_type->resolved_type);
  space();
  (*ss) << node->function.name.get_str() << '(';
  for (int i = 0; i < node->function.parameters.size(); ++i) {
    auto &param = node->function.parameters[i];
    (*ss) << get_cpp_scalar_type(param.resolved_type);
    if (i != node->function.parameters.size() - 1) {
      (*ss) << ", ";
    }
  }
  if ((node->function.flags & FUNCTION_IS_VARARGS) != 0) {
    (*ss) << ", ...);";
  } else {
    (*ss) << ");";
  }
}

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
  if (!should_emit_function(node, test_flag)) {
    return;
  }

  if ((node->function.flags & FUNCTION_IS_FOREIGN) != 0) {
    emit_foreign_function(node);
    return;
  }

  emit_various_function_declarations();
}

void Emitter ::visit_declaration(AST *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  emit_line_directive(node);

  // Emit switch / if expressions.
  if (node->declaration.value &&
      (node->declaration.value.get()->node_type == AST_SWITCH || node->declaration.value.get()->node_type == AST_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    visit(node->declaration.value.get());
    (*ss) << to_cpp_string(global_get_type(node->declaration.type->resolved_type)) << " "
          << node->declaration.name.get_str() << " = " << str;
    return;
  }

  if (node->declaration.type->resolved_type == Type::INVALID_TYPE_ID) {
    throw_error("internal compiler error: type was null upon emitting an ASTDeclaration", node->source_range);
  }

  auto type = global_get_type(node->declaration.type->resolved_type);
  auto symbol = node->parent->local_lookup(node->declaration.name);

  auto handle_initialization = [&]() {
    if (node->declaration.value.is_not_null() && emit_default_value) {
      (*ss) << " = ";
      visit(node->declaration.value.get());
    } else if (emit_default_init) {
      auto type = global_get_type(node->declaration.type->resolved_type);
      if (type->is_kind(TYPE_STRUCT)) {
        (*ss) << "= (" + to_cpp_string(type) + ") {}";
      } else {
        (*ss) << "= (" + to_cpp_string(type) + ") {0}";
      }
    }
    (*ss) << ";\n";
  };

  auto old = emit_default_init;
  Defer _([&] { emit_default_init = old; });
  if (node->declaration.is_extern) {
    (*ss) << "extern ";
    emit_default_init = false;
  }
  if (node->declaration.is_static) {
    (*ss) << "static ";
  }
  if (node->declaration.is_constexpr) {
    (*ss) << "static constexpr ";
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    (*ss) << get_declaration_type_signature_and_identifier(node->declaration.name.get_str(), type);
    handle_initialization();
    return;
  }

  if (node->declaration.is_bitfield) {
    visit(node->declaration.type);
    space();
    (*ss) << node->declaration.name.get_str();
    space();
    (*ss) << ": " << node->declaration.bitsize.get_str();
    handle_initialization();
    return;
  }

  if (type->meta.is_array()) {
    (*ss) << get_declaration_type_signature_and_identifier(node->declaration.name.get_str(), type);
    if (node->declaration.value.is_not_null()) {
      (*ss) << " = ";
      visit(node->declaration.value.get());
    } else if (emit_default_init) {
      (*ss) << "= {0}";
    }
    return;
  }

  visit(node->declaration.type);
  space();
  (*ss) << node->declaration.name.get_str();
  space();
  handle_initialization();
  return;
}

void Emitter ::visit_bin_expr(AST *node) {
  if (node->binary.op == Token_Type::Assign &&
      (node->binary.right->node_type == AST_SWITCH || node->binary.right->node_type == AST_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    visit(node->binary.right);
    visit(node->binary.left);
    (*ss) << " = " << str;
    return;
  }

  auto left_ty = global_get_type(node->binary.left->resolved_type);

  if (left_ty && node->binary.is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_BINARY, node->binary.op, node->binary.left,
                           node->binary.right);
    return;
  }

  auto op_ty = node->binary.op;
  (*ss) << "(";
  visit(node->binary.left);
  space();
  (*ss) << get_operator_value_string(node->binary.op);
  if (node->binary.op == Token_Type::Assign) {
    auto type = global_get_type(node->resolved_type);
    auto isptr = type->meta.is_pointer();
    if (isptr)
      (*ss) << "(" << to_cpp_string(type) << ")";
  }
  space();
  visit(node->binary.right);
  (*ss) << ")";
}

void Emitter ::visit_unary_expr(AST *node) {
  if (node->unary.op == Token_Type::Sub) {
    auto type = to_cpp_string(global_get_type(node->unary.operand->resolved_type));
    (*ss) << '(' << type << ')';
  }
  auto left_type = node->unary.operand->resolved_type;
  auto left_ty = global_get_type(left_type);

  if (left_ty && node->unary.is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_UNARY, node->unary.op, node->unary.operand, node);
    return;
  }

  auto type = global_get_type(left_type);

  // we always do these as postfix unary since if we don't it's kinda undefined
  // behaviour and it messes up unary expressions at the end of dot expressions
  if (node->unary.op == Token_Type::Increment || node->unary.op == Token_Type::Decrement) {
    visit(node->unary.operand);
    (*ss) << get_operator_value_string(node->unary.op);
  } else {
    (*ss) << '(';
    (*ss) << get_operator_value_string(node->unary.op);
    visit(node->unary.operand);
    (*ss) << ")";
  }
}

void Emitter ::visit_identifier(AST *node) { (*ss) << node->identifier.get_str(); }

void Emitter ::visit_literal(AST *node) {
  auto type = to_cpp_string(global_get_type(node->resolved_type));
  std::string output;
  switch (node->literal.tag) {
    case LITERAL_NULL:
      (*ss) << "NULL";
      return;
    case LITERAL_STRING: {
      if (node->literal.is_c_string) {
        output = std::format("\"{}\"", node->literal.value.get_str());
      } else {
        // TODO:
        // We don't want null terminated strings, but the problem is, if we use an initializer list for an array of
        // bytes, then all of our string literals are stack allocated. If we make them static, then there's a chance
        // that the user mutates the string literal, and it will change it's meaning for the rest of the program

        // I have spent literally all day figting these two probelms, and I have decided it is time to move on, for now,
        // we will keep the null terminated strings until we have a solution for this.
        auto str = node->literal.value.get_str();
        (*ss) << std::format("(str) {{ .data = \"{}\", .length = {} }}", str, calculate_actual_length(str));
        return;
      }
    } break;
    case LITERAL_FLOAT:
      if (node->resolved_type != f64_type()) {
        output = node->literal.value.get_str() + "f";
      } else {
        output = node->literal.value.get_str();
      }
      break;
    // TODO : emit character literals as hexadecimal values so we're UTF8 friendly.
    // that's why we have fat u32 chars anyway.
    case LITERAL_CHAR:
      output = '\'' + node->literal.value.get_str() + '\'';
      break;
    case LITERAL_INTEGER:
      output = node->literal.value.get_str();
      break;
    case LITERAL_BOOL:
      output = node->literal.value.get_str();
      break;
  }
  (*ss) << output;
}

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

void Emitter ::visit_tuple(AST *node) {
  auto type = global_get_type(node->resolved_type);
  auto name = "(" + to_cpp_string(type) + ")";
  (*ss) << name << " {";
  for (int i = 0; i < node->tuple.size(); ++i) {
    auto &value = node->tuple[i];
    (*ss) << ".$" << std::to_string(i) << " = ";
    visit(value);
    if (i != node->tuple.size() - 1)
      (*ss) << ", ";
  }
  (*ss) << "}";
}

void Emitter ::visit_call(AST *node) {
  auto base_symbol = typer.get_symbol(node->call.callee);

  std::vector<int> generic_args;
  for (const auto arg : node->call.generic_arguments) {
    generic_args.push_back(arg->resolved_type);
  }

  auto symbol = base_symbol.get();
  if (node->call.callee->node_type == AST_DOT) {
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

void Emitter::visit_return(AST *node) {
  emit_line_directive(node);

  // Emit switch / if expressions.
  if (node->$return && (node->$return.get()->node_type == AST_SWITCH || node->$return.get()->node_type == AST_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    visit(node->$return.get());

    if (emitting_function_with_defer ||
        (node->declaring_block && node->declaring_block.get()->block.defer_count != 0)) {
      (*ss) << to_cpp_string(type) << " " << defer_return_value_key << " = " << str << ";\n";
      emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
      (*ss) << "return " << defer_return_value_key << ";\n";
    } else {
      (*ss) << "return " << str << ";\n";
    }
    return;
  }

  if (emitting_function_with_defer || (node->declaring_block && node->declaring_block.get()->block.defer_count != 0)) {
    if (node->$return) {
      auto type = global_get_type(node->resolved_type);
      (*ss) << to_cpp_string(type) << " " << defer_return_value_key << " = ";
      visit(node->$return.get());
      (*ss) << ";\n";
    }
    emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
    (*ss) << "return";
    if (node->$return) {
      (*ss) << " " << defer_return_value_key;
    }
    (*ss) << ";\n";
  } else if (cf_expr_return_register.is_not_null() || !node->$return) {
    indented("return");
    if (node->$return) {
      space();
      visit(node->$return.get());
    }
    (*ss) << ";\n";
  } else {
    (*ss) << cf_expr_return_register.get() << " = ";
    visit(node->$return.get());
    (*ss) << ";\n";
  }
}

void Emitter ::visit_continue(AST *node) {
  emit_line_directive(node);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  indented("continue;\n");
}

void Emitter ::visit_break(AST *node) {
  emit_line_directive(node);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  indented("break;\n");
}

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

void Emitter ::visit_struct_declaration(AST *node) {
  if (node->is_emitted) {
    return;
  }

  if (!node->$struct.generic_parameters.empty()) {
    return;
  }

  node->is_emitted = true;

  auto old_init = emit_default_init;
  auto old_default_val = emit_default_value;

  Defer _defer([&] {
    emit_default_init = old_init;
    emit_default_value = old_default_val;
  });

  emit_default_init = false;
  emit_default_value = false;

  emit_line_directive(node);
  auto type = global_get_type(node->resolved_type);

  auto info = type->info.$struct;

  std::string type_name = type->base.get_str();
  std::string type_tag = (node->$struct.is_union ? "typedef union " : "typedef struct ");

  if ((info.flags & STRUCT_FLAG_FORWARD_DECLARED || node->$struct.is_fwd_decl) != 0) {
    if (node->$struct.is_extern) {
      // (*ss) << "extern ";
      // I do not believe this is ever neccesary in C, you can alwasy just define an
      // opaque struct and link against it, or redefine it: it doesn't matter.
    }
    (*ss) << type_tag << " " << type_name << " " << type_name << ";\n";
    return;
  }

  if ((info.flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << (node->$struct.is_union ? "union " : "struct ");
    (*ss) << "{\n";
  } else {
    if (node->$struct.is_extern) {
      (*ss) << "extern ";
    }
    (*ss) << type_tag << " " << type_name << "{\n";
  }
  indent_level++;

  auto old = emit_default_init;

  emit_default_init = false;

  Defer _defer1([&] { emit_default_init = old; });

  for (const auto &subtype : node->$struct.subtypes) {
    indented("");
    visit(subtype);
    semicolon();
    newline();
  }

  for (const auto &member : node->$struct.members) {
    indented("");
    auto type = global_get_type(member.type->resolved_type);
    if (type->is_kind(TYPE_FUNCTION)) {
      auto name = member.name.get_str();
      auto name_nullable = Nullable(&name);
      (*ss) << get_function_pointer_type_string(type, name_nullable);
    } else if (type->meta.is_array()) {
      (*ss) << get_declaration_type_signature_and_identifier(member.name.get_str(), type);
    } else {
      visit(member.type);
      space();
      (*ss) << member.name.get_str();
    }
    semicolon();
    newline();
  }

  // this is for anonymous substructs which just unfold at C compile time into the struct's namespace.
  if ((info.flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << "};\n";
  } else {
    (*ss) << "} " << type_name << ";\n";
  }

  indent_level--;
}

void Emitter ::visit_dot_expr(AST *node) {
  auto base_ty_id = node->dot.base->resolved_type;
  auto base_ty = global_get_type(base_ty_id);
  auto op = ".";
  if (base_ty->meta.back_type() == TYPE_EXT_POINTER) {
    op = "->";
  }
  visit(node->dot.base);
  (*ss) << op;
  if (base_ty->is_kind(TYPE_TUPLE)) {
    (*ss) << "$";
  }
  (*ss) << node->dot.member_name.get_str();
}

void Emitter ::visit_scope_resolution(AST *node) {
  auto type = global_get_type(node->scope_resolution.base->resolved_type);

  if (node->scope_resolution.base->node_type == AST_IDENTIFIER || node->scope_resolution.base->node_type == AST_TYPE) {
    if (type->is_kind(TYPE_ENUM)) {
      (*ss) << type->base.get_str();
    } else {
      (*ss) << "$" + std::to_string(type->id);
    }
  } else {
    visit(node->scope_resolution.base);
  }
  auto op = "_";
  (*ss) << op << node->scope_resolution.member_name.get_str();
}

void Emitter ::visit_subscript(AST *node) {
  auto left_ty = global_get_type(node->subscript.left->resolved_type);
  if (left_ty && node->subscript.is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_SUBSCRIPT, Token_Type::LBrace, node->subscript.left,
                           node->subscript.index_expression);
    return;
  }
  visit(node->subscript.left);
  (*ss) << '[';
  visit(node->subscript.index_expression);
  (*ss) << ']';
}

void Emitter ::visit_initializer_list(AST *node) {
  auto type = global_get_type(node->resolved_type);

  if (!type->meta.is_array()) {
    (*ss) << "(" + to_cpp_string(type) + ")";
  }
  (*ss) << " {";

  switch (node->initializer.tag) {
    case INITIALIZER_EMPTY: {
      (*ss) << "0}";
      return;
    }
    case INITIALIZER_NAMED: {
      const auto size = node->initializer.key_values.size();
      for (int i = 0; i < node->initializer.key_values.size(); ++i) {
        const auto &[key, value] = node->initializer.key_values[i];
        (*ss) << '.' << key.get_str() << " = ";
        (*ss) << "(" << to_cpp_string(global_get_type(value->resolved_type)) << ")";
        visit(value);
        if (i != size - 1) {
          (*ss) << ",\n";
        }
      }
    } break;
    case INITIALIZER_COLLECTION: {
      if (type->base.get_str().starts_with("Init_List$")) {
        auto element_type = type->generic_args[0];
        (*ss) << " .data = ";
        (*ss) << "(" << to_cpp_string(global_get_type(element_type)) << "[]) {";
        for (const auto &expr : node->initializer.values) {
          visit(expr);
          if (expr != node->initializer.values.back()) {
            (*ss) << ", ";
          }
        }
        (*ss) << "}, .length = " << std::to_string(node->initializer.values.size());
      } else {
        for (const auto &expr : node->initializer.values) {
          visit(expr);
          if (expr != node->initializer.values.back()) {
            (*ss) << ", ";
          }
        }
      }

    } break;
  }
  (*ss) << "}";
}

void Emitter ::visit_enum_declaration(AST *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  emit_line_directive(node);
  auto type_name = node->$enum.name.get_str();
  int n = 0;
  (*ss) << "typedef enum {\n";
  for (const auto &[key, value] : node->$enum.key_values) {
    (*ss) << type_name << "_" << key.get_str();
    if (node->$enum.is_flags) {
      (*ss) << " = ";
      (*ss) << std::to_string(1 << n);
    } else if (value) {
      (*ss) << " = ";
      visit(value);
    }
    if (n != node->$enum.key_values.size() - 1) {
      (*ss) << ",\n";
    }
    n++;
  }
  (*ss) << "} " << type_name << ";\n";

  auto type = global_get_type(node->resolved_type);
}

void Emitter::forward_decl_type(Type *type) {
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  if (type->emitted_forward_declaration) {
    return;
  } else {
    type->emitted_forward_declaration = true;
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->info.function;
      for (const auto param : info.parameter_types) {
        forward_decl_type(global_get_type(param));
      }
      forward_decl_type(global_get_type(info.return_type));
    } break;
    case TYPE_STRUCT: {
      auto info = type->info.$struct;
      std::string kw = "typedef struct ";
      if ((info.flags & STRUCT_FLAG_IS_UNION) != 0)
        kw = "typedef union ";
      (*ss) << kw << type->base.get_str() << " " << type->base.get_str() << ";\n";
    } break;
    case TYPE_TUPLE:
      (*ss) << "typedef struct " << to_cpp_string(type) << " " << to_cpp_string(type) << ";\n";
      break;
    default:
      return;
  }
}

// Helper function to emit deferred statements
void Emitter::emit_deferred_statements(DeferBlockType type) {
  auto defer_block = defer_blocks.rbegin();
  while (defer_block->type != type) {
    if (defer_block == defer_blocks.rend()) {
      throw_error("internal compiler error: could not find defer block type in stack", {});
    }
    for (auto defer : defer_block->defers) {
      visit(defer);
      semicolon();
      newline();
    }
    defer_block++;
  }
  if (defer_block == defer_blocks.rend()) {
    throw_error("internal compiler error: could not find defer block type in stack", {});
  }
  for (auto defer : defer_block->defers) {
    visit(defer);
    semicolon();
    newline();
  }
}

void Emitter ::visit_noop(AST *node) {}

void Emitter ::visit_alias(AST *node) {}

void Emitter ::visit_impl(AST *node) {
  if (!node->impl.generic_parameters.empty())
    return;

  auto target = global_get_type(node->impl.target->resolved_type);

  if (!target)
    throw_error("internal compiler error: impl target type was null in the emitter", node->source_range);

  auto old_type = type_context;
  type_context = node->impl.target;
  Defer _([&] { type_context = old_type; });

  for (const auto &method : node->impl.methods)
    visit(method);
}

void Emitter ::visit_interface_declaration(AST *node) {}

void Emitter ::visit_size_of(AST *node) {
  (*ss) << "sizeof(";
  visit(node->size_of);
  (*ss) << ")";
}

int Emitter::get_expr_left_type_sr_dot(AST *node) {
  switch (node->node_type) {
    case AST_TYPE:
      return node->resolved_type;
    case AST_IDENTIFIER:
      return node->parent->scope.lookup(node->identifier)->type_id;
    case AST_DOT: {
      return node->dot.base->resolved_type;
    } break;
    case AST_SCOPE_RESOLUTION: {
      return node->scope_resolution.base->resolved_type;
    } break;
    default:
      throw_error(std::format("internal compiler error: 'get_dot_left_type' encountered an unexpected node, kind {}",
                              (int)node->node_type),
                  node->source_range);
  }
  return Type::INVALID_TYPE_ID;
}

void Emitter ::visit_defer(AST *node) { defer_blocks.back().defers.push_back(node); }

void Emitter ::visit_cast(AST *node) {
  auto type_string = to_cpp_string(global_get_type(node->cast.target_type->resolved_type));
  (*ss) << "(" << type_string << ")";
  visit(node->cast.expression);
}

void Emitter ::emit_lambda(AST *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  emit_line_directive(node);
  visit(node->lambda.return_type);
  (*ss) << ' ' << node->lambda.unique_identifier.get_str() << ' ';
  visit_parameters(node->lambda.parameters);
  visit(node->lambda.block);
  newline();
}

void Emitter::emit_tuple(int type_id) {
  auto type = global_get_type(type_id);
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  if (type->emitted_tuple) {
    return;
  } else {
    type->emitted_tuple = true;
  }
  auto name = to_cpp_string(type);

  (*ss) << "typedef struct {";
  auto info = type->info.tuple;
  for (int i = 0; i < info.types.size(); ++i) {
    auto type = global_get_type(info.types[i]);
    if (type->is_kind(TYPE_FUNCTION)) {
      (*ss) << get_declaration_type_signature_and_identifier("$" + std::to_string(i), type) << ";\n";
    } else {
      auto name = to_cpp_string(type);
      (*ss) << name << " $" << std::to_string(i) << ";\n";
    }
  }
  (*ss) << "} " << name << ";\n";
}

void Emitter ::visit_lambda(AST *node) { (*ss) << node->lambda.unique_identifier.get_str(); }

void Emitter ::visit_range(AST *node) {
  (*ss) << "(" << to_cpp_string(global_get_type(node->resolved_type)) << ") {";
  (*ss) << ".begin = ";
  visit(node->range.left);
  (*ss) << ", .end = ";
  visit(node->range.right);
  (*ss) << "}";
}

void Emitter ::visit_switch(AST *node) {
  auto type = global_get_type(node->$switch.target->resolved_type);
  bool use_eq_operator = true;

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->meta.is_pointer()) {
    use_eq_operator = false;
  }

  auto emit_switch_case = [&](AST *target, const SwitchCase &_case, bool first) {
    if (!first) {
      (*ss) << " else ";
    }
    emit_line_directive(target);
    (*ss) << " if (";
    if (use_eq_operator) {
      visit(target);
      (*ss) << " == ";
      visit(_case.expression);
    } else {
      call_operator_overload(target->source_range, type, OPERATION_BINARY, Token_Type::EQ, target, _case.expression);
    }
    (*ss) << ") ";
    emit_line_directive(_case.block);
    visit(_case.block);
  };
  bool first = true;
  for (const auto &_case : node->$switch.cases) {
    emit_switch_case(node->$switch.target, _case, first);
    first = false;
  }
}

void Emitter ::visit_tuple_deconstruction(AST *node) {
  emit_line_directive(node);

  auto type = global_get_type(node->resolved_type);

  if (type->is_kind(TYPE_TUPLE)) {
    auto block = node->declaring_block;
    if (!block) {
      throw_error("internal compiler error: couldn't generate temporary variable because declaring block was null",
                  node->source_range);
    }
    auto id = block.get()->block.temp_iden_idx++;
    std::string temp_id = "$temp_tuple$" + std::to_string(id++);
    (*ss) << "auto " << temp_id << " = ";
    visit(node->tuple_deconstruction.right);
    (*ss) << ";\n";

    if (node->tuple_deconstruction.op == Token_Type::ColonEquals) {
      for (size_t i = 0; i < node->tuple_deconstruction.idens.size(); ++i) {
        (*ss) << "auto " << node->tuple_deconstruction.idens[i]->identifier.get_str() << " = ";
        (*ss) << temp_id << ".$" << std::to_string(i) << ";\n";
      }
    } else {
      for (size_t i = 0; i < node->tuple_deconstruction.idens.size(); ++i) {
        (*ss) << node->tuple_deconstruction.idens[i]->identifier.get_str() << " = ";
        (*ss) << temp_id << ".$" << std::to_string(i) << ";\n";
      }
    }
  } else {
    auto scope = type->info.scope;
    auto index = 0;
    static int temp_idx = 0;
    std::string identifier = "$deconstruction$" + std::to_string(temp_idx++);
    (*ss) << to_cpp_string(type) << " " << identifier << " = ";
    visit(node->tuple_deconstruction.right);
    semicolon();

    for (const auto symbol : scope) {
      if (symbol.is_function()) {
        continue;
      }
      if (node->tuple_deconstruction.op == Token_Type::ColonEquals) {
        (*ss) << to_cpp_string(global_get_type(symbol.type_id)) << " "
              << node->tuple_deconstruction.idens[index++]->identifier.get_str() << " = ";
        (*ss) << identifier << "." << symbol.name.get_str() << ";\n";
      } else {
        (*ss) << node->tuple_deconstruction.idens[index++]->identifier.get_str() << " = ";
        (*ss) << identifier << "." << symbol.name.get_str() << ";\n";
      }
    }
  }
}

void Emitter ::visit_where(AST *node) {};

Emitter::Emitter(Typer &type_visitor) : typer(type_visitor) { ss = &code; }

std::string Emitter::to_cpp_string(const Type_Metadata &extensions, const std::string &base) {
  std::stringstream ss;
  ss << base;
  for (const auto meta : extensions.extensions) {
    if (meta.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(meta.array_size) << "]";
    } else if (meta.type == TYPE_EXT_POINTER) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string Emitter::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";

  return to_cpp_string(type);

  if (type->meta.has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->meta, name);
}

std::string Emitter::to_cpp_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT: {
      output = to_cpp_string(type->meta, type->base.get_str());
      break;
    }
    case TYPE_TUPLE: {
      auto info = type->info.tuple;
      output = "$tuple";
      for (int i = 0; i < info.types.size(); ++i) {
        output += std::to_string(info.types[i]);
        if (i != info.types.size() - 1) {
          output += "$";
        }
      }
      output = to_cpp_string(type->meta, output);
      break;
    }
    case TYPE_INTERFACE:
      throw_error("can't declare an instance of an interface", {});
      break;
  }
  return output;
}

void Emitter::call_operator_overload(const Source_Range &range, Type *left_ty, OperationKind operation, Token_Type op,
                                     AST *left, AST *right) {
  AST call(AST_CALL);
  AST dot(AST_DOT);
  dot.dot.base = left;
  dot.dot.member_name = get_operator_overload_name(op, operation);
  call.call.callee = &dot;
  if (right) {
    call.call.arguments = {
        right,
    };
  }
  dot.source_range = range;
  call.source_range = range;
  typer.visit(&call);
  this->visit(&call);
}