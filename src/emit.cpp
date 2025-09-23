#include <cstdio>
#include <format>
#include <functional>
#include <iterator>
#include <sstream>
#include <string>

#include "ast.hpp"
#include "builder.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "visitor.hpp"

/*
  TODO:
   This entire visitor needs a huge cleanup. there's some absolutely terrible
  code in here and it's super messy. ? However it works xD
*/
Emitter::Emitter(Context &context, Typer &type_visitor) : typer(type_visitor), ctx(context) {}

static inline size_t calculate_actual_length(const std::string_view &str_view) {
  size_t length = 0;
  for (size_t i = 0; i < str_view.size(); ++i) {
    if (str_view[i] == '\\' && i + 1 < str_view.size()) {
      switch (str_view[i + 1]) {
        case 'n':
        case 't':
        case 'r':
        case '\\':
        case '"':
          ++i;  // Skip the escape character
          break;
        case 'x':  // Hexadecimal escape sequence
          i += 2;  // Skip \x and the next two hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'u':  // Unicode escape sequence
          i += 4;  // Skip \u and the next four hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'U':  // Unicode escape sequence
          i += 8;  // Skip \U and the next eight hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        default:
          if (str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {  // Octal escape sequence
            ++i;                                                   // Skip the first digit
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i;  // Skip the second digit
            }
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i;  // Skip the third digit
            }
          }
          break;
      }
    }
    ++length;
  }
  return length;
}

void append_reflection_type_flags(Type *type, StringBuilder &builder) {
  int kind_flags = 0;
  switch (type->kind) {
    case TYPE_SCALAR: {
      auto sint =
          type == s32_type() || type == s8_type() || type == s16_type() || type == s32_type() || type == s64_type();

      auto uint = type == u8_type() || type == u16_type() || type == u32_type() || type == u64_type();

      auto floating_pt = type == f32_type() || type == f64_type();
      if (sint) {
        kind_flags |= TYPE_FLAGS_SIGNED;
      } else if (uint) {
        kind_flags |= TYPE_FLAGS_UNSIGNED;
      }

      if (sint || uint) {
        kind_flags |= TYPE_FLAGS_INTEGER;
      } else if (floating_pt) {
        kind_flags |= TYPE_FLAGS_FLOAT;
      } else if (type == bool_type()) {
        kind_flags |= TYPE_FLAGS_BOOL;
      }
      break;
    }
    case TYPE_FUNCTION:
      kind_flags = TYPE_FLAGS_FUNCTION;
      break;
    case TYPE_STRUCT: {
      kind_flags = TYPE_FLAGS_STRUCT;
      auto info = type->info->as<StructTypeInfo>();
      if (info->is_union) {
        kind_flags = TYPE_FLAGS_UNION;
      }
    } break;
    case TYPE_ENUM:
      kind_flags = TYPE_FLAGS_ENUM;
      if (type->info->as<EnumTypeInfo>()->is_flags) {
        kind_flags |= TYPE_FLAGS_FLAGS_ENUM;
      }
      break;
    case TYPE_TUPLE:
      kind_flags = TYPE_FLAGS_TUPLE;
      break;
    case TYPE_CHOICE:
      kind_flags = TYPE_FLAGS_CHOICE;
      break;
    case TYPE_TRAIT:
      kind_flags = TYPE_FLAGS_TRAIT;
      break;
    case TYPE_DYN:
      kind_flags = TYPE_FLAGS_DYN;
      break;
  }
  for (const auto &ext : type->extensions) {
    switch (ext.type) {
      // TODO: add specific type flags for mut / const pointers?
      case TYPE_EXT_POINTER_MUT:
      case TYPE_EXT_POINTER_CONST:
        kind_flags |= TYPE_FLAGS_POINTER;
        break;
      case TYPE_EXT_ARRAY:
        kind_flags |= TYPE_FLAGS_ARRAY;
        break;
    }
  }
  builder << ".flags = " + std::to_string(kind_flags) + ",\n";
}

std::string Emitter::get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context) {
  std::stringstream ss;

  const auto name_string =
      std::format(".name = (str){{.data=\"{}\", .length={}}}, ", name, calculate_actual_length(name));

  ss << "(Field) { " << name_string << std::format("._type = {}, ", to_reflection_type_struct(type, context));

  if (type->is_pointer()) {
    ss << std::format(".size = sizeof(void*), ");
  } else if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    if (type->is_kind(TYPE_STRUCT)) {
      auto info = type->info->as<StructTypeInfo>();
      if (info->is_forward_declared) {
        ss << std::format(".size = sizeof({}), ", type_to_string(type));
      } else {
        ss << ".size = 0, ";  // ZST.
      }
    } else {
      ss << std::format(".size = sizeof({}), ", type_to_string(type));
    }

    if (parent_type->is_kind(TYPE_TUPLE)) {
      ss << std::format(
          ".offset = offsetof({}, {})",
          type_to_string(parent_type->base_type == Type::INVALID_TYPE ? parent_type : parent_type->base_type), name);
    } else {
      ss << std::format(
          ".offset = offsetof({}, {})",
          type_to_string(parent_type->base_type == Type::INVALID_TYPE ? parent_type : parent_type->base_type), name);
    }
  }

  if (parent_type->is_kind(TYPE_ENUM)) {
    auto symbol = parent_type->info->as<EnumTypeInfo>()->scope->local_lookup(name);
    // We don't check the nullable here because it's an absolute guarantee that enum variables all have
    // a value always.
    auto value = evaluate_constexpr((ASTExpr *)symbol->variable.initial_value.get(), ctx);
    ss << std::format(".enum_value = {}", value.integer);
  }

  ss << " }";
  return ss.str();
}

std::string Emitter::create_reflection_type_struct(Type *type, int id, Context &context) {
  StringBuilder builder;

  if (!type) {
    throw_error("internal compiler error: type was null in 'get_type_struct()' reflection emitter", {});
  }

  const auto string_id = std::format(REFL_TY_FORMAT_STRING, id);
  const auto type_name_string = type->to_string();
  const auto type_name_strlit =
      std::format("(str){{.data=\"{}\", .length = {}}}", type_name_string, calculate_actual_length(type_name_string));

  const auto get_size = [&] -> std::string {
    // Get the size.
    // We should have our own sizer,
    // But also enums should get a size probably..
    if (!type->is_kind(TYPE_ENUM) && !type->is_kind(TYPE_TRAIT) && !type->is_kind(TYPE_FUNCTION)) {
      if (type->is_pointer()) {
        return ".size = sizeof(void*), ";
      } else {
        return ".size = sizeof(" + type_to_string(type) + "), ";
      }
    }
    return "";
  };

  const auto get_element_type = [&]() -> std::string {
    if (type->is_pointer() || type->is_fixed_sized_array()) {
      return ".element_type = " + to_reflection_type_struct(type->get_element_type(), context) + ",\n";
    } else if (type->is_kind(TYPE_ENUM) && !type->has_extensions()) {
      auto underyling_type = type->info->as<EnumTypeInfo>()->underlying_type;
      return ".element_type = " + to_reflection_type_struct(underyling_type, context) + ",\n";
    } else {
      return ".element_type = NULL,\n";
    }
  };

  const auto get_fields_init_statements = [&] -> std::string {
    if (type->is_kind(TYPE_FUNCTION) || type->info->scope->fields_count() == 0) {
      return ".fields = {.data = NULL, .length = 0, .capacity = 0}, ";
    }
    size_t length = 0;
    StringBuilder builder;

    const auto label = std::format("$fields_array{}", id);
    builder << std::format("Field {}[] = {{", label);

    auto info = type->info;
    length = info->scope->fields_count();
    if (length == 0) {
      return {};
    }

    for (const auto &[name, sym_type, _, __] : info->members) {
      builder << get_field_struct(name.get_str(), sym_type, type, context) << ", ";
    }

    builder << "};\n";
    reflection_initialization << builder.str();

    return std::format(".fields = {{ .data = {}, .length = {}, .capacity = {}}},", label, length, length);
  };

  // Not done.
  const auto get_generic_args_init_statements = [&] -> std::string {
    const auto label = std::format("$gen_args_array{}", id);

    if (type->generic_args.empty()) {
      return ".generic_args = { .data = NULL, .length = 0, .capacity = 0 }, ";
    }

    StringBuilder generics_ss;

    generics_ss << std::format("Type *{}[] = {{", label);

    for (const auto &arg : type->generic_args) {
      generics_ss << to_reflection_type_struct(arg, ctx) << ", ";
    }

    generics_ss << "};\n";

    const int length = type->generic_args.size();

    reflection_initialization << generics_ss.str() << "\n";

    return std::format(".generic_args = {{.data = {}, .length = {}, .capacity = {}}}, ", label, length, length);
  };

  const auto get_traits_init_stmts = [&] -> std::string {
    if (type->traits.empty()) {
      return ".traits = { .data = NULL, .length = 0, .capacity = 0}, ";
    }

    const auto label = std::format("$traits_array{}", id);
    StringBuilder builder;

    builder << std::format("Type *{}[] = {{", label);
    for (const auto &trait : type->traits) {
      builder << to_reflection_type_struct(trait, ctx) << ", \n";
    }
    builder << "};\n";
    reflection_initialization << builder.str();

    const size_t length = type->traits.size();
    return std::format(".traits = {{ .data = {}, .length = {}, .capacity = {}}}, ", label, length, length);
  };

  const auto get_methods_init_statements = [&] -> std::string {
    std::stringstream builder;

    auto scope = type->info->scope;
    size_t length = 0;

    for (const auto &[name, symbol] : scope->symbols) {
      if (!symbol.is_function || !symbol.function.declaration) {
        continue;
      }
      auto declaration = symbol.function.declaration;
      if (!declaration->is_method || declaration->generic_arguments.size() != 0) {
        continue;
      }
      length++;
    }

    if (length == 0) {
      return ".methods = {}, ";
    }

    const auto label = std::format("$methods_array{}", id);
    builder << std::format("Method {}[] = {{", label);

    for (const auto &[name, symbol] : scope->symbols) {
      if (!symbol.is_function || symbol.is_generic_function()) {
        continue;
      }

      auto declaration = symbol.function.declaration;
      if (!declaration->is_method) {
        continue;
      }

      const auto name_strlit = std::format("(str){{.data = \"{}\", .length =  {}}}", name.get_str(),
                                           calculate_actual_length(name.get_str()));

      builder << std::format("{{ .name = {}, .pointer = {} }}, ", name_strlit, emit_symbol((Symbol *)&symbol));
    }
    builder << "};\n";

    reflection_initialization << builder.str();

    return std::format(".methods = {{ .data = {}, .length = {}, .capacity = {}}}, ", label, length, length);
  };

  builder << std::format("const Type {} = (Type){{ .id = {}, .name = {}, ", string_id, id, type_name_strlit);
  builder << get_size();
  append_reflection_type_flags(type, builder);
  builder << get_element_type();
  builder << get_fields_init_statements();
  builder << get_generic_args_init_statements();
  builder << get_traits_init_stmts();

  if (!type->is_kind(TYPE_TRAIT)) {
    builder << get_methods_init_statements();
  }

  builder << " };\n";

  reflection_externs << std::format("extern const Type {};\n", string_id);
  reflection_initialization << builder.str();

  reflected_upon_types.insert(id);

  return "((Type *)(&" + string_id + "))";
}

std::string Emitter::to_reflection_type_struct(Type *type, Context &context) {
  if (!type) {
    throw_error("internal compiler error: Reflection system got a null type", {});
  }

  const auto id = type->uid;
  static bool *reflection_type_cache = [] {
    auto arr = new bool[type_table.size()];
    memset(arr, 0, type_table.size() * sizeof(bool));
    return arr;
  }();

  /*
    Has this type already been emitted to a struct?
    if so, reference it.
  */
  if (reflection_type_cache[id]) {
    return "((Type *)(&" + std::format(REFL_TY_FORMAT_STRING, id) + "))";
  }

  reflection_type_cache[id] = true;
  return create_reflection_type_struct(type, id, context);
}

// These should never get hit.
void Emitter::visit(ASTModule *) {}
void Emitter::visit(ASTImport *) {}
void Emitter::visit(ASTWhere *) {}
void Emitter::visit(ASTTraitDeclaration *) {}
void Emitter::visit(ASTAlias *) {}

void Emitter::forward_decl_type(Type *type) {
  if (type->base_type != Type::INVALID_TYPE) {
    type = type->base_type;
  }
  if (type->fwd_decl_is_emitted) {
    return;
  } else {
    type->fwd_decl_is_emitted = true;
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->info->as<FunctionTypeInfo>();
      for (size_t i = 0; i < info->params_len; i++) {
        forward_decl_type(info->parameter_types[i]);
      }
      forward_decl_type(info->return_type);
    } break;
    case TYPE_TUPLE:
    case TYPE_CHOICE:
    case TYPE_STRUCT: {
      auto info = type->info->as<StructTypeInfo>();
      std::string kw = "typedef struct ";

      if (info->is_union) {
        kw = "typedef union ";
      }
      code << kw << type_to_string(type) << " " << type_to_string(type) << ";\n";
    } break;
    case TYPE_DYN: {
      dep_emitter->define_type(type);
    } break;
    default:

      return;
  }
}

void Emitter::visit(ASTWhile *node) {
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  emit_line_directive(node);
  if (node->condition.is_not_null()) {
    auto condition = node->condition.get();
    if (condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
      emit_pattern_match_for_while(node, (ASTPatternMatch *)condition);
      return;
    }
    code << indent() << "while (";
    condition->accept(this);
  } else {
    code << indent() << "while (";
    code << "true";
  }
  code << ") ";
  node->block->accept(this);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  defer_blocks.pop_back();
  return;
}

void Emitter::visit(ASTIf *node) {
  if (node->condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
    emit_pattern_match_for_if(node, (ASTPatternMatch *)node->condition);
    return;
  }

  newline();
  emit_line_directive(node);
  if (node->is_expression) {
    if (!node->_else) {  // TODO: put this in typer.
      throw_error("cannot use an 'if' expression without at least an 'else'.", node->source_range);
    }
    code << "({";
    static std::string the_register;
    the_register = "regsiter$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &the_register;

    auto type = type_to_string(node->resolved_type);
    code << type << " " << the_register;
    semicolon();

    code << indent() << "if (";
    node->condition->accept(this);
    code << ") ";
    node->block->accept(this);
    node->_else.get()->accept(this);

    code << the_register;
    cf_expr_return_register = nullptr;
    semicolon();

    code << "})";
  } else {
    code << indent() << "if (";
    node->condition->accept(this);
    code << ") ";
    node->block->accept(this);
    if (node->_else.is_not_null()) {
      node->_else.get()->accept(this);
    }
  }
}

void Emitter::visit(ASTElse *node) {
  emit_line_directive(node);
  code << indent() << "else";
  if (node->_if.is_not_null()) {
    newline();
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return;
}

void Emitter::visit(ASTFor *node) {
  auto old_scope = ctx.scope;
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  ctx.set_scope(node->block->scope);

  emit_line_directive(node);
  code << indent() << "{\n";
  indent_level++;

  auto iterable_type = node->iterable_type;
  auto iterable_scope = iterable_type->info->scope;
  auto iterator_type = node->iterator_type;
  auto iterator_scope = iterator_type->info->scope;

  switch (node->iteration_kind) {
    case ASTFor::ITERABLE:
      emit_line_directive(node);
      code << indent() << type_to_string(iterable_type) << " $iterable = ";
      node->right->accept(this);
      code << ";\n";
      emit_line_directive(node);
      code << indent() << type_to_string(iterator_type)
           << " $iterator = " << emit_symbol(iterable_scope->local_lookup("iter")) << "(&$iterable);\n";
      break;
    case ASTFor::ITERATOR:
      emit_line_directive(node);
      code << indent() << type_to_string(iterator_type) << " $iterator = ";
      node->right->accept(this);
      code << ";\n";
      break;
  }

  code << '\n';
  emit_line_directive(node);
  code << '\n';
  code << indent() << "for (auto $next = ";
  code << emit_symbol(iterator_scope->local_lookup("next")) << "(&$iterator);\n";
  code << "$next.index != 1; ";
  code << "$next = ";
  code << emit_symbol(iterator_scope->local_lookup("next")) << "(&$iterator)) {\n";

  auto identifier_type_str = type_to_string(node->identifier_type);

  if (node->left_tag == ASTFor::IDENTIFIER) {
    emit_line_directive(node);

    // we have to do this for function pointers.
    // it's likely we'll have to do this for all the tuple destructures and all that crap.
    code << indent()
         << get_declaration_type_signature_and_identifier(emit_symbol(ctx.scope->local_lookup(node->left.identifier)),
                                                          node->identifier_type);

    code << " = $next.Some.$0;\n";
  } else if (node->left_tag == ASTFor::DESTRUCTURE) {
    auto type = node->identifier_type;

    auto block = node->block;
    auto id = block->temp_iden_idx++;
    std::string temp_id = "$deconstruction$" + std::to_string(id++);

    code << indent() << "auto " << temp_id << " = $next.Some.$0;\n";

    int i = 0;
    for (auto &[name, sym_type, _, __] : type->info->members) {
      auto &destruct = node->left.destructure[i];
      auto iden = destruct.identifier;
      emit_line_directive(node);
      code << indent() << "auto " << iden.get_str();
      code << " = ";

      if (is_pointer_semantic(destruct.semantic)) {
        if (type->is_pointer()) {
          code << "&" << temp_id << "->" << name.get_str() << ";\n";
        } else {
          code << "&" << temp_id << "." << name.get_str() << ";\n";
        }
      } else {
        if (type->is_pointer()) {
          code << temp_id << "->" << name.get_str() << ";\n";
        } else {
          code << temp_id << "." << name.get_str() << ";\n";
        }
      }
      i++;
    }
  }

  code << indent();
  node->block->accept(this);

  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);

  indent_level--;
  indentedln("}");

  indent_level--;
  indentedln("}");

  defer_blocks.pop_back();
  ctx.set_scope(old_scope);
  return;
}

void Emitter::visit(ASTArguments *node) {
  code << "(";
  emit_arguments_no_parens(node);
  code << ")";
  return;
}

void Emitter::visit(ASTType_Of *node) {
  auto id = node->target->resolved_type;
  if (!type_is_valid(id)) throw_error("Invalid type in typeof() node", node->source_range);
  auto type = id;
  code << to_reflection_type_struct(type, ctx);
}

void Emitter::visit(ASTType *node) {
  auto type = node->resolved_type;
  if (!type) {
    throw_error("internal compiler error: ASTType* resolved to null in emitter.", node->source_range);
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    code << get_function_pointer_type_string(type);
    return;
  }

  if (type->is_kind(TYPE_ENUM)) {
    auto enum_info = (type->info->as<EnumTypeInfo>());
    type = global_find_type_id(enum_info->underlying_type, type->extensions);
  }

  auto type_string = type_to_string(type);

  code << type_string;
  return;
}

void Emitter::emit_arguments_with_defaults(ASTExpr *callee, ASTArguments *arguments, std::vector<Type *> generic_args) {
  auto symbol = ctx.get_symbol(callee);

  if (symbol && symbol.get()->is_function && symbol.get()->function.declaration) {
    const auto sym = symbol.get();

    ASTDeclaration *declaration = sym->function.declaration;
    if (generic_args.size() || declaration->generic_parameters.size()) {
      declaration = find_generic_instance(declaration->generic_instantiations, generic_args);
    }
    ASTFunctionDeclaration *function_declaration = (ASTFunctionDeclaration *)declaration;

    const auto params = function_declaration->params;
    const auto args_ct = arguments->arguments.size();
    const auto params_ct = params->params.size();
    const auto is_varargs = params->is_varargs;

    bool first = true;

    size_t param_start_index = params->has_self;
    size_t arg_index = 0;

    for (size_t param_index = param_start_index; param_index < params_ct; ++param_index) {
      const auto &param = params->params[param_index];

      if (!first) code << ",";

      first = false;
      if (arg_index < args_ct) {
        arguments->arguments[arg_index++]->accept(this);
      } else if (param->normal.default_value) {
        auto old_scope = ctx.scope;
        ctx.scope = function_declaration->scope;
        param->normal.default_value.get()->accept(this);
        ctx.scope = old_scope;
      }
    }

    if (is_varargs && args_ct > params_ct) {
      for (size_t arg_index = params_ct; arg_index < args_ct; ++arg_index) {
        if (!first) code << ",";

        first = false;
        arguments->arguments[arg_index]->accept(this);
      }
    }
    return;
  }

  emit_arguments_no_parens(arguments);
}

void Emitter::visit(ASTCall *node) {
  std::vector<Type *> generic_args;
  if (node->has_generics()) {
    generic_args = typer.get_generic_arg_types(*node->get_generic_arguments().get());
  }

  auto resolved_func_type = node->callee->resolved_type;

  if (node->callee->get_node_type() == AST_NODE_PATH && resolved_func_type &&
      resolved_func_type->is_kind(TYPE_CHOICE)) {
    // Creating a choice type's tuple-variant, such as Option!<T>::Some(10), etc.
    emit_choice_tuple_variant_instantiation((ASTPath *)node->callee, node->arguments);
  } else {
    // normal function call, or a static method.
    node->callee->accept(this);
    code << mangled_type_args(generic_args);
    code << "(";
    emit_arguments_with_defaults(node->callee, node->arguments, generic_args);
    code << ")";
  }
}

void Emitter::visit(ASTLiteral *node) {
  auto type = type_to_string(node->resolved_type);
  std::string output;
  switch (node->tag) {
    case ASTLiteral::Null:
      code << "NULL";
      return;
    case ASTLiteral::String: {
      if (node->is_c_string) {
        output = std::format("\"{}\"", node->value.get_str());
      } else {
        auto str = node->value.get_str();
        code << std::format("(str) {{ .data = \"{}\", .length = {} }}", str, calculate_actual_length(str));
        return;
      }
    } break;
    case ASTLiteral::MultiLineString: {
      auto str = node->value.get_str();
      size_t length = 0;
      std::stringstream ss;
      ss << "(str) { .data = ";
    
      size_t start = 0;
      while (start < str.size()) {
        size_t end = str.find('\n', start);
        if (end == std::string::npos) end = str.size();
        std::string line = str.substr(start, end - start);
    
        ss << "\"";
        // Emit the actual line content, escaping quotes and backslashes
        for (char c : line) {
          if (c == '\\' || c == '"') ss << '\\';
          ss << c;
        }
        length += line.size();
    
        if (end < str.size()) {
          ss << "\\n\"";
          length += 1; // for the newline
        } else {
          ss << "\"";
        }
    
        start = end + 1;
      }
    
      ss << ", .length = " << length << " }";
      code << ss.str();
      return;
    } break;
    case ASTLiteral::Float:
      if (node->resolved_type != f64_type()) {
        output = node->value.get_str() + "f";
      } else {
        output = node->value.get_str();
      }
      break;
    case ASTLiteral::Char:
      output = node->value.get_str();
      break;
    case ASTLiteral::Integer:
      output = node->value.get_str();
      break;
    case ASTLiteral::Bool:
      output = node->value.get_str();
      break;
  }
  code << output;
  return;
}

void Emitter::visit(ASTUnaryExpr *node) {
  if (node->op == TType::Sub) {
    auto type = type_to_string(node->operand->resolved_type);
    code << '(' << type << ')';
  }

  auto left_type = node->operand->resolved_type;
  auto left_ty = left_type;

  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, OPERATION_UNARY, node->op, node->operand, nullptr);
    return;
  }

  // we always do these as postfix unary since if we don't it's kinda undefined
  // behaviour and it messes up unary expressions at the end of dot expressions
  if (node->op == TType::Increment || node->op == TType::Decrement) {
    node->operand->accept(this);
    code << ttype_get_operator_string(node->op, node->source_range);
  } else {
    code << '(';
    code << ttype_get_operator_string(node->op, node->source_range);
    node->operand->accept(this);
    code << ")";
  }
  return;
}

void Emitter::visit(ASTBinExpr *node) {
  auto left_ty = node->left->resolved_type;
  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, OPERATION_BINARY, node->op, node->left, node->right);
    return;
  }

  code << "(";
  node->left->accept(this);
  space();
  code << ttype_get_operator_string(node->op, node->source_range);
  if (node->op == TType::Assign) {
    auto type = node->resolved_type;
    if (type->is_kind(TYPE_CHOICE) && node->right->get_node_type() == AST_NODE_PATH) {
      auto path = (ASTPath *)node->right;
      if (path->length() > 1) {
        emit_choice_marker_variant_instantiation(type, path);
        code << ")";
        return;
      }
    }
  }
  space();
  node->right->accept(this);
  code << ")";
}

void Emitter::visit(ASTExprStatement *node) {
  emit_line_directive(node);
  code << indent();
  node->expression->accept(this);
  code << ";\n";
  return;
}

void Emitter::visit(ASTVariable *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }

  emit_line_directive(node);

  auto variable = ctx.scope->lookup(node->name);

  if (!variable) {
    throw_error(std::format("INTERNAL_COMPILER_ERROR: variable '{}' somehow wasn't able to be found while emitting it's declaration", node->name), node->source_range);
    return;
  }

  auto name = emit_symbol(variable);

  // We exclude initializer lists, and array types. this is because arrays are not assignable in C, and init lists have
  // temporary memory that would go out of scope after the global ini function
  const bool is_init_list = node->type->resolved_type->basename.str_ptr->starts_with("InitList");
  const bool is_array = node->type->resolved_type->is_fixed_sized_array();

  // TODO!: @Cooper-Pilot I don't know how to make this work but we need to just mark this as global so we can get
  // global static initializers. We have to somehow tell the dependency emitter to forward declare it as extern at the
  // correct moment.
  const bool is_global_variable =
      !node->is_local && !node->is_constexpr && !is_init_list && !is_array; /*  || node->is_static; */

  // if (node->is_static) {
  //   node->type->accept(dep_emitter);
  //   reflection_externs << "extern " << type_to_string(node->type->resolved_type) << " " << name << ";\n";
  // }

  if (node->type->resolved_type == Type::INVALID_TYPE) {
    throw_error("internal compiler error: type was null upon emitting an ASTDeclaration", node->source_range);
  }
  auto type = node->type->resolved_type;
  auto handle_initialization = [&]() {
    if (node->value.is_not_null() && emit_default_value) {
      code << " = ";
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      auto type = node->type->resolved_type;
      code << " = ";
      if (type->is_kind(TYPE_STRUCT)) {
        emit_default_construction(type);
      } else {
        code << "(" << type_to_string(type) << ")" << "{0}";
      }
    }
    code << ";\n";
  };

  auto old = emit_default_init;
  Defer _([&] { emit_default_init = old; });
  if (node->is_extern) {
    code << "extern ";
    emit_default_init = false;
  }
  if (node->is_static) {
    code << "static ";
  }
  if (node->is_constexpr) {
    code << "const static ";
  }

  // This will work fine if global
  if (type->is_kind(TYPE_FUNCTION)) {
    code << get_declaration_type_signature_and_identifier(name, type);
    handle_initialization();
    return;
  }

  // this obviously cannot be a global variable; bitfields only ever appear in structs & unions.
  if (node->is_bitfield) {
    node->type->accept(this);
    space();
    code << name;
    space();
    code << ": " << node->bitsize.get_str();
    handle_initialization();
    return;
  }

  if (is_global_variable && !node->is_extern) {
    // Emit the 'forward declaration' or whatever.

    if (type->is_fixed_sized_array()) {
      code << get_declaration_type_signature_and_identifier(name, type);
    } else {
      node->type->accept(this);
    }
    space();
    code << name;
    semicolon();

    auto old_code = std::move(code);
    code = std::move(global_initializer_builder);
    Defer _([&] {
      global_initializer_builder = std::move(code);
      code = std::move(old_code);
      code << "\n";
    });
    // Now, we emit the initialization logic into the global ini builder.
    code << name;

    if (type->is_fixed_sized_array()) {
      if (node->value.is_not_null()) {
        code << " = ";
        node->value.get()->accept(this);
      } else if (emit_default_init) {
        code << " = {0}";  // I think this is wholly unneccesary. I think a global, non-initialized array will be zeroed
                           // in modern C.
      }
      code << ";\n";
      return;
    }

    // TODO: it's not ideal to have this special case all over the place.
    if (type->is_kind(TYPE_CHOICE) && node->value.is_not_null() &&
        node->value.get()->get_node_type() == AST_NODE_PATH) {
      auto value = (ASTPath *)node->value.get();
      code << " = ";
      emit_choice_marker_variant_instantiation(type, value);
      code << ";\n";
    } else {
      handle_initialization();
    }
  } else {
    if (type->is_fixed_sized_array()) {
      code << get_declaration_type_signature_and_identifier(name, type);
      if (node->value.is_not_null()) {
        code << " = ";
        node->value.get()->accept(this);
      } else if (emit_default_init) {
        code << " = {0}";
      }
      code << ";\n";
      return;
    }

    node->type->accept(this);
    space();
    code << name;
    space();

    // TODO: it's not ideal to have this special case all over the place.
    if (type->is_kind(TYPE_CHOICE) && node->value.is_not_null() &&
        node->value.get()->get_node_type() == AST_NODE_PATH) {
      auto value = (ASTPath *)node->value.get();
      code << " = ";
      emit_choice_marker_variant_instantiation(type, value);
      code << ";\n";
    } else {
      handle_initialization();
    }
  }

  return;
}

void Emitter::emit_forward_declaration(ASTFunctionDeclaration *node) {
  if (node->name == "main" || node->is_entry) {
    return;
  }
  if (!node->generic_parameters.empty()) {
    return;
  }

  Scope *scope;
  if (node->declaring_type != Type::INVALID_TYPE) {
    scope = node->declaring_type->info->scope;
  } else if (node->scope) {
    scope = node->scope;
  } else {
    scope = ctx.scope;
  }

  auto symbol = scope->lookup(node->name);

  if (!symbol) {
    // ! TODO: fix this.
    // right now when we search for a symbol from in a module this just crashes.
    return;
  }

  auto decl = symbol->function.declaration;
  if (decl->is_declared || decl->is_emitted) {
    return;
  } else {
    decl->is_declared = true;
  }

  if (node->is_exported) {
    code << "extern ";
  }

  auto name = emit_symbol(symbol) + mangled_type_args(node->generic_arguments);
  auto returns = node->return_type->resolved_type;

  if (returns->is_kind(TYPE_FUNCTION)) {
    auto return_function_type = static_cast<FunctionTypeInfo *>(returns->info);

    // we take fixed array extensions as pointer here because it's invalid and would get casted off anyway.
    auto depth = returns->extensions.size();
    auto extensions = std::string(depth, '*');

    code << type_to_string(return_function_type->return_type) << "(" << extensions << name;
    node->params->accept(this);
    code << ")";
  } else {
    node->return_type->accept(this);
    code << " " + name;
    node->params->accept(this);
  }

  code << ";\n";
}

void Emitter::emit_extern_function(ASTFunctionDeclaration *node) {
  if (node->name == "main" || node->is_entry) {
    throw_error("entry point function cannot be extern", node->source_range);
  }

  auto emit_params = [&] {
    code << '(';
    for (const auto &param : node->params->params) {
      code << type_to_string(param->resolved_type);
      if (param != node->params->params.back()) {
        code << ", ";
      }
    }
    if (node->is_varargs) {
      code << ", ...);";
    } else {
      code << ")";
    }
  };

  code << "extern ";
  auto returns = node->return_type->resolved_type;
  if (returns->is_kind(TYPE_FUNCTION)) {
    auto return_function_type = static_cast<FunctionTypeInfo *>(returns->info);
    code << type_to_string(return_function_type->return_type) << "(*" << node->name.get_str();
    emit_params();
    code << ")";
  } else {
    code << type_to_string(returns);
    space();
    code << " " + node->name.get_str();
    emit_params();
  }

  semicolon();
  newline();
}

void Emitter::visit(ASTStructDeclaration *node) {
  if (node->is_emitted) {
    return;
  }

  if (!node->generic_parameters.empty()) {
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

  const Type *type = node->resolved_type;
  const StructTypeInfo *info = (type->info->as<StructTypeInfo>());

  const std::string type_tag = (node->is_union ? "typedef union" : "typedef struct");
  const std::string name = info->scope->full_name();

  if (info->is_forward_declared || node->is_forward_declared) {
    // We don't care about extern here.
    code << indent() << type_tag << " " << name << " " << name << ";\n";
    return;
  }

  ENTER_SCOPE(info->scope);
  if (info->is_anonymous) {
    code << indent() << (node->is_union ? "union" : "struct") << " {\n";
  } else {
    code << type_tag << " " << name << " {\n";
  }

  indent_level++;
  bool old = emit_default_init;
  emit_default_init = false;
  Defer _defer1([&] { emit_default_init = old; });

  for (const auto &subtype : node->subtypes) {
    subtype->accept(this);
  }
  for (const auto &member : node->members) {
    indented("");
    auto type = member.type->resolved_type;

    // Erase zero sized type members for generic instantiation to get rid of unneccesary padding.
    // if (type == unit_type()) {
    //   continue;
    // }

    if (type->is_kind(TYPE_FUNCTION)) {
      auto name = member.name.get_str();
      auto name_nullable = Nullable(&name);
      code << get_function_pointer_type_string(type, name_nullable);
    } else if (type->is_fixed_sized_array()) {
      code << get_declaration_type_signature_and_identifier(member.name.get_str(), type);
    } else {
      member.type->accept(this);
      space();
      code << member.name.get_str();
    }
    semicolon();
    newline();
  }
  indent_level--;

  // this is for anonymous substructs which just unfold at C compile time into the struct's namespace.
  if (info->is_anonymous) {
    code << indent() << "};\n";
  } else {
    code << indent() << "} " << name << ";\n";
  }
}

void Emitter::visit(ASTEnumDeclaration *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  size_t n = 0;
  code << "typedef enum: " << type_to_string(node->underlying_type) << " {\n";
  indent_level++;
  auto scope = node->resolved_type->info->scope;
  for (const auto &[key, value] : node->key_values) {
    code << indent() << emit_symbol(scope->lookup(key));
    if (node->is_flags) {
      code << " = ";
      code << std::to_string(1 << n);
    } else if (value) {
      code << " = ";
      value->accept(this);
    }
    if (n != node->key_values.size() - 1) {
      code << ",";
    }
    newline();
    n++;
  }
  indent_level--;
  code << indent() << "} " << scope->full_name() << ";\n";
}

void Emitter::visit(ASTParamDecl *node) {
  auto type = node->resolved_type;

  if (node->tag == ASTParamDecl::Normal) {
    if (type->is_kind(TYPE_FUNCTION) || type->is_fixed_sized_array()) {
      code << get_declaration_type_signature_and_identifier(node->normal.name.get_str(), type);
    } else {
      node->normal.type->accept(this);
      code << ' ' << node->normal.name.get_str();
    }
  } else if (node->tag == ASTParamDecl::Self) {
    code << type_to_string(type) << " self";
  }

  return;
}

void Emitter::visit(ASTParamsDecl *node) {
  code << "(";
  size_t i = 0;
  for (const auto &param : node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      code << ", ";
    }
    ++i;
  }

  if (node->is_varargs) {
    code << ", ...)";
  } else {
    code << ")";
  }
}

void Emitter::visit(ASTProgram *) {
  static const auto testing = compile_command.has_flag("test");
  ctx.set_scope(ctx.root_scope);

  code << TESTING_BOILERPLATE;

  if (testing) {
    auto test_init = test_functions.str();
    if (test_init.ends_with(',')) {
      test_init.pop_back();
    }
    // deploy the array of test struct wrappers.
    code << std::format("$ela_test tests[{}] = {}\n", num_tests, "{ " + test_init + " };");
    // use the test runner main macro.
  }

  if ((has_user_defined_main || testing) && !is_freestanding && !compile_command.has_flag("nostdlib")) {
    auto env_scope = ctx.scope->find_type_id("Env", {})->info->scope;
    code << "void ela_run_global_initializers() {\n";
    const auto global_init = global_initializer_builder.str();
    code << global_init;
    code << "}\n";
    code << std::format(MAIN_FMT, emit_symbol(env_scope->lookup("initialize")));
  }

  // TODO: if we're freestanding, we should just emit ID's only for typeof().
  if (is_freestanding && reflected_upon_types.size()) {
    throw_error(
        "You cannot use runtime type reflection in a freestanding or "
        "nostdlib environment, due to a lack of a `core` library. To be changed in the future",
        {});
  }
}

void Emitter::visit(ASTDotExpr *node) {
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = base_ty_id;
  auto op = ".";

  if (base_ty->is_pointer()) {
    op = "->";
  }

  node->base->accept(this);
  code << op;
  if (base_ty->is_kind(TYPE_TUPLE)) {
    code << "$";
  }

  /*
    ! TODO: mangle generics here?
  */
  code << node->member.identifier.get_str();
  return;
}

void Emitter::visit(ASTIndex *node) {
  auto left_ty = node->base->resolved_type;
  if (left_ty && node->is_operator_overload) {
    code << "(*";  // always dereference via index. for `type[10] = 10` and such.
    call_operator_overload(node->source_range, OPERATION_INDEX, TType::LBrace, node->base, node->index);
    code << ")";

    return;
  }

  node->base->accept(this);
  code << '[';
  node->index->accept(this);
  code << ']';
  return;
}

void Emitter::visit(ASTInitializerList *node) {
  auto type = node->resolved_type;

  if (!type->is_fixed_sized_array()) {
    code << "(" + type_to_string(type) + ")";
  }

  if (node->target_type.is_not_null() && node->target_type.get()->normal.path->get_node_type() == AST_NODE_PATH &&
      type->is_kind(TYPE_CHOICE)) {
    emit_choice_struct_variant_instantation(node->target_type.get()->normal.path, node);
    return;
  }

  if (node->tag == ASTInitializerList::INIT_LIST_EMPTY) {
    emit_default_construction(type);
  } else if (node->tag == ASTInitializerList::INIT_LIST_NAMED) {
    emit_default_construction(node->resolved_type, node->key_values);
  } else if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
    code << "{";
    if (type->basename.get_str().starts_with("InitList$")) {
      auto element_type = type->generic_args[0];
      code << " .data = ";
      code << "(" << type_to_string(element_type) << "[]) {";
      for (const auto &expr : node->values) {
        expr->accept(this);
        if (expr != node->values.back()) {
          code << ", ";
        }
      }
      code << "}, .length = " << std::to_string(node->values.size());
    } else {
      for (const auto &expr : node->values) {
        expr->accept(this);
        if (expr != node->values.back()) {
          code << ", ";
        }
      }
    }
    code << "}";
  }
}

void Emitter::visit(ASTRange *node) {
  code << "(" << type_to_string(node->resolved_type) << ") {";
  code << ".begin = ";
  node->left->accept(this);
  code << ", .end = ";
  node->right->accept(this);
  code << "}";
  return;
}

void Emitter::visit(ASTTuple *node) {
  auto type = node->resolved_type;
  auto name = "(" + type_to_string(type) + ")";
  code << name << " {";
  for (size_t i = 0; i < node->values.size(); ++i) {
    auto &value = node->values[i];
    code << ".$" << std::to_string(i) << " = ";
    value->accept(this);
    if (i != node->values.size() - 1) code << ", ";
  }
  code << "}";
  return;
}

// This needs serious work. It's crap.
void Emitter::visit(ASTDestructure *node) {
  emit_line_directive(node);
  auto type = node->resolved_type;

  size_t index = 0;
  static int temp_idx = 0;

  std::string identifier = "$deconstruction$" + std::to_string(temp_idx++);
  // declare a temporary variable referring to the right, so we can avoid re-evaluating the expression if it's a literal
  // or function call. this probably needs work.
  {
    code << type_to_string(type) << " " << identifier << " = ";
    node->right->accept(this);
    code << ";\n";
  }

  emit_line_directive(node);

  for (auto [name, sym_type, _, __] : type->info->members) {
    if (index > node->elements.size()) break;

    auto semantic = node->elements[index].semantic;

    if (node->op == TType::ColonEquals) {
      code << "auto " << node->elements[index++].identifier.get_str() << " = ";
      if (is_pointer_semantic(semantic)) {
        code << "&";
      }
      code << identifier << "." << name.get_str() << ";\n";
    } else {
      code << node->elements[index++].identifier.get_str() << " = ";
      if (is_pointer_semantic(semantic)) {
        code << "&";
      }
      code << identifier << "." << name.get_str() << ";\n";
    }
  }

  emit_line_directive(node);
}

// This should be preferred in almost every case it can be; It properly emits function pointers, arrays etc.
std::string Emitter::get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;

  std::string sym_name;

  if (ctx.scope->local_lookup(name)) {
    sym_name = emit_symbol(ctx.scope->lookup(name));
  } else {
    sym_name = name;
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = sym_name;
    return get_function_pointer_type_string(type, &identifier);
  }

  std::string base_type_str = type_to_string(type->base_type == Type::INVALID_TYPE ? type : type->base_type);
  std::string identifier = sym_name;

  for (const auto &ext : type->extensions) {
    if (ext.type == TYPE_EXT_POINTER_MUT || ext.type == TYPE_EXT_POINTER_CONST) {
      base_type_str += "*";
    } else if (ext.type == TYPE_EXT_ARRAY) {
      identifier += "[" + std::to_string(ext.array_size) + "]";
    }
  }

  tss << base_type_str << " " << identifier;
  return tss.str();
}

// Identifier may contain a fixed buffer size like name[30] due to the way
// function pointers have to work in C.
// I wish this oculd be simpler.
std::string Emitter::get_function_pointer_type_string(Type *type, Nullable<std::string> identifier,
                                                      bool type_erase_self) {
  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(
        "internal compiler error: tried to get a function pointer from "
        "a non-function type",
        {});
  }

  std::stringstream ss;

  int pointer_depth = 0;
  TypeExtensions other_extensions;
  for (auto ext : type->extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT)
      pointer_depth++;
    else
      other_extensions.push_back(ext);
  }

  auto type_prefix = std::string(pointer_depth, '*');
  auto type_postfix = extensions_to_string(other_extensions);

  auto info = (type->info->as<FunctionTypeInfo>());
  auto return_type = info->return_type;

  ss << type_to_string(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

  ss << type_postfix;

  ss << ")(";

  for (size_t i = 0; i < info->params_len; ++i) {
    if (i == 0 && type_erase_self) {
      ss << "void*";
    } else {
      auto type = info->parameter_types[i];
      ss << type_to_string(type);
    }

    if (i != info->params_len - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

bool Emitter::should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->is_test) {
    return false;
  }

  auto sym = ctx.scope->lookup(node->name);
  if (node->declaring_type != Type::INVALID_TYPE) {
    sym = node->declaring_type->info->scope->local_lookup(node->name);
  }

  // !! For some reason compiling freestanding this just becomes a big problem for extern functions?
  if (!sym) {
    // !! throw_error(std::format("internal compiler error: should_emit_function got a null symbol? function={}",
    // node->name), node->source_range);
    return true;
  }

  auto sym_name = emit_symbol(sym);

  // generate a test based on this function pointer.
  if (test_flag && node->is_test) {
    visitor->test_functions << "($ela_test){.name = \"" << sym_name << "\", .function = &" << sym_name << "},";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && (node->is_entry || node->name == "main")) {
    return false;
  }
  return true;
}

std::string Emitter::type_to_string_with_extensions(const TypeExtensions &extensions, const std::string &base) {
  std::stringstream ss;
  ss << base;
  for (const auto ext : extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(ext.array_size) << "]";
    } else if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string Emitter::type_to_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_DYN: {
      auto info = type->info->as<DynTypeInfo>();
      output = "dyn$" + type_to_string(info->trait_type);
      output = type_to_string_with_extensions(type->extensions, output);
    } break;
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TRAIT:
    case TYPE_CHOICE: {
      output = type_to_string_with_extensions(type->extensions, type->info->scope->full_name());
      break;
    }
    case TYPE_TUPLE: {
      auto info = (type->info->as<TupleTypeInfo>());
      output = "$tuple";
      for (size_t i = 0; i < info->types.size(); ++i) {
        output += std::to_string(info->types[i]->uid);
        if (i != info->types.size() - 1) {
          output += "$";
        }
      }
      output = type_to_string_with_extensions(type->extensions, output);
      break;
    }
  }
  return output;
}

void Emitter::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    return;
  }

  auto target = node->target->resolved_type;

  if (!target) {
    throw_error("internal compiler error: impl target type was null in the emitter", node->source_range);
  }
  auto old_type = type_context;
  type_context = node->target;
  Defer _([&] { type_context = old_type; });

  for (const auto &constant : node->constants) {
    constant->accept(this);
  }

  for (const auto &method : node->methods) {
    method->accept(this);
  }

  return;
}

void Emitter::visit(ASTCast *node) {
  auto type_string = type_to_string(node->target_type->resolved_type);
  code << "(" << type_string << ")";
  node->expression->accept(this);
  return;
}

void Emitter::visit(ASTChoiceDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    return;
  }

  if (node->is_emitted) {
    return;
  }

  node->is_emitted = true;

  emit_line_directive(node);

  auto old_scope = ctx.scope;
  Defer _defer([&] { ctx.scope = old_scope; });

  auto type = node->resolved_type;
  auto info = type->info;
  ctx.scope = info->scope;

  auto name = node->scope->full_name();
  code << "typedef struct " << name << " " << name << ";\n";

  auto old_init = emit_default_init;
  emit_default_init = false;

  for (const auto &variant : node->variants) {
    auto variant_name = variant.name.get_str();
    if (variant.kind == ASTChoiceVariant::STRUCT) {
      auto subtype_name = name + "$" + variant_name;
      code << "typedef struct " << subtype_name << " {\n";
      for (const auto &field : variant.struct_declarations) {
        code << get_declaration_type_signature_and_identifier(field->name.get_str(), field->resolved_type) << ";\n";
        // code << to_cpp_string(field->resolved_type) << " " << field->name.get_str() << ";\n";
      }
      code << "} " << subtype_name << ";\n";
    } else if (variant.kind == ASTChoiceVariant::TUPLE) {
      auto subtype_name = name + "$" + variant_name;
      code << "typedef ";
      variant.tuple->accept(this);
      code << " " << subtype_name << ";\n";
    }
  }

  emit_default_init = old_init;
  code << "typedef struct " << name << " {\n";
  code << "  int index;\n";  // TODO: we can compress this by the max value of the discriminant.
  code << "  union {\n";
  for (const auto &variant : node->variants) {
    auto variant_name = variant.name.get_str();
    auto subtype_name = name + "$" + variant_name;
    if (variant.kind == ASTChoiceVariant::STRUCT) {
      code << "    " << subtype_name << " " << variant_name << ";\n";
    } else if (variant.kind == ASTChoiceVariant::TUPLE) {
      code << "    " << subtype_name << " " << variant_name << ";\n";
    }
  }
  code << "  };\n";
  code << "} " << name << ";\n";
}

// Helper function to emit deferred statements
void Emitter::emit_deferred_statements(DeferBlockType type) {
  auto defer_block = defer_blocks.rbegin();
  while (defer_block->type != type) {
    if (defer_block == defer_blocks.rend()) {
      throw_error("internal compiler error: could not find defer block type in stack", {});
    }
    for (auto defer : defer_block->defers) {
      defer->statement->accept(this);
    }
    defer_block++;
  }
  if (defer_block == defer_blocks.rend()) {
    throw_error("internal compiler error: could not find defer block type in stack", {});
  }
  for (auto defer : defer_block->defers) {
    defer->statement->accept(this);
  }
}

void Emitter::emit_lambda(ASTLambda *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  emit_line_directive(node);
  node->return_type->accept(this);
  code << ' ' << node->unique_identifier.get_str() << ' ';
  node->params->accept(this);
  newline();
  node->block->accept(this);
  newline();
}

void Emitter::visit(ASTFunctionDeclaration *node) {
  auto emit_function_signature_and_body = [&](const std::string &name) {
    auto returns = node->return_type->resolved_type;

    if (returns->is_kind(TYPE_FUNCTION)) {
      auto return_function_type = static_cast<FunctionTypeInfo *>(returns->info);

      // we take fixed array extensions as pointer here because it's invalid and would get casted off anyway.
      auto depth = returns->extensions.size();
      auto extensions = std::string(depth, '*');

      code << type_to_string(return_function_type->return_type) << "(" << extensions << name;
      node->params->accept(this);
      code << ")";
    } else {
      node->return_type->accept(this);
      code << " " + name;
      node->params->accept(this);
    }
    defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_FUNC});
    if (node->block.is_not_null()) {
      code << " ";
      auto block = node->block.get();
      block->accept(this);
    }
    emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
    defer_blocks.pop_back();
  };

  auto emit_various_function_declarations = [&] {
    if (!node->generic_parameters.empty()) {
      return;
    }

    Symbol *sym;
    if (node->declaring_type != Type::INVALID_TYPE) {
      sym = node->declaring_type->info->scope->local_lookup(node->name);
    } else {
      sym = ctx.scope->lookup(node->name);
    }
    auto name = emit_symbol(sym) + mangled_type_args(node->generic_arguments);

    if (node->name != "main" && !node->is_entry) {
      if (node->is_forward_declared) {
        emit_forward_declaration(node);
        return;
      } else if (node->is_inline) {
        // We have to use static here otherwise the standalone inline function has
        // external linkage.
        // We should have a way to specify static linkage anyway.
        code << "static inline ";
      }
    }

    if (node->is_exported) {
      code << "extern  ";
    }

    if ((node->name == "main" || node->is_entry) && !is_freestanding && !compile_command.has_flag("nostdlib")) {
      has_user_defined_main = true;
      user_defined_entry_point = node->name;
      node->return_type->accept(this);
      code << " __ela_main_()\n";
      node->block.get()->accept(this);
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
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);
  Defer deferred = {[&]() { ctx.set_scope(old_scope); }};

  // this also happens to emit the test boilerplate that bootstraps it into the
  // test runner, if applicable.
  if (!should_emit_function(this, node, test_flag)) {
    return;
  }

  if (node->is_extern) {
    emit_extern_function(node);
    return;
  }

  emit_various_function_declarations();

  return;
}

void Emitter::visit(ASTReturn *node) {
  if (emitting_function_with_defer ||
      (node->declaring_block.is_not_null() && node->declaring_block.get()->defer_count != 0)) {
    if (node->expression.is_not_null()) {
      emit_line_directive(node);
      auto type = node->expression.get()->resolved_type;

      if (type->is_kind(TYPE_CHOICE) && node->expression.get()->get_node_type() == AST_NODE_PATH) {
        auto path = (ASTPath *)node->expression.get();
        emit_choice_marker_variant_instantiation(type, path);
      } else {
        code << indent() << type_to_string(type) << " " << defer_return_value_key << " = ";
        node->expression.get()->accept(this);
      }
      code << ";\n";
    }
    emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
    emit_line_directive(node);
    code << indent() << "return";
    if (node->expression.is_not_null()) {
      code << " " << defer_return_value_key;
    }
    code << ";\n";
  } else if (cf_expr_return_register.is_null() || node->expression.is_null()) {
    emit_line_directive(node);
    indented("return");
    if (node->expression.is_not_null()) {
      space();
      auto type = node->expression.get()->resolved_type;
      if (type->is_kind(TYPE_CHOICE) && node->expression.get()->get_node_type() == AST_NODE_PATH) {
        auto path = (ASTPath *)node->expression.get();
        emit_choice_marker_variant_instantiation(type, path);
      } else {
        node->expression.get()->accept(this);
      }
    }
    code << ";\n";
  } else {
    emit_line_directive(node);
    code << *cf_expr_return_register.get() << " = ";
    auto type = node->expression.get()->resolved_type;
    if (type->is_kind(TYPE_CHOICE) && node->expression.get()->get_node_type() == AST_NODE_PATH) {
      auto path = (ASTPath *)node->expression.get();
      emit_choice_marker_variant_instantiation(type, path);
    } else {
      node->expression.get()->accept(this);
    }
    code << ";\n";
  }
  return;
}

void Emitter::visit(ASTBreak *node) {
  emit_line_directive(node);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  indented("break;\n");
}

void Emitter::visit(ASTContinue *node) {
  emit_line_directive(node);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  indented("continue;\n");
}

void Emitter::visit(ASTDefer *node) { defer_blocks.back().defers.push_back(node); }

void Emitter::visit(ASTBlock *node) {
  code << "{\n";
  indent_level++;
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  defer_blocks.emplace_back();

  for (const auto &statement : node->statements) {
    if (statement->get_node_type() == AST_NODE_VARIABLE) {
      indented("");
    }
    statement->accept(this);
  }

  emit_deferred_statements(DEFER_BLOCK_TYPE_OTHER);
  defer_blocks.pop_back();

  indent_level--;
  indentedln("}");
  ctx.scope = old_scope;
  return;
}

void Emitter::visit(ASTLambda *node) { code << node->unique_identifier.get_str(); }

void Emitter::emit_tuple(Type *type) {
  if (type->base_type != Type::INVALID_TYPE) {
    type = type->base_type;
  }
  if (type->tuple_is_emitted) {
    return;
  } else {
    type->tuple_is_emitted = true;
  }
  auto name = type_to_string(type);

  code << "typedef struct " << name << " {\n";
  indent_level++;
  auto info = type->info->as<TupleTypeInfo>();
  for (size_t i = 0; i < info->types.size(); ++i) {
    auto type = info->types[i];
    if (type->is_kind(TYPE_FUNCTION)) {
      auto name = "$" + std::to_string(i);
      code << indent() << get_function_pointer_type_string(type, &name, false) << ";\n";
    } else {
      code << indent() << type_to_string(type) << " $" << std::to_string(i) << ";\n";
    }
  }
  indent_level--;
  code << "} " << name << ";\n";
}

void Emitter::visit(ASTSize_Of *node) {
  code << "sizeof(";
  node->target_type->accept(this);
  code << ")";
}

void Emitter::call_operator_overload(const SourceRange &range, OperationKind operation, TType op, ASTExpr *left,
                                     ASTExpr *right) {
  auto call = ASTMethodCall{};
  auto dot = ASTDotExpr{};
  dot.base = left;
  dot.member = ASTPath::Segment{get_operator_overload_name(op, operation)};
  call.callee = &dot;
  auto args = ASTArguments{};
  if (right) {
    args.arguments = {right};
  }
  call.arguments = &args;
  dot.source_range = range;
  call.arguments->source_range = range;
  call.source_range = range;
  call.accept(&typer);
  if (dot.member.identifier == "deref") {
    code << "(*";
  } else {
    code << "(";
  }
  call.accept(this);
  code << ")";
}

std::string Emitter::emit_symbol(Symbol *symbol) {

  if (symbol == nullptr) {
    throw_error("Symbol was nullptr at emit time", {});
    return {};
  }

  if (symbol->is_local ||
      (symbol->is_function && symbol->function.declaration && symbol->function.declaration->is_extern)) {
    return symbol->name.get_str();
  }

  auto full_name = symbol->scope->full_name();
  if (!full_name.empty()) {
    full_name += "$";
  }
  full_name += symbol->name.get_str();
  return full_name;
};

void Emitter::visit(ASTDyn_Of *node) {
  if (node->is_emitted) {
    return;
  }

  code << "(" << type_to_string(node->resolved_type) << ") {\n";
  indent();
  code << ".instance = (void*)";
  node->object->accept(this);
  code << ",\n";

  // ugliest code ever freaking written.
  auto dyn_info = node->resolved_type->info->as<DynTypeInfo>();

  auto object_scope_bare = node->object->resolved_type->get_element_type();
  auto object_scope = object_scope_bare->info->scope;

  for (auto &[name, method_type] : dyn_info->methods) {
    indent();
    code << "." << name.get_str() << " = ";
    auto symbol = object_scope->local_lookup(name);
    auto typestring = get_function_pointer_type_string(method_type, nullptr, true);
    code << "(" << typestring << ")" << emit_symbol(symbol) << ",\n";
  }

  code << "}";
}

void Emitter::emit_dyn_dispatch_object(Type *trait, Type *dyn_type) {
  if (trait->dyn_emitted) {
    return;
  }

  trait->dyn_emitted = true;

  auto dyn_ty = dyn_type;
  auto methods_to_emit = dyn_ty->info->as<DynTypeInfo>()->methods;

  for (auto [name, function_type] : methods_to_emit) {
    this->dep_emitter->define_type(function_type);
  }

  auto name = type_to_string(dyn_ty);
  code << "typedef struct " << name << "{\n";
  code << "void *instance;\n";
  for (auto [name, type] : methods_to_emit) {
    std::string method_pointer_name = name.get_str();
    code << get_function_pointer_type_string(type, &method_pointer_name) << ";";
    newline_indented();
  }
  code << "} " << name << ";";
  newline_indented();
}

void Emitter::visit(ASTMethodCall *node) {
  std::vector<Type *> generic_args = node->callee->member.get_resolved_generics();

  auto symbol = ctx.get_symbol(node->callee).get();
  // Call a function pointer via a dot expression
  if (symbol->is_variable) {
    {
      // Implicitly pass the 'dyn.instance' when calling the function pointers
      // that the dyn thingy sets up.
      auto object = node->callee->base;
      auto obj_type = object->resolved_type;

      if (obj_type->is_kind(TYPE_DYN)) {
        auto &args = node->arguments->arguments;
        auto dot = ast_alloc<ASTDotExpr>();
        dot->base = object;
        dot->member = ASTPath::Segment{"instance"};
        dot->resolved_type = global_find_type_id(void_type(), {{TYPE_EXT_POINTER_MUT}});
        args.insert(args.begin(), dot);
      }
    }

    auto func = node->callee;
    func->accept(this);
    code << mangled_type_args(generic_args);
    node->arguments->accept(this);
    return;
  }

  auto func = symbol->function.declaration;

  Type *function_type = symbol->resolved_type;
  // if generic function
  if (!func->generic_parameters.empty()) {
    func = (ASTFunctionDeclaration *)find_generic_instance(func->generic_instantiations, generic_args);
    function_type = func->resolved_type;
  }

  code << emit_symbol(symbol) + mangled_type_args(generic_args);

  auto self_param_ty = function_type->info->as<FunctionTypeInfo>()->parameter_types[0];

  auto base_type = node->callee->base->resolved_type;

  code << "(";

  // Emit the self arg.
  {
    if (self_param_ty->is_pointer() && !base_type->is_pointer()) {
      auto base_node_ty = node->callee->base->get_node_type();

      // TODO: It would be preferable to use a compound literal here, but we'd have to get all the fields from structs
      // so I don't think we can. I don't know what kind of memory implications this might have, it may be messed up
      // idk.
      if (base_node_ty == AST_NODE_METHOD_CALL || base_node_ty == AST_NODE_CALL || base_node_ty == AST_NODE_LITERAL) {
        code << "({ static " << type_to_string(base_type) << " __temp; __temp = ";
        node->callee->base->accept(this);
        code << "; &__temp; })";
      } else {
        code << "&";
        node->callee->base->accept(this);
      }

    } else if (!self_param_ty->is_pointer() && base_type->is_pointer()) {
      code << "*";
      node->callee->base->accept(this);
    } else {
      node->callee->base->accept(this);
    }

    bool has_default_params = false;
    for (const auto &param : symbol->function.declaration->params->params) {
      if (param->tag == ASTParamDecl::Normal && param->normal.default_value) {
        has_default_params = true;
        break;
      }
    }

    auto args_remaining = node->arguments->arguments.size();
    if (args_remaining != 0 || (args_remaining == 0 && has_default_params)) {
      code << ", ";
    }
  }

  emit_arguments_with_defaults(node->callee, node->arguments, generic_args);

  code << ")";
}

void Emitter::visit(ASTPath *node) {
  auto symbol = ctx.get_symbol(node);
  if (!symbol) {
    throw_error("internal compiler error: failed to emit path symbol.", node->source_range);
  }
  code << emit_symbol(symbol.get());
}

/*
  This probably should be limited to exclusively be done on 'if' statements, or anything that's followed by a block
  and has statement semantics. it needs to be scoped, and also that scope only should be reachable upon success.
*/
void Emitter::visit(ASTPatternMatch *node) {
  throw_error(
      "pattern matches cannot exist outside of 'if' statements currently. they will be expanded to 'while', "
      "'switch', and possibly other nodes in the future.",
      node->source_range);
}

void Emitter::emit_choice_marker_variant_instantiation(Type *type, ASTPath *value) {
  const auto last_segment = value->segments.back();
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto variant_type = info->get_variant_type(last_segment.identifier);
  if (!variant_type) {
    value->accept(this);
    return;
  }
  if (variant_type == void_type()) {
    const auto index = info->get_variant_discriminant(last_segment.identifier);
    const auto type_string = type_to_string(type);
    code << " (" << type_string << ") { .index = " << index << "}";
  }
}

void Emitter::emit_choice_tuple_variant_instantiation(ASTPath *path, ASTArguments *arguments) {
  const auto choice_type = path->resolved_type;
  const auto info = choice_type->info->as<ChoiceTypeInfo>();
  const auto last_segment = path->segments.back();
  const auto variant_name = last_segment.identifier;
  const auto variant_type = info->get_variant_type(variant_name);
  const auto type_string = type_to_string(choice_type);

  code << "(" << type_string << ") {\n";
  code << ".index = " << std::to_string(info->get_variant_discriminant(variant_name)) << ",\n";
  ASTTuple tuple;

  code << "." << variant_name.get_str() << " = ";

  tuple.resolved_type = variant_type;
  tuple.values = arguments->arguments;
  tuple.accept(this);

  code << "}";
}

void Emitter::emit_choice_struct_variant_instantation(ASTPath *path, ASTInitializerList *initializer) {
  const auto choice_type = path->resolved_type;
  const auto info = choice_type->info->as<ChoiceTypeInfo>();
  const auto last_segment = path->segments.back();
  const auto variant_name = last_segment.identifier;
  const auto type_string = type_to_string(choice_type);

  code << "(" << type_string << ") {\n";
  code << ".index = " << std::to_string(info->get_variant_discriminant(variant_name)) << ",\n";
  code << "." << variant_name.get_str() << " = " << "{";
  for (const auto &[name, value] : initializer->key_values) {
    code << "." << name.get_str() << " = ";
    value->accept(this);
    code << ", ";
  }
  code << "},";
  code << "}";
}

std::string Emitter::declare_temporary_for_pattern_match(bool is_pointer, Type *object_type, ASTPatternMatch *pattern) {
  static size_t idx = 0;
  const std::string patmatch_target = "$pat_match_target" + std::to_string(idx++);
  if (is_pointer) {
    code << type_to_string(object_type) << " " << patmatch_target << " = ";
    pattern->object->accept(this);
  } else {
    code << type_to_string(object_type->take_pointer_to(true)) << " " << patmatch_target << " = ";
    if (pattern->object->is_temporary_value()) {
      code << "({ static " << type_to_string(object_type) << " __temp; __temp = ";
      pattern->object->accept(this);
      code << "; &__temp; })";
    } else {
      code << "&";
      pattern->object->accept(this);
    }
  }
  code << ";\n";
  return patmatch_target;
}

void Emitter::emit_pattern_match_for_if(ASTIf *the_if, ASTPatternMatch *pattern) {
  auto old_scope = ctx.scope;
  ctx.set_scope(pattern->target_block->scope);
  Defer _([&] { ctx.scope = old_scope; });

  auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  auto variant_type = info->get_variant_type(segment.identifier);
  const auto variant_index = info->get_variant_discriminant(segment.identifier);

  const auto object_type = pattern->object->resolved_type;
  const auto is_pointer = object_type->is_pointer();

  static std::string the_register;
  if (the_if->is_expression) {
    the_register = "register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &the_register;
    code << "({\n";

    code << type_to_string(the_if->resolved_type) << " " << the_register;
    semicolon();
    newline();
  }

  const auto patmatch_target = declare_temporary_for_pattern_match(is_pointer, object_type, pattern);

  code << "if (";
  code << patmatch_target << "->";
  code << "index == " << variant_index << ") {\n";

  emit_pattern_match_destructure(patmatch_target, segment.identifier.get_str(), pattern, variant_type);
  the_if->block->accept(this);
  // exiting the if block.
  code << "}\n";

  if (the_if->_else.get()) {
    the_if->_else.get()->accept(this);
  }

  if (the_if->is_expression) {
    cf_expr_return_register = nullptr;
    code << the_register;
    code << ";\n})\n";
  }
}

void Emitter::emit_pattern_match_for_while(ASTWhile *the_while, ASTPatternMatch *pattern) {
  auto old_scope = ctx.scope;
  ctx.set_scope(pattern->target_block->scope);
  Defer _([&] { ctx.scope = old_scope; });

  auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  auto variant_type = info->get_variant_type(segment.identifier);
  const auto variant_index = info->get_variant_discriminant(segment.identifier);

  const auto object_type = pattern->object->resolved_type;
  const auto is_pointer = object_type->is_pointer();

  code << "while (true) {\n";
  const auto patmatch_target = declare_temporary_for_pattern_match(is_pointer, object_type, pattern);
  code << "if (";
  code << patmatch_target << "->";
  code << "index == " << variant_index << ") {\n";
  // ? within the while's block
  emit_pattern_match_destructure(patmatch_target, segment.identifier.get_str(), pattern, variant_type);
  // the_if->block->scope->parent = ctx.scope;
  the_while->block->accept(this);

  // ? exiting the while's block.
  code << "} else break; \n}\n";
}

void Emitter::emit_pattern_match_destructure(const std::string &temp_register, const std::string &variant_name,
                                             ASTPatternMatch *pattern, Type *variant_type) {
  if (variant_type->is_kind(TYPE_STRUCT)) {
    for (StructPattern::Part &part : pattern->struct_pattern.parts) {
      auto type = part.resolved_type;

      /*
        Here we cast arrays to pointers since arrays are not assignable but we still want to take references to them.
      */
      if (type->is_fixed_sized_array()) {
        // mut doesn't really matter here.
        type = type->get_element_type()->take_pointer_to(true);
      }

      code << get_declaration_type_signature_and_identifier(part.var_name.get_str(), type) << " = ";
      if (part.semantic != PTRN_MTCH_PTR_NONE) {
        code << "&";
      };
      code << "(";
      code << temp_register << "->";
      code << variant_name << "." << part.field_name.get_str() << ");\n";
    }
  } else if (variant_type->is_kind(TYPE_TUPLE)) {
    auto index = 0;
    for (TuplePattern::Part &part : pattern->tuple_pattern.parts) {
      auto type = part.resolved_type;

      /*
        Here we cast arrays to pointers since arrays are not assignable but we still want to take references to them.
      */
      if (type->is_fixed_sized_array()) {
        // mut doesn't really matter here.
        type = type->get_element_type()->take_pointer_to(MUT);
      }

      code << get_declaration_type_signature_and_identifier(part.var_name.get_str(), type) << " = ";
      if (part.semantic != PTRN_MTCH_PTR_NONE) {
        code << "&";
      }
      code << "(";
      code << temp_register << "->";
      code << variant_name << ".$" << std::to_string(index++) << ");\n";
    }
  } else if (variant_type == void_type()) {
    return;
  }
}

void Emitter::emit_pattern_match_for_switch_case(const Type *target_type, const std::string &target_temp_identifier,
                                                 const SwitchBranch &the_case, ASTPatternMatch *pattern) {
  auto old_scope = ctx.scope;
  ctx.set_scope(pattern->target_block->scope);
  Defer _([&] { ctx.scope = old_scope; });

  const auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  const auto variant_name = segment.identifier;
  const auto variant_type = info->get_variant_type(variant_name);
  const auto variant_index = info->get_variant_discriminant(variant_name);
  const auto is_pointer = target_type->is_pointer();

  code << "if (" << target_temp_identifier;
  if (is_pointer) {
    code << "->index == ";
  } else {
    code << ".index == ";
  }
  code << std::to_string(variant_index);
  code << ") {\n";

  {  // TODO: we should try to make this work with the exiting method to do this,
    // Copy pasting this is annoying.
    if (variant_type->is_kind(TYPE_STRUCT)) {
      for (const StructPattern::Part &part : pattern->struct_pattern.parts) {
        auto type = part.resolved_type;
        /*
          Here we cast arrays to pointers since arrays are not assignable but we still want to take references to them.
        */
        if (type->is_fixed_sized_array()) {
          // mut doesn't really matter here.
          type = type->get_element_type()->take_pointer_to(MUT);
        }

        code << get_declaration_type_signature_and_identifier(part.var_name.get_str(), type) << " = ";
        if (part.semantic != PTRN_MTCH_PTR_NONE) {
          code << "&";
        };
        code << "(" << target_temp_identifier;
        if (is_pointer) {
          code << "->";
        } else {
          code << ".";
        }

        code << variant_name.get_str() << "." << part.field_name.get_str() << ");\n";
      }
    } else if (variant_type->is_kind(TYPE_TUPLE)) {
      auto index = 0;
      for (TuplePattern::Part &part : pattern->tuple_pattern.parts) {
        auto type = part.resolved_type;
        /*
          Here we cast arrays to pointers since arrays are not assignable but we still want to take references to them.
        */
        if (type->is_fixed_sized_array()) {
          // mut doesn't really matter here.
          type = type->get_element_type()->take_pointer_to(MUT);
        }

        code << get_declaration_type_signature_and_identifier(part.var_name.get_str(), type) << " = ";
        if (part.semantic != PTRN_MTCH_PTR_NONE) {
          code << "&";
        }
        code << "(" << target_temp_identifier;
        if (is_pointer) {
          code << "->";
        } else {
          code << ".";
        }
        code << variant_name.get_str() << ".$" << std::to_string(index++) << ");\n";
      }
    }
  }

  the_case.block->accept(this);

  code << "}\n";
}

void Emitter::visit(ASTSwitch *node) {
  auto type = node->expression->resolved_type;
  bool use_default_eq_operator = true;

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->is_pointer()) {
    use_default_eq_operator = false;
  }

  // This is static to help with the lifetime issues taking an `address of` here would cause.
  static std::string the_register;
  if (!node->is_statement) {  // Declare a temporary for the C extension "Expression block";
    code << "({";
    the_register = "register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &the_register;
    code << type_to_string(node->return_type) << " " << the_register;
    semicolon();
  }

  static size_t index = 0;
  const auto target_unique_id = "$switch_target$" + std::to_string(index++);

  code << indent() << type_to_string(node->expression->resolved_type) << " " << target_unique_id << " = ";
  node->expression->accept(this);

  semicolon();
  newline();

  const auto target_type = node->expression->resolved_type;

  const auto emit_branch = [&](ASTExpr *target, const SwitchBranch &branch, bool first) {
    emit_line_directive(target);
    if (!first) {
      code << indent() << "else ";
    }

    if (branch.expression->get_node_type() == AST_NODE_PATTERN_MATCH) {
      emit_pattern_match_for_switch_case(target_type, target_unique_id, branch, (ASTPatternMatch *)branch.expression);
      return;
    }

    code << indent() << "if (";
    if (use_default_eq_operator) {
      code << target_unique_id;
      code << " == ";
      branch.expression->accept(this);
    } else {
      call_operator_overload(target->source_range, OPERATION_BINARY, TType::EQ, target, branch.expression);
    }
    code << ") ";
    branch.block->accept(this);
  };

  bool first = true;
  for (const auto &branch : node->branches) {
    emit_branch(node->expression, branch, first);
    first = false;
  }

  if (node->default_branch.is_not_null()) {
    code << "else {\n";
    node->default_branch.get()->accept(this);
    code << "}\n";
  }

  if (!node->is_statement) {
    cf_expr_return_register = nullptr;
    code << the_register;
    semicolon();
    code << "})";
  }
}

void Emitter::emit_default_construction(Type *type, std::vector<std::pair<InternedString, ASTExpr *>> values) {
  parenthesized(type_to_string(type));

  if (type->is_kind(TYPE_STRUCT) && type->has_no_extensions() && type->info->as<StructTypeInfo>()->is_union) {
    code << "{}";
    return;
  }

  if (type->is_pointer()) {
    code << "NULL";
    return;
  }

  code << "{";

  for (auto &member : type->info->members) {
    ASTExpr *initializer = nullptr;
    for (auto &[key, value] : values) {
      if (key == member.name) {
        initializer = value;
        break;
      }
    }

    if (initializer) {
      newline();
      emit_line_directive(initializer);
      indent_level++;
      code << indent() << '.' << member.name.get_str() << " = ";
      auto type = initializer->resolved_type;

      if (type->is_kind(TYPE_CHOICE) && initializer->get_node_type() == AST_NODE_PATH) {
        auto path = (ASTPath *)initializer;
        if (path->length() > 1) {
          emit_choice_marker_variant_instantiation(type, path);
        } else
          goto NORMAL;
      } else {
      NORMAL:
        code << "(" << type_to_string(type) << ")";
        initializer->accept(this);
      }
      indent_level--;
      code << ",";
    } else if (member.default_value) {
      code << "\n";
      auto value = member.default_value.get();
      code << "." << member.name.get_str() << " = ";
      value->accept(this);
      code << ",";
    } else if (member.type->is_kind(TYPE_STRUCT) && member.type->has_no_extensions() &&
               !member.type->info->as<StructTypeInfo>()->is_union) {
      code << "." << member.name.get_str() << " = ";
      emit_default_construction(member.type);
      code << ",";
    }
  }

  code << "}";
}

void Emitter::emit_arguments_no_parens(ASTArguments *node) {
  for (size_t i = 0; i < node->arguments.size(); ++i) {
    if (i != 0) {
      code << ", ";
    }

    auto argument = node->arguments[i];
    auto type = argument->resolved_type;

    /*
      TODO: we should get rid of this too.
    */
    if (type->is_kind(TYPE_CHOICE) && argument->get_node_type() == AST_NODE_PATH) {
      auto path = (ASTPath *)argument;
      emit_choice_marker_variant_instantiation(type, path);
    } else {
      node->arguments[i]->accept(this);
    }
  }
}

void Emitter::visit(ASTWhereStatement *node) {
  if (node->should_compile) {
    node->block->accept(this);
    return;
  }
  if (node->branch.is_null()) return;

  auto branch = node->branch.get();
  if (branch->where_stmt.is_not_null()) {
    branch->where_stmt.get()->accept(this);
    return;
  }
  if (branch->block.is_not_null()) {
    branch->block.get()->accept(this);
    return;
  }
}
