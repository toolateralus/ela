#include <format>
#include <functional>
#include <iterator>
#include <ostream>
#include <sstream>
#include <string>

#include "ast.hpp"
#include "builder.hpp"
#include "core.hpp"
#include "error.hpp"
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

constexpr auto TYPE_FLAGS_INTEGER = 1 << 0;
constexpr auto TYPE_FLAGS_FLOAT = 1 << 1;
constexpr auto TYPE_FLAGS_BOOL = 1 << 2;
constexpr auto TYPE_FLAGS_STRING = 1 << 3;
constexpr auto TYPE_FLAGS_STRUCT = 1 << 4;
constexpr auto TYPE_FLAGS_CHOICE = 1 << 5;
constexpr auto TYPE_FLAGS_ENUM = 1 << 6;
constexpr auto TYPE_FLAGS_TUPLE = 1 << 7;

constexpr auto TYPE_FLAGS_ARRAY = 1 << 8;
constexpr auto TYPE_FLAGS_FUNCTION = 1 << 9;
constexpr auto TYPE_FLAGS_POINTER = 1 << 10;

constexpr auto TYPE_FLAGS_SIGNED = 1 << 11;
constexpr auto TYPE_FLAGS_UNSIGNED = 1 << 12;
constexpr auto TYPE_FLAGS_TRAIT = 1 << 13;
constexpr auto TYPE_FLAGS_DYN = 1 << 14;
constexpr auto TYPE_FLAGS_UNION = 1 << 15;

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
      for (auto i = 0; i < info->params_len; i++) {
        forward_decl_type(info->parameter_types[i]);
      }
      forward_decl_type(info->return_type);
    } break;
    case TYPE_TUPLE:
    case TYPE_CHOICE:
    case TYPE_STRUCT: {
      auto info = type->info->as<StructTypeInfo>();
      std::string kw = "typedef struct ";
      if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
        kw = "typedef union ";
      }
      code << kw << to_cpp_string(type) << " " << to_cpp_string(type) << ";\n";
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

  emit_line_directive(node);
  code << indent() << "if (";
  node->condition->accept(this);
  code << ") ";
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  return;
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
      code << indent() << to_cpp_string(iterable_type) << " $iterable = ";
      node->right->accept(this);
      code << ";\n";
      emit_line_directive(node);
      code << indent() << to_cpp_string(iterator_type)
           << " $iterator = " << emit_symbol(iterable_scope->local_lookup("iter")) << "(&$iterable);\n";
      break;
    case ASTFor::ITERATOR:
      emit_line_directive(node);
      code << indent() << to_cpp_string(iterator_type) << " $iterator = ";
      node->right->accept(this);
      code << ";\n";
      break;
  }

  emit_line_directive(node);
  code << indent() << "while (1) {\n";
  indent_level++;

  // get the Option!<T> from next().
  emit_line_directive(node);
  code << indent() << "auto $next = ";
  code << emit_symbol(iterator_scope->local_lookup("next")) << "(&$iterator);\n";

  // end condition
  emit_line_directive(node);

  /// TODO: we should use a ASTPatternMatch here so that the compiler isn't completely tied to the Option!<T>
  /// implementation, For example, I changed the order of the type (switch Some and None) and every for loop was broken.
  /// It would be more reliable to just use a stack allocated ASTPatternMatch to emit here.
  code << indent() << "if ($next.index == 0) break;\n";

  auto identifier_type_str = to_cpp_string(node->identifier_type);

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
    auto scope = type->info->scope;

    if (type->extensions.is_pointer()) {
      auto t = type->get_element_type();
      scope = t->info->scope;
    }

    auto block = node->block;
    auto id = block->temp_iden_idx++;
    std::string temp_id = "$deconstruction$" + std::to_string(id++);

    code << indent() << "auto " << temp_id << " = $next.Some.$0;\n";

    auto is_tuple = type->is_kind(TYPE_TUPLE);

    int i = 0;
    for (auto name : scope->ordered_symbols) {
      auto symbol = scope->local_lookup(name);
      if (symbol->is_function() || symbol->is_type())
        continue;

      if (is_tuple) {
        name = "$" + name.get_str();
      }

      auto &destruct = node->left.destructure[i];
      auto iden = destruct.identifier;
      emit_line_directive(node);
      code << indent() << "auto " << iden.get_str();
      code << " = ";

      if (destruct.semantic == VALUE_SEMANTIC_POINTER) {
        if (type->extensions.is_pointer()) {
          code << "&" << temp_id << "->" << name.get_str() << ";\n";
        } else {
          code << "&" << temp_id << "." << name.get_str() << ";\n";
        }
      } else {
        if (type->extensions.is_pointer()) {
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

void Emitter::visit(ASTAlias *node) { return; }

void Emitter::visit(ASTArguments *node) {
  code << "(";
  for (int i = 0; i < node->arguments.size(); ++i) {
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
  code << ")";
  return;
}

void Emitter::visit(ASTType_Of *node) {
  auto id = node->target->resolved_type;
  if (!type_is_valid(id))
    throw_error("Invalid type in typeof() node", node->source_range);
  auto type = id;
  code << to_type_struct(type, ctx);
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

  auto type_string = to_cpp_string(type);

  code << type_string;
  return;
}

Type *Emitter::get_expr_left_type_sr_dot(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_TYPE:
      return node->resolved_type;
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      return dotnode->base->resolved_type;
    } break;
    case AST_NODE_PATH: {
      auto path = static_cast<ASTPath *>(node);
      return path->segments[path->segments.size() - 1].resolved_type;
    } break;
    default:
      throw_error(std::format("internal compiler error: 'get_dot_left_type' encountered an unexpected node, kind {}",
                              (int)node->get_node_type()),
                  node->source_range);
  }
  return Type::INVALID_TYPE;
}

void Emitter::visit(ASTCall *node) {
  std::vector<Type *> generic_args;
  if (node->has_generics()) {
    generic_args = typer.get_generic_arg_types(*node->get_generic_arguments().get());
  }

  auto resolved_func_type = node->function->resolved_type;

  if (node->function->get_node_type() == AST_NODE_PATH && resolved_func_type &&
      resolved_func_type->is_kind(TYPE_CHOICE)) {
    // Creating a choice type's tuple-variant, such as Option!<T>::Some(10), etc.
    emit_choice_tuple_variant_instantiation((ASTPath *)node->function, node->arguments);
  } else {
    // normal function call, or a static method.
    node->function->accept(this);
    code << mangled_type_args(generic_args);
    node->arguments->accept(this);
  }
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

void Emitter::visit(ASTLiteral *node) {
  auto type = to_cpp_string(node->resolved_type);
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
    auto type = to_cpp_string(node->operand->resolved_type);
    code << '(' << type << ')';
  }
  auto left_type = node->operand->resolved_type;
  auto left_ty = left_type;

  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_UNARY, node->op, node->operand, nullptr);
    return;
  }

  auto type = left_type;

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
  if (node->op == TType::Assign &&
      (node->right->get_node_type() == AST_NODE_SWITCH || node->right->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = node->resolved_type;
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    emit_line_directive(node);
    code << indent() << to_cpp_string(type) << " " << str << ";\n";

    code << indent();
    node->right->accept(this);
    node->left->accept(this);
    code << " = " << str;
    return;
  }

  auto left_ty = node->left->resolved_type;

  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_BINARY, node->op, node->left, node->right);
    return;
  }

  auto op_ty = node->op;
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
  return;
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
  auto name = emit_symbol(ctx.scope->lookup(node->name));

  // Emit switch / if expressions.
  if (node->value &&
      (node->value.get()->get_node_type() == AST_NODE_SWITCH || node->value.get()->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = node->resolved_type;
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    code << indent() << to_cpp_string(type) << " " << str << ";\n";
    node->value.get()->accept(this);
    emit_line_directive(node);
    code << indent() << to_cpp_string(node->type->resolved_type) << " " << name << " = " << str << ";\n";
    return;
  }

  if (node->type->resolved_type == Type::INVALID_TYPE) {
    throw_error("internal compiler error: type was null upon emitting an ASTDeclaration", node->source_range);
  }

  auto type = node->type->resolved_type;

  auto symbol = ctx.scope->local_lookup(node->name);

  auto handle_initialization = [&]() {
    if (node->value.is_not_null() && emit_default_value) {
      code << " = ";
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      auto type = node->type->resolved_type;
      if (type->is_kind(TYPE_STRUCT)) {
        code << "= {}";
      } else {
        code << "= {0}";
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
    code << "static constexpr ";
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    code << get_declaration_type_signature_and_identifier(name, type);
    handle_initialization();
    return;
  }

  if (node->is_bitfield) {
    node->type->accept(this);
    space();
    code << name;
    space();
    code << ": " << node->bitsize.get_str();
    handle_initialization();
    return;
  }

  if (type->extensions.is_fixed_sized_array()) {
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
  if (type->is_kind(TYPE_CHOICE) && node->value.is_not_null() && node->value.get()->get_node_type() == AST_NODE_PATH) {
    auto value = (ASTPath *)node->value.get();
    code << " = ";
    emit_choice_marker_variant_instantiation(type, value);
    code << ";\n";
  } else {
    handle_initialization();
  }
  return;
}

void Emitter::emit_forward_declaration(ASTFunctionDeclaration *node) {
  if (node->name == "main" || HAS_FLAG(node->flags, FUNCTION_IS_ENTRY)) {
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
  auto decl = symbol->function.declaration;
  if (decl->is_declared || decl->is_emitted) {
    return;
  } else {
    decl->is_declared = true;
  }

  if (HAS_FLAG(node->flags, FUNCTION_IS_EXPORTED)) {
    code << "extern ";
  }

  auto name = emit_symbol(symbol) + mangled_type_args(node->generic_arguments);
  auto returns = node->return_type->resolved_type;

  if (returns->is_kind(TYPE_FUNCTION)) {
    auto return_function_type = static_cast<FunctionTypeInfo *>(returns->info);

    // we take fixed array extensions as pointer here because it's invalid and would get casted off anyway.
    auto depth = returns->extensions.extensions.size();
    auto extensions = std::string(depth, '*');

    code << to_cpp_string(return_function_type->return_type) << "(" << extensions << name;
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
  if (node->name == "main" || HAS_FLAG(node->flags, FUNCTION_IS_ENTRY)) {
    throw_error("entry point function cannot be extern", node->source_range);
  }

  auto emit_params = [&] {
    code << '(';
    for (const auto &param : node->params->params) {
      code << get_cpp_scalar_type(param->resolved_type);
      if (param != node->params->params.back()) {
        code << ", ";
      }
    }
    if (HAS_FLAG(node->flags, FUNCTION_IS_VARARGS)) {
      code << ", ...);";
    } else {
      code << ")";
    }
  };

  code << "extern ";
  auto returns = node->return_type->resolved_type;
  if (returns->is_kind(TYPE_FUNCTION)) {
    auto return_function_type = static_cast<FunctionTypeInfo *>(returns->info);
    code << to_cpp_string(return_function_type->return_type) << "(*" << node->name.get_str();
    emit_params();
    code << ")";
  } else {
    code << to_cpp_string(returns);
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

  auto type = node->resolved_type;

  auto info = (type->info->as<StructTypeInfo>());

  std::string type_tag = (node->is_union ? "typedef union" : "typedef struct");
  auto name = node->scope->full_name();

  if (HAS_FLAG(info->flags, STRUCT_FLAG_FORWARD_DECLARED) || node->is_fwd_decl) {
    // We don't care about extern here.
    code << indent() << type_tag << " " << name << " " << name << ";\n";
    return;
  }

  auto previous = ctx.scope;
  ctx.set_scope(info->scope);
  Defer _defer2([&] { ctx.set_scope(previous); });

  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_ANONYMOUS)) {
    code << indent() << (node->is_union ? "union" : "struct") << " {\n";
  } else {
    if (node->is_extern) {
      code << indent() << "extern ";
    }
    code << type_tag << " " << name << " {\n";
  }
  indent_level++;

  auto old = emit_default_init;

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
    } else if (type->extensions.is_fixed_sized_array()) {
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
  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_ANONYMOUS)) {
    code << indent() << "};\n";
  } else {
    code << indent() << "} " << name << ";\n";
  }

  bool has_default_ctor = false;
  bool has_dtor = false;

  return;
}

void Emitter::visit(ASTEnumDeclaration *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  int n = 0;
  code << "typedef enum {\n";
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

  auto type = node->resolved_type;
  return;
}

void Emitter::visit(ASTParamDecl *node) {
  auto type = node->resolved_type;

  if (node->tag == ASTParamDecl::Normal) {
    if (type->is_kind(TYPE_FUNCTION) || type->extensions.is_fixed_sized_array()) {
      code << get_declaration_type_signature_and_identifier(node->normal.name.get_str(), type);
    } else {
      node->normal.type->accept(this);
      code << ' ' << node->normal.name.get_str();
    }
  } else if (node->tag == ASTParamDecl::Self) {
    code << to_cpp_string(type) << " self";
  }

  return;
}

void Emitter::visit(ASTParamsDecl *node) {
  code << "(";
  int i = 0;
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

void Emitter::visit(ASTProgram *node) {
  static const auto testing = compile_command.has_flag("test");
  size_t index = 0;
  ctx.set_scope(ctx.root_scope);

  // Emit runtime reflection type info for requested types,
  if (!type_info_strings.empty() && !is_freestanding) {
    std::stringstream type_info{};
    for (const auto &str : type_info_strings) {
      type_info << str.get_str() << ";\n";
    }

    code << "\nvoid $initialize_reflection_system() {\n";
    {
      // we don't bother doing pushes into type info, it's easier for us to do it this way.
      code << std::format("_type_info.length = _type_info.capacity = {};\n", type_info_strings.size());
      code << std::format("_type_info.data = calloc(sizeof(Type*), {});", type_table.size());
      code << type_info.str() << ";\n";
    }
    code << "}\n";

    code << R"_(
void $deinit_type(Type *type) {
  if (type->methods.length > 0 || type->methods.data != NULL)
    free(type->methods.data);

  if (type->traits.length  > 0 || type->traits.data != NULL)
    free(type->traits.data);

  if (type->fields.length  > 0 || type->fields.data != NULL)
    free(type->fields.data);

  if (type->generic_args.length  > 0 || type->generic_args.data != NULL)
    free(type->generic_args.data);

  free(type);
}
)_";

    StringBuilder builder(1024);
    for (auto type : reflected_upon_types) {
      builder << std::format("  $deinit_type(_type_info.data[{}]);\n", type);
    }
    code << "void $deinitialize_reflection_system() {\n";
    code << builder.str();
    code << "free(_type_info.data);\n";
    code << "\n}\n";
  }

  // this is just a macro, and it's guarded by an ifdef.
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

  // We now use a normalized main, with init and deinit code for Env and the reflection system, even in testing.
  // I am not sure how this ever worked before, when it was selectively initialized.
  // but this is better anyway
  if ((has_user_defined_main || testing) && !is_freestanding && !compile_command.has_flag("nostdlib")) {
    auto env_scope = ctx.scope->find_type_id("Env", {})->info->scope;

    const auto reflection_initialization =
        type_info_strings.size() != 0 ? "$initialize_reflection_system();" : "{/* no reflection present in module */};";

    const auto reflection_deinitialization = type_info_strings.size() != 0 ? "$deinitialize_reflection_system();"
                                                                           : "{/* no reflection present in module */};";
    constexpr auto main_format = R"_(
int main (int argc, char** argv) {{
  /* initialize command line args. */
  {}(argc, argv);
  /* reflection system */
  {}
  /* call user main, or dispatch tests, depending on the build type. */
  __TEST_RUNNER_MAIN;
  /* deinitialize command line args. */
  {}
}}
)_";

    code << std::format(main_format, emit_symbol(env_scope->lookup("initialize")), reflection_initialization,
                        reflection_deinitialization);
  }

  // TODO: if we're freestanding, we should just emit ID's only for typeof().
  if (is_freestanding && !type_info_strings.empty()) {
    throw_error("You cannot use runtime type reflection in a freestanding or "
                "nostdlib environment, due to a lack of allocators. To compare "
                "types, use typeid.",
                {});
  }

  return;
}

void Emitter::visit(ASTDotExpr *node) {
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = base_ty_id;
  auto op = ".";

  if (base_ty->extensions.is_pointer()) {
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
  auto left_ty = node->left->resolved_type;
  if (left_ty && node->is_operator_overload) {
    code << "(*"; // always dereference via subscript. for `type[10] = 10` and such.
    call_operator_overload(node->source_range, left_ty, OPERATION_SUBSCRIPT, TType::LBrace, node->left, node->index);
    code << ")";

    return;
  }

  node->left->accept(this);
  code << '[';
  node->index->accept(this);
  code << ']';
  return;
}

void Emitter::visit(ASTInitializerList *node) {
  auto type = node->resolved_type;

  if (!type->extensions.is_fixed_sized_array()) {
    code << "(" + to_cpp_string(type) + ")";
  }

  if (node->target_type.is_not_null() && node->target_type.get()->normal.path->get_node_type() == AST_NODE_PATH &&
      type->is_kind(TYPE_CHOICE)) {
    emit_choice_struct_variant_instantation(node->target_type.get()->normal.path, node);
    return;
  }

  code << "{";

  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      code << "0}";
      return;
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      newline();
      indent_level++;
      const auto size = node->key_values.size();
      for (int i = 0; i < node->key_values.size(); ++i) {
        const auto &[key, value] = node->key_values[i];
        emit_line_directive(value);
        code << indent() << '.' << key.get_str() << " = ";

        auto type = value->resolved_type;

        if (type->is_kind(TYPE_CHOICE) && value->get_node_type() == AST_NODE_PATH) {
          auto path = (ASTPath *)value;
          if (path->length() > 1) {
            emit_choice_marker_variant_instantiation(type, path);
          } else
            goto NORMAL;
        } else {
        NORMAL:
          code << "(" << to_cpp_string(type) << ")";
          value->accept(this);
        }
        if (i != size - 1) {
          code << ",\n";
        }
      }
      indent_level--;
    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      if (type->basename.get_str().starts_with("InitList$")) {
        auto element_type = type->generic_args[0];
        code << " .data = ";
        code << "(" << to_cpp_string(element_type) << "[]) {";
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

    } break;
  }
  code << "}";
  return;
}

void Emitter::visit(ASTRange *node) {
  code << "(" << to_cpp_string(node->resolved_type) << ") {";
  code << ".begin = ";
  node->left->accept(this);
  code << ", .end = ";
  node->right->accept(this);
  code << "}";
  return;
}

void Emitter::visit(ASTTuple *node) {
  auto type = node->resolved_type;
  auto name = "(" + to_cpp_string(type) + ")";
  code << name << " {";
  for (int i = 0; i < node->values.size(); ++i) {
    auto &value = node->values[i];
    code << ".$" << std::to_string(i) << " = ";
    value->accept(this);
    if (i != node->values.size() - 1)
      code << ", ";
  }
  code << "}";
  return;
}

void Emitter::visit(ASTTupleDeconstruction *node) {
  emit_line_directive(node);
  auto type = node->resolved_type;

  auto scope = type->info->scope;
  auto index = 0;
  static int temp_idx = 0;

  std::string identifier = "$deconstruction$" + std::to_string(temp_idx++);
  // declare a temporary variable referring to the right, so we can avoid re-evaluating the expression if it's a literal
  // or function call. this probably needs work.
  {
    code << to_cpp_string(type) << " " << identifier << " = ";
    node->right->accept(this);
    code << ";\n";
  }

  emit_line_directive(node);

  auto is_tuple = type->is_kind(TYPE_TUPLE);

  for (auto name : scope->ordered_symbols) {
    auto symbol = scope->local_lookup(name);
    if (symbol->is_function() || symbol->is_type())
      continue;

    if (is_tuple) {
      // tuples just have .0 .1 .2 .3 which isn't valid in
      // C, so we have to prefix it with a cash.
      name = "$" + name.get_str();
    }

    if (index > node->elements.size())
      break;

    auto semantic = node->elements[index].semantic;

    if (node->op == TType::ColonEquals) {
      code << "auto " << node->elements[index++].identifier.get_str() << " = ";
      if (semantic == VALUE_SEMANTIC_POINTER) {
        code << "&";
      }
      code << identifier << "." << name.get_str() << ";\n";
    } else {
      code << node->elements[index++].identifier.get_str() << " = ";
      if (semantic == VALUE_SEMANTIC_POINTER) {
        code << "&";
      }
      code << identifier << "." << name.get_str() << ";\n";
    }
  }

  emit_line_directive(node);
}

std::string Emitter::get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;
  auto sym_name = emit_symbol(ctx.scope->lookup(name));
  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = sym_name;
    auto &ext = type->extensions;
    return get_function_pointer_type_string(type, &identifier);
  }

  std::string base_type_str = to_cpp_string(type->base_type == Type::INVALID_TYPE ? type : type->base_type);
  std::string identifier = sym_name;

  for (const auto &ext : type->extensions.extensions) {
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
std::string Emitter::get_function_pointer_type_string(Type *type, Nullable<std::string> identifier,
                                                      bool type_erase_self) {
  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error("internal compiler error: tried to get a function pointer from "
                "a non-function type",
                {});
  }

  std::stringstream ss;

  int pointer_depth = 0;
  TypeExtensions other_extensions;
  for (auto ext : type->extensions.extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT)
      pointer_depth++;
    else
      other_extensions.extensions.push_back(ext);
  }

  auto type_prefix = std::string(pointer_depth, '*');
  auto type_postfix = other_extensions.to_string();

  auto info = (type->info->as<FunctionTypeInfo>());
  auto return_type = info->return_type;

  ss << to_cpp_string(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

  ss << type_postfix;

  ss << ")(";

  for (int i = 0; i < info->params_len; ++i) {
    if (i == 0 && type_erase_self) {
      ss << "void*";
    } else {
      auto type = info->parameter_types[i];
      ss << to_cpp_string(type);
    }

    if (i != info->params_len - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

std::string Emitter::get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context) {
  std::stringstream ss;
  ss << "(Field) { " << std::format(".name = (str){{.data=\"{}\", .length={}}}, ", name, calculate_actual_length(name))
     << std::format(".type = {}, ", to_type_struct(type, context));

  if (type->extensions.is_pointer()) {
    ss << std::format(".size = sizeof(void*), ");
  } else if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    if (type->is_kind(TYPE_STRUCT)) {
      auto flags = type->info->as<StructTypeInfo>()->flags;
      if (HAS_FLAG(flags, STRUCT_FLAG_FORWARD_DECLARED)) {
        ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
      } else {
        ss << ".size = 0, "; // non sized type
      }
    } else {
      ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    }

    if (parent_type->is_kind(TYPE_TUPLE)) {
      ss << std::format(
          ".offset = offsetof({}, {})",
          to_cpp_string(parent_type->base_type == Type::INVALID_TYPE ? parent_type : parent_type->base_type),
          "$" + name);
    } else {
      ss << std::format(
          ".offset = offsetof({}, {})",
          to_cpp_string(parent_type->base_type == Type::INVALID_TYPE ? parent_type : parent_type->base_type), name);
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

std::string get_type_flags(Type *type) {
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
      if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
        kind_flags = TYPE_FLAGS_UNION;
      }
    } break;
    case TYPE_ENUM:
      kind_flags = TYPE_FLAGS_ENUM;
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
  for (const auto &ext : type->extensions.extensions) {
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
  return ".flags = " + std::to_string(kind_flags) + "\n";
}

std::string Emitter::get_type_struct(Type *type, int id, Context &context, const std::string &fields) {
  std::stringstream ss;

  if (!type) {
    throw_error("internal compiler error: type was null in 'get_type_struct()' reflection emitter", {});
  }

  auto kind = 0;
  ss << "_type_info.data[" << id << "]" << "= malloc(sizeof(Type));\n";

  const auto type_string = type->to_string();
  ss << std::format("*_type_info.data[{}] = (Type){{ .id = {}, .name = (str){{.data=\"{}\", .length = {}}}, ", id, id,
                    type_string, calculate_actual_length(type_string));

  if (!type->is_kind(TYPE_ENUM) && !type->is_kind(TYPE_TRAIT) && !type->is_kind(TYPE_FUNCTION)) {
    if (type->extensions.is_pointer()) {
      ss << ".size = sizeof(void*), ";
    } else {
      ss << ".size = sizeof(" << to_cpp_string(type) << "), ";
    }
  }

  ss << get_type_flags(type) << ",\n";

  if (type->extensions.is_pointer() || type->extensions.is_fixed_sized_array()) {
    ss << ".element_type = " << to_type_struct(type->get_element_type(), context) << ",\n";
  } else {
    ss << ".element_type = NULL,\n";
  }

  ss << " };";

  auto get_fields_init_statements = [&] {
    std::stringstream fields_ss;
    if (!type->is_kind(TYPE_FUNCTION) && !type->is_kind(TYPE_ENUM)) {
      auto info = type->info;
      int count = info->scope->fields_count();
      if (count == 0) {
        return std::string("{}");
      }

      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &name : info->scope->ordered_symbols) {
        auto sym = info->scope->local_lookup(name);

        if (sym->is_type() || sym->is_function())
          continue;

        auto t = sym->type_id;

        if (!t)
          throw_error("internal compiler error: Type was null in reflection 'to_type_struct()'", {});

        fields_ss << "_type_info.data[" << id << "]->fields.data[" << it << "] = ";
        fields_ss << get_field_struct(name.get_str(), t, type, context) << ";\n";
        ++it;
      }
    } else if (type->kind == TYPE_ENUM) {
      // TODO: we have to fix this!.
      auto info = type->info;
      if (info->scope->ordered_symbols.empty()) {
        return std::string("{}");
      }

      int count = info->scope->fields_count();

      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &field : info->scope->ordered_symbols) {
        auto symbol = info->scope->local_lookup(field);
        if (!symbol || symbol->is_function())
          continue;

        auto t = s32_type();

        if (!t) {
          throw_error("internal compiler error: Type was null in reflection 'to_type_struct()'", {});
        }

        fields_ss << "_type_info.data[" << id << "]->fields.data[" << it << "] = ";
        fields_ss << get_field_struct(field.get_str(), t, type, context) << ";\n";
        ++it;
      }
    }
    return fields_ss.str();
  };

  auto get_generic_args_init_statements = [&] {
    std::stringstream generics_ss;

    if (type->generic_args.empty()) {
      return std::format(";\n{{ auto args = &_type_info.data[{}]->generic_args;\nargs->length = 0;\nargs->data = "
                         "NULL\n;args->capacity = 0;\n }}",
                         id);
    }

    int count = type->generic_args.size();
    {
      generics_ss << "_type_info.data[" << id << "]->generic_args.data = malloc(" << count << " * sizeof(Type));\n";
      generics_ss << "_type_info.data[" << id << "]->generic_args.length = " << count << ";\n";
      generics_ss << "_type_info.data[" << id << "]->generic_args.capacity = " << count << ";\n";
    }

    int idx = 0;
    for (const auto &arg : type->generic_args) {
      generics_ss << "_type_info.data[" << id << "]->generic_args.data[" << idx << "] = ";
      generics_ss << to_type_struct(arg, ctx) << ";\n";
      ++idx;
    }

    return generics_ss.str();
  };

  auto get_traits_init_stmts = [&] {
    std::stringstream traits_ss;

    if (type->traits.empty()) {
      return std::format(";\n{{ auto args = &_type_info.data[{}]->traits;\nargs->length = 0;\nargs->data = "
                         "NULL\n;args->capacity = 0;\n }}",
                         id);
    }

    int count = type->traits.size();
    {
      traits_ss << "_type_info.data[" << id << "]->traits.data = malloc(" << count << " * sizeof(Type));\n";
      traits_ss << "_type_info.data[" << id << "]->traits.length = " << count << ";\n";
      traits_ss << "_type_info.data[" << id << "]->traits.capacity = " << count << ";\n";
    }

    int idx = 0;
    for (const auto &trait : type->traits) {
      traits_ss << "_type_info.data[" << id << "]->traits.data[" << idx << "] = ";
      traits_ss << to_type_struct(trait, ctx) << ";\n";
      ++idx;
    }

    return traits_ss.str();
  };

  auto get_methods_init_statements = [&] {
    std::stringstream methods_ss;

    auto scope = type->info->scope;
    unsigned count = 0;

    for (const auto &[name, symbol] : scope->symbols) {
      if (!symbol.is_function() || !symbol.function.declaration) {
        continue;
      }
      auto declaration = symbol.function.declaration;
      if (DOESNT_HAVE_FLAG(declaration->flags, FUNCTION_IS_METHOD) || declaration->generic_arguments.size() != 0) {
        continue;
      }
      count++;
    }

    if (count == 0) {
      return std::format(";\n{{ auto args = &_type_info.data[{}]->traits;\nargs->length = 0;\nargs->data = "
                         "NULL\n;args->capacity = 0;\n }}",
                         id);
    }

    {
      methods_ss << "_type_info.data[" << id << "]->methods.data = malloc(" << count << " * sizeof(Type));\n";
      methods_ss << "_type_info.data[" << id << "]->methods.length = " << count << ";\n";
      methods_ss << "_type_info.data[" << id << "]->methods.capacity = " << count << ";\n";
    }

    int idx = 0;
    for (auto sym : scope->symbols) {
      auto name = sym.first;
      auto symbol = sym.second;
      if (!symbol.is_function() || symbol.is_generic_function()) {
        continue;
      }

      auto declaration = symbol.function.declaration;
      if (DOESNT_HAVE_FLAG(declaration->flags, FUNCTION_IS_METHOD) || declaration->generic_arguments.size() != 0) {
        continue;
      }
      methods_ss << "_type_info.data[" << id << "]->methods.data[" << idx << "].$0 = (str){.data=\"" << name.get_str()
                 << "\", .length= " << calculate_actual_length(name.get_str()) << "};\n";

      methods_ss << "_type_info.data[" << id << "]->methods.data[" << idx << "].$1 = " << emit_symbol(&symbol) << ";\n";
      ++idx;
    }
    return methods_ss.str();
  };

  ss << get_fields_init_statements();
  ss << get_generic_args_init_statements();

  ss << get_traits_init_stmts();

  if (!type->is_kind(TYPE_TRAIT))
    ss << get_methods_init_statements();

  type_info_strings.push_back(ss.str());
  reflected_upon_types.insert(id);
  return std::format("_type_info.data[{}]", id);
}

std::string Emitter::to_type_struct(Type *type, Context &context) {
  if (!type) {
    throw_error("internal compiler error: Reflection system got a null type", {});
  }

  auto id = type->uid;

  static bool *type_cache = [] {
    auto arr = new bool[type_table.size()];
    memset(arr, 0, type_table.size() * sizeof(bool));
    return arr;
  }();

  if (type_cache[id]) {
    return std::format("_type_info.data[{}]", id);
  }

  type_cache[id] = true;

  return get_type_struct(type, id, context, "{}");
}

bool Emitter::should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && HAS_FLAG(node->flags, FUNCTION_IS_TEST)) {
    return false;
  }

  auto sym = ctx.scope->lookup(node->name);
  if (node->declaring_type != Type::INVALID_TYPE) {
    sym = node->declaring_type->info->scope->local_lookup(node->name);
  }
  auto sym_name = emit_symbol(sym);

  // generate a test based on this function pointer.
  if (test_flag && HAS_FLAG(node->flags, FUNCTION_IS_TEST)) {
    visitor->test_functions << "($ela_test){.name = \"" << sym_name << "\", .function = &" << sym_name << "},";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && (HAS_FLAG(node->flags, FUNCTION_IS_ENTRY) || node->name == "main")) {
    return false;
  }
  return true;
}

std::string Emitter::to_cpp_string(const TypeExtensions &extensions, const std::string &base) {
  std::stringstream ss;
  ss << base;
  for (const auto ext : extensions.extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(ext.array_size) << "]";
    } else if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string Emitter::get_cpp_scalar_type(Type *id) {
  auto type = id;
  std::string name = "";

  return to_cpp_string(type);

  if (type->extensions.has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->extensions, name);
}

std::string Emitter::to_cpp_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_DYN: {
      auto info = type->info->as<DynTypeInfo>();
      output = "dyn$" + to_cpp_string(info->trait_type);
      output = to_cpp_string(type->extensions, output);
    } break;
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TRAIT:
    case TYPE_CHOICE: {
      output = to_cpp_string(type->extensions, type->info->scope->full_name());
      break;
    }
    case TYPE_TUPLE: {
      auto info = (type->info->as<TupleTypeInfo>());
      output = "$tuple";
      for (int i = 0; i < info->types.size(); ++i) {
        output += std::to_string(info->types[i]->uid);
        if (i != info->types.size() - 1) {
          output += "$";
        }
      }
      output = to_cpp_string(type->extensions, output);
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
  auto type_string = to_cpp_string(node->target_type->resolved_type);
  code << "(" << type_string << ")";
  node->expression->accept(this);
  return;
}

void Emitter::visit(ASTTraitDeclaration *node) { return; }

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
        code << to_cpp_string(field->resolved_type) << " " << field->name.get_str() << ";\n";
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
  code << "  int index;\n"; // TODO: we can compress this by the max value of the discriminant.
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
      auto depth = returns->extensions.extensions.size();
      auto extensions = std::string(depth, '*');

      code << to_cpp_string(return_function_type->return_type) << "(" << extensions << name;
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

    if (node->name != "main" && DOESNT_HAVE_FLAG(node->flags, FUNCTION_IS_ENTRY)) {
      if (HAS_FLAG(node->flags, FUNCTION_IS_STATIC)) {
        code << "static ";
      }
      if (HAS_FLAG(node->flags, FUNCTION_IS_FORWARD_DECLARED)) {
        emit_forward_declaration(node);
        return;
      } else if (HAS_FLAG(node->flags, FUNCTION_IS_INLINE)) {
        // We have to use static here otherwise the standalone inline function has
        // external linkage.
        // We should have a way to specify static linkage anyway.
        code << "static inline ";
      }
    }

    if (HAS_FLAG(node->flags, FUNCTION_IS_EXPORTED)) {
      code << "extern  ";
    }

    if ((node->name == "main" || HAS_FLAG(node->flags, FUNCTION_IS_ENTRY)) && !is_freestanding &&
        !compile_command.has_flag("nostdlib")) {
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

  if (HAS_FLAG(node->flags, FUNCTION_IS_EXTERN)) {
    emit_extern_function(node);
    return;
  }

  emit_various_function_declarations();

  return;
}

void Emitter::visit(ASTReturn *node) {
  // Emit switch / if expressions.
  if (node->expression && (node->expression.get()->get_node_type() == AST_NODE_SWITCH ||
                           node->expression.get()->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = node->resolved_type;
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    emit_line_directive(node);
    code << indent() << to_cpp_string(type) << " " << str << ";\n";
    node->expression.get()->accept(this);

    if (emitting_function_with_defer ||
        (node->declaring_block.is_not_null() && node->declaring_block.get()->defer_count != 0)) {
      emit_line_directive(node);
      code << indent() << to_cpp_string(type) << " " << defer_return_value_key << " = " << str << ";\n";
      emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
      emit_line_directive(node);
      code << indent() << "return " << defer_return_value_key << ";\n";
    } else {
      emit_line_directive(node);
      code << indent() << "return " << str << ";\n";
    }
    return;
  }

  if (emitting_function_with_defer ||
      (node->declaring_block.is_not_null() && node->declaring_block.get()->defer_count != 0)) {
    if (node->expression.is_not_null()) {
      emit_line_directive(node);
      auto type = node->expression.get()->resolved_type;

      if (type->is_kind(TYPE_CHOICE) && node->expression.get()->get_node_type() == AST_NODE_PATH) {
        auto path = (ASTPath *)node->expression.get();
        emit_choice_marker_variant_instantiation(type, path);
      } else {
        code << indent() << to_cpp_string(type) << " " << defer_return_value_key << " = ";
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

// This should never get hit.
void Emitter::visit(ASTWhere *node) {
  throw_error("internal compiler error: 'where' expression was visited in the emitter", node->source_range);
}

void Emitter::emit_tuple(Type *type) {
  if (type->base_type != Type::INVALID_TYPE) {
    type = type->base_type;
  }
  if (type->tuple_is_emitted) {
    return;
  } else {
    type->tuple_is_emitted = true;
  }
  auto name = to_cpp_string(type);

  code << "typedef struct " << name << " {\n";
  indent_level++;
  auto info = type->info->as<TupleTypeInfo>();
  for (int i = 0; i < info->types.size(); ++i) {
    auto type = info->types[i];
    if (type->is_kind(TYPE_FUNCTION)) {
      code << indent() << get_declaration_type_signature_and_identifier("$" + std::to_string(i), type) << ";\n";
    } else {
      code << indent() << to_cpp_string(type) << " $" << std::to_string(i) << ";\n";
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

void Emitter::visit(ASTImport *node) {}

void Emitter::call_operator_overload(const SourceRange &range, Type *left_ty, OperationKind operation, TType op,
                                     ASTExpr *left, ASTExpr *right) {
  auto call = ASTMethodCall{};
  auto dot = ASTDotExpr{};
  dot.base = left;
  dot.member = ASTPath::Segment{get_operator_overload_name(op, operation)};
  call.dot = &dot;
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

Emitter::Emitter(Context &context, Typer &type_visitor) : typer(type_visitor), ctx(context) {}

void Emitter::visit(ASTModule *node) {}

std::string Emitter::emit_symbol(Symbol *symbol) {
  if (symbol->is_local() ||
      (symbol->is_function() && HAS_FLAG(symbol->function.declaration->flags, FUNCTION_IS_EXTERN))) {
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

  code << "(" << to_cpp_string(node->resolved_type) << ") {\n";
  indent();
  code << ".instance = (void*)";
  node->object->accept(this);
  code << ",\n";

  // ugliest code ever freaking written.
  auto trait_scope = node->resolved_type->info->as<DynTypeInfo>()->trait_type->info->scope;

  auto object_scope_bare = node->object->resolved_type->get_element_type();
  auto object_scope = object_scope_bare->info->scope;

  for (auto [name, sym] : trait_scope->symbols) {
    if (sym.is_function() && !sym.is_generic_function()) {
      indent();
      code << "." << name.get_str() << " = ";
      auto symbol = object_scope->local_lookup(name);
      auto type = global_find_type_id(symbol->type_id, {{{TYPE_EXT_POINTER_MUT}}});
      auto typestring = get_function_pointer_type_string(type, nullptr, true);
      code << "(" << typestring << ")" << emit_symbol(symbol) << ",\n";
    }
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

  auto name = to_cpp_string(dyn_ty);
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
  std::vector<Type *> generic_args = node->dot->member.get_resolved_generics();

  auto symbol = ctx.get_symbol(node->dot).get();
  // Call a function pointer via a dot expression
  if (symbol->is_variable()) {
    {
      // Implicitly pass the 'dyn.instance' when calling the function pointers
      // that the dyn thingy sets up.
      auto object = node->dot->base;
      auto obj_type = object->resolved_type;

      if (obj_type->is_kind(TYPE_DYN)) {
        auto &args = node->arguments->arguments;
        auto dot = ast_alloc<ASTDotExpr>();
        dot->base = object;
        dot->member = ASTPath::Segment{"instance"};
        dot->resolved_type = global_find_type_id(void_type(), {{{TYPE_EXT_POINTER_MUT}}});
        args.insert(args.begin(), dot);
      }
    }

    auto func = node->dot;
    func->accept(this);
    code << mangled_type_args(generic_args);
    node->arguments->accept(this);
    return;
  }

  auto func = symbol->function.declaration;

  Type *function_type = symbol->type_id;
  // if generic function
  if (!func->generic_parameters.empty()) {
    func = (ASTFunctionDeclaration *)find_generic_instance(func->generic_instantiations, generic_args);
    function_type = func->resolved_type;
  }

  code << emit_symbol(symbol) + mangled_type_args(generic_args);
  code << "(";

  auto self_param_ty = function_type->info->as<FunctionTypeInfo>()->parameter_types[0];

  auto base_type = node->dot->base->resolved_type;

  if (self_param_ty->extensions.is_pointer() && !base_type->extensions.is_pointer()) {
    // TODO: add an r-value analyzer, since we can't take a pointer to temporary memory like literals & rvalues.
    code << "&";
  } else if (!self_param_ty->extensions.is_pointer() && base_type->extensions.is_pointer()) {
    code << "*";
  }

  ASTExpr *base = node->dot->base;
  base->accept(this);
  if (node->arguments->arguments.size() > 0) {
    code << ", ";
  }

  for (auto &arg : node->arguments->arguments) {
    arg->accept(this);
    if (arg != node->arguments->arguments.back()) {
      code << ", ";
    }
  }
  code << ")";
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
    const auto index = info->get_variant_index(last_segment.identifier);
    const auto type_string = to_cpp_string(type);
    code << " (" << type_string << ") { .index = " << index << "}";
  }
}

void Emitter::emit_choice_tuple_variant_instantiation(ASTPath *path, ASTArguments *arguments) {
  const auto choice_type = path->resolved_type;
  const auto info = choice_type->info->as<ChoiceTypeInfo>();
  const auto last_segment = path->segments.back();
  const auto variant_name = last_segment.identifier;
  const auto variant_type = info->get_variant_type(variant_name);
  const auto type_string = to_cpp_string(choice_type);

  code << "(" << type_string << ") {\n";
  code << ".index = " << std::to_string(info->get_variant_index(variant_name)) << ",\n";
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
  const auto variant_type = info->get_variant_type(variant_name);
  const auto type_string = to_cpp_string(choice_type);

  code << "(" << type_string << ") {\n";
  code << ".index = " << std::to_string(info->get_variant_index(variant_name)) << ",\n";
  code << "." << variant_name.get_str() << " = " << "{";
  for (const auto &[name, value] : initializer->key_values) {
    code << "." << name.get_str() << " = ";
    value->accept(this);
    code << ", ";
  }
  code << "},";
  code << "}";
}

void Emitter::visit(ASTPath *node) {
  auto symbol = ctx.get_symbol(node);
  code << emit_symbol(symbol.get());
}

/*
  This probably should be limited to exclusively be done on 'if' statements, or anything that's followed by a block
  and has statement semantics. it needs to be scoped, and also that scope only should be reachable upon success.
*/
void Emitter::visit(ASTPatternMatch *node) {
  throw_error("pattern matches cannot exist outside of 'if' statements currently. they will be expanded to 'while', "
              "'switch', and possibly other nodes in the future.",
              node->source_range);
}

void Emitter::emit_pattern_match_for_if(ASTIf *the_if, ASTPatternMatch *pattern) {
  ctx.set_scope(pattern->scope);
  auto old_scope = ctx.scope;
  Defer _([&] { ctx.scope = old_scope; });

  auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  auto variant_type = info->get_variant_type(segment.identifier);
  const auto variant_index = info->get_variant_index(segment.identifier);

  const auto object_type = pattern->object->resolved_type;
  const auto is_pointer = object_type->extensions.is_pointer();

  code << "if (";
  pattern->object->accept(this);
  if (is_pointer) {
    code << "->";
  } else {
    code << ".";
  }
  code << "index == " << variant_index << ") {\n";
  // within the if block
  emit_pattern_match_destructure(pattern->object, segment.identifier.get_str(), pattern, variant_type);
  // the_if->block->scope->parent = ctx.scope;
  the_if->block->accept(this);
  // exiting the if block.
  code << "}\n";

  if (the_if->_else.get()) {
    the_if->_else.get()->accept(this);
  }
}

void Emitter::emit_pattern_match_for_while(ASTWhile *the_while, ASTPatternMatch *pattern) {
  ctx.set_scope(pattern->scope);
  auto old_scope = ctx.scope;
  Defer _([&] { ctx.scope = old_scope; });

  auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  auto variant_type = info->get_variant_type(segment.identifier);
  const auto variant_index = info->get_variant_index(segment.identifier);

  const auto object_type = pattern->object->resolved_type;
  const auto is_pointer = object_type->extensions.is_pointer();

  code << "while (";
  pattern->object->accept(this);
  if (is_pointer) {
    code << "->";
  } else {
    code << ".";
  }
  code << "index == " << variant_index << ") {\n";
  // ? within the while's block

  emit_pattern_match_destructure(pattern->object, segment.identifier.get_str(), pattern, variant_type);
  // the_if->block->scope->parent = ctx.scope;
  the_while->block->accept(this);

  // ? exiting the while's block.
  code << "}\n";
}

void Emitter::emit_pattern_match_destructure(ASTExpr *object, const std::string &variant_name, ASTPatternMatch *pattern,
                                             Type *variant_type) {
  const auto object_type = object->resolved_type;
  const auto is_pointer = object_type->extensions.is_pointer();

  /* I'm just gonna use auto in here cause im lazy. */
  if (variant_type->is_kind(TYPE_STRUCT)) {
    auto info = variant_type->info->as<StructTypeInfo>();
    for (StructPattern::Part &part : pattern->struct_pattern.parts) {
      auto type = part.resolved_type;
      code << to_cpp_string(type) << " " << part.var_name.get_str() << " = ";
      if (part.semantic == PTR_MUT || part.semantic == PTR_CONST) {
        code << "&";
      };
      code << "(";
      object->accept(this);

      if (is_pointer) {
        code << "->";
      } else {
        code << ".";
      }

      code << variant_name << "." << part.field_name.get_str() << ");\n";
    }
  } else if (variant_type->is_kind(TYPE_TUPLE)) {
    auto info = variant_type->info->as<TupleTypeInfo>();
    auto index = 0;
    for (TuplePattern::Part &part : pattern->tuple_pattern.parts) {
      auto type = part.resolved_type;
      code << to_cpp_string(type) << " " << part.var_name.get_str() << " = ";
      if (part.semantic == PTR_MUT || part.semantic == PTR_CONST) {
        code << "&";
      }
      code << "(";
      object->accept(this);
      if (is_pointer) {
        code << "->";
      } else {
        code << ".";
      }
      code << variant_name << ".$" << std::to_string(index++) << ");\n";
    }
  } else if (variant_type == void_type()) {
    return;
  }
}

void Emitter::emit_pattern_match_for_switch_case(const Type *target_type, const std::string &target_temp_identifier,
                                                 const SwitchCase &the_case, ASTPatternMatch *pattern) {
  ctx.set_scope(pattern->scope);
  auto old_scope = ctx.scope;
  Defer _([&] { ctx.scope = old_scope; });

  auto path = pattern->target_type_path;
  const auto type = pattern->target_type_path->resolved_type;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto segment = path->segments.back();

  const auto variant_name = segment.identifier;
  auto variant_type = info->get_variant_type(variant_name);
  const auto variant_index = info->get_variant_index(variant_name);
  const auto is_pointer = target_type->extensions.is_pointer();

  code << "if (" << target_temp_identifier;
  if (is_pointer) {
    code << "->index == ";
  } else {
    code << ".index == ";
  }
  code << std::to_string(variant_index);
  code << ") {\n";

  { // TODO: we should try to make this work with the exiting method to do this,
    // Copy pasting this is annoying.
    if (variant_type->is_kind(TYPE_STRUCT)) {
      auto info = variant_type->info->as<StructTypeInfo>();
      for (StructPattern::Part &part : pattern->struct_pattern.parts) {
        auto type = part.resolved_type;
        code << to_cpp_string(type) << " " << part.var_name.get_str() << " = ";
        if (part.semantic == PTR_MUT || part.semantic == PTR_CONST) {
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
      auto info = variant_type->info->as<TupleTypeInfo>();
      auto index = 0;
      for (TuplePattern::Part &part : pattern->tuple_pattern.parts) {
        auto type = part.resolved_type;
        code << to_cpp_string(type) << " " << part.var_name.get_str() << " = ";
        if (part.semantic == PTR_MUT || part.semantic == PTR_CONST) {
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
    } else if (variant_type == void_type()) {
      return;
    }
  }

  the_case.block->accept(this);

  code << "}\n";
}

void Emitter::visit(ASTSwitch *node) {
  auto type = node->target->resolved_type;
  bool use_eq_operator = true;

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->extensions.is_pointer()) {
    use_eq_operator = false;
  }

  static size_t index = 0;
  auto target_unique_id = "$switch_target$" + std::to_string(index++);

  code << indent() << "auto " << target_unique_id << " = ";
  node->target->accept(this);

  semicolon();
  newline();

  const auto target_type = node->target->resolved_type;
  const auto is_choice_type = target_type->is_kind(TYPE_CHOICE);

  auto emit_switch_case = [&](ASTExpr *target, const SwitchCase &_case, bool first) {
    emit_line_directive(target);
    if (!first) {
      code << indent() << "else ";
    }

    /*
      The 'else' is handled above.
    */
    if (_case.expression->get_node_type() == AST_NODE_PATTERN_MATCH) {
      emit_pattern_match_for_switch_case(target_type, target_unique_id, _case, (ASTPatternMatch *)_case.expression);
      return;
    }

    code << indent() << "if (";
    if (use_eq_operator) {
      code << target_unique_id;
      code << " == ";
      _case.expression->accept(this);
    } else {
      call_operator_overload(target->source_range, type, OPERATION_BINARY, TType::EQ, target, _case.expression);
    }
    code << ") ";
    _case.block->accept(this);
  };

  bool first = true;
  for (const auto &_case : node->cases) {
    emit_switch_case(node->target, _case, first);
    first = false;
  }

  if (node->default_case.is_not_null()) {
    code << "else {\n";
    node->default_case.get()->accept(this);
    code << "}\n";
  }
}