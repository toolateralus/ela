#include <format>
#include <functional>
#include <iterator>
#include <ostream>
#include <sstream>
#include <string>

#include "ast.hpp"
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
constexpr auto TYPE_FLAGS_TAGGED_UNION = 1 << 5;
constexpr auto TYPE_FLAGS_ENUM = 1 << 6;
constexpr auto TYPE_FLAGS_TUPLE = 1 << 7;

constexpr auto TYPE_FLAGS_ARRAY = 1 << 8;
constexpr auto TYPE_FLAGS_FUNCTION = 1 << 9;
constexpr auto TYPE_FLAGS_POINTER = 1 << 10;

constexpr auto TYPE_FLAGS_SIGNED = 1 << 11;
constexpr auto TYPE_FLAGS_UNSIGNED = 1 << 12;

void Emitter::forward_decl_type(Type *type) {
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  if (type->fwd_decl_is_emitted) {
    return;
  } else {
    type->fwd_decl_is_emitted = true;
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->get_info()->as<FunctionTypeInfo>();
      for (auto i = 0; i < info->params_len; i++) {
        forward_decl_type(global_get_type(info->parameter_types[i]));
      }
      forward_decl_type(global_get_type(info->return_type));
    } break;
    case TYPE_TUPLE:
    case TYPE_TAGGED_UNION:
    case TYPE_STRUCT: {
      auto info = type->get_info()->as<StructTypeInfo>();
      std::string kw = "typedef struct ";
      if ((info->flags & STRUCT_FLAG_IS_UNION) != 0)
        kw = "typedef union ";
      (*ss) << kw << to_cpp_string(type) << " " << to_cpp_string(type) << ";\n";
    } break;
    default:
      return;
  }
}

void Emitter::visit(ASTWhile *node) {
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  emit_line_directive(node);
  (*ss) << indent() << "while (";
  if (node->condition.is_not_null()) {
    node->condition.get()->accept(this);
  } else {
    (*ss) << "true";
  }
  (*ss) << ") ";
  node->block->accept(this);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  defer_blocks.pop_back();
  return;
}

void Emitter::visit(ASTIf *node) {
  emit_line_directive(node);
  (*ss) << indent() << "if (";
  node->condition->accept(this);
  (*ss) << ")";
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  return;
}

void Emitter::visit(ASTElse *node) {
  emit_line_directive(node);
  (*ss) << " else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return;
}

void Emitter::visit(ASTFor *node) {
  emit_line_directive(node);
  auto old_scope = ctx.scope;
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  ctx.set_scope(node->block->scope);

  static size_t depth = 0;
  std::string range_unique_id = "$_range_id" + std::to_string(depth);
  std::string unique_id = "$_loop_id" + std::to_string(depth);
  depth++;

  Defer _defer([] { depth--; });

  (*ss) << indent() << "{\n";
  indent_level++;

  std::string range_type_str = to_cpp_string(global_get_type(node->range_type));
  std::string iterable_type_str = to_cpp_string(global_get_type(node->iterable_type));
  std::string identifier_type_str = to_cpp_string(global_get_type(node->identifier_type));
  auto iterable_method_str = "$" + std::to_string(node->iterable_type);

  switch (node->iteration_kind) {
    case ASTFor::ITERABLE:
      (*ss) << indent() << range_type_str << " " << range_unique_id << " = ";
      node->right->accept(this);
      (*ss) << ";\n";
      (*ss) << indent() << iterable_type_str << " " << unique_id << " = $" << std::to_string(node->range_type)
            << "_iter(&" << range_unique_id << ");\n";
      break;
    case ASTFor::ITERATOR:
      (*ss) << indent() << iterable_type_str << " " << unique_id << " = ";
      node->right->accept(this);
      (*ss) << ";\n";
      break;
  }

  (*ss) << indent() << "while (1) {\n";
  indent_level++;

  static size_t temp_iden_idx = 0;
  std::string option_temp_identiifer = "$next" + std::to_string(temp_iden_idx);

  // get the Option!<T> from next().
  (*ss) << "auto " << option_temp_identiifer << " = ";

  (*ss) << iterable_method_str << "_next(&" << unique_id << ");\n";

  // end condition
  (*ss) << "if (!" << option_temp_identiifer << ".has_value) break;\n";

  if (node->left_tag == ASTFor::IDENTIFIER) {
    (*ss) << indent() << identifier_type_str << " ";
    node->left.identifier->accept(this);
    auto type = global_get_type(node->identifier_type);

    if (node->left.semantic == VALUE_SEMANTIC_POINTER) {
      (*ss) << " = " << option_temp_identiifer << ".s;";
    } else {
      // This is terrible, because the auto dereference is really just presuming a ton.
      // What if i want to copy a pointer while iterating over a list of s8*'s?
      if (node->needs_dereference) {
        (*ss) << " = *" << option_temp_identiifer << ".s;";
      } else {
        (*ss) << " = " << option_temp_identiifer << ".s;";
      }
    }

  } else if (node->left_tag == ASTFor::DESTRUCTURE) {
    auto type = global_get_type(node->identifier_type);
    auto scope = type->get_info()->scope;
    if (type->get_ext().is_pointer()) {
      auto t = global_get_type(type->get_element_type());
      scope = t->get_info()->scope;
    }
    auto block = node->block;
    auto id = block->temp_iden_idx++;
    std::string temp_id = "$deconstruction$" + std::to_string(id++);

    (*ss) << "auto " << temp_id; 
    if (node->left.semantic == VALUE_SEMANTIC_POINTER) {
      (*ss) << option_temp_identiifer << ".s;";
    } else {
      if (node->needs_dereference) {
        (*ss) << " = *" << option_temp_identiifer << ".s;";
      } else {
        (*ss) << " = " << option_temp_identiifer << ".s;";
      }
    }

    auto is_tuple = type->is_kind(TYPE_TUPLE);

    int i = 0;
    for (auto name: scope->ordered_symbols) {
      auto symbol = scope->local_lookup(name);
      if (symbol->is_function() || symbol->is_type())
        continue;
      
      if (is_tuple) {
        name = "$" + name.get_str();
      }

      auto &destruct = node->left.destructure[i];
      auto iden = destruct.identifier;
      (*ss) << indent() << "auto ";
      iden->accept(this);
      (*ss) << " = ";

      if (destruct.semantic == VALUE_SEMANTIC_POINTER) {
        if (type->get_ext().is_pointer()) {
          (*ss) << "&" << temp_id << "->" << name.get_str() << ";\n";
        } else {
          (*ss) << "&" << temp_id << "." << name.get_str() << ";\n";
        }
      } else {
        if (type->get_ext().is_pointer()) {
          (*ss) << temp_id << "->" << name.get_str() << ";\n";
        } else {
          (*ss) << temp_id << "." << name.get_str() << ";\n";
        }
      }
      i++;
    }
  }

  node->block->accept(this);

  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);

  indent_level--;
  (*ss) << indent() << "}\n";

  indent_level--;
  (*ss) << indent() << "}\n";

  defer_blocks.pop_back();
  ctx.set_scope(old_scope);
  return;
}

void Emitter::visit(ASTAlias *node) { return; }

void Emitter::visit(ASTArguments *node) {
  (*ss) << "(";
  for (int i = 0; i < node->arguments.size(); ++i) {
    if (i != 0) {
      (*ss) << ", ";
    }
    node->arguments[i]->accept(this);
  }
  (*ss) << ")";
  return;
}

void Emitter::visit(ASTType_Of *node) {
  auto id = node->target->resolved_type;
  if (id == -1)
    throw_error("Invalid type in typeof() node", node->source_range);
  auto type = global_get_type(id);
  (*ss) << to_type_struct(type, ctx);
}

void Emitter::visit(ASTType *node) {
  auto type = global_get_type(node->resolved_type);
  if (!type) {
    throw_error("internal compiler error: ASTType* resolved to null in emitter.", node->source_range);
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    (*ss) << get_function_pointer_type_string(type);
    return;
  }

  if (type->is_kind(TYPE_ENUM)) {
    auto enum_info = (type->get_info()->as<EnumTypeInfo>());
    auto elem_ty = global_get_type(enum_info->element_type);
    (*ss) << to_cpp_string(elem_ty);
    return;
  }

  auto type_string = to_cpp_string(type);

  (*ss) << type_string;
  return;
}

int Emitter::get_expr_left_type_sr_dot(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_TYPE:
      return node->resolved_type;
    case AST_NODE_IDENTIFIER:
      return ctx.scope->lookup(static_cast<ASTIdentifier *>(node)->value)->type_id;
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      return dotnode->base->resolved_type;
    } break;
    case AST_NODE_SCOPE_RESOLUTION: {
      auto srnode = static_cast<ASTScopeResolution *>(node);
      return srnode->base->resolved_type;
    } break;
    default:
      throw_error(std::format("internal compiler error: 'get_dot_left_type' encountered an unexpected node, kind {}",
                              (int)node->get_node_type()),
                  node->source_range);
  }
  return Type::INVALID_TYPE_ID;
}

void Emitter::visit(ASTCall *node) {
  auto base_symbol = ctx.get_symbol(node->function);

  std::vector<int> generic_args;
  for (const auto arg : node->generic_arguments) {
    generic_args.push_back(arg->resolved_type);
  }

  auto symbol = base_symbol.get();
  if (node->function->get_node_type() == AST_NODE_DOT_EXPR) {
    if (!base_symbol || !base_symbol.get()->is_function()) {
      throw_error("can't call a non-function", node->source_range);
    }

    auto func = symbol->function.declaration;

    auto method_call = (func->flags & FUNCTION_IS_METHOD) != 0;
    auto static_method = (func->flags & FUNCTION_IS_STATIC) != 0;

    if (!method_call || static_method) {
      throw_error("cannot call a static method from an instance", node->source_range);
    }

    auto base_type = global_get_type(get_expr_left_type_sr_dot(node->function));
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
      auto instance =
          find_generic_instance(func->generic_instantiations, typer.get_generic_arg_types(node->generic_arguments));
      function_type = global_get_type(instance->resolved_type);
    }
    auto param_0_ty = global_get_type(function_type->get_info()->as<FunctionTypeInfo>()->parameter_types[0]);

    if (param_0_ty->get_ext().is_pointer() && !base_type->get_ext().is_pointer()) {
      // TODO: add an r-value analyzer, since we can't take a pointer to temporary memory like literals & rvalues.
      (*ss) << "&";
    } else if (!param_0_ty->get_ext().is_pointer() && base_type->get_ext().is_pointer()) {
      (*ss) << "*";
    }

    ASTExpr *base = static_cast<ASTDotExpr *>(node->function)->base;
    base->accept(this);
    if (node->arguments->arguments.size() > 0) {
      (*ss) << ", ";
    }

    for (auto &arg : node->arguments->arguments) {
      arg->accept(this);
      if (arg != node->arguments->arguments.back()) {
        (*ss) << ", ";
      }
    }
    (*ss) << ")";
  } else {
    auto func = node->function;
    if (func->get_node_type() == AST_NODE_TYPE) {
      auto ast_type = static_cast<ASTType *>(func);
      if (ast_type->kind != ASTType::NORMAL) {
        throw_error("Cannot call a tuple or function type", node->source_range);
      }
      if (!ast_type->normal.generic_arguments.empty()) {
        throw_error("internal compiler error: generic args to call put on base", node->source_range);
      }
      func = ast_type->normal.base;
    }
    // normal function call, or a static method.
    func->accept(this);
    (*ss) << mangled_type_args(generic_args);
    node->arguments->accept(this);
  }

  return;
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
  auto type = to_cpp_string(global_get_type(node->resolved_type));
  std::string output;
  switch (node->tag) {
    case ASTLiteral::Null:
      (*ss) << "NULL";
      return;
    case ASTLiteral::String: {
      if (node->is_c_string) {
        output = std::format("\"{}\"", node->value.get_str());
      } else {
        // TODO:
        // We don't want null terminated strings, but the problem is, if we use an initializer list for an array of
        // bytes, then all of our string literals are stack allocated. If we make them static, then there's a chance
        // that the user mutates the string literal, and it will change it's meaning for the rest of the program

        // I have spent literally all day figting these two probelms, and I have decided it is time to move on, for now,
        // we will keep the null terminated strings until we have a solution for this.
        auto str = node->value.get_str();
        (*ss) << std::format("(str) {{ .data = \"{}\", .length = {} }}", str, calculate_actual_length(str));
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
    // TODO : emit character literals as hexadecimal values so we're UTF8 friendly.
    // that's why we have fat u32 chars anyway.
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
  (*ss) << output;
  return;
}

void Emitter::visit(ASTIdentifier *node) {
  (*ss) << node->value.get_str();
  return;
}
void Emitter::visit(ASTUnaryExpr *node) {
  if (node->op.type == TType::Sub) {
    auto type = to_cpp_string(global_get_type(node->operand->resolved_type));
    (*ss) << '(' << type << ')';
  }
  auto left_type = node->operand->resolved_type;
  auto left_ty = global_get_type(left_type);

  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_UNARY, node->op.type, node->operand, node);
    return;
  }

  auto type = global_get_type(left_type);

  // we always do these as postfix unary since if we don't it's kinda undefined
  // behaviour and it messes up unary expressions at the end of dot expressions
  if (node->op.type == TType::Increment || node->op.type == TType::Decrement) {
    node->operand->accept(this);
    (*ss) << node->op.value.get_str();
  } else {
    (*ss) << '(';
    (*ss) << node->op.value.get_str();
    node->operand->accept(this);
    (*ss) << ")";
  }
  return;
}
void Emitter::visit(ASTBinExpr *node) {
  if (node->op.type == TType::Assign &&
      (node->right->get_node_type() == AST_NODE_SWITCH || node->right->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    node->right->accept(this);
    node->left->accept(this);
    (*ss) << " = " << str;
    return;
  }

  auto left_ty = global_get_type(node->left->resolved_type);

  if (left_ty && node->is_operator_overload) {
    call_operator_overload(node->source_range, left_ty, OPERATION_BINARY, node->op.type, node->left, node->right);
    return;
  }

  auto op_ty = node->op.type;
  (*ss) << "(";
  node->left->accept(this);
  space();
  (*ss) << node->op.value.get_str();
  if (node->op.type == TType::Assign) {
    auto type = global_get_type(node->resolved_type);
    auto isptr = type->get_ext().is_pointer();
    if (isptr)
      (*ss) << "(" << to_cpp_string(type) << ")";
  }
  space();
  node->right->accept(this);
  (*ss) << ")";
  return;
}
void Emitter::visit(ASTExprStatement *node) {
  emit_line_directive(node);
  (*ss) << indent();
  node->expression->accept(this);
  return;
}

void Emitter::visit(ASTVariable *node) {
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }
  emit_line_directive(node);

  // Emit switch / if expressions.
  if (node->value &&
      (node->value.get()->get_node_type() == AST_NODE_SWITCH || node->value.get()->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    node->value.get()->accept(this);
    (*ss) << to_cpp_string(global_get_type(node->type->resolved_type)) << " " << node->name.get_str() << " = " << str;
    return;
  }

  if (node->type->resolved_type == Type::INVALID_TYPE_ID) {
    throw_error("internal compiler error: type was null upon emitting an ASTDeclaration", node->source_range);
  }

  auto type = global_get_type(node->type->resolved_type);
  auto symbol = ctx.scope->local_lookup(node->name);

  auto handle_initialization = [&]() {
    if (node->value.is_not_null() && emit_default_value) {
      (*ss) << " = ";
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      auto type = global_get_type(node->type->resolved_type);
      if (type->is_kind(TYPE_STRUCT)) {
        (*ss) << "= {}";
      } else {
        (*ss) << "= {0}";
      }
    }
    (*ss) << ";\n";
  };

  auto old = emit_default_init;
  Defer _([&] { emit_default_init = old; });
  if (node->is_extern) {
    (*ss) << "extern ";
    emit_default_init = false;
  }
  if (node->is_static) {
    (*ss) << "static ";
  }
  if (node->is_constexpr) {
    (*ss) << "static constexpr ";
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    (*ss) << get_declaration_type_signature_and_identifier(node->name.get_str(), type);
    handle_initialization();
    return;
  }

  if (node->is_bitfield) {
    node->type->accept(this);
    space();
    (*ss) << node->name.get_str();
    space();
    (*ss) << ": " << node->bitsize.get_str();
    handle_initialization();
    return;
  }

  if (type->get_ext().is_fixed_sized_array()) {
    (*ss) << get_declaration_type_signature_and_identifier(node->name.get_str(), type);
    if (node->value.is_not_null()) {
      (*ss) << " = ";
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      (*ss) << "= {0}";
    }
    return;
  }

  node->type->accept(this);
  space();
  (*ss) << node->name.get_str();
  space();
  handle_initialization();
  return;
}

void Emitter::emit_forward_declaration(ASTFunctionDeclaration *node) {
  if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
    (*ss) << "extern  ";
  }

  node->return_type->accept(this);
  (*ss) << ' ' << node->name.get_str() << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
}

void Emitter::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name == "main" || node->name == "маин") {
    throw_error("main/маин function cannot be foreign", node->source_range);
  }

  auto emit_params = [&] {
    (*ss) << '(';
    for (const auto &param : node->params->params) {
      (*ss) << get_cpp_scalar_type(param->resolved_type);
      if (param != node->params->params.back()) {
        (*ss) << ", ";
      }
    }
    if ((node->flags & FUNCTION_IS_VARARGS) != 0) {
      (*ss) << ", ...);";
    } else {
      (*ss) << ")";
    }
  };

  (*ss) << "extern ";
  auto returns = global_get_type(node->return_type->resolved_type);
  if (returns->is_kind(TYPE_FUNCTION)) {
    auto return_function_type = static_cast<FunctionTypeInfo *>(returns->get_info());
    (*ss) << to_cpp_string(global_get_type(return_function_type->return_type)) << "(*" << node->name.get_str();
    emit_params();
    (*ss) << ")";
  } else {
    (*ss) << to_cpp_string(returns);
    space();
    (*ss) << " " + node->name.get_str();
    emit_params();
  }

  semicolon();
  space();
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

  emit_line_directive(node);
  auto type = global_get_type(node->resolved_type);

  auto info = (type->get_info()->as<StructTypeInfo>());

  std::string type_name = type->get_base().get_str();
  std::string type_tag = (node->is_union ? "typedef union " : "typedef struct ");

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED || node->is_fwd_decl) != 0) {
    if (node->is_extern) {
      // (*ss) << "extern ";
      // I do not believe this is ever neccesary in C, you can alwasy just define an
      // opaque struct and link against it, or redefine it: it doesn't matter.
    }
    (*ss) << type_tag << " " << type_name << " " << type_name << ";\n";
    return;
  }

  auto previous = ctx.scope;
  ctx.set_scope(info->scope);
  Defer _defer2([&] { ctx.set_scope(previous); });

  if ((info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << (node->is_union ? "union " : "struct ");
    (*ss) << "{\n";
  } else {
    if (node->is_extern) {
      (*ss) << "extern ";
    }
    (*ss) << type_tag << " " << type_name << "{\n";
  }
  indent_level++;

  auto old = emit_default_init;

  emit_default_init = false;

  Defer _defer1([&] { emit_default_init = old; });

  for (const auto &subtype : node->subtypes) {
    indented("");
    subtype->accept(this);
    semicolon();
    newline();
  }

  for (const auto &member : node->members) {
    indented("");
    auto type = global_get_type(member.type->resolved_type);
    if (type->is_kind(TYPE_FUNCTION)) {
      auto name = member.name.get_str();
      auto name_nullable = Nullable(&name);
      (*ss) << get_function_pointer_type_string(type, name_nullable);
    } else if (type->get_ext().is_fixed_sized_array()) {
      (*ss) << get_declaration_type_signature_and_identifier(member.name.get_str(), type);
    } else {
      member.type->accept(this);
      space();
      (*ss) << member.name.get_str();
    }
    semicolon();
    newline();
  }

  // this is for anonymous substructs which just unfold at C compile time into the struct's namespace.
  if ((info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << "};\n";
  } else {
    (*ss) << "} " << type_name << ";\n";
  }

  indent_level--;

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
  emit_line_directive(node);
  auto type_name = node->name.get_str();
  int n = 0;
  (*ss) << "typedef enum {\n";
  for (const auto &[key, value] : node->key_values) {
    (*ss) << type_name << "_" << key.get_str();
    if (node->is_flags) {
      (*ss) << " = ";
      (*ss) << std::to_string(1 << n);
    } else if (value) {
      (*ss) << " = ";
      value->accept(this);
    }
    if (n != node->key_values.size() - 1) {
      (*ss) << ",\n";
    }
    n++;
  }
  (*ss) << "} " << type_name << ";\n";

  auto type = global_get_type(node->resolved_type);
  return;
}

void Emitter::visit(ASTParamDecl *node) {
  auto type = global_get_type(node->resolved_type);

  if (node->tag == ASTParamDecl::Normal) {
    if (type->is_kind(TYPE_FUNCTION)) {
      (*ss) << get_declaration_type_signature_and_identifier(node->normal.name.get_str(), type);
    } else {
      node->normal.type->accept(this);
      (*ss) << ' ' << node->normal.name.get_str();
    }
  } else if (node->tag == ASTParamDecl::Self) {
    (*ss) << ' ' << to_cpp_string(type) << " self";
  } else {
    (*ss) << ' ' << to_cpp_string(type) << " себя";
  }

  return;
}
void Emitter::visit(ASTParamsDecl *node) {
  (*ss) << "(";
  int i = 0;
  for (const auto &param : node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      (*ss) << ", ";
    }
    ++i;
  }

  if (node->is_varargs) {
    (*ss) << ", ...)";
    return;
  }

  (*ss) << ")";
  return;
}

void Emitter::visit(ASTProgram *node) {
  emit_line_directive(node);

  static const auto testing = compile_command.has_flag("test");

  size_t index = 0;
  ctx.set_scope(ctx.root_scope);
  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
    }
    statement->accept(this);
    semicolon();
    newline();
    index++;
  }
  ctx.set_scope(ctx.root_scope);

  // Emit runtime reflection type info for requested types, only when we have
  // actually requested runtime type information.
  if (!ctx.type_info_strings.empty() && !is_freestanding) {
    std::stringstream type_info{};
    for (const auto &str : ctx.type_info_strings) {
      type_info << str.get_str() << ";\n";
    }

    if (!compile_command.has_flag("release")) {
      code << "#line 0 \"boilerplate.h\"";
    }

    code << "\nvoid $initialize_reflection_system() {\n";
    {
      // we don't bother doing pushes into type info, it's easier for us to do it this way.
      code << std::format("_type_info.length = _type_info.capacity = {};\n", ctx.type_info_strings.size());
      code << std::format("_type_info.data = realloc(_type_info.data, sizeof(Type*) * {});", type_table.size());
      code << type_info.str() << ";\n";
    }

    code << "}\n";

    if (!compile_command.has_flag("release")) {
      code << "#line 0 \"boilerplate.h\"";
    }
  }

  if (testing) {
    auto test_init = test_functions.str();
    if (test_init.ends_with(',')) {
      test_init.pop_back();
    }
    if (!compile_command.has_flag("release")) {
      code << "#line 0 \"boilerplate.hpp\"\n";
    }
    code << TESTING_MAIN_BOILERPLATE_AAAAGHH << '\n';
    // deploy the array of test struct wrappers.
    code << std::format("$ela_test tests[{}] = {}\n", num_tests, "{ " + test_init + " };");
    // use the test runner main macro.
    code << "__TEST_RUNNER_MAIN;";
  } else {
    if (has_user_defined_main && !is_freestanding) {
      if (!compile_command.has_flag("release")) {
        code << "#line 0 \"boilerplate.h\"\n";
      }

      code << std::format("int main (int argc, char** argv) {{\n${}_initialize(argc, "
                          "argv);\n{}\n__ela_main_();\n}}\n",
                          ctx.scope->find_type_id("Env", {}),
                          ctx.type_info_strings.size() != 0 ? "$initialize_reflection_system();"
                                                            : "{/* no reflection present in module */};");
    }
  }

  // TODO: if we're freestanding, we should just emit ID's only for typeof().
  if (is_freestanding && !ctx.type_info_strings.empty()) {
    throw_error("You cannot use runtime type reflection in a freestanding or "
                "nostdlib environment, due to a lack of allocators. To compare "
                "types, use typeid.",
                {});
  }

  return;
}

void Emitter::visit(ASTDotExpr *node) {
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = global_get_type(base_ty_id);
  auto op = ".";

  if (base_ty->get_ext().is_pointer()) {
    op = "->";
  }

  node->base->accept(this);
  (*ss) << op;
  if (base_ty->is_kind(TYPE_TUPLE)) {
    (*ss) << "$";
  }
  (*ss) << node->member_name.get_str();
  return;
}

void Emitter::visit(ASTSubscript *node) {
  auto left_ty = global_get_type(node->left->resolved_type);
  if (left_ty && node->is_operator_overload) {
    (*ss) << "*"; // always dereference via subscript. for `type[10] = 10` and such.
    call_operator_overload(node->source_range, left_ty, OPERATION_SUBSCRIPT, TType::LBrace, node->left,
                           node->subscript);

    return;
  }

  node->left->accept(this);
  (*ss) << '[';
  node->subscript->accept(this);
  (*ss) << ']';
  return;
}
void Emitter::visit(ASTInitializerList *node) {
  auto type = global_get_type(node->resolved_type);

  if (!type->get_ext().is_fixed_sized_array()) {
    (*ss) << "(" + to_cpp_string(type) + ")";
  }
  (*ss) << " {";

  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      (*ss) << "0}";
      return;
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      const auto size = node->key_values.size();
      for (int i = 0; i < node->key_values.size(); ++i) {
        const auto &[key, value] = node->key_values[i];
        (*ss) << '.' << key.get_str() << " = ";
        (*ss) << "(" << to_cpp_string(global_get_type(value->resolved_type)) << ")";
        value->accept(this);
        if (i != size - 1) {
          (*ss) << ",\n";
        }
      }
    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      if (type->get_base().get_str().starts_with("Init_List$")) {
        auto element_type = type->generic_args[0];
        (*ss) << " .data = ";
        (*ss) << "(" << to_cpp_string(global_get_type(element_type)) << "[]) {";
        for (const auto &expr : node->values) {
          expr->accept(this);
          if (expr != node->values.back()) {
            (*ss) << ", ";
          }
        }
        (*ss) << "}, .length = " << std::to_string(node->values.size());
      } else {
        for (const auto &expr : node->values) {
          expr->accept(this);
          if (expr != node->values.back()) {
            (*ss) << ", ";
          }
        }
      }

    } break;
  }
  (*ss) << "}";
  return;
}

void Emitter::visit(ASTRange *node) {
  (*ss) << "(" << to_cpp_string(global_get_type(node->resolved_type)) << ") {";
  (*ss) << ".begin = ";
  node->left->accept(this);
  (*ss) << ", .end = ";
  node->right->accept(this);
  (*ss) << "}";
  return;
}

void Emitter::visit(ASTSwitch *node) {
  auto type = global_get_type(node->target->resolved_type);
  bool use_eq_operator = true;

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->get_ext().is_pointer()) {
    use_eq_operator = false;
  }

  static size_t index = 0;
  auto target_unique_id = "$switch_target$" + std::to_string(index++);

  (*ss) << "auto " << target_unique_id << " = ";
  node->target->accept(this);
  semicolon();
  newline();

  auto emit_switch_case = [&](ASTExpr *target, const SwitchCase &_case, bool first) {
    if (!first) {
      (*ss) << " else ";
    }
    emit_line_directive(target);
    (*ss) << " if (";
    if (use_eq_operator) {
      (*ss) << target_unique_id;
      (*ss) << " == ";
      _case.expression->accept(this);
    } else {
      call_operator_overload(target->source_range, type, OPERATION_BINARY, TType::EQ, target, _case.expression);
    }
    (*ss) << ") ";
    emit_line_directive(_case.block);
    _case.block->accept(this);
  };

  bool first = true;
  for (const auto &_case : node->cases) {
    emit_switch_case(node->target, _case, first);
    first = false;
  }

  if (node->default_case.is_not_null()) {
    (*ss) << "else {\n";
    node->default_case.get()->accept(this);
    (*ss) << "}\n";
  }
}
void Emitter::visit(ASTTuple *node) {
  auto type = global_get_type(node->resolved_type);
  auto name = "(" + to_cpp_string(type) + ")";
  (*ss) << name << " {";
  for (int i = 0; i < node->values.size(); ++i) {
    auto &value = node->values[i];
    (*ss) << ".$" << std::to_string(i) << " = ";
    value->accept(this);
    if (i != node->values.size() - 1)
      (*ss) << ", ";
  }
  (*ss) << "}";
  return;
}

void Emitter::visit(ASTTupleDeconstruction *node) {
  emit_line_directive(node);
  auto type = global_get_type(node->resolved_type);

  auto scope = type->get_info()->scope;
  auto index = 0;
  static int temp_idx = 0;

  std::string identifier = "$deconstruction$" + std::to_string(temp_idx++);
  // declare a temporary variable referring to the right, so we can avoid re-evaluating the expression if it's a literal
  // or function call. this probably needs work.
  {
    (*ss) << to_cpp_string(type) << " " << identifier << " = ";
    node->right->accept(this);
  }

  semicolon();
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
      (*ss) << "auto " << node->elements[index++].identifier->value.get_str() << " = ";
      if (semantic == VALUE_SEMANTIC_POINTER) { 
        (*ss) << "&";
      }
      (*ss) << identifier << "." << name.get_str() << ";\n";
    } else {
      (*ss) << node->elements[index++].identifier->value.get_str() << " = ";
      if (semantic == VALUE_SEMANTIC_POINTER)  {
        (*ss) << "&";
      }
      (*ss) << identifier << "." << name.get_str() << ";\n";
    }
  }

  emit_line_directive(node);
}

std::string Emitter::get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;
  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    auto &ext = type->get_ext();
    return get_function_pointer_type_string(type, &identifier);
  }

  std::string base_type_str = to_cpp_string(global_get_type(type->base_id == -1 ? type->id : type->base_id));
  std::string identifier = name;

  for (const auto &ext : type->get_ext().extensions) {
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
std::string Emitter::get_function_pointer_type_string(Type *type, Nullable<std::string> identifier) {
  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error("internal compiler error: tried to get a function pointer from "
                "a non-function type",
                {});
  }

  std::stringstream ss;

  int pointer_depth = 0;
  TypeExtensions other_extensions;
  for (auto ext : type->get_ext().extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT)
      pointer_depth++;
    else
      other_extensions.extensions.push_back(ext);
  }

  auto type_prefix = std::string(pointer_depth, '*');
  auto type_postfix = other_extensions.to_string();

  auto info = (type->get_info()->as<FunctionTypeInfo>());
  auto return_type = global_get_type(info->return_type);

  ss << to_cpp_string(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

  ss << type_postfix;

  ss << ")(";

  for (int i = 0; i < info->params_len; ++i) {
    auto type = global_get_type(info->parameter_types[i]);
    ss << to_cpp_string(type);
    if (i != info->params_len - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

std::string Emitter::get_field_struct(const std::string &name, Type *type, Type *parent_type, Context &context) {
  std::stringstream ss;
  ss << "(Field) { " << std::format(".name = \"{}\", ", name)
     << std::format(".type = {}, ", to_type_struct(type, context));

  if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    if (type->is_kind(TYPE_STRUCT)) {
      if ((type->get_info()->as<StructTypeInfo>()->flags & STRUCT_FLAG_FORWARD_DECLARED) == 0) {
        ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
      } else {
        ss << ".size = 0"; // non sized type
      }
    } else {
      ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    }

    if (parent_type->is_kind(TYPE_TUPLE)) {
      ss << std::format(
          ".offset = offsetof({}, {})",
          to_cpp_string(global_get_type(parent_type->base_id == -1 ? parent_type->id : parent_type->base_id)),
          "$" + name);
    } else {
      ss << std::format(
          ".offset = offsetof({}, {})",
          to_cpp_string(global_get_type(parent_type->base_id == -1 ? parent_type->id : parent_type->base_id)), name);
    }
  }

  if (parent_type->is_kind(TYPE_ENUM)) {
    auto symbol = parent_type->get_info()->as<EnumTypeInfo>()->scope->local_lookup(name);
    // We don't check the nullable here because it's an absolute guarantee that enum variables all have
    // a value always.
    auto value = evaluate_constexpr((ASTExpr *)symbol->variable.initial_value.get(), ctx);
    ss << std::format(".enum_value = {}", value.integer);
  }

  ss << " }";
  return ss.str();
}

std::string Emitter::get_elements_function(Type *type) {
  //! We have to remove these lambdas so we can compile down to C.
  auto element_type = global_get_type(type->get_element_type());
  if (!type->get_ext().is_fixed_sized_array()) {
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
                       to_cpp_string(element_type), to_type_struct(element_type, ctx));
  } else {
    auto size = type->get_ext().extensions.back().array_size;
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
                       to_cpp_string(element_type), size, to_type_struct(element_type, ctx));
  }
}

std::string get_type_flags(Type *type) {
  int kind_flags = 0;
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
    case TYPE_TAGGED_UNION:
      kind_flags = TYPE_FLAGS_TAGGED_UNION;
      break;
    case TYPE_INTERFACE:
      kind_flags = 0;
      break;
  }
  for (const auto &ext : type->get_ext().extensions) {
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
  ss << "*_type_info.data[" << id << "] = (Type) {" << ".id = " << id << ", "
     << ".name = \"" << type->to_string() << "\", ";

  if (!type->is_kind(TYPE_ENUM))
    ss << ".size = sizeof(" << to_cpp_string(type) << "), ";

  ss << get_type_flags(type) << ",\n";

  // ! We can't use this either: it uses a lambda.
  //   if (type->get_ext().is_fixed_sized_array()) {
  //     ss << get_elements_function(type) << ",\n";
  //   }

  if (type->get_ext().is_pointer() || type->get_ext().is_fixed_sized_array()) {
    ss << ".element_type = " << to_type_struct(global_get_type(type->get_element_type()), context) << ",\n";
  } else {
    ss << ".element_type = NULL,\n";
  }

  ss << " };";

  auto get_fields_init_statements = [&] {
    std::stringstream fields_ss;
    if (!type->is_kind(TYPE_FUNCTION) && !type->is_kind(TYPE_ENUM)) {
      auto info = type->get_info();
      if (info->scope->symbols.empty()) {
        return std::string("{}");
      }

      int count = info->scope->fields_count();
      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &tuple : info->scope->symbols) {
        auto &[name, sym] = tuple;
        if (sym.is_type() || sym.is_function())
          continue;
        auto t = global_get_type(sym.type_id);

        if (!t)
          throw_error("internal compiler error: Type was null in reflection 'to_type_struct()'", {});

        fields_ss << "_type_info.data[" << id << "]->fields.data[" << it << "] = ";
        fields_ss << get_field_struct(name.get_str(), t, type, context) << ";\n";
        ++it;
      }
    } else if (type->kind == TYPE_ENUM) {
      // TODO: we have to fix this!.
      auto info = type->get_info();
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

        auto t = global_get_type(s32_type());

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

  ss << get_fields_init_statements();
  context.type_info_strings.push_back(ss.str());
  return std::format("_type_info.data[{}]", id);
}

std::string Emitter::to_type_struct(Type *type, Context &context) {
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

  return get_type_struct(type, id, context, "{}");
}

bool Emitter::should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "($ela_test){.name = \"" << node->name.get_str() << "\", .function = &"
                            << node->name.get_str() << "},";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && (node->name == "main" || node->name == "маин")) {
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

std::string Emitter::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";

  return to_cpp_string(type);

  if (type->get_ext().has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->get_ext(), name);
}

std::string Emitter::to_cpp_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TAGGED_UNION: {
      output = to_cpp_string(type->get_ext(), type->get_base().get_str());
      break;
    }
    case TYPE_TUPLE: {
      auto info = (type->get_info()->as<TupleTypeInfo>());
      output = "$tuple";
      for (int i = 0; i < info->types.size(); ++i) {
        output += std::to_string(info->types[i]);
        if (i != info->types.size() - 1) {
          output += "$";
        }
      }
      output = to_cpp_string(type->get_ext(), output);
      break;
    }
    case TYPE_INTERFACE:
      throw_error("can't declare an instance of an interface", {});
      break;
  }
  return output;
}

void Emitter::visit(ASTScopeResolution *node) {
  auto type = global_get_type(node->base->resolved_type);
  // for static function aclls and enum access, but this probably encompasses all of the usage.
  // The reason we check here, is because the left of this may be another Scope Resolution node.
  // This should probably be a lot more robust
  if (node->base->get_node_type() == AST_NODE_IDENTIFIER || node->base->get_node_type() == AST_NODE_TYPE) {
    if (type->is_kind(TYPE_ENUM)) {
      (*ss) << type->get_base().get_str();
    } else {
      (*ss) << "$" + std::to_string(type->id);
    }
  } else {
    node->base->accept(this);
  }
  auto op = "_";
  (*ss) << op << node->member_name.get_str();
  return;
}

void Emitter::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    return;
  }

  auto target = global_get_type(node->target->resolved_type);

  if (!target) {
    throw_error("internal compiler error: impl target type was null in the emitter", node->source_range);
  }
  auto old_type = type_context;
  type_context = node->target;
  Defer _([&] { type_context = old_type; });

  for (const auto &method : node->methods) {
    method->accept(this);
  }

  return;
}

void Emitter::visit(ASTCast *node) {
  auto type_string = to_cpp_string(global_get_type(node->target_type->resolved_type));
  (*ss) << "(" << type_string << ")";
  node->expression->accept(this);
  return;
}

void Emitter::visit(ASTInterfaceDeclaration *node) { return; }
void Emitter::visit(ASTTaggedUnionDeclaration *node) {
  if (node->is_emitted) {
    return;
  }

  node->is_emitted = true;

  emit_line_directive(node);

  (*ss) << "typedef struct " << node->name.get_str() << " " << node->name.get_str() << ";\n";
  auto name = node->name.get_str();

  for (const auto &variant : node->variants) {
    if (variant.kind == ASTTaggedUnionVariant::STRUCT) {
      auto subtype_name = name + "_" + variant.name.get_str();
      (*ss) << "typedef struct " << subtype_name << " {\n";
      for (const auto &field : variant.struct_declarations) {
        field->accept(this);
        (*ss) << ";\n";
      }
      (*ss) << "} " << subtype_name << ";\n";
    } else if (variant.kind == ASTTaggedUnionVariant::TUPLE) {
      auto subtype_name = name + "_" + variant.name.get_str();
      (*ss) << "typedef ";
      variant.tuple->accept(this);
      (*ss) << " " << subtype_name << ";\n";
    }
  }

  (*ss) << "typedef struct " << node->name.get_str() << " {\n";
  (*ss) << "  int index;\n";
  (*ss) << "  union {\n";

  int n = 0;
  for (const auto &variant : node->variants) {
    if (variant.kind == ASTTaggedUnionVariant::STRUCT) {
      auto subtype_name = name + "_" + variant.name.get_str();
      (*ss) << "    " << subtype_name << " $index_" << std::to_string(n) << ";\n";
    } else if (variant.kind == ASTTaggedUnionVariant::TUPLE) {
      auto subtype_name = name + "_" + variant.name.get_str();
      (*ss) << "    " << subtype_name << " $index_" << std::to_string(n) << ";\n";
    }
    n++;
  }

  (*ss) << "  };\n";
  (*ss) << "} " << node->name.get_str() << ";\n";
  return;
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
      semicolon();
      newline();
    }
    defer_block++;
  }
  if (defer_block == defer_blocks.rend()) {
    throw_error("internal compiler error: could not find defer block type in stack", {});
  }
  for (auto defer : defer_block->defers) {
    defer->statement->accept(this);
    semicolon();
    newline();
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
  (*ss) << ' ' << node->unique_identifier.get_str() << ' ';
  node->params->accept(this);
  node->block->accept(this);
  newline();
}

void Emitter::visit(ASTFunctionDeclaration *node) {
  auto emit_function_signature_and_body = [&](const std::string &name) {
    auto returns = global_get_type(node->return_type->resolved_type);

    if (returns->is_kind(TYPE_FUNCTION)) {
      auto return_function_type = static_cast<FunctionTypeInfo *>(returns->get_info());

      // we take fixed array extensions as pointer here because it's invalid and would get casted off anyway.
      auto depth = returns->get_ext().extensions.size();
      auto extensions = std::string(depth, '*');

      (*ss) << to_cpp_string(global_get_type(return_function_type->return_type)) << "(" << extensions << name;
      node->params->accept(this);
      (*ss) << ")";
    } else {
      node->return_type->accept(this);
      (*ss) << " " + name;
      node->params->accept(this);
    }
    defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_FUNC});
    if (node->block.is_not_null()) {
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

    if (node->name == "to_string" && !node->generic_arguments.empty() && node->generic_arguments[0] == 30) {
      int x = 0;
    }

    if (node->name != "main" && node->name != "маин") {
      if ((node->flags & FUNCTION_IS_STATIC) != 0) {
        (*ss) << "static ";
      }
      if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        emit_forward_declaration(node);
        return;
      }
    }

    if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
      (*ss) << "extern  ";
    }

    std::string name;
    if (node->declaring_type != Type::INVALID_TYPE_ID) {
      name += "$" + std::to_string(node->declaring_type) + "_";
    }
    name += node->name.get_str();
    if (!node->generic_arguments.empty()) {
      name += mangled_type_args(node->generic_arguments);
    }

    if ((node->name == "main" || node->name == "маин") && !is_freestanding) {
      has_user_defined_main = true;
      user_defined_entry_point = node->name;
      node->return_type->accept(this);
      (*ss) << " __ela_main_()"; // We use Env::args() to get args now.
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

  if ((node->flags & FUNCTION_IS_FOREIGN) != 0) {
    emit_foreign_function(node);
    return;
  }

  emit_various_function_declarations();

  return;
}

void Emitter::visit(ASTReturn *node) {
  emit_line_directive(node);

  // Emit switch / if expressions.
  if (node->expression && (node->expression.get()->get_node_type() == AST_NODE_SWITCH ||
                           node->expression.get()->get_node_type() == AST_NODE_IF)) {
    auto old = std::move(cf_expr_return_register);
    Defer _defer([&]() { cf_expr_return_register = std::move(old); });
    auto type = global_get_type(node->resolved_type);
    std::string str = "$register$" + std::to_string(cf_expr_return_id++);
    cf_expr_return_register = &str;

    (*ss) << to_cpp_string(type) << " " << str << ";\n";
    node->expression.get()->accept(this);

    if (emitting_function_with_defer ||
        (node->declaring_block.is_not_null() && node->declaring_block.get()->defer_count != 0)) {
      (*ss) << to_cpp_string(type) << " " << defer_return_value_key << " = " << str << ";\n";
      emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
      (*ss) << "return " << defer_return_value_key << ";\n";
    } else {
      (*ss) << "return " << str << ";\n";
    }
    return;
  }

  if (emitting_function_with_defer ||
      (node->declaring_block.is_not_null() && node->declaring_block.get()->defer_count != 0)) {
    if (node->expression.is_not_null()) {
      auto type = global_get_type(node->expression.get()->resolved_type);
      (*ss) << to_cpp_string(type) << " " << defer_return_value_key << " = ";
      node->expression.get()->accept(this);
      (*ss) << ";\n";
    }
    emit_deferred_statements(DEFER_BLOCK_TYPE_FUNC);
    (*ss) << "return";
    if (node->expression.is_not_null()) {
      (*ss) << " " << defer_return_value_key;
    }
    (*ss) << ";\n";
  } else if (cf_expr_return_register.is_null() || node->expression.is_null()) {
    indented("return");
    if (node->expression.is_not_null()) {
      space();
      node->expression.get()->accept(this);
    }
    (*ss) << ";\n";
  } else {
    (*ss) << *cf_expr_return_register.get() << " = ";
    node->expression.get()->accept(this);
    (*ss) << ";\n";
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
  emit_line_directive(node);
  (*ss) << (" {\n");
  indent_level++;
  ctx.set_scope(node->scope);

  defer_blocks.emplace_back();

  for (const auto &statement : node->statements) {
    emit_line_directive(statement);
    if (statement->get_node_type() == AST_NODE_DECLARATION) {
      indented("");
    }
    statement->accept(this);
    semicolon();
    newline();
  }

  emit_deferred_statements(DEFER_BLOCK_TYPE_OTHER);
  defer_blocks.pop_back();

  indent_level--;
  indented("}");
  ctx.exit_scope();
  return;
}

void Emitter::visit(ASTLambda *node) { (*ss) << node->unique_identifier.get_str(); }

// This should never get hit.
void Emitter::visit(ASTWhere *node) {
  throw_error("internal compiler error: 'where' expression was visited in the emitter", node->source_range);
}

void Emitter::emit_tuple(int type_id) {
  auto type = global_get_type(type_id);
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  if (type->tuple_is_emitted) {
    return;
  } else {
    type->tuple_is_emitted = true;
  }
  auto name = to_cpp_string(type);

  (*ss) << "typedef struct " << name << " {";
  auto info = type->get_info()->as<TupleTypeInfo>();
  for (int i = 0; i < info->types.size(); ++i) {
    auto type = global_get_type(info->types[i]);
    if (type->is_kind(TYPE_FUNCTION)) {
      (*ss) << get_declaration_type_signature_and_identifier("$" + std::to_string(i), type) << ";\n";
    } else {
      auto name = to_cpp_string(type);
      (*ss) << name << " $" << std::to_string(i) << ";\n";
    }
  }
  (*ss) << "} " << name << ";\n";
}

void Emitter::visit(ASTSize_Of *node) {
  (*ss) << "sizeof(";
  node->target_type->accept(this);
  (*ss) << ")";
}

void Emitter::visit(ASTImport *node) {}

void Emitter::call_operator_overload(const SourceRange &range, Type *left_ty, OperationKind operation, TType op,
                                     ASTExpr *left, ASTExpr *right) {
  auto call = ASTCall{};
  auto dot = ASTDotExpr{};
  dot.base = left;
  dot.member_name = get_operator_overload_name(op, operation);
  call.function = &dot;
  auto args = ASTArguments{};
  if (right) {
    args.arguments = {right};
  }
  call.arguments = &args;
  dot.source_range = range;
  call.arguments->source_range = range;
  call.source_range = range;
  call.accept(&typer);
  call.accept(this);
}

Emitter::Emitter(Context &context, Typer &type_visitor) : typer(type_visitor), ctx(context) { ss = &code; }

void Emitter::visit(ASTModule *node) {}
