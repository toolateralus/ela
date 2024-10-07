#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <functional>
#include <jstl/containers/vector.hpp>
#include <sstream>
#include <string>

std::any EmitVisitor::visit(ASTWhile *node) {
  emit_line_directive(node);
  (*ss) << indent() << "while ";
  if (node->condition.is_not_null()) {
    (*ss) << "(";
    node->condition.get()->accept(this);
    (*ss) << ")";
  } else {
    (*ss) << "(true)";
  }
  node->block->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTElse *node) {
  emit_line_directive(node);
  (*ss) << " else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTIf *node) {
  emit_line_directive(node);
  (*ss) << indent() << "if (";
  node->condition->accept(this);
  (*ss) << ")";
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTFor *node) {
  emit_line_directive(node);
  const auto emit_range_based = [&] {
    auto v = node->value.range_based;

    switch (v.value_semantic) {
    case VALUE_SEMANTIC_COPY: {
      (*ss) << "auto ";
      v.target->accept(this);
      (*ss) << " : ";
      v.collection->accept(this);
    } break;
    case VALUE_SEMANTIC_POINTER: {
      (*ss) << "auto* ";
      v.target->accept(this);
      (*ss) << " = ";
      v.collection->accept(this);
      (*ss) << ".begin(); ";
      v.target->accept(this);
      (*ss) << "!= ";
      v.collection->accept(this);
      (*ss) << ".end();";
      v.target->accept(this);
      (*ss) << "++";

    } break;
    }
  };

  const auto emit_c_style = [&] {
    auto v = node->value.c_style;
    v.decl->accept(this);
    semicolon();
    space();
    v.condition->accept(this);
    semicolon();
    space();
    v.increment->accept(this);
  };

  auto old_scope = ctx.scope;
  ctx.set_scope(node->block->scope);
  (*ss) << indent() << "for (";
  switch (node->tag) {
  case ASTFor::RangeBased:
    emit_range_based();
    break;
  case ASTFor::CStyle:
    emit_c_style();
    break;
  }
  (*ss) << ")";
  node->block->accept(this);
  ctx.set_scope(old_scope);
  return {};
}

std::any EmitVisitor::visit(ASTBreak *node) {
  emit_line_directive(node);
  indented("break");
  return {};
}

std::any EmitVisitor::visit(ASTContinue *node) {
  emit_line_directive(node);
  indented("continue");
  return {};
}

std::any EmitVisitor::visit(ASTReturn *node) {
  emit_line_directive(node);
  indented("return");
  if (node->expression.is_not_null()) {
    space();
    node->expression.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTArguments *node) {
  (*ss) << "(";
  for (int i = 0; i < node->arguments.size(); ++i) {
    node->arguments[i]->accept(this);
    if (i != node->arguments.size() - 1) {
      (*ss) << ", ";
    }
  }
  (*ss) << ")";
  return {};
}

std::any EmitVisitor::visit(ASTType *node) {

  auto type = global_get_type(node->resolved_type);

  // For reflection
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    int pointed_to_ty =
        std::any_cast<int>(node->pointing_to.get()->accept(&type_visitor));
    (*ss) << to_type_struct(global_get_type(pointed_to_ty), ctx);
    return {};
  }

  auto type_string = to_cpp_string(type);
  (*ss) << type_string;
  return {};
}

std::any EmitVisitor::visit(ASTCall *node) {
  (*ss) << node->name.value;
  node->arguments->accept(this);
  return {};
}

void EmitVisitor::interpolate_string(ASTLiteral *node) {
  if (node->value.empty()) {
    throw_warning(
        "using an empty interpolated string causes memory leaks right now.",
        node->source_range);
    (*ss) << "string(\"\")"; // !BUG: fix this. this will cause memory leaks.
    return;
  }

  if (!import_set.contains("/usr/local/lib/ela/core.ela")) {
    throw_error("You must '#import core' before you use interpolated strings "
                "temporarily, due to a dependency on sprintf.",
                node->source_range);
  }

  std::string str;
  auto get_format_str = [&](int type_id) {
    auto type = global_get_type(type_id);
    type = global_get_type(type->get_true_type());

    if (type->is_kind(TYPE_STRUCT)) {
      return "%s";
    }
    if (type->id == charptr_type() ||
        (type->get_base() == "string" && type->get_ext().has_no_extensions())) {
      return "%s";
    }
    if (type->get_ext().is_pointer()) {
      return "%p";
    }
    if (type->is_kind(TYPE_SCALAR)) {
      if (type->id == s8_type() || type->id == s16_type() ||
          type->id == s32_type() || type->id == u8_type() ||
          type->id == u16_type() || type->id == u32_type() ||
          type->id == int_type()) {
        return "%d";
      } else if (type->id == s64_type() || type->id == u64_type()) {
        return "%ld";
      } else if (type->id == float_type() || type->id == float32_type()) {
        return "%f";
      } else if (type->id == float64_type()) {
        return "%lf";
      } else if (type->id == bool_type()) {
        return "%d";
      }
    }
    throw_error(std::format("Cannot deduce a format specifier for interpolated "
                            "string. type: {}",
                            type->to_string()),
                node->source_range);
  };

  auto replace_next_brace_pair = [&](std::string &in,
                                     const std::string &replacement) {
    auto start = in.find('{');
    if (start != std::string::npos) {
      auto end = in.find('}', start);
      if (end != std::string::npos) {
        in.replace(start, end - start + 1, replacement);
      }
    }
  };

  for (const auto &value : node->interpolated_values) {
    auto type_id = std::any_cast<int>(value->accept(&type_visitor));
    std::string format_specifier = get_format_str(type_id);
    replace_next_brace_pair(node->value, format_specifier);
  }

  (*ss) << "[&] -> string { char* buf = new char[1024];\nsprintf(buf, \""
        << node->value << "\",";

  for (const auto &value : node->interpolated_values) {
    auto type_id = std::any_cast<int>(value->accept(&type_visitor));
    auto type = global_get_type(type_id);
    if (type->get_base() == "string" && type->get_ext().has_no_extensions()) {
      value->accept(this);
      (*ss) << ".data";
    } else if (type->is_kind(TYPE_STRUCT)) {
      auto info = static_cast<StructTypeInfo *>(type->get_info());
      if (info->scope->lookup("to_string")) {
        value->accept(this);
        (*ss) << ".to_string()";
      }
    } else {
      value->accept(this);
    }
    if (value != node->interpolated_values.back()) {
      (*ss) << ", ";
    }
  }

  (*ss) << ");\n auto str = string(); str.data = buf; str.length = "
           "strlen(buf); return str; }()";
}

std::any EmitVisitor::visit(ASTLiteral *node) {
  auto type = global_get_type(std::any_cast<int>(node->accept(&type_visitor)))
                  ->to_string();
  std::string output;
  switch (node->tag) {
  case ASTLiteral::InterpolatedString: {
    interpolate_string(node);
    return {};
  }
  case ASTLiteral::Null:
    (*ss) << "(std::nullptr_t)nullptr";
    return {};
  case ASTLiteral::String:
    output = std::format("\"{}\"", node->value);
    break;
  case ASTLiteral::RawString:
    output = std::format("R\"__({})__\"", node->value);
    break;
  case ASTLiteral::Float:
    if (std::any_cast<int>(node->accept(&type_visitor)) != float64_type()) {
      output = node->value + "f";
    } else {
      output = node->value;
    }
    break;
  case ASTLiteral::Char:
    output = '\'' + std::to_string(std::atoi(node->value.c_str())) + '\'';
    break;
  case ASTLiteral::Integer:
    output = node->value;
    break;
  case ASTLiteral::Bool:
    output = node->value;
    break;
  }
  (*ss) << '(' << type << ')' << output;
  return {};
}

std::any EmitVisitor::visit(ASTIdentifier *node) {
  (*ss) << node->value.value;
  return {};
}

std::any EmitVisitor::visit(ASTUnaryExpr *node) {
  if (node->op.type == TType::Sub) {
    auto type = global_get_type(
                    std::any_cast<int>(node->operand->accept(&type_visitor)))
                    ->to_string();
    (*ss) << '(' << type << ')';
  }
  // (*ss) << '(' << node->op.value;
  (*ss) << node->op.value;
  node->operand->accept(this);
  // (*ss) << ")";
  return {};
}

std::any EmitVisitor::visit(ASTBinExpr *node) {
  // type inference assignment.
  if (node->op.type == TType::ColonEquals) {
    if (node->right->is_constexpr()) {
      (*ss) << "const";
    }
    auto id = std::any_cast<int>(node->right->accept(&type_visitor));
    auto type = global_get_type(id);
    (*ss) << to_cpp_string(type) << ' ';
    node->left->accept(this);
    (*ss) << " = ";
    node->right->accept(this);
    return {};
  }

  // SIMPLIFY(Josh) We probably don't want to always parenthesize every single
  // expression. We can just have a table of which operators need custom
  // precedence 9/30/2024, 10:20:00 AM
  (*ss) << "(";
  auto left = node->left->accept(this);
  space();
  (*ss) << node->op.value;

  if (node->op.type == TType::Assign) {
    auto type = global_get_type(node->resolved_type);
    auto isptr = type->get_ext().is_pointer(1);
    if (isptr)
      (*ss) << "(" << to_cpp_string(type) << ")";
  }

  space();
  auto right = node->right->accept(this);
  (*ss) << ")";
  return {};
}

std::any EmitVisitor::visit(ASTExprStatement *node) {
  emit_line_directive(node);
  (*ss) << indent();
  node->expression->accept(this);
  return {};
}

// TODO: remove me, add explicit casting, at least for non-void pointers.
// I don't mind implicit casting to void*/u8*
void EmitVisitor::cast_pointers_implicit(ASTDeclaration *&node) {
  auto type = global_get_type(node->type->resolved_type);
  if (type->get_ext().is_pointer(1))
    (*ss) << "(" << to_cpp_string(type) << ")";
}

void EmitVisitor::get_declaration_type_signature_and_identifier(
    ASTDeclaration *&node, Type *&type) {
  {
    std::stringstream tss;
    std::vector<Nullable<ASTExpr>> array_sizes = type->get_ext().array_sizes;
    tss << type->get_base();

    if (!type->get_ext().is_fixed_sized_array()) {
      tss << node->name.value << ' ';
    }

    bool emitted_iden = false;
    for (const auto ext : type->get_ext().extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        auto size = array_sizes.back();
        array_sizes.pop_back();
        if (size.is_null()) {
          std::string current = tss.str();
          tss.str("");
          tss.clear();
          tss << "_array<" << current << ">";
        } else {
          auto old = this->ss;
          this->ss = &tss;
          if (!emitted_iden) {
            emitted_iden = true;
            tss << ' ' << node->name.value;
          }
          tss << "[";
          size.get()->accept(this);
          tss << "]";
          this->ss = old;
        }
      }
      if (ext == TYPE_EXT_POINTER) {
        tss << "*";
      }
    }
    (*ss) << tss.str();
  }
}
std::any EmitVisitor::visit(ASTDeclaration *node) {
  emit_line_directive(node);
  auto type = global_get_type(node->type->resolved_type);
  auto symbol = ctx.scope->local_lookup(node->name.value);
  if (symbol && (symbol->flags & SYMBOL_WAS_MUTATED) == 0 &&
      !ctx.scope->is_struct_or_union_scope && !type->get_ext().is_pointer()) {
    (*ss) << "const ";
  }

  if (type->get_ext().is_fixed_sized_array()) {
    get_declaration_type_signature_and_identifier(node, type);

    if (node->value.is_not_null()) {
      node->value.get()->accept(this);
    } else if (emit_default_init) {

      bool cancelled = false;
      std::string init = "{";
      for (int i = 0; i < type->get_ext().array_sizes[0]; ++i) {
        auto elem = type->get_element_type();
        auto ty = global_get_type(elem);
        // * We never emit initializers for these sub arrays.
        // TODO: find a way to actually zero initialize all these fixed buffers
        // without hacky lambdas everywhere.
        if (ty->get_ext().is_fixed_sized_array()) {
          cancelled = true;
          break;
        }
        init += " " + to_cpp_string(ty) + "(),";
      }
      if (!cancelled) {
        init.pop_back();
        init += "}";
        (*ss) << init;
      }
    }
  } else {
    node->type->accept(this);
    space();
    (*ss) << node->name.value;
    space();
    if (node->value.is_not_null()) {
      (*ss) << " = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      (*ss) << "{}";
    }
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamDecl *node) {
  auto type = global_get_type(node->type->resolved_type);
  node->type->accept(this);
  (*ss) << ' ' << node->name;
  if (node->default_value.is_not_null() && emit_default_args) {
    (*ss) << " = ";
    node->default_value.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamsDecl *node) {
  (*ss) << "(";
  int i = 0;
  for (const auto &param : node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      (*ss) << ", ";
    }
    ++i;
  }

  if (current_func_decl.is_not_null() &&
      (current_func_decl.get()->flags & FUNCTION_IS_VARARGS) != 0) {
    (*ss) << ", ...)";
    return {};
  }

  (*ss) << ")";
  return {};
}

void EmitVisitor::emit_forward_declaration(ASTFunctionDeclaration *node) {

  if ((node->flags & FUNCTION_IS_METHOD) != 0) {
    return;
  }

  emit_default_args = true;
  use_header();

  if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
    (*ss) << "extern \"C\" ";
  }

  node->return_type->accept(this);
  (*ss) << ' ' << node->name.value << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
  use_code();
  emit_default_args = false;
}

void EmitVisitor::emit_local_function(ASTFunctionDeclaration *node) {
  // Right now we just always do a closure on local lambda functions.
  // This probably isn't desirable for simple in-out functions
  (*ss) << indent() << "auto " << node->name.value << " = [&]";
  node->params->accept(this);
  (*ss) << " -> ";
  node->return_type->accept(this);
  if (node->block.is_null()) {
    throw_error("local function cannot be #foreign", node->source_range);
  }
  node->block.get()->accept(this);
}

void EmitVisitor::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name.value == "main") {
    throw_error("main function cannot be foreign", node->source_range);
  }

  use_header();
  (*ss) << "extern \"C\" ";
  (*ss) << get_cpp_scalar_type(node->return_type->resolved_type);
  space();
  (*ss) << node->name.value << '(';
  for (const auto &param : node->params->params) {
    (*ss) << get_cpp_scalar_type(param->type->resolved_type);
    if (param != node->params->params.back()) {
      (*ss) << ", ";
    }
  }

  if ((node->flags & FUNCTION_IS_VARARGS) != 0) {
    (*ss) << ", ...);";
  } else {
    (*ss) << ");";
  }

  use_code();
}

std::any EmitVisitor::visit(ASTFunctionDeclaration *node) {
  auto emit_various_function_declarations = [&] {
    // local function
    if (!ctx.scope->is_struct_or_union_scope && ctx.scope != root_scope &&
        (node->flags & FUNCTION_IS_METHOD) == 0) {
      emit_local_function(node);
      return;
    }

    if ((node->flags & FUNCTION_IS_DTOR) != 0) {
      auto name = current_struct_decl ? current_struct_decl.get()->type->base
                                      : current_union_decl.get()->type->base;
      (*ss) << '~' << name;
      node->params->accept(this);
      if (!node->block) {
        throw_error("Cannot forward declare a constructor", node->source_range);
      }
      node->block.get()->accept(this);
      return;
    }

    if ((node->flags & FUNCTION_IS_CTOR) != 0) {
      auto name = current_struct_decl ? current_struct_decl.get()->type->base
                                      : current_union_decl.get()->type->base;
      (*ss) << name;

      auto is_copy_ctor =
          node->params->params.size() == 1 &&
          node->params->params[0]->type->resolved_type ==
              (current_struct_decl
                   ? current_struct_decl.get()->type->resolved_type
                   : current_union_decl.get()->type->resolved_type);

      if (is_copy_ctor) {
        (*ss) << "(" << name << " &" << node->params->params[0]->name << ")";
        node->block.get()->accept(this);
        (*ss) << ";\n";
        (*ss) << name;
        (*ss) << "(const " << name << " &" << node->params->params[0]->name
              << ")";
        node->block.get()->accept(this);
        (*ss) << ";\n";
        return;

      } else {
        node->params->accept(this);
      }

      if (!node->block) {
        throw_error("Cannot forward declare a constructor", node->source_range);
      }
      node->block.get()->accept(this);
      return;
    }

    if ((node->flags & FUNCTION_IS_OPERATOR) != 0) {

      auto op = node->name;
      emit_warnings_or_errors_for_operator_overloads(op.type,
                                                     node->source_range);

      if (op.type == TType::LParen) {
        op.value = "()";
      }

      if (op.type == TType::LBrace) {
        op.value = "[]";
      }

      if (op.type == TType::Dot) {
        op.value = "->";
      }
      node->return_type->accept(this);
      (*ss) << " operator " << op.value;
      node->params->accept(this);

      if ((node->flags & FUNCTION_IS_MUTATING) == 0) {
        (*ss) << " const ";
      }

      node->block.get()->accept(this);
      return;
    }

    if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
      (*ss) << "extern \"C\" ";
    }

    // we override main's return value to allow compilation without explicitly
    // returning int from main.
    if (node->name.value == "main") {
      (*ss) << "int";
    } else {
      node->return_type->accept(this);
    }

    // emit parameter signature && name.
    (*ss) << " " + node->name.value;
    node->params->accept(this);

    if ((node->flags & FUNCTION_IS_METHOD) != 0) {
      if ((node->flags & FUNCTION_IS_MUTATING) == 0) {
        (*ss) << " const ";
      }
    }

    // the function's block would only be null in a #foreign function
    if (node->block.is_not_null())
      node->block.get()->accept(this);

    // main functions do not get forward declared.
    if (node->name.value != "main") {
      emit_forward_declaration(node);
    }
  };

  emit_line_directive(node);

  auto last_func_decl = current_func_decl;
  current_func_decl = node;

  auto test_flag = get_compilation_flag("test");

  Defer deferred = {[&]() { current_func_decl = last_func_decl; }};

  // this also happens to emit the test boilerplate that bootstraps it into the
  // test runner, if applicable.
  if (!should_emit_function(this, node, test_flag)) {
    return {};
  }

  auto symbol = ctx.scope->lookup(node->name.value);

  // for #foreign declarations
  if (node->meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN) {
    emit_foreign_function(node);
    return {};
  }

  // emit a bunch of generic funcz
  if ((node->flags & FUNCTION_IS_GENERIC) != 0) {
    auto variants = node->generic_types;
    for (const auto variant : variants) {
      auto variant_fun_ty = global_get_type(variant);
      auto fun_info =
          static_cast<FunctionTypeInfo *>(variant_fun_ty->get_info());

      auto &params = node->params->params;
      for (int i = 0; i < fun_info->params_len; ++i) {
        auto &param = params[i];
        if (param->is_type_param) {
          type_alias_map[param->type->base] = fun_info->parameter_types[i];
        }
      }

      if (node->has_generic_return_type) {
        type_alias_map[node->return_type->base] = fun_info->return_type;
      }

      type_visitor.ignore_generic_functions = false;
      node->accept(&type_visitor);
      type_visitor.ignore_generic_functions = true;

      // emit the new function
      emit_various_function_declarations();

      // delete teh dam aliasez
      for (int i = 0; i < fun_info->params_len; ++i) {
        if (node->params->params[i]->is_type_param) {
          type_alias_map.erase(node->params->params[i]->type->base);
        }
      }

      if (node->has_generic_return_type) {
        type_alias_map.erase(node->return_type->base);
      }
    }
  } else {
    emit_various_function_declarations();
  }

  return {};
}

std::any EmitVisitor::visit(ASTBlock *node) {
  emit_line_directive(node);
  (*ss) << (" {\n");
  indentLevel++;
  ctx.set_scope(node->scope);
  for (const auto &statement : node->statements) {
    if (dynamic_cast<ASTDeclaration *>(statement)) {
      indented("");
    }

    statement->accept(this);

    semicolon();
    newline();
  }
  indentLevel--;
  indented("}");
  ctx.exit_scope();
  return {};
}

std::any EmitVisitor::visit(ASTProgram *node) {
  emit_line_directive(node);
  const auto testing = get_compilation_flag("test");
  header << "#include \"/usr/local/lib/ela/boilerplate.hpp\"\n";
  use_code();
  for (const auto &statement : node->statements) {
    statement->accept(this);
    semicolon();
    newline();
  }
  if (testing) {
    auto test_init = test_functions.str();
    if (test_init.ends_with(',')) {
      test_init.pop_back();
    }

    code << std::format("__COMPILER_GENERATED_TEST tests[{}] = {}\n", num_tests,
                        "{ " + test_init + " };");

    code << "__TEST_RUNNER_MAIN;";
  }
  // Emit runtime reflection type info for requested types
  {
    std::stringstream type_info{};
    for (const auto &str : ctx.type_info_strings) {
      type_info << str << ";\n";
    }
    header << std::format("static Type** _type_info = []{{ Type **_type_info = "
                          "new Type*[{}]; {}; return _type_info; }}();",
                          num_types, type_info.str());
  }

  return {};
}

std::any EmitVisitor::visit(ASTStructDeclaration *node) {
  emit_line_directive(node);
  auto type = global_get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->get_info());

  current_struct_decl = node;

  Defer deferred([&] { current_struct_decl = nullptr; });

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0) {
    header << "struct " << node->type->base << ";\n";
    return {};
  }

  const auto is_anonymous = (info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0;

  ctx.set_scope(node->scope);

  if (!node->generic_parameters.empty()) {
    auto emit = [&] {
      for (const GenericParameter &param : node->generic_parameters) {
        if (param.is_named) {
          (*ss) << param.type->base;
          (*ss) << " " << param.name;
        } else {
          (*ss) << "class ";
          (*ss) << param.type->base;
        }
        if (param != node->generic_parameters.back()) (*ss) << ", ";
      }
    };

    (*ss) << "template<";
    emit();
    (*ss) << ">\n";
    use_header();
    (*ss) << "template<";
    emit();
    (*ss) << ">\n";
    use_code();
  }

  if (!is_anonymous) {
    (*ss) << "struct " << node->type->base << "{\n";
    header << "struct " << node->type->base << ";\n";
  } else {
    (*ss) << "struct {\n";
  }
  indentLevel++;

  for (const auto &decl : node->fields) {
    indented("");
    decl->accept(this);
    semicolon();
    newline();
  }
  for (const auto &method : node->methods) {
    indented("");
    method->accept(this);
    semicolon();
    newline();
  }
  ctx.exit_scope();

  (*ss) << "};\n";
  indentLevel--;
  return {};
}

std::any EmitVisitor::visit(ASTDotExpr *node) {
  auto left = std::any_cast<int>(node->left->accept(&type_visitor));
  auto left_ty = global_get_type(left);

  auto op = ".";

  if (!left_ty->get_ext().extensions.empty() &&
      left_ty->get_ext().extensions.back() == TYPE_EXT_POINTER)
    op = "->";

  // TODO: remove this hack to get array length, or at least make a nicer system
  // for getting properties of builtin types that aren't considered structs by
  // the langauge.
  if (left_ty->get_ext().is_array() &&
      !left_ty->get_ext().is_fixed_sized_array()) {
    auto right = dynamic_cast<ASTIdentifier *>(node->right);
    if (right && right->value.value == "length") {
      node->left->accept(this);
      (*ss) << op;
      node->right->accept(this);
      return {};
    }
    if (right && right->value.value == "data") {
      node->left->accept(this);
      (*ss) << op;
      node->right->accept(this);
      return {};
    }
  }

  if (left_ty->is_kind(TYPE_ENUM)) {
    (*ss) << left_ty->get_base() << '_';
    node->right->accept(this);
    return {};
  }

  if (left_ty->kind != TYPE_STRUCT && left_ty->kind != TYPE_UNION) {
    throw_error(
        std::format("cannot use dot expr on non-struct currently, got {}",
                    left_ty->to_string()),
        node->source_range);
  }

  Scope *scope;
  if (auto info = dynamic_cast<StructTypeInfo *>(left_ty->get_info())) {
    scope = info->scope;
  } else if (auto info = dynamic_cast<UnionTypeInfo *>(left_ty->get_info())) {
    scope = info->scope;
  }

  auto previous_scope = ctx.scope;

  auto prev_parent = scope->parent;
  bool root = !within_dot_expression;

  if (prev_parent && root) {
    scope->parent = previous_scope;
    within_dot_expression = true;
  }

  node->left->accept(this);

  ctx.set_scope(scope);
  (*ss) << (op);
  node->right->accept(this);
  ctx.set_scope(previous_scope);

  if (prev_parent && root) {
    within_dot_expression = false;
    scope->parent = prev_parent;
  }

  return {};
}

std::any EmitVisitor::visit(ASTMake *node) {
  auto type = global_get_type(node->type_arg->resolved_type);
  if (node->kind == MAKE_CAST) {
    if (node->arguments->arguments.empty()) {
      throw_error("cannot create a pointer currently with #make. it only casts "
                  "pointers.",
                  node->source_range);
    }
    (*ss) << "(" << to_cpp_string(type) << ")";
    node->arguments->arguments[0]->accept(this);
  } else if (node->kind == MAKE_CTOR || node->kind == MAKE_COPY_CTOR) {
    (*ss) << to_cpp_string(type);
    node->arguments->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTSubscript *node) {
  node->left->accept(this);
  (*ss) << '[';
  node->subscript->accept(this);
  (*ss) << ']';
  return {};
}

std::any EmitVisitor::visit(ASTInitializerList *node) {
  (*ss) << "{";
  for (const auto &expr : node->expressions) {
    expr->accept(this);
    if (expr != node->expressions.back()) {
      (*ss) << ", ";
    }
  }
  (*ss) << "}";
  return {};
}

std::any EmitVisitor::visit(ASTEnumDeclaration *node) {
  emit_line_directive(node);
  use_header();
  std::string iden;
  if (node->is_flags) {
    iden = "enum ";
  } else {
    iden = "enum ";
  }
  auto type = global_get_type(node->element_type);
  (*ss) << iden << node->type->base << " : " << type->get_base() << "{\n";
  int i = 0;
  auto get_next_index = [&] {
    int value;
    if (node->is_flags) {
      value = 1 << i;
    } else {
      value = i;
    }
    i++;
    return value;
  };
  int n = 0;

  auto type_name = node->type->base;
  for (const auto &[key, value] : node->key_values) {
    (*ss) << type_name << '_' << key;
    (*ss) << " = ";
    if (value.is_not_null()) {
      value.get()->accept(this);
    } else {
      (*ss) << std::to_string(get_next_index());
    }
    if (n != node->key_values.size() - 1) {
      (*ss) << ",\n";
    }
    n++;
  }
  (*ss) << "\n};";
  use_code();
  return {};
}

std::any EmitVisitor::visit(ASTUnionDeclaration *node) {
  // FEATURE(Josh) 10/1/2024, 12:58:56 PM  implement sum types
  use_header();
  (*ss) << "union " << node->name.value << ";\n";
  use_code();

  (*ss) << "union " << node->name.value << "{\n";
  current_union_decl = node;
  Defer _([&] { current_union_decl = nullptr; });
  indentLevel++;
  ctx.set_scope(node->scope);
  emit_default_init = false;

  // DOCUMENT THIS:
  // we will always default-initialize the first field in a union type.
  // this may not pan out, but ideally unions would only be used for stuff like
  // vector3's and ASTnodes.
  for (const auto &field : node->fields) {
    if (field == node->fields.front()) {
      emit_default_init = true;
      field->accept(this);
      emit_default_init = false;
    } else {
      field->accept(this);
    }

    (*ss) << ";\n";
  }
  for (const auto &method : node->methods) {
    method->accept(this);
    (*ss) << ";\n";
  }
  for (const auto &_struct : node->structs) {
    _struct->accept(this);
    (*ss) << ";\n";
  }
  emit_default_init = true;
  indentLevel--;
  ctx.exit_scope();
  (*ss) << "};\n";
  return {};
}

std::any EmitVisitor::visit(ASTAllocate *node) {
  switch (node->kind) {
  case ASTAllocate::New: {
    auto ptr_type = global_get_type(node->type.get()->resolved_type);
    (*ss) << "new ";
    auto ext = ptr_type->get_ext();
    ext.extensions.pop_back();
    auto nonptr = global_find_type_id(ptr_type->get_base(), ext);
    auto nonptr_ty = global_get_type(nonptr);
    auto str = to_cpp_string(nonptr_ty);
    (*ss) << str;
    if (!node->arguments) {
      (*ss) << "()";
    } else {
      node->arguments.get()->accept(this);
    }
  } break;
  case ASTAllocate::Delete:
    auto args = node->arguments.get()->arguments;
    for (const auto &arg : args) {
      (*ss) << "delete ";
      arg->accept(this);
      (*ss) << ";\n" << indent();
      arg->accept(this);
      (*ss) << " = nullptr";
      (*ss) << ";\n" << indent();
    }
    break;
  }
  return {};
}

bool EmitVisitor::should_emit_function(EmitVisitor *visitor,
                                       ASTFunctionDeclaration *node,
                                       bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "__COMPILER_GENERATED_TEST(\""
                            << node->name.value << "\", " << node->name.value
                            << "),";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->name.value == "main") {
    return false;
  }
  return true;
}

std::string EmitVisitor::to_cpp_string(const TypeExt &ext,
                                       const std::string &base) {
  std::vector<Nullable<ASTExpr>> array_sizes = ext.array_sizes;
  std::stringstream ss;
  ss << base;
  for (const auto ext : ext.extensions) {
    if (ext == TYPE_EXT_ARRAY) {
      auto size = array_sizes.back();
      array_sizes.pop_back();
      if (size.is_null()) {
        std::string current = ss.str();
        ss.str("");
        ss.clear();
        ss << "_array<" << current << ">";
      } else {
        auto old = this->ss;
        this->ss = &ss;
        ss << "[";
        size.get()->accept(this);
        ss << "]";
        this->ss = old;
      }
    }
    if (ext == TYPE_EXT_POINTER) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string EmitVisitor::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";
  if (type->get_base() == "s64")
    name = "int64_t";
  else if (type->get_base() == "s32")
    name = "int32_t";
  else if (type->get_base() == "s16")
    name = "int16_t";
  else if (type->get_base() == "s8")
    name = "int8_t";
  else if (type->get_base() == "u64")
    name = "size_t";
  else if (type->get_base() == "u32")
    name = "uint32_t";
  else if (type->get_base() == "u16")
    name = "uint16_t";
  else if (type->get_base() == "char" && type->get_ext().is_pointer(1))
    name = "const char";
  else if (type->get_base() == "u8" && type->get_ext().is_pointer(1))
    name = "char";
  else if (type->get_base() == "u8")
    name = "uint8_t";
  else if (type->get_base() == "float32")
    name = "float";
  else if (type->get_base() == "float64")
    name = "double";
  else if (type->get_base() == "float")
    name = "float";
  else if (type->get_base() == "int")
    name = "int";
  else if (type->get_base() == "char")
    name = "char";
  else if (type->get_base() == "bool")
    name = "bool";
  else if (type->get_base() == "void")
    name = "void";
  else {
    return to_cpp_string(type);
  }

  if (type->get_ext().has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->get_ext(), name);
}

std::string EmitVisitor::to_cpp_string(Type *type) {
  type = global_get_type(type->get_true_type());
  
  auto output = std::string{};
  switch (type->kind) {
  case TYPE_SCALAR:
  case TYPE_STRUCT:
    output= to_cpp_string(type->get_ext(), type->get_base());
    break;
  case TYPE_FUNCTION: {
    if (type->get_ext().has_extensions()) {
      return to_cpp_string(type->get_ext(), type->get_base());
    }
    auto info = static_cast<FunctionTypeInfo *>(type->get_info());
    auto ret = to_cpp_string(global_get_type(info->return_type));
    std::string params = "(";
    for (int i = 0; i < info->params_len; ++i) {
      params += to_cpp_string(global_get_type(info->parameter_types[i]));
      if (i != info->params_len - 1) {
        params += ", ";
      }
    }
    params += ")";
    output= ret + params;
    break;
  }
  case TYPE_ENUM:
    output= type->get_base();
    break;
  case TYPE_UNION:
    output= to_cpp_string(type->get_ext(), type->get_base());
    break;
  }
  // TODO: re-implement generic structs.
  // auto args = type->get_ext().generic_arguments;
  // if (!args.empty()) {
  //   std::stringstream ss;
  //   auto old = this->ss;
  //   this->ss = &ss;
  //   ss << "<";
  //   for (const auto &arg: args) {
  //     arg->accept(this);
  //     if (arg != args.back()) ss << ", ";
  //   }
  //   ss << ">";
  //   this->ss = old;
  //   output += ss.str();
  // }
  return output;
}
