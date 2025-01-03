#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <any>
#include <functional>

#include <sstream>
#include <string>

/*
  TODO:
   This entire visitor needs a huge cleanup. there's some absolutely terrible
  code in here and it's super messy. ? However it works xD
*/

constexpr auto TYPE_FLAGS_INTEGER = 2;
constexpr auto TYPE_FLAGS_FLOAT = 4;
constexpr auto TYPE_FLAGS_BOOL = 8;
constexpr auto TYPE_FLAGS_STRING = 16;
constexpr auto TYPE_FLAGS_STRUCT = 32;
constexpr auto TYPE_FLAGS_UNION = 64;
constexpr auto TYPE_FLAGS_ENUM = 128;
constexpr auto TYPE_FLAGS_TUPLE = 256;

constexpr auto TYPE_FLAGS_ARRAY = 512;
constexpr auto TYPE_FLAGS_FIXED_ARRAY = 1024;
constexpr auto TYPE_FLAGS_MAP = 2048;
constexpr auto TYPE_FLAGS_FUNCTION = 4096;
constexpr auto TYPE_FLAGS_POINTER = 8192;

constexpr auto TYPE_FLAGS_SIGNED = 16384;
constexpr auto TYPE_FLAGS_UNSIGNED = 32768;

std::any Emitter::visit(ASTWhile *node) {
  emit_condition_block(node, "while", node->condition, node->block);
  return {};
}
std::any Emitter::visit(ASTIf *node) {
  emit_condition_block(node, "if", node->condition, node->block);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  return {};
}
std::any Emitter::visit(ASTElse *node) {
  emit_line_directive(node);
  (*ss) << " else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return {};
}
std::any Emitter::visit(ASTFor *node) {
  emit_line_directive(node);

  auto old_scope = ctx.scope;
  ctx.set_scope(node->block->scope);
  (*ss) << indent() << "for (";

  if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
    // Emit old-style for loop for pointer semantics
    (*ss) << "auto* ";
    node->iden->accept(this);
    (*ss) << " = ";
    node->range->accept(this);
    (*ss) << ".begin(); ";
    node->iden->accept(this);
    (*ss) << " != ";
    node->range->accept(this);
    (*ss) << ".end(); ";
    node->iden->accept(this);
    (*ss) << "++";
  } else {
    // Emit range-based for loop for copy semantics
    (*ss) << "auto ";
    node->iden->accept(this);
    (*ss) << " : ";
    node->range->accept(this);
  }

  (*ss) << ")";
  node->block->accept(this);
  ctx.set_scope(old_scope);
  return {};
}
std::any Emitter::visit(ASTBreak *node) {
  emit_line_directive(node);
  indented("break");
  return {};
}
std::any Emitter::visit(ASTContinue *node) {
  emit_line_directive(node);
  indented("continue");
  return {};
}
std::any Emitter::visit(ASTReturn *node) {
  emit_line_directive(node);
  indented("return");
  if (node->expression.is_not_null()) {
    space();
    node->expression.get()->accept(this);
  }
  return {};
}
std::any Emitter::visit(ASTArguments *node) {
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
std::any Emitter::visit(ASTType *node) {
  auto type = global_get_type(node->resolved_type);

  if (!type) {
    throw_error("Internal compiler error: an ASTType* was null when trying to "
                "find it's type in the table",
                node->source_range);
  }

  // For reflection
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    int pointed_to_ty =
        std::any_cast<int>(node->pointing_to.get()->accept(&type_visitor));
    (*ss) << to_type_struct(global_get_type(pointed_to_ty), ctx);
    return {};
  }

  if (type->is_kind(TYPE_FUNCTION)) {
    emit_function_pointer_type_string(type);
    return {};
  }

  if (type->is_kind(TYPE_ENUM)) {
    auto enum_info = static_cast<EnumTypeInfo *>(type->get_info());
    auto elem_ty = global_get_type(enum_info->element_type);
    (*ss) << to_cpp_string(elem_ty);
    return {};
  }

  auto type_string = to_cpp_string(type);
  (*ss) << type_string;
  return {};
}
std::any Emitter::visit(ASTCall *node) {
  node->function->accept(this);
  node->arguments->accept(this);
  return {};
}
std::any Emitter::visit(ASTLiteral *node) {
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
      output = node->value.get_str() + "f";
    } else {
      output = node->value.get_str();
    }
    break;
  case ASTLiteral::Char:
    output = '\'' + node->value.get_str() + '\'';
    break;
  case ASTLiteral::Integer:
    output = node->value.get_str();
    break;
  case ASTLiteral::Bool:
    output = node->value.get_str();
    break;
  }
  (*ss) << '(' << type << ')' << output;
  return {};
}
std::any Emitter::visit(ASTIdentifier *node) {
  (*ss) << node->value.get_str();
  return {};
}
std::any Emitter::visit(ASTUnaryExpr *node) {
  if (node->op.type == TType::Sub) {
    auto type = global_get_type(
                    std::any_cast<int>(node->operand->accept(&type_visitor)))
                    ->to_string();
    (*ss) << '(' << type << ')';
  }
  auto left_type = std::any_cast<int>(node->operand->accept(&type_visitor));
  auto type = global_get_type(left_type);
  if (node->op.type == TType::BitwiseNot && type->get_ext().is_array()) {
    node->operand->accept(this);
    (*ss) << ".pop()";
    return {};
  }
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
  return {};
}
std::any Emitter::visit(ASTBinExpr *node) {
  if (node->op.type == TType::Erase) {
    node->left->accept(this);
    (*ss) << ".erase(";
    node->right->accept(this);
    (*ss) << ");\n";
    return {};
  }

  // CLEANUP(Josh) 10/14/2024, 10:13:23 AM
  // Get rid of these janky operators and just use methods.
  if (node->op.type == TType::Concat) {
    node->left->accept(this);
    (*ss) << ".push(";
    node->right->accept(this);
    (*ss) << ");\n";
    return {};
  }

  // SIMPLIFY(Josh) We probably don't want to always parenthesize every single
  // expression. We can just have a table of which operators need custom
  // precedence 9/30/2024, 10:20:00 AM
  (*ss) << "(";
  auto left = node->left->accept(this);
  space();
  (*ss) << node->op.value.get_str();

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
std::any Emitter::visit(ASTExprStatement *node) {
  emit_line_directive(node);
  (*ss) << indent();
  node->expression->accept(this);
  return {};
}
std::any Emitter::visit(ASTDeclaration *node) {
  emit_line_directive(node);
  auto type = global_get_type(node->type->resolved_type);
  auto symbol = ctx.scope->local_lookup(node->name.value);

  if (type->is_kind(TYPE_FUNCTION)) {
    get_declaration_type_signature_and_identifier(node->name.value.get_str(),
                                                  type);
    if (node->value.is_not_null()) {
      (*ss) << " = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    }
    return {};
  }

  const auto is_freestanding =
      compile_command.compilation_flags.contains("-ffreestanding") ||
      compile_command.compilation_flags.contains("-nostdlib");

  if (node->is_bitfield) {
    node->type->accept(this);
    space();
    (*ss) << node->name.value.get_str();
    space();
    (*ss) << ": " << node->bitsize.value.get_str();
    if (node->value.is_not_null()) {
      (*ss) << " = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      (*ss) << "{}";
    }
    return {};
  }

  if (type->get_ext().is_fixed_sized_array()) {
    get_declaration_type_signature_and_identifier(node->name.value.get_str(),
                                                  type);
    if (node->value.is_not_null()) {
      node->value.get()->accept(this);
    } else if (emit_default_init && !type->get_ext().is_pointer() &&
               !is_freestanding) {
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
    return {};
  }

  // TODO: in the type checker, we should assert that this isn't a static member
  // / function.
  if (node->is_static) {
    (*ss) << "static ";
  } else if (node->is_constexpr) {
    (*ss) << "static constexpr ";
  }

  node->type->accept(this);
  space();
  (*ss) << node->name.value.get_str();
  space();
  if (node->value.is_not_null()) {
    (*ss) << " = ";
    cast_pointers_implicit(node);
    node->value.get()->accept(this);
  } else if (emit_default_init) {
    (*ss) << "{}";
  }

  return {};
}
void Emitter::emit_forward_declaration(ASTFunctionDeclaration *node) {
  if ((node->flags & FUNCTION_IS_METHOD) != 0 ||
      (node->flags & FUNCTION_IS_STATIC) != 0) {
    return;
  }

  emit_default_args = true;

  if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
    (*ss) << "extern \"C\" ";
  }

  node->return_type->accept(this);
  (*ss) << ' ' << node->name.value.get_str() << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
  emit_default_args = false;
}
void Emitter::emit_local_function(ASTFunctionDeclaration *node) {
  // Right now we just always do a closure on local lambda functions.
  // This probably isn't desirable for simple in-out functions
  (*ss) << indent() << "auto " << node->name.value.get_str() << " = [&]";
  node->params->accept(this);
  (*ss) << " -> ";
  node->return_type->accept(this);
  if (node->block.is_null()) {
    throw_error("local function cannot be #foreign", node->source_range);
  }
  node->block.get()->accept(this);
}
void Emitter::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name.value == "main") {
    throw_error("main function cannot be foreign", node->source_range);
  }

  (*ss) << "extern \"C\" ";
  (*ss) << get_cpp_scalar_type(node->return_type->resolved_type);
  space();
  (*ss) << node->name.value.get_str() << '(';
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
}
std::any Emitter::visit(ASTFunctionDeclaration *node) {
  auto emit_various_function_declarations = [&] {
    if ((node->flags & FUNCTION_IS_STATIC) != 0) {
      (*ss) << "static ";
    }

    if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
      emit_forward_declaration(node);
      return;
    }

    // local function
    if (!ctx.scope->is_struct_or_union_scope && ctx.scope != root_scope &&
        (node->flags & FUNCTION_IS_METHOD) == 0) {
      emit_local_function(node);
      return;
    }

    if ((node->flags & FUNCTION_IS_DTOR) != 0) {
      auto type_id = current_struct_decl
                         ? current_struct_decl.get()->type->resolved_type
                         : current_union_decl.get()->type->resolved_type;
      auto type = global_get_type(type_id);
      auto name = type->get_base().get_str();
      // TODO: in the typer, we need to assert that a destructor never takes
      // parameters.
      (*ss) << '~' << name << "()";
      if (!node->block)
        throw_error("Cannot forward declare a constructor", node->source_range);
      node->block.get()->accept(this);
      return;
    }

    if ((node->flags & FUNCTION_IS_CTOR) != 0) {
      auto type_id = current_struct_decl
                         ? current_struct_decl.get()->type->resolved_type
                         : current_union_decl.get()->type->resolved_type;
      auto type = global_get_type(type_id);
      auto name = type->get_base().get_str();
      (*ss) << name;

      auto is_copy_ctor =
          node->params->params.size() == 1 &&
          node->params->params[0]->type->resolved_type ==
              (current_struct_decl
                   ? current_struct_decl.get()->type->resolved_type
                   : current_union_decl.get()->type->resolved_type);

      if (is_copy_ctor) {
        (*ss) << "(" << name << " &" << node->params->params[0]->name.get_str()
              << ")";
        node->block.get()->accept(this);
        (*ss) << ";\n";
        (*ss) << name;
        (*ss) << "(const " << name << " &"
              << node->params->params[0]->name.get_str() << ")";
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
      (*ss) << " operator " << op.value.get_str();

      if (op.type == TType::Increment || op.type == TType::Decrement) {
        (*ss) << "(int)";
      } else {
        node->params->accept(this);
      }
      node->block.get()->accept(this);
      return;
    }

    if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
      (*ss) << "extern \"C\" ";
    }

    static auto use_stdlib =
        !compile_command.compilation_flags.contains("-nostdlib") &&
        !compile_command.compilation_flags.contains("-ffreestanding");

    // we override main's return value to allow compilation without explicitly
    // returning int from main.
    if (node->name.value == "main" && use_stdlib) {
      has_user_defined_main = true;
      node->return_type->accept(this);
      (*ss) << " __ela_main_()"; // We use Env::args() to get args now.
      node->block.get()->accept(this);
      // This is for a main function when we're not using the stdlib, so you
      // just take a C style main, since you don't have dynamic arrays.
    } else if (node->name.value == "main") {
      (*ss) << "int ";
      node->return_type->accept(this);
      // emit parameter signature && name.
      (*ss) << " " + node->name.value.get_str();
      node->params->accept(this);
      // the function's block would only be null in a #foreign function
      if (node->block.is_not_null())
        node->block.get()->accept(this);
    } else {
      node->return_type->accept(this);
      // emit parameter signature && name.
      (*ss) << " " + node->name.value.get_str();
      node->params->accept(this);
      // the function's block would only be null in a #foreign function
      if (node->block.is_not_null())
        node->block.get()->accept(this);
    }
  };

  emit_line_directive(node);

  auto last_func_decl = current_func_decl;
  current_func_decl = node;

  auto test_flag = compile_command.has_flag("test");

  Defer deferred = {[&]() { current_func_decl = last_func_decl; }};

  // this also happens to emit the test boilerplate that bootstraps it into the
  // test runner, if applicable.
  if (!should_emit_function(this, node, test_flag)) {
    return {};
  }

  auto symbol = ctx.scope->lookup(node->name.value);

  if (node->meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN) {
    emit_foreign_function(node);
    return {};
  }

  emit_various_function_declarations();

  return {};
}
std::any Emitter::visit(ASTStructDeclaration *node) {
  emit_line_directive(node);
  auto type = global_get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo *>(type->get_info());

  current_struct_decl = node;

  Defer _defer([&] { current_struct_decl = nullptr; });

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED || node->is_fwd_decl) != 0) {
    if (node->is_extern)
      *ss << "extern \"C\" ";
    *ss << "struct " << type->get_base().get_str() << ";\n";
    return {};
  }

  ctx.set_scope(node->scope);

  if ((info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << "struct {\n";
  } else {
    if (node->is_extern) {
      (*ss) << "extern \"C\" ";
    }
    (*ss) << "struct " << type->get_base().get_str() << "{\n";
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

std::any Emitter::visit(ASTEnumDeclaration *node) {
  emit_line_directive(node);
  auto type_name = node->type->base.get_str();
  int n = 0;
  (*ss) << "enum " << type_name << " {\n";
  for (const auto &[key, value] : node->key_values) {
    (*ss) << type_name << "_" << key.get_str();
    if (value.is_not_null()) {
      (*ss) << " = ";
      value.get()->accept(this);
    } else if (node->is_flags) {
      (*ss) << " = ";
      (*ss) << std::to_string(1 << n);
    }
    if (n != node->key_values.size() - 1) {
      (*ss) << ",\n";
    }
    n++;
  }
  (*ss) << "};\n";
  return {};
}

std::any Emitter::visit(ASTUnionDeclaration *node) {
  if (node->is_fwd_decl) {
    (*ss) << "union " << node->name.value.get_str() << ";\n";
    return {};
  }

  Defer _([&] { current_union_decl = nullptr; });
  current_union_decl = node;

  (*ss) << "union " << node->name.value.get_str() << "{\n";

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
std::any Emitter::visit(ASTParamDecl *node) {
  auto type = global_get_type(node->type->resolved_type);

  if (type->is_kind(TYPE_FUNCTION)) {
    get_declaration_type_signature_and_identifier(node->name.get_str(), type);
  } else {
    node->type->accept(this);
    auto sym = ctx.scope->local_lookup(node->name);
    if (sym && type->is_kind(TYPE_STRUCT) || type->is_kind(TYPE_UNION)) {
      (*ss) << " const& " << node->name.get_str();
    } else {
      (*ss) << ' ' << node->name.get_str();
    }
  }

  if (node->default_value.is_not_null() && emit_default_args) {
    (*ss) << " = ";
    node->default_value.get()->accept(this);
  }
  return {};
}
std::any Emitter::visit(ASTParamsDecl *node) {
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
std::any Emitter::visit(ASTBlock *node) {
  emit_line_directive(node);
  (*ss) << (" {\n");
  indentLevel++;
  ctx.set_scope(node->scope);
  for (const auto &statement : node->statements) {
    if (statement->get_node_type() == AST_NODE_DECLARATION) {
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
std::any Emitter::visit(ASTProgram *node) {
  emit_line_directive(node);

  const auto testing = compile_command.has_flag("test");
  const auto use_stdlib =
      !compile_command.compilation_flags.contains("-nostdlib") &&
      !compile_command.compilation_flags.contains("-ffreestanding");

  if (use_stdlib) {
    code << "#define USE_STD_LIB 1\n";
  } else {
    for (int i = 0; i < type_table.size(); ++i) {
      Type *type = &type_table[i];
      TypeExt ext = type->get_ext();

      if (type->get_base() == "Field")
        continue;
      if (type->get_base() == "Element")
        continue;
      if (type->get_base() == "Type")
        continue;

      if (ext.is_array() && !ext.is_fixed_sized_array()) {
        throw_error(
            std::format(
                "You cannot use dynamic arrays in a freestanding or nostdlib "
                "environment, due to lack of allocators. Type: {}",
                type->to_string()),
            {});
      }
      if (ext.is_map()) {
        throw_error(
            std::format("You cannot use maps in a freestanding or nostdlib "
                        "environment, due to lack of allocators. Type: {}",
                        type->to_string()),
            {});
      }
    }

    if (compile_command.has_flag("test")) {
      throw_error("You cannot use unit tests in a freestanding or nostlib "
                  "environment due to lack of exception handling",
                  {});
    }
  }

  code << "#include \"/usr/local/lib/ela/boilerplate.hpp\"\n";

  if (use_stdlib)
    code << "extern Type **_type_info;\n";

  if (testing) {
    code << "#define TESTING\n";
  }

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

    // deploy the array of test struct wrappers.
    code << std::format("__COMPILER_GENERATED_TEST tests[{}] = {}\n", num_tests,
                        "{ " + test_init + " };");

    // use the test runner main macro.
    code << "__TEST_RUNNER_MAIN;";
  } else {

    if (has_user_defined_main && use_stdlib)
      code << R"__(
int main (int argc, char** argv) {
  // Bootstrap the env
  for (int i = 0; i < argc; ++i) {
    Env::args().push(string(argv[i]));
  }
  // call our user's main function.
  __ela_main_();
}
)__";
  }

  // Emit runtime reflection type info for requested types, only when we have
  // actually requested runtime type information.
  if (!ctx.type_info_strings.empty() && use_stdlib) {

    std::stringstream type_info{};
    for (const auto &str : ctx.type_info_strings) {
      type_info << str.get_str() << ";\n";
    }
    code << std::format("Type **_type_info = new Type*[{}];\n"
                        "auto __ts_init_func_result__ = []{{\n"
                        "  {};\n"
                        "  return 0;\n"
                        "}}();\n",
                        type_table.size(), type_info.str());
  }

  if (!use_stdlib && !ctx.type_info_strings.empty()) {
    throw_error("You cannot use runtime type reflection in a freestanding or "
                "nostdlib environment, due to a lack of allocators. To compare "
                "types, use #typeid.",
                {});
  }

  return {};
}
std::any Emitter::visit(ASTDotExpr *node) {
  auto base_ty_id = std::any_cast<int>(node->base->accept(&type_visitor));
  auto base_ty = global_get_type(base_ty_id);

  auto op = ".";

  if (!base_ty->get_ext().extensions.empty() &&
      base_ty->get_ext().extensions.back() == TYPE_EXT_POINTER) {
    op = "->";
  }

  node->base->accept(this);
  (*ss) << op << node->member_name.get_str();

  return {};
}
std::any Emitter::visit(ASTMake *node) {
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
std::any Emitter::visit(ASTSubscript *node) {
  node->left->accept(this);
  (*ss) << '[';
  node->subscript->accept(this);
  (*ss) << ']';
  return {};
}
std::any Emitter::visit(ASTInitializerList *node) {
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
std::any Emitter::visit(ASTAllocate *node) {
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
      switch (arg->get_node_type()) {
      case AST_NODE_IDENTIFIER:
      case AST_NODE_DOT_EXPR:
      case AST_NODE_SCOPE_RESOLUTION:
      case AST_NODE_SUBSCRIPT: {
        arg->accept(this);
        (*ss) << " = nullptr";
        (*ss) << ";\n" << indent();
      } break;
      default: {
        throw_warning(WarningNonNullDeletedPointer, "Cannot set deleted pointer to null. Delete as a "
                      "variable to silence this.",
                      arg->source_range);
      }
      }
    }
    break;
  }
  return {};
}
std::any Emitter::visit(ASTRange *node) {
  (*ss) << "Range(";
  node->left->accept(this);
  (*ss) << ", ";
  node->right->accept(this);
  (*ss) << ")";
  return {};
}
std::any Emitter::visit(ASTSwitch *node) {

  if (!node->is_statement) {
    (*ss) << "[&] ->";
    auto type = global_get_type(node->return_type);
    (*ss) << to_cpp_string(type);
    (*ss) << "{\n";
    ;
  }

  auto emit_switch_case = [&](ASTExpr *target, const SwitchCase &_case,
                              bool first) {
    if (!first) {
      (*ss) << " else ";
    }
    emit_line_directive(target);
    (*ss) << " if (";
    target->accept(this);
    (*ss) << " == ";
    _case.expression->accept(this);
    (*ss) << ") ";
    emit_line_directive(_case.block);
    _case.block->accept(this);
  };

  bool first = true;

  for (const auto &_case : node->cases) {
    emit_switch_case(node->target, _case, first);
    first = false;
  }

  if (!node->is_statement) {
    (*ss) << "else {";

    auto type = global_get_type(node->return_type);
    (*ss) << "return " << to_cpp_string(type) << "{};";
    (*ss) << "\n}\n";

    (*ss) << "}()";
  }

  return {};
}
std::any Emitter::visit(ASTTuple *node) {
  (*ss) << "_tuple(";
  for (const auto &value : node->values) {
    value->accept(this);
    if (value != node->values.back())
      (*ss) << ", ";
  }
  (*ss) << ")";
  return {};
}
std::any Emitter::visit(ASTTupleDeconstruction *node) {
  (*ss) << "auto [";
  for (auto &iden : node->idens) {
    (*ss) << iden->value.get_str();
    if (iden != node->idens.back()) {
      (*ss) << ", ";
    }
  }
  (*ss) << "] = ";
  node->right->accept(this);
  (*ss) << ";\n";
  return {};
};

// TODO: remove me, add explicit casting, at least for non-void pointers.
// I don't mind implicit casting to void*/u8*
void Emitter::cast_pointers_implicit(ASTDeclaration *&node) {
  auto type = global_get_type(node->type->resolved_type);
  if (type->get_ext().is_pointer(1))
    (*ss) << "(" << to_cpp_string(type) << ")";
}

void Emitter::emit_function_pointer_dynamic_array_declaration(
    const std::string &type_string, const std::string &name, Type *type) {
  //? type string will equal something like void(*)();
  //? we need to emit _array<void(*)()>
  //? or possibley _array<_array<void(*)()>*>
  auto string = to_cpp_string(type->get_ext(), type_string);
  if (!string.contains(' ' + name + ' ')) {
    (*ss) << string << ' ' << name;
  } else {
    (*ss) << string;
  }
}

void Emitter::get_declaration_type_signature_and_identifier(
    const std::string &name, Type *type) {
  StringBuilder tss;

  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    auto &ext = type->get_ext();
    if (ext.is_fixed_sized_array()) {
      identifier += ext.to_string();
    } else if (ext.is_array()) {
      StringBuilder my_ss;
      auto old = ss;
      ss = &my_ss;
      emit_function_pointer_type_string(type, nullptr);
      ss = old;
      auto type_string = my_ss.str();
      emit_function_pointer_dynamic_array_declaration(type_string, name, type);
      return;
    }

    emit_function_pointer_type_string(type, &identifier);
    return;
  }

  auto array_sizes = type->get_ext().array_sizes;
  tss << type->get_base().get_str();
  if (!type->get_ext().is_fixed_sized_array()) {
    tss << name << ' ';
  }
  bool emitted_iden = false;
  for (const auto ext : type->get_ext().extensions) {
    if (ext == TYPE_EXT_ARRAY) {
      auto size = array_sizes.back();
      array_sizes.pop_back();
      if (size.is_null()) {
        std::string current = tss.str();
        tss.clear();
        tss << "_array<" << current << ">";
      } else {
        auto old = this->ss;
        this->ss = &tss;
        if (!emitted_iden) {
          emitted_iden = true;
          tss << ' ' << name;
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

void Emitter::interpolate_string(ASTLiteral *node) {
  if (node->value.get_str().empty()) {
    throw_warning(WarningEmptyStringInterpolation, "Empty interpolated string.", node->source_range);
    (*ss) << "string()";
    return;
  }

  std::string str;
  auto get_format_str = [&](int type_id) {
    auto type = global_get_type(type_id);
    type = global_get_type(type->get_true_type());

    // We just assume that the type-checker has validated that this struct has a
    // to_string() function
    if (type->is_kind(TYPE_STRUCT) || type->is_kind(TYPE_UNION)) {
      return "%s";
    }
    if (type->id == charptr_type() ||
        (type->get_base() == "string" && type->get_ext().has_no_extensions())) {
      return "%s";
    }
    if (type->get_ext().is_pointer()) {
      return "%p";
    }
    if (type->id == bool_type()) {
      return "%s";
    }
    if (type->is_kind(TYPE_SCALAR)) {
      if (type->id == char_type()) {
        return "%c";
      } else if (type->id == s8_type() || type->id == s16_type() ||
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
    if (type->is_kind(TYPE_ENUM)) {
      return "%d";
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
    auto v = node->value.get_str();
    replace_next_brace_pair(v, format_specifier);
    node->value = v;
  }

  (*ss) << "[&] -> string { char* buf = new char[1024];\nsprintf(buf, \""
        << node->value.get_str() << "\",";

  auto interpolate_value = [&](ASTExpr *value) {
    auto type_id = std::any_cast<int>(value->accept(&type_visitor));
    auto type = global_get_type(type_id);

    auto interpolate_to_string_struct_union = [&](Scope *scope) {
      auto sym = scope->lookup("to_string");

      if (!sym)
        throw_error(
            "Cannot use a struct in an interpolated string without defining a "
            "`to_string` function that returns either a char* or a string",
            value->source_range);

      auto sym_ty = static_cast<FunctionTypeInfo *>(
          global_get_type(sym->type_id)->get_info());

      auto return_ty = global_get_type(sym_ty->return_type);
      value->accept(this);

      auto &extensions = type->get_ext();
      if (extensions.has_extensions() &&
          extensions.extensions.back() == TYPE_EXT_POINTER) {
        (*ss) << "->to_string()";
      } else {
        (*ss) << ".to_string()";
      }

      if (return_ty->get_base() == "string" &&
          return_ty->get_ext().has_no_extensions()) {
        (*ss) << ".data";
      }
    };

    if (type->id == bool_type()) {
      value->accept(this);
      (*ss) << " ? \"true\" : \"false\"";
    } else if (type->get_base() == "string" &&
               type->get_ext().has_no_extensions()) {
      value->accept(this);
      (*ss) << ".data";
    } else if (type->is_kind(TYPE_STRUCT)) {
      auto info = static_cast<StructTypeInfo *>(type->get_info());
      interpolate_to_string_struct_union(info->scope);
    } else if (type->is_kind(TYPE_UNION)) {
      auto info = static_cast<UnionTypeInfo *>(type->get_info());
      interpolate_to_string_struct_union(info->scope);
    } else {
      value->accept(this);
    }
    if (value != node->interpolated_values.back())
      (*ss) << ", ";
  };

  for (const auto &value : node->interpolated_values)
    interpolate_value(value);

  (*ss) << ");\n auto str = string(); str.data = buf; str.length = "
           "strlen(buf); return str; }()";
}

// Identifier may contain a fixed buffer size like name[30] due to the way
// function pointers have to work in C.
void Emitter::emit_function_pointer_type_string(
    Type *type, Nullable<std::string> identifier) {

  auto type_prefix = std::string{"*"};

  // TODO:
  // ! We need to be able to take function pointers to member methods.
  // ! It's certainly possible but the syntax is really hard to wrangle.
  if (current_struct_decl.is_not_null()) {
    auto t = global_get_type(current_struct_decl.get()->type->resolved_type);
    auto info = static_cast<StructTypeInfo *>(t->get_info());
    for (const auto &[name, sym] : info->scope->symbols) {
      auto sym_ty = global_get_type(sym.type_id);

      if ((sym.flags & SYMBOL_IS_FUNCTION) != 0 && sym_ty == type &&
          identifier.is_not_null() && *identifier.get() == name.get_str()) {
        type_prefix = to_cpp_string(t) + "::*";
        *identifier.get() = to_cpp_string(t) + *identifier.get();
      }
    }
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error("Internal compiler error: tried to get a function pointer from "
                "a non-function type",
                {});
  }

  auto info = static_cast<FunctionTypeInfo *>(type->get_info());
  auto return_type = global_get_type(info->return_type);

  (*ss) << to_cpp_string(return_type) << "(" << type_prefix;

  if (identifier) {
    (*ss) << *identifier.get();
  }

  (*ss) << ")(";

  for (int i = 0; i < info->params_len; ++i) {
    auto type = global_get_type(info->parameter_types[i]);
    (*ss) << to_cpp_string(type);
    if (i != info->params_len - 1) {
      (*ss) << ", ";
    }
  }
  (*ss) << ")";
}

std::string Emitter::get_field_struct(const std::string &name, Type *type,
                                      Type *parent_type, Context &context) {
  std::stringstream ss;
  ss << "new Field { " << std::format(".name = \"{}\", ", name)
     << std::format(".type = {}, ", to_type_struct(type, context));

  if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    ss << std::format(".offset = offsetof({}, {})",
                      parent_type->get_base().get_str(), name);
  }

  ss << " }";
  return ss.str();
}

std::string Emitter::get_elements_function(Type *type) {
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
                       to_cpp_string(element_type),
                       to_type_struct(element_type, ctx));
  } else {
    auto old = this->ss;
    auto ss = StringBuilder{};
    this->ss = &ss;
    type->get_ext().array_sizes.back().get()->accept(this);
    auto length = ss.str();
    this->ss = old;
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
                       to_cpp_string(element_type), length,
                       to_type_struct(element_type, ctx));
  }
}

std::string get_type_flags(Type *type) {
  int kind_flags = 0;
  switch (type->kind) {
  case TYPE_SCALAR: {
    auto sint = type->id == int_type() || type->id == s8_type() ||
                type->id == s16_type() || type->id == s32_type() ||
                type->id == s64_type();

    auto uint = type->id == u8_type() || type->id == u16_type() ||
                type->id == u32_type() || type->id == u64_type();

    auto floating_pt = type->id == float32_type() ||
                       type->id == float64_type() || type->id == float_type();
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
    } else if (type->get_base() == "string") {
      kind_flags |= TYPE_FLAGS_STRING;
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
  case TYPE_UNION:
    kind_flags = TYPE_FLAGS_UNION;
    break;
  case TYPE_TUPLE:
    kind_flags = TYPE_FLAGS_TUPLE;
    break;
  }
  for (const auto &ext : type->get_ext().extensions) {
    switch (ext) {
    case TYPE_EXT_POINTER:
      kind_flags |= TYPE_FLAGS_POINTER;
      break;
    case TYPE_EXT_ARRAY:
      if (type->get_ext().is_fixed_sized_array()) {
        kind_flags |= TYPE_FLAGS_FIXED_ARRAY;
      } else {
        kind_flags |= TYPE_FLAGS_ARRAY;
      }

      break;
    case TYPE_EXT_MAP:
      kind_flags |= TYPE_FLAGS_MAP;
      break;
    }
  }
  return ".flags = " + std::to_string(kind_flags) + "\n";
}

std::string Emitter::get_type_struct(Type *type, int id, Context &context,
                                     const std::string &fields) {
  std::stringstream ss;

  auto kind = 0;

  ss << "_type_info[" << id << "] = new Type {" << ".id = " << id << ", "
     << ".name = \"" << type->to_string() << "\", ";

  if (!type->is_kind(TYPE_ENUM))
    ss << ".size = sizeof(" << to_cpp_string(type) << "), ";

  ss << get_type_flags(type) << ",\n"
     << ".fields = " << fields << ",\n";
  if (type->get_ext().is_array()) {
    ss << get_elements_function(type) << ",\n";
  }
  ss << " };";
  context.type_info_strings.push_back(ss.str());
  return std::format("_type_info[{}]", id);
}

std::string Emitter::to_type_struct(Type *type, Context &context) {
  auto id = type->get_true_type();
  auto new_type = global_get_type(id);

  if (new_type != type) {
    type = new_type;
  }

  static bool *type_cache = [] {
    auto arr = new bool[type_table.size()];
    memset(arr, 0, type_table.size());
    return arr;
  }();

  if (type_cache[id]) {
    return std::format("_type_info[{}]", id);
  }

  type_cache[id] = true;

  // TODO:
  // ! This needs serious improvement to be really really useful. It's a great
  // starting point, ! but it could be far better.

  std::stringstream fields_ss;
  if (type->kind == TYPE_STRUCT) {
    auto info = static_cast<StructTypeInfo *>(type->get_info());
    if (info->scope->symbols.empty()) {
      return get_type_struct(type, id, context, "{}");
    }
    fields_ss << "{";
    int count = info->scope->symbols.size();
    int it = 0;
    for (const auto &tuple : info->scope->symbols) {
      auto &[name, sym] = tuple;

      if (name == "this")
        continue;

      auto t = global_get_type(sym.type_id);
      // TODO: handle methods separately
      if (t->is_kind(TYPE_FUNCTION) || (sym.flags & SYMBOL_IS_FUNCTION))
        continue;

      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << get_field_struct(name.get_str(), t, type, context);
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else if (type->kind == TYPE_UNION) {
    auto info = static_cast<UnionTypeInfo *>(type->get_info());
    if (info->scope->symbols.empty()) {
      return get_type_struct(type, id, context, "{}");
    }
    fields_ss << "{";
    int count = info->scope->symbols.size();
    int it = 0;
    for (const auto &tuple : info->scope->symbols) {
      auto &[name, sym] = tuple;

      if (name == "this")
        continue;

      auto t = global_get_type(sym.type_id);
      // TODO: handle methods separately
      if (t->is_kind(TYPE_FUNCTION) || (sym.flags & SYMBOL_IS_FUNCTION))
        continue;

      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << get_field_struct(name.get_str(), t, type, context);
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else if (type->kind == TYPE_ENUM) {
    auto info = static_cast<EnumTypeInfo *>(type->get_info());
    if (info->keys.empty()) {
      return get_type_struct(type, id, context, "{}");
    }
    fields_ss << "{";
    int count = info->keys.size();
    int it = 0;
    for (const auto &name : info->keys) {
      auto t = global_get_type(s32_type());
      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << get_field_struct(name.get_str(), t, type, context);
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else {
    return get_type_struct(type, id, context, "{}");
  }

  return get_type_struct(type, id, context, fields_ss.str());
}

bool Emitter::should_emit_function(Emitter *visitor,
                                   ASTFunctionDeclaration *node,
                                   bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "__COMPILER_GENERATED_TEST(\""
                            << node->name.value.get_str() << "\", "
                            << node->name.value.get_str() << "),";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->name.value == "main") {
    return false;
  }
  return true;
}

std::string Emitter::to_cpp_string(const TypeExt &extensions,
                                   const std::string &base) {
  std::vector<Nullable<ASTExpr>> array_sizes = extensions.array_sizes;
  StringBuilder ss;
  if (base == "c_string") {
    ss << "const char *";
  } else {
    ss << base;
  }

  for (const auto ext : extensions.extensions) {
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
    if (ext == TYPE_EXT_MAP) {
      std::string current = ss.str();
      auto key_string = to_cpp_string(global_get_type(extensions.key_type));
      ss.str("");
      ss.clear();
      ss << "_map<" << key_string << ", " << current << ">";
    }
  }
  return ss.str();
}

std::string Emitter::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";
  if (id == c_string_type()) {
    name = "const char";
  } else if (type->get_base() == "u8" && type->get_ext().is_pointer(1)) {
    name = "char";
  } else {
    return to_cpp_string(type);
  }

  if (type->get_ext().has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->get_ext(), name);
}

std::string Emitter::to_cpp_string(Type *type) {
  type = global_get_type(type->get_true_type());

  auto output = std::string{};
  switch (type->kind) {
  case TYPE_SCALAR:
  case TYPE_STRUCT:
    output = to_cpp_string(type->get_ext(), type->get_base().get_str());
    break;
  case TYPE_FUNCTION: {
    StringBuilder my_ss;
    auto old = ss;
    ss = &my_ss;
    emit_function_pointer_type_string(type);
    ss = old;
    return my_ss.str();
  }
  case TYPE_ENUM:
    output = type->get_base().get_str();
    break;
  case TYPE_UNION:
    output = to_cpp_string(type->get_ext(), type->get_base().get_str());
    break;
  case TYPE_TUPLE: {
    auto info = static_cast<TupleTypeInfo *>(type->get_info());
    output = "_tuple" + get_tuple_type_name(info->types).get_str();
    output = to_cpp_string(type->get_ext(), output);
    break;
  }
  }
  return output;
}

void Emitter::emit_condition_block(ASTNode *node, const std::string &keyword,
                                   Nullable<ASTExpr> condition,
                                   Nullable<ASTBlock> block) {
  emit_line_directive(node);
  (*ss) << indent() << keyword << " ";
  if (condition.is_not_null()) {
    (*ss) << "(";
    condition.get()->accept(this);
    (*ss) << ")";
  } else {
    (*ss) << "(true)";
  }
  block.get()->accept(this);
}

std::any Emitter::visit(ASTScopeResolution *node) {

  bool emitted = false;

  if (node->base->get_node_type() == AST_NODE_TYPE) {
    auto t = static_cast<ASTType *>(node->base);
    auto type = global_get_type(t->resolved_type);
    if (type->is_kind(TYPE_ENUM)) {
      (*ss) << type->get_base().get_str();
      emitted = true;
    }
  }

  if (!emitted) {
    node->base->accept(this);
  }

  auto op = "::";
  auto type_id = std::any_cast<int>(node->base->accept(&type_visitor));
  if (global_get_type(type_id)->is_kind(TYPE_ENUM)) {
    op = "_";
  }
  (*ss) << op << node->member_name.get_str();
  return {};
}
