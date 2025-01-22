
#include <functional>
#include <ostream>
#include <sstream>
#include <string>

#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
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

constexpr auto TYPE_FLAGS_ARRAY = 1 << 9;
constexpr auto TYPE_FLAGS_FUNCTION = 1 << 10;
constexpr auto TYPE_FLAGS_POINTER = 1 << 11;

constexpr auto TYPE_FLAGS_SIGNED = 1 << 12;
constexpr auto TYPE_FLAGS_UNSIGNED = 1 << 13;

void Emitter::visit(ASTWhile *node) {
  defer_blocks.push_back({{}, DEFER_BLOCK_TYPE_LOOP});
  emit_condition_block(node, "while", node->condition, node->block);
  emit_deferred_statements(DEFER_BLOCK_TYPE_LOOP);
  defer_blocks.pop_back();
  return;
}
void Emitter::visit(ASTIf *node) {
  emit_condition_block(node, "if", node->condition, node->block);
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

  static int depth = 0;
  std::string range_unique_id = "$_range_id" + std::to_string(depth);
  std::string unique_id = "$_loop_id" + std::to_string(depth);
  depth++;

  Defer _defer([]{
    depth--;
  });

  (*ss) << indent() << "{\n";
  indent_level++;

  std::string range_type_str = to_cpp_string(global_get_type(node->range_type));
  (*ss) << indent() << range_type_str << " " << range_unique_id << " = ";
  node->range->accept(this);
  (*ss) << ";\n";

  std::string iterable_type_str = to_cpp_string(global_get_type(node->iterable_type));
  (*ss) << indent() << iterable_type_str << " " << unique_id << " = ";
  if (node->is_enumerable) {
    (*ss) << range_type_str << "_enumerator(&" << range_unique_id << ");\n";
  } else {
    (*ss) << range_type_str << "_iter(&" << range_unique_id << ");\n";
  }

  (*ss) << indent() << "while (!" << iterable_type_str << "_done(&" << unique_id << ")) {\n";
  indent_level++;

  std::string identifier_type_str = to_cpp_string(global_get_type(node->identifier_type));
  (*ss) << indent() << identifier_type_str << " ";
  
  node->iden->accept(this);
  (*ss) << " = ";
  if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
    (*ss) << iterable_type_str << "_current(&" << unique_id << ");\n";
  } else if (node->is_enumerable) { // Enumerables don't use the * value semantic.
    (*ss) << iterable_type_str << "_current(&" << unique_id << ");\n";
  } else {
    (*ss) << "*" << iterable_type_str << "_current(&" << unique_id << ");\n";
  }

  // this MUST happen before the block or continue will cause a permanent hangup!!!
  (*ss) << indent() << iterable_type_str << "_next(&" << unique_id << ");\n";

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
    bool emitted = false;

    // ! This is a terrible hack. 
    // ! We should have a specific node for functions that take types as arguments
    // ! sizeof(), typeof(), etc etc.
    if (node->arguments[i]->get_node_type() == AST_NODE_IDENTIFIER)  {
      auto iden = static_cast<ASTIdentifier*>(node->arguments[i]);
      auto type = ctx.scope->find_type_id(iden->value, {});
      if (type != -1) {
        emitted = true;
        (*ss) << to_cpp_string(global_get_type(type));
      }
    }

    if (!emitted) {
      node->arguments[i]->accept(this);
    }
    
    if (i != node->arguments.size() - 1) {
      (*ss) << ", ";
    }
  }
  (*ss) << ")";
  return;
}
void Emitter::visit(ASTType *node) {
  auto type = global_get_type(node->resolved_type);
  if (!type) {
    throw_error("internal compiler error: ASTType* resolved to null in emitter.", node->source_range);
  }

  // For reflection
  if (node->kind == ASTType::REFLECTION) {
    (*ss) << to_type_struct(global_get_type(node->pointing_to.get()->resolved_type), ctx);
    return;
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
      throw_error(std::format("Internal Compiler Error: 'get_dot_left_type' encountered an unexpected node, kind {}",
                              (int)node->get_node_type()),
                  node->source_range);
  }
  return Type::invalid_id;
}

void Emitter::visit(ASTCall *node) {
  auto node_type = node->function->get_node_type();
  auto base_symbol = typer.get_symbol(node->function);

  std::vector<int> generic_args;
  for (const auto arg : node->generic_arguments) {
    generic_args.push_back(arg->resolved_type);
  }

  auto symbol = base_symbol.get();
  if (base_symbol && node_type == AST_NODE_DOT_EXPR && symbol->declaring_node.is_not_null() &&
      symbol->declaring_node.get()->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
    auto func = static_cast<ASTFunctionDeclaration *>(symbol->declaring_node.get());
    auto method_call = (func->flags & FUNCTION_IS_METHOD) != 0;
    auto static_method = (func->flags & FUNCTION_IS_STATIC) != 0;

    if (!method_call || static_method) {
      throw_error("cannot call a static method from an instance", node->source_range);
    }

    ASTExpr *base = static_cast<ASTDotExpr *>(node->function)->base;
    Type *function_type = global_get_type(base_symbol.get()->type_id);
    auto param_0_ty = global_get_type(function_type->get_info()->as<FunctionTypeInfo>()->parameter_types[0]);
    auto base_type = global_get_type(get_expr_left_type_sr_dot(node->function));
    if (!base_type) {
      throw_error("Internal compiler error: unable to find method call", node->source_range);
    }
    (*ss) << base_type->get_base().get_str() << "_" << base_symbol.get()->name.get_str();
    (*ss) << mangled_type_args(generic_args);
    (*ss) << "(";

    if (param_0_ty->get_ext().is_pointer() && !base_type->get_ext().is_pointer()) {
      // TODO: add an r-value analyzer, since we can't take a pointer to temporary memory like literals & rvalues.
      (*ss) << "&";
    }
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
    // normal function call, or a static method.
    node->function->accept(this);
    (*ss) << mangled_type_args(generic_args);
    node->arguments->accept(this);
  }

  return;
}
void Emitter::visit(ASTLiteral *node) {
  auto type = to_cpp_string(global_get_type(node->resolved_type));
  std::string output;
  switch (node->tag) {
    case ASTLiteral::InterpolatedString: {
      interpolate_string(node);
      return;
    }
    case ASTLiteral::Null:
      (*ss) << "(std::nullptr_t)nullptr";
      return;
    case ASTLiteral::String:
      output = std::format("\"{}\"", node->value);
      break;
    case ASTLiteral::RawString:
      output = std::format("R\"__({})__\"", node->value);
      break;
    case ASTLiteral::Float:
      if (node->resolved_type != float64_type()) {
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
    // !! THIS IS A TOTAL HACK!!!
    // !! JUST TRYING THIS OUT!!!
    auto function_type = find_operator_overload(node->op.type, left_ty, OPERATION_UNARY);
    auto call = ast_alloc<ASTCall>();
    auto sr = ast_alloc<ASTDotExpr>();
    auto left_id = ast_alloc<ASTIdentifier>();
    sr->base = node->operand;
    sr->member_name = get_operator_overload_name(node->op.type, OPERATION_UNARY);
    call->function = sr;
    call->arguments = ast_alloc<ASTArguments>();
    call->accept(&typer);
    call->accept(this);
    sr->source_range = node->source_range;
    call->arguments->source_range = node->source_range;
    call->source_range = node->source_range;
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
  auto left_ty = global_get_type(node->left->resolved_type);

  if (left_ty && node->is_operator_overload) {
    // !! THIS IS A TOTAL HACK!!!
    // !! JUST TRYING THIS OUT!!!
    auto function_type = find_operator_overload(node->op.type, left_ty, OPERATION_BINARY);
    auto call = ast_alloc<ASTCall>();
    auto sr = ast_alloc<ASTDotExpr>();
    auto left_id = ast_alloc<ASTIdentifier>();
    sr->base = node->left;
    sr->member_name = get_operator_overload_name(node->op.type, OPERATION_BINARY);
    call->function = sr;
    call->arguments = ast_alloc<ASTArguments>();
    call->arguments->arguments = {node->right};
    call->accept(&typer);
    call->accept(this);
    sr->source_range = node->source_range;
    call->arguments->source_range = node->source_range;
    call->source_range = node->source_range;
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

void Emitter::visit(ASTDeclaration *node) {
  emit_line_directive(node);

  if (node->type->resolved_type == Type::invalid_id) {
    throw_error("Internal Compiler Error: type was null upon emitting an ASTDeclaration", node->source_range);
  }

  auto type = global_get_type(node->type->resolved_type);
  auto symbol = ctx.scope->local_lookup(node->name);

  auto handle_initialization = [&]() {
    if (node->value.is_not_null()) {
      (*ss) << " = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      (*ss) << "{}";
    }
  };

  auto old = emit_default_init;
  Defer _([&]{ emit_default_init = old; });
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
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      (*ss) << "{}";
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
  emit_default_args = true;

  if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
    (*ss) << "extern \"C\" ";
  }

  node->return_type->accept(this);
  (*ss) << ' ' << node->name.get_str() << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
  emit_default_args = false;
}
void Emitter::emit_local_function(ASTFunctionDeclaration *node) {
  // Right now we just always do a closure on local lambda functions.
  // This probably isn't desirable for simple in-out functions
  (*ss) << indent() << "auto " << node->name.get_str() << " = [&]";
  node->params->accept(this);
  (*ss) << " -> ";
  node->return_type->accept(this);
  if (node->block.is_null()) {
    throw_error("local function cannot be #foreign", node->source_range);
  }
  node->block.get()->accept(this);
}
void Emitter::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name == "main") {
    throw_error("main function cannot be foreign", node->source_range);
  }

  (*ss) << "extern \"C\" ";
  (*ss) << get_cpp_scalar_type(node->return_type->resolved_type);
  space();
  (*ss) << node->name.get_str() << '(';
  for (const auto &param : node->params->params) {
    (*ss) << get_cpp_scalar_type(param->resolved_type);
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

void Emitter::visit(ASTStructDeclaration *node) {
  if (node->is_emitted) {
    return;
  }
  if (!node->generic_parameters.empty()) {
    for (auto &instantiation : node->generic_instantiations) {
      auto type = global_get_type(instantiation.node->resolved_type);
      static_cast<ASTStructDeclaration *>(instantiation.node)->resolved_type = type->id;
      for (auto type_id : instantiation.arguments) {
        auto type = global_get_type(type_id);
        if (type->declaring_node) {
          type->declaring_node.get()->accept(this);
        }
      }
      instantiation.node->accept(this);
    }
    return;
  }

  for (auto field : node->fields) {
    auto type = global_get_type(field->type->resolved_type);
    if (type->declaring_node) {
      type->declaring_node.get()->accept(this);
    }
  }

  node->is_emitted = true;

  emit_line_directive(node);
  auto type = global_get_type(node->resolved_type);
  auto info = (type->get_info()->as<StructTypeInfo>());

  auto type_tag = node->is_union ? " union " : " struct ";

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED || node->is_fwd_decl) != 0) {
    if (node->is_extern) {
      (*ss) << "extern \"C\" ";
    }
    (*ss) << type_tag << type->get_base().get_str() << ";\n";
    return;
  }

  ctx.set_scope(node->scope);

  if ((info->flags & STRUCT_FLAG_IS_ANONYMOUS) != 0) {
    (*ss) << type_tag;
    (*ss) << "{\n";
  } else {
    if (node->is_extern) {
      (*ss) << "extern \"C\" ";
    }
    (*ss) << type_tag;
    (*ss) << type->get_base().get_str() << "{\n";
  }
  indent_level++;

  auto old = emit_default_init;
  if (node->is_union)
    emit_default_init = false;

  Defer _defer1([&] { emit_default_init = old; });

  for (const auto &subtype : node->subtypes) {
    indented("");
    subtype->accept(this);
    semicolon();
    newline();
  }

  for (const auto &decl : node->fields) {
    indented("");
    decl->accept(this);
    semicolon();
    newline();
  }

  (*ss) << "};\n";
  indent_level--;

  bool has_default_ctor = false;
  bool has_dtor = false;

  ctx.exit_scope();
  return;
}
void Emitter::visit(ASTEnumDeclaration *node) {
  emit_line_directive(node);
  auto type_name = node->name.get_str();
  int n = 0;
  (*ss) << "enum " << type_name << " {\n";
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
  (*ss) << "};\n";
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
    if (node->normal.default_value.is_not_null() && emit_default_args) {
      (*ss) << " = ";
      node->normal.default_value.get()->accept(this);
    }
  } else {
    (*ss) << ' ' << to_cpp_string(type) << " self";
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

  if (!is_freestanding) {
    code << "#define USE_STD_LIB 1\n";
  } else {
    if (compile_command.has_flag("test")) {
      throw_error("You cannot use unit tests in a freestanding or nostlib "
                  "environment due to lack of exception handling",
                  {});
    }
  }

  if (compile_command.has_flag("test-verbose")) {
    code << "#define TEST_VERBOSE;\n";
    std ::cout << "adding TEST_VERBOSE\n";
  }

  code << "#include \"/usr/local/lib/ela/boilerplate.hpp\"\n";

  if (!is_freestanding) {
    code << "extern Type **_type_info;\n";
  }

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
    code << std::format("__COMPILER_GENERATED_TEST tests[{}] = {}\n", num_tests, "{ " + test_init + " };");

    // use the test runner main macro.
    code << "__TEST_RUNNER_MAIN;";
  } else {
    if (has_user_defined_main && !is_freestanding) {
      code << R"__(
int main (int argc, char** argv) {
  Env_initialize(argc, argv);
  __ela_main_();
}
)__";
    } // C calls main() for freestanding
  }

  // Emit runtime reflection type info for requested types, only when we have
  // actually requested runtime type information.
  if (!ctx.type_info_strings.empty() && !is_freestanding) {
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

    code << std::format(R"_(
Type *find_type(string name) {{
  for (size_t i = 0; i < {}; ++i) {{
    Type *type = _type_info[i];
    const char *type_name = type->name;
    const char *name_data = name.data;
    bool match = true;
    while (*type_name && *name_data) {{
      if (*type_name != *name_data) {{
        match = false;
        break;
      }}
      ++type_name;
      ++name_data;
    }}
    if (match && *type_name == '\\0' && *name_data == '\\0') {{
      return type;
    }}
  }}
  return nullptr; // Return nullptr if the type is not found
}}
)_",
                        type_table.size());
  }

  // TODO: if we're freestanding, we should just emit ID's only for #type().
  if (is_freestanding && !ctx.type_info_strings.empty()) {
    throw_error("You cannot use runtime type reflection in a freestanding or "
                "nostdlib environment, due to a lack of allocators. To compare "
                "types, use #typeid.",
                {});
  }

  return;
}
void Emitter::visit(ASTDotExpr *node) {
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = global_get_type(base_ty_id);
  auto op = ".";
  if (base_ty->get_ext().back_type() == TYPE_EXT_POINTER) {
    op = "->";
  }
  node->base->accept(this);
  (*ss) << op << node->member_name.get_str();

  return;
}

void Emitter::visit(ASTSubscript *node) {
  auto left_ty = global_get_type(node->left->resolved_type);
  if (left_ty && node->is_operator_overload) {
    // !! THIS IS A TOTAL HACK!!!
    // !! JUST TRYING THIS OUT!!!
    auto function_type = find_operator_overload(TType::LBrace, left_ty, OPERATION_SUBSCRIPT);
    auto call = ast_alloc<ASTCall>();
    auto sr = ast_alloc<ASTDotExpr>();
    auto left_id = ast_alloc<ASTIdentifier>();
    sr->base = node->left;
    sr->member_name = get_operator_overload_name(TType::LBrace, OPERATION_SUBSCRIPT);
    call->function = sr;
    call->arguments = ast_alloc<ASTArguments>();
    call->arguments->arguments = {node->subscript};
    call->accept(&typer);
    call->accept(this);
    sr->source_range = node->source_range;
    call->arguments->source_range = node->source_range;
    call->source_range = node->source_range;
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
    (*ss) << to_cpp_string(type);
  }
  (*ss) << " {";

  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      (*ss) << "}";
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
      for (const auto &expr : node->values) {
        expr->accept(this);
        if (expr != node->values.back()) {
          (*ss) << ", ";
        }
      }
    } break;
  }
  (*ss) << "}";
  return;
}

void Emitter::visit(ASTRange *node) {
  // TODO: fix this dog crap casting and lack of calculating the span of the range.
  (*ss) << "Range{.begin = (s64)";
  node->left->accept(this);
  (*ss) << ", .end = (s64)";
  node->right->accept(this);
  (*ss) << "}";
  return;
}
void Emitter::visit(ASTSwitch *node) {
  if (!node->is_statement) {
    (*ss) << "[&] ->";
    auto type = global_get_type(node->return_type);
    (*ss) << to_cpp_string(type);
    (*ss) << "{\n";
    ;
  }

  auto emit_switch_case = [&](ASTExpr *target, const SwitchCase &_case, bool first) {
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

  return;
}
void Emitter::visit(ASTTuple *node) {
  (*ss) << "std::tuple(";
  for (const auto &value : node->values) {
    value->accept(this);
    if (value != node->values.back())
      (*ss) << ", ";
  }
  (*ss) << ")";
  return;
}
void Emitter::visit(ASTTupleDeconstruction *node) {
  emit_line_directive(node);
  if (node->op == TType::ColonEquals) {
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
  } else {
    (*ss) << "std::tie(";
    for (auto &iden : node->idens) {
      (*ss) << iden->value.get_str();
      if (iden != node->idens.back()) {
        (*ss) << ", ";
      }
    }
    (*ss) << ") = ";
    node->right->accept(this);
    (*ss) << ";\n";
  }
  return;
};

// TODO: remove me, add explicit casting, at least for non-void pointers.
// I don't mind implicit casting to void*/u8*
void Emitter::cast_pointers_implicit(ASTDeclaration *&node) {
  auto type = global_get_type(node->type->resolved_type);
  if (type->get_ext().is_pointer())
    (*ss) << "(" << to_cpp_string(type) << ")";
}


std::string Emitter::get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;
  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    auto &ext = type->get_ext();

    if (ext.is_fixed_sized_array()) {
      identifier += ext.to_string();
    }

    return get_function_pointer_type_string(type, &identifier);
  }
  tss << type->get_base().get_str();
  if (!type->get_ext().is_fixed_sized_array()) {
    tss << name << ' ';
  }
  bool emitted_iden = false;
  for (const auto ext : type->get_ext().extensions) {
    if (ext.type == TYPE_EXT_POINTER) {
      tss << "*";
    } else if (ext.type == TYPE_EXT_ARRAY) {
      if (!emitted_iden) {
        emitted_iden = true;
        tss << ' ' << name;
      }
      tss << "[" << std::to_string(ext.array_size) << "]";
    }
  }
  return tss.str();
}

std::string get_format_str(int type_id, ASTNode *node) {
  auto type = global_get_type(type_id);
  // We just assume that the type-checker has validated that this struct has a
  // to_string() function

  if (type->is_kind(TYPE_TUPLE)) {
    auto info = type->get_info()->as<TupleTypeInfo>();
    std::string format_str = "<";
    int i = 0;
    for (const auto &t : info->types) {
      format_str += get_format_str(t, node);
      if (i != info->types.size() - 1) {
        format_str += ", ";
      }
      ++i;
    }
    format_str += ">";
    return format_str;
  }

  if (type->is_kind(TYPE_STRUCT)) {
    return "%s";
  }
  if (type->id == charptr_type()) {
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
    } else if (type->id == s8_type() || type->id == s16_type() || type->id == s32_type() || type->id == u8_type() ||
               type->id == u16_type() || type->id == u32_type() || type->id == int_type()) {
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
}

// TODO: This needs a lot of work, front to back.
// Parsing, lexing, and emitting.
void Emitter::interpolate_string(ASTLiteral *node) {
  emit_line_directive(node);

  std::string str;
  auto current = node->interpolated_string_root;
  std::stringstream interp_ss;

  while (current) {
    interp_ss << current->prefix.get_str();
    if (current->expression) {
      auto type_id = current->expression->resolved_type;
      interp_ss << get_format_str(type_id, node);
    }
    current = current->next;
  }

  // ! ! I refactored string interpolation to return char* for now until we build our string back in to it's full glory,
  // ! ! Or replace this garbage string interpolation with a format!() function or macro or something.
  // ! ! However, it leaks like a siev now.
  (*ss) << "[&] -> char* { char* buf = (char*)malloc(1024); memset(buf, 0, 1024);\nsprintf(buf, \"" << interp_ss.str()
        << "\",";

  current = node->interpolated_string_root;
  while (current) {
    if (current->expression) {
      auto type_id = current->expression->resolved_type;
      auto type = global_get_type(type_id);

      const auto interpolate_to_string_struct_union = [&](Scope *scope) {
        auto sym = scope->lookup("to_string");

        if (!sym)
          throw_error("Cannot use a struct in an interpolated string without defining a "
                      "`to_string` function that returns either a char* or a string",
                      current->expression->source_range);

        auto sym_ty = static_cast<FunctionTypeInfo *>(global_get_type(sym->type_id)->get_info());
        auto return_ty = global_get_type(sym_ty->return_type);
        auto param_0 = global_get_type(sym_ty->parameter_types[0]);
        auto takes_pointer = param_0->get_ext().is_pointer();
        auto &extensions = type->get_ext();
        auto name = type->get_base();

        if (extensions.back_type() == TYPE_EXT_POINTER) {
          (*ss) << name.get_str() << "_to_string(";
          if (!takes_pointer) {
            (*ss) << "*";
          }
          current->expression->accept(this);
          (*ss) << ")";
        } else {
          (*ss) << name.get_str() << "_to_string(";
          if (takes_pointer) {
            (*ss) << "&";
          }
          current->expression->accept(this);
          (*ss) << ")";
        }
      };

      if (type->id == bool_type()) {
        current->expression->accept(this);
        (*ss) << " ? \"true\" : \"false\"";
      } else if (type->is_kind(TYPE_STRUCT)) {
        auto info = (type->get_info()->as<StructTypeInfo>());
        interpolate_to_string_struct_union(info->scope);
      } else if (type->is_kind(TYPE_TUPLE)) {
        auto info = type->get_info()->as<TupleTypeInfo>();
        for (int i = 0; i < info->types.size(); ++i) {
          (*ss) << "std::get<" << std::to_string(i) << ">(";
          current->expression->accept(this);
          (*ss) << ")";
          if (i != info->types.size() - 1) {
            (*ss) << ", ";
          }
        }
      } else {
        current->expression->accept(this);
      }
      if (current->next && current->next->expression) {
        (*ss) << ", ";
      }
    }
    current = current->next;
  }

  (*ss) << ");\n "
           " return buf; }()";
}

// Identifier may contain a fixed buffer size like name[30] due to the way
// function pointers have to work in C.
std::string Emitter::get_function_pointer_type_string(Type *type, Nullable<std::string> identifier) {
  auto type_prefix = std::string{"*"};

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error("Internal compiler error: tried to get a function pointer from "
                "a non-function type",
                {});
  }

  std::stringstream ss;

  auto info = (type->get_info()->as<FunctionTypeInfo>());
  auto return_type = global_get_type(info->return_type);

  ss << to_cpp_string(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

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
  ss << "new Field { " << std::format(".name = \"{}\", ", name)
     << std::format(".type = {}, ", to_type_struct(type, context));

  if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    ss << std::format(".offset = offsetof({}, {})", parent_type->get_base().get_str(), name);
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
      auto sint = type->id == int_type() || type->id == s8_type() || type->id == s16_type() || type->id == s32_type() ||
                  type->id == s64_type();

      auto uint = type->id == u8_type() || type->id == u16_type() || type->id == u32_type() || type->id == u64_type();

      auto floating_pt = type->id == float32_type() || type->id == float64_type() || type->id == float_type();
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
      case TYPE_EXT_POINTER:
        kind_flags |= TYPE_FLAGS_POINTER;
        break;
      case TYPE_EXT_ARRAY:
        kind_flags |= TYPE_FLAGS_ARRAY;
        break;
      case TYPE_EXT_INVALID:
        throw_error("Internal Compiler Error: Extension type not set.", {});
        break;
    }
  }
  return ".flags = " + std::to_string(kind_flags) + "\n";
}

std::string Emitter::get_type_struct(Type *type, int id, Context &context, const std::string &fields) {
  std::stringstream ss;

  auto kind = 0;

  ss << "_type_info[" << id << "] = new Type {" << ".id = " << id << ", "
     << ".name = \"" << type->to_string() << "\", ";

  if (!type->is_kind(TYPE_ENUM))
    ss << ".size = sizeof(" << to_cpp_string(type) << "), ";

  ss << get_type_flags(type) << ",\n"
     << ".fields = " << fields << ",\n";

  if (type->get_ext().is_fixed_sized_array()) {
    ss << get_elements_function(type) << ",\n";
  }

  if (type->get_ext().is_pointer() || type->get_ext().is_fixed_sized_array()) {
    ss << ".element_type = " << to_type_struct(global_get_type(type->get_element_type()), context) << ",\n";
  } else {
    ss << ".element_type = nullptr,\n";
  }

  ss << " };";
  context.type_info_strings.push_back(ss.str());
  return std::format("_type_info[{}]", id);
}

std::string Emitter::to_type_struct(Type *type, Context &context) {
  auto id = type->id;

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
    auto info = type->get_info();
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
    // TODO: we have to fix this!.
    auto info = type->get_info();
    if (info->scope->ordered_symbols.empty()) {
      return get_type_struct(type, id, context, "{}");
    }
    fields_ss << "{";
    for (const auto &field : info->scope->ordered_symbols) {
      auto t = global_get_type(s32_type());

      if (!t) {
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      }

      fields_ss << get_field_struct(field.get_str(), t, type, context);

      if (field != info->scope->ordered_symbols.back()) {
        fields_ss << ",\n";
      }
    }

    fields_ss << "}";
  } else {
    return get_type_struct(type, id, context, "{}");
  }

  return get_type_struct(type, id, context, fields_ss.str());
}

bool Emitter::should_emit_function(Emitter *visitor, ASTFunctionDeclaration *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "__COMPILER_GENERATED_TEST(\"" << node->name.get_str() << "\", " << node->name.get_str()
                            << "),";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->name == "main") {
    return false;
  }
  return true;
}

std::string Emitter::to_cpp_string(const TypeExtensions &extensions, const std::string &base) {
  std::stringstream ss;

  // TODO: we need to fix the emitting of 'c_string' as const char*,
  // right now it's fricked up.
  ss << base;

  for (const auto ext : extensions.extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      std::string current = ss.str();
      ss.str("");
      ss.clear();
      ss << "_array<" << current << ">";
    } else if (ext.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(ext.array_size) << "]";
    } else if (ext.type == TYPE_EXT_POINTER) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string Emitter::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";

  if (id == c_string_type()) {
    name = "const char";
  } else if (type->get_base() == "u8" && type->get_ext().is_pointer()) {
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
      output = "std::tuple<";
      for (int i = 0; i < info->types.size(); ++i) {
        output += to_cpp_string(global_get_type(info->types[i]));
        if (i != info->types.size() - 1) {
          output += ", ";
        }
      }
      output += ">";
      output = to_cpp_string(type->get_ext(), output);
      break;
    }
    case TYPE_INTERFACE:
      throw_error("can't declare an instance of an interface", {});
      break;
  }
  return output;
}

void Emitter::emit_condition_block(ASTNode *node, const std::string &keyword, Nullable<ASTExpr> condition,
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

void Emitter::visit(ASTScopeResolution *node) {
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
  auto op = "_"; // ! this may cause issues, but this is going to be a permanent change. we obviously can't lower to C,
                 // when we use C++ features like `::`
  (*ss) << op << node->member_name.get_str();
  return;
}

void Emitter::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    for (auto &instantiation : node->generic_instantiations) {
      for (auto type_id : instantiation.arguments) {
        auto type = global_get_type(type_id);
        if (type->declaring_node) {
          type->declaring_node.get()->accept(this);
        }
      }
      instantiation.node->accept(this);
    }
    return;
  }

  // !! If we visit this here, we get 'use of undeclared identifier T'
  // !! if we just use the resolved type, we get Type::invalid_id.
  // !! it's a lose lose, what is causing this?
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
  (*ss) << "typedef struct " << node->name.get_str() << " " << node->name.get_str() << ";\n";
  auto name = node->name.get_str();
  for (const auto &member : node->members) {
    if (member->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
      auto struct_node = static_cast<ASTStructDeclaration *>(member);
      auto subtype_name = name + "_" + struct_node->name.get_str();
      (*ss) << "typedef struct " << subtype_name << "{\n";
      for (const auto &field : struct_node->fields) {
        field->accept(this);
        (*ss) << ";\n";
      }
      for (const auto &$union : struct_node->subtypes) {
        $union->accept(this);
      }
      (*ss) << "} " << subtype_name << ";\n";
    } else if (member->get_node_type() == AST_NODE_DECLARATION) {
      auto declaration_node = static_cast<ASTDeclaration *>(member);
      auto subtype_name = name + "_" + declaration_node->name.get_str();

      (*ss) << "typedef ";
      declaration_node->type->accept(this);
      (*ss) << " " << subtype_name << ";\n";
    }
  }

  (*ss) << "typedef struct " << node->name.get_str() << "{\n int index;\n union {\n";

  int n = 0;
  for (const auto &member : node->members) {
    if (member->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
      auto struct_node = static_cast<ASTStructDeclaration *>(member);
      (*ss) << name + "_" + struct_node->name.get_str() << " $index_" << std::to_string(n) << ";\n";
    } else if (member->get_node_type() == AST_NODE_DECLARATION) {
      auto declaration_node = static_cast<ASTDeclaration *>(member);
      auto alias = name + "_" + declaration_node->name.get_str();
      (*ss) << alias << " $index_" << std::to_string(n) << ";\n";
    }
    n++;
  }
  (*ss) << "\n };\n} " << node->name.get_str() << " ;\n";
  return;
}

// Helper function to emit deferred statements
void Emitter::emit_deferred_statements(DeferBlockType type) {
  auto defer_block = defer_blocks.rbegin();
  while (defer_block->type != type) {
    if (defer_block == defer_blocks.rend()) {
      throw_error("Internal Compiler Error: could not find defer block type in stack", {});
    }
    for (auto defer : defer_block->defers) {
      defer->statement->accept(this);
      semicolon();
      newline();
    }
    defer_block++;
  }
  if (defer_block == defer_blocks.rend()) {
    throw_error("Internal Compiler Error: could not find defer block type in stack", {});
  }
  for (auto defer : defer_block->defers) {
    defer->statement->accept(this);
    semicolon();
    newline();
  }
}

void Emitter::visit(ASTFunctionDeclaration *node) {
  auto emit_function_signature_and_body = [&](const std::string &name) {
    node->return_type->accept(this);
    (*ss) << " " + name;
    emit_default_args = true;
    node->params->accept(this);
    emit_default_args = false;
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
      for (auto &instantiation : node->generic_instantiations) {
        for (auto type_id : instantiation.arguments) {
          auto type = global_get_type(type_id);
          if (type->declaring_node) {
            type->declaring_node.get()->accept(this);
          }
        }
        instantiation.node->accept(this);
      }
      return;
    }
    auto is_local = (node->flags & FUNCTION_IS_LOCAL) != 0;

    if (node->name != "main" && !is_local) {
      if ((node->flags & FUNCTION_IS_STATIC) != 0) {
        (*ss) << "static ";
      }
      if ((node->flags & FUNCTION_IS_FORWARD_DECLARED) != 0) {
        emit_forward_declaration(node);
        return;
      }
    }

    // local function
    if (is_local) {
      emit_local_function(node);
      return;
    }

    if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
      (*ss) << "extern \"C\" ";
    }

    std::string name;
    if (type_context) {
      auto type = global_get_type(type_context.get()->resolved_type);
      name += to_cpp_string(type) + "_";
    }
    name += node->name.get_str();
    if (!node->generic_arguments.empty()) {
      name += mangled_type_args(node->generic_arguments);
    }

    if (node->name == "main" && !is_freestanding) {
      has_user_defined_main = true;
      node->return_type->accept(this);
      (*ss) << " __ela_main_()"; // We use Env::args() to get args now.
      node->block.get()->accept(this);
    } else {
      emit_function_signature_and_body(name);
    }
  };

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
  } else {
    indented("return");
    if (node->expression.is_not_null()) {
      space();
      node->expression.get()->accept(this);
    }
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
    emit_line_directive(node);
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
void Emitter::visit(ASTLambda *node) {
  // We parenthesize this because if you call it on the spot,
  // it thinks you're trying to do + on whatever this returns.

  
  (*ss) << "(+[]";
  node->params->accept(this);
  (*ss) << " -> ";
  node->return_type->accept(this);
  node->block->accept(this);
  (*ss) << ")";
}

// This should never get hit.
void Emitter::visit(ASTWhere *node) {
  throw_error("Internal compiler error: 'where' expression was visited in the emitter", node->source_range);
}
