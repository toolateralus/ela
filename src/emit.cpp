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

static constexpr auto TESTING_MAIN_BOILERPLATE_AAAAGHH = R"__(
#ifdef TESTING
#define __TEST_RUNNER_MAIN                                                                                             \
  int main() {                                                                                                         \
    for (int i = 0; i < sizeof(tests) / sizeof(__COMPILER_GENERATED_TEST); i++) {                                      \
      __COMPILER_GENERATED_TEST_RUN(&tests[i]);                                                                        \
    }                                                                                                                  \
  }                                                                                                                     
#endif
)__";

// This is stuff we just can't really get rid of while using a transpiled backend.
static constexpr auto INESCAPABLE_BOILERPLATE_AAAGHHH = R"__(

typedef double float64;
typedef unsigned long long int u64;
typedef signed long long int s64;

typedef signed int s32;
typedef unsigned int u32;
typedef float float32;

typedef short int s16;
typedef unsigned short int u16;

typedef signed char s8;
typedef unsigned char u8;
#if USE_STD_LIB
  #include <stddef.h>
  #include <stdint.h>
  #include <errno.h>
  #undef RAND_MAX
#endif

#ifdef TESTING
  int printf(const char *, ...);
  void exit(int);
  
  typedef struct {
    const char *name;
    void (*function)();
  } __COMPILER_GENERATED_TEST;
  static void __COMPILER_GENERATED_TEST_RUN(__COMPILER_GENERATED_TEST *test) {
    #if TEST_VERBOSE 
      printf("running %s\n", test->name);
    #endif
    test->function();
  }

  #define assert(message, condition)                                                                                     \
  if (!(condition)) {                                                                                                    \
    printf("\033[31mAssertion failed: \n\t\033[1;31mcondition ::\033[0m(\033[1;34m%s\033[0m), "                          \
           "\n\t\033[1;31mmessage   ::\033[0m(\033[1;34m%s\033[0m])\033[0m\n",                                           \
           #condition, message);                                                                                         \
    exit(1);                                                                                                             \
  }
#else
  #define assert(message, condition)                                                                                     \
    if (!(condition)) {                                                                                                  \
      printf("assertion failed: %s\n " #condition "\n", message);                                                        \
      exit(1);                                                                                                           \
    }
#endif
)__";

void Emitter::forward_decl_type(Type *type) {
  switch (type->kind) {
    case TYPE_STRUCT: {
      auto info = type->get_info()->template as<StructTypeInfo>();
      std::string kw = "typedef struct ";
      if ((info->flags & STRUCT_FLAG_IS_UNION) != 0)
        kw = "typedef union ";
      (*ss) << kw << type->get_base().get_str() << " " << type->get_base().get_str() << ";\n";
    } break;
    case TYPE_TUPLE:
    case TYPE_TAGGED_UNION: {
      (*ss) << "struct " << to_cpp_string(type) << ";\n";
    } break;
    case TYPE_FUNCTION: {
      auto info = type->get_info()->template as<FunctionTypeInfo>();
      for (int index = 0; index < info->params_len; index++) {
        auto param_ty = info->parameter_types[index];
        forward_decl_type(global_get_type(param_ty));
      }
      forward_decl_type(global_get_type(info->return_type));
    } break;
    case TYPE_SCALAR:
      break;
    case TYPE_ENUM:
    case TYPE_INTERFACE:
      throw_error(
          std::format("internal compiler error: tried to forward declare an invalid type :: {}", type->to_string()),
          {});
      break;
  }
}

template <typename T> void Emitter::emit_generic_instantiations(std::vector<GenericInstance<T>> instantiations) {
  for (auto &instantiation : instantiations) {
    for (auto type_id : instantiation.arguments) {
      auto type = global_get_type(type_id);
      if (type->base_id != Type::invalid_id) {
        type = global_get_type(type->base_id);
        forward_decl_type(type);
      } else if (type->declaring_node) {
        type->declaring_node.get()->accept(this);
      }
    }
    instantiation.node->accept(this);
    auto type = global_get_type(instantiation.node->resolved_type);
    emit_tuple_dependants(type->tuple_dependants);
  }
}

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

  Defer _defer([] { depth--; });

  (*ss) << indent() << "{\n";
  indent_level++;

  std::string range_type_str = to_cpp_string(global_get_type(node->range_type));
  (*ss) << indent() << range_type_str << " " << range_unique_id << " = ";
  node->range->accept(this);
  (*ss) << ";\n";

  std::string iterable_type_str = to_cpp_string(global_get_type(node->iterable_type));
  (*ss) << indent() << iterable_type_str << " " << unique_id << " = ";
  if (node->is_enumerable) {
    (*ss) << "$" + std::to_string(node->range_type) << "_enumerator(&" << range_unique_id << ");\n";
  } else {
    (*ss) << "$" + std::to_string(node->range_type) << "_iter(&" << range_unique_id << ");\n";
  }

  (*ss) << indent() << "while (!$" << std::to_string(node->iterable_type) << "_done(&" << unique_id << ")) {\n";
  indent_level++;

  std::string identifier_type_str = to_cpp_string(global_get_type(node->identifier_type));
  (*ss) << indent() << identifier_type_str << " ";

  node->iden->accept(this);
  (*ss) << " = ";
  auto iterable_method_str = "$" + std::to_string(node->iterable_type);
  if (node->value_semantic == VALUE_SEMANTIC_POINTER) {
    (*ss) << iterable_method_str << "_current(&" << unique_id << ");\n";
  } else if (node->is_enumerable) { // Enumerables don't use the * value semantic.
    (*ss) << iterable_method_str << "_current(&" << unique_id << ");\n";
  } else {
    (*ss) << "*" << iterable_method_str << "_current(&" << unique_id << ");\n";
  }

  // this MUST happen before the block or continue will cause a permanent hangup!!!
  (*ss) << indent() << iterable_method_str << "_next(&" << unique_id << ");\n";

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
    if (node->arguments[i]->get_node_type() == AST_NODE_IDENTIFIER) {
      auto iden = static_cast<ASTIdentifier *>(node->arguments[i]);
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
      throw_error(std::format("internal compiler error: 'get_dot_left_type' encountered an unexpected node, kind {}",
                              (int)node->get_node_type()),
                  node->source_range);
  }
  return Type::invalid_id;
}

void Emitter::visit(ASTCall *node) {
  auto base_symbol = typer.get_symbol(node->function);

  std::vector<int> generic_args;
  for (const auto arg : node->generic_arguments) {
    generic_args.push_back(arg->resolved_type);
  }

  auto symbol = base_symbol.get();
  if (node->function->get_node_type() == AST_NODE_DOT_EXPR && base_symbol && symbol->declaring_node.is_not_null() &&
      symbol->declaring_node.get()->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
    auto func = static_cast<ASTFunctionDeclaration *>(symbol->declaring_node.get());
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
void Emitter::visit(ASTLiteral *node) {
  auto type = to_cpp_string(global_get_type(node->resolved_type));
  std::string output;
  switch (node->tag) {
    case ASTLiteral::InterpolatedString: {
      interpolate_string(node);
      return;
    }
    case ASTLiteral::Null:
      (*ss) << "NULL";
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

  if (node->type->resolved_type == Type::invalid_id) {
    throw_error("internal compiler error: type was null upon emitting an ASTDeclaration", node->source_range);
  }

  auto type = global_get_type(node->type->resolved_type);
  auto symbol = ctx.scope->local_lookup(node->name);

  auto handle_initialization = [&]() {
    if (node->value.is_not_null() && emit_default_value) {
      (*ss) << " = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    } else if (emit_default_init) {
      auto type = global_get_type(node->type->resolved_type);
      if (type->is_kind(TYPE_STRUCT)) {
        (*ss) << "= (" + to_cpp_string(type) + ") {}";
      } else {
        (*ss) << "= (" + to_cpp_string(type) + ") {0}";
      }
    }
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
  emit_default_args = true;

  if ((node->flags & FUNCTION_IS_EXPORTED) != 0) {
    (*ss) << "extern  ";
  }

  node->return_type->accept(this);
  (*ss) << ' ' << node->name.get_str() << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
  emit_default_args = false;
}
void Emitter::emit_local_function(ASTFunctionDeclaration *node) {
  //! We cannot do this kind of lambda when transpiling to C.
  //! We need to figure out how to place a function above us at the global scope with the correct dependencies.

  // // Right now we just always do a closure on local lambda functions.
  // // This probably isn't desirable for simple in-out functions
  // (*ss) << indent() << "auto " << node->name.get_str() << " = [&]";
  // node->params->accept(this);
  // (*ss) << " -> ";
  // node->return_type->accept(this);
  // if (node->block.is_null()) {
  //   throw_error("local function cannot be #foreign", node->source_range);
  // }
  // node->block.get()->accept(this);
}
void Emitter::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name == "main") {
    throw_error("main function cannot be foreign", node->source_range);
  }

  (*ss) << "extern ";
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
    emit_generic_instantiations(node->generic_instantiations);
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

  for (auto member : node->members) {
    auto type = global_get_type(member.type->resolved_type);
    if (type->base_id != Type::invalid_id) {
      type = global_get_type(type->base_id);
      forward_decl_type(type);
    } else if (type->declaring_node) {
      type->declaring_node.get()->accept(this);
    }
  }

  emit_line_directive(node);
  auto type = global_get_type(node->resolved_type);

  auto info = (type->get_info()->as<StructTypeInfo>());

  std::string type_name = type->get_base().get_str();
  ;
  std::string type_tag = (node->is_union ? "typedef union " : "typedef struct ");

  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED || node->is_fwd_decl) != 0) {
    if (node->is_extern) {
      (*ss) << "extern ";
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

  emit_tuple_dependants(type->tuple_dependants);

  indent_level--;

  bool has_default_ctor = false;
  bool has_dtor = false;

  return;
}
void Emitter::visit(ASTEnumDeclaration *node) {
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

  code << INESCAPABLE_BOILERPLATE_AAAGHHH << '\n';

  if (!is_freestanding) {
    code << "typedef struct Type Type;\n";
    auto type_ptr_id = ctx.scope->find_type_id("Type", {{{TYPE_EXT_POINTER}}});
    code << std::format("typedef struct List${} List${};\nextern List${} _type_info;\n", type_ptr_id, type_ptr_id,
                        type_ptr_id);
  }

  if (testing) {
    code << "#define TESTING\n";
  }

  for (auto &type : type_table) {
    if (type->is_kind(TYPE_SCALAR) || type->is_kind(TYPE_FUNCTION)) {
      emit_tuple_dependants(type->tuple_dependants);
    }
  }

  for (const auto &statement : node->statements) {
    statement->accept(this);
    semicolon();
    newline();
  }

  // Emit runtime reflection type info for requested types, only when we have
  // actually requested runtime type information.
  if (!ctx.type_info_strings.empty() && !is_freestanding) {
    std::stringstream type_info{};
    for (const auto &str : ctx.type_info_strings) {
      type_info << str.get_str() << ";\n";
    }

    code << "void $initialize_reflection_system() {\n";
    {
      // we don't bother doing pushes into type info, it's easier for us to do it this way.
      code << std::format("_type_info.length = _type_info.capacity = {};\n", ctx.type_info_strings.size());
      code << std::format("_type_info.data = realloc(_type_info.data, sizeof(Type*) * {});", type_table.size());
      code << type_info.str() << ";\n";
    }
    code << "}\n";

    // code << std::format("auto __ts_init_func_result__ = []{{\n"
    //                     "  {};\n"
    //                     "  return 0;\n"
    //                     "}}();\n",
    //                     type_table.size(), type_info.str());

    code << std::format(R"_(
Type *find_type(string name) {{
  for (size_t i = 0; i < {}; ++i) {{
    Type *type = _type_info.data[i];
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
  return NULL; // Return nullptr if the type is not found
}}
)_",
                        type_table.size());
  }

  if (testing) {
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
#define FORMAT_STR "int main (int argc, char** argv) {{\n${}_initialize(argc, argv);\n{}\n__ela_main_();\n}}\n"

      // I made that macro because the formatting goes CRAZY with a raw string as an arg
      code << std::format(FORMAT_STR, ctx.scope->find_type_id("Env", {}),
                          ctx.type_info_strings.size() != 0 ? "$initialize_reflection_system();\n"
                                                            : "{/* no reflection present in module */};\n");
    } // C calls main() for freestanding
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
  (*ss) << "(Range) {.begin = (s64)";
  node->left->accept(this);
  (*ss) << ", .end = (s64)";
  node->right->accept(this);
  (*ss) << "}";
  return;
}

void Emitter::visit(ASTSwitch *node) {
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

  auto block = node->declaring_block;
  if (!block) {
    throw_error("internal compiler error: couldn't generate temporary variable because declaring block was null",
                node->source_range);
  }
  auto id = block.get()->temp_iden_idx++;
  std::string temp_id = "$temp_tuple$" + std::to_string(id++);
  (*ss) << "auto " << temp_id << " = ";
  node->right->accept(this);
  (*ss) << ";\n";

  if (node->op == TType::ColonEquals) {
    for (size_t i = 0; i < node->idens.size(); ++i) {
      (*ss) << "auto " << node->idens[i]->value.get_str() << " = ";
      (*ss) << temp_id << ".$" << std::to_string(i) << ";\n";
    }
  } else {
    for (size_t i = 0; i < node->idens.size(); ++i) {
      (*ss) << node->idens[i]->value.get_str() << " = ";
      (*ss) << temp_id << ".$" << std::to_string(i) << ";\n";
    }
  }
}

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
  auto base = type->get_base().get_str();
  ;
  tss << to_cpp_string(global_get_type(type->get_element_type()));
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
    std::string format_str = "(";
    int i = 0;
    for (const auto &t : info->types) {
      format_str += get_format_str(t, node);
      if (i != info->types.size() - 1) {
        format_str += ", ";
      }
      ++i;
    }
    format_str += ")";
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
  // TODO: we should remove interpolated strings, and replace with a variadic template system with statement unfolding.
  (*ss) << "NULL";
  return;

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
          current->expression->accept(this);
          (*ss) << ".$" << std::to_string(i);

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
    throw_error("internal compiler error: tried to get a function pointer from "
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
  ss << "(Field) { " << std::format(".name = \"{}\", ", name)
     << std::format(".type = {}, ", to_type_struct(type, context));

  if (!type->is_kind(TYPE_FUNCTION) && !parent_type->is_kind(TYPE_ENUM)) {
    ss << std::format(".size = sizeof({}), ", to_cpp_string(type));
    ss << std::format(".offset = offsetof({}, {})", parent_type->get_base().get_str(), name);
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
        throw_error("internal compiler error: Extension type not set.", {});
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
    if (type->kind == TYPE_STRUCT) {
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

        if (name == "this")
          continue;

        auto t = global_get_type(sym.type_id);
        // TODO: handle methods separately
        if (t->is_kind(TYPE_FUNCTION) || (sym.flags & SYMBOL_IS_FUNCTION))
          continue;

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

      int count = info->scope->ordered_symbols.size();
      fields_ss << "_type_info.data[" << id << "]->fields.data = malloc(" << count << " * sizeof(Field));\n";
      fields_ss << "_type_info.data[" << id << "]->fields.length = " << count << ";\n";
      fields_ss << "_type_info.data[" << id << "]->fields.capacity = " << count << ";\n";

      int it = 0;
      for (const auto &field : info->scope->ordered_symbols) {
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
    visitor->test_functions << "(__COMPILER_GENERATED_TEST){.name = \"" << node->name.get_str() << "\", .function = &"
                            << node->name.get_str() << "},";
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
    emit_generic_instantiations(node->generic_instantiations);
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
      emit_generic_instantiations(node->generic_instantiations);
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
      (*ss) << "extern  ";
    }

    std::string name;
    if (type_context) {
      name += "$" + std::to_string(type_context.get()->resolved_type) + "_";
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

  // Emit all the lambdas that this function depends on.
  for (const auto &lambda: node->lambdas) {
    emit_line_directive(lambda);
    lambda->return_type->accept(this);
    (*ss) << ' ' << lambda->unique_identifier.get_str() << ' ';
    lambda->params->accept(this);
    lambda->block->accept(this);
    newline();
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
  (*ss) << node->unique_identifier.get_str();
}

// This should never get hit.
void Emitter::visit(ASTWhere *node) {
  throw_error("internal compiler error: 'where' expression was visited in the emitter", node->source_range);
}

void Emitter::emit_tuple_dependants(std::vector<int> &types) {
  std::unordered_set<int> emitted_tuples;

  while (!types.empty()) {
    auto type_id = types.back();

    if (emitted_tuples.contains(type_id)) {
      continue;
    }
    emitted_tuples.insert(type_id);

    types.pop_back();
    auto type = global_get_type(type_id);
    auto name = to_cpp_string(type);
    (*ss) << "typedef struct {";
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
    // We have to do this recursively for nested tuples.
    emit_tuple_dependants(type->tuple_dependants);
  }
}

void Emitter::visit(ASTSize_Of *node) {
  (*ss) << "sizeof(";
  node->target_type->accept(this);
  (*ss) << ")";
}
