#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <functional>
#include <jstl/containers/vector.hpp>
#include <sstream>

std::any EmitVisitor::visit(ASTCompAssign *node) {
  (*ss) <<indent() << node->name.value << " ";
  (*ss) <<node->op.value << " ";
  node->expr->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTWhile *node) {
  (*ss) <<indent() << "while ";
  if (node->condition.is_not_null()) {
    (*ss) <<"(";
    node->condition.get()->accept(this);
    (*ss) <<")";
  } else {
    (*ss) <<"(true)";
  }
  node->block->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTElse *node) {
  (*ss) <<" else ";
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
  } else if (node->block.is_not_null()) {
    node->block.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTIf *node) {
  (*ss) <<indent() << "if (";
  node->condition->accept(this);
  (*ss) <<")";
  node->block->accept(this);
  if (node->_else.is_not_null()) {
    node->_else.get()->accept(this);
  }
  
  return {};
}

std::any EmitVisitor::visit(ASTFor *node) {
  const auto emit_range_based = [&] {
    auto v = node->value.range_based;
    (*ss) <<"auto ";
    v.target->accept(this);
    (*ss) <<" : ";
    v.collection->accept(this);
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
  
  (*ss) <<indent() << "for (";
  switch (node->tag) {
  case ASTFor::RangeBased:
    emit_range_based();
    break;
  case ASTFor::CStyle:
    emit_c_style();
    break;
  }
  (*ss) <<")";
  node->block->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTBreak *node) {
  indented("break");
  return {};  
}

std::any EmitVisitor::visit(ASTContinue *node) {
  indented("continue");
  return {};
}

std::any EmitVisitor::visit(ASTReturn *node) {
  indented("return");
  if (node->expression.is_not_null()) {
    space();
    node->expression.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTArguments *node) {
  (*ss) <<"(";
  for (int i = 0; i < node->arguments.size(); ++i) {
    node->arguments[i]->accept(this);
    if (i != node->arguments.size() - 1) {
      (*ss) <<", ";
    }
  }
  (*ss) <<")";
  return {};
}

std::any EmitVisitor::visit(ASTType *node) {
  auto type = get_type(node->resolved_type);
  if (node->flags == ASTTYPE_EMIT_OBJECT) {
    (*ss) << get_type(node->pointing_to.get()->resolved_type)->to_type_struct(context);
    return {};
  }
  (*ss) << type->to_cpp_string();
  return {};
}

std::any EmitVisitor::visit(ASTCall *node) {
  (*ss) <<node->name.value;
  node->arguments->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTLiteral *node) {
  if (node->tag == ASTLiteral::Null) {
    (*ss) <<"nullptr";
  } else if (node->tag == ASTLiteral::String) {
    (*ss) <<std::format("\"{}\"", node->value);
  } else if (node->tag == ASTLiteral::RawString) {
    // TODO: search for a pattern '__()__' for example, that doesn't exist at all in the string.
    (*ss) <<std::format("R\"__({})__\"", node->value);
  } else {
    (*ss) <<node->value;
  }
  return {};
}

std::any EmitVisitor::visit(ASTIdentifier *node) {
  (*ss) <<node->value.value;
  return {};
}

std::any EmitVisitor::visit(ASTUnaryExpr *node) {
  (*ss) <<node->op.value;
  node->operand->accept(this);
  return {};
}

std::any EmitVisitor::visit(ASTBinExpr *node) {
  
  
  // type inference assignment.
  if (node->op.type == TType::ColonEquals) {
    (*ss) << "auto ";
    node->left->accept(this);
    (*ss) << " = ";
    node->right->accept(this);
    return{};
  }
  
  
  // TODO: Figure out how we want to control custom precedence. Right now,
  // TODO(cont): We'll just parenthesize every single sub-expression;
  (*ss) <<"(";
  auto left = node->left->accept(this);
  space();
  (*ss) <<node->op.value;
  
  if (node->op.type == TType::Assign) {
    auto type = get_type(node->resolved_type);
    auto isptr = type->extensions.is_pointer(1);
    if (isptr)
      (*ss) << "(" << type->to_cpp_string() << ")";
  }
  
  space();
  auto right = node->right->accept(this);
  (*ss) <<")";
  return {};
}

std::any EmitVisitor::visit(ASTExprStatement *node) {
  (*ss) <<indent();
  node->expression->accept(this);
  return {};
}
    
// TODO: remove me, add explicit casting.
// CASTING ALL POINTERS ALWAYS::
// This is so we can use malloc tempoarily. 
// It's very bad.
void EmitVisitor::cast_pointers_implicit(ASTDeclaration *&node) {
    auto type = get_type(node->type->resolved_type);
    auto isptr = type->extensions.is_pointer(1);
    if (isptr)
      (*ss) << "(" << type->to_cpp_string() << ")";
}

std::any EmitVisitor::visit(ASTDeclaration *node) {
  auto type = get_type(node->type->resolved_type);
  if (type->extensions.is_fixed_sized_array()) {
    auto type_str = type->extensions.to_string();
    (*ss) << type->base << ' ' << node->name.value << type_str;
  } else {
    node->type->accept(this);
    space();
    (*ss) <<node->name.value;
    space();
    if (node->value.is_not_null()) {
      (*ss) <<" = ";
      cast_pointers_implicit(node);
      node->value.get()->accept(this);
    } else {
      (*ss) <<"{}";
    }
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamDecl *node) {
  node->type->accept(this);
  (*ss) << ' ' << node->name;
  if (node->default_value.is_not_null() && emit_default_args) {
    (*ss) <<" = ";
    node->default_value.get()->accept(this);
  }
  return {};
}

std::any EmitVisitor::visit(ASTParamsDecl *node) {
  (*ss) <<"(";
  int i = 0;
  for (const auto &param : node->params) {
    param->accept(this);
    if (i != node->params.size() - 1) {
      (*ss) <<", ";
    }
    ++i;
  }
  
  if (current_func_decl.is_not_null() && (current_func_decl.get()->flags & FUNCTION_IS_VARARGS) != 0) {
    (*ss) << ", ...)";
    return {};
  }
  
  (*ss) <<")";
  return {};
}

static bool should_emit_function(EmitVisitor *visitor, ASTFunctionDeclaration *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  } 
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "__COMPILER_GENERATED_TEST(\"" << node->name.value << "\", " << node->name.value << "),";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->name.value == "main") {
    return false;
  }
  return true;
}

void EmitVisitor::emit_forward_declaration(ASTFunctionDeclaration *node) {
  
  if ((node->flags & FUNCTION_IS_METHOD) != 0) {
    return;
  }
  
  emit_default_args = true;
  use_header();
  node->return_type->accept(this);
  (*ss) << ' ' <<node->name.value << ' ';
  node->params->accept(this);
  (*ss) << ";\n";
  use_code();
  emit_default_args = false;
}

void EmitVisitor::emit_local_function(ASTFunctionDeclaration *node) {
  // creates a constexpr auto function = []() -> return_type { ... };
  // these are alwyas constexpr because we do not do closure objects, nor do we have the ability to check that right now.
  // TODO: we could have an option for like #closure to just use & and hope that it works fine. It should work fine
  (*ss) << indent() <<  "constexpr auto " << node->name.value << " = []";   
  node->params->accept(this);
  (*ss) << " -> ";
  node->return_type->accept(this);
  if (node->block.is_null()) {
    throw_error("local function cannot be #foreign", ERROR_FAILURE , node->source_range);
  }
  node->block.get()->accept(this);
}

void EmitVisitor::emit_foreign_function(ASTFunctionDeclaration *node) {
  if (node->name.value == "main") {
    throw_error("main function cannot be foreign", ERROR_CRITICAL,
                node->source_range);
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
  
  auto last_func_decl = current_func_decl;
  current_func_decl = node;
  
  auto test_flag = get_compilation_flag("test");
  
  Defer deferred = { [&](){ current_func_decl = last_func_decl; } };
  
  // this also happens to emit the test boilerplate that bootstraps it into the test runner, if applicable.
  if (!should_emit_function(this, node, test_flag)) {
    return {};
  }
  
  auto symbol = context.current_scope->lookup(node->name.value);
  
  // for #foreign declarations  
  if (node->meta_type == FunctionMetaType::FUNCTION_TYPE_FOREIGN) {
    emit_foreign_function(node);
    return {};
  }
  
  // local function
  if (!context.current_scope->is_struct_scope && context.current_scope != context.root_scope && (node->flags & FUNCTION_IS_METHOD) == 0) {
    emit_local_function(node);
    return {};
  }
  
  if ((node->flags & FUNCTION_IS_DTOR) != 0) {
    auto name = current_struct_decl.get()->type->base;
    (*ss) << '~' << name;
    node->params->accept(this);
    if (!node->block) {
      throw_error("Cannot forward declare a constructor", ERROR_FAILURE, node->source_range);
    }
    node->block.get()->accept(this);
    return {};
  }
  
  if ((node->flags & FUNCTION_IS_CTOR) != 0) {
    auto name = current_struct_decl.get()->type->base;
    (*ss) << name;
    
    
    // This is a copy constructor, so we need to pass the parameter by reference.
    // TODO: add a nicer way to take args by reference, or have a good way to dicate when to do so implicitly, if we want that ::  2024-09-29 14:02:16
    if (node->params->params.size() == 1 && node->params->params[0]->type->resolved_type == current_struct_decl.get()->type->resolved_type) {
      (*ss) << "(" << name << " &" << node->params->params[0]->name << ")";
    } else {
      node->params->accept(this);
    }
    
    if (!node->block) {
      throw_error("Cannot forward declare a constructor", ERROR_FAILURE, node->source_range);
    }
    node->block.get()->accept(this);
    return {};
  }
  
  // we override main's return value to allow compilation without explicitly returning int from main.
  if (node->name.value == "main") {
    (*ss) <<"int";
  } else {
    node->return_type->accept(this);
  }
  
  // emit parameter signature && name.
  (*ss) << " " + node->name.value;
  node->params->accept(this);
  
  // the function's block would only be null in a #foreign function
  if (node->block.is_not_null()) 
    node->block.get()->accept(this);
  
  // emit a forward declaration in the header to allow use-before-defined.
  // main is not forward declared.
  if (node->name.value != "main") {
    emit_forward_declaration(node);
  }
  
  return {};
}

std::any EmitVisitor::visit(ASTBlock *node) {
  (*ss) <<(" {\n");
  indentLevel++;
  context.enter_scope(node->scope);
  for (const auto &statement : node->statements) {
    if (dynamic_cast<ASTDeclaration*>(statement)){
      indented("");
    }
    
    statement->accept(this);
    
    semicolon();
    newline();
  }
  indentLevel--;
  indented("}");
  context.exit_scope();
  return {};
}

std::any EmitVisitor::visit(ASTProgram *node) {
  const auto testing = get_compilation_flag("test");
  
  header << "#include \"/usr/local/lib/ela/boilerplate.hpp\"\n";
  
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
       
    code << std::format("__COMPILER_GENERATED_TEST tests[{}] = {}\n", num_tests, "{ " + test_init + " };");
    
    code << "__TEST_RUNNER_MAIN;";
  }
  
  // Emit runtime reflection type info for requested types 
  {
    std::stringstream type_info{};
    for (const auto &str: context.type_info_strings) {
      type_info << str;
      if (str != context.type_info_strings.back()) {
        type_info << ", ";
      }
    }
    header << std::format("static Type** _type_info = []{{ Type **_type_info = new Type*[{}]; {}; return _type_info; }}();", num_types, type_info.str());
  }
  
  return {};
}


std::any EmitVisitor::visit(ASTStructDeclaration *node) {
  auto type = get_type(node->type->resolved_type);
  auto info = static_cast<StructTypeInfo*>(type->info.get());
  
  current_struct_decl = node;
  
  Defer deferred([&]{
    current_struct_decl = nullptr;
  });
  
  if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) != 0) {
    header << "struct " << node->type->base << ";\n";  
    return {};
  }
  
  (*ss) << "struct " << node->type->base << "{\n";
  header << "struct " << node->type->base << ";\n";
  indentLevel++;
  
  context.enter_scope(node->scope);
  for (const auto &decl: node->fields) {
    indented("");
    decl->accept(this);
    semicolon();
    newline();
  }
  for (const auto &method: node->methods) {
    indented("");
    method->accept(this);
    semicolon();
    newline();
  }
  context.exit_scope();
  
  (*ss) << "};\n";
  indentLevel--;
  return {};
}

std::any EmitVisitor::visit(ASTDotExpr *node) {
  auto left_ty = std::any_cast<int>(node->left->accept(&type_visitor));
  auto left_type = get_type(left_ty);
  
  auto op = ".";
  if (left_type->extensions.is_pointer()) 
    op = "->";
  
  auto left_info = static_cast<StructTypeInfo*>(left_type->info.get());;
  
  auto previous_scope = context.current_scope;
  
  context.enter_scope(left_info->scope);
  node->left->accept(this);
  (*ss) << (op);
  node->right->accept(this);
  context.current_scope = previous_scope;
  
  return {};
}


std::any EmitVisitor::visit(ASTMake *node) {
  // TODO: make this robust to casting all types, copy construction only right now :: 2024-09-29 13:34:16
  auto type = get_type(node->type_arg->resolved_type);
  if (node->kind == MAKE_CAST) {
    if (node->arguments->arguments.empty()) {
      throw_error("cannot create a pointer currently with #make. it only casts pointers.", ERROR_FAILURE, node->source_range);
    }
    (*ss) << "(" << type->to_cpp_string() << ")";
    node->arguments->arguments[0]->accept(this);
  } else if (node->kind == MAKE_CTOR || node->kind == MAKE_COPY_CTOR) {
    (*ss) << type->to_cpp_string();
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
  for (const auto &expr: node->expressions) {
    expr->accept(this);
    if (expr != node->expressions.back()) {
      (*ss) << ", ";
    }
  }
  (*ss) << "}";
  return {};
}
