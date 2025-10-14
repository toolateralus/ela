#include "typer.hpp"
#include "ast.hpp"
#include "thir_interpreter.hpp"

void Typisting::visit_block(ASTBlock *) {}
void Typisting::visit_alias(ASTAlias *) {}
void Typisting::visit_impl(ASTImpl *) {}
void Typisting::visit_import(ASTImport *) {}
void Typisting::visit_module(ASTModule *) {}
void Typisting::visit_return(ASTReturn *) {}
void Typisting::visit_continue(ASTContinue *) {}
void Typisting::visit_break(ASTBreak *) {}
void Typisting::visit_for(ASTFor *) {}
void Typisting::visit_if(ASTIf *) {}
void Typisting::visit_else(ASTElse *) {}
void Typisting::visit_while(ASTWhile *) {}

void Typisting::visit_enum_declaration(ASTEnumDeclaration *node) {
  Scope *scope = node->declaring_scope;

  if (scope->find_type(node->name, {}, false, {}) != Type::INVALID_TYPE) {
    throw_error("Redefinition of enum " + node->name.str(), node->source_range);
  }

  auto underlying_type = Type::INVALID_TYPE;

  if (node->underlying_type_ast) {
    visit_node(node->underlying_type_ast);
    underlying_type = node->underlying_type_ast->resolved_type;
  }

  auto enum_type = scope->create_enum_type(node->name, node->is_flags, node);
  enum_type->declaring_node = node;
  auto info = enum_type->info->as<EnumTypeInfo>();

  for (auto &[key, value] : node->key_values) {
    visit_node(value);
    Type *node_ty = value->resolved_type;

    Value *evaluated = interpret_from_ast(value);
    size_t evaluated_integer = evaluated->as<IntValue>()->value;

    constexpr uint64_t u32_max = 0xFFFFFFFFull;
    constexpr int64_t s32_max = 0x7FFFFFFF;
    constexpr int64_t s32_min = -0x80000000;

    // Why would we change the sign only if it reaches greater than s32, default should be u8 -> u16 -> u32
    // based on the number of enumerations.
    if (evaluated_integer > s32_max || evaluated_integer < s32_min) {
      if (evaluated_integer <= u32_max) {
        underlying_type = u32_type();
      } else {
        underlying_type = u64_type();
      }
    }

    TypeMember member = {.name = key, .type = node_ty, .default_value = value};

    info->members.push_back(member);

    if (underlying_type == Type::INVALID_TYPE) {
      underlying_type = node_ty;
    } else {
      assert_types_can_cast_or_are_equal(value, underlying_type, node->source_range,
                                         "inconsistent types in enum declaration.");
    }
  }

  if (underlying_type == void_type()) {
    throw_error("Invalid enum declaration: got void for underlying type", node->source_range);
  }

  node->underlying_type = underlying_type;
  info->underlying_type = underlying_type;
  node->resolved_type = enum_type;
}

void Typisting::visit_function_declaration(ASTFunctionDeclaration *) {}
void Typisting::visit_struct_declaration(ASTStructDeclaration *) {}
void Typisting::visit_choice_declaration(ASTChoiceDeclaration *) {}
void Typisting::visit_trait_declaration(ASTTraitDeclaration *) {}
void Typisting::visit_variable(ASTVariable *) {}
void Typisting::visit_expr_statement(ASTExprStatement *) {}
void Typisting::visit_bin_expr(ASTBinExpr *) {}
void Typisting::visit_unary_expr(ASTUnaryExpr *) {}
void Typisting::visit_literal(ASTLiteral *) {}
void Typisting::visit_path(ASTPath *) {}
void Typisting::visit_type(ASTType *) {}
void Typisting::visit_tuple(ASTTuple *) {}
void Typisting::visit_call(ASTCall *) {}
void Typisting::visit_method_call(ASTMethodCall *) {}
void Typisting::visit_arguments(ASTArguments *) {}
void Typisting::visit_dot_expr(ASTDotExpr *) {}
void Typisting::visit_index(ASTIndex *) {}
void Typisting::visit_initializer_list(ASTInitializerList *) {}
void Typisting::visit_size_of(ASTSize_Of *) {}
void Typisting::visit_type_of(ASTType_Of *) {}
void Typisting::visit_dyn_of(ASTDyn_Of *) {}
void Typisting::visit_defer(ASTDefer *) {}
void Typisting::visit_cast(ASTCast *) {}
void Typisting::visit_lambda(ASTLambda *) {}
void Typisting::visit_unpack(ASTUnpack *) {}
void Typisting::visit_unpack_element(ASTUnpackElement *) {}
void Typisting::visit_range(ASTRange *) {}
void Typisting::visit_switch(ASTSwitch *) {}
void Typisting::visit_destructure(ASTDestructure *) {}
void Typisting::visit_where(ASTWhere *) {}
void Typisting::visit_pattern_match(ASTPatternMatch *) {}
void Typisting::visit_statement_list(ASTStatementList *) {}
void Typisting::visit_where_statement(ASTWhereStatement *) {}
void Typisting::visit_run(ASTRun *) {}

void Typisting::process_type_headers() {
  for (auto *decl : struct_declarations) {
    Type *type = decl->declaring_scope->create_struct_type(decl->name, decl, {}, decl->generic_parameters.size());
    decl->resolved_type = type;
  }

  for (auto *decl : trait_declarations) {
    Type *type = decl->declaring_scope->create_trait_type(decl->name, decl, {}, decl->generic_parameters.size());
    decl->resolved_type = type;
  }

  for (auto *decl : choice_declarations) {
    Type *type = decl->declaring_scope->create_choice_type(decl->name, decl, {}, decl->generic_parameters.size());
    decl->resolved_type = type;
  }

  // since enums can have out-of-order dependencies such as constants in types
  // or global variables we can't just define it straight away.
  for (auto *decl : enum_declarations) {
    Type *type = decl->declaring_scope->create_enum_type(decl->name, decl, {});
    decl->resolved_type = type;
  }

  for (auto *decl : alias_declarations) {
    decl->declaring_scope->create_type_alias(decl->name, nullptr, decl, {}, decl->generic_parameters.size());
  }
}

void Typisting::collect_declarations(ASTNode *node) {
  if (!node) return;
  switch (node->get_node_type()) {
    case AST_NODE_STATEMENT_LIST: {
      for (const auto &stmt : ((ASTStatementList *)node)->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_STRUCT_DECLARATION:
      struct_declarations.push_back((ASTStructDeclaration *)(node));
      break;
    case AST_NODE_TRAIT_DECLARATION:
      trait_declarations.push_back((ASTTraitDeclaration *)(node));
      break;
    case AST_NODE_CHOICE_DECLARATION:
      choice_declarations.push_back((ASTChoiceDeclaration *)(node));
      break;
    case AST_NODE_ENUM_DECLARATION:
      enum_declarations.push_back((ASTEnumDeclaration *)(node));
      break;
    case AST_NODE_FUNCTION_DECLARATION:
      function_declarations.push_back((ASTFunctionDeclaration *)(node));
      break;
    case AST_NODE_ALIAS:
      alias_declarations.push_back((ASTAlias *)(node));
      break;
    case AST_NODE_IMPL:
      impl_declarations.push_back((ASTImpl *)(node));
      break;
    case AST_NODE_PROGRAM: {
      auto *program = (ASTProgram *)(node);
      for (auto *stmt : program->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_BLOCK: {
      auto *block = (ASTBlock *)(node);
      for (auto *stmt : block->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_MODULE: {
      auto *module = (ASTModule *)(node);
      for (auto *stmt : module->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_IF: {
      auto *if_stmt = (ASTIf *)(node);
      collect_declarations(if_stmt->block);
      if (if_stmt->_else.ptr) {
        collect_declarations(if_stmt->_else.ptr);
      }
      break;
    }
    case AST_NODE_FOR: {
      auto *for_stmt = (ASTFor *)(node);
      collect_declarations(for_stmt->block);
      break;
    }
    case AST_NODE_WHILE: {
      auto *while_stmt = (ASTWhile *)(node);
      collect_declarations(while_stmt->block);
      break;
    }
    default:
      break;
  }
}