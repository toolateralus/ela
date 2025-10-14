#include "symbol_pass.hpp"
#include "ast.hpp"
#include "thir_interpreter.hpp"
#include "visitor.hpp"

void SymbolPass::collect_declarations(ASTNode* node) {
  if (!node) return;
  switch (node->get_node_type()) {
    case AST_NODE_STATEMENT_LIST: {
      for (const auto& stmt : ((ASTStatementList*)node)->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_STRUCT_DECLARATION:
      struct_declarations.push_back((ASTStructDeclaration*)(node));
      break;
    case AST_NODE_TRAIT_DECLARATION:
      trait_declarations.push_back((ASTTraitDeclaration*)(node));
      break;
    case AST_NODE_CHOICE_DECLARATION:
      choice_declarations.push_back((ASTChoiceDeclaration*)(node));
      break;
    case AST_NODE_ENUM_DECLARATION:
      enum_declarations.push_back((ASTEnumDeclaration*)(node));
      break;
    case AST_NODE_FUNCTION_DECLARATION:
      function_declarations.push_back((ASTFunctionDeclaration*)(node));
      break;
    case AST_NODE_ALIAS:
      alias_declarations.push_back((ASTAlias*)(node));
      break;
    case AST_NODE_IMPL:
      impl_declarations.push_back((ASTImpl*)(node));
      break;
    case AST_NODE_PROGRAM: {
      auto* program = (ASTProgram*)(node);
      for (auto* stmt : program->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_BLOCK: {
      auto* block = (ASTBlock*)(node);
      for (auto* stmt : block->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_MODULE: {
      auto* module = (ASTModule*)(node);
      for (auto* stmt : module->statements) {
        collect_declarations(stmt);
      }
      break;
    }
    case AST_NODE_IF: {
      auto* if_stmt = (ASTIf*)(node);
      collect_declarations(if_stmt->block);
      if (if_stmt->_else.ptr) {
        collect_declarations(if_stmt->_else.ptr);
      }
      break;
    }
    case AST_NODE_FOR: {
      auto* for_stmt = (ASTFor*)(node);
      collect_declarations(for_stmt->block);
      break;
    }
    case AST_NODE_WHILE: {
      auto* while_stmt = (ASTWhile*)(node);
      collect_declarations(while_stmt->block);
      break;
    }
    default:
      break;
  }
}

void SymbolPass::process_type_declarations() {
  for (auto* decl : struct_declarations) {
    auto type = decl->declaring_scope->create_struct_type(decl->name, decl->scope, decl);
    auto info = type->info->as<StructTypeInfo>();
    info->is_forward_declared = true;
    decl->resolved_type = type;
  }
  for (auto* decl : trait_declarations) {
    auto type = decl->declaring_scope->create_trait_type(decl->name, decl->scope, {}, decl);
    decl->resolved_type = type;
  }
  for (auto* decl : choice_declarations) {
    auto type = decl->declaring_scope->create_choice_type(decl->name, decl->scope, decl);
    decl->resolved_type = type;
  }

  for (auto* decl : enum_declarations) {
    typer.visit(decl);  // We just visit enum declarations, because they cannot have any dependencies.
  }
  enum_declarations.clear();

  for (auto* decl : alias_declarations) {
    decl->declaring_scope->create_type_alias(decl->name, nullptr, decl);
  }
}

void SymbolPass::process_function_headers() {}

void SymbolPass::type_enum(ASTEnumDeclaration* node) {
  Scope* scope = node->declaring_scope;

  if (scope->find_type(node->name, {}) != Type::INVALID_TYPE) {
    throw_error("Redefinition of enum " + node->name.get_str(), node->source_range);
  }

  auto underlying_type = Type::INVALID_TYPE;

  if (node->underlying_type_ast) {
    node->underlying_type_ast->accept(&typer);
    underlying_type = node->underlying_type_ast->resolved_type;
  }

  auto enum_type = scope->create_enum_type(node->name, create_child(scope), node->is_flags, node);
  enum_type->declaring_node = node;
  auto info = enum_type->info->as<EnumTypeInfo>();

  for (auto& [key, value] : node->key_values) {
    value->accept(&typer);
    Type* node_ty = value->resolved_type;

    // We need to adjust the underlying type for larger-than-default (s32) values.
    // C will infer this, but we need to be explicit and not rely on any target language stuff.
    // we upcast to unsigned to get the largest maximum value for huge values
    Value* evaluated = interpret_from_ast(value, typer.ctx);
    size_t evaluated_integer = evaluated->as<IntValue>()->value;

    constexpr uint64_t u32_max = 0xFFFFFFFFull;
    constexpr int64_t s32_max = 0x7FFFFFFF;
    constexpr int64_t s32_min = -0x80000000;
    if (evaluated_integer > s32_max || evaluated_integer < s32_min) {
      if (evaluated_integer <= u32_max) {
        underlying_type = u32_type();
      } else {
        underlying_type = u64_type();
      }
    }

    info->scope->insert(key, node_ty, value, CONST);
    info->members.push_back({key, node_ty, value, nullptr});
    if (underlying_type == Type::INVALID_TYPE) {
      underlying_type = node_ty;
    } else {
      assert_types_can_cast_or_are_equal(value, underlying_type, node->source_range,
                                     "inconsistent types in enum declaration.");
    }
  }

  if (underlying_type == void_type()) {
    throw_error("Invalid enum declaration.. got null or no type.", node->source_range);
  }

  node->underlying_type = underlying_type;
  info->underlying_type = underlying_type;
  node->resolved_type = enum_type;
}

void SymbolPass::run(ASTProgram* ast) {
  collect_declarations(ast);

  for (auto* func : function_declarations) {
    printf("inserting function %s\n", func->name.get_str().c_str());
    func->declaring_scope->insert_function(func->name, nullptr, func);
  }

  process_type_declarations();

  for (auto* func : function_declarations) {
    typer.visit(func);
  }

  for (auto* decl : struct_declarations) {
    typer.visit(decl);
  }

  for (auto* decl : trait_declarations) {
    typer.visit(decl);
  }

  for (auto* decl : choice_declarations) {
    typer.visit(decl);
  }

  for (auto* decl : enum_declarations) {
    typer.visit(decl);
  }

  for (auto* decl : alias_declarations) {
    typer.visit(decl);
  }
}
