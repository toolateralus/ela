#pragma once

#include "ast.hpp"

struct ASTCopier {
  ASTProgram* copy_program(ASTProgram* node) {
    if (!node) return nullptr;
    auto new_node = new ASTProgram(*node);
    new_node->statements.clear();
    for (auto stmt : node->statements) {
      if (stmt->get_node_type() == AST_NODE_NOOP) continue;
      new_node->statements.push_back(static_cast<ASTStatement*>(copy_node(stmt)));
    }
    return new_node;
  }

  ASTBlock* copy_block(ASTBlock* node) {
    if (!node) return nullptr;
    auto new_node = new ASTBlock(*node);
    new_node->statements.clear();
    for (auto stmt : node->statements) {
      new_node->statements.push_back(static_cast<ASTStatement*>(copy_node(stmt)));
    }
    return new_node;
  }

  ASTFunctionDeclaration* copy_function_declaration(ASTFunctionDeclaration* node) {
    if (!node) return nullptr;
    auto new_node = new ASTFunctionDeclaration(*node);
    new_node->params = static_cast<ASTParamsDecl*>(copy_node(node->params));
    if (node->block) new_node->block = static_cast<ASTBlock*>(copy_node(node->block.get()));
    return new_node;
  }

  ASTParamsDecl* copy_params_decl(ASTParamsDecl* node) {
    if (!node) return nullptr;
    auto new_node = new ASTParamsDecl(*node);
    new_node->params.clear();
    for (auto param : node->params) {
      new_node->params.push_back(static_cast<ASTParamDecl*>(copy_node(param)));
    }
    return new_node;
  }

  ASTParamDecl* copy_param_decl(ASTParamDecl* node) {
    if (!node) return nullptr;
    auto new_node = new ASTParamDecl(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    if (node->default_value) new_node->default_value = static_cast<ASTExpr*>(copy_node(node->default_value.get()));
    return new_node;
  }

  ASTDeclaration* copy_declaration(ASTDeclaration* node) {
    if (!node) return nullptr;
    auto new_node = new ASTDeclaration(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    if (node->value) new_node->value = static_cast<ASTExpr*>(copy_node(node->value.get()));
    return new_node;
  }

  ASTExprStatement* copy_expr_statement(ASTExprStatement* node) {
    if (!node) return nullptr;
    auto new_node = new ASTExprStatement(*node);
    new_node->expression = static_cast<ASTExpr*>(copy_node(node->expression));
    return new_node;
  }

  ASTBinExpr* copy_bin_expr(ASTBinExpr* node) {
    if (!node) return nullptr;
    auto new_node = new ASTBinExpr(*node);
    new_node->left = static_cast<ASTExpr*>(copy_node(node->left));
    new_node->right = static_cast<ASTExpr*>(copy_node(node->right));
    return new_node;
  }

  ASTUnaryExpr* copy_unary_expr(ASTUnaryExpr* node) {
    if (!node) return nullptr;
    auto new_node = new ASTUnaryExpr(*node);
    new_node->operand = static_cast<ASTExpr*>(copy_node(node->operand));
    return new_node;
  }

  ASTIdentifier* copy_identifier(ASTIdentifier* node) {
    if (!node) return nullptr;
    return new ASTIdentifier(*node);
  }

  ASTLiteral* copy_literal(ASTLiteral* node) {
    if (!node) return nullptr;
    auto new_node = new ASTLiteral(*node);
    new_node->interpolated_values.clear();
    for (auto expr : node->interpolated_values) {
      new_node->interpolated_values.push_back(static_cast<ASTExpr*>(copy_node(expr)));
    }
    return new_node;
  }

  ASTType* copy_type(ASTType* node) {
    if (!node) return nullptr;
    auto new_node = new ASTType(*node);
    if (node->pointing_to) new_node->pointing_to = static_cast<ASTType*>(copy_node(node->pointing_to.get()));
    new_node->tuple_types.clear();
    for (auto type : node->tuple_types) {
      new_node->tuple_types.push_back(static_cast<ASTType*>(copy_node(type)));
    }
    return new_node;
  }

  ASTCall* copy_call(ASTCall* node) {
    if (!node) return nullptr;
    auto new_node = new ASTCall(*node);
    new_node->function = static_cast<ASTExpr*>(copy_node(node->function));
    new_node->arguments = static_cast<ASTArguments*>(copy_node(node->arguments));
    return new_node;
  }

  ASTArguments* copy_arguments(ASTArguments* node) {
    if (!node) return nullptr;
    auto new_node = new ASTArguments(*node);
    new_node->arguments.clear();
    for (auto arg : node->arguments) {
      new_node->arguments.push_back(static_cast<ASTExpr*>(copy_node(arg)));
    }
    return new_node;
  }

  ASTReturn* copy_return(ASTReturn* node) {
    if (!node) return nullptr;
    auto new_node = new ASTReturn(*node);
    if (node->expression) new_node->expression = static_cast<ASTExpr*>(copy_node(node->expression.get()));
    return new_node;
  }

  ASTContinue* copy_continue(ASTContinue* node) {
    if (!node) return nullptr;
    return new ASTContinue(*node);
  }

  ASTBreak* copy_break(ASTBreak* node) {
    if (!node) return nullptr;
    return new ASTBreak(*node);
  }

  ASTFor* copy_for(ASTFor* node) {
    if (!node) return nullptr;
    auto new_node = new ASTFor(*node);
    new_node->iden = static_cast<ASTIdentifier*>(copy_node(node->iden));
    new_node->range = static_cast<ASTRange*>(copy_node(node->range));
    new_node->block = static_cast<ASTBlock*>(copy_node(node->block));
    return new_node;
  }

  ASTIf* copy_if(ASTIf* node) {
    if (!node) return nullptr;
    auto new_node = new ASTIf(*node);
    new_node->condition = static_cast<ASTExpr*>(copy_node(node->condition));
    new_node->block = static_cast<ASTBlock*>(copy_node(node->block));
    if (node->_else) new_node->_else = static_cast<ASTElse*>(copy_node(node->_else.get()));
    return new_node;
  }

  ASTElse* copy_else(ASTElse* node) {
    if (!node) return nullptr;
    auto new_node = new ASTElse(*node);
    if (node->_if) new_node->_if = static_cast<ASTIf*>(copy_node(node->_if.get()));
    if (node->block) new_node->block = static_cast<ASTBlock*>(copy_node(node->block.get()));
    return new_node;
  }

  ASTWhile* copy_while(ASTWhile* node) {
    if (!node) return nullptr;
    auto new_node = new ASTWhile(*node);
    if (node->condition) new_node->condition = static_cast<ASTExpr*>(copy_node(node->condition.get()));
    new_node->block = static_cast<ASTBlock*>(copy_node(node->block));
    return new_node;
  }

  ASTStructDeclaration* copy_struct_declaration(ASTStructDeclaration* node) {
    if (!node) return nullptr;
    auto new_node = new ASTStructDeclaration(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    new_node->fields.clear();
    for (auto field : node->fields) {
      new_node->fields.push_back(static_cast<ASTDeclaration*>(copy_node(field)));
    }
    new_node->methods.clear();
    for (auto method : node->methods) {
      new_node->methods.push_back(static_cast<ASTFunctionDeclaration*>(copy_node(method)));
    }
    return new_node;
  }

  ASTDotExpr* copy_dot_expr(ASTDotExpr* node) {
    if (!node) return nullptr;
    auto new_node = new ASTDotExpr(*node);
    new_node->base = static_cast<ASTExpr*>(copy_node(node->base));
    return new_node;
  }

  ASTSubscript* copy_subscript(ASTSubscript* node) {
    if (!node) return nullptr;
    auto new_node = new ASTSubscript(*node);
    new_node->left = static_cast<ASTExpr*>(copy_node(node->left));
    new_node->subscript = static_cast<ASTExpr*>(copy_node(node->subscript));
    return new_node;
  }

  ASTMake* copy_make(ASTMake* node) {
    if (!node) return nullptr;
    auto new_node = new ASTMake(*node);
    new_node->type_arg = static_cast<ASTType*>(copy_node(node->type_arg));
    new_node->arguments = static_cast<ASTArguments*>(copy_node(node->arguments));
    return new_node;
  }

  ASTInitializerList* copy_initializer_list(ASTInitializerList* node) {
    if (!node) return nullptr;
    auto new_node = new ASTInitializerList(*node);
    new_node->expressions.clear();
    for (auto expr : node->expressions) {
      new_node->expressions.push_back(static_cast<ASTExpr*>(copy_node(expr)));
    }
    return new_node;
  }

  ASTEnumDeclaration* copy_enum_declaration(ASTEnumDeclaration* node) {
    if (!node) return nullptr;
    auto new_node = new ASTEnumDeclaration(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    new_node->key_values.clear();
    for (auto &kv : node->key_values) {
      new_node->key_values.push_back({kv.first, kv.second.is_not_null() ? (ASTExpr*)copy_node(kv.second.get()) : nullptr});
    }
    return new_node;
  }

  ASTUnionDeclaration* copy_union_declaration(ASTUnionDeclaration* node) {
    if (!node) return nullptr;
    auto new_node = new ASTUnionDeclaration(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    new_node->fields.clear();
    for (auto field : node->fields) {
      new_node->fields.push_back(static_cast<ASTDeclaration*>(copy_node(field)));
    }
    new_node->methods.clear();
    for (auto method : node->methods) {
      new_node->methods.push_back(static_cast<ASTFunctionDeclaration*>(copy_node(method)));
    }
    new_node->structs.clear();
    for (auto strct : node->structs) {
      new_node->structs.push_back(static_cast<ASTStructDeclaration*>(copy_node(strct)));
    }
    return new_node;
  }

  ASTAllocate* copy_allocate(ASTAllocate* node) {
    if (!node) return nullptr;
    auto new_node = new ASTAllocate(*node);
    if (node->type) new_node->type = static_cast<ASTType*>(copy_node(node->type.get()));
    if (node->arguments) new_node->arguments = static_cast<ASTArguments*>(copy_node(node->arguments.get()));
    return new_node;
  }

  ASTRange* copy_range(ASTRange* node) {
    if (!node) return nullptr;
    auto new_node = new ASTRange(*node);
    new_node->left = static_cast<ASTExpr*>(copy_node(node->left));
    new_node->right = static_cast<ASTExpr*>(copy_node(node->right));
    return new_node;
  }

  ASTSwitch* copy_switch(ASTSwitch* node) {
    if (!node) return nullptr;
    auto new_node = new ASTSwitch(*node);
    new_node->target = static_cast<ASTExpr*>(copy_node(node->target));
    new_node->cases.clear();
    for (auto &case_ : node->cases) {
      new_node->cases.push_back({static_cast<ASTExpr*>(copy_node(case_.expression)), static_cast<ASTBlock*>(copy_node(case_.block))});
    }
    return new_node;
  }

  ASTTuple* copy_tuple(ASTTuple* node) {
    if (!node) return nullptr;
    auto new_node = new ASTTuple(*node);
    new_node->type = static_cast<ASTType*>(copy_node(node->type));
    new_node->values.clear();
    for (auto value : node->values) {
      new_node->values.push_back(static_cast<ASTExpr*>(copy_node(value)));
    }
    return new_node;
  }

  ASTTupleDeconstruction* copy_tuple_deconstruction(ASTTupleDeconstruction* node) {
    if (!node) return nullptr;
    auto new_node = new ASTTupleDeconstruction(*node);
    new_node->idens.clear();
    for (auto iden : node->idens) {
      new_node->idens.push_back(static_cast<ASTIdentifier*>(copy_node(iden)));
    }
    new_node->right = static_cast<ASTExpr*>(copy_node(node->right));
    return new_node;
  }

  ASTScopeResolution* copy_scope_resolution(ASTScopeResolution* node) {
    if (!node) return nullptr;
    auto new_node = new ASTScopeResolution(*node);
    new_node->base = static_cast<ASTExpr*>(copy_node(node->base));
    return new_node;
  }

  ASTNode* copy_node(ASTNode* node) {
    if (!node) return nullptr;
    switch (node->get_node_type()) {
      case AST_NODE_PROGRAM:
        return copy_program(static_cast<ASTProgram*>(node));
      case AST_NODE_BLOCK:
        return copy_block(static_cast<ASTBlock*>(node));
      case AST_NODE_FUNCTION_DECLARATION:
        return copy_function_declaration(static_cast<ASTFunctionDeclaration*>(node));
      case AST_NODE_PARAMS_DECL:
        return copy_params_decl(static_cast<ASTParamsDecl*>(node));
      case AST_NODE_PARAM_DECL:
        return copy_param_decl(static_cast<ASTParamDecl*>(node));
      case AST_NODE_DECLARATION:
        return copy_declaration(static_cast<ASTDeclaration*>(node));
      case AST_NODE_EXPR_STATEMENT:
        return copy_expr_statement(static_cast<ASTExprStatement*>(node));
      case AST_NODE_BIN_EXPR:
        return copy_bin_expr(static_cast<ASTBinExpr*>(node));
      case AST_NODE_UNARY_EXPR:
        return copy_unary_expr(static_cast<ASTUnaryExpr*>(node));
      case AST_NODE_IDENTIFIER:
        return copy_identifier(static_cast<ASTIdentifier*>(node));
      case AST_NODE_LITERAL:
        return copy_literal(static_cast<ASTLiteral*>(node));
      case AST_NODE_TYPE:
        return copy_type(static_cast<ASTType*>(node));
      case AST_NODE_TUPLE:
        return copy_tuple(static_cast<ASTTuple*>(node));
      case AST_NODE_CALL:
        return copy_call(static_cast<ASTCall*>(node));
      case AST_NODE_ARGUMENTS:
        return copy_arguments(static_cast<ASTArguments*>(node));
      case AST_NODE_RETURN:
        return copy_return(static_cast<ASTReturn*>(node));
      case AST_NODE_CONTINUE:
        return copy_continue(static_cast<ASTContinue*>(node));
      case AST_NODE_BREAK:
        return copy_break(static_cast<ASTBreak*>(node));
      case AST_NODE_FOR:
        return copy_for(static_cast<ASTFor*>(node));
      case AST_NODE_IF:
        return copy_if(static_cast<ASTIf*>(node));
      case AST_NODE_ELSE:
        return copy_else(static_cast<ASTElse*>(node));
      case AST_NODE_WHILE:
        return copy_while(static_cast<ASTWhile*>(node));
      case AST_NODE_STRUCT_DECLARATION:
        return copy_struct_declaration(static_cast<ASTStructDeclaration*>(node));
      case AST_NODE_DOT_EXPR:
        return copy_dot_expr(static_cast<ASTDotExpr*>(node));
      case AST_NODE_SCOPE_RESOLUTION:
        return copy_scope_resolution(static_cast<ASTScopeResolution*>(node));
      case AST_NODE_SUBSCRIPT:
        return copy_subscript(static_cast<ASTSubscript*>(node));
      case AST_NODE_MAKE:
        return copy_make(static_cast<ASTMake*>(node));
      case AST_NODE_INITIALIZER_LIST:
        return copy_initializer_list(static_cast<ASTInitializerList*>(node));
      case AST_NODE_ENUM_DECLARATION:
        return copy_enum_declaration(static_cast<ASTEnumDeclaration*>(node));
      case AST_NODE_UNION_DECLARATION:
        return copy_union_declaration(static_cast<ASTUnionDeclaration*>(node));
      case AST_NODE_ALLOCATE:
        return copy_allocate(static_cast<ASTAllocate*>(node));
      case AST_NODE_RANGE:
        return copy_range(static_cast<ASTRange*>(node));
      case AST_NODE_SWITCH:
        return copy_switch(static_cast<ASTSwitch*>(node));
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return copy_tuple_deconstruction(static_cast<ASTTupleDeconstruction*>(node));
      default:
        return nullptr;
    }
  }
};

static inline ASTNode *deep_copy_ast(ASTNode *root) {
  ASTCopier copier;
  return copier.copy_node(root);
}