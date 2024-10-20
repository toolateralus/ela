#pragma once
#include "ast.hpp"
#include "scope.hpp"

struct Copier {
  ASTNode* copy(ASTNode* node) {
    if (!node) {
      return nullptr;
    }
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
      case AST_NODE_TUPLE:
        return copy_tuple(static_cast<ASTTuple*>(node));
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return copy_tuple_deconstruction(static_cast<ASTTupleDeconstruction*>(node));
      case AST_NODE_NOOP: return node; // We don't need to clone noops.
      default:
        throw_error("Failed to copy node: ", node->source_range);
    }
  }

  ASTProgram* copy_program(ASTProgram* node) {
    auto program = ast_alloc<ASTProgram>();
    for (const auto& statement : node->statements) {
      program->statements.push_back(static_cast<ASTStatement*>(copy(statement)));
    }
    program->source_range = node->source_range;
    return program;
  }

  Scope* copy_scope(Scope* scope) {
    auto new_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    new_scope->is_struct_or_union_scope = scope->is_struct_or_union_scope;
    new_scope->aliases = scope->aliases;
    new_scope->ordered_symbols = scope->ordered_symbols;
    new_scope->parent = scope->parent;
    new_scope->symbols = scope->symbols;
    return new_scope;
  }

  ASTBlock* copy_block(ASTBlock* node) {
    auto block = ast_alloc<ASTBlock>();
    for (const auto& statement : node->statements) {
      auto _node = dynamic_cast<ASTStatement*>(copy(statement));
      if (!node){
        throw_error("Internal compiler error: Copier failed to copy block statement", node->source_range);
      }
      block->statements.push_back(_node);
    }
    
    
    
    block->flags = node->flags;
    block->return_type = node->return_type;
    block->scope = copy_scope(node->scope);
    block->source_range = node->source_range;
    return block;
  }

  ASTFunctionDeclaration* copy_function_declaration(ASTFunctionDeclaration* node) {
    auto function = ast_alloc<ASTFunctionDeclaration>();
    function->block = static_cast<ASTBlock*>(copy(node->block.get()));
    function->flags = node->flags;
    function->return_type = node->return_type;
    function->meta_type = node->meta_type;
    function->name = node->name;
    function->params = static_cast<ASTParamsDecl*>(copy(node->params));
    function->source_range = node->source_range;
    return function;
  }

  ASTParamsDecl* copy_params_decl(ASTParamsDecl* node) {
    auto params = ast_alloc<ASTParamsDecl>();
    for (const auto& param : node->params) {
      params->params.push_back(static_cast<ASTParamDecl*>(copy(param)));
    }
    params->source_range = node->source_range;
    return params;
  }

  ASTParamDecl* copy_param_decl(ASTParamDecl* node) {
    auto param = ast_alloc<ASTParamDecl>();
    param->type = static_cast<ASTType*>(copy(node->type));
    if (node->default_value)
      param->default_value = static_cast<ASTExpr*>(copy(node->default_value.get()));
    param->name = node->name;
    param->source_range = node->source_range;
    return param;
  }

  ASTDeclaration* copy_declaration(ASTDeclaration* node) {
    auto decl = ast_alloc<ASTDeclaration>();
    decl->name = node->name;
    decl->type = static_cast<ASTType*>(copy(node->type));
    if (node->value)
      decl->value = static_cast<ASTExpr*>(copy(node->value.get()));
    decl->source_range = node->source_range;
    return decl;
  }

  ASTExprStatement* copy_expr_statement(ASTExprStatement* node) {
    auto expr_stmt = ast_alloc<ASTExprStatement>();
    expr_stmt->expression = static_cast<ASTExpr*>(copy(node->expression));
    expr_stmt->source_range = node->source_range;
    return expr_stmt;
  }

  ASTBinExpr* copy_bin_expr(ASTBinExpr* node) {
    auto bin_expr = ast_alloc<ASTBinExpr>();
    bin_expr->left = static_cast<ASTExpr*>(copy(node->left));
    bin_expr->right = static_cast<ASTExpr*>(copy(node->right));
    bin_expr->op = node->op;
    bin_expr->resolved_type = node->resolved_type;
    bin_expr->source_range = node->source_range;
    return bin_expr;
  }

  ASTUnaryExpr* copy_unary_expr(ASTUnaryExpr* node) {
    auto unary_expr = ast_alloc<ASTUnaryExpr>();
    unary_expr->operand = static_cast<ASTExpr*>(copy(node->operand));
    unary_expr->op = node->op;
    unary_expr->source_range = node->source_range;
    return unary_expr;
  }

  ASTIdentifier* copy_identifier(ASTIdentifier* node) {
    auto identifier = ast_alloc<ASTIdentifier>();
    identifier->value = node->value;
    identifier->source_range = node->source_range;
    return identifier;
  }

  ASTLiteral* copy_literal(ASTLiteral* node) {
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = node->tag;
    literal->value = node->value;
    for (const auto& interp_value : node->interpolated_values) {
      literal->interpolated_values.push_back(static_cast<ASTExpr*>(copy(interp_value)));
    }
    literal->source_range = node->source_range;
    return literal;
  }

  ASTType* copy_type(ASTType* node) {
    auto type = ast_alloc<ASTType>();
    type->flags = node->flags;
    type->base = node->base;
    type->extension_info = node->extension_info;
    type->resolved_type = node->resolved_type;
    if (node->pointing_to)
      type->pointing_to = static_cast<ASTExpr*>(copy(node->pointing_to.get()));
    for (const auto& tuple_type : node->tuple_types) {
      type->tuple_types.push_back(static_cast<ASTType*>(copy(tuple_type)));
    }
    type->source_range = node->source_range;
    return type;
  }

  ASTCall* copy_call(ASTCall* node) {
    auto call = ast_alloc<ASTCall>();
    call->function = node->function;
    call->arguments = static_cast<ASTArguments*>(copy(node->arguments));
    call->type = node->type;
    call->source_range = node->source_range;
    return call;
  }

  ASTArguments* copy_arguments(ASTArguments* node) {
    auto arguments = ast_alloc<ASTArguments>();
    for (const auto& arg : node->arguments) {
      arguments->arguments.push_back(static_cast<ASTExpr*>(copy(arg)));
    }
    arguments->source_range = node->source_range;
    return arguments;
  }

  ASTReturn* copy_return(ASTReturn* node) {
    auto ret = ast_alloc<ASTReturn>();
    if (node->expression)
      ret->expression = static_cast<ASTExpr*>(copy(node->expression.get()));
    ret->source_range = node->source_range;
    return ret;
  }

  ASTContinue* copy_continue(ASTContinue* node) {
    auto cont = ast_alloc<ASTContinue>();
    cont->source_range = node->source_range;
    return cont;
  }

  ASTBreak* copy_break(ASTBreak* node) {
    auto brk = ast_alloc<ASTBreak>();
    brk->source_range = node->source_range;
    return brk;
  }

  ASTFor* copy_for(ASTFor* node) {
    auto for_stmt = ast_alloc<ASTFor>();
    for_stmt->tag = node->tag;
    switch (node->tag) {
      case ASTFor::CollectionBased:
        for_stmt->value.collection_based.value_semantic = node->value.collection_based.value_semantic;
        for_stmt->value.collection_based.target = static_cast<ASTExpr*>(copy(node->value.collection_based.target));
        for_stmt->value.collection_based.collection = static_cast<ASTExpr*>(copy(node->value.collection_based.collection));
        break;
      case ASTFor::RangeBased:
        for_stmt->value.range_based.iden = static_cast<ASTExpr*>(copy(node->value.range_based.iden));
        for_stmt->value.range_based.range = static_cast<ASTRange*>(copy(node->value.range_based.range));
        break;
      case ASTFor::CStyle:
        for_stmt->value.c_style.decl = static_cast<ASTDeclaration*>(copy(node->value.c_style.decl));
        for_stmt->value.c_style.condition = static_cast<ASTExpr*>(copy(node->value.c_style.condition));
        for_stmt->value.c_style.increment = static_cast<ASTExpr*>(copy(node->value.c_style.increment));
        break;
    }
    for_stmt->block = static_cast<ASTBlock*>(copy(node->block));
    for_stmt->source_range = node->source_range;
    return for_stmt;
  }

  ASTIf* copy_if(ASTIf* node) {
    auto if_stmt = ast_alloc<ASTIf>();
    if_stmt->condition = static_cast<ASTExpr*>(copy(node->condition));
    if_stmt->block = static_cast<ASTBlock*>(copy(node->block));
    if (node->_else)
      if_stmt->_else = static_cast<ASTElse*>(copy(node->_else.get()));
    if_stmt->source_range = node->source_range;
    return if_stmt;
  }

  ASTElse* copy_else(ASTElse* node) {
    auto else_stmt = ast_alloc<ASTElse>();
    if (node->_if)
      else_stmt->_if = static_cast<ASTIf*>(copy(node->_if.get()));
    if (node->block)
      else_stmt->block = static_cast<ASTBlock*>(copy(node->block.get()));
    else_stmt->source_range = node->source_range;
    return else_stmt;
  }

  ASTWhile* copy_while(ASTWhile* node) {
    auto while_stmt = ast_alloc<ASTWhile>();
    if (node->condition)
      while_stmt->condition = static_cast<ASTExpr*>(copy(node->condition.get()));
    while_stmt->block = static_cast<ASTBlock*>(copy(node->block));
    while_stmt->source_range = node->source_range;
    return while_stmt;
  }

  ASTStructDeclaration* copy_struct_declaration(ASTStructDeclaration* node) {
    auto struct_decl = ast_alloc<ASTStructDeclaration>();
    struct_decl->type = static_cast<ASTType*>(copy(node->type));
    struct_decl->scope = copy_scope(node->scope);
    struct_decl->is_fwd_decl = node->is_fwd_decl;
    struct_decl->is_extern = node->is_extern;
    for (const auto& field : node->fields) {
      struct_decl->fields.push_back(static_cast<ASTDeclaration*>(copy(field)));
    }
    for (const auto& method : node->methods) {
      struct_decl->methods.push_back(static_cast<ASTFunctionDeclaration*>(copy(method)));
    }
    struct_decl->source_range = node->source_range;
    return struct_decl;
  }

  ASTDotExpr* copy_dot_expr(ASTDotExpr* node) {
    auto dot_expr = ast_alloc<ASTDotExpr>();
    dot_expr->left = static_cast<ASTExpr*>(copy(node->left));
    dot_expr->right = static_cast<ASTExpr*>(copy(node->right));
    dot_expr->type = static_cast<ASTType*>(copy(node->type));
    dot_expr->source_range = node->source_range;
    return dot_expr;
  }

  ASTSubscript* copy_subscript(ASTSubscript* node) {
    auto subscript = ast_alloc<ASTSubscript>();
    subscript->left = static_cast<ASTExpr*>(copy(node->left));
    subscript->subscript = static_cast<ASTExpr*>(copy(node->subscript));
    subscript->source_range = node->source_range;
    return subscript;
  }

  ASTMake* copy_make(ASTMake* node) {
    auto make = ast_alloc<ASTMake>();
    make->kind = node->kind;
    make->type_arg = static_cast<ASTType*>(copy(node->type_arg));
    make->arguments = static_cast<ASTArguments*>(copy(node->arguments));
    make->source_range = node->source_range;
    return make;
  }

  ASTInitializerList* copy_initializer_list(ASTInitializerList* node) {
    auto init_list = ast_alloc<ASTInitializerList>();
    init_list->types_are_homogenous = node->types_are_homogenous;
    init_list->types = node->types;
    for (const auto& expr : node->expressions) {
      init_list->expressions.push_back(static_cast<ASTExpr*>(copy(expr)));
    }
    init_list->source_range = node->source_range;
    return init_list;
  }

  ASTEnumDeclaration* copy_enum_declaration(ASTEnumDeclaration* node) {
    auto enum_decl = ast_alloc<ASTEnumDeclaration>();
    enum_decl->is_flags = node->is_flags;
    enum_decl->element_type = node->element_type;
    enum_decl->type = static_cast<ASTType*>(copy(node->type));
    for (const auto& key_value : node->key_values) {
      enum_decl->key_values.push_back({key_value.first, key_value.second ? static_cast<ASTExpr*>(copy(key_value.second.get())) : nullptr});
    }
    enum_decl->source_range = node->source_range;
    return enum_decl;
  }

  ASTUnionDeclaration* copy_union_declaration(ASTUnionDeclaration* node) {
    auto union_decl = ast_alloc<ASTUnionDeclaration>();
    union_decl->scope = copy_scope(node->scope);
    union_decl->name = node->name;
    union_decl->type = static_cast<ASTType*>(copy(node->type));
    union_decl->kind = node->kind;
    union_decl->is_fwd_decl = node->is_fwd_decl;
    for (const auto& field : node->fields) {
      union_decl->fields.push_back(static_cast<ASTDeclaration*>(copy(field)));
    }
    for (const auto& method : node->methods) {
      union_decl->methods.push_back(static_cast<ASTFunctionDeclaration*>(copy(method)));
    }
    for (const auto& struct_decl : node->structs) {
      union_decl->structs.push_back(static_cast<ASTStructDeclaration*>(copy(struct_decl)));
    }
    union_decl->source_range = node->source_range;
    return union_decl;
  }

  ASTAllocate* copy_allocate(ASTAllocate* node) {
    auto alloc = ast_alloc<ASTAllocate>();
    if (node->type)
      alloc->type = static_cast<ASTType*>(copy(node->type.get()));
    if (node->arguments)
      alloc->arguments = static_cast<ASTArguments*>(copy(node->arguments.get()));
    alloc->kind = node->kind;
    alloc->source_range = node->source_range;
    return alloc;
  }

  ASTRange* copy_range(ASTRange* node) {
    auto range = ast_alloc<ASTRange>();
    range->left = static_cast<ASTExpr*>(copy(node->left));
    range->right = static_cast<ASTExpr*>(copy(node->right));
    range->source_range = node->source_range;
    return range;
  }

  ASTSwitch* copy_switch(ASTSwitch* node) {
    auto switch_stmt = ast_alloc<ASTSwitch>();
    switch_stmt->is_statement = node->is_statement;
    switch_stmt->return_type = node->return_type;
    switch_stmt->target = static_cast<ASTExpr*>(copy(node->target));
    for (const auto& case_stmt : node->cases) {
      switch_stmt->cases.push_back({static_cast<ASTExpr*>(copy(case_stmt.expression)), static_cast<ASTBlock*>(copy(case_stmt.block))});
    }
    switch_stmt->source_range = node->source_range;
    return switch_stmt;
  }

  ASTTuple* copy_tuple(ASTTuple* node) {
    auto tuple = ast_alloc<ASTTuple>();
    tuple->type = static_cast<ASTType*>(copy(node->type));
    for (const auto& value : node->values) {
      tuple->values.push_back(static_cast<ASTExpr*>(copy(value)));
    }
    tuple->source_range = node->source_range;
    return tuple;
  }

  ASTTupleDeconstruction* copy_tuple_deconstruction(ASTTupleDeconstruction* node) {
    auto tuple_deconstruction = ast_alloc<ASTTupleDeconstruction>();
    for (const auto& iden : node->idens) {
      tuple_deconstruction->idens.push_back(static_cast<ASTIdentifier*>(copy(iden)));
    }
    tuple_deconstruction->right = static_cast<ASTExpr*>(copy(node->right));
    tuple_deconstruction->source_range = node->source_range;
    return tuple_deconstruction;
  }
};