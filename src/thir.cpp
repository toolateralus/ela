#include "thir.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "type.hpp"

#define ENTER_SCOPE(new_scope)  \
  auto _old_scope_ = ctx.scope; \
  ctx.scope = new_scope;        \
  Defer _defer([&] { ctx.scope = _old_scope_; });

/*
  I put some of these super trivial nodes up top here so they stay out of the way
*/
THIR *THIRVisitor::visit_size_of(ASTSize_Of *ast) {
  THIR_ALLOC_EXPR(THIRSizeOf, size_of, ast);
  size_of->target = ast->target_type->resolved_type;
  size_of->type = u64_type();
  return size_of;
}

THIR *THIRVisitor::visit_continue(ASTContinue *ast) {
  THIR_ALLOC_STMT(THIRContinue, cont, ast);
  return cont;
}

THIR *THIRVisitor::visit_break(ASTBreak *ast) {
  THIR_ALLOC_STMT(THIRBreak, brk, ast);
  return brk;
}

THIR *THIRVisitor::visit_return(ASTReturn *ast) {
  THIR_ALLOC_STMT(THIRReturn, thir, ast);
  if (ast->expression) {
    thir->expression = visit_node(ast->expression.get());
  }
  return thir;
}

THIR *THIRVisitor::visit_expr_statement(ASTExprStatement *ast) { return visit_node(ast->expression); }

/*
  ! These three are going to be tough, with generics and all.
  @Cooper-Pilot Need Sum Backup :O
*/
THIR *THIRVisitor::visit_call(ASTCall *ast) { return nullptr; }
THIR *THIRVisitor::visit_method_call(ASTMethodCall *ast) {
  THIR_ALLOC_EXPR(THIRCall, thir, ast);
  auto base = ast->callee->base;
  auto member = ast->callee->member;
  auto type_scope = base->resolved_type->info->scope;

  // ! TODO: we need to have a centralized way to get monomorphized geneeric functions,
  // ! and we need a way to monomorphize generic types too.
  auto symbol = type_scope->local_lookup(member.identifier);
  if (symbol->is_variable()) {
    thir->callee = visit_dot_expr(ast->callee);
  }
  for (const auto &argument : ast->arguments->arguments) {
    thir->arguments.push_back(visit_node(argument));
  }
  thir->type = ast->resolved_type;
  thir->source_range = ast->source_range;
  return thir;
}
THIR *THIRVisitor::visit_path(ASTPath *ast) { return nullptr; }
THIR *THIRVisitor::visit_dot_expr(ASTDotExpr *ast) { return nullptr; }

// This is gonna be tricky to do right, for nested patterns, which we 100% plan on supporting.
THIR *THIRVisitor::visit_pattern_match(ASTPatternMatch *ast) { return nullptr; }

THIR *THIRVisitor::visit_bin_expr(ASTBinExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC_EXPR(THIRBinExpr, binexpr, ast);
    binexpr->left = visit_node(ast->left);
    binexpr->right = visit_node(ast->right);
    binexpr->op = ast->op;
    return binexpr;
  } else {
    THIR_ALLOC_EXPR(THIRCall, overload_call, ast);
    auto scope = ast->left->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(ast->op, OPERATION_BINARY));
    THIR_ALLOC_EXPR(THIRPath, path, ast);
    path->type = symbol->resolved_type;
    path->function = (THIRFunction *)visit_function_declaration(symbol->function.declaration);
    path->tag = THIRPath::Function;
    overload_call->callee = path;
    overload_call->arguments.push_back(visit_node(ast->left));
    overload_call->arguments.push_back(visit_node(ast->right));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_unary_expr(ASTUnaryExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC_EXPR(THIRUnaryExpr, unary, ast);
    unary->operand = visit_node(ast->operand);
    unary->op = ast->op;
    return unary;
  } else {
    THIR_ALLOC_EXPR(THIRCall, overload_call, ast);
    auto scope = ast->operand->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(ast->op, OPERATION_UNARY));
    THIR_ALLOC_EXPR(THIRPath, path, ast);
    path->type = symbol->resolved_type;
    path->function = (THIRFunction *)visit_function_declaration(symbol->function.declaration);
    path->tag = THIRPath::Function;
    overload_call->callee = path;
    overload_call->arguments.push_back(visit_node(ast->operand));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_index(ASTIndex *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC_EXPR(THIRIndex, index, ast);
    index->base = visit_node(ast->base);
    index->index = visit_node(ast->index);
    return index;
  } else {
    THIR_ALLOC_EXPR(THIRCall, overload_call, ast);
    auto scope = ast->base->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(TType::LBrace, OPERATION_INDEX));
    THIR_ALLOC_EXPR(THIRPath, path, ast);
    path->type = symbol->resolved_type;
    path->function = (THIRFunction *)visit_function_declaration(symbol->function.declaration);
    path->tag = THIRPath::Function;
    overload_call->callee = path;
    overload_call->arguments.push_back(visit_node(ast->base));
    overload_call->arguments.push_back(visit_node(ast->index));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_literal(ASTLiteral *ast) {
  THIR_ALLOC_EXPR(THIRLiteral, literal, ast);
  literal->value = ast->value;
  return literal;
}

// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_dyn_of(ASTDyn_Of *ast) { return nullptr; }
// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_tuple(ASTTuple *ast) { return nullptr; }
// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_range(ASTRange *ast) { return nullptr; }
// Use THIRAggregateInitializer/Collection/Empty here.
// Really, the AST could benefit from the seperation of those possibly.
THIR *THIRVisitor::visit_initializer_list(ASTInitializerList *ast) { return nullptr; }

THIR *THIRVisitor::visit_type_of(ASTType_Of *ast) {
  // TODO: Gotta make a decision about the future of the RTTI:
  /*
    We can continue to use a big runtime initialized array of data, which slows program startup, and causes a ton of
    memory leaks, but works as is.

    Or, we can switch to using a ton of fields that are forward declared with extern, and embedded in the binary as
    const static / equivalent in LLVM.

    this would probably be most ideal, compile time setting up RTTI.

    In one case, we'll take the address of a specific field here (the second option)
    and in the other case (how it works now) we'll subscript the __type_info[..] array
    to get our heap allocated pointer.

    We should rework it, it would be great to use the second idea. cheaper accesses, cheaper initialization, a little
    trade off in binary size.
  */

  THIR_ALLOC_EXPR(THIRIndex, index, ast);
  THIR_ALLOC_EXPR(THIRLiteral, type_index, ast);
  type_index->value = std::to_string(ast->target->resolved_type->uid);
  type_index->type = u64_type();

  index->index = type_index;
  THIR_ALLOC_EXPR(THIRPath, type_info_path, ast);

  {  // Ugly but we have to do some type resolution here, for the List!<*const Type>. then, set the type_infp_path->type
    // to that.
    const static Type *type_ptr = ctx.scope->find_type_id("Type", {{{TYPE_EXT_POINTER_CONST}}});
    static Type *list = ctx.scope->find_type_id("List", {});
    const static ASTDeclaration *declaring_node = (ASTStructDeclaration *)list->declaring_node.get();
    const static ASTDeclaration *instance = find_generic_instance(declaring_node->generic_instantiations, {list});
    static Type *type_ptr_list = instance->resolved_type;
    type_info_path->type = type_ptr_list;
  }

  type_info_path->tag = THIRPath::Variable;
  type_info_path->variable->name = "__type_info";
  type_info_path->variable->source_range = ast->source_range;
  index->base = type_info_path;
  return index;
}

THIR *THIRVisitor::visit_cast(ASTCast *ast) {
  THIR_ALLOC_EXPR(THIRCast, cast, ast);
  cast->operand = visit_node(ast->expression);
  cast->type = ast->target_type->resolved_type;
  return cast;
}

THIR *THIRVisitor::visit_lambda(ASTLambda *ast) {
  // TODO: We need to insert the lambdas into the scope by their UID, so we can fetch the symbol,
  // and get the THIRFunction* node from the declaration.
  return nullptr;
}

THIR *THIRVisitor::visit_block(ASTBlock *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC_STMT(THIRBlock, block, ast);

  std::vector<ASTDefer *> deferred_statements;
  for (const auto &statement : ast->statements) {
    if (statement->get_node_type() == AST_NODE_DEFER) {
      deferred_statements.push_back((ASTDefer *)statement);
    }
    auto thir = visit_node(statement);
    block->statements.push_back(thir);
  }

  // !
  // TODO: this is gonna have to be a lot more complex than this. we have to look up at our parent blocks
  // and grab up all their deferred statements on early returns.
  // This is just the basic idea, append them at the end, and all the continues, breaks, and returns, when applicable.
  // !
  for (const auto &deferred : deferred_statements) {
    auto thir = visit_node(deferred->statement);
    block->statements.push_back(thir);
  }

  return block;
}

/*
  This needs a caching mechanism so we can always just lookup the symbol, call visit function declaration,
  and get the THIRFunction * for any given function at any time.

  Then, we don't need a dual symbol table.

  As well as ASTVariable, right below.
*/
THIR *THIRVisitor::visit_function_declaration(ASTFunctionDeclaration *ast) { return nullptr; }
THIR *THIRVisitor::visit_variable(ASTVariable *ast) { return nullptr; }

THIR *THIRVisitor::visit_struct_declaration(ASTStructDeclaration *ast) { return nullptr; }
THIR *THIRVisitor::visit_choice_declaration(ASTChoiceDeclaration *) { return nullptr; }
THIR *THIRVisitor::visit_enum_declaration(ASTEnumDeclaration *ast) { return nullptr; }

THIR *THIRVisitor::visit_switch(ASTSwitch *ast) { return nullptr; }
THIR *THIRVisitor::visit_program(ASTProgram *ast) { return nullptr; }

// Optimization idea:
// For simple 'for i in 0..10' etc loops over range literals,
// we can optimize down into a classic C style for loop. Yank out some iterator overhead.
// We'd still have RangeIter for other uses, and for runtime loops that take a non constant range.
THIR *THIRVisitor::visit_for(ASTFor *ast) { return nullptr; }

// this should have the capability to return values. It's easier than ever with my new knowledge of C,
// and we can easily support it.
THIR *THIRVisitor::visit_if(ASTIf *ast) { return nullptr; }
THIR *THIRVisitor::visit_else(ASTElse *ast) { return nullptr; }
THIR *THIRVisitor::visit_while(ASTWhile *ast) { return nullptr; }

// This would likely result in just declaring several variables, it's tough to return several nodes from one visit.
THIR *THIRVisitor::visit_tuple_deconstruction(ASTDestructure *ast) { return nullptr; }
// This shouldn't be carried past this point; they should be placed in the correct parts of each block,
// and 'instantiated' for lack of a better term, then shelled off.
THIR *THIRVisitor::visit_defer(ASTDefer *ast) { return nullptr; }
// Not really sure what significance this would have either.
THIR *THIRVisitor::visit_import(ASTImport *ast) { return nullptr; }
// Not sure how we'd use this. I guess module $name { ... } has code in it.
THIR *THIRVisitor::visit_module(ASTModule *ast) { return nullptr; }
// I don't think we need this, this should completely done after typing.
THIR *THIRVisitor::visit_impl(ASTImpl *ast) { return nullptr; }
