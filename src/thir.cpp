#include "thir.hpp"
#include <map>
#include "ast.hpp"
#include "core.hpp"
#include "scope.hpp"
#include "type.hpp"

#define ENTER_SCOPE(new_scope)  \
  auto _old_scope_ = ctx.scope; \
  ctx.scope = new_scope;        \
  Defer _defer([&] { ctx.scope = _old_scope_; });

/*
  I put some of these super trivial nodes up top here so they stay out of the way
*/

std::map<ASTNode *, THIR *> symbol_map;

THIR *THIRVisitor::visit_size_of(ASTSize_Of *ast) {
  THIR_ALLOC(THIRSizeOf, thir, ast);
  thir->target = ast->target_type->resolved_type;
  thir->type = u64_type();
  return thir;
}

THIR *THIRVisitor::visit_continue(ASTContinue *ast) {
  THIR_ALLOC(THIRContinue, thir, ast);
  return thir;
}

THIR *THIRVisitor::visit_break(ASTBreak *ast) {
  THIR_ALLOC(THIRBreak, thir, ast);
  return thir;
}

THIR *THIRVisitor::visit_return(ASTReturn *ast) {
  THIR_ALLOC(THIRReturn, thir, ast);
  if (ast->expression) {
    thir->expression = visit_node(ast->expression.get());
  }
  return thir;
}

THIR *THIRVisitor::visit_expr_statement(ASTExprStatement *ast) {
  auto thir = visit_node(ast->expression);
  thir->is_statement = true;
  return thir;
}

/*
  ! These three are going to be tough, with generics and all.
  @Cooper-Pilot Need Sum Backup |:O| <- that's a face.
*/
THIR *THIRVisitor::visit_call(ASTCall *ast) {
  THIR_ALLOC(THIRCall, thir, ast);
  auto callee = thir->callee = visit_node(ast->callee);

  // Only handle default arguments for function calls that have a definition, not function pointers.
  if (callee->get_node_type() == THIRNodeType::Function) {
    const auto function = (const THIRFunction *)callee;
    const auto &params = function->parameters;
    const auto &args = ast->arguments->arguments;

    size_t i = 0;
    for (; i < args.size(); ++i) {
      thir->arguments.push_back(visit_node(args[i]));
    }

    for (; i < params.size(); ++i) {
      if (params[i].default_value) {
        thir->arguments.push_back(params[i].default_value);
      } else {
        throw_error("Missing argument with no default value", ast->source_range);
      }
    }
  } else { // Other calls, via function pointers, variables, non-symbol calls &c &c.
    for (const auto &argument : ast->arguments->arguments) {
      thir->arguments.push_back(visit_node(argument));
    }
  }

  return thir;
}
THIR *THIRVisitor::visit_method_call(ASTMethodCall *ast) {
  THIR_ALLOC(THIRCall, thir, ast);
  auto base = ast->callee->base;
  auto member = ast->callee->member;
  auto type_scope = base->resolved_type->info->scope;

  // ! TODO: we need to have a centralized way to get monomorphized generic functions,
  // ! and we need a way to monomorphize generic types too.
  auto symbol = type_scope->local_lookup(member.identifier);
  if (symbol->is_variable()) {
    thir->callee = visit_dot_expr(ast->callee);
  }
  for (const auto &argument : ast->arguments->arguments) {
    thir->arguments.push_back(visit_node(argument));
  }
  return thir;
}

/*
  TODO: we need to basically rework the symbol table (or the AST) for this to work.
  right now, it's impossible to refer to a parameter.

  OLD NOTE:
    Many variables do not come from AST.
    for loop identifiers, tuple/struct destructured elements,
    enum variants, the self variable.
    This should be fine for now, but will be problematic.
*/
THIR *THIRVisitor::visit_path(ASTPath *ast) {
  auto sym = ctx.get_symbol(ast).get();
  if (sym->is_function()) {
    return symbol_map[sym->function.declaration];
  }
  return symbol_map[sym->variable.declaration.get()];
}
THIR *THIRVisitor::visit_dot_expr(ASTDotExpr *ast) {
  THIR_ALLOC(THIRMemberAccess, thir, ast);
  thir->base = visit_node(ast->base);
  // TODO:
  // I am pretty sure this is okay to do? Dot expressions cant have generics,
  // this was just for parsing ease and QOL
  thir->member = ast->member.identifier;
  return thir;
}

// This is gonna be tricky to do right, for nested patterns, which we 100% plan on supporting.
THIR *THIRVisitor::visit_pattern_match(ASTPatternMatch *ast) {
  throw_error("visit_pattern_match not implemented", ast->source_range);
}

THIR *THIRVisitor::visit_bin_expr(ASTBinExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC(THIRBinExpr, binexpr, ast);
    binexpr->left = visit_node(ast->left);
    binexpr->right = visit_node(ast->right);
    binexpr->op = ast->op;
    return binexpr;
  } else {
    THIR_ALLOC(THIRCall, overload_call, ast);
    auto scope = ast->left->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(ast->op, OPERATION_BINARY));
    overload_call->callee = visit_function_declaration(symbol->function.declaration);
    overload_call->arguments.push_back(visit_node(ast->left));
    overload_call->arguments.push_back(visit_node(ast->right));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_unary_expr(ASTUnaryExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC(THIRUnaryExpr, unary, ast);
    unary->operand = visit_node(ast->operand);
    unary->op = ast->op;
    return unary;
  } else {
    THIR_ALLOC(THIRCall, overload_call, ast);
    auto scope = ast->operand->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(ast->op, OPERATION_UNARY));
    overload_call->callee = visit_function_declaration(symbol->function.declaration);
    overload_call->arguments.push_back(visit_node(ast->operand));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_index(ASTIndex *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC(THIRIndex, index, ast);
    index->base = visit_node(ast->base);
    index->index = visit_node(ast->index);
    return index;
  } else {
    THIR_ALLOC(THIRCall, overload_call, ast);
    auto scope = ast->base->resolved_type->info->scope;
    auto symbol = scope->local_lookup(get_operator_overload_name(TType::LBrace, OPERATION_INDEX));
    overload_call->callee = visit_function_declaration(symbol->function.declaration);
    overload_call->arguments.push_back(visit_node(ast->base));
    overload_call->arguments.push_back(visit_node(ast->index));
    return overload_call;
  }
}

THIR *THIRVisitor::visit_literal(ASTLiteral *ast) {
  THIR_ALLOC(THIRLiteral, literal, ast);
  literal->value = ast->value;
  return literal;
}

// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_dyn_of(ASTDyn_Of *ast) { throw_error("visit_dyn_of not implemented", ast->source_range); }
// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_tuple(ASTTuple *ast) { throw_error("visit_tuple not implemented", ast->source_range); }
// Use THIRAggregateInitializer here.
THIR *THIRVisitor::visit_range(ASTRange *ast) { throw_error("visit_range not implemented", ast->source_range); }
// Use THIRAggregateInitializer/Collection/Empty here.
// Really, the AST could benefit from the seperation of those possibly.
THIR *THIRVisitor::visit_initializer_list(ASTInitializerList *ast) {
  throw_error("visit_initializer_list not implemented", ast->source_range);
}

THIR *THIRVisitor::visit_type_of(ASTType_Of *ast) { throw_error("visit_type_of not implemented", ast->source_range); }

THIR *THIRVisitor::visit_cast(ASTCast *ast) {
  THIR_ALLOC(THIRCast, thir, ast);
  thir->operand = visit_node(ast->expression);
  thir->type = ast->target_type->resolved_type;
  return thir;
}

THIR *THIRVisitor::visit_lambda(ASTLambda *ast) {
  // TODO: We need to insert the lambdas into the scope by their UID, so we can fetch the symbol,
  // and get the THIRFunction* node from the declaration.
  throw_error("visit_lambda not implemented", ast->source_range);
}

THIR *THIRVisitor::visit_block(ASTBlock *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRBlock, thir, ast);

  std::vector<ASTDefer *> deferred_statements;
  for (const auto &ast_statement : ast->statements) {
    if (ast_statement->get_node_type() == AST_NODE_DEFER) {
      deferred_statements.push_back((ASTDefer *)ast_statement);
    }
    if (auto thir_statement = visit_node(ast_statement)) {
      thir->statements.push_back(thir_statement);
    }
  }

  // !
  // TODO: this is gonna have to be a lot more complex than this. we have to look up at our parent blocks
  // and grab up all their deferred statements on early returns.
  // This is just the basic idea, append them at the end, and all the continues, breaks, and returns, when applicable.
  // !
  for (const auto &ast_deferred : deferred_statements) {
    auto thir_deferred = visit_node(ast_deferred->statement);
    thir->statements.push_back(thir_deferred);
  }

  return thir;
}

static inline void convert_function_flags(THIRFunction *reciever, FunctionInstanceFlags flags) {
  if (HAS_FLAG(flags, FUNCTION_IS_INLINE)) {
    reciever->is_inline = true;
  }
  if (HAS_FLAG(flags, FUNCTION_IS_VARARGS)) {
    reciever->is_varargs = true;
  }
  if (HAS_FLAG(flags, FUNCTION_IS_EXTERN)) {
    reciever->is_extern = true;
  }
  if (HAS_FLAG(flags, FUNCTION_IS_ENTRY)) {
    reciever->is_entry = true;
  }
  if (HAS_FLAG(flags, FUNCTION_IS_EXPORTED)) {
    reciever->is_exported = true;
  }
  if (HAS_FLAG(flags, FUNCTION_IS_TEST)) {
    reciever->is_test = true;
  }
}

// TODO: fix the overlap between the flags and attributes. we should just have the flags, the typer
// should bake all the attributes into the appropriate location via flags/bools.
static inline void convert_function_attributes(THIRFunction *reciever, const std::vector<Attribute> &attrs) {
  for (const auto &attr : attrs) {
    switch (attr.tag) {
      case ATTRIBUTE_NO_MANGLE:
        reciever->is_no_mangle = true;
        break;
      case ATTRIBUTE_NO_RETURN:
        reciever->is_no_return = true;
        break;
      default:
        break;
    }
  }
}

THIR *THIRVisitor::visit_function_declaration(ASTFunctionDeclaration *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRFunction, thir, ast);
  convert_function_flags(thir, (FunctionInstanceFlags)ast->flags);
  convert_function_attributes(thir, ast->attributes);

  for (const auto &param : ast->params->params) {
    THIR *default_value = nullptr;
    THIR_ALLOC(THIRVariable, thir_param, param);
    if (param->tag == ASTParamDecl::Normal) {
      thir_param->name = param->normal.name;
      thir_param->type = param->normal.type->resolved_type;
      if (param->normal.default_value.is_not_null()) {
        default_value = visit_node(param->normal.default_value.get());
      }
      if (param->normal.default_value) {
        thir_param->value = visit_node(param->normal.default_value.get());
      } else {
        // TODO: this may be problematic. Not certain yet we can assume this is never null. Maybe we make this nullable
        // in def
        thir_param->value = nullptr;
      }
    } else {
      thir_param->name = "self";
      thir_param->type = param->resolved_type;
    }

    symbol_map[param] = thir_param;
    thir->parameters.push_back(THIRParameter{
        .name = thir_param->name,
        .default_value = default_value,
    });
  }

  if (thir->name == "main") {
    thir->is_entry = true;
  }

  if (thir->is_exported || thir->is_extern || thir->is_no_mangle) {
    thir->name = ast->name;
  } else {
    thir->name = ast->scope->full_name();
  }

  printf(
      "function %s = {\n  is_extern=%s,\n  is_inline=%s\n  is_exported=%s\n  is_test=%s\n  is_varargs=%s\n  "
      "is_entry=%s\n  is_no_return=%s }\n",
      thir->name.get_str().c_str(), thir->is_extern ? "true" : "false", thir->is_inline ? "true" : "false",
      thir->is_exported ? "true" : "false", thir->is_test ? "true" : "false", thir->is_varargs ? "true" : "false",
      thir->is_entry ? "true" : "false", thir->is_no_return ? "true" : "false");

  symbol_map[ast] = thir;
  if (ast->block) {
    thir->block = (THIRBlock *)visit_block(ast->block.get());
  }
  return thir;
}

THIR *THIRVisitor::visit_variable(ASTVariable *ast) {
  THIR_ALLOC(THIRVariable, thir, ast);

  symbol_map[ast] = thir;
  if (!ast->is_local) {
    thir->name = ctx.scope->full_name() + "$" + ast->name.get_str();
  } else {
    thir->name = ast->name;
  }
  if (ast->value) {
    thir->value = visit_node(ast->value.get());
  } else {
    // TODO: we should have an option for initializing variable declarations with no value that lets it circumvent zero
    // init.
    // TODO: akin to jai's --- operator.

    // we use this for default construction, to be more explicit.
    THIR_ALLOC(THIREmptyInitializer, empty_init, ast);
    thir->value = empty_init;
  }
  thir->type = ast->type->resolved_type;
  return thir;
}

THIR *THIRVisitor::visit_struct_declaration(ASTStructDeclaration *ast) {
  THIR_ALLOC(THIRStruct, thir, ast);
  thir->name = ast->name;

  for (const auto &subtype : ast->subtypes) {
    thir->subtypes.push_back((THIRStruct *)visit_struct_declaration(subtype));
  }

  for (const auto &member : ast->members) {
    // use the type here to just get the source_location, and resolved_type
    THIR_ALLOC(THIRVariable, field, member.type);
    field->name = member.name;
    if (member.default_value) {
      field->value = visit_node(member.default_value.get());
    } else {
      THIR_ALLOC(THIREmptyInitializer, init, member.type);
      field->value = init;
    }
    thir->fields.push_back(field);
  }

  return thir;
}

THIR *THIRVisitor::visit_choice_declaration(ASTChoiceDeclaration *ast) { return nullptr; }

THIR *THIRVisitor::visit_enum_declaration(ASTEnumDeclaration *ast) { return nullptr; }

THIR *THIRVisitor::visit_switch(ASTSwitch *ast) { throw_error("visit_switch not implemented", ast->source_range); }

THIR *THIRVisitor::visit_program(ASTProgram *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRProgram, thir, ast);

  for (const auto &ast_statement : ast->statements) {
    if (auto thir_statement = visit_node(ast_statement)) {
      thir->statements.push_back(thir_statement);
    }
  }
  return thir;
}

// Optimization idea:
// For simple 'for i in 0..10' etc loops over range literals,
// we can optimize down into a classic C style for loop. Yank out some iterator overhead.
// We'd still have RangeIter for other uses, and for runtime loops that take a non constant range.
THIR *THIRVisitor::visit_for(ASTFor *ast) { throw_error("visit_for not implemented", ast->source_range); }

THIR *THIRVisitor::visit_if(ASTIf *ast) { throw_error("visit_if not implemented", ast->source_range); }
THIR *THIRVisitor::visit_else(ASTElse *ast) { throw_error("visit_else not implemented", ast->source_range); }

THIR *THIRVisitor::visit_while(ASTWhile *ast) { throw_error("visit_while not implemented", ast->source_range); }

// This would likely result in just declaring several variables, it's tough to return several nodes from one visit.
THIR *THIRVisitor::visit_tuple_deconstruction(ASTDestructure *ast) {
  throw_error("visit_tuple_deconstruction not implemented", ast->source_range);
}
// This shouldn't be carried past this point; they should be placed in the correct parts of each block,
// and 'instantiated' for lack of a better term, then shelled off.
THIR *THIRVisitor::visit_defer(ASTDefer *ast) { throw_error("visit_defer not implemented", ast->source_range); }
// Not really sure what significance this would have either.
THIR *THIRVisitor::visit_import(ASTImport *ast) { throw_error("visit_import not implemented", ast->source_range); }
// Not sure how we'd use this. I guess module $name { ... } has code in it.
THIR *THIRVisitor::visit_module(ASTModule *ast) { throw_error("visit_module not implemented", ast->source_range); }
// I don't think we need this, this should completely done after typing.
THIR *THIRVisitor::visit_impl(ASTImpl *ast) { throw_error("visit_impl not implemented", ast->source_range); }
THIR *THIRVisitor::visit_where_statement(ASTWhereStatement *node) {}
