#include "thir.hpp"
#include <string>
#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"

// this is used directly. clangd stop b*tchin
#include "visitor.hpp"

#define ENTER_SCOPE($new_scope)       \
  const auto $old_scope_ = ctx.scope; \
  ctx.scope = $new_scope;             \
  const Defer $scope_defer([&] { ctx.scope = $old_scope_; });

// This is for nodes that don't return, instead just push right into their parent. there's a few funamental ones, so
// this is very important.
#define ENTER_STMT_VEC($stmt_vector)                                                       \
  static_assert(std::is_same_v<std::decay_t<decltype($stmt_vector)>, std::vector<THIR *>>, \
                "ENTER_STMT_VEC expects a std::vector<THIR *>");                           \
  const auto $old_stmt_vector = current_statement_list;                                    \
  current_statement_list = (&$stmt_vector);                                                \
  const Defer $stmt_container_defer([&] { current_statement_list = $old_stmt_vector; });

static inline bool should_emit_choice_type_marker_variant_instantiation(ASTPath *ast) {
  if (!ast->resolved_type->is_kind(TYPE_CHOICE) || ast->resolved_type->extensions.has_extensions()) {
    return false;
  }

  ChoiceTypeInfo *info = ast->resolved_type->info->as<ChoiceTypeInfo>();
  const auto last_segment = ast->segments.back();

  for (const auto &member : info->members) {
    if (last_segment.identifier == member.name) {
      return true;
    }
  }

  return false;
}

static inline THIRAggregateInitializer *get_choice_type_instantiation_boilerplate(ASTPath *ast) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast);
  const auto type = ast->resolved_type;
  const auto path = ast;
  const auto variant_name = path->segments.back().identifier;
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto discriminant = info->get_variant_discriminant(variant_name);
  THIR_ALLOC_NO_SRC_RANGE(THIRLiteral, discriminant_literal);
  // TODO: optimize the discriminant type size based on the number of variants in a choice type.
  discriminant_literal->value = std::to_string(discriminant);
  discriminant_literal->type = u32_type();
  thir->key_values.push_back({DISCRIMINANT_KEY, discriminant_literal});
  return thir;
}

/*
  I put some of these super trivial nodes up top here so they stay out of the way
*/
THIR *THIRGen::visit_size_of(ASTSize_Of *ast) {
  THIR_ALLOC(THIRSizeOf, thir, ast);
  thir->target = ast->target_type->resolved_type;
  thir->type = u64_type();
  return thir;
}

THIR *THIRGen::visit_continue(ASTContinue *ast) {
  THIR_ALLOC(THIRContinue, thir, ast);
  return thir;
}

THIR *THIRGen::visit_break(ASTBreak *ast) {
  THIR_ALLOC(THIRBreak, thir, ast);
  return thir;
}

THIR *THIRGen::visit_return(ASTReturn *ast) {
  THIR_ALLOC(THIRReturn, thir, ast);
  if (ast->expression) {
    thir->expression = visit_node(ast->expression.get());
  }
  return thir;
}

THIR *THIRGen::visit_expr_statement(ASTExprStatement *ast) {
  auto thir = visit_node(ast->expression);
  thir->is_statement = true;
  return thir;
}

/*
  ! These three are going to be tough, with generics and all.
  @Cooper-Pilot Need Sum Backup |:O| <- that's a face.
*/
void THIRGen::extract_arguments_desugar_defaults(const THIR *callee, const ASTArguments *in_args,
                                                 std::vector<THIR *> &out_args) {
  // Only handle default arguments for function calls that have a definition, not function pointers.
  if (callee->get_node_type() == THIRNodeType::Function) {
    const auto function = (const THIRFunction *)callee;
    const auto &params = function->parameters;
    const auto &ast_args = in_args->arguments;

    size_t i = 0;
    for (; i < ast_args.size(); ++i) {
      out_args.push_back(visit_node(ast_args[i]));
    }

    for (; i < params.size(); ++i) {
      if (params[i].default_value) {
        out_args.push_back(params[i].default_value);
      }
    }
  } else {
    // Other calls, via function pointers, variables, non-symbol calls &c &c.
    for (const auto &argument : in_args->arguments) {
      out_args.push_back(visit_node(argument));
    }
  }
}

THIR *THIRGen::visit_call(ASTCall *ast) {
  THIR_ALLOC(THIRCall, thir, ast);

  // Tuple style constructor for choice type.
  if (ast->callee->resolved_type->is_kind(TYPE_CHOICE) && ast->callee->get_node_type() == AST_NODE_PATH) {
    const auto path = (ASTPath *)ast->callee;
    const auto variant_name = path->segments.back().identifier;
    const auto type = ast->callee->resolved_type;
    const auto info = type->info->as<ChoiceTypeInfo>();
    const auto thir = get_choice_type_instantiation_boilerplate(path);
    ASTTuple tuple;
    tuple.resolved_type = info->get_variant_type(variant_name);
    tuple.values = ast->arguments->arguments;
    thir->key_values.push_back({variant_name, visit_node(&tuple)});
    return thir;
  }

  auto callee = thir->callee = visit_node(ast->callee);
  extract_arguments_desugar_defaults(callee, ast->arguments, thir->arguments);
  return thir;
}

THIR *THIRGen::visit_method_call(ASTMethodCall *ast) {
  THIR_ALLOC(THIRCall, thir, ast);
  const auto base = ast->callee->base;
  const auto member = ast->callee->member;
  const auto type_scope = base->resolved_type->info->scope;
  const auto symbol = type_scope->local_lookup(member.identifier);
  if (symbol->is_variable()) {
    thir->callee = visit_dot_expr(ast->callee);
  } else {
    // Push the self argument
    const auto thir_base = visit_node(base);
    thir->arguments.push_back(thir_base);
    thir->callee = visit_function_declaration(symbol->function.declaration);
  }
  extract_arguments_desugar_defaults(thir->callee, ast->arguments, thir->arguments);
  return thir;
}

THIR *THIRGen::visit_path(ASTPath *ast) {
  // This is just nonsense in many ways; What about choice type variants, what about enum variants?
  // This just won't work.
  // Not even the string rework will work perfectly.

  if (should_emit_choice_type_marker_variant_instantiation(ast)) {
    return get_choice_type_instantiation_boilerplate(ast);
  }

  auto sym = ctx.get_symbol(ast).get();
  if (sym->is_function()) {
    return symbol_map[sym->function.declaration];
  }
  return symbol_map[sym->variable.declaration.get()];
}

THIR *THIRGen::visit_dot_expr(ASTDotExpr *ast) {
  THIR_ALLOC(THIRMemberAccess, thir, ast);
  thir->base = visit_node(ast->base);
  // TODO:
  // I am pretty sure this is okay to do? Dot expressions cant have generics,
  // this was just for parsing ease and QOL
  thir->member = ast->member.identifier;
  return thir;
}

// This is gonna be tricky to do right, for nested patterns, which we 100% plan on supporting.
THIR *THIRGen::visit_pattern_match(ASTPatternMatch *ast) {
  throw_error("visit_pattern_match not implemented", ast->source_range);
}

THIR *THIRGen::visit_bin_expr(ASTBinExpr *ast) {
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

THIR *THIRGen::visit_unary_expr(ASTUnaryExpr *ast) {
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

THIR *THIRGen::visit_index(ASTIndex *ast) {
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

THIR *THIRGen::visit_literal(ASTLiteral *ast) {
  THIR_ALLOC(THIRLiteral, literal, ast);
  literal->value = ast->value;
  return literal;
}

// Use THIRAggregateInitializer here.
THIR *THIRGen::visit_dyn_of(ASTDyn_Of *ast) { throw_error("visit_dyn_of not implemented", ast->source_range); }

// Use THIRAggregateInitializer here.
THIR *THIRGen::visit_tuple(ASTTuple *ast) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast)
  size_t index = 0;
  for (const auto value : ast->values) {
    thir->key_values.push_back({"$" + std::to_string(index++), visit_node(value)});
  }
  return thir;
}

// Use THIRAggregateInitializer here.
THIR *THIRGen::visit_range(ASTRange *ast) { throw_error("visit_range not implemented", ast->source_range); }

// Use THIRAggregateInitializer/Collection/Empty here.
// Really, the AST could benefit from the seperation of those possibly.
THIR *THIRGen::visit_initializer_list(ASTInitializerList *ast) {
  switch (ast->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      THIR_ALLOC(THIREmptyInitializer, thir, ast);
      return thir;
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      const auto type = ast->resolved_type;
      if (type->kind == TYPE_STRUCT) {
        THIR_ALLOC(THIRAggregateInitializer, thir, ast);
        const auto info = type->info->as<StructTypeInfo>();
        for (const auto &member : info->members) {
          bool set = false;
          for (size_t i = 0; i < ast->key_values.size(); ++i) {
            const auto &[key, value] = ast->key_values[i];
            if (key == member.name) {
              thir->key_values.push_back({key, visit_node(value)});
              set = true;
              break;
            }
          }
          if (!set) {
            THIR_ALLOC(THIREmptyInitializer, empty_init, ast);
            empty_init->type = member.type;
            thir->key_values.push_back({member.name, empty_init});
          }
        }
        return thir;
      } else if (type->kind == TYPE_CHOICE) {  // Choice variant instantiation.
        const auto path = ast->target_type.get()->normal.path;
        const auto info = ast->resolved_type->info->as<ChoiceTypeInfo>();
        const auto thir = get_choice_type_instantiation_boilerplate(path);
        const auto variant_name = path->segments.back().identifier;
        const auto variant_type = info->get_variant_type(variant_name);

        // Mock up a temporary initializer list to just take advantage of existing THIR generation
        ASTInitializerList subinit{};
        subinit.resolved_type = variant_type;
        subinit.tag = ASTInitializerList::INIT_LIST_NAMED;

        new (&subinit.key_values) decltype(subinit.key_values)();
        for (const auto kv : ast->key_values) {
          subinit.key_values.push_back(kv);
        }

        thir->key_values.push_back({variant_name, visit_node(&subinit)});
        return thir;
      }
    }
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      THIR_ALLOC(THIRCollectionInitializer, thir, ast)
      for (const auto &value : ast->values) {
        thir->values.push_back(visit_node(value));
      }
      return thir;
    }
  }
}

THIR *THIRGen::visit_type_of(ASTType_Of *ast) { throw_error("visit_type_of not implemented", ast->source_range); }

THIR *THIRGen::visit_cast(ASTCast *ast) {
  THIR_ALLOC(THIRCast, thir, ast);
  thir->operand = visit_node(ast->expression);
  thir->type = ast->target_type->resolved_type;
  return thir;
}

THIR *THIRGen::visit_lambda(ASTLambda *ast) {
  // TODO: We need to insert the lambdas into the scope by their UID, so we can fetch the symbol,
  // and get the THIRFunction* node from the declaration.
  throw_error("visit_lambda not implemented", ast->source_range);
}

THIR *THIRGen::visit_block(ASTBlock *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRBlock, thir, ast);
  ENTER_STMT_VEC(thir->statements);

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

THIR *THIRGen::visit_function_declaration(ASTFunctionDeclaration *ast) {
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

  symbol_map[ast] = thir;
  if (ast->block) {
    thir->block = (THIRBlock *)visit_block(ast->block.get());
  }
  return thir;
}

THIR *THIRGen::visit_variable(ASTVariable *ast) {
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

void THIRGen::extract_thir_values_for_type_members(Type *type) {
  for (auto &member : type->info->members) {
    if (member.default_value) {
      member.thir_value = visit_node(member.default_value.get());
    }
  }
}

THIR *THIRGen::visit_struct_declaration(ASTStructDeclaration *ast) {
  THIR_ALLOC(THIRType, thir, ast);
  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_choice_declaration(ASTChoiceDeclaration *ast) {
  THIR_ALLOC(THIRType, thir, ast);
  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_enum_declaration(ASTEnumDeclaration *ast) {
  THIR_ALLOC(THIRType, thir, ast);
  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_switch(ASTSwitch *ast) { throw_error("visit_switch not implemented", ast->source_range); }

THIR *THIRGen::visit_program(ASTProgram *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRProgram, thir, ast);
  ENTER_STMT_VEC(thir->statements);

  for (const auto &ast_statement : ast->statements) {
    if (auto thir_statement = visit_node(ast_statement)) {
      thir->statements.push_back(thir_statement);
    }
  }

  return thir;
}

/*
  * Here's some example compiled code of how we handle iterators, in this case, a constant Range.

  RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = argc};

  RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);

  for (
    auto $next = RangeIter$1$next(&$iterator);
    $next.index != 0;
    $next = RangeIter$1$next(&$iterator)) {
      s32 i = $next.Some.$0;
      -- ! user code goes here ! --
  }
*/

THIR *THIRGen::visit_for(ASTFor *ast) {
  THIR_ALLOC(THIRFor, thir, ast)

  THIR *initialization = nullptr;

  if (ast->right->resolved_type->implements(iterable_trait())) {
    ASTMethodCall iter_method;
    ASTDotExpr dot_iter;
    dot_iter.base = ast->right;
    dot_iter.member = {
        .identifier = "iter",
    };
    iter_method.callee = &dot_iter;
    iter_method.arguments->arguments.push_back(ast->right);

    ASTMethodCall next_method;
    ASTDotExpr dot_next;
    dot_next.base = &iter_method;
    dot_next.member = {"next"};

    next_method.accept(&typer);
    initialization = visit_node(&next_method);

  } else if (ast->right->resolved_type->implements(iterator_trait())) {
  }

  thir->initialization = initialization;
  thir->block = visit_node(ast->block);
}

THIR *THIRGen::visit_if(ASTIf *ast) { throw_error("visit_if not implemented", ast->source_range); }

THIR *THIRGen::visit_else(ASTElse *ast) { throw_error("visit_else not implemented", ast->source_range); }

THIR *THIRGen::visit_while(ASTWhile *ast) { throw_error("visit_while not implemented", ast->source_range); }

THIR *THIRGen::visit_defer(ASTDefer *ast) { throw_error("visit_defer not implemented", ast->source_range); }

void THIRGen::visit_tuple_deconstruction(ASTDestructure *ast) {
  throw_error("visit_tuple_deconstruction not implemented", ast->source_range);
}

void THIRGen::visit_impl(ASTImpl *ast) {
  for (const auto &method : ast->methods) {
    current_statement_list->push_back(visit_node(method));
  }
  for (const auto &constant : ast->constants) {
    current_statement_list->push_back(visit_node(constant));
  }
}

void THIRGen::visit_import(ASTImport *ast) { throw_error("visit_import not implemented", ast->source_range); }
void THIRGen::visit_module(ASTModule *ast) { throw_error("visit_module not implemented", ast->source_range); }

void THIRGen::visit_where_statement(ASTWhereStatement *ast) {
  throw_error("visit_where_statement not yet implemented", ast->source_range);
}
