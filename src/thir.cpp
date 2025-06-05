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

#define ENTER_RETURN_OVERRIDE($ast, $block)       \
  ENTER_STMT_VEC(($block))                        \
  $block.push_back(init_override_register($ast)); \
  Defer $ovr_reg_defer([&] { return_override_register = {nullptr}; });

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
  // For defers and block expressions, we write to a variable instead of actually returning.
  if (return_override_register.is_not_null() && ast->expression) {
    THIR_ALLOC(THIRBinExpr, thir, ast)
    thir->op = TType::Assign;
    thir->left = return_override_register.get();
    thir->right = visit_node(ast->expression.get());
    thir->is_statement = true;
    return thir;
  }

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
    auto self = visit_node(base);
    const auto requires_self_ptr = symbol->function.declaration->params->params[0]->self.is_pointer;
    auto base_type = self->type;

    // auto dereference / address of logic.
    if (!base_type->extensions.is_pointer() && requires_self_ptr) {
      THIR_ALLOC(THIRUnaryExpr, thir, base)
      thir->op = TType::And;
      thir->operand = self;
      self = thir;
    } else if (base_type->extensions.is_pointer() && !requires_self_ptr) {
      THIR_ALLOC(THIRUnaryExpr, thir, base)
      thir->op = TType::Mul;
      thir->operand = self;
      self = thir;
    }

    thir->arguments.push_back(self);
    thir->callee = visit_function_declaration_via_symbol(symbol);
  }
  extract_arguments_desugar_defaults(thir->callee, ast->arguments, thir->arguments);
  return thir;
}

THIR *THIRGen::visit_path(ASTPath *ast) {
  if (should_emit_choice_type_marker_variant_instantiation(ast)) {
    return get_choice_type_instantiation_boilerplate(ast);
  }

  auto symbol = ctx.get_symbol(ast).get();
  if (symbol->is_function()) {
    return symbol_map[symbol];
  }
  return symbol_map[symbol];
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
  return nullptr;
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
    overload_call->callee = visit_function_declaration_via_symbol(symbol);
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
    overload_call->callee = visit_function_declaration_via_symbol(symbol);
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
    overload_call->callee = visit_function_declaration_via_symbol(symbol);
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

THIR *THIRGen::visit_tuple(ASTTuple *ast) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast)
  size_t index = 0;
  for (const auto value : ast->values) {
    thir->key_values.push_back({"$" + std::to_string(index++), visit_node(value)});
  }
  return thir;
}
// Use THIRAggregateInitializer here.
THIR *THIRGen::visit_dyn_of(ASTDyn_Of *ast) {
  throw_error("visit_dyn_of not implemented", ast->source_range);
  return nullptr;
}

THIR *THIRGen::visit_range(ASTRange *ast) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast);
  thir->key_values.push_back({RANGE_TYPE_BEGIN_KEY, visit_node(ast->left)});
  thir->key_values.push_back({RANGE_TYPE_END_KEY, visit_node(ast->left)});
  return thir;
}

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
            if (member.default_value) {
              printf("default member emitted for %s\n", member.name.get_str().c_str());
              thir->key_values.push_back({member.name, visit_node(member.default_value.get())});
            } else {
              THIR_ALLOC(THIREmptyInitializer, empty_init, ast);
              empty_init->type = member.type;
              thir->key_values.push_back({member.name, empty_init});
            }
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

THIR *THIRGen::visit_type_of(ASTType_Of *ast) {
  throw_error("visit_type_of not implemented", ast->source_range);
  return nullptr;
}

THIR *THIRGen::visit_cast(ASTCast *ast) {
  THIR_ALLOC(THIRCast, thir, ast);
  thir->operand = visit_node(ast->expression);
  thir->type = ast->target_type->resolved_type;
  return thir;
}

THIR *THIRGen::visit_lambda(ASTLambda *ast) {
  auto symbol = ctx.scope->lookup(ast->unique_identifier);
  if (auto thir = symbol_map[symbol]) {
    return thir;
  }
  THIR_ALLOC(THIRFunction, thir, ast);
  symbol_map[symbol] = thir;
  thir->block = (THIRBlock *)visit_node(ast->block);
  thir->name = ast->unique_identifier;
  for (const auto &ast_param : ast->params->params) {
    THIRParameter thir_param = {
        .name = ast_param->normal.name,
    };
    if (ast_param->normal.default_value) {
      thir_param.default_value = visit_node(ast_param->normal.default_value.get());
    }
    thir->parameters.push_back(thir_param);
  }
  return thir;
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

THIR *THIRGen::visit_function_declaration_via_symbol(Symbol *symbol) {
  if (auto thir = symbol_map[symbol]) {
    return thir;
  }
  return symbol_map[symbol] = visit_function_declaration(symbol->function.declaration);
}

THIR *THIRGen::visit_function_declaration(ASTFunctionDeclaration *ast) {
  const auto symbol = ctx.scope->local_lookup(ast->name);
  THIR_ALLOC(THIRFunction, thir, ast);
  symbol_map[symbol] = thir;

  ENTER_SCOPE(ast->scope);
  convert_function_flags(thir, (FunctionInstanceFlags)ast->flags);
  convert_function_attributes(thir, ast->attributes);
  for (const auto &param : ast->params->params) {
    THIR_ALLOC(THIRVariable, thir_param, param);
    if (param->tag == ASTParamDecl::Normal) {
      thir_param->name = param->normal.name;
      thir_param->type = param->normal.type->resolved_type;
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
    if (DOESNT_HAVE_FLAG(ast->flags, FUNCTION_IS_FORWARD_DECLARED)) {
      auto param_sym = ctx.scope->local_lookup(thir_param->name);
      symbol_map[param_sym] = thir_param;
    }
    thir->parameters.push_back(THIRParameter{
        .name = thir_param->name,
        .default_value = thir_param->value,
    });
  }

  if (thir->name == "main" || thir->is_entry) {
    if (entry_point && entry_point->get_node_type() != THIRNodeType::Program) {
      throw_error(
          "multiple functions with the @[entry] attribute were found, or multiple 'main()' functions were found",
          ast->source_range);
    }

    entry_point = thir;
    thir->is_entry = true;
  }

  if (thir->is_exported || thir->is_extern || thir->is_no_mangle) {
    thir->name = ast->name;
  } else {
    thir->name = ast->scope->full_name();
  }

  if (ast->block) {
    thir->block = (THIRBlock *)visit_block(ast->block.get());
  }

  return thir;
}

THIR *THIRGen::visit_variable(ASTVariable *ast) {
  THIR_ALLOC(THIRVariable, thir, ast);

  thir->is_global = !ast->is_local;

  auto symbol = ctx.scope->local_lookup(ast->name);
  symbol_map[symbol] = thir;

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
    empty_init->type = ast->type->resolved_type;
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

// TODO:
/*
  Right now, we're just going to lower this into a bunch of if-else branches.
  This maintains the previous meaning of all the programs that exist in the language

  However, with a future LLVM backend and an improvement of the behaviour of switch statements in general,
  We're going to need a THIRSwitch, because it won't just act as an if-else (fall throughs, jump table optimizations,
  etc)
*/
THIR *THIRGen::visit_switch(ASTSwitch *ast) {
  // TODO: maybe we want to throw an error for this, instead of just optimizing it out.
  if (ast->branches.empty() && !ast->default_case) {
    return nullptr;
  }

  // this is totally redundant so, whatever, just emit it as if it were a block.
  if (ast->branches.empty() && ast->default_case) {
    return visit_node(ast->default_case.get());
  }

  static int idx = 0;

  THIR_ALLOC(THIRVariable, cached_expr, ast);
  cached_expr->name = std::format(THIR_SWITCH_CACHED_EXPRESSION_KEY_FORMAT, idx++);
  cached_expr->is_global = false;
  cached_expr->type = ast->expression->resolved_type;
  cached_expr->value = visit_node(ast->expression);
  current_statement_list->push_back(cached_expr);

  const auto get_condition_comparator = [&](size_t index) -> THIR * {
    auto operator_overload_ty = find_operator_overload(CONST, cached_expr->type, TType::EQ, OPERATION_BINARY);
    auto left = cached_expr;
    auto right = visit_node(ast->branches[index].expression);
    if (operator_overload_ty == Type::INVALID_TYPE) {  // normal equality comparison.
      THIR_ALLOC(THIRBinExpr, binexpr, ast);
      binexpr->left = left;
      binexpr->right = right;
      binexpr->op = TType::EQ;
      binexpr->type = bool_type();
      return binexpr;
    } else {  // call an operator overload
      THIR_ALLOC(THIRCall, overload_call, ast);
      auto scope = left->type->info->scope;
      auto symbol = scope->local_lookup(get_operator_overload_name(TType::EQ, OPERATION_BINARY));
      overload_call->callee = visit_function_declaration_via_symbol(symbol);
      overload_call->arguments.push_back(left);
      overload_call->arguments.push_back(right);
      return overload_call;
    }
  };

  THIR_ALLOC(THIRIf, first_case, ast)
  first_case->condition = get_condition_comparator(0);
  first_case->block = (THIRBlock *)visit_node(ast->branches[0].block);
  first_case->is_statement = true;

  THIRIf *the_if = first_case;
  for (size_t i = 1; i < ast->branches.size(); ++i) {
    const auto &ast_branch = ast->branches[i];
    THIR_ALLOC(THIRIf, thir_branch, ast)
    thir_branch->condition = get_condition_comparator(i);
    thir_branch->block = (THIRBlock *)visit_node(ast_branch.block);
    thir_branch->is_statement = true;
    the_if->_else = thir_branch;
    the_if = thir_branch;
  }

  if (ast->default_case) {
    the_if->_else = (THIRBlock *)visit_node(ast->default_case.get());
  }

  return first_case;
}

THIR *THIRGen::visit_program(ASTProgram *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRProgram, thir, ast);
  entry_point = thir;  // We default to using the entire program as an entry point (for emitting), but if we get a
                       // main/@[entry] function, this gets replaced.
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
  // ! I put this on pause because I hadn't done `choice` types yet, and for loops depend on them.

  THIRVariable *variable = nullptr;
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

    THIR_ALLOC(THIRVariable, variable, ast);
    variable->is_global = false;
    variable->name = ast->left.identifier;
    variable->value = visit_node(&next_method);
    thir->initialization = variable;
  } else if (ast->right->resolved_type->implements(iterator_trait())) {
    ASTMethodCall next_method;
    ASTDotExpr dot_next;
    dot_next.base = ast->right;
    dot_next.member = {"next"};
    next_method.accept(&typer);

    THIR_ALLOC(THIRVariable, variable, ast);
    variable->is_global = false;
    variable->name = ast->left.identifier;
    variable->value = visit_node(&next_method);
    variable->type = variable->value->type;
    thir->initialization = variable;
  }

  THIR_ALLOC(THIRBinExpr, condition, ast);
  THIR_ALLOC(THIRMemberAccess, member_access, ast);

  member_access->base = variable;
  member_access->member = DISCRIMINANT_KEY;
  member_access->type = u32_type();

  THIR_ALLOC(THIRLiteral, discriminant_literal, ast)
  discriminant_literal->value = OPTION_NONE_DISCRIMINANT_VALUE;
  discriminant_literal->type = u32_type();

  condition->left = member_access;
  condition->op = TType::NEQ;
  condition->right = discriminant_literal;
  thir->condition = condition;

  THIR_ALLOC(THIRBinExpr, increment, ast)
  increment->type = void_type();
  increment->left = variable;
  increment->op = TType::Assign;
  increment->right = variable->value;

  thir->block = visit_node(ast->block);
  return thir;
}

THIR *THIRGen::visit_if(ASTIf *ast) {
  THIR_ALLOC(THIRIf, thir, ast)
  thir->condition = visit_node(ast->condition);

  const auto finish_visiting = [&] {
    thir->block = (THIRBlock *)visit_block(ast->block);
    if (ast->_else) {
      thir->_else = visit_else(ast->_else.get());
    }
    return thir;
  };

  if (ast->is_expression) {
    THIR_ALLOC(THIRExprBlock, block, ast);
    ENTER_RETURN_OVERRIDE(ast, block->statements);
    block->return_register = return_override_register.get();
    block->statements.push_back(finish_visiting());
    return block;
  }

  return finish_visiting();
}

THIR *THIRGen::visit_else(ASTElse *ast) {
  if (ast->_if) {
    return visit_node(ast->_if.get());
  } else {
    return visit_node(ast->block.get());
  }
}

THIR *THIRGen::visit_while(ASTWhile *ast) {
  THIR_ALLOC(THIRWhile, thir, ast)
  if (ast->condition) {
    thir->condition = visit_node(ast->condition.get());
  }
  thir->block = (THIRBlock *)visit_node(ast->block);
  return thir;
}

THIR *THIRGen::visit_defer(ASTDefer *ast) {
  throw_error("visit_defer not implemented", ast->source_range);
  return nullptr;
}

// x, y := (0, 0);, *x, *y := ...
// we should refactor the syntax for this to be &const x, &mut y := ...
void THIRGen::visit_tuple_deconstruction(ASTDestructure *ast) {
  const auto type = ast->right->resolved_type;
  auto members_iter = type->info->members.begin();

  THIR *base = visit_node(ast->right);
  THIR_ALLOC(THIRVariable, cached_base, ast->right);
  static int key = 0;
  cached_base->name = "$destructure$" + std::to_string(key);
  cached_base->value = base;
  cached_base->type = type;
  cached_base->is_global = false;
  cached_base->is_statement = true;
  current_statement_list->push_back(cached_base);

  for (const DestructureElement &element : ast->elements) {
    THIR_ALLOC(THIRVariable, var, ast);
    var->name = element.identifier;
    var->type = element.type;

    THIR_ALLOC(THIRMemberAccess, member_access, ast);
    var->value = member_access;

    const auto member = *members_iter;
    members_iter++;

    member_access->member = member.name;
    member_access->base = cached_base;
    member_access->type = element.type;

    if (element.semantic == VALUE_SEMANTIC_POINTER) {
      THIR_ALLOC(THIRUnaryExpr, unary, ast);
      unary->op = TType::And;
      unary->operand = member_access;
      var->value = unary;
    }

    auto symbol = ctx.scope->local_lookup(element.identifier);
    symbol_map[symbol] = var;

    current_statement_list->push_back(var);
  }
}

void THIRGen::visit_impl(ASTImpl *ast) {
  for (const auto &method : ast->methods) {
    current_statement_list->push_back(visit_node(method));
  }
  for (const auto &constant : ast->constants) {
    current_statement_list->push_back(visit_node(constant));
  }
}

void THIRGen::visit_import(ASTImport *ast) {
  ENTER_SCOPE(ast->scope);
  for (const auto &ast_stmt : ast->statements) {
    visit_node(ast_stmt);
  }
}

void THIRGen::visit_module(ASTModule *ast) {
  ENTER_SCOPE(ast->scope);
  for (const auto &ast_stmt : ast->statements) {
    if (auto thir_stmt = visit_node(ast_stmt)) {
      current_statement_list->push_back(thir_stmt);
    }
  }
}

void THIRGen::visit_where_statement(ASTWhereStatement *ast) {
  if (ast->should_compile) {
    ENTER_SCOPE(ast->block->scope);
    for (const auto &ast_stmt : ast->block->statements) {
      if (auto thir_stmt = visit_node(ast_stmt)) {
        current_statement_list->push_back(thir_stmt);
      }
    }
  }
}
