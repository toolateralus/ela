#include "thir.hpp"
#include <deque>
#include <format>
#include <string>
#include "ast.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
// this is used directly. clangd stop b*tchin
#include "visitor.hpp"


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

void THIRGen::make_destructure_for_pattern_match(ASTPatternMatch *ast, THIR *object, Scope *block_scope,
                                                 std::vector<THIR *> &statements, Type *variant_type,
                                                 const InternedString &variant_name) {
  auto variant_member_access = make_member_access(ast->source_range, object, {{variant_type, variant_name}});

  switch (ast->pattern_tag) {
    case ASTPatternMatch::STRUCT: {
      auto &pattern = ast->struct_pattern;
      for (const StructPattern::Part &part : pattern.parts) {
        THIR_ALLOC(THIRVariable, var, ast)
        var->name = part.var_name;
        var->type = part.resolved_type;
        var->value =
            make_member_access(ast->source_range, variant_member_access, {{part.resolved_type, part.field_name}});
        if (part.semantic == PTRN_MTCH_PTR_MUT || part.semantic == PTR_MTCH_PTR_CONST) {
          var->value = take_address_of(var->value, ast);
        }
        statements.insert(statements.begin(), var);

        auto symbol = block_scope->lookup(part.var_name);
        symbol_map[symbol] = var;
      }
    } break;
    case ASTPatternMatch::TUPLE: {
      auto &pattern = ast->tuple_pattern;
      size_t index = 0;
      for (const TuplePattern::Part &part : pattern.parts) {
        THIR_ALLOC(THIRVariable, var, ast)
        var->name = part.var_name;
        var->type = part.resolved_type;
        var->value = make_member_access(ast->source_range, variant_member_access,
                                        {{part.resolved_type, std::to_string(index++)}});
        if (part.semantic == PTRN_MTCH_PTR_MUT || part.semantic == PTR_MTCH_PTR_CONST) {
          var->value = take_address_of(var->value, ast);
        }
        statements.insert(statements.begin(), var);

        auto symbol = block_scope->lookup(part.var_name);
        symbol_map[symbol] = var;
      }
    } break;
    default:
      return;
  }
}

THIR *THIRGen::visit_pattern_match_condition(ASTPatternMatch *ast, THIR *cached_object, const size_t discriminant) {
  THIR_ALLOC(THIRMemberAccess, discriminant_access, ast)
  discriminant_access->base = cached_object;
  discriminant_access->member = DISCRIMINANT_KEY;
  discriminant_access->type = u64_type();
  THIR_ALLOC(THIRBinExpr, thir, ast);
  thir->left = discriminant_access;
  thir->op = TType::EQ;
  thir->right = make_literal(std::to_string(discriminant), ast->source_range, u64_type());
  return thir;
}

THIR *THIRGen::visit_pattern_match(ASTPatternMatch *ast, Scope *scope, std::vector<THIR *> &statements) {
  static size_t id = 0;

  auto cached_object =
      make_variable(std::format(THIR_PATTERN_MATCH_CACHED_KEY_FORMAT, id++), visit_node(ast->object), ast->object);

  current_statement_list->push_back(cached_object);

  const Type *choice_type = ast->target_type_path->resolved_type;
  const ChoiceTypeInfo *info = choice_type->info->as<ChoiceTypeInfo>();
  const ASTPath::Segment &segment = ast->target_type_path->segments.back();

  const size_t discriminant = info->get_variant_discriminant(segment.identifier);
  Type *variant_type = info->get_variant_type(segment.identifier);
  const InternedString variant_name = segment.identifier;

  auto condition = visit_pattern_match_condition(ast, cached_object, discriminant);
  make_destructure_for_pattern_match(ast, cached_object, scope, statements, variant_type, variant_name);
  return condition;
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
  thir->key_values.push_back({RANGE_TYPE_END_KEY, visit_node(ast->right)});
  return thir;
}

THIR *THIRGen::initialize(const SourceRange &source_range, Type *type,
                          const std::vector<std::pair<InternedString, ASTExpr *>> &key_values) {
  const auto info = type->info;

  // TODO: might need more ignored things here
  if (info->members.empty() || type->is_kind(TYPE_CHOICE)) {
    THIR_ALLOC_NO_SRC_RANGE(THIREmptyInitializer, thir);
    thir->source_range = source_range;
    thir->type = type;
    return thir;
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->source_range = source_range;
  thir->type = type;

  for (const auto &member : info->members) {
    ASTExpr *initializer = nullptr;
    for (const auto &[key, value] : key_values) {
      if (key == member.name) {
        initializer = value;
        break;
      }
    }
    const auto is_non_ptr_non_union_struct =
        member.type->is_kind(TYPE_STRUCT) && member.type->extensions.has_no_extensions() &&
        DOESNT_HAVE_FLAG(member.type->info->as<StructTypeInfo>()->flags, STRUCT_FLAG_IS_UNION);

    if (initializer) {
      thir->key_values.push_back({member.name, visit_node(initializer)});
    } else if (member.default_value) {
      thir->key_values.push_back({member.name, visit_node(member.default_value.get())});
    } else if (is_non_ptr_non_union_struct) {
      thir->key_values.push_back({member.name, initialize(source_range, member.type, {})});
    } else {
      THIR_ALLOC_NO_SRC_RANGE(THIREmptyInitializer, empty_init);
      empty_init->source_range = source_range;
      empty_init->type = type;
      empty_init->type = member.type;
      thir->key_values.push_back({member.name, empty_init});
    }
  }
  return thir;
}

THIR *THIRGen::visit_initializer_list(ASTInitializerList *ast) {
  const auto type = ast->resolved_type;
  switch (ast->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      return initialize(ast->source_range, type, {});
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      if (type->kind == TYPE_STRUCT) {
        return initialize(ast->source_range, type, ast->key_values);
      } else if (type->kind == TYPE_CHOICE) {
        // Choice variant instantiation
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
      break;
    }
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      THIR_ALLOC(THIRCollectionInitializer, thir, ast)
      for (const auto &value : ast->values) {
        thir->values.push_back(visit_node(value));
      }
      return thir;
    }
  }
  return nullptr;
}

THIR *THIRGen::visit_type_of(ASTType_Of *ast) { return to_reflection_type_struct(ast->target->resolved_type); }

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
  if (ast->generic_parameters.size()) {
    for (auto &monomorphization : ast->generic_instantiations) {
      visit_function_declaration((ASTFunctionDeclaration *)monomorphization.declaration);
    }
    return nullptr;
  }
  THIR_ALLOC(THIRFunction, thir, ast);
  Symbol *symbol;
  if (ast->declaring_type) {
    symbol = ast->declaring_type->info->scope->local_lookup(ast->name);
  } else {
    symbol = ctx.scope->local_lookup(ast->name);
  }
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

  current_statement_list->push_back(thir);
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
    thir->value = initialize(thir->source_range, ast->type->resolved_type, {});
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
  if (ast->generic_parameters.size()) {
    return nullptr;
  }
  THIR_ALLOC(THIRType, thir, ast);
  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_choice_declaration(ASTChoiceDeclaration *ast) {
  if (ast->generic_parameters.size()) {
    return nullptr;
  }
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
  if (ast->branches.empty() && !ast->default_branch) {
    return nullptr;
  }

  // this is totally redundant so, whatever, just emit it as if it were a block.
  if (ast->branches.empty() && ast->default_branch) {
    return visit_node(ast->default_branch.get());
  }

  static int idx = 0;

  auto cached_expr = make_variable(std::format(THIR_SWITCH_CACHED_EXPRESSION_KEY_FORMAT, idx++),
                                   visit_node(ast->expression), ast->expression);

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

  if (ast->default_branch) {
    the_if->_else = (THIRBlock *)visit_node(ast->default_branch.get());
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

THIR *THIRGen::take_address_of(THIR *operand, ASTNode *ast) {
  THIR_ALLOC(THIRUnaryExpr, thir, ast);
  thir->op = TType::And;
  thir->type = operand->type->take_pointer_to(true);
  thir->operand = operand;
  return thir;
}

THIRVariable *THIRGen::make_variable(const InternedString &name, THIR *value, ASTNode *ast, bool is_global) {
  THIR_ALLOC_NO_SRC_RANGE(THIRVariable, thir)
  if (ast) {
    thir->source_range = ast->source_range;
  }
  thir->name = name;
  thir->is_global = is_global;
  thir->is_statement = true;
  thir->type = value->type;
  thir->value = value;
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
  THIR_ALLOC(THIRFor, thir, ast);

  static size_t id = 0;
  const InternedString result_key = std::format(THIR_FOR_LOOP_ITER_OPTION_KEY_FORMAT, id++);
  const InternedString cached_key = std::format(THIR_FOR_LOOP_ITER_CACHED_KEY_FORMAT, id);

  ENTER_SCOPE(ast->block->scope);

  // 1. Cache the iterable/expression (evaluate once)
  THIR *iterable_value = visit_node(ast->right);
  THIR *iterable_var = make_variable(cached_key, iterable_value, ast->right);

  current_statement_list->push_back(iterable_var);

  // 2. Get the iterator (call iter() or use as iterator directly)
  THIR *iterator_var = nullptr;
  if (iterable_var->type->implements(iterable_trait())) {
    auto iter_symbol = iterable_var->type->info->scope->local_lookup("iter");
    bool expects_ptr = iter_symbol && iter_symbol->function.declaration->params->params[0]->self.is_pointer;
    THIR *iter_arg = (expects_ptr && !iterable_var->type->extensions.is_pointer()) ? take_address_of(iterable_var, ast)
                                                                                   : iterable_var;

    THIR_ALLOC(THIRCall, iter_call, ast);
    iter_call->callee = visit_function_declaration_via_symbol(iter_symbol);
    iter_call->arguments.push_back(iter_arg);

    const auto *info = iter_call->callee->type->info->as<FunctionTypeInfo>();
    iter_call->type = info->return_type;

    iterator_var = make_variable(std::format(THIR_FOR_LOOP_ITER_CACHED_KEY_FORMAT, ++id), iter_call, ast);
    current_statement_list->push_back(iterator_var);
  } else if (iterable_var->type->implements(iterator_trait())) {
    iterator_var = iterable_var;
  }

  // 3. Call next() on the iterator
  auto next_symbol = iterator_var->type->info->scope->local_lookup("next");
  bool next_expects_ptr = next_symbol && next_symbol->function.declaration->params->params[0]->self.is_pointer;
  THIR *next_arg = (next_expects_ptr && !iterator_var->type->extensions.is_pointer())
                       ? take_address_of(iterator_var, ast)
                       : iterator_var;

  THIR_ALLOC(THIRCall, next_call, ast);
  next_call->callee = visit_function_declaration_via_symbol(next_symbol);

  const auto *next_info = next_call->callee->type->info->as<FunctionTypeInfo>();
  next_call->type = next_info->return_type;
  next_call->arguments.push_back(next_arg);

  // 4. Assign next_call to a loop variable
  THIR_ALLOC(THIRVariable, next_var, ast);
  next_var->is_global = false;
  next_var->name = result_key;
  next_var->value = next_call;
  next_var->type = next_call->type;
  thir->initialization = next_var;

  // 5. Build the loop condition: $next.index == OPTION_SOME_DISCRIMINANT_VALUE
  THIR_ALLOC(THIRBinExpr, condition, ast);
  THIR_ALLOC(THIRMemberAccess, member_access, ast);
  member_access->base = next_var;
  member_access->member = DISCRIMINANT_KEY;
  member_access->type = u32_type();

  THIR *discriminant_literal = make_literal(OPTION_SOME_DISCRIMINANT_VALUE, ast->source_range, u32_type());

  condition->left = member_access;
  condition->op = TType::EQ;
  condition->right = discriminant_literal;
  thir->condition = condition;

  // 6. Increment: $next = next($iterator)
  THIR_ALLOC(THIRBinExpr, increment, ast);
  increment->type = void_type();
  increment->left = next_var;
  increment->op = TType::Assign;
  increment->right = next_var->value;
  thir->increment = increment;

  // 7. Loop variable binding (identifier only, destructure TODO)
  THIR_ALLOC(THIRVariable, identifier_var, ast);
  identifier_var->name = ast->left.identifier;
  identifier_var->type = ast->identifier_type;
  auto identifier_symbol = ctx.scope->lookup(ast->left.identifier);
  symbol_map[identifier_symbol] = identifier_var;

  // Unwrap: $next.Some.$0
  const auto some_type = next_var->type->info->as<ChoiceTypeInfo>()->get_variant_type("Some");
  THIR_ALLOC(THIRMemberAccess, unwrap_some, ast);
  unwrap_some->base = next_var;
  unwrap_some->member = "Some";
  unwrap_some->type = some_type;

  THIR_ALLOC(THIRMemberAccess, unwrap_0, ast);
  unwrap_0->base = unwrap_some;
  unwrap_0->member = "0";

  identifier_var->value = unwrap_0;

  thir->block = visit_node(ast->block);
  THIRBlock *block = (THIRBlock *)thir->block;
  block->statements.insert(block->statements.begin(), identifier_var);

  return thir;
}

THIR *THIRGen::visit_if(ASTIf *ast) {
  THIR_ALLOC(THIRIf, thir, ast)

  std::vector<THIR *> statements;
  if (ast->condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
    thir->condition = visit_pattern_match((ASTPatternMatch *)ast->condition, ast->block->scope, statements);
  } else {
    thir->condition = visit_node(ast->condition);
  }

  const auto finish_visiting = [&] {
    thir->block = (THIRBlock *)visit_block(ast->block);
    for (const auto &stmt : statements) {
      thir->block->statements.insert(thir->block->statements.begin(), stmt);
    }
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
  static size_t id = 0;
  const auto cached_base = make_variable("$destructure$" + std::to_string(id), base, ast->right);
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
  if (ast->generic_parameters.size()) {
    for (auto &monomorphization : ast->generic_instantiations) {
      visit_impl((ASTImpl *)monomorphization.declaration);
    }
    return;
  }

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

THIR *THIRGen::get_method_struct(const std::string &name, Type *type) {
  (void)name;
  (void)type;
  return nullptr;
}

THIR *THIRGen::get_field_struct(const std::string &name, Type *type, Type *parent_type) {
  (void)name;
  (void)type;
  (void)parent_type;
  return nullptr;
}

ReflectionInfo THIRGen::create_reflection_type_struct(Type *type) {
  ReflectionInfo info;
  info.created = true;
  static Type *type_type = ctx.scope->find_type_id("Type", {});
  info.definition =
      make_variable(std::format(TYPE_INFO_IDENTIFIER_FORMAT, type->uid), initialize({}, type_type, {}), nullptr);
  return info;
}

THIR *THIRGen::to_reflection_type_struct(Type *type) {
  if (!type) {
    throw_error("internal compiler error: encountered a null type while emitting THIR for a reflection object.", {});
  }

  /*
    Has this type already been emitted to a struct?
    if so, reference it.
  */
  ReflectionInfo &reflection_info = reflected_upon_types[type];
  if (reflection_info.has_been_created()) {
    return reflection_info.reference;
  }

  reflection_info = create_reflection_type_struct(type);
  return reflection_info.reference;
}

THIR *THIRGen::make_str(const InternedString &value, const SourceRange &src_range) {
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->source_range = src_range;
  thir->key_values.push_back({"data", make_literal(value, src_range, u8_ptr_type())});
  thir->key_values.push_back({"length", make_literal(std::to_string(calculate_strings_actual_length(value.get_str())),
                                                     src_range, u64_type())});
  return thir;
}

THIR *THIRGen::make_literal(const InternedString &value, const SourceRange &src_range, Type *type) {
  THIR_ALLOC_NO_SRC_RANGE(THIRLiteral, thir);
  thir->source_range = src_range;
  thir->value = value;
  thir->type = type;
  return thir;
}

THIR *THIRGen::make_member_access(const SourceRange &range, THIR *base,
                                  std::deque<std::pair<Type *, InternedString>> parts) {
  THIR_ALLOC_NO_SRC_RANGE(THIRMemberAccess, thir)
  thir->source_range = range;
  thir->base = base;
  auto [type, member] = parts.front();
  thir->member = member;
  thir->type = type;
  parts.pop_front();
  THIRMemberAccess *last_part = thir;
  while (!parts.empty()) {
    THIR_ALLOC_NO_SRC_RANGE(THIRMemberAccess, member_access);
    auto [type, member] = parts.front();
    parts.pop_front();
    member_access->base = last_part;
    member_access->member = member;
    member_access->type = type;
    last_part = member_access;
  }
  return thir;
}