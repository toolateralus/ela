#include "thir.hpp"
#include <deque>
#include <string>
#include "ast.hpp"
#include "error.hpp"
#include "interpreter.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "visitor.hpp"

static void check_deprecated(THIR *reciever, const std::vector<Attribute> &attrs) {
  for (const auto &attr : attrs) {
    switch (attr.tag) {
      case ATTRIBUTE_DEPRECATED:
        reciever->deprecated_attr = attr;
        reciever->deprecated = true;
        break;
      default:
        break;
    }
  }
}

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
  if (!ast->resolved_type) {
    // This should never happen, debugging something
    // TODO: remove this
    return false;
  }

  if (!ast->resolved_type->is_kind(TYPE_CHOICE) || ast->resolved_type->has_extensions()) {
    return false;
  }

  ChoiceTypeInfo *info = ast->resolved_type->info->as<ChoiceTypeInfo>();
  const auto last_segment = ast->segments.back();

  for (const auto &member : info->members) {
    if (last_segment.get_identifier() == member.name) {
      return true;
    }
  }

  return false;
}

static inline THIRAggregateInitializer *get_choice_type_instantiation_boilerplate(ASTPath *ast, THIRGen *thirgen) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast);
  const auto type = ast->resolved_type;
  const auto path = ast;
  const auto variant_name = path->segments.back().get_identifier();
  const auto info = type->info->as<ChoiceTypeInfo>();
  const auto discriminant = info->get_variant_discriminant(variant_name);
  auto literal = thirgen->make_literal(std::to_string(discriminant), {}, u32_type(), ASTLiteral::Integer);
  thir->key_values.push_back({CHOICE_TYPE_DISCRIMINANT_KEY, literal});
  return thir;
}

/*
  I put some of these super trivial nodes up top here so they stay out of the way
*/
THIR *THIRGen::visit_size_of(ASTSize_Of *ast) {
  return make_literal(std::to_string(ast->target_type->resolved_type->size_in_bytes()), ast->span, u64_type(),
                      ASTLiteral::Integer);
}

THIR *THIRGen::visit_continue(ASTContinue *ast) {
  THIR_ALLOC(THIRContinue, thir, ast);
  auto defers = collect_defers_up_to(DeferBoundary::LOOP);
  for (auto d : defers) {
    current_statement_list->push_back(d);
  }
  return thir;
}

THIR *THIRGen::visit_break(ASTBreak *ast) {
  THIR_ALLOC(THIRBreak, thir, ast);
  auto defers = collect_defers_up_to(DeferBoundary::LOOP);
  for (auto d : defers) {
    current_statement_list->push_back(d);
  }
  return thir;
}

THIR *THIRGen::visit_return(ASTReturn *ast) {
  if (return_override_register.is_not_null() && ast->expression) {
    THIR_ALLOC(THIRBinExpr, assign_thir, ast)
    assign_thir->op = TType::Assign;
    assign_thir->left = return_override_register.get();
    assign_thir->right = visit_node(ast->expression.get());
    assign_thir->is_statement = true;

    auto defers = collect_defers_up_to(DeferBoundary::FUNCTION);
    for (auto d : defers) current_statement_list->push_back(d);

    return assign_thir;
  }

  THIR_ALLOC(THIRReturn, ret, ast);

  if (ast->expression) {
    THIR *expr_val = visit_node(ast->expression.get());

    auto defers = collect_defers_up_to(DeferBoundary::FUNCTION);
    if (!defers.empty()) {
      THIRVariable *tmp = make_variable(get_temporary_variable(), expr_val, ast);
      current_statement_list->push_back(tmp);

      for (auto d : defers) {
        current_statement_list->push_back(d);
      }

      ret->expression = tmp;
    } else {
      ret->expression = expr_val;
    }
  } else {
    auto defers = collect_defers_up_to(DeferBoundary::FUNCTION);
    for (auto d : defers) {
      current_statement_list->push_back(d);
    }
  }

  return ret;
}

THIR *THIRGen::visit_expr_statement(ASTExprStatement *ast) {
  auto thir = visit_node(ast->expression);
  thir->is_statement = true;
  return thir;
}

void THIRGen::extract_arguments_desugar_defaults(const THIR *callee, const ASTArguments *in_args, std::vector<THIR *> &out_args,
                                                 Nullable<THIR> self) {
  auto old_list = current_expression_list;
  Defer _([&] { current_expression_list = old_list; });
  current_expression_list = &out_args;
  out_args.clear();
  if (callee->get_node_type() == THIRNodeType::Function) {
    const auto function = (const THIRFunction *)callee;
    const auto &params = function->parameters;
    const auto &ast_args = in_args->arguments;

    if (self.is_not_null()) {
      out_args.push_back(self.get());
    }
    size_t param_index = out_args.size();  // start at current args (handles implicit self)
    size_t ast_index = 0;

    for (; param_index < params.size() && ast_index < ast_args.size(); ++param_index, ++ast_index) {
      auto result = visit_node(ast_args[ast_index]);
      if (result != THIRNoop::shared()) {
        out_args.push_back(result);
      }
    }

    for (; ast_index < ast_args.size(); ++ast_index) {
      auto result = visit_node(ast_args[ast_index]);
      if (result != THIRNoop::shared()) {
        out_args.push_back(result);
      }
    }

    for (; param_index < params.size(); ++param_index) {
      if (params[param_index].default_value) {
        out_args.push_back(params[param_index].default_value);
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
    const auto variant_name = path->segments.back().get_identifier();
    const auto type = ast->callee->resolved_type;
    const auto info = type->info->as<ChoiceTypeInfo>();
    const auto thir = get_choice_type_instantiation_boilerplate(path, this);
    ASTTuple tuple;
    tuple.resolved_type = info->get_variant_type(variant_name);
    tuple.values = ast->arguments->arguments;
    thir->key_values.push_back({variant_name, visit_node(&tuple)});
    return thir;
  }

  auto old = is_making_call;
  is_making_call = true;
  thir->callee = visit_node(ast->callee);
  is_making_call = old;

  if (!thir->callee) {
    throw_error("INTERNAL COMPILER ERROR: unable to locate callee for function", ast->span);
  }

  // macro expansion, we dont emit anything.
  if (thir->callee == THIRNoop::shared()) {
    return THIRNoop::shared();
  }

  check_for_deprecation(ast->span, thir->callee);

  extract_arguments_desugar_defaults(thir->callee, ast->arguments, thir->arguments);
  return thir;
}

THIR *THIRGen::try_deref_or_take_ptr_to_if_needed(ASTExpr *const base, THIR *target, const bool requires_self_ptr) {
  if (!target->type->is_pointer() && requires_self_ptr) {
    THIR_ALLOC(THIRUnaryExpr, thir, base)
    thir->op = TType::And;
    thir->operand = target;
    return thir;
  } else if (target->type->is_pointer() && !requires_self_ptr) {
    THIR_ALLOC(THIRUnaryExpr, thir, base)
    thir->op = TType::Mul;
    thir->operand = target;
    return thir;
  }
  return target;
}

THIR *THIRGen::visit_method_call(ASTMethodCall *ast) {
  THIR_ALLOC(THIRCall, thir, ast);
  const auto base = ast->callee->base;
  const auto symbol = get_symbol(ast->callee);

  thir->is_dyn_call = ast->inserted_dyn_arg;
  thir->dyn_method_name = ast->dyn_method_name;

  THIR *self = nullptr;
  if (symbol->is_variable) {
    thir->callee = visit_node(ast->callee);
  } else {
    // Push the self argument
    self = visit_node(base);

    const auto requires_self_ptr = symbol->function.declaration->requires_self_ptr();

    // auto dereference / address of logic.
    self = try_deref_or_take_ptr_to_if_needed(base, self, requires_self_ptr);

    auto fn_sym = get_symbol(ast->callee);
    auto fn = fn_sym->function.declaration;

    auto old = is_making_call;
    is_making_call = true;
    if (fn->generic_parameters.size()) {
      auto generics = ast->callee->member.get_resolved_generics();
      auto function = find_generic_instance(fn->generic_instantiations, generics);
      thir->callee = visit_function_declaration((ASTFunctionDeclaration *)function);
    } else {
      thir->callee = visit_function_declaration(fn);
    }
    is_making_call = old;
  }

  if (!thir->callee) {
    throw_error("THIRGen unable to get callee for method", ast->span);
  }

  extract_arguments_desugar_defaults(thir->callee, ast->arguments, thir->arguments, self);
  return thir;
}

Symbol *THIRGen::get_symbol(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_TYPE: {
      auto type_node = static_cast<ASTType *>(node);
      if (type_node->kind != ASTType::NORMAL) {
        return nullptr;
      }
      return get_symbol(type_node->normal.path);
    }
    case AST_NODE_PATH: {
      auto path = static_cast<ASTPath *>(node);
      Scope *scope = ctx.scope;
      size_t index = 0;
      for (auto &segment : path->segments) {
        Symbol *symbol = nullptr;
        Type *type = nullptr;
        if (segment.tag == ASTPath::Segment::IDENTIFIER) {
          auto ident = segment.get_identifier();
          symbol = scope->lookup(ident);
          if (!symbol) {
            throw_error("INTERNAL COMPILER ERROR: symbol null in path", node->span);
          }
        } else if (segment.tag == ASTPath::Segment::TYPE) {
          type = segment.get_type()->resolved_type;
        } else {
          throw_error("INTERNAL COMPILER ERROR: path segment was neither expression nor identifier", node->span);
          return nullptr;
        }

        if (index == path->length() - 1) {
          return symbol;
        }

        if (type) {
          scope = type->info->scope;
        } else if (!segment.generic_arguments.empty()) {
          if (symbol->is_type) {
            auto decl = dynamic_cast<ASTDeclaration *>(symbol->type.declaration.get());

            if (!decl) {
              throw_error("Cannot apply generic arguments to that type", node->span);
            }

            auto instantiation = find_generic_instance(((ASTDeclaration *)symbol->type.declaration.get())->generic_instantiations,
                                                       segment.get_resolved_generics());
            auto type = instantiation->resolved_type;
            scope = type->info->scope;
          } else {
            return nullptr;
          }
        } else {
          if (symbol->is_module) {
            scope = symbol->module.declaration->scope;
          } else if (symbol->is_type) {
            auto resolved_type = symbol->resolved_type;
            scope = resolved_type->info->scope;

            // We do this here because it's not neccesarily true that these would have been visited already,
            // duplicates don't matter all that much, and we _need_ the child symbols to be bound otherwise
            // we get null variables in paths.

            // There's probably a much better solution than this, but this works.

            if (resolved_type->declaring_node) {
              auto thir = visit_node(resolved_type->declaring_node.get());
              if (thir) program->statements.push_back(thir);
            }

          } else {
            return nullptr;
          }
        }
        index++;
      }
    } break;
    case AST_NODE_DOT_EXPR: {
      auto dotnode = static_cast<ASTDotExpr *>(node);
      auto type = dotnode->base->resolved_type;
      auto symbol = type->info->scope->local_lookup(dotnode->member.get_identifier());
      // Implicit dereference, we look at the base scope.
      if (!symbol && type->is_pointer()) {
        type = type->get_element_type();
        symbol = type->info->scope->local_lookup(dotnode->member.get_identifier());
      }
      return symbol;
    }
    default:
      return nullptr;
  }
  return nullptr;
}

THIR *THIRGen::visit_path(ASTPath *ast) {
  if (should_emit_choice_type_marker_variant_instantiation(ast)) {
    return get_choice_type_instantiation_boilerplate(ast, this);
  }

  auto symbol = get_symbol(ast);

  if (!symbol) {
    throw_error("INTERNAL COMPILER ERROR: visiting path yielded no symbol but the typer didn't catch it", ast->span);
  }

  if (symbol_map.contains(symbol)) {
    return symbol_map[symbol];
  }

  if (symbol->is_variable) {
    auto var_ast = symbol->variable.declaration.get();
    if (!var_ast) {
      throw_error("INTERNAL COMPILER ERROR: variable declaration null in path", ast->span);
    }
    return visit_node(var_ast);
  }

  ASTDeclaration *decl = nullptr;
  if (symbol->is_function) {
    decl = symbol->function.declaration;
  } else if (symbol->is_type) {
    decl = dynamic_cast<ASTDeclaration *>(symbol->type.declaration.get());
    if (!decl) {
      decl = dynamic_cast<ASTDeclaration *>(symbol->resolved_type->declaring_node.get());
    }
  }

  if (!decl) {
    throw_error("INTERNAL COMPILER ERROR: Invalid type in path", ast->span);
  }

  if (!decl->generic_parameters.empty()) {
    decl = find_generic_instance(decl->generic_instantiations, ast->segments.back().get_resolved_generics());
  }

  return visit_node(decl);
}

THIR *THIRGen::visit_dot_expr(ASTDotExpr *ast) {
  THIR_ALLOC(THIRMemberAccess, thir, ast);
  thir->base = visit_node(ast->base);

  thir->member = ast->member.get_identifier();

  // Tuple.0;
  if (thir->member == "0" || atoi(thir->member.c_str()) != 0) {
    thir->member = "$" + thir->member.str();
  }

  return thir;
}

void THIRGen::make_destructure_for_pattern_match(ASTPatternMatch *ast, THIR *object, Scope *block_scope,
                                                 std::vector<THIR *> &statements, Type *variant_type,
                                                 const InternedString &variant_name) {
  auto variant_member_access = make_member_access(ast->span, object, {{variant_type, variant_name}});

  switch (ast->pattern_tag) {
    case ASTPatternMatch::STRUCT: {
      auto &pattern = ast->struct_pattern;
      for (StructPattern::Part &part : pattern.parts) {
        if (part.resolved_type->is_fixed_sized_array()) {
          // mut doesn't really matter here.
          part.resolved_type = part.resolved_type->get_element_type()->take_pointer_to(true);
        }

        THIR_ALLOC(THIRVariable, var, ast)
        var->name = part.var_name;
        var->type = part.resolved_type;
        var->value = make_member_access(ast->span, variant_member_access, {{part.resolved_type, part.field_name}});
        if (part.semantic == PATTERN_MATCH_PTR_MUT || part.semantic == PATTERN_MATCH_PTR_CONST) {
          var->value = take_address_of(var->value, ast);
        }
        statements.insert(statements.begin(), var);

        auto symbol = block_scope->lookup(part.var_name);
        bind(symbol, var);
      }
    } break;
    case ASTPatternMatch::TUPLE: {
      auto &pattern = ast->tuple_pattern;
      size_t index = 0;
      for (TuplePattern::Part &part : pattern.parts) {
        // We have to take references to fixed arrays as pointers
        if (part.resolved_type->is_fixed_sized_array()) {
          part.resolved_type = part.resolved_type->get_element_type()->take_pointer_to(true);
        }

        THIR_ALLOC(THIRVariable, var, ast)
        var->name = part.var_name;
        var->type = part.resolved_type;
        var->value = make_member_access(ast->span, variant_member_access, {{part.resolved_type, "$" + std::to_string(index++)}});
        if (part.semantic == PATTERN_MATCH_PTR_MUT || part.semantic == PATTERN_MATCH_PTR_CONST) {
          var->value = take_address_of(var->value, ast);
        }
        statements.insert(statements.begin(), var);

        auto symbol = block_scope->lookup(part.var_name);
        bind(symbol, var);
      }
    } break;
    default:
      return;
  }
}

THIR *THIRGen::visit_pattern_match_condition(ASTPatternMatch *ast, THIR *cached_object, const size_t discriminant) {
  THIR_ALLOC(THIRMemberAccess, discriminant_access, ast)
  discriminant_access->base = cached_object;
  discriminant_access->member = CHOICE_TYPE_DISCRIMINANT_KEY;
  discriminant_access->type = u32_type(); // This depends completely on the size of the choice type...
  THIR_ALLOC(THIRBinExpr, thir, ast);
  thir->left = discriminant_access;
  thir->op = TType::EQ;
  thir->right = make_literal(std::to_string(discriminant), ast->span, u32_type(), ASTLiteral::Integer);
  return thir;
}

THIR *THIRGen::visit_pattern_match(ASTPatternMatch *ast, Scope *scope, std::vector<THIR *> &statements) {
  THIR *object = visit_node(ast->object);

  if (!ast->object->resolved_type->is_pointer()) {
    // If the target object isnt a pointer, we always cache it as a pointer so mutations persist
    object = take_address_of(object, ast->object);
  }

  auto cached_object = make_variable(get_temporary_variable(), object, ast->object);

  current_statement_list->push_back(cached_object);
  const Type *choice_type = ast->target_type_path->resolved_type;
  const ChoiceTypeInfo *info = choice_type->info->as<ChoiceTypeInfo>();
  const ASTPath::Segment &segment = ast->target_type_path->segments.back();

  const auto identifier = segment.get_identifier();

  const size_t discriminant = info->get_variant_discriminant(identifier);
  Type *variant_type = info->get_variant_type(identifier);
  const InternedString variant_name = identifier;

  auto condition = visit_pattern_match_condition(ast, cached_object, discriminant);
  make_destructure_for_pattern_match(ast, cached_object, scope, statements, variant_type, variant_name);
  return condition;
}

THIR *THIRGen::visit_bin_expr(ASTBinExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC(THIRBinExpr, binexpr, ast);
    binexpr->left = visit_node(ast->left);
    binexpr->right = visit_node(ast->right);

    // Even for permissible conversions, we should explicitly cast.
    // As well as having special pointer arithmetic nodes.
    if (binexpr->left->type != binexpr->right->type) {
      Type *left = binexpr->left->type, *right = binexpr->right->type;

      // Pointer arithmetic gets a special node since it has completely
      // different lowering semantics from here on out.
      if ((left->is_pointer() || right->is_pointer()) && ast->op != TType::Assign) {
        THIR_ALLOC(THIRPtrBinExpr, ptr_binary, ast);
        ptr_binary->left = binexpr->left;
        ptr_binary->right = binexpr->right;
        ptr_binary->op = ast->op;
        return ptr_binary;
      } else {
        // Cast the right operand since the entire compiler works under the assumption
        // that for permissible mismatched types in a binary expression,
        // the resulting expression, and therefore the right operand, inherits the type
        // of the left operand in an implicit cast.

        // This isn't always great, so we should re-think this.
        THIR_ALLOC(THIRCast, cast, ast);
        cast->operand = binexpr->right;
        cast->type = ast->resolved_type;
        binexpr->right = cast;
      }
    }

    binexpr->op = ast->op;
    return binexpr;
  } else {
    THIR_ALLOC(THIRCall, overload_call, ast);
    overload_call->callee = visit_node(ast->resolved_operator_overload);
    overload_call->arguments.push_back(try_deref_or_take_ptr_to_if_needed(ast->left, visit_node(ast->left),
                                                                          ast->resolved_operator_overload->requires_self_ptr()));
    overload_call->arguments.push_back(visit_node(ast->right));
    return overload_call;
  }
}

THIR *THIRGen::visit_unary_expr(ASTUnaryExpr *ast) {
  if (!ast->is_operator_overload) {
    THIR_ALLOC(THIRUnaryExpr, unary, ast);
    unary->operand = visit_node(ast->operand);
    unary->op = ast->op;

    // Even for permissible conversions, we should explicitly cast.
    // As well as having special pointer arithmetic nodes.
    // Dereference expressions do not use a ptrunary expression.
    // It's only for pointer arithmetic.
    if (unary->operand->type->is_pointer() && ast->op != TType::Mul) {
      THIR_ALLOC(THIRPtrUnaryExpr, ptr_unary, ast);
      ptr_unary->operand = unary->operand;
      ptr_unary->op = ast->op;
      return ptr_unary;
    }

    return unary;
  } else {
    THIR_ALLOC(THIRCall, overload_call, ast);
    overload_call->callee = visit_node(ast->resolved_operator_overload);
    overload_call->callee = visit_node(ast->resolved_operator_overload);
    overload_call->arguments.push_back(try_deref_or_take_ptr_to_if_needed(ast->operand, visit_node(ast->operand),
                                                                          ast->resolved_operator_overload->requires_self_ptr()));

    if (ast->resolved_operator_overload->name == "deref") {
      THIR_ALLOC(THIRUnaryExpr, deref, ast);
      deref->op = TType::Mul;
      deref->operand = overload_call;
      deref->type = ast->resolved_type;
      deref->is_statement = overload_call->is_statement;
      return deref;
    }

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
    auto index = visit_node(ast->index);
    overload_call->callee = visit_node(ast->resolved_operator_overload);

    auto self = try_deref_or_take_ptr_to_if_needed(ast->base, visit_node(ast->base),
                                                   ast->resolved_operator_overload->requires_self_ptr());

    overload_call->arguments.push_back(self);
    overload_call->arguments.push_back(index);

    // We always dereference index operator overLOADs, unless it's slice_index

    const InternedString name = ast->resolved_operator_overload->name;

    if (name != "slice_index" && name != "slice_index_mut") {
      THIR_ALLOC(THIRUnaryExpr, deref, ast);
      deref->type = ast->resolved_type;
      deref->op = TType::Mul;
      deref->operand = overload_call;
      return deref;
    }
    return overload_call;
  }
}

THIR *THIRGen::visit_literal(ASTLiteral *ast) {
  // string literals are aggregate initializers, of 'str' type.
  // so as long it doesnt have the 'c' suffix, and it's not a freestanding/nostdlib env.
  if (ast->tag == ASTLiteral::String && !ast->is_c_string && !compile_command.has_flag("nostdlib") &&
      !compile_command.has_flag("freestanding")) {
    return make_str(ast->value, ast->span);
  }

  THIR_ALLOC(THIRLiteral, literal, ast);
  literal->tag = ast->tag;
  if (ast->is_c_string) {
    literal->is_c_string = true;
  }
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
  THIR_ALLOC(THIRAggregateInitializer, dynof, ast);

  dynof->key_values.push_back({"instance", visit_node(ast->object)});

  auto dyn_info = ast->resolved_type->info->as<DynTypeInfo>();
  auto object_type_nonptr = ast->object->resolved_type->get_element_type();
  auto scope = object_type_nonptr->info->scope;

  const auto get_function_pointer = [&](ASTFunctionDeclaration *func) -> THIR * {
    auto function = visit_node(func);
    auto addr = take_address_of(function, func);

    const auto type = function->type;
    const auto info = type->info->as<FunctionTypeInfo>();

    FunctionTypeInfo new_info;
    new_info.params_len = info->params_len;
    memcpy(new_info.parameter_types, info->parameter_types, sizeof(void *) * info->params_len);
    new_info.return_type = info->return_type;
    new_info.parameter_types[0] = void_type()->take_pointer_to(true);

    auto new_type = global_find_function_type_id(new_info, {{TYPE_EXT_POINTER_CONST}});

    return make_cast(addr, new_type);
  };

  for (auto &[name, method_type] : dyn_info->methods) {
    auto symbol = scope->local_lookup(name);
    dynof->key_values.push_back({
        name,
        get_function_pointer(symbol->function.declaration),
    });
  }

  return dynof;
}

THIR *THIRGen::option_some(THIR *value, Type *interior_type) {
  ASTDeclaration *option = find_generic_instance(g_Option_type->generic_instantiations, {interior_type});
  auto type = option->resolved_type;
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, init);
  init->type = type;
  init->is_statement = false;
  init->key_values = {{
                          CHOICE_TYPE_DISCRIMINANT_KEY,
                          make_literal(OPTION_SOME_DISCRIMINANT_VALUE, {}, u32_type(), ASTLiteral::Integer),
                      },
                      {
                          "Some",
                          value,
                      }};
  return init;
}

THIR *THIRGen::option_none(Type *interior_type) {
  ASTDeclaration *option = find_generic_instance(g_Option_type->generic_instantiations, {interior_type});
  auto type = option->resolved_type;
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, init);
  init->type = type;
  init->is_statement = false;
  init->key_values = {{
      CHOICE_TYPE_DISCRIMINANT_KEY,
      make_literal(OPTION_NONE_DISCRIMINANT_VALUE, {}, u32_type(), ASTLiteral::Integer),
  }};
  return init;
}

THIR *THIRGen::visit_range(ASTRange *ast) {
  THIR_ALLOC(THIRAggregateInitializer, thir, ast);

  auto element_type = ast->resolved_type->generic_args[0];

  if (ast->left) {
    thir->key_values.push_back({RANGE_TYPE_BEGIN_KEY, option_some(visit_node(ast->left), element_type)});
  } else {
    thir->key_values.push_back({RANGE_TYPE_BEGIN_KEY, option_none(element_type)});
  }
  if (ast->right) {
    thir->key_values.push_back({RANGE_TYPE_END_KEY, option_some(visit_node(ast->right), element_type)});
  } else {
    thir->key_values.push_back({RANGE_TYPE_END_KEY, option_none(element_type)});
  }
  thir->key_values.push_back(
      {RANGE_TYPE_IS_INCLUSIVE_KEY, make_literal(ast->inclusive ? "true" : "false", {}, bool_type(), ASTLiteral::Bool)});
  return thir;
}

THIR *THIRGen::initialize(const Span &span, Type *type, std::vector<std::pair<InternedString, ASTExpr *>> key_values) {
  const auto info = type->info;

  if (type->is_pointer() && key_values.empty()) {
    THIR_ALLOC_NO_SRC_RANGE(THIRLiteral, literal);
    literal->tag = ASTLiteral::Null;
    literal->value = "nullptr";
    literal->span = span;
    literal->is_statement = false;
    return literal;
  }

  // TODO: might need more ignored things here
  if (info->members.empty() || type->is_kind(TYPE_CHOICE) || type->is_fixed_sized_array() || type->is_kind(TYPE_ENUM)) {
    THIR_ALLOC_NO_SRC_RANGE(THIREmptyInitializer, thir);
    thir->span = span;
    thir->type = type;
    return thir;
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->span = span;
  thir->type = type;

  bool type_is_union = type->is_kind(TYPE_STRUCT) && type->info->as<StructTypeInfo>()->is_union;

  for (const auto &member : info->members) {
    ASTExpr *initializer = nullptr;
    for (const auto &[key, value] : key_values) {
      if (key == member.name) {
        initializer = value;
        break;
      }
    }

    bool is_extensionless_non_union_struct = false;
    bool is_anonymous_subtype = false;
    if (member.type->is_kind(TYPE_STRUCT)) {
      const StructTypeInfo *info = member.type->info->as<StructTypeInfo>();
      is_extensionless_non_union_struct = member.type->has_no_extensions() && !info->is_union;
      is_anonymous_subtype = member.type->has_no_extensions() && info->is_anonymous;
    }

    if (is_anonymous_subtype) {
      const TypeInfo *info = member.type->info;
      std::vector<std::pair<InternedString, ASTExpr *>> subinitializer_elements;

      // Collect keys for the anonymous subtype
      for (const auto &[key, value] : key_values) {
        if (info->find_member(key)) {
          subinitializer_elements.push_back({key, value});
        }
      }

      if (subinitializer_elements.empty()) {
        continue;
      }

      for (const auto &[subkey, _] : subinitializer_elements) {
        std::erase_if(key_values, [&](const auto &kv) { return kv.first == subkey; });
      }

      auto *initialization = (THIRAggregateInitializer *)initialize(span, member.type, subinitializer_elements);

      // get the keys from that initializer and drop them in here.
      for (auto &[key, value] : initialization->key_values) {
        thir->key_values.push_back({key, value});
      }

      continue;
    }

    if (initializer) {
      thir->key_values.push_back({member.name, visit_node(initializer)});
    } else if (member.default_value) {
      thir->key_values.push_back({member.name, visit_node(member.default_value.get())});
    } else if (is_extensionless_non_union_struct) {
      thir->key_values.push_back({member.name, initialize(span, member.type, {})});
    } else if (!type_is_union) {
      THIR_ALLOC_NO_SRC_RANGE(THIREmptyInitializer, empty_init);
      empty_init->span = span;
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
      return initialize(ast->span, type, {});
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      if (type->kind == TYPE_STRUCT) {
        return initialize(ast->span, type, ast->key_values);
      } else if (type->kind == TYPE_CHOICE) {
        // Choice variant instantiation
        const auto path = ast->target_type.get()->normal.path;
        const auto info = ast->resolved_type->info->as<ChoiceTypeInfo>();
        const auto thir = get_choice_type_instantiation_boilerplate(path, this);
        const auto variant_name = path->segments.back().get_identifier();
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
      // InitList is basically just a fat pointered VLA, so we use an aggregate around the VLA.
      if (type->basename.str_ptr->starts_with("InitList$")) {
        const size_t length = ast->values.size();

        /// typeof InitList!<T>.data;
        TypeExtension extension = {.type = TYPE_EXT_ARRAY, length};
        ast->resolved_type = global_find_type_id(type->generic_args[0], TypeExtensions{{extension}});

        auto collection = (THIRCollectionInitializer *)visit_initializer_list(ast);
        collection->is_variable_length_array = true;

        THIR_ALLOC(THIRAggregateInitializer, thir, ast)
        thir->type = type;
        thir->key_values.push_back({"data", collection});
        thir->key_values.push_back({"length", make_literal(std::to_string(length), ast->span, u64_type(), ASTLiteral::Integer)});
        return thir;
      }

      THIR_ALLOC(THIRCollectionInitializer, thir, ast)
      for (const auto &value : ast->values) {
        thir->values.push_back(visit_node(value));
      }

      thir->is_variable_length_array = ast->resolved_type->is_pointer();

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
  // TODO: why is thir->operand->type null here sometimes?
  if (thir->operand->type->is_kind(TYPE_STRUCT) && thir->operand->type->info->as<StructTypeInfo>()->is_structural) {
    auto addr = take_address_of(thir->operand, ast);

    THIR_ALLOC_NO_SRC_RANGE(THIRCast, ptr_cast);
    ptr_cast->type = thir->type->take_pointer_to(true);
    ptr_cast->operand = addr;

    THIR_ALLOC(THIRUnaryExpr, deref, ast);
    deref->op = TType::Mul;
    deref->operand = ptr_cast;
    deref->type = thir->type;
    return deref;
  }

  return thir;
}

THIR *THIRGen::visit_lambda(ASTLambda *ast) {
  auto symbol = ctx.scope->lookup(ast->unique_identifier);
  if (auto thir = symbol_map[symbol]) {
    return thir;
  }

  THIR_ALLOC(THIRFunction, thir, ast);
  thir->type = ast->resolved_type->get_element_type();

  bind(symbol, thir);
  bind(ast, thir);

  thir->name = ast->unique_identifier;

  for (const auto &ast_param : ast->params->params) {
    THIRParameter thir_param = {
        .name = ast_param->normal.name,
    };

    THIR_ALLOC(THIRVariable, var, ast_param);
    var->name = ast_param->normal.name;
    var->type = ast_param->resolved_type;
    var->is_global = false;
    
    if (ast_param->normal.default_value) {
      thir_param.default_value = visit_node(ast_param->normal.default_value.get());
      var->value = thir_param.default_value;
    }
    
    auto symbol = ast->block->scope->local_lookup(ast_param->normal.name);
    
    bind(symbol, var);
    bind(ast_param, var);
    
    thir_param.associated_variable = var;
    thir->parameters.push_back(thir_param);
  }

  auto saved_defer_stack = std::move(defer_stack);
  defer_stack.clear();

  enter_defer_boundary(DeferBoundary::FUNCTION);

  thir->block = (THIRBlock *)visit_block(ast->block);

  auto func_defers = collect_defers_up_to(DeferBoundary::FUNCTION);
  for (auto d : func_defers) {
    thir->block->statements.push_back(d);
  }

  exit_defer_boundary();
  defer_stack = std::move(saved_defer_stack);

  return take_address_of(thir, ast);
}

THIR *THIRGen::visit_block(ASTBlock *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRBlock, thir, ast);

  enter_defer_boundary(DeferBoundary::BLOCK);

  {
    ENTER_STMT_VEC(thir->statements);
    for (const auto &ast_statement : ast->statements) {
      if (auto thir_statement = visit_node(ast_statement)) {
        thir->statements.push_back(thir_statement);
      }
    }
  }

  auto block_defers = collect_defers_up_to(DeferBoundary::BLOCK);
  exit_defer_boundary();

  for (auto d : block_defers) {
    thir->statements.push_back(d);
  }

  return thir;
}

static inline void convert_function_flags(THIRFunction *reciever, ASTFunctionDeclaration *function) {
  reciever->is_inline = function->is_inline;
  reciever->is_varargs = function->is_varargs;
  reciever->is_extern = function->is_extern;
  reciever->is_entry = function->is_entry;
  reciever->is_exported = function->is_exported;
  reciever->is_test = function->is_test;
  reciever->is_macro = function->is_macro;
}

void THIRGen::convert_function_attributes(THIRFunction *reciever, const std::vector<Attribute> &attrs) {
  for (const auto &attr : attrs) {
    switch (attr.tag) {
      case ATTRIBUTE_CONSTRUCTOR:
        if (attr.arguments.size() != 1) {
          throw_error(
              "@[constructor($degree)] attribute expects exactly one argument, true or false, which determines "
              "whether this will run before or after global initializers. 'false' is not reccomended, as accessing global "
              "data is undefined behaviour from within them, since it runs at an indeterminate time, possibly before "
              "global initializers. 'true' is absolutely reccomended, as it runs after global initializers with certainty.",
              reciever->span);
        }
        // see THIRFunction::constructor_index for info on what the value means.
        reciever->constructor_index = 1 + (((ASTLiteral *)attr.arguments[0])->value == "false");
        constructors.push_back(reciever);
        if (reciever->parameters.size()) {
          throw_error("@[constructor] functions cannot take parameters, as they're called by the runtime automatically.",
                      reciever->span);
        }
        break;
      case ATTRIBUTE_DEPRECATED:
        reciever->deprecated_attr = attr;
        reciever->deprecated = true;
        break;
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

void THIRGen::convert_parameters(ASTFunctionDeclaration *&ast, THIRFunction *&thir) {
  for (const auto &param : ast->params->params) {
    THIR_ALLOC(THIRVariable, var, param);
    if (param->tag == ASTParamDecl::Normal) {
      var->name = param->normal.name;
      var->type = param->normal.type->resolved_type;
      if (param->normal.default_value) {
        auto vec = std::vector<THIR *>();
        auto previous = current_expression_list;
        current_expression_list = &vec;
        auto result = visit_node(param->normal.default_value.get());
        if (vec.size() || result == THIRNoop::shared()) {
          var->value = vec[0];
        } else {
          var->value = result;
        }
        current_expression_list = previous;
      } else {
        var->value = nullptr;
      }
    } else {
      var->name = "self";
      var->type = param->resolved_type;
    }

    if (!ast->is_forward_declared) {
      auto param_sym = ctx.scope->local_lookup(var->name);

      bind(param_sym, var);
      bind(param, var);
    }

    thir->parameters.push_back(THIRParameter{
        .mutability = param->mutability,
        .name = var->name,
        .default_value = var->value,
        .associated_variable = var,
    });
  }
}

void THIRGen::mangle_function_name_for_thir(ASTFunctionDeclaration *&ast, THIRFunction *&thir) {
  if (thir->name == "main" || thir->is_entry) {
    if (entry_point && entry_point->get_node_type() != THIRNodeType::Program) {
      throw_error(std::format("multiple functions with the @[entry] attribute were found, or multiple 'main()' "
                              "functions were found. previous definition: {}",
                              entry_point->span.to_string()),
                  ast->span);
    }
    entry_point = thir;
    thir->is_entry = true;
    thir->name = USER_MAIN_FUNCTION_NAME;
  } else if (thir->is_exported || thir->is_extern || thir->is_no_mangle ||
             ast->is_forward_declared /* !TODO: REMOVE THIS LAST CASE __ IT IS WRONG */) {
    thir->name = ast->name;
  } else {
    std::string name;
    if (ast->declaring_type) {
      name = ast->declaring_type->info->scope->full_name() + "$" + ast->name.str();
    } else {
      name = ast->scope->full_name();
    }
    if (ast->generic_arguments.size()) {
      name += "$" + mangled_type_args(ast->generic_arguments);
    }
    thir->name = name;
  }
}

THIR *THIRGen::visit_function_declaration(ASTFunctionDeclaration *ast) {
  if (ast_map.contains(ast)) {
    return ast_map[ast];
  }

  const static bool is_testing = compile_command.has_flag("test");

  if (ast->is_macro && !is_making_call) {
    return nullptr;
  }

  if (ast->is_test && !is_testing) {
    // TODO: is this acceptable?
    return nullptr;
  }

  if (ast->generic_parameters.size()) {
    return nullptr;
  }

  if (auto thir = get_thir(ast)) {
    return thir;
  }

  THIR_ALLOC(THIRFunction, thir, ast);

  Symbol *symbol = nullptr;
  if (ast->declaring_type) {
    symbol = ast->declaring_type->info->scope->local_lookup(ast->name);
  } else {
    symbol = ast->declaring_scope->local_lookup(ast->name);
  }

  if (!symbol) {
    throw_error("Unable to find symbol for function", ast->span);
  }

  bind(ast, thir);

  ENTER_SCOPE(ast->scope);
  convert_parameters(ast, thir);
  convert_function_flags(thir, ast);
  convert_function_attributes(thir, ast->attributes);

  mangle_function_name_for_thir(ast, thir);

  // Temporary override this so we don't leak it into callers from call sites.
  auto old_return_register = return_override_register;
  return_override_register = nullptr;
  Defer _([&] { return_override_register = old_return_register; });

  if (ast->block) {
    // SUPER naive macro expansion, this will explode with C errors if you misuse it
    // I'm really only adding expand blocks to aide in the development of the self hosted compiler.
    if (ast->is_macro) {
      ENTER_SCOPE(ast->block.get()->scope);
      for (const auto &stmt : ast->block.get()->statements) {
        if (stmt->get_node_type() == AST_NODE_DEFER && !stmt->is_insert_node) {
          continue;
        }
        auto result = visit_node(stmt);

        if (stmt->is_insert_node && stmt->get_node_type() == AST_NODE_EXPR_STATEMENT && current_expression_list) {
          current_expression_list->push_back(result);
        } else if (stmt->is_insert_node) {
          current_statement_list->push_back(result);
        }
      }
      return THIRNoop::shared();
    }

    auto saved_defer_stack = std::move(defer_stack);
    defer_stack.clear();

    enter_defer_boundary(DeferBoundary::FUNCTION);

    thir->block = (THIRBlock *)visit_block(ast->block.get());

    auto func_defers = collect_defers_up_to(DeferBoundary::FUNCTION);
    for (auto d : func_defers) {
      thir->block->statements.push_back(d);
    }

    exit_defer_boundary();
    defer_stack = std::move(saved_defer_stack);
  }

  // Either the tests have to be in the root scope, i.e declared or included via a main.ela file or a test.ela file,
  // or this flag has to be present
  auto should_be_tested = ast->declaring_scope->name.str().empty() || compile_command.has_flag("run-every-test");

  if (is_testing && ast->is_test && should_be_tested) {
    test_functions.push_back(thir);
  }

  return thir;
}

THIR *THIRGen::visit_variable(ASTVariable *ast) {
  if (ast_map.contains(ast)) {
    return ast_map[ast];
  }

  THIR_ALLOC(THIRVariable, thir, ast);
  check_deprecated(thir, ast->attributes);
  thir->is_uninitialized = ast->is_uninitialized;
  thir->is_global = !ast->is_local;
  thir->is_static = ast->is_static;
  thir->is_constexpr = ast->is_constexpr;
  thir->is_extern = ast->is_extern;

  auto symbol = ast->declaring_scope->local_lookup(ast->name);

  thir->symbol = symbol;

  bind(ast, thir);
  bind(symbol, thir);

  if (!ast->is_local && !ast->is_extern) {
    auto scope_name = ast->declaring_scope->full_name();
    if (scope_name.empty()) {
      thir->name = ast->name.str();
    } else {
      thir->name = scope_name + ast->name.str();
    }
  } else {
    thir->name = ast->name;
  }

  if (thir->is_global && !ast->is_constexpr && !ast->is_extern) {
    // global variables don't get directly assigned, we will always use a static global initializer.
    // TODO: make some kind of analyzer that will actually figure this out for us, simple assignments dont
    // need to work like this for compile time constant friendly values.
    make_global_initializer(ast->type->resolved_type, thir, ast->value);
  } else {
    if (ast->value) {
      thir->value = visit_node(ast->value.get());
    } else if (!thir->is_uninitialized) {
      thir->value = initialize(thir->span, ast->type->resolved_type, {});
    }
  }

  thir->type = ast->type->resolved_type;
  return thir;
}

void THIRGen::extract_thir_values_for_type_members(Type *type) {
  for (auto &member : type->info->members) {
    if (member.default_value) {
      auto value = member.default_value.get();
      member.thir_value = visit_node(value);

      // !TERRIBLE HACK
      // For some reason enumeration values like -1 would randomly cast to (u64)
      if (value->resolved_type != type->info->as<EnumTypeInfo>()->underlying_type) {
        member.thir_value = make_cast(member.thir_value.get(), type->info->as<EnumTypeInfo>()->underlying_type);
      }
    }
  }
}

THIR *THIRGen::visit_struct_declaration(ASTStructDeclaration *ast) {
  if (ast->generic_parameters.size()) {
    return nullptr;
  }

  // We just ignore these i think?
  if (ast->is_forward_declared) {
    return nullptr;
  }

  if (ast_map.contains(ast)) {
    return ast_map[ast];
  }

  THIR_ALLOC(THIRType, thir, ast);
  check_deprecated(thir, ast->attributes);

  bind(ast, thir);

  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_choice_declaration(ASTChoiceDeclaration *ast) {
  if (ast->generic_parameters.size()) {
    return nullptr;
  }

  if (ast_map.contains(ast)) {
    return ast_map[ast];
  }

  // We just ignore these i think?
  if (ast->is_forward_declared) {
    return nullptr;
  }

  THIR_ALLOC(THIRType, thir, ast);
  check_deprecated(thir, ast->attributes);
  bind(ast, thir);

  extract_thir_values_for_type_members(thir->type);
  return thir;
}

THIR *THIRGen::visit_enum_declaration(ASTEnumDeclaration *ast) {
  if (ast_map.contains(ast)) {
    return ast_map[ast];
  }

  THIR_ALLOC(THIRType, thir, ast);
  check_deprecated(thir, ast->attributes);
  bind(ast, thir);

  extract_thir_values_for_type_members(thir->type);
  for (const auto &member : ast->resolved_type->info->members) {
    THIR_ALLOC_NO_SRC_RANGE(THIRVariable, var)

    // TODO: instead of even doing this, we should just lower
    // these directly to integer literals.

    // Also, with the new MIR api, we don't need any THIRType at all.
    // would save a lot by refactoring this to be even simpler.
    thir->enum_members.push_back(var);
    var->is_from_enum_declaration = true;


    var->span = ast->span;
    var->enum_type = thir;
    var->is_global = false;
    var->is_statement = true;
    var->name = ast->resolved_type->basename.str() + '$' + member.name.str();
    var->value = member.thir_value.get();
    var->type = member.type;
    auto symbol = ast->resolved_type->info->scope->local_lookup(member.name);
    bind(symbol, var);
  }

  return thir;
}

THIR *THIRGen::visit_switch(ASTSwitch *ast) {
  // TODO: maybe we want to throw an error for this, instead of just optimizing it out.
  if (ast->branches.empty() && !ast->default_branch) {
    return nullptr;
  }

  // this is totally redundant so, whatever, just emit it as if it were a block.
  if (ast->branches.empty() && ast->default_branch) {
    return visit_node(ast->default_branch.get());
  }

  // Build the switch as a THIRIf chain. Encapsulate construction so we can reuse for
  // both statement and expression forms (expression needs return override handling).
  const auto build_if_chain = [&]() -> THIRIf * {
    auto cached_expr = make_variable(get_temporary_variable(), visit_node(ast->expression), ast->expression);

    current_statement_list->push_back(cached_expr);

    // when applicable:
    // gets cleared for every branch, and populated for every branch
    std::vector<THIR *> extra_statements_generated_by_pattern_match;

    const auto get_condition_comparator = [&](size_t index) -> THIR * {
      auto operator_overload_ty = find_operator_overload(CONST, cached_expr->type, TType::EQ, OPERATION_BINARY);
      auto left = cached_expr;

      auto &branch = ast->branches[index];
      if (!operator_overload_ty) {  // normal equality comparison.
        THIR *condition;

        if (branch.expression->get_node_type() == AST_NODE_PATTERN_MATCH) {
          // clear -> populate extra statements.
          extra_statements_generated_by_pattern_match.clear();
          condition = visit_pattern_match((ASTPatternMatch *)branch.expression, branch.block->scope,
                                          extra_statements_generated_by_pattern_match);
        } else {
          THIR_ALLOC(THIRBinExpr, binexpr, ast);
          binexpr->left = left;
          binexpr->right = visit_node(ast->branches[index].expression);
          binexpr->op = TType::EQ;
          binexpr->type = bool_type();
          condition = binexpr;
        }

        return condition;
      } else {  // call an operator overload
        THIR_ALLOC(THIRCall, overload_call, ast);
        auto scope = left->type->info->scope;
        auto symbol = scope->local_lookup(get_operator_overload_name(TType::EQ, OPERATION_BINARY));
        overload_call->callee = visit_node(symbol->function.declaration);
        overload_call->arguments.push_back(
            try_deref_or_take_ptr_to_if_needed(branch.expression, left, symbol->function.declaration->requires_self_ptr()));
        overload_call->arguments.push_back(visit_node(ast->branches[index].expression));
        return overload_call;
      }
    };

    THIR_ALLOC(THIRIf, first_case, ast)
    first_case->condition = get_condition_comparator(0);
    first_case->block = (THIRBlock *)visit_node(ast->branches[0].block);
    for (auto &stmt : extra_statements_generated_by_pattern_match) {
      first_case->block->statements.insert(first_case->block->statements.begin(), stmt);
    }
    first_case->is_statement = true;

    THIRIf *the_if = first_case;
    for (size_t i = 1; i < ast->branches.size(); ++i) {
      const auto &ast_branch = ast->branches[i];
      THIR_ALLOC(THIRIf, thir_branch, ast)
      thir_branch->condition = get_condition_comparator(i);
      thir_branch->block = (THIRBlock *)visit_node(ast_branch.block);
      for (auto &stmt : extra_statements_generated_by_pattern_match) {
        thir_branch->block->statements.insert(thir_branch->block->statements.begin(), stmt);
      }
      thir_branch->is_statement = true;
      the_if->_else = thir_branch;
      the_if = thir_branch;
    }

    if (ast->default_branch) {
      the_if->_else = (THIRBlock *)visit_node(ast->default_branch.get());
    }

    return first_case;
  };

  if (!ast->is_statement) {
    THIR_ALLOC(THIRExprBlock, block, ast);
    ENTER_RETURN_OVERRIDE(ast, block->statements);
    block->return_register = return_override_register.get();
    block->statements.push_back(build_if_chain());
    return block;
  }

  return build_if_chain();
}

THIR *THIRGen::visit_program(ASTProgram *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRProgram, thir, ast);

  program = thir;
  entry_point = thir;  // We default to using the entire program as an entry point (for emitting), but if we get a
                       // main/@[entry] function, this gets replaced.

  ENTER_STMT_VEC(thir->statements);

  for (const auto &ast_statement : ast->statements) {
    if (auto thir_statement = visit_node(ast_statement)) {
      thir->statements.push_back(thir_statement);
    }
  }

  // set the initializer for _all_tests before we finish writing this out (main is compiled after this already has been
  // written out to a string)
  setup__all_tests();

  return thir;
}

THIR *THIRGen::take_address_of(THIR *operand, ASTNode *ast) {
  THIR_ALLOC_NO_SRC_RANGE(THIRUnaryExpr, thir);
  thir->is_statement = false;
  if (ast) {
    thir->span = ast->span;
  }
  thir->op = TType::And;
  thir->type = operand->type->take_pointer_to(true);
  operand->is_statement = false;
  thir->operand = (THIR *)operand;
  return thir;
}

THIRVariable *THIRGen::make_variable(const InternedString &name, THIR *value, ASTNode *ast, bool is_global) {
  THIR_ALLOC_NO_SRC_RANGE(THIRVariable, thir)
  if (ast) {
    thir->span = ast->span;
  }
  thir->is_uninitialized = false;
  thir->name = name;
  thir->is_global = is_global;
  thir->is_statement = true;
  thir->type = value->type;
  thir->value = value;
  return thir;
}

THIR *THIRGen::visit_for(ASTFor *ast) {
  THIR_ALLOC(THIRFor, thir, ast);

  const InternedString result_key = get_temporary_variable();
  const InternedString cached_key = get_temporary_variable();

  ENTER_SCOPE(ast->block->scope);

  // Cache the iterable/expression (evaluate once)
  THIR *iterable_value = visit_node(ast->right);
  THIR *iterable_var = make_variable(cached_key, iterable_value, ast->right);

  current_statement_list->push_back(iterable_var);

  // Get the iterator (call iter() or use as iterator directly)
  THIR *iterator_var = nullptr;
  if (iterable_var->type->implements(iterable_trait())) {
    auto iter_symbol = iterable_var->type->info->scope->local_lookup("iter");
    bool expects_ptr = iter_symbol && iter_symbol->function.declaration->params->params[0]->self.is_pointer;
    THIR *iter_arg = (expects_ptr && !iterable_var->type->is_pointer()) ? take_address_of(iterable_var, ast) : iterable_var;

    THIR_ALLOC(THIRCall, iter_call, ast);
    iter_call->callee = visit_node(iter_symbol->function.declaration);
    iter_call->arguments.push_back(iter_arg);

    const auto *info = iter_call->callee->type->info->as<FunctionTypeInfo>();
    iter_call->type = info->return_type;

    iterator_var = make_variable(get_temporary_variable(), iter_call, ast);
    current_statement_list->push_back(iterator_var);
  } else if (iterable_var->type->implements(iterator_trait())) {
    iterator_var = iterable_var;
  }

  // Call next() on the iterator
  auto next_symbol = iterator_var->type->info->scope->local_lookup("next");

  auto next_arg =
      try_deref_or_take_ptr_to_if_needed(ast->right, iterator_var, next_symbol->function.declaration->requires_self_ptr());

  THIR_ALLOC(THIRCall, next_call, ast);
  next_call->callee = visit_node(next_symbol->function.declaration);

  const auto *next_info = next_call->callee->type->info->as<FunctionTypeInfo>();
  next_call->type = next_info->return_type;
  next_call->arguments.push_back(next_arg);

  // Assign next_call to a loop variable
  THIR_ALLOC(THIRVariable, next_var, ast);
  next_var->is_global = false;
  next_var->name = result_key;
  next_var->value = next_call;
  next_var->type = next_call->type;
  thir->initialization = next_var;

  // Build the loop condition: $next.index == OPTION_SOME_DISCRIMINANT_VALUE
  THIR_ALLOC(THIRBinExpr, condition, ast);
  THIR_ALLOC(THIRMemberAccess, member_access, ast);
  member_access->base = next_var;
  member_access->member = CHOICE_TYPE_DISCRIMINANT_KEY;
  member_access->type = u32_type();

  THIR *discriminant_literal = make_literal(OPTION_SOME_DISCRIMINANT_VALUE, ast->span, u32_type(), ASTLiteral::Integer);

  condition->left = member_access;
  condition->op = TType::EQ;
  condition->right = discriminant_literal;
  condition->type = bool_type();
  thir->condition = condition;

  // Increment: $next = next($iterator)
  THIR_ALLOC(THIRBinExpr, increment, ast);
  increment->type = void_type();
  increment->left = next_var;
  increment->op = TType::Assign;
  increment->right = next_var->value;
  thir->increment = increment;

  // Unwrap: $next.Some.$0
  // ugly ahh code.
  const auto some_type = next_var->type->info->as<ChoiceTypeInfo>()->get_variant_type("Some");
  const auto value_type = some_type->info->members[0].type;

  auto unwrapped_some = make_member_access(ast->span, next_var, {{some_type, "Some"}, {value_type, "$0"}});

  if (ast->left_tag == ASTFor::DESTRUCTURE) {
    std::vector<THIR *> statements;

    THIR *ptr_to_some = unwrapped_some;

    if (!ptr_to_some->type->is_pointer()) {
      ptr_to_some = take_address_of(ptr_to_some, ast);
    }

    // we take the address of when caching the result because 'for &x, &y in ...' needs to mutate the original.
    THIRVariable *cached_base = make_variable(get_temporary_variable(), ptr_to_some, ast);
    statements.push_back(cached_base);
    const auto type = value_type;
    auto members_iter = type->info->members.begin();

    for (const DestructureElement &element : ast->left.destructure) {
      const auto member = *members_iter;
      const auto member_access = make_member_access(ast->span, cached_base, {{element.type, member.name}});
      const auto variable = make_variable(element.identifier, member_access, ast);
      members_iter++;

      if (is_pointer_semantic(element.semantic)) {
        variable->value = take_address_of(member_access, ast);
      }

      auto symbol = ctx.scope->local_lookup(element.identifier);
      bind(symbol, variable);
      statements.push_back(variable);
    }

    enter_defer_boundary(DeferBoundary::LOOP);
    thir->block = visit_node(ast->block);
    THIRBlock *block = (THIRBlock *)thir->block;

    auto loop_defers = collect_defers_up_to(DeferBoundary::LOOP);
    exit_defer_boundary();
    for (auto d : loop_defers) {
      block->statements.push_back(d);
    }

    block->statements.insert(block->statements.begin(), statements.begin(), statements.end());
  } else {
    const auto identifier_var = make_variable(ast->left.identifier, unwrapped_some, ast);
    const auto identifier_symbol = ctx.scope->lookup(ast->left.identifier);

    bind(identifier_symbol, identifier_var);

    enter_defer_boundary(DeferBoundary::LOOP);
    THIRBlock *block = (THIRBlock *)visit_node(ast->block);
    thir->block = block;
    auto loop_defers = collect_defers_up_to(DeferBoundary::LOOP);
    exit_defer_boundary();
    for (auto d : loop_defers) {
      block->statements.push_back(d);
    }
    block->statements.insert(block->statements.begin(), identifier_var);
  }

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
  } else {
    thir->condition = make_literal("true", ast->span, bool_type(), ASTLiteral::Bool);
  }

  enter_defer_boundary(DeferBoundary::LOOP);
  thir->block = (THIRBlock *)visit_node(ast->block);
  auto loop_defers = collect_defers_up_to(DeferBoundary::LOOP);
  exit_defer_boundary();
  for (auto d : loop_defers) {
    thir->block->statements.push_back(d);
  }

  return thir;
}

THIR *THIRGen::visit_defer(ASTDefer *ast) {
  std::vector<THIR *> tmp_stmts;

  {
    ENTER_STMT_VEC(tmp_stmts);
    if (auto s = visit_node(ast->statement)) {
      tmp_stmts.push_back(s);
    }
  }

  if (defer_stack.empty()) {
    throw_error("got a defer statement where we were not expecting it. they're only valid within functions and blocks",
                ast->span);
  }

  // append the collected statements into the current frame's flat defer list
  auto &vec = defer_stack.back().defers;
  vec.insert(vec.end(), std::make_move_iterator(tmp_stmts.begin()), std::make_move_iterator(tmp_stmts.end()));

  return THIRNoop::shared();
}

void THIRGen::visit_destructure(ASTDestructure *ast) {
  const auto type = ast->right->resolved_type;
  auto members_iter = type->info->members.begin();

  THIR *base = visit_node(ast->right);
  const auto cached_base = make_variable(get_temporary_variable(), take_address_of(base, ast), ast->right);
  current_statement_list->push_back(cached_base);

  for (const DestructureElement &element : ast->elements) {
    if (ast->op == TType::Assign) {
      THIR_ALLOC(THIRBinExpr, assign, ast);
      assign->op = TType::Assign;
      assign->is_statement = true;

      auto symbol = ctx.scope->local_lookup(element.identifier);
      auto declaration = symbol_map[symbol];
      assign->left = declaration;

      THIR_ALLOC(THIRMemberAccess, member_access, ast);
      assign->right = member_access;

      const auto member = *members_iter;
      members_iter++;

      member_access->member = member.name;
      member_access->base = cached_base;
      member_access->type = element.type;

      if (is_pointer_semantic(element.semantic)) {
        assign->right = take_address_of(member_access, ast);
      }

      current_statement_list->push_back(assign);
    } else {
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

      if (is_pointer_semantic(element.semantic)) {
        var->value = take_address_of(member_access, ast);
      }

      auto symbol = ctx.scope->local_lookup(element.identifier);
      bind(symbol, var);
      current_statement_list->push_back(var);
    }
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

void THIRGen::visit_import(ASTImport *_) {
  // ! I don't think this does anything anymore, when an import creates stuff via a file, it's handled in an ASTModule
  // ENTER_SCOPE(ast->scope);
  // for (const auto &ast_stmt : ast->statements) {
  //   visit_node(ast_stmt);
  // }
}

void THIRGen::visit_module(ASTModule *ast) {
  ENTER_SCOPE(ast->scope);
  for (const auto &ast_stmt : ast->statements) {
    if (auto thir_stmt = visit_node(ast_stmt)) {
      current_statement_list->push_back(thir_stmt);
    }
  }
}

void THIRGen::visit_where_branch(const WhereBranch *branch) {
  if (branch->where_stmt.is_not_null()) {
    visit_node(branch->where_stmt.get());
  } else {
    auto block = branch->block.get();
    ENTER_SCOPE(block->scope);
    for (const auto &ast_stmt : block->statements) {
      if (auto thir_stmt = visit_node(ast_stmt)) {
        current_statement_list->push_back(thir_stmt);
      }
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
  } else if (ast->branch) {
    // the 'else' cases
    visit_where_branch(ast->branch.get());
  }
}

THIR *THIRGen::make_str(const InternedString &value, const Span &src_range) {
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  static Type *str_type = ctx.scope->find_type_id("str", {});
  thir->span = src_range;
  thir->type = str_type;
  thir->key_values.push_back({"data", make_literal(value, src_range, u8_ptr_type(), ASTLiteral::String)});
  thir->key_values.push_back({"length", make_literal(std::to_string(calculate_strings_actual_length(value.str())), src_range,
                                                     u64_type(), ASTLiteral::Integer)});
  return thir;
}

THIR *THIRGen::make_literal(const InternedString &value, const Span &src_range, Type *type, ASTLiteral::Tag tag) {
  THIR_ALLOC_NO_SRC_RANGE(THIRLiteral, thir);
  thir->tag = tag;
  thir->span = src_range;
  thir->value = value;
  thir->type = type;
  return thir;
}

THIR *THIRGen::make_member_access(const Span &range, THIR *base, std::deque<std::pair<Type *, InternedString>> parts) {
  THIR_ALLOC_NO_SRC_RANGE(THIRMemberAccess, thir)
  thir->span = range;
  thir->base = base;
  auto [type, member] = parts.front();
  thir->member = member;
  if (!type) {
    throw_error("THIRGen tried to make member access but type was null", range);
  }

  thir->type = type;
  parts.pop_front();
  THIRMemberAccess *last_part = thir;
  while (!parts.empty()) {
    THIR_ALLOC_NO_SRC_RANGE(THIRMemberAccess, member_access);
    auto [type, member] = parts.front();
    if (!type) {
      throw_error("THIRGen tried to make member access but type was null", range);
    }
    parts.pop_front();
    member_access->base = last_part;
    member_access->member = member;
    member_access->type = type;
    last_part = member_access;
  }
  return last_part;
}

THIR *THIRGen::get_method_struct(const std::string &name, Type *type) {
  static Type *method_struct_type = ctx.scope->find_type_id("Method", {});
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = method_struct_type;
  thir->key_values.push_back({
      "name",
      make_str(name, {}),
  });

  if (type->kind != TYPE_TRAIT) {
    ASTFunctionDeclaration *method;
    if (type->base_type) {
      method = type->base_type->info->scope->lookup(name)->function.declaration;
    } else {
      method = type->info->scope->lookup(name)->function.declaration;
    }

    if (method->generic_parameters.empty()) {
      thir->key_values.push_back({
          "pointer",
          take_address_of(visit_node(method), method),
      });
    } else {
      thir->key_values.push_back({
          "pointer",
          make_literal("nullptr", {}, void_type()->take_pointer_to(false), ASTLiteral::Null),
      });
    }
  } else {
    thir->key_values.push_back({
        "pointer",
        make_literal("nullptr", {}, void_type()->take_pointer_to(false), ASTLiteral::Null),
    });
  }

  return thir;
}

THIR *THIRGen::get_field_struct(const std::string &name, Type *type, Type *parent_type) {
  static Type *field_struct_type = ctx.scope->find_type_id("Field", {});
  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = field_struct_type;

  thir->key_values.push_back({"name", make_str(name, {})});

  thir->key_values.push_back({
      "_type",
      to_reflection_type_struct(type),
  });

  thir->key_values.push_back({
      "size",
      make_literal(std::to_string(type->size_in_bytes()), {}, u64_type(), ASTLiteral::Integer),
  });

  if (parent_type->is_kind(TYPE_ENUM)) {
    const auto info = parent_type->info->as<EnumTypeInfo>();
    const auto member = info->find_member(name);
    if (member && member->thir_value) {
      thir->key_values.push_back({"enum_value", member->thir_value.get()});
    }
  } else if (parent_type->is_kind(TYPE_CHOICE)) {
    const auto info = parent_type->info->as<ChoiceTypeInfo>();
    const auto discriminant = info->get_variant_discriminant(name);
    thir->key_values.push_back({"discriminant", make_literal(std::to_string(discriminant), {}, u32_type(), ASTLiteral::Integer)});
  } else if (parent_type->has_no_extensions()) {
    thir->key_values.push_back({
        "offset",
        make_literal(std::to_string(parent_type->offset_in_bytes(name)), {}, u64_type(), ASTLiteral::Integer),
    });
  }

  return thir;
}

THIR *THIRGen::get_field_struct_list(Type *type) {
  static Type *field_type = ctx.scope->find_type_id("Field", {});
  const auto length = type->info->members.size();
  THIR_ALLOC_NO_SRC_RANGE(THIRCollectionInitializer, collection);
  collection->is_variable_length_array = true;
  collection->type = field_type->make_array_of(length);

  for (const auto &member : type->info->members) {
    collection->values.push_back(get_field_struct(member.name.str(), member.type, type));
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = field_list;
  thir->key_values.push_back({"data", collection});

  const auto length_literal = make_literal(std::to_string(length), {}, u64_type(), ASTLiteral::Integer);

  thir->key_values.push_back({
      "length",
      length_literal,
  });

  return thir;
}

THIR *THIRGen::get_methods_list(Type *type) {
  const auto length = type->info->scope->methods_count();
  const auto length_literal = make_literal(std::to_string(length), {}, u64_type(), ASTLiteral::Integer);
  static Type *method_type = ctx.scope->find_type_id("Method", {});

  THIR_ALLOC_NO_SRC_RANGE(THIRCollectionInitializer, collection);
  collection->is_variable_length_array = true;
  collection->type = method_type->make_array_of(length);
  for (const auto &[name, member] : type->info->scope->symbols) {
    if (member.is_function) {
      collection->values.push_back(get_method_struct(name.str(), type));
    }
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = method_list;
  thir->key_values.push_back({"data", collection});
  thir->key_values.push_back({
      "length",
      length_literal,
  });
  return thir;
}

THIR *THIRGen::get_traits_list(Type *type) {
  const auto length = type->traits.size();
  const auto length_literal = make_literal(std::to_string(length), {}, u64_type(), ASTLiteral::Integer);

  static Type *type_type = ctx.scope->find_type_id("Type", {{TYPE_EXT_POINTER_CONST}});

  THIR_ALLOC_NO_SRC_RANGE(THIRCollectionInitializer, collection);
  collection->is_variable_length_array = true;
  collection->type = type_type->make_array_of(length);

  for (const auto &trait : type->traits) {
    collection->values.push_back(to_reflection_type_struct(trait));
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = type_ptr_list;
  thir->key_values.push_back({"data", collection});
  thir->key_values.push_back({
      "length",
      length_literal,
  });
  return thir;
}

THIR *THIRGen::get_generic_args_list(Type *type) {
  const auto length = type->generic_args.size();
  const auto length_literal = make_literal(std::to_string(length), {}, u64_type(), ASTLiteral::Integer);

  static Type *type_type = ctx.scope->find_type_id("Type", {{TYPE_EXT_POINTER_CONST}});

  THIR_ALLOC_NO_SRC_RANGE(THIRCollectionInitializer, collection);
  collection->is_variable_length_array = true;
  collection->type = type_type->make_array_of(length);

  for (const auto &type_arg : type->generic_args) {
    collection->values.push_back(to_reflection_type_struct(type_arg));
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->type = type_ptr_list;

  thir->key_values.push_back({"data", collection});
  thir->key_values.push_back({
      "length",
      length_literal,
  });
  return thir;
}

ReflectionInfo THIRGen::create_reflection_type_struct(Type *type) {
  // for recursive calls
  ReflectionInfo &reflection_info = reflected_upon_types[type];
  if (reflection_info.has_been_created()) {
    return reflection_info;
  }

  static Type *type_type = ctx.scope->find_type_id("Type", {});

  ReflectionInfo &info = reflected_upon_types[type];
  info.created = true;
  info.definition = make_variable(get_temporary_variable(), initialize({}, type_type, {}), nullptr);
  info.definition->is_global = true;
  info.definition->is_statement = true;
  info.reference = (THIRUnaryExpr *)take_address_of(info.definition, nullptr);

  reflected_upon_types[type] = info;

  THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, thir);
  thir->is_statement = false;
  thir->type = type_type;
  info.definition->value = thir;

  thir->key_values.push_back({"id", make_literal(std::to_string(type->uid), {}, u64_type(), ASTLiteral::Integer)});

  thir->key_values.push_back({
      "name",
      make_str(get_unmangled_name(type), {}),
  });

  thir->key_values.push_back({
      "size",
      make_literal(std::to_string(type->size_in_bytes()), {}, u64_type(), ASTLiteral::Integer),
  });

  thir->key_values.push_back(
      {"flags", make_literal(std::to_string(get_reflection_type_flags(type)), {}, u64_type(), ASTLiteral::Integer)});

  thir->key_values.push_back({"generic_args", get_generic_args_list(type)});

  thir->key_values.push_back({
      "fields",
      get_field_struct_list(type),
  });

  if (type->has_extensions()) {
    thir->key_values.push_back({"element_type", to_reflection_type_struct(type->get_element_type())});
  }

  thir->key_values.push_back({"traits", get_traits_list(type)});

  thir->key_values.push_back({
      "methods",
      get_methods_list(type),
  });

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
    return (THIRUnaryExpr *)take_address_of(reflection_info.definition, nullptr);
  }

  reflection_info = create_reflection_type_struct(type);

  return (THIRUnaryExpr *)take_address_of(reflection_info.definition, nullptr);
}

THIR *THIRGen::make_structural_typing_bitcast(Type *to, THIR *expr) {
  THIR_ALLOC_NO_SRC_RANGE(THIRCast, ptr_cast);
  ptr_cast->span = expr->span;
  ptr_cast->type = to->take_pointer_to(true);
  ptr_cast->operand = take_address_of(expr, nullptr);

  THIR_ALLOC_NO_SRC_RANGE(THIRUnaryExpr, deref)
  deref->op = TType::Mul;
  deref->operand = ptr_cast;
  deref->type = to;
  deref->span = expr->span;

  return deref;
}

THIR *THIRGen::visit_run(ASTRun *ast) {
  compile_command.request_compile_time_code_execution(ast->span);

  auto thir = visit_node(ast->node_to_run);
  auto result = interpret(thir, ctx);
  thir = result->to_thir();
  // Fix this.
  thir->is_statement = true;

  if (ast->replace_prev_parent) {
    return thir;
  } else {
    return THIRNoop::shared();
  }
}

void THIRGen::format_and_print_deprecated_warning(Span call_site, THIR *node, const Attribute &attr) {
  if (attr.tag != ATTRIBUTE_DEPRECATED) {
    return;
  }
  Typer typer{ctx};

  if (attr.arguments.size() < 2) {
    throw_error(
        std::format("@[deprecated] attribute expects at least 2 arguments, but got {}. "
                    "the first argument should be the deprecation message, the second should be the symbol to highlight as a"
                    "replacement, or a string describing a replacement if it's not accessible in the scope of the "
                    "deprecated declaration",
                    attr.arguments.size()),
        node ? node->span : Span{});
  }

  // we have to do this since this may refer to out of order code, and that's why we process this so late
  attr.arguments[1]->accept(&typer);
  auto old_scope = ctx.scope;
  ctx.scope = attr.arguments[1]->declaring_scope;
  auto symbol = ctx.get_symbol(attr.arguments[1]).get();
  ctx.scope = old_scope;

  Span range = attr.arguments[1]->span;

  if (symbol) {
    if (symbol->is_variable && symbol->variable.declaration.get()) {
      range = symbol->variable.declaration.get()->span;
    } else if (symbol->is_function && symbol->function.declaration) {
      range = symbol->function.declaration->span;
    } else if (symbol->is_module && symbol->module.declaration) {
      range = symbol->module.declaration->span;
    } else if (symbol->is_type && symbol->type.declaration.get()) {
      range = symbol->type.declaration.get()->span;
    }
  }

  switch (node->get_node_type()) {
    case THIRNodeType::Variable: {
      auto var = static_cast<THIRVariable *>(node);
      fprintf(stderr, "Deprecated variable: %s\n", var->name.c_str());
      break;
    }
    case THIRNodeType::Function: {
      auto func = static_cast<THIRFunction *>(node);
      fprintf(stderr, "Deprecated function: %s\n", func->name.c_str());
      break;
    }
    case THIRNodeType::Type: {
      // Could be struct, enum, choice, etc.
      auto type = static_cast<THIRType *>(node);
      fprintf(stderr, "Deprecated type: %s\n", type->type->basename.c_str());
      break;
    }
    default:
      fprintf(stderr, "Deprecated symbol\n");
      break;
  }

  auto string_literal = (ASTLiteral *)(attr.arguments[0]);

  printf("from: %s\n", call_site.to_string().c_str());

  printf("\n %s --- instead, use: ---\n", string_literal->value.c_str());
  auto sl = format_source_location(range, ERROR_WARNING, 5);
  printf("%s\n", sl.c_str());
}

THIR *THIRGen::visit_node(ASTNode *ast, bool instantiate_conversions) {
  if (ast->is_expr() && instantiate_conversions) {
    const ASTExpr *expr = (ASTExpr *)ast;
    THIR *result = visit_node(ast, false);

    if (!expr->conversion.has_value) {
      return result;
    }

    if (expr->conversion.to->has_no_extensions() && expr->conversion.from->has_no_extensions() &&
        (expr->conversion.to->is_kind(TYPE_STRUCT) || expr->conversion.from->is_kind(TYPE_STRUCT))) {
      return make_structural_typing_bitcast((Type *)expr->conversion.to, result);
    }

    THIR_ALLOC(THIRCast, cast, ast);
    cast->type = (Type *)expr->conversion.to;
    cast->operand = result;
    return cast;
  }

  switch (ast->get_node_type()) {
    case AST_NODE_STATEMENT_LIST: {
      for (const auto &ast_stmt : ((ASTStatementList *)ast)->statements) {
        if (auto thir = visit_node(ast_stmt)) {
          current_statement_list->push_back(thir);
        }
      }
      return nullptr;
    }
    // These nodes can return many nodes, so they always return void, and push the nodes manually.
    case AST_NODE_TUPLE_DECONSTRUCTION: {
      visit_destructure((ASTDestructure *)ast);
      return nullptr;
    }
    case AST_NODE_WHERE_STATEMENT: {
      visit_where_statement((ASTWhereStatement *)ast);
      return nullptr;
    }
    case AST_NODE_IMPL: {
      visit_impl((ASTImpl *)ast);
      return nullptr;
    }
    case AST_NODE_IMPORT: {
      visit_import((ASTImport *)ast);
      return nullptr;
    }
    case AST_NODE_MODULE: {
      visit_module((ASTModule *)ast);
      return nullptr;
    }

    // Actual nodes.
    case AST_NODE_IF:
      return visit_if((ASTIf *)ast);
    case AST_NODE_LAMBDA:
      return visit_lambda((ASTLambda *)ast);
    case AST_NODE_BIN_EXPR:
      return visit_bin_expr((ASTBinExpr *)ast);
    case AST_NODE_UNARY_EXPR:
      return visit_unary_expr((ASTUnaryExpr *)ast);
    case AST_NODE_LITERAL:
      return visit_literal((ASTLiteral *)ast);
    case AST_NODE_PATH:
      return visit_path((ASTPath *)ast);
    case AST_NODE_TUPLE:
      return visit_tuple((ASTTuple *)ast);
    case AST_NODE_CALL:
      return visit_call((ASTCall *)ast);
    case AST_NODE_METHOD_CALL:
      return visit_method_call((ASTMethodCall *)ast);
    case AST_NODE_DOT_EXPR:
      return visit_dot_expr((ASTDotExpr *)ast);
    case AST_NODE_INDEX:
      return visit_index((ASTIndex *)ast);
    case AST_NODE_INITIALIZER_LIST:
      return visit_initializer_list((ASTInitializerList *)ast);
    case AST_NODE_SIZE_OF:
      return visit_size_of((ASTSize_Of *)ast);
    case AST_NODE_TYPE_OF:
      return visit_type_of((ASTType_Of *)ast);
    case AST_NODE_DYN_OF:
      return visit_dyn_of((ASTDyn_Of *)ast);
    case AST_NODE_CAST:
      return visit_cast((ASTCast *)ast);
    case AST_NODE_RANGE:
      return visit_range((ASTRange *)ast);
    case AST_NODE_SWITCH:
      return visit_switch((ASTSwitch *)ast);
    case AST_NODE_PATTERN_MATCH:
      throw_error(
          "INTERNAL COMPILER ERROR:THIR :: you cannot visit a PatternMatch without explicitly calling into it, with "
          "the list of statements it needs to unload into.",
          ast->span);
      return nullptr;
    // Statement nodes
    case AST_NODE_BLOCK:
      return visit_block((ASTBlock *)ast);
    case AST_NODE_FUNCTION_DECLARATION: {
      auto result = visit_function_declaration((ASTFunctionDeclaration *)ast);
      return result;
    }

    case AST_NODE_RETURN:
      return visit_return((ASTReturn *)ast);
    case AST_NODE_CONTINUE:
      return visit_continue((ASTContinue *)ast);
    case AST_NODE_BREAK:
      return visit_break((ASTBreak *)ast);
    case AST_NODE_FOR:
      return visit_for((ASTFor *)ast);
    case AST_NODE_ELSE:
      return visit_else((ASTElse *)ast);
    case AST_NODE_WHILE:
      return visit_while((ASTWhile *)ast);
    case AST_NODE_STRUCT_DECLARATION:
      return visit_struct_declaration((ASTStructDeclaration *)ast);
    case AST_NODE_ENUM_DECLARATION:
      return visit_enum_declaration((ASTEnumDeclaration *)ast);
    case AST_NODE_CHOICE_DECLARATION:
      return visit_choice_declaration((ASTChoiceDeclaration *)ast);
    case AST_NODE_VARIABLE:
      return visit_variable((ASTVariable *)ast);
    case AST_NODE_EXPR_STATEMENT:
      return visit_expr_statement((ASTExprStatement *)ast);
    case AST_NODE_DEFER:
      return visit_defer((ASTDefer *)ast);
    case AST_NODE_PROGRAM:
      return visit_program((ASTProgram *)ast);
    case AST_NODE_FOR_C_STYLE:
      return visit_for_c_style((ASTForCStyle *)ast);
    // Ignored nodes.
    case AST_NODE_NOOP:
    case AST_NODE_ALIAS:
    case AST_NODE_TRAIT_DECLARATION:
      return nullptr;

    case AST_NODE_PARAMS_DECL:
    case AST_NODE_TYPE:
    case AST_NODE_PARAM_DECL:
    case AST_NODE_ARGUMENTS:
    case AST_NODE_WHERE:
    case AST_NODE_UNPACK:
      throw_error(
          "INTERNAL COMPILER ERROR: ast node not supported by thir gen. it's likely it just needs to be moved to the "
          "ignored cases",
          ast->span);
      return nullptr;
    case AST_NODE_UNPACK_ELEMENT: {
      return visit_unpack_element((ASTUnpackElement *)ast);
    } break;

    case AST_NODE_RUN:
      return visit_run((ASTRun *)ast);

    default:
      throw_error("AST node not supported by THIR generator. needs to be implemented", ast->span);
      return nullptr;
  }
}

THIR *THIRGen::make_cast(THIR *operand, Type *type) {
  THIR_ALLOC_NO_SRC_RANGE(THIRCast, cast);
  cast->span = operand->span;
  cast->type = type;
  cast->operand = operand;
  return cast;
}

void THIRGen::setup__all_tests() {
  const bool is_testing = compile_command.has_flag("test");
  if (is_testing) {
    Type *test_struct_type;
    THIRVariable *all_tests_slice_thir;
    ASTPath test_path;
    test_struct_type = g_testing_Test_type;
    all_tests_slice_thir = (THIRVariable *)visit_node(g_testing_tests_declaration);

    THIR_ALLOC_NO_SRC_RANGE(THIRCollectionInitializer, slice_data);
    slice_data->is_variable_length_array = true;
    slice_data->type = g_testing_Test_type->make_array_of(test_functions.size());

    for (const auto &function : test_functions) {
      auto fn_ptr = take_address_of(function, {});
      auto function_name = function->name.str();

      // Replace all occurrences of '$' with "::", so the tests match what the user typed.
      size_t pos = 0;
      while ((pos = function_name.find('$', pos)) != std::string::npos) {
        function_name.replace(pos, 1, "::");
        pos += 2;
      }

      THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, test_struct_init);
      test_struct_init->key_values = {{"function", fn_ptr}, {"name", make_str(function_name, function->span)}};
      test_struct_init->type = test_struct_type;
      slice_data->values.push_back(test_struct_init);
    }

    THIR_ALLOC_NO_SRC_RANGE(THIRVariable, static_variable);
    static_variable->is_static = true;
    static_variable->is_global = false;
    static_variable->name = get_temporary_variable();
    static_variable->value = slice_data;
    static_variable->type = slice_data->type;

    THIR_ALLOC_NO_SRC_RANGE(THIRAggregateInitializer, ini);
    ini->is_statement = false;
    ini->type = all_tests_slice_thir->type;
    ini->key_values = {{"data", static_variable},
                       {"length", make_literal(std::to_string(slice_data->values.size()), {}, u64_type(), ASTLiteral::Integer)}};

    auto &statements = global_initializer_function->block->statements;
    statements.insert(statements.begin(), static_variable);

    all_tests_slice_thir->global_initializer_assignment->right = ini;
  }
}

THIRFunction *THIRGen::emit_runtime_entry_point() {
  // TODO: implement global static initializers in the emitter.
  THIR_ALLOC_NO_SRC_RANGE(THIRFunction, global_ini) { global_ini->name = "ela_run_global_initializers"; }
  const bool is_testing = compile_command.has_flag("test");
  Type *const_u8_ptr_ptr_type = u8_ptr_type()->take_pointer_to(false);  // u8 const* const*;

  THIR_ALLOC_NO_SRC_RANGE(THIRVariable, argv_var);
  argv_var->name = "argv";
  argv_var->type = const_u8_ptr_ptr_type;
  argv_var->is_global = false;

  THIR_ALLOC_NO_SRC_RANGE(THIRVariable, argc_var);
  argc_var->name = "argc";
  argc_var->type = s32_type();
  argc_var->is_global = false;

  if (entry_point->get_node_type() == THIRNodeType::Function || is_testing) {
    THIR_ALLOC_NO_SRC_RANGE(THIRFunction, main) {  // setup main function, get type, parameters, etc.
      FunctionTypeInfo main_info{};
      main_info.params_len = 2;
      main_info.parameter_types[0] = s32_type();             // int
      main_info.parameter_types[1] = const_u8_ptr_ptr_type;  // u8 **
      main_info.return_type = s32_type();

      Type *main_type = global_find_function_type_id(main_info, {});

      main->name = "main";
      main->type = main_type;

      THIRParameter argv = {.name = "argv", .associated_variable = argv_var},
                    argc = {.name = "argc", .associated_variable = argc_var};

      main->parameters.push_back(argc);
      main->parameters.push_back(argv);
    }
    THIR_ALLOC_NO_SRC_RANGE(THIRBlock, block) {  // Create block for main.
      if (!compile_command.has_flag("nostdlib") &&
          !compile_command.has_flag("freestanding")) {  // Call Env::initialize(argc, argv);
        // Find the damn call
        ASTFunctionDeclaration *env_initialize = nullptr;
        ASTPath env_initialize_path;
        env_initialize_path.push_segment("Env");
        env_initialize_path.push_segment("initialize");
        auto symbol = get_symbol(&env_initialize_path);
        env_initialize = symbol->function.declaration;

        THIR_ALLOC_NO_SRC_RANGE(THIRCall, initialize);
        initialize->callee = visit_node(env_initialize);
        initialize->type = env_initialize->return_type->resolved_type;

        initialize->arguments.push_back(argc_var);
        initialize->arguments.push_back(argv_var);
        initialize->is_statement = true;
        // Call the damn call
        block->statements.push_back(initialize);
      }

      if (!is_testing && !compile_command.has_flag("freestanding") &&
          this->entry_point->get_node_type() == THIRNodeType::Function) {
        // Call __ela_main_();
        // TODO: actually check this. won't work in freestanding builds or library (main-less) builds
        THIRFunction *entry_point = (THIRFunction *)this->entry_point;
        THIR_ALLOC_NO_SRC_RANGE(THIRCall, entry_call)
        entry_call->callee = entry_point;
        entry_call->arguments = {};
        entry_call->is_statement = true;

        FunctionTypeInfo entry_type;
        entry_type.return_type = void_type();
        entry_type.is_varargs = false;
        entry_type.params_len = 0;
        entry_call->type = global_find_function_type_id(entry_type, {});
        block->statements.push_back(entry_call);
      }

      if (is_testing) {
        THIR_ALLOC_NO_SRC_RANGE(THIRCall, _run_all_tests_call) {
          ASTPath run_all_tests;
          run_all_tests.push_segment("testing");
          run_all_tests.push_segment(RUN_ALL_TESTS_GLOBAL_FUNCTION);

          auto _run_all_tests_function = get_symbol(&run_all_tests);

          ASTFunctionDeclaration *ast_func = _run_all_tests_function->function.declaration;

          _run_all_tests_call->callee = visit_node(ast_func);
          _run_all_tests_call->arguments = {};
          _run_all_tests_call->type = void_type();
          _run_all_tests_call->is_statement = true;
          block->statements.push_back(_run_all_tests_call);
        }
      }
    }

    // Don't rely on C's weird implicit return 0 behaviour, always return 0.
    THIR_ALLOC_NO_SRC_RANGE(THIRReturn, ret_0);
    ret_0->expression = make_literal("0", {}, s32_type(), ASTLiteral::Integer);
    ret_0->type = s32_type();
    ret_0->is_statement = true;
    block->statements.push_back(ret_0);

    main->block = block;
    return main;
  }

  return nullptr;
}

THIR *THIRGen::visit_unpack_element(ASTUnpackElement *ast) {
  switch (ast->tag) {
    case ASTUnpackElement::TUPLE_ELEMENT: {
      THIR_ALLOC(THIRMemberAccess, thir, ast);
      thir->member = "$" + std::to_string(ast->tuple.element_index);
      thir->base = visit_node(ast->tuple.source_tuple);
      return thir;
    }
    case ASTUnpackElement::RANGE_ELEMENT: {
      return visit_node(ast->range_literal_value);
    } break;
  }
}

void THIRGen::make_global_initializer(const Type *type, THIRVariable *thir, Nullable<ASTExpr> value_n) {
  THIR *value;
  if (value_n.is_not_null()) {
    value = visit_node(value_n.get());
  } else {
    value = initialize(thir->span, (Type *)type, {});
  }

  thir->value = nullptr;  // global variables never get a value before the initializer.

  THIR_ALLOC_NO_SRC_RANGE(THIRBinExpr, expr)
  expr->left = thir;
  expr->right = value;
  expr->is_statement = true;
  expr->op = TType::Assign;
  expr->type = value->type;

  thir->global_initializer_assignment = expr;

  //  Have to call memcpy for array assignment to global variables. Really, what we should do is not this, and just not
  //  make
  // a global initializer for it, but we wanna support non-constants in arrays.
  if (type->is_fixed_sized_array()) {
    if (value->get_node_type() == THIRNodeType::EmptyInitializer) {
      return;
    }
    if (value->get_node_type() == THIRNodeType::CollectionInitializer) {
      THIR_ALLOC_NO_SRC_RANGE(THIRCall, memcpy_call)
      ASTPath p;
      p.push_segment("std");
      p.push_segment("c");
      p.push_segment("memcpy");
      auto symbol = get_symbol(&p);
      auto memcpy_fn = visit_node(symbol->function.declaration);
      memcpy_call->callee = memcpy_fn;

      THIR_ALLOC_NO_SRC_RANGE(THIRVariable, temp);
      temp->value = value;
      temp->is_global = false;
      temp->type = value->type;
      temp->name = get_temporary_variable();
      temp->is_statement = true;

      auto init = (THIRCollectionInitializer *)value;

      memcpy_call->arguments = {
          thir, temp,
          make_literal(std::to_string(temp->type->get_element_type()->size_in_bytes() * init->values.size()), {}, u64_type(),
                       ASTLiteral::Integer)};
      memcpy_call->is_statement = true;

      // This just wont work with compile time, which is annoying
      global_initializer_function->block->statements.push_back(temp);
      global_initializer_function->block->statements.push_back(memcpy_call);

      return;
    }
  }

  global_initializer_function->block->statements.push_back(expr);
}

void THIRGen::enter_defer_boundary(DeferBoundary boundary) { defer_stack.push_back({boundary, {}}); }

void THIRGen::exit_defer_boundary() { defer_stack.pop_back(); }

// inclusive
std::vector<THIR *> THIRGen::collect_defers_up_to(DeferBoundary boundary) {
  std::vector<THIR *> out;
  // Walk the defer_stack from the top (back) to the bottom without mutating it.
  for (auto frame_it = defer_stack.rbegin(); frame_it != defer_stack.rend(); ++frame_it) {
    const DeferFrame &frame = *frame_it;
    for (auto it = frame.defers.rbegin(); it != frame.defers.rend(); ++it) {
      out.push_back(*it);
    }
    if (frame.boundary == boundary) {
      break;
    }
  }
  return out;
}

THIRGen::THIRGen(Context &ctx, bool for_emitter) : ctx(ctx) {
  if (!for_emitter) {
    return;
  }
  if (!compile_command.has_flag("freestanding") && !compile_command.has_flag("nostdlib")) {
    Type *type_ptr_ty = g_refl_Type_type->take_pointer_to(CONST);
    Type *method_ty = g_refl_Method_type;
    Type *field_ty = g_refl_Field_type;

    const ASTDeclaration *slice_decl = g_Slice_declaration;

    const ASTDeclaration *list_instance = find_generic_instance(slice_decl->generic_instantiations, {type_ptr_ty});
    const ASTDeclaration *method_instance = find_generic_instance(slice_decl->generic_instantiations, {method_ty});
    const ASTDeclaration *field_instance = find_generic_instance(slice_decl->generic_instantiations, {field_ty});

    if (!list_instance) {
      throw_error(
          "INTERNAL COMPILER ERROR: unable to find [*Type] for reflection. if you're compiling with nostdlib, make "
          "sure you satisfy the compiler dependencies you use.",
          {});
    }
    if (!method_instance) {
      throw_error(
          "INTERNAL COMPILER ERROR: unable to find [Method] for reflection. if you're compiling with nostdlib, make "
          "sure you satisfy the compiler dependencies you use.",
          {});
    }
    if (!field_instance) {
      throw_error(
          "INTERNAL COMPILER ERROR: unable to find [Field] for reflection. if you're compiling with nostdlib, make "
          "sure you satisfy the compiler dependencies you use.",
          {});
    }

    type_ptr_list = list_instance->resolved_type;
    method_list = method_instance->resolved_type;
    field_list = field_instance->resolved_type;
  }

  THIR_ALLOC_NO_SRC_RANGE(THIRFunction, global_ini);
  global_initializer_function = global_ini;
  global_ini->constructor_index = 0; // highest priority.

  FunctionTypeInfo info;
  info.params_len = 0;
  info.return_type = void_type();
  global_ini->type = global_find_function_type_id(info, {});
  global_ini->is_statement = true;
  global_ini->name = "ela_run_global_initializers";
  global_ini->parameters = {};
  global_ini->block = thir_alloc<THIRBlock>();
  global_ini->block->is_statement = true;

  THIR_ALLOC_NO_SRC_RANGE(THIRCall, global_ini_call);
  global_initializer_call = global_ini_call;
  global_ini_call->callee = global_ini;
  global_ini_call->is_statement = true;
  global_ini_call->arguments = {};
}

void THIRGen::check_for_deprecation(Span call_site, THIR *thir) {
  if (thir->deprecated) {
    format_and_print_deprecated_warning(call_site, thir, thir->deprecated_attr);
  }
}

THIR *THIRGen::visit_for_c_style(ASTForCStyle *ast) {
  ENTER_SCOPE(ast->scope);
  THIR_ALLOC(THIRFor, thir, ast);
  thir->initialization = visit_node(ast->initialization);
  thir->condition = visit_node(ast->condition);
  thir->increment = visit_node(ast->increment);
  thir->block = visit_node(ast->block);
  thir->is_statement = true;
  return thir;
}
