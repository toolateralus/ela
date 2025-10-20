#include <algorithm>
#include <cassert>
#include <csetjmp>
#include <ctime>
#include <format>
#include <iostream>
#include <linux/limits.h>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "ast.hpp"
#include "copier.hpp"
#include "strings.hpp"
#include "interpreter.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "value.hpp"
#include "visitor.hpp"

// TODO:
/*
  TODO: we can probably replace the generic panic handler with an exception. depending on how much bigger the compiler
  binary is.
  TODO: use the ENTER_SCOPE macro everywhere in here. so many lines of code to do the same thing.
  TODO: use an SET_EXPECTED_TYPE that also does an RAII for that same shit. again, a ton of code eliminated.
*/

#define ENTER_EXPECTED_TYPE($type)         \
  auto $old_expected_type = expected_type; \
  expected_type = $type;                   \
  Defer $expected_type_defer([&] { expected_type = $old_expected_type; });

#define USE_GENERIC_PANIC_HANDLER

#ifdef USE_GENERIC_PANIC_HANDLER
#define GENERIC_PANIC_HANDLER(data_name, uid, block, span) \
  GenericInstantiationErrorUserData data_name;             \
  set_panic_handler(generic_instantiation_panic_handler);  \
  set_error_user_data(&data_name);                         \
  Defer defer_##uid([] { reset_panic_handler(); });        \
  if (setjmp(data_name.save_state) == 0) {                 \
    /* clang-format off */\
    block                                \
    /* clang-format on */                                  \
  } else {                                                 \
    handle_generic_error(&data_name, span);                \
  }
#else
#define GENERIC_PANIC_HANDLER(data_name, uid, block, span) block
#endif

std::string print_where_predicate(ASTExpr *predicate) {
  switch (predicate->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin = static_cast<ASTBinExpr *>(predicate);
      auto op = bin->op;
      if (op == TType::And) {
        return print_where_predicate(bin->left) + " & " + print_where_predicate(bin->right);
      } else if (op == TType::Or) {
        return print_where_predicate(bin->left) + " | " + print_where_predicate(bin->right);
      } else {
        throw_error(
            "Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
            "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
            bin->span);
      }
    } break;
    case AST_NODE_TYPE: {
      return predicate->resolved_type->to_string();
    } break;
    default:
      throw_error("Invalid node in 'where' clause predicate", predicate->span);
  }
  return "";
}

void handle_generic_error(GenericInstantiationErrorUserData *data, const Span &range) {
  reset_panic_handler();
  throw_error(std::format("Error at definition: {}\nerror: {}", format_source_location(data->definition_range, ERROR_FAILURE, 3),
                          data->message),
              range);
}

auto generic_instantiation_panic_handler(auto msg, auto range, auto void_data) {
  auto data = (GenericInstantiationErrorUserData *)(void_data);
  data->definition_range = range;
  data->message = msg;
  longjmp(data->save_state, 1);
};

void assert_types_can_cast_or_equal(ASTExpr *expr, Type *to, const Span &span, const std::string &message) {
  auto from_t = expr->resolved_type;
  auto to_t = to;
  auto conv_rule = type_conversion_rule(from_t, to_t, span);

  if (to != expr->resolved_type && (conv_rule == CONVERT_PROHIBITED || conv_rule == CONVERT_EXPLICIT)) {
    throw_error(message + '\n' + std::format("expected \"{}\", got \"{}\"", to_t->to_string(), from_t->to_string()), span);
  }

  if (conv_rule == CONVERT_IMPLICIT) {
    if (expr->conversion.has_value) {
      // printf("a conversion was done twice on a single expression.\n");
      // return;
    }
    expr->resolved_type = to;
    // TODO: would a single expression ever go through multiple implicit conversions? -- the answer is yes via monomorphization
    // !BUG and this is a bug.
    // See the copier as to why, I have no idea what's going on there.
    // primarily its calls in default trait methods that seem to fail.
    expr->conversion = {
        .has_value = true,
        .from = from_t,
        .to = to_t,
    };
  }
}

void implicit_cast(ASTExpr *expr, Type *to) {
  expr->conversion = {.has_value = true, .from = expr->resolved_type, .to = to};
  expr->resolved_type = to;
}

bool expr_is_literal(const ASTExpr *expr) {
  switch (expr->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin_expr = static_cast<const ASTBinExpr *>(expr);
      return expr_is_literal(bin_expr->left) && expr_is_literal(bin_expr->right);
    }
    case AST_NODE_UNARY_EXPR:
      return expr_is_literal(static_cast<const ASTUnaryExpr *>(expr)->operand);
    case AST_NODE_LITERAL:
      return true;
    default:
      return false;
  }
}

const InternedString get_identifier_from_attribute_arguments(const Span &span, const Attribute &attribute) {
  if (attribute.arguments.size() == 0) {
    throw_error(
        "@[compiler_feature(...)] must have exactly one argument, and it must be an identifier pointing to the "
        "correct feature.",
        span);
  }

  ASTNode *argument = attribute.arguments[0];

  if (argument->get_node_type() != AST_NODE_PATH) {
    throw_error(
        "@[compiler_feature(...)] must have exactly one argument, and it must be an identifier pointing to the "
        "correct feature.",
        span);
  }

  ASTPath *path = (ASTPath *)argument;

  if (path->segments.size() != 1 || path->segments[0].tag != ASTPath::Segment::IDENTIFIER) {
    throw_error(
        "@[compiler_feature(...)] must have exactly one argument, and it must be an identifier pointing to the "
        "correct feature.",
        span);
  }

  return path->segments[0].get_identifier();
}

void fetch_compiler_features_for_function_declaration(ASTFunctionDeclaration *node) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString identifier = get_identifier_from_attribute_arguments(node->span, attribute);
      if (identifier == COMPILER_FEATURE_TEST_RUNNER_FN_KEY) {
        g_testing_runner_declaration = node;
      } else if (!compiler_feature_exists(identifier.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", identifier.str()), node->span);
      }
    }
  }
}

void fetch_compiler_features_for_varaible_declaration(ASTVariable *node) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString identifier = get_identifier_from_attribute_arguments(node->span, attribute);
      if (identifier == COMPILER_FEATURE_TESTS_LIST_KEY) {
        g_testing_tests_declaration = node;
        break;
      } else if (!compiler_feature_exists(identifier.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", identifier.str()), node->span);
      }
    }
  }
}

void fetch_compiler_features_for_choice_declaration_generic(ASTChoiceDeclaration *node) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString identifier = get_identifier_from_attribute_arguments(node->span, attribute);
      if (identifier == COMPILER_FEATURE_OPTION_KEY) {
        g_Option_type = node;
        break;
      } else if (!compiler_feature_exists(identifier.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", identifier.str()), node->span);
      }
    }
  }
}

void fetch_compiler_features_for_trait_declaration(ASTTraitDeclaration *node, Type *type) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString identifier = get_identifier_from_attribute_arguments(node->span, attribute);
      if (identifier == COMPILER_FEATURE_DESTROY_KEY) {
        g_Destroy_trait_type = type;
        break;
      } else if (identifier == COMPILER_FEATURE_ITERATOR_KEY) {
        g_Iterator_trait_type = type;
      } else if (identifier == COMPILER_FEATURE_ITERABLE_KEY) {
        g_Iterable_trait_type = type;
      } else if (identifier == COMPILER_FEATURE_INIT_KEY) {
        g_Init_trait_type = type;
      } else if (!compiler_feature_exists(identifier.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", identifier.str()), node->span);
      }
    }
  }
}

void fetch_compiler_features_for_struct_declaration_concrete(ASTStructDeclaration *node, Type *attribute_feature_type) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString feature = get_identifier_from_attribute_arguments(node->span, attribute);
      if (feature == COMPILER_FEATURE_FIELD_KEY) {
        g_refl_Field_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_METHOD_KEY) {
        g_refl_Method_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_TYPE_KEY) {
        g_refl_Type_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_TEST_KEY) {
        g_testing_Test_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_STR_KEY) {
        g_str_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_STRING_KEY) {
        g_String_type = attribute_feature_type;
        break;
      } else if (feature == COMPILER_FEATURE_TESTS_LIST_KEY) {
        // TODO handle in ASTVariable* visitor
        break;
      } else if (feature == COMPILER_FEATURE_DESTROY_KEY) {
        // TODO: handling in trait visitor
        break;
      } else if (feature == COMPILER_FEATURE_TEST_RUNNER_FN_KEY) {
        // TODO: handle in function visitor.
      } else if (feature == COMPILER_FEATURE_OPTION_KEY) {
        // TODO: handle in choice
        break;
      } else if (!compiler_feature_exists(feature.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", feature.str()), node->span);
      }
    }
  }
}

void fetch_compiler_features_for_struct_declaration_generic(ASTStructDeclaration *node) {
  for (const auto &attribute : node->attributes) {
    if (attribute.tag == ATTRIBUTE_COMPILER_FEATURE) {
      const InternedString feature = get_identifier_from_attribute_arguments(node->span, attribute);
      if (feature == COMPILER_FEATURE_INITLIST_KEY) {
        g_InitList_declaration = node;
        break;
      } else if (feature == COMPILER_FEATURE_SLICE_KEY) {
        g_Slice_declaration = node;
        break;
      } else if (feature == COMPILER_FEATURE_SLICEMUT_KEY) {
        g_SliceMut_declaration = node;
        break;
      } else if (!compiler_feature_exists(feature.c_str())) {
        throw_error(std::format("unknown compiler feature: '{}'", feature.str()), node->span);
      }
    }
  }
}

void Typer::visit_structural_type_declaration(ASTStructDeclaration *node) {
  std::vector<Type *> members;
  for (auto member : node->members) {
    member.type->accept(this);
    members.push_back(member.type->resolved_type);
  }

  for (const auto &type : structural_type_table) {
    const StructTypeInfo *info = type->info->as<StructTypeInfo>();
    if (info->structural_match(members)) {
      node->resolved_type = type;
      node->name = type->basename;
      return;
    }
  }

  auto type = ctx.scope->create_struct_type(node->name, node->scope, node);
  structural_type_table.push_back(type);

  StructTypeInfo *info = type->info->as<StructTypeInfo>();
  info->is_structural = true;

  node->resolved_type = type;

  for (const auto &member : node->members) {
    type->info->members.push_back({
        .name = member.name,
        .type = member.type->resolved_type,
    });
    type->info->scope->insert_local_variable(member.name, member.type->resolved_type, nullptr, MUT);
  }
}

void Typer::visit_struct_declaration(ASTStructDeclaration *node, bool generic_instantiation, std::vector<Type *> generic_args) {
  if (node->is_structural) {
    return visit_structural_type_declaration(node);
  }

  Type *type = nullptr;
  bool type_just_created = false;

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto type_argument = *generic_arg;
      node->scope->create_type_alias(param.identifier, *generic_arg, type_argument->declaring_node.get());
      generic_arg++;
    }
    type = global_create_struct_type(node->name, node->scope, generic_args);
    type_just_created = true;
  } else {
    auto symbol = ctx.scope->local_lookup(node->name);
    if (symbol && type_is_valid(symbol->resolved_type)) {
      type = symbol->resolved_type;
    }
    if (type_is_valid(type)) {
      if (type->is_kind(TYPE_STRUCT)) {
        auto info = (type->info->as<StructTypeInfo>());
        if (!info->is_forward_declared) {
          throw_error("Redefinition of struct", node->span);
        }
      } else {
        throw_error("cannot redefine already existing type", node->span);
      }
    } else {
      type = ctx.scope->create_struct_type(node->name, node->scope, node);
      type_just_created = true;
    }
  }

  if (!type) {
    throw_error("internal compiler error: struct type was null on declaration", node->span);
  }

  Type *attribute_feature_type = type;
  if (generic_instantiation) {
    attribute_feature_type = type->generic_base_type;
  }
  fetch_compiler_features_for_struct_declaration_concrete(node, attribute_feature_type);

  // tidy up some references.
  type->declaring_node = node;
  node->resolved_type = type;

  // assign scope to ensure it's correct.
  auto info = type->info->as<StructTypeInfo>();
  info->scope = node->scope;

  // swap to the struct's scope.
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  // create a type context for #self.
  auto old_type_context = type_context;
  ASTType ast_type;
  ast_type.resolved_type = type;
  type_context = &ast_type;

  // make sure to exit the type context and scope when done here.
  Defer _([&] {
    type_context = old_type_context;
    ctx.scope = old_scope;
  });

  // setup some flags.
  info->is_anonymous = node->is_anonymous;
  info->is_structural = node->is_structural;
  info->is_union = node->is_union;

  if (node->is_forward_declared && type_just_created) {
    info->is_forward_declared = true;
  } else {
    info->is_forward_declared = false;
  }

  // if this is a forward declaration, exit out.
  if (info->is_forward_declared || node->is_forward_declared) {
    return;
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  for (auto subunion : node->subtypes) {
    subunion->accept(this);

    // TODO: we shouldn't just unload these members in there.
    // This shells off the information that shows that these members came from a union, in the type descriptor
    for (const auto &field : subunion->members) {
      field.type->accept(this);
      info->scope->insert_variable(field.name, field.type->resolved_type, nullptr, MUT);
    }

    info->members.push_back({
        .name = subunion->name,
        .type = subunion->resolved_type,
        .default_value = nullptr,
    });
  }

  for (const ASTStructMember &member : node->members) {
    member.type->accept(this);

    ctx.scope->insert_local_variable(member.name, member.type->resolved_type, nullptr, MUT);

    if (member.default_value) {
      auto old_expected_type = expected_type;
      expected_type = member.type->resolved_type;
      member.default_value.get()->accept(this);
      expected_type = old_expected_type;
    }

    info->members.push_back({
        .name = member.name,
        .type = member.type->resolved_type,
        .default_value = member.default_value,
    });
  }
}

void Typer::visit_choice_declaration(ASTChoiceDeclaration *node, bool generic_instantiation, std::vector<Type *> generic_args) {
  AST_ENTER_SCOPE(node->scope);

  /* get the type, if it's a concrete declaration. */
  auto type = node->resolved_type;

  /* otherwise, we alias our generic parameters by name, then create the instantation. */
  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto type = *generic_arg;
      ctx.scope->create_type_alias(param.identifier, *generic_arg, type->declaring_node.get());
      generic_arg++;
    }
    type = global_create_choice_type(node->name.str(), node->scope, generic_args);
  }

  node->resolved_type = type;
  type->declaring_node = node;
  auto info = type->info->as<ChoiceTypeInfo>();

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  /*
    * important!

    I'm doing this because you may make variants that have the same name as types that are used
    in the variants themselves.

    In doing so, you will inadvertently get the wrong type when you say

    ... :: choice {
      Expression(*mut Expression),
      If {
        expression: *mut Expression
      }
    }

    because the type system will look up the previous variant, isntead of taking *mut Expression,
    which is either the parent type, or whatever external type.

    So, we defer the creation of all aliases and substruct types, and this is fine because
    you can't even refer to them directly anyway.
  */

  if (node->is_forward_declared) {
    return;
  }

  using alias_tuple = std::tuple<InternedString, Type *, TypeKind, ASTNode *>;
  using struct_tuple = std::tuple<InternedString, Scope *>;

  constexpr auto ALIAS_VARIANT_INDEX = 0;
  constexpr auto STRUCT_VARIANT_INDEX = 1;

  std::vector<std::variant<alias_tuple, struct_tuple>> variants;

  info->scope = node->scope;
  for (const auto &variant : node->variants) {
    switch (variant.kind) {
      case ASTChoiceVariant::NORMAL: {
        variants.emplace_back(alias_tuple{variant.name, void_type(), TYPE_SCALAR, nullptr});
      } break;
      case ASTChoiceVariant::TUPLE: {
        variant.tuple->accept(this);
        auto type = variant.tuple->resolved_type;
        variants.emplace_back(alias_tuple{variant.name, type, TYPE_TUPLE, variant.tuple});
      } break;
      case ASTChoiceVariant::STRUCT: {
        ctx.set_scope();  // create new scope.
        for (const auto &field : variant.struct_declarations) {
          field->accept(this);
          field->resolved_type = field->type->resolved_type;
        }
        variants.emplace_back(struct_tuple{variant.name, ctx.exit_scope()});  // exit and get scope.
      } break;
    }
  }

  for (const auto variant : variants) {
    switch (variant.index()) {
      case ALIAS_VARIANT_INDEX: {
        const auto &[name, type, kind, declaring_node] = std::get<ALIAS_VARIANT_INDEX>(variant);
        info->scope->create_type_alias(name, type, declaring_node);
        info->members.push_back(TypeMember{.name = name, .type = type});
        info->scope->local_lookup(name)->type.choice = node;
      } break;
      case STRUCT_VARIANT_INDEX: {
        const auto &[name, scope] = std::get<STRUCT_VARIANT_INDEX>(variant);
        const auto variant_type = info->scope->create_struct_type(name, scope, nullptr);
        info->members.push_back(TypeMember{.name = name, .type = variant_type});
        info->scope->local_lookup(name)->type.choice = node;
        variant_type->choice_parent = type;
      } break;
    }
  }

  // TODO: add an auto implementation of Destroy for any variants that support it, if any.

  implement_destroy_glue_for_choice_type(node, generic_instantiation, generic_args);
}

void Typer::visit_function_body(ASTFunctionDeclaration *node, bool macro_expansion) {
  //// TODO: handle more attributes
  for (auto attr : node->attributes) {
    switch (attr.tag) {
      case ATTRIBUTE_INLINE: {
        node->is_inline = true;
      } break;
      case ATTRIBUTE_ENTRY: {
        node->is_entry = true;
      } break;
      default:
        break;
    }
  }

  if (node->name == "main") {
    node->is_entry = true;
  }

  if (node->is_macro && !macro_expansion) {
    // These get thier bodies type checked on call, not here.
    return;
  }

  ENTER_EXPECTED_TYPE(node->return_type->resolved_type);
  AST_ENTER_SCOPE(node->scope);

  auto block = node->block.get();
  if (!block) {
    throw_error("internal compiler error: attempting to visit body of function forward declaration.", node->span);
  }
  block->accept(this);
  auto control_flow = block->control_flow;
  if (control_flow.type == Type::INVALID_TYPE) control_flow.type = void_type();
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_CONTINUE)) throw_error("Keyword \"continue\" must be in a loop.", node->span);
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_BREAK)) throw_error("Keyword \"break\" must be in a loop.", node->span);
  if (HAS_FLAG(control_flow.flags, BLOCK_FLAGS_FALL_THROUGH) && node->return_type->resolved_type != void_type())
    throw_error("Not all code paths return a value.", node->span);
}

Type *Typer::get_self_type() {
  if (type_context.is_not_null()) {
    type_context.get()->accept(this);
    return type_context.get()->resolved_type;
  }
  return Type::INVALID_TYPE;
}

Type *Typer::find_generic_type_of(const InternedString &base, std::vector<Type *> generic_args, const Span &span) {
  ASTStatement *instantiation = nullptr;
  auto symbol = ctx.scope->lookup(base);

  // Probably not a generic type?
  if (!symbol || !symbol->is_type) {
    return Type::INVALID_TYPE;
  }

  auto declaring_node = symbol->type.declaration.get();

  if (!declaring_node) {
    throw_error("internal compiler error: unable to find type's declaring node", span);
  }

  switch (declaring_node->get_node_type()) {
    case AST_NODE_STRUCT_DECLARATION:
    case AST_NODE_FUNCTION_DECLARATION:
    case AST_NODE_TRAIT_DECLARATION:
      break;
    default:
      throw_error("Invalid target to generic args", span);
      break;
  }

  instantiation = visit_generic((ASTDeclaration *)declaring_node, generic_args, span);

  return global_find_type_id(instantiation->resolved_type, {});
}

void Typer::visit_function_header(ASTFunctionDeclaration *node, bool visit_where_clause, bool generic_instantiation,
                                  std::vector<Type *> generic_args) {
  // Setup context.
  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  Defer _([&] { ctx.set_scope(old_scope); });

  //// TODO: handle more attributes
  for (auto attr : node->attributes) {
    switch (attr.tag) {
      case ATTRIBUTE_INLINE: {
        node->is_inline = true;
      } break;
      case ATTRIBUTE_ENTRY: {
        node->is_entry = true;
      } break;
      default:
        break;
    }
  }

  if (node->name == "main") {
    node->is_entry = true;
  }

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto type = *generic_arg;
      ctx.scope->create_type_alias(param.identifier, *generic_arg, type->declaring_node.get());
      generic_arg++;
    }
  }

  if (!node->is_forward_declared && !node->is_extern) {
    node->scope->name = node->name.str() + mangled_type_args(generic_args);
  }

  if (node->where_clause && visit_where_clause) {
    node->where_clause.get()->accept(this);
  }

  node->return_type->accept(this);

  if (node->return_type->resolved_type->is_fixed_sized_array()) {
    throw_error("cannot return a fixed sized array from a function! the memory would be invalid immediately!", node->span);
  }

  node->params->accept(this);

  FunctionTypeInfo info;
  // Get function type id from header.
  info.return_type = node->return_type->resolved_type;
  info.is_varargs = node->is_varargs;

  for (const auto &param : node->params->params) {
    if (param->tag == ASTParamDecl::Normal) {
      auto &normal = param->normal;
      ctx.scope->insert_local_variable(normal.name, param->resolved_type, nullptr, param->mutability, param);
      info.parameter_types[info.params_len] = param->resolved_type;
    } else {
      auto type = get_self_type();
      if (param->self.is_pointer) {
        type = type->take_pointer_to(param->mutability);
      }

      ctx.scope->insert_local_variable("self", type, nullptr, param->mutability, param);
      info.parameter_types[info.params_len] = type;
    }

    info.params_len++;
  }

  if (info.return_type == Type::UNRESOLVED_GENERIC) {
    throw_error("internal compiler error: unresolved generic return type.", node->span);
  }
  node->resolved_type = global_find_function_type_id(info, {});
}

bool type_equals_or_matches_trait(Type *expected, Type *input) {
  if (expected->is_kind(TYPE_TRAIT)) {
    if (expected->generic_base_type != Type::INVALID_TYPE) {
      if (!input->implements(expected->generic_base_type)) {
        return false;
      }
      if (expected->generic_args != input->generic_args) {
        return false;
      }
    } else {
      if (!input->implements(expected)) {
        return false;
      }
    }
  } else if (expected != input) {
    return false;
  }
  return true;
}

bool impl_method_matches_trait(Type *trait_method, Type *impl_method) {
  FunctionTypeInfo *trait_method_info = trait_method->info->as<FunctionTypeInfo>();
  FunctionTypeInfo *impl_method_info = impl_method->info->as<FunctionTypeInfo>();

  if (trait_method_info->params_len != impl_method_info->params_len) {
    return false;
  }

  for (size_t i = 0; i < trait_method_info->params_len; ++i) {
    Type *trait_param = trait_method_info->parameter_types[i];
    Type *impl_param = impl_method_info->parameter_types[i];
    if (!type_equals_or_matches_trait(trait_param, impl_param)) {
      return false;
    }
  }

  Type *trait_return = trait_method_info->return_type;
  Type *impl_return = impl_method_info->return_type;
  return type_equals_or_matches_trait(trait_return, impl_return);
}

void Typer::visit_impl_declaration(ASTImpl *node, bool generic_instantiation, std::vector<Type *> generic_args) {
  auto previous = ctx.scope;
  auto old_type = type_context;
  Defer _([&] {
    ctx.set_scope(previous);
    type_context = old_type;
  });

  type_context = node->target;
  ctx.set_scope(node->scope);

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto type = *generic_arg;
      ctx.scope->create_type_alias(param.identifier, *generic_arg, type->declaring_node.get());
      generic_arg++;
    }
  }

  if (node->where_clause) {
    node->where_clause.get()->accept(this);
  }

  node->target->accept(this);

  if (node->target->resolved_type == Type::UNRESOLVED_GENERIC) {
    throw_error(
        "the target of an impl was a generic type, but no type arguments were provided. use `impl!<T> "
        "MyType!<T> {...}`, or provide a concrete type, such as `impl MyType!<s32> {...}`",
        node->span);
  }

  node->scope->name = "$" + std::to_string(node->target->resolved_type->uid) + "impl";

  auto target_ty = node->target->resolved_type;
  if (!target_ty) {
    if (node->target->resolved_type == Type::INVALID_TYPE) {
      throw_error("use of undeclared type", node->target->span);
    } else if (node->target->resolved_type == Type::UNRESOLVED_GENERIC) {
      throw_error("use of unresolved generic type", node->target->span);
    }
  }
  Type *trait_ty = nullptr;

  if (node->trait) {
    node->trait.get()->accept(this);
    auto trait_id = node->trait.get()->resolved_type;
    if (trait_id == Type::INVALID_TYPE) {
      throw_error("internal compiler error: type of impl trait was invalid", node->span);
    }
    trait_ty = trait_id;

    if (trait_ty->info->as<TraitTypeInfo>()->is_forward_declared) {
      throw_error("Attempted to implement a forward declared but not yet defined trait type", node->span);
    }

    node->scope->name = node->scope->name.str() + "_of" + std::to_string(trait_id->uid);

    auto decl_node = (ASTTraitDeclaration *)trait_id->declaring_node.get();

    if (decl_node && decl_node->where_clause) {
      // TODO: figure out why this doesn't really work right for generic impls.
      // for some reason it doesn't report the error at the impl location.

      // Again, I think that having exceptions is totally acceptable here, right now, it's rather fragile with
      // having recursive generic panic handling logic etc.

      // it will swell and grow the size of the binary, and maybe we can improve the current system, but
      // it's just too fragile.
      GENERIC_PANIC_HANDLER(
          data, 1,
          {
            ctx.set_scope(decl_node->scope);
            decl_node->where_clause.get()->accept(this);
            ctx.set_scope(node->scope);
          },
          node->span);
    } else if (decl_node && !decl_node->trait_bounds.empty()) {
      GenericInstantiationErrorUserData data;
      set_panic_handler(generic_instantiation_panic_handler);
      set_error_user_data(&data);
      Defer defer_1([] { reset_panic_handler(); });
      if (_setjmp(data.save_state) == 0) {
        for (auto &constraint : decl_node->trait_bounds) {
          auto [target_type, predicate] = constraint;
          target_type->accept(this);
          auto type = target_type->resolved_type;
          auto satisfied = visit_where_predicate(type, predicate);
          if (!satisfied) {
            throw_error(
                std::format("constraint \"{}\" not satisfied for {}", print_where_predicate(predicate), get_unmangled_name(type)),
                node->span);
          }
        }
      } else {
        handle_generic_error(&data, node->span);
      };
    }
  }

  auto type_scope = target_ty->info->scope;
  Scope impl_scope = {};

  for (const auto &constant : node->constants) {
    auto old = ctx.scope;
    ctx.scope = type_scope;
    constant->accept(this);
    ctx.scope = old;
  }

  for (const auto &alias : node->aliases) {
    alias->accept(this);
  }

  // We forward declare all the methods so they can refer to each other without obnoxious crud crap.
  for (const auto &method : node->methods) {
    method->declaring_type = target_ty;

    if (!method->generic_parameters.empty()) {
      // TODO: actually generate a signature for a generic function so that you can compare them
      type_scope->insert_function(method->name, Type::UNRESOLVED_GENERIC, method);
      impl_scope.symbols[method->name] = type_scope->symbols[method->name];
      continue;
    }

    visit_function_header(method, false, false, {});

    if (auto symbol = type_scope->local_lookup(method->name)) {
      if (!symbol->is_forward_declared && !method->is_forward_declared) {
        throw_error("Redefinition of method", method->span);
      } else {
        symbol->is_forward_declared = false;
      }
    } else {
      if (method->is_forward_declared) {
        type_scope->forward_declare_function(method->name, method->resolved_type, method);
      } else {
        type_scope->insert_function(method->name, method->resolved_type, method);
      }
    }

    impl_scope.symbols[method->name] = type_scope->symbols[method->name];
  }

  for (const auto &method : node->methods) {
    method->declaring_type = target_ty;
    if (!method->generic_parameters.empty() || method->is_extern || method->is_forward_declared) {
      continue;
    }

    visit_function_body(method, false);
  }

  if (trait_ty) {
    auto declaring_node = trait_ty->declaring_node.get();

    if (!declaring_node || declaring_node->get_node_type() != AST_NODE_TRAIT_DECLARATION) {
      throw_error(std::format("\'impl <trait> for <type>\' must implement a trait. got {}", trait_ty->to_string()), node->span);
    }

    auto trait = static_cast<ASTTraitDeclaration *>(declaring_node);
    ctx.scope = trait->scope;

    for (auto trait_method : trait->methods) {
      auto method = (ASTFunctionDeclaration *)deep_copy_ast(trait_method);
      method->declaring_type = target_ty;

      if (auto impl_symbol = impl_scope.local_lookup(method->name)) {
        method->accept(this);
        if (!impl_method_matches_trait(method->resolved_type, impl_symbol->resolved_type)) {
          if (method->resolved_type != Type::INVALID_TYPE && impl_symbol->resolved_type != Type::INVALID_TYPE) {
            throw_error(std::format("method \"{}\" doesn't match trait.\nexpected {},\ngot {}", method->name,
                                    method->resolved_type->to_string(), impl_symbol->resolved_type->to_string()),
                        node->span);
          } else {
            throw_error("internal compiler error: method.type_id or impl_symbol.type_id was null", node->span);
          }
        }
      } else if (!method->is_forward_declared) {
        if (!method->generic_parameters.empty()) {
          type_scope->insert_function(method->name, Type::UNRESOLVED_GENERIC, method);
          impl_scope.symbols[method->name] = type_scope->symbols[method->name];
          continue;
        }

        visit_function_header(method, false, false, {});

        if (auto symbol = type_scope->local_lookup(method->name)) {
          if (!symbol->is_forward_declared) {
            throw_error("Redefinition of method", method->span);
          } else {
            symbol->is_forward_declared = false;
          }
        } else {
          if (method->is_forward_declared) {
            type_scope->forward_declare_function(method->name, method->resolved_type, method);
          } else {
            type_scope->insert_function(method->name, method->resolved_type, method);
          }
          impl_scope.symbols[method->name] = type_scope->symbols[method->name];
          if (method->is_extern || method->is_forward_declared) {
            continue;
          }
        }
        visit_function_body(method, false);
      } else {
        throw_error(
            std::format("required method \"{}\" (from trait {}) not implemented in impl", method->name, trait_ty->to_string()),
            node->span);
      }
    }

    for (auto &[name, impl_sym] : impl_scope.symbols) {
      if (!trait->scope->local_lookup(name)) {
        throw_error(std::format("impl method \"{}\" not found in trait", name), node->span);
      }
    }

    target_ty->traits.push_back(trait_ty);
  }

  node->resolved_type = target_ty;

  if (target_ty->is_kind(TYPE_TRAIT)) {
    for (Type *type : type_table) {
      if (type->implements(target_ty)) {
        // ! ????????
        // We patch up any already-implemented types when adding a method to a trait.
        // You can't add new trait methods, just defaults.
        node->target->resolved_type = type;
        node->accept(this);
      }
    }
  }
}

void Typer::visit_trait_declaration(ASTTraitDeclaration *node, bool generic_instantiation, std::vector<Type *> generic_args) {
  bool was_forward_declared = false;
  auto id = ctx.scope->find_type_id(node->name, {});

  if (id != Type::INVALID_TYPE) {
    auto type = id;
    if (type->is_kind(TYPE_TRAIT)) {
      auto info = type->info->as<TraitTypeInfo>();
      was_forward_declared = info->is_forward_declared;
      if (!generic_instantiation && !was_forward_declared) {
        throw_error("re-definition of trait type.", node->span);
      }
      info->is_forward_declared = false;
    } else {
      throw_error("re-definition of a type", node->span);
    }
  }

  auto previous = ctx.scope;
  Defer _([&] { ctx.set_scope(previous); });
  ctx.set_scope(node->scope);

  Type *type = nullptr;
  if (was_forward_declared) {
    type = ctx.scope->find_type_id(node->name, {});
  } else {
    type = global_create_trait_type(node->name, ctx.scope, generic_args);
    type->info->as<TraitTypeInfo>()->is_forward_declared = node->is_forward_declared;
  }

  if (auto symbol = ctx.scope->lookup(node->name)) {
    type->generic_base_type = symbol->resolved_type;
  }

  if (generic_instantiation) {
    auto generic_arg = generic_args.begin();
    for (const auto &param : node->generic_parameters) {
      auto type = *generic_arg;
      ctx.scope->create_type_alias(param.identifier, *generic_arg, type->declaring_node.get());
      generic_arg++;
    }
  }

  Type *attribute_feature_type = type;
  if (generic_instantiation) {
    attribute_feature_type = type->generic_base_type;
  }
  fetch_compiler_features_for_trait_declaration(node, attribute_feature_type);

  node->scope->name = node->name.str() + mangled_type_args(generic_args);

  type->declaring_node = node;
  node->resolved_type = type;
}

void Typer::compiler_mock_associated_function_call_visit_impl(Type *left_type, const InternedString &method_name) {
  ASTCall call;
  ASTArguments arguments;
  call.arguments = &arguments;

  // Type.
  ASTPath path;
  path.push_segment(left_type->basename);
  path.push_segment(method_name);

  call.callee = &path;
  call.accept(this);
}

void Typer::compiler_mock_method_call_visit_impl(Type *left_type, const InternedString &method_name) {
  ASTMethodCall call;
  ASTArguments arguments;
  call.arguments = &arguments;

  // Type.
  ASTPath path;
  static int depth = 0;

  InternedString varname = "$$temp$$" + std::to_string(depth++);
  path.push_segment(varname);
  ctx.scope->insert_local_variable(varname, left_type, nullptr, MUT);
  Defer erase_temp_symbol([&] {
    depth--;
    ctx.scope->erase("$$temp$$");
  });

  // .method
  ASTDotExpr dot;
  dot.base = &path;
  dot.member = ASTPath::Segment::Identifier(method_name);

  call.callee = &dot;
  call.accept(this);
}

bool is_const_pointer(ASTNode *node) {
  if (node == nullptr) return false;

  if (auto index = dynamic_cast<ASTIndex *>(node)) {
    return is_const_pointer(index->base);
  } else if (auto dot = dynamic_cast<ASTDotExpr *>(node)) {
    return is_const_pointer(dot->base);
  }

  auto type = node->resolved_type;
  if (type->is_const_pointer()) {
    return true;
  }

  return false;
}

void Typer::type_check_args_from_params(ASTArguments *node, ASTParamsDecl *params, ASTFunctionDeclaration *function,
                                        Nullable<ASTExpr> self_nullable, bool is_deinit_call) {
  // this just stores the old type and restores it when we exit.
  // the actual expected type is set per parameter.
  ENTER_EXPECTED_TYPE(nullptr);
  auto params_ct = params->params.size();
  size_t param_index = self_nullable.is_not_null() ? 1 : 0;

  {  // Check the other parameters, besides self.
    for (size_t arg_index = 0; arg_index < node->arguments.size() || param_index < params_ct; ++arg_index, ++param_index) {
      if (param_index < params_ct) {
        auto &param = params->params[param_index];

        if (arg_index < node->arguments.size()) {
          // Argument provided, type-check it
          expected_type = param->resolved_type;

          ASTExpr *arg = node->arguments[arg_index];
          if (arg->get_node_type() == AST_NODE_UNPACK) {
            node->arguments.erase(node->arguments.begin() + arg_index);
            arg_index--;
            param_index--;
            arg->accept(this);
            continue;
          }

          arg->accept(this);

          assert_types_can_cast_or_equal(
              arg, param->resolved_type, arg->span,
              std::format("unexpected argument type.. parameter #{} of function",
                          arg_index + 1));  // +1 here to make it 1-based indexing for user. more intuitive

        } else if (param->normal.default_value) {
          auto old_scope = ctx.scope;
          ctx.scope = function->scope;
          // No argument provided, use the default value
          expected_type = param->resolved_type;
          param->normal.default_value.get()->accept(this);  // Type-check the default value
          ctx.scope = old_scope;
        } else {
          // No argument provided and no default value, throw an error
          std::stringstream ss;
          ss << "Too few arguments to function. Expected:\n  fn(";
          for (auto param : params->params) {
            if (param->tag == ASTParamDecl::Normal) {
              ss << param->normal.name.str() << ": " << param->normal.type->resolved_type->to_string();
              ss << ", ";
            } else {
              ss << (param->self.is_pointer ? "*" : "") << "self, ";
            }
          }
          ss << ")\nbut got:\n";
          ss << "  fn(";

          for (auto arg : node->arguments) {
            ss << arg->resolved_type->to_string() << ", ";
          }
          ss << ")\n";
          throw_error(ss.str(), node->span);
        }
      } else {
        if (!params->is_varargs) {
          // Too many arguments
          std::stringstream ss;
          ss << "Too many arguments to function. Expected:\n  fn(";
          for (auto param : params->params) {
            if (param->tag == ASTParamDecl::Normal) {
              ss << param->normal.name.str() << ": " << param->normal.type->resolved_type->to_string();
              ss << ", ";
            } else {
              ss << (param->self.is_pointer ? "*" : "") << "self, ";
            }
          }
          ss << ")\nbut got:\n  fn(";

          for (auto arg : node->arguments) {
            if (type_is_valid(arg->resolved_type)) {
              ss << arg->resolved_type->to_string() << ", ";
            } else {
              ss << "(null), ";
            }
          }
          ss << ")\n";
          throw_error(ss.str(), node->span);
        }

        ASTExpr *arg = node->arguments[arg_index];
        if (arg->get_node_type() == AST_NODE_UNPACK) {
          node->arguments.erase(node->arguments.begin() + arg_index);
          arg_index--;
          param_index--;
          arg->accept(this);
          continue;
        }

        expected_type = Type::INVALID_TYPE;
        node->arguments[arg_index]->accept(this);
      }
    }
  }

  /*
    We use some strange semantics for destroy.
    It is technically a mutating function, but simply declaring a string that you later want
    to destroy would require mut EVERYWHERE.

    So, we compromise, and allow constant variables and pointers to be passed to destroy calls,
    just for QOL.
  */
  if (self_nullable.is_not_null() && !is_deinit_call) {
    auto self = self_nullable.get();
    auto first = params->params[0]->resolved_type;
    auto self_symbol = ctx.get_symbol(self);
    auto self_type = self->resolved_type;

    if (first->is_mut_pointer()) {
      if (!self_type->is_pointer() && self_symbol && self_symbol.get()->is_const()) {
        throw_error("cannot call a '*mut self' method with an immutable variable, consider adding 'mut' to the declaration.",
                    node->span);
      }
      if (is_const_pointer(self)) {
        throw_error(
            "cannot call a '*mut self' method with an immutable pointer, consider taking it as '&mut' (or however "
            "you obtained this pointer)",
            node->span);
      }
    }
  }
}

void Typer::type_check_args_from_info(ASTArguments *node, FunctionTypeInfo *info) {
  ENTER_EXPECTED_TYPE(nullptr);
  // Process arguments dynamically because unpack expressions (`...expr`) expand into
  // multiple arguments at visit time. We therefore must not cache the initial
  // argument count.
  size_t i = 0;
  while (i < node->arguments.size()) {
    auto arg = node->arguments[i];

    // Handle unpack nodes by erasing the placeholder and visiting it which will
    // push `ASTUnpackElement` nodes into `node->arguments` at the current
    // insertion point via `current_expression_list` (see `visit(ASTUnpackExpr)`).
    if (arg->get_node_type() == AST_NODE_UNPACK) {
      node->arguments.erase(node->arguments.begin() + i);
      arg->accept(this);
      // Do not advance `i` because new elements were inserted at position `i`.
      continue;
    }

    if (i >= info->params_len) {
      // We've exceeded the declared parameters.
      if (!info->is_varargs) {
        std::stringstream ss;
        ss << "Function call has incorrect number of arguments. Expected: " << info->params_len
           << ", Found: " << node->arguments.size() << "... function type: " << info->to_string();
        throw_error(ss.str(), node->span);
      }

      // Varargs: accept remaining args without type checks.
      expected_type = Type::INVALID_TYPE;
      arg->accept(this);
      ++i;
      continue;
    }

    // Normal parameter position — type-check against parameter type.
    expected_type = info->parameter_types[i];
    arg->accept(this);
    assert_types_can_cast_or_equal(arg, info->parameter_types[i], arg->span,
                                   std::format("invalid argument type for parameter #{}", i + 1));

    ++i;
  }

  // After expanding unpack expressions, ensure we have at least the required parameters.
  if (node->arguments.size() < info->params_len) {
    std::stringstream ss;
    ss << "Function call has incorrect number of arguments. Expected: " << info->params_len
       << ", Found: " << node->arguments.size() << "... function type: " << info->to_string();
    throw_error(ss.str(), node->span);
  }
}

/*
  ! When you provide the wrong number of arguments (or at least too few) to a generic function that is inferring it's
  genericsd, ! the compiler just crashes and doesn't report an error REPRO: 103
*/
ASTFunctionDeclaration *Typer::resolve_generic_function_call(ASTFunctionDeclaration *func, std::vector<ASTExpr *> *generic_args,
                                                             ASTArguments *arguments, Span span) {
  if (generic_args->empty()) {
    // infer generic parameter (return type only) from expected type
    if (arguments->arguments.empty() && func->generic_parameters.size() == 1) {
      if (func->return_type->kind == ASTType::NORMAL && type_is_valid(expected_type)) {
        auto return_ty_path = func->return_type->normal.path;
        /*
          TODO: we need to do a better job of checking whether the return type name

          TODO: this is not a todo it's a massive BUG
          ! BUG
          This is freaking horrible. get rid of this STAT.

          * matches the generic parameter it's trying to infer.
          * we shouldn't restrict the path length nor should we be using a simple string comparison here.
        */
        if (return_ty_path->length() == 1 && return_ty_path->segments[0].tag == ASTPath::Segment::IDENTIFIER &&
            (func->generic_parameters[0].identifier == return_ty_path->segments[0].get_identifier())) {
          auto type = ast_alloc<ASTType>();
          type->resolved_type = expected_type;
          type->span = span;
          if (!type_is_valid(type->resolved_type)) {
            throw_error("INTERNAL COMPILER ERROR: Invalid type during inference", type->span);
          }
          generic_args->push_back(type);
        }
      }

    } else {  // Infer generic parameter(S) from arguments.

      /*
        ! This is the cause of repro 106.
        ! I don't quite know  how to fix it, but somehow we need to
        ! at least unset the expected type here? that will be a temporary fix.
      */
      {
        auto old_expected = expected_type;
        expected_type = Type::INVALID_TYPE;
        arguments->accept(this);
        expected_type = old_expected;
      }

      auto args = arguments->resolved_argument_types;
      auto parameters = func->params->params;
      auto generics = func->generic_parameters;

      // TODO: I added this little ternary to stop crashes but this whole thing needs a rework if this was commonly
      // going out of bounds, which it was.
      std::vector<std::pair<bool, int>> arg_to_generic_map(parameters.size() > args.size() ? parameters.size() : args.size());

      for (size_t i = 0; i < parameters.size(); ++i) {
        auto &param = parameters[i];
        switch (param->tag) {
          case ASTParamDecl::Self: {
            arg_to_generic_map[i] = {false, -1};
          } break;
          case ASTParamDecl::Normal: {
            bool is_generic = false;
            int generic_index = 0;

            for (const auto &generic : generics) {
              // ! This condition is terrible
              // ! Update: this condition is truly awful. FIXME. had to patch this because it crashes the compiler.
              if (param->normal.type->kind == ASTType::NORMAL &&
                  param->normal.type->normal.path->segments[0].tag == ASTPath::Segment::IDENTIFIER &&
                  generic.identifier == param->normal.type->normal.path->segments[0].get_identifier()) {
                is_generic = true;
                break;
              }
              ++generic_index;
            }

            arg_to_generic_map[i] = {is_generic, generic_index};
          } break;
        }
      }

      std::vector<Type *> inferred_generics(generics.size(), Type::INVALID_TYPE);

      auto start_index = (parameters[0]->tag == ASTParamDecl::Self) ? 1 : 0;

      for (size_t i = 0; i < args.size(); ++i) {
        auto arg_ty_id = args[i];

        if (i + start_index >= arg_to_generic_map.size()) {
          break;
        }

        auto [is_generic, generic_index] = arg_to_generic_map[i + start_index];

        if (is_generic) {
          auto extensions = parameters[i + start_index]->normal.type->extensions;

          int pointer_levels = 0;

          while (!extensions.empty() &&
                 (extensions.back().type == TYPE_EXT_POINTER_CONST || extensions.back().type == TYPE_EXT_POINTER_MUT)) {
            pointer_levels++;
            extensions.pop_back();
          }

          auto type = arg_ty_id;
          while (pointer_levels > 0 && type->is_pointer()) {
            arg_ty_id = type->get_element_type();
            type = arg_ty_id;
            pointer_levels--;
          }

          inferred_generics[generic_index] = arg_ty_id;
        }
      }

      for (size_t i = 0; i < generics.size(); ++i) {
        auto type = ast_alloc<ASTType>();
        type->span = span;
        if (!type_is_valid(inferred_generics[i])) {
          throw_error("failed in inferring type argument from function argument list", arguments->span);
        }
        type->resolved_type = inferred_generics[i];
        generic_args->push_back(type);
      }
    }
  }
  auto resolved_generic_args = get_generic_arg_types(*generic_args);
  auto instantiation = (ASTFunctionDeclaration *)visit_generic(func, resolved_generic_args, span);
  if (generic_args->size() != resolved_generic_args.size()) {
    generic_args->resize(resolved_generic_args.size());
  }
  auto generic_arg = generic_args->begin();
  for (auto resolved_generic_arg : resolved_generic_args) {
    if (*generic_arg == nullptr) {
      auto type = ast_alloc<ASTType>();
      type->span = span;
      type->resolved_type = resolved_generic_arg;
      if (!type_is_valid(type->resolved_type)) {
        throw_error("INTERNAL COMPILER ERROR: Invalid type during inference", type->span);
      }
      *generic_arg = type;
    }
    generic_arg++;
  }
  return instantiation;
}

ASTDeclaration *Typer::visit_generic(ASTDeclaration *definition, std::vector<Type *> &args, Span span) {
#ifdef USE_GENERIC_PANIC_HANDLER
  GenericInstantiationErrorUserData data;
  set_panic_handler(generic_instantiation_panic_handler);
  set_error_user_data(&data);
  Defer defer_1([] { reset_panic_handler(); });
  if (_setjmp(data.save_state) == 0) {
#endif
    if (definition->generic_parameters.size() < args.size()) {
      throw_error(
          std::format("too many generic arguments. expected {}, got {}", definition->generic_parameters.size(), args.size()),
          definition->span);
    } else if (definition->generic_parameters.size() > args.size()) {
      args.resize(definition->generic_parameters.size(), nullptr);
      auto arg = args.begin();
      for (auto param : definition->generic_parameters) {
        if (*arg == nullptr) {
          if (param.default_value) {
            *arg = param.default_value->resolved_type;
          } else {
            throw_error(
                std::format("too few generic arguments. expected {}, got {}", definition->generic_parameters.size(), args.size()),
                definition->span);
          }
        }
        arg++;
      }
    }
    auto instantiation = find_generic_instance(definition->generic_instantiations, args);
    if (!instantiation) {
      instantiation = static_cast<ASTDeclaration *>(deep_copy_ast(definition));
      definition->generic_instantiations.emplace_back(args, instantiation);
      switch (definition->get_node_type()) {
        case AST_NODE_STRUCT_DECLARATION: {
          visit_struct_declaration((ASTStructDeclaration *)instantiation, true, args);
        } break;
        case AST_NODE_FUNCTION_DECLARATION: {
          visit_function_header((ASTFunctionDeclaration *)instantiation, true, true, args);
          auto func = static_cast<ASTFunctionDeclaration *>(instantiation);
          func->generic_arguments = args;
          visit_function_body(func, false);
        } break;
        case AST_NODE_TRAIT_DECLARATION:
          visit_trait_declaration((ASTTraitDeclaration *)instantiation, true, args);
          break;
        case AST_NODE_CHOICE_DECLARATION: {
          visit_choice_declaration((ASTChoiceDeclaration *)instantiation, true, args);
        } break;
        case AST_NODE_IMPL: {
          visit_impl_declaration((ASTImpl *)instantiation, true, args);
        } break;
        default:
          throw_error("Invalid target to generic args", span);
          break;
      }
      for (auto impl : instantiation->impls) {
        if (impl->resolved_type == Type::INVALID_TYPE) {
          visit_generic(impl, args, span);
        }
      }
      instantiation->generic_parameters.clear();
      instantiation->generic_instantiations.clear();
    }
    return instantiation;
#ifdef USE_GENERIC_PANIC_HANDLER
  } else {
    handle_generic_error(&data, span);
    return nullptr;
  }
#endif
}

std::vector<TypeExtension> Typer::accept_extensions(std::vector<ASTTypeExtension> ast_extensions) {
  std::vector<TypeExtension> extensions;
  for (auto &ext : ast_extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      auto val = interpret_from_ast(ext.expression, ctx);
      if (val->get_value_type() != ValueType::INTEGER) {
        throw_error("Fixed array must have integer size.", ext.expression->span);
      }
      extensions.push_back({ext.type, val->as<IntValue>()->value});
    } else {
      extensions.push_back({.type = ext.type});
    }
  }
  return extensions;
}

std::vector<Type *> Typer::get_generic_arg_types(const std::vector<ASTExpr *> &args) {
  std::vector<Type *> generic_args;
  for (const auto &arg : args) {
    arg->accept(this);
    generic_args.push_back(arg->resolved_type);
  }
  return generic_args;
}

void Typer::visit(ASTProgram *node) {
  ctx.set_scope(ctx.root_scope);
  size_t index = 0;
  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
    }
    statement->accept(this);
    index++;
  }
  ctx.set_scope(ctx.root_scope);
}

void Typer::visit(ASTChoiceDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    for (auto param : node->generic_parameters) {
      if (param.default_value) {
        param.default_value->accept(this);
      }
    }
    ctx.scope->create_type_alias(node->name, Type::UNRESOLVED_GENERIC, node);
    fetch_compiler_features_for_choice_declaration_generic(node);
    return;
  }
  visit_choice_declaration(node, false);
}

void Typer::visit(ASTLambda *node) {
  node->unique_identifier = "$lambda$" + std::to_string(lambda_unique_id++);
  node->params->accept(this);
  node->return_type->accept(this);
  ENTER_EXPECTED_TYPE(node->return_type->resolved_type);
  std::vector<int> param_types;
  FunctionTypeInfo info;

  int parameter_index = 0;
  for (const auto &param : node->params->params) {
    info.parameter_types[parameter_index] = param->resolved_type;
    info.params_len++;
    node->block->scope->insert_local_variable(param->normal.name, param->resolved_type, nullptr, param->mutability, param);
    parameter_index++;
  }

  node->block->accept(this);
  info.return_type = node->return_type->resolved_type;
  auto type = global_find_function_type_id(info, {});
  node->resolved_type = type->take_pointer_to(MUT);

  ctx.scope->insert_variable(node->unique_identifier, type, node, MUT);
}

void Typer::visit(ASTStructDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    for (auto param : node->generic_parameters) {
      if (param.default_value) {
        param.default_value->accept(this);
      }
    }
    ctx.scope->create_type_alias(node->name, Type::UNRESOLVED_GENERIC, node);
    fetch_compiler_features_for_struct_declaration_generic(node);
  } else {
    visit_struct_declaration(node, false);
    assess_and_try_add_blittable_trait(node->resolved_type);
  }
}

void Typer::visit(ASTEnumDeclaration *node) {
  if (ctx.scope->find_type_id(node->name, {}) != Type::INVALID_TYPE) {
    throw_error("Redefinition of enum " + node->name.str(), node->span);
  }

  auto underlying_type = Type::INVALID_TYPE;

  if (node->underlying_type_ast) {
    node->underlying_type_ast->accept(this);
    underlying_type = node->underlying_type_ast->resolved_type;
  }

  auto enum_type = ctx.scope->create_enum_type(node->name, create_child(ctx.scope), node->is_flags, node);
  enum_type->declaring_node = node;
  auto info = enum_type->info->as<EnumTypeInfo>();

  for (auto &[key, value] : node->key_values) {
    value->accept(this);
    Type *node_ty = value->resolved_type;

    // We need to adjust the underlying type for larger-than-default (s32) values.
    // C will infer this, but we need to be explicit and not rely on any target language stuff.
    // we upcast to unsigned to get the largest maximum value for huge values

    Value *evaluated = interpret_from_ast(value, ctx);
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

    info->scope->insert_variable(key, node_ty, value, CONST);
    info->members.push_back({key, node_ty, value, nullptr});
    if (underlying_type == Type::INVALID_TYPE) {
      underlying_type = node_ty;
    } else {
      assert_types_can_cast_or_equal(value, underlying_type, node->span, "inconsistent types in enum declaration.");
    }
  }

  if (underlying_type == void_type()) {
    throw_error("Invalid enum declaration.. got null or no type.", node->span);
  }

  node->underlying_type = underlying_type;
  info->underlying_type = underlying_type;
  node->resolved_type = enum_type;
}

void Typer::visit(ASTFunctionDeclaration *node) {
  for (auto attr : node->attributes) {
    if (attr.tag == ATTRIBUTE_INLINE) {
      node->is_inline = true;
    }
  }

  // TODO: actually generate a signature for a generic function so that you can compare them
  if (!node->generic_parameters.empty()) {
    for (auto param : node->generic_parameters) {
      if (param.default_value) {
        param.default_value->accept(this);
      }
    }
    ctx.scope->insert_function(node->name, Type::UNRESOLVED_GENERIC, node);
    return;
  }

  visit_function_header(node, true, false, {});

  fetch_compiler_features_for_function_declaration(node);

  if (node->is_forward_declared) {
    ctx.scope->forward_declare_function(node->name, node->resolved_type, node);
    return;
  }

  ctx.scope->insert_function(node->name, node->resolved_type, node);

  if (node->is_extern) {
    return;
  }

  visit_function_body(node, false);
}

void Typer::visit(ASTVariable *node) {
  // Inferred declaration.
  if (node->type == nullptr) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      throw_error("Cannot use a type as a value.", node->value.get()->span);
    }
    node->value.get()->accept(this);
    auto value_ty = node->value.get()->resolved_type;
    if (value_ty == void_type()) {
      throw_error("Cannot assign a variable with value type of 'void'", node->span);
    }
    auto type = value_ty;

    // CLEANUP: This is nonsense.
    node->type = ast_alloc<ASTType>();
    node->type->span = node->span;
    node->type->resolved_type = value_ty;
    node->resolved_type = value_ty;

    // TODO: so, we just don't set the type if it can't be assigned to int??? what?
    if (type->is_kind(TYPE_SCALAR) && type->has_no_extensions() && expr_is_literal(node->value.get())) {
      auto info = (type->info->as<ScalarTypeInfo>());
      auto rule = type_conversion_rule(type, s32_type(), node->span);
      if (info->is_integral && rule != CONVERT_PROHIBITED && rule != CONVERT_EXPLICIT) {
        node->type->resolved_type = s32_type();
      }
    }
  }

  if (ctx.scope->find_type_id(node->name, {}) != Type::INVALID_TYPE || keywords.contains(node->name.str())) {
    throw_error(
        "Invalid variable declaration: a type or keyword exists with "
        "that name,",
        node->span);
  }

  fetch_compiler_features_for_varaible_declaration(node);

  node->type->accept(this);

  if (node->type->resolved_type == Type::INVALID_TYPE) {
    throw_error("Declaration of a variable with a non-existent type.", node->span);
  }

  if (node->type->resolved_type->is_kind(TYPE_TRAIT)) {
    throw_error(
        "You cannot have a variable of a 'trait' type -- it is sizeless and only used at compile time to enforce "
        "rules.",
        node->span);
  }

  if (node->value.is_not_null()) {
    if (node->value.get()->get_node_type() == AST_NODE_TYPE) {
      throw_error("Cannot use a type as a value.", node->value.get()->span);
    }

    ENTER_EXPECTED_TYPE(node->type->resolved_type);
    node->value.get()->accept(this);
    assert_types_can_cast_or_equal(node->value.get(), node->type->resolved_type, node->span, "invalid type in declaration");
  }

  if (!node->is_constexpr && ctx.scope->local_lookup(node->name)) {
    throw_error(std::format("re-definition of '{}'", node->name), node->span);
  }

  auto variable_type = node->type->resolved_type;

  if (node->is_local) {
    ctx.scope->insert_local_variable(node->name, variable_type, node->value.get(), node->mutability, node);
  } else {
    ctx.scope->insert_variable(node->name, variable_type, node->value.get(), node->mutability, node);
  }

  if (variable_type == void_type()) {
    throw_error(std::format("cannot assign variable to type 'void' :: {}", node->name.str()), node->span);
  }

  if (node->is_constexpr) {
    // TODO: we should probably improve this.
    // Our interpreter can't handle structs, but we want structs.
    // auto type = node->type->resolved_type;
    // if ((!type->is_kind(TYPE_SCALAR) || type->extensions.has_extensions())) {
    //   throw_error(std::format("Can only use scalar types (integers, floats, "
    //                           "bools) as constant expressions, got {}",
    //                           type->to_string()),
    //               node->value.get()->span);
    // }
  }
}

void Typer::visit(ASTBlock *node) {
  auto old_scope = ctx.scope;

  Defer _([&] { ctx.scope = old_scope; });
  ctx.set_scope(node->scope);

  node->control_flow.type = Type::INVALID_TYPE;
  for (auto &statement : node->statements) {
    statement->accept(this);
    auto &stmnt_cf = statement->control_flow;

    // Handle 'no_return' calls.
    // TODO: probably add a never type so the analysis here is much cheaper.
    if (statement->get_node_type() == AST_NODE_EXPR_STATEMENT) {
      auto expr_stmt = (ASTExprStatement *)statement;
      if (expr_stmt->expression->get_node_type() == AST_NODE_CALL) {
        auto call = (ASTCall *)expr_stmt->expression;
        auto symbol = ctx.get_symbol(call->callee).get();
        if (!symbol || !symbol->is_function || !symbol->function.declaration) {
          continue;
        }
        auto &function = symbol->function;
        for (auto attr : function.declaration->attributes) {
          if (attr.tag == ATTRIBUTE_NO_RETURN) {
            stmnt_cf = {BLOCK_FLAGS_RETURN, expected_type};
            break;
          }
        }
      }
    }

    auto &block_cf = node->control_flow;
    block_cf.flags |= stmnt_cf.flags;

    if (DOESNT_HAVE_FLAG(stmnt_cf.flags, BLOCK_FLAGS_FALL_THROUGH)) {
      block_cf.flags &= ~BLOCK_FLAGS_FALL_THROUGH;
      block_cf.type = stmnt_cf.type;
    }
  }

  node->resolved_type = node->control_flow.type;
}

void Typer::visit(ASTParamsDecl *node) {
  for (auto &param : node->params) {
    param->accept(this);
  }
  return;
}

void Typer::visit(ASTParamDecl *node) {
  if (node->tag == ASTParamDecl::Self) {
    if (!type_context) {
      throw_error("No target type for self", node->span);
    }
    node->resolved_type = get_self_type();
    if (node->self.is_pointer) {
      node->resolved_type = node->resolved_type->take_pointer_to(node->mutability);
    }
    return;
  } else {
    node->normal.type->accept(this);
    Type *param_type = node->normal.type->resolved_type;
    node->resolved_type = param_type;

    if (param_type == Type::INVALID_TYPE) {
      throw_error("Use of undeclared type.", node->span);
    }

    if (node->normal.default_value) {
      auto old_type = expected_type;
      expected_type = param_type;
      node->normal.default_value.get()->accept(this);
      expected_type = old_type;
    }
  }
}

void Typer::visit(ASTReturn *node) {
  Type *type;

  if (node->expression.is_not_null()) {
    ASTExpr *expr = node->expression.get();
    expr->accept(this);
    type = expr->resolved_type;

    if (expected_type != Type::INVALID_TYPE && expected_type != void_type()) {
      assert_types_can_cast_or_equal(expr, expected_type, node->span, "invalid return type");
    }

    if (expr->get_node_type() == AST_NODE_PATH) {
      // since there hasnt been an error (we got to this point), we can assume this exists.
      Symbol *symbol = ctx.get_symbol(expr).get();

      bool is_static_var = false;

      if (symbol->is_variable && symbol->variable.declaration.is_not_null()) {
        ASTVariable *variable = (ASTVariable *)symbol->variable.declaration.get();
        is_static_var = variable->is_static;
      }

      if (symbol->resolved_type->is_fixed_sized_array() && symbol->is_local && !is_static_var) {
        throw_warning(WARNING_RETURNING_ARRAY,
                      "returning a non-static fixed array from a function can lead to unexpected behaviour, as it's "
                      "allocated on the functions stack, and becomes invalid upon return immediately.",
                      node->span);
      }
    }
  } else {
    type = ctx.scope->find_type_id("void", {});
  }
  node->resolved_type = type;
  node->control_flow = {BLOCK_FLAGS_RETURN, type};
}

void Typer::visit(ASTContinue *node) { node->control_flow = {BLOCK_FLAGS_CONTINUE, Type::INVALID_TYPE}; }

void Typer::visit(ASTBreak *node) { node->control_flow = {BLOCK_FLAGS_BREAK, Type::INVALID_TYPE}; }

void Typer::visit(ASTFor *node) {
  auto old_scope = ctx.scope;
  ctx.set_scope(node->block->scope);

  node->right->accept(this);
  Type *iterable_type_id = node->right->resolved_type;
  Type *iterable_type = iterable_type_id;
  node->iterable_type = iterable_type;

  if (iterable_type->is_pointer()) {
    throw_error(std::format("Cannot iterate over a pointer. Did you mean to dereference a "
                            "pointer to an array, range or struct? got type {}",
                            iterable_type->to_string()),
                node->span);
  }

  Type *iter_ty = Type::INVALID_TYPE;
  auto scope = iterable_type->info->scope;

  if (iterable_type->implements(g_Iterable_trait_type)) {  // can return an iterator.
    node->iteration_kind = ASTFor::ITERABLE;

    compiler_mock_method_call_visit_impl(iterable_type_id, "iter");
    auto symbol = scope->local_lookup("iter");
    auto symbol_ty = symbol->resolved_type;
    auto iter_return_ty = symbol_ty->info->as<FunctionTypeInfo>()->return_type;
    node->iterator_type = iter_return_ty;

    // make sure the impl is actually emitted if this is generic.
    compiler_mock_method_call_visit_impl(iter_return_ty, "next");
    symbol = iter_return_ty->info->scope->local_lookup("next");
    iter_ty = symbol->resolved_type->info->as<FunctionTypeInfo>()->return_type;
    auto option = iter_ty;
    iter_ty = option->generic_args[0];
  } else if (iterable_type->implements(g_Iterator_trait_type)) {  // directly an iterator.
    node->iteration_kind = ASTFor::ITERATOR;
    node->iterator_type = iterable_type_id;

    // make sure the impl is actually emitted if this is generic.
    auto iterable_type = iterable_type_id;
    compiler_mock_method_call_visit_impl(iterable_type_id, "next");
    auto symbol = iterable_type->info->scope->local_lookup("next");
    iter_ty = symbol->resolved_type->info->as<FunctionTypeInfo>()->return_type;
    auto option = iter_ty;
    iter_ty = option->generic_args[0];
  } else {
    throw_error(
        "cannot iterate with for-loop on a type that doesn't implement either the 'Iterable!<T>' or the "
        "'Iterator!<T>' trait. ",
        node->span);
  }

  node->identifier_type = iter_ty;

  if (node->left_tag == ASTFor::IDENTIFIER) {
    auto iden = node->left.identifier;
    ctx.scope->insert_local_variable(iden, iter_ty, nullptr, CONST);
  } else {
    auto type = iter_ty;

    // if our iterable type returns a pointer, we "dereference" here because of the way destructuring needs to be done
    // it doesn't actually get generated as a dereference but we need to analyze the scope of the base type.
    if (type->is_pointer()) {
      type = type->get_element_type();
    }

    auto members = type->info->members;
    if (node->left.destructure.size() != members.size()) {
      throw_error(std::format("Cannot currently partially deconstruct a struct. "
                              "expected {} identifiers to assign, got {}, for type {}",
                              members.size(), node->left.destructure.size(), type->to_string()),
                  node->span);
    }
    int i = 0;
    for (auto [name, type_id, _, __] : members) {
      auto &destructure = node->left.destructure[i];
      auto iden = destructure.identifier;
      if (destructure.semantic == VALUE_SEMANTIC_POINTER_MUT) {
        type_id = type_id->take_pointer_to(MUT);
      } else if (destructure.semantic == VALUE_SEMANTIC_POINTER_CONST) {
        type_id = type_id->take_pointer_to(CONST);
      }
      destructure.type = type_id;
      ctx.scope->insert_local_variable(iden, type_id, nullptr, MUT);
      i++;
    }
  }

  node->right->accept(this);

  ctx.scope = old_scope;
  node->block->accept(this);

  auto control_flow = node->block->control_flow;
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  node->control_flow = control_flow;
}

void Typer::visit(ASTIf *node) {
  auto condition = node->condition;
  if (condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
    auto pattern = (ASTPatternMatch *)condition;
    pattern->target_block = node->block;
    condition->accept(this);
  } else {
    condition->accept(this);
  }

  auto cond_ty = node->condition->resolved_type;
  auto conversion_rule = type_conversion_rule(cond_ty, bool_type());

  if (conversion_rule == CONVERT_PROHIBITED) {
    throw_error(std::format("cannot convert 'if' condition to a boolean, implicitly nor explicitly. got type \"{}\"",
                            cond_ty->to_string()),
                node->span);
  }

  node->block->accept(this);
  auto control_flow = node->block->control_flow;

  if (node->_else.is_not_null()) {
    auto _else = node->_else.get();
    _else->accept(this);
    auto else_cf = _else->control_flow;
    control_flow.flags |= else_cf.flags;
  } else {
    control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  }

  node->control_flow = control_flow;
  node->resolved_type = control_flow.type;

  if ((node->resolved_type == nullptr || node->resolved_type == void_type()) && node->is_expression) {
    throw_error("'if' expressions must return a value that's non-void", node->condition->span);
  }
}

void Typer::visit(ASTElse *node) {
  if (node->_if.is_not_null()) {
    node->_if.get()->accept(this);
    node->control_flow = node->_if.get()->control_flow;
  } else {
    node->block.get()->accept(this);
    node->control_flow = node->block.get()->control_flow;
  }
}

void Typer::visit(ASTWhile *node) {
  if (node->condition.is_not_null()) {
    auto condition = node->condition.get();
    if (condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
      auto pattern = (ASTPatternMatch *)condition;
      pattern->target_block = node->block;
      condition->accept(this);
    } else {
      condition->accept(this);
    }
  }
  node->block->accept(this);
  auto control_flow = node->block->control_flow;
  control_flow.flags &= ~BLOCK_FLAGS_BREAK;
  control_flow.flags &= ~BLOCK_FLAGS_CONTINUE;
  // we add fall through here because we dont know if this will get
  // excecuted since we cant evaluate the condition to know
  control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  node->control_flow = control_flow;
}

void Typer::expand_macro(ASTFunctionDeclaration *macro, ASTCall *call) {
  // TODO: do something, this is super bad. just adding it as a hack feature.
  ENTER_SCOPE(call->declaring_scope);
  macro->scope->parent = call->declaring_scope;
  macro->block.get()->accept(this);
}

void Typer::visit(ASTCall *node) {
  Type *type = nullptr;
  ASTFunctionDeclaration *func_decl = nullptr;
  // Try to find the function via a dot expression, scope resolution, identifier, etc.
  // Otherwise find it via a type resolution, for things like array[10](); or what have you.
  //
  // We have to use a custom path visitor for calls due to the fact that visit(ASTPath *) will try to instantiate
  // a generic function when it's partially defined and doesn't allow the call to try to fill out the rest of
  // the generic args through inference.

  auto old_expression_list = current_expression_list;
  current_expression_list = &node->arguments->arguments;
  Defer _defer([&] { current_expression_list = old_expression_list; });

  if (node->callee->get_node_type() == AST_NODE_PATH) {
    visit_path((ASTPath *)node->callee, true);
  } else {
    node->callee->accept(this);
  }
  auto symbol = ctx.get_symbol(node->callee).get();

  if (symbol && symbol->is_function) {
    if (!type) {
      type = symbol->resolved_type;
    }
    auto declaring_node = symbol->function.declaration;

    if (declaring_node->is_macro) {
      expand_macro(declaring_node, node);
    }

    if (declaring_node && declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      func_decl = static_cast<ASTFunctionDeclaration *>(declaring_node);

      // resolve a generic call.
      if (!func_decl->generic_parameters.empty()) {
        // doing this so self will get the right type when we call generic methods
        // TODO: handle this in the function decl itself, maybe insert self into symbol table
        auto old_type = type_context;
        Defer _([&] { type_context = old_type; });

        ASTType func_type_ast;
        if (func_decl->declaring_type != Type::INVALID_TYPE) {
          func_type_ast.resolved_type = func_decl->declaring_type;
          type_context = &func_type_ast;
        }

        func_decl = resolve_generic_function_call(func_decl, node->get_generic_arguments().get(), node->arguments, node->span);
      }

      if (func_decl->params->has_self) {
        throw_error("cannot call a method as if it was an associated function anymore", node->span);
      }

      type = func_decl->resolved_type;

      // Why did I have to add this, when refactoring the type system??
      node->callee->resolved_type = func_decl->resolved_type;
    }

  } else if (symbol && symbol->is_type) {
    if (!symbol->type.choice) {
      throw_error(
          std::format("type {} must be a choice variant to use '(..)' constructor for now", symbol->resolved_type->basename),
          node->span);
    }
    if (!symbol->resolved_type->is_kind(TYPE_TUPLE)) {
      throw_error(std::format("type {} must be tuple to use '(..)' constructor", symbol->resolved_type->basename), node->span);
    }
    ASTTuple tuple;
    tuple.values = node->arguments->arguments;
    ENTER_EXPECTED_TYPE(symbol->resolved_type);
    tuple.accept(this);
    node->resolved_type = symbol->type.choice.get()->resolved_type;
    return;
  } else {
    type = node->callee->resolved_type;
  }

  if (!type) {
    throw_error("use of undeclared function", node->span);
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("unable to call a non-function, got {}", type->to_string()), node->span);
  }

  auto info = type->info->as<FunctionTypeInfo>();

  // If we have the declaring node representing this function, type check it against the parameters in that definition.
  // else, use the type.
  if (func_decl) {
    type_check_args_from_params(node->arguments, func_decl->params, func_decl, nullptr, func_decl->name == "destroy");
  } else {
    type_check_args_from_info(node->arguments, info);
  }

  node->resolved_type = info->return_type;
}

void Typer::visit(ASTArguments *node) {
  auto type = expected_type;
  FunctionTypeInfo *info = nullptr;
  if (type) {
    info = dynamic_cast<FunctionTypeInfo *>(type->info);
  }
  for (size_t i = 0; i < node->arguments.size(); ++i) {
    auto arg = node->arguments[i];

    /*
      TODO: let's add a more graceful way to do this
    */
    if (arg->get_node_type() == AST_NODE_UNPACK) {
      node->arguments.erase(node->arguments.begin() + i);
      i--;
      arg->accept(this);
      continue;
    }

    if (!info) {
      arg->accept(this);
      node->resolved_argument_types.push_back(arg->resolved_type);
      continue;
    }
    ENTER_EXPECTED_TYPE(info->parameter_types[i]);
    arg->accept(this);
    node->resolved_argument_types.push_back(arg->resolved_type);
  }
}

void Typer::visit(ASTExprStatement *node) {
  node->expression->accept(this);
  switch (node->expression->get_node_type()) {
    case AST_NODE_SWITCH: {
      auto _switch = static_cast<ASTSwitch *>(node->expression);
      node->control_flow = _switch->control_flow;
      node->resolved_type = _switch->resolved_type;
      node->resolved_type = _switch->resolved_type;
      break;
    }
    case AST_NODE_IF: {
      auto _if = static_cast<ASTIf *>(node->expression);
      node->control_flow = _if->control_flow;
      node->resolved_type = _if->resolved_type;
      node->resolved_type = _if->resolved_type;
      break;
    }
    default:
      break;
  }
}

void Typer::visit(ASTType_Of *node) {
  static auto type_ptr = ctx.scope->find_type_id("Type", {{TYPE_EXT_POINTER_CONST}});
  node->target->accept(this);
  node->resolved_type = type_ptr;
}

void Typer::visit(ASTType *node) {
  if (type_is_valid(node->resolved_type)) {
    return;
  }

  TypeExtensions extensions = accept_extensions(node->extensions);

  switch (node->kind) {
    case ASTType::NORMAL: {
      auto &normal_ty = node->normal;
      normal_ty.path->accept(this);

      if (!type_is_valid(normal_ty.path->resolved_type)) {
        throw_error("use of undeclared type", node->span);
        return;
      }

      auto base_ty = normal_ty.path->resolved_type;
      if (!base_ty) {
        if (normal_ty.path->resolved_type == Type::INVALID_TYPE) {
          throw_error("use of undeclared type", node->span);
        } else if (normal_ty.path->resolved_type == Type::UNRESOLVED_GENERIC) {
          throw_error("use of unresolved generic type", node->span);
        }
      }

      node->resolved_type = global_find_type_id(base_ty, extensions);

      if (node->normal.is_dyn) {
        auto type = node->resolved_type;
        auto extension = type->extensions;
        auto ty = ctx.scope->find_or_create_dyn_type_of(type->base_type == Type::INVALID_TYPE ? type : type->base_type,
                                                        node->span, this);
        if (extensions.size()) {
          node->resolved_type = global_find_type_id(ty, extensions);
        } else {
          node->resolved_type = ty;
        }
      }
    } break;
    case ASTType::TUPLE: {
      std::vector<Type *> types;
      for (const auto &t : node->tuple_types) {
        t->accept(this);
        types.push_back(t->resolved_type);
      }
      node->resolved_type = global_find_type_id(types, extensions);
    } break;
    case ASTType::FUNCTION: {
      auto &func = node->function;
      FunctionTypeInfo info;
      // TODO: I don' think is is ever null, ever.
      if (func.return_type.is_not_null()) {
        func.return_type.get()->accept(this);
        info.return_type = func.return_type.get()->resolved_type;
      } else {
        info.return_type = void_type();
      }
      for (auto &param_ty : func.parameter_types) {
        param_ty->accept(this);
        info.parameter_types[info.params_len] = param_ty->resolved_type;
        info.params_len++;
      }
      node->resolved_type = global_find_function_type_id(info, extensions);
    } break;
    case ASTType::SELF: {
      auto self = get_self_type();
      if (self == Type::INVALID_TYPE) {
        throw_error("Cannot locate #self type.", node->span);
      }
      auto self_w_ext = global_find_type_id(self, extensions);
      if (self_w_ext == Type::INVALID_TYPE) {
        throw_error("Cannot locate #self type with extensions", node->span);
      }
      node->resolved_type = self_w_ext;
    } break;
    case ASTType::STRUCTURAL_DECLARATIVE_ASCRIPTION: {
      node->declaration->accept(this);
      node->resolved_type = global_find_type_id(node->declaration->resolved_type, extensions);
    } break;
    default:
      throw_error("internal compiler error: Invalid type kind", node->span);
  }
}

void Typer::visit(ASTBinExpr *node) {
  node->left->accept(this);
  auto left = node->left->resolved_type;

  ENTER_EXPECTED_TYPE(left);

  node->right->accept(this);
  auto right = node->right->resolved_type;

  if (node->op == TType::Assign || ttype_is_comp_assign(node->op)) {
    if (node->left->get_node_type() == AST_NODE_PATH) {
      auto path = (ASTPath *)node->left;
      if (path->length() == 1 && path->segments[0].generic_arguments.empty()) {
        if (auto symbol = ctx.get_symbol(path)) {
          if (symbol && symbol.get()->mutability == CONST) {
            throw_error("cannot assign to an immutable variable. consider adding 'mut' to the parameter or variable.",
                        node->span);
          }
        } else {
          throw_error("can't assign a non-existent variable", node->span);
        }

        // we assume this is mutable since we made it past that?
        auto symbol = ctx.get_symbol(path).get();
        if (symbol && symbol->is_variable) {
          symbol->variable.initial_value = node->right;
        } else {
          throw_error("cannot assign to non-variable", node->span);
        }
      }
    }
  }

  auto left_ty = left;

  // Do checks for constness.
  if (node->op == TType::Assign) {
    if (node->left->get_node_type() == AST_NODE_UNARY_EXPR) {
      auto unary = (ASTUnaryExpr *)node->left;
      auto unary_operand_ty = unary->operand->resolved_type;
      if (unary->op == TType::Mul && unary_operand_ty->is_const_pointer()) {
        throw_error("cannot dereference into an immutable pointer!", node->span);
      }

      auto left_ty = unary->operand->resolved_type;
      auto symbol = ctx.get_symbol(unary->operand);
      if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->is_mut_pointer()) {
        throw_error("cannot assign into an immutable variable!", node->span);
      }

    } else if (node->left->get_node_type() == AST_NODE_INDEX) {
      auto index = (ASTIndex *)node->left;

      auto subscript_left_ty = index->base->resolved_type;
      if (subscript_left_ty->is_const_pointer()) {
        throw_error("cannot index-assign into an immutable pointer!", node->span);
      }

      auto symbol = ctx.get_symbol(index->base);
      if (symbol.is_not_null() && symbol.get()->is_const() && !subscript_left_ty->is_mut_pointer()) {
        throw_error("cannot index-assign into an immutable variable!", node->span);
      }

    } else if (node->left->get_node_type() == AST_NODE_DOT_EXPR) {
      auto dot = (ASTDotExpr *)node->left;

      auto symbol = ctx.get_symbol(dot->base);
      auto left_ty = dot->base->resolved_type;

      if (left_ty->is_const_pointer()) {
        throw_error("cannot dot-assign into an immutable pointer!", dot->base->span);
      }

      if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->is_mut_pointer()) {
        throw_error("cannot dot-assign into an immutable variable!", node->span);
      }

      /*
        We have to check all the way down the left of the dot expression.
      */
      while (dot->base->get_node_type() == AST_NODE_DOT_EXPR) {
        dot = (ASTDotExpr *)dot->base;
        auto left_ty = dot->base->resolved_type;
        if (left_ty->is_const_pointer()) {
          throw_error("cannot dot-assign into an immutable pointer!", dot->base->span);
        }
        auto symbol = ctx.get_symbol(dot->base);
        if (symbol.is_not_null() && symbol.get()->is_const() && !left_ty->is_mut_pointer()) {
          throw_error("cannot dot-assign into an immutable variable!", dot->base->span);
        }
      }
    }
  }

  auto operator_overload_sym = find_operator_overload(CONST, left_ty, node->op, OPERATION_BINARY);
  if (operator_overload_sym) {
    node->is_operator_overload = true;
    auto ty = operator_overload_sym->resolved_type;
    node->resolved_type = ty->info->as<FunctionTypeInfo>()->return_type;
    node->resolved_operator_overload = operator_overload_sym->function.declaration;
    return;
  }

  if (node->op != TType::Assign && !left_ty->is_pointer() && left_ty->kind != TYPE_SCALAR && !operator_overload_sym &&
      left_ty->kind != TYPE_ENUM) {
    throw_error(
        std::format(
            "unable to use operator {}, it is a non trivial operation and no operator overload was implemented for type {}",
            TTypeToString(node->op), left_ty->to_string()),
        node->span);
  }

  // TODO(Josh) 9/30/2024, 8:24:17 AM relational expressions need to have
  // their operands type checked, but right now that would involve casting
  // scalars to each other, which makes no  sense.
  if (ttype_is_relational(node->op)) {
    node->resolved_type = bool_type();
  } else {
    auto left_t = left;
    auto right_t = right;
    auto conv_rule_0 = type_conversion_rule(left_t, right_t, node->left->span);
    auto conv_rule_1 = type_conversion_rule(right_t, left_t, node->right->span);

    if (((conv_rule_0 == CONVERT_PROHIBITED) && (conv_rule_1 == CONVERT_PROHIBITED)) ||
        ((conv_rule_0 == CONVERT_EXPLICIT) && (conv_rule_1 == CONVERT_EXPLICIT))) {
      throw_error(std::format("Type error in binary expression: cannot convert between {} and {}", left_t->to_string(),
                              right_t->to_string()),
                  node->span);
    }
    // TODO: is this correct??? do we even need to assign that here?
    node->resolved_type = left;
  }
}

void Typer::visit(ASTUnaryExpr *node) {
  node->operand->accept(this);
  auto operand_ty = node->operand->resolved_type;

  auto type = operand_ty;

  auto operator_overload_sym = find_operator_overload(CONST, type, node->op, OPERATION_UNARY);
  if (operator_overload_sym) {
    node->is_operator_overload = true;
    auto overload_ty = operator_overload_sym->resolved_type;
    node->resolved_type = overload_ty->info->as<FunctionTypeInfo>()->return_type;
    node->resolved_operator_overload = operator_overload_sym->function.declaration;
    auto name = get_operator_overload_name(node->op, OPERATION_UNARY);
    if (name == "deref") {
      auto type = node->resolved_type;
      if (!type->is_pointer()) {
        throw_error(
            "'deref' operator overload must return a pointer, the compiler will auto dereference this when "
            "it's used. it allows us to assign via this function",
            node->span);
      }
      node->resolved_type = type->get_element_type();
    }

    return;
  }

  // Address-Of.
  if (node->op == TType::And) {
    auto symbol = ctx.get_symbol(node->operand);
    auto op_ty = operand_ty;

    if (symbol) {
      auto sym = symbol.get();
      if (sym->is_const() && node->mutability == MUT && !op_ty->is_mut_pointer() && !op_ty->is_kind(TYPE_FUNCTION)) {
        throw_error("cannot take a mutable pointer to an immutable variable", node->span);
      }
    }

    node->resolved_type = op_ty->take_pointer_to(node->mutability);
    return;
  }

  // Dereference.
  if (node->op == TType::Mul) {
    auto type = operand_ty;

    if (type->is_pointer()) {
      node->resolved_type = type->get_element_type();
      return;
    } else {
      throw_error(std::format("Cannot dereference a non-pointer type, got \"{}\"", type->to_string()), node->span);
    }
  }

  // Convert to boolean if implicitly possible, for ! expressions
  {
    auto conversion_rule = type_conversion_rule(operand_ty, bool_type(), node->operand->span);
    auto can_convert = (conversion_rule != CONVERT_PROHIBITED && conversion_rule != CONVERT_EXPLICIT);

    if (node->op == TType::LogicalNot && can_convert) {
      node->resolved_type = bool_type();
      return;
    }
  }

  node->resolved_type = operand_ty;
  return;
}

void Typer::visit(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer: {
      auto value = node->value.str();

      if (value.starts_with("0x")) {
        if (value.length() > 18) {
          throw_error("Hexidecimal literal is too large to be represented by a 64 bit integer.", node->span);
        }
      } else if (value.starts_with("0b")) {
        if (value.length() > 64 + 2) {
          throw_error("Binary literal is too large to be represented by a 64 bit integer", node->span);
        }
      }

      if (expected_type != Type::INVALID_TYPE && type_is_numerical(expected_type)) {
        node->resolved_type = expected_type;
        return;
      }

      node->resolved_type = s32_type();
      return;
    }
    case ASTLiteral::Float:
      if (expected_type == f64_type()) {
        node->resolved_type = f64_type();
      } else {
        node->resolved_type = f32_type();
      }
      return;
    case ASTLiteral::MultiLineString:
    case ASTLiteral::String: {
      static bool nostdlib = compile_command.has_flag("nostdlib");
      if (nostdlib) {
        node->is_c_string = true;
      }
      if (node->is_c_string) {
        static Type *type = global_find_type_id(u8_type(), {{TYPE_EXT_POINTER_CONST}});
        node->resolved_type = type;
      } else {
        static Type *type = ctx.scope->find_type_id("str", {});
        node->resolved_type = type;
      }
      return;
    }

    case ASTLiteral::Bool:
      node->resolved_type = bool_type();
      return;
    case ASTLiteral::Null:
      // infer pointer type from decl or assign type, else we just use void*, for like n := null;
      if (expected_type != Type::INVALID_TYPE) {
        node->resolved_type = expected_type;
        return;
      }
      node->resolved_type = ctx.scope->find_type_id("void", {{TYPE_EXT_POINTER_CONST}});
      return;
    case ASTLiteral::Char:
      if (expected_type == u8_type()) {
        node->resolved_type = u8_type();
      } else {
        node->resolved_type = u32_type();
      }
      return;
  }
}

void Typer::visit(ASTDotExpr *node) {
  node->base->accept(this);
  auto base_ty_id = node->base->resolved_type;
  auto base_ty = base_ty_id;

  if (!base_ty) {
    throw_error(
        "internal compiler error: un-typed variable on lhs of dot "
        "expression?",
        node->span);
  }

  Scope *base_scope = base_ty->info->scope;

  // Implicit dereference, we look at the base scope.
  if (base_ty->is_pointer()) {
    base_ty = base_ty_id = base_ty->get_element_type();
    base_scope = base_ty->info->scope;
  }

  if (!base_scope) {
    throw_error("internal compiler error: dot expression used on a type that had a null scope", node->span);
  }

  if (node->member.tag != ASTPath::Segment::IDENTIFIER) {
    throw_error("cannot use a non-identifier as the right hand side of a dot expression. what were you doing??", node->span);
  }

  auto identifier = node->member.get_identifier();

  if (auto member = base_scope->local_lookup(identifier)) {
    /*
      TODO Resolve generic arguments for dot expression?
    */
    node->resolved_type = member->resolved_type;
  } else {
    for (const auto &[name, _] : base_scope->symbols) {
      std::cout << "symbol: " << name.str() << '\n';
    }
    throw_error(std::format("Member \"{}\" not found in type \"{}\"", identifier, base_ty->to_string()), node->span);
  }
}

void Typer::visit(ASTIndex *node) {
  node->base->accept(this);
  node->index->accept(this);
  auto left_ty = node->base->resolved_type;

  if (node->is_pointer_subscript && !left_ty->is_pointer()) {
    throw_error("tried to use the pointer index operator (`![..]`) on a non-pointer", node->span);
  }

  if (!node->is_pointer_subscript && left_ty->is_pointer()) {
    throw_error(
        "you must use the `![..]` pointer subscript operator, instead of a normal index operator, when doing "
        "subscripts on pointers.",
        node->span);
  }

  bool is_range = node->index->resolved_type->implements(is_range_trait());

  OperationKind operation = OPERATION_INDEX;
  if (is_range) {
    operation = OPERATION_SLICE_INDEX;
  }

  auto symbol = ctx.get_symbol(node->base);

  Mutability mutability = symbol ? symbol.get()->mutability : CONST;
  auto operator_overload_sym = find_operator_overload(mutability, left_ty, TType::LBrace, operation);

  if (operator_overload_sym) {
    node->is_operator_overload = true;
    node->resolved_operator_overload = operator_overload_sym->function.declaration;
    node->resolved_type = operator_overload_sym->resolved_type->info->as<FunctionTypeInfo>()->return_type;
    auto type = node->resolved_type;

    if (!type->is_pointer() && !is_range) {
      throw_error(
          "index methods MUST return a pointer, so that we can dereference into them."
          "if you want a slice operator, e.g (some_container[0..10]) use the Slice_Index trait.",
          node->span);
    }

    if (!is_range) {
      node->resolved_type = type->get_element_type();
    }
    return;
  }

  auto ext = left_ty->extensions;
  if (!type_extensions_is_back_array(ext) && !type_extensions_is_back_pointer(ext)) {
    throw_error(std::format("cannot index into non-array, non-pointer type that doesn't implement the `Index` trait. {}",
                            left_ty->to_string()),
                node->span);
  }

  node->resolved_type = left_ty->get_element_type();
}

void Typer::visit(ASTInitializerList *node) {
  Type *target_type;
  if (node->target_type.is_null()) {
    target_type = expected_type;
  } else {
    node->target_type.get()->accept(this);
    target_type = node->target_type.get()->resolved_type;

    if (node->tag == ASTInitializerList::INIT_LIST_COLLECTION) {
      auto expected = expected_type;
      if (expected && expected->is_fixed_sized_array()) {
        auto elem = expected->get_element_type();

        auto rule = type_conversion_rule(target_type, elem);
        if (rule == CONVERT_PROHIBITED) {
          throw_error("invalid initializer list element type", node->span);
        }
        target_type = expected;
      } else {
        target_type = find_generic_type_of("InitList", {target_type}, node->span);
      }
    }
  }

  if (!target_type) {
    throw_error("Can't use initializer list, no target type was provided", node->span);
  }

  if (target_type->is_pointer() ||
      (target_type->is_kind(TYPE_SCALAR) && target_type->has_no_extensions())) {  // !! I ADDED PARENTHESIS HERE IT MAY CAUSE BUGS
    throw_error(std::format("Cannot use an initializer list on a pointer, or a scalar type (int/float, etc) that's "
                            "not an array\n\tgot {}",
                            target_type->to_string()),
                node->span);
  }

  /*
    for collection style initializer lists.
  */
  Type *target_element_type = Type::INVALID_TYPE;
  if (target_type->basename.str().starts_with("InitList$")) {
    target_element_type = target_type->generic_args[0];
  } else if (target_type->is_fixed_sized_array()) {
    target_element_type = target_type->get_element_type();
  }

  auto scope = target_type->info->scope;
  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_NAMED: {
      std::set<InternedString> names;
      auto old_target = target_type;
      auto old_scope = scope;
      if (target_type->is_kind(TYPE_CHOICE)) {
        if (node->target_type.get() && node->target_type.get()->normal.path->get_node_type() == AST_NODE_PATH) {
          const auto path = (ASTPath *)node->target_type.get()->normal.path;
          const auto last_segment = path->segments.back();

          if (last_segment.tag != ASTPath::Segment::IDENTIFIER) {
            throw_error("INTERNAL COMPILER ERROR: got a non-identifier as the last segment of a choice type instantiation path",
                        node->span);
          }

          const auto info = target_type->info->as<ChoiceTypeInfo>();
          const auto variant_type = info->get_variant_type(last_segment.get_identifier());

          target_type = variant_type;
          scope = variant_type->info->scope;
        }
      } else if (!target_type->is_kind(TYPE_STRUCT)) {
        throw_error(std::format("named initializer lists can only be used for structs & unions, got type {}\nNote, for "
                                "unions, you can only provide one value.",
                                target_type->to_string()),
                    node->span);
      }

      // @Cleanup this is useful for returning a default value.
      // we would probably prefer a type::default(),
      // but for now we'll leave it.
      if (node->key_values.empty()) {
        node->resolved_type = target_type;
        return;
      }

      for (const auto &[id, value] : node->key_values) {
        if (names.contains(id)) {
          throw_error(std::format("Duplicate member initialization in named initializer list. member {}", id.str()), value->span);
        }
        names.insert(id);
        auto symbol = scope->local_lookup(id);
        if (!symbol) throw_error(std::format("Invalid named initializer list: couldn't find {}", id), node->span);

        if (symbol->is_function) {
          throw_error(std::format("Cannot initialize a function :: ({}) with an initializer list.", id), value->span);
        }

        ENTER_EXPECTED_TYPE(symbol->resolved_type);
        value->accept(this);
        assert_types_can_cast_or_equal(
            value, symbol->resolved_type, value->span,
            std::format("Unable to cast type to target field for named initializer list, field: {}", id.str()));
      }

      scope = old_scope;
      target_type = old_target;

    } break;
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      auto &values = node->values;

      if (values.empty()) {
        node->resolved_type = target_type;
        return;
      }

      for (size_t i = 0; i < values.size(); ++i) {
        const auto old = expected_type;
        expected_type = target_element_type;
        auto value = values[i];

        if (value->get_node_type() == AST_NODE_UNPACK) {
          auto old_expr_list = current_expression_list;
          values.erase(values.begin() + i);
          i--;
          current_expression_list = &values;
          value->accept(this);
          current_expression_list = old_expr_list;
          continue;
        }

        value->accept(this);
        expected_type = old;
        assert_types_can_cast_or_equal(
            value, target_element_type, value->span,
            "Found inconsistent types in a collection-style initializer list. These types must be homogenous");
      }
      node->resolved_type = target_type;
    } break;
    case ASTInitializerList::INIT_LIST_EMPTY:
      node->resolved_type = target_type;
      return;
  }
  node->resolved_type = target_type;
}

void Typer::visit(ASTRange *node) {
  if (node->left) {
    node->left->accept(this);
  }

  if (node->right) {
    node->right->accept(this);
  }

  if (node->left && node->right) {
    auto left = node->left->resolved_type;
    auto right = node->right->resolved_type;

    auto conversion_rule_left_to_right = type_conversion_rule(left, right);
    auto conversion_rule_right_to_left = type_conversion_rule(right, left);

    // Alwyas cast to the left? or should we upcast to the largest number type?
    if (conversion_rule_left_to_right == CONVERT_NONE_NEEDED || conversion_rule_left_to_right == CONVERT_IMPLICIT) {
      implicit_cast(node->right, right = left);
    } else if (conversion_rule_right_to_left == CONVERT_NONE_NEEDED || conversion_rule_right_to_left == CONVERT_IMPLICIT) {
      implicit_cast(node->left, left = right);
    } else {
      throw_error(
          "Can only use ranges when both types are implicitly castable to each other. Range will always take the "
          "left side's type",
          node->span);
    }
  }

  Type *type;
  if (!node->left && !node->right) {
    throw_error("ranges must have at least one value, on either the left or right, or both", node->span);
  }

  if (node->left) {
    type = node->left->resolved_type;
  } else {
    type = node->right->resolved_type;
  }

  node->resolved_type = find_generic_type_of("RangeBase", {type}, node->span);

  if (node->resolved_type == Type::INVALID_TYPE) {
    throw_error(std::format("Unable to find range type for `{}..{}`", type->to_string(), type->to_string()), node->span);
  }
}

void Typer::visit(ASTSwitch *node) {
  node->expression->accept(this);
  auto type_id = node->expression->resolved_type;
  auto type = type_id;

  if (node->is_pattern_match) {
    for (auto branch = node->branches.begin(); branch != node->branches.end(); branch++) {
      auto condition = branch->expression;
      if (condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
        auto pattern = (ASTPatternMatch *)condition;
        pattern->target_block = branch->block;
        condition->accept(this);
      } else {
        condition->accept(this);
      }
    }
  }

  if (!type->is_kind(TYPE_CHOICE) && !type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->is_pointer()) {
    auto operator_overload_symbol = find_operator_overload(CONST, type, TType::EQ, OPERATION_BINARY);

    if (!operator_overload_symbol) {
      throw_error(std::format("Can't use a 'switch' statement/expression on a non-scalar, non-enum, non-choice type that "
                              "doesn't implement "
                              "Eq (== operator on #self), or qualify for pattern matching (choice types).\ngot type '{}'",
                              type->to_string()),
                  node->expression->span);
    }
  }

  auto old_expected_type = expected_type;
  if (!node->is_statement) {
    expected_type = Type::INVALID_TYPE;
  }
  Defer _([&] { expected_type = old_expected_type; });

  Type *return_type = void_type();
  int flags = 0;

  for (const auto &branch : node->branches) {
    if (!node->is_pattern_match) branch.expression->accept(this);

    branch.block->accept(this);
    auto &block_cf = branch.block->control_flow;
    flags |= block_cf.flags;

    if (HAS_FLAG(block_cf.flags, BLOCK_FLAGS_RETURN)) {
      return_type = block_cf.type;
    }

    if (type_is_numerical(type)) {
      continue;
    } else if (branch.expression->get_node_type() != AST_NODE_PATTERN_MATCH) {
      assert_types_can_cast_or_equal(branch.expression, type_id, node->span, "Invalid switch case.");
    }
  }

  if (node->default_branch) {
    auto branch = node->default_branch.get();
    branch->accept(this);
    auto &block_cf = branch->control_flow;
    flags |= block_cf.flags;

    if (HAS_FLAG(block_cf.flags, BLOCK_FLAGS_RETURN)) {
      return_type = block_cf.type;
    }
  } else {
    flags |= BLOCK_FLAGS_FALL_THROUGH;
  }

  node->resolved_type = node->return_type = return_type;
  if (node->is_statement) {
    node->control_flow = ControlFlow{flags, return_type};
  } else {
    if (HAS_FLAG(flags, BLOCK_FLAGS_BREAK)) {
      throw_warning(WARNING_SWITCH_BREAK, "You do not need to break from switch cases.", node->span);
    } else if (HAS_FLAG(flags, BLOCK_FLAGS_CONTINUE)) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->span);
    }
  }
}

void Typer::visit(ASTTuple *node) {
  std::vector<Type *> types;
  auto declaring_tuple = expected_type;
  size_t type_index = 0;

  for (size_t i = 0; i < node->values.size(); ++i) {
    const auto &value = node->values[i];

    if (value->get_node_type() == AST_NODE_UNPACK) {
      auto old_expression_list = current_expression_list;
      current_expression_list = &node->values;

      node->values.erase(node->values.begin() + i);
      --i;
      value->accept(this);

      current_expression_list = old_expression_list;
      continue;
    }

    ENTER_EXPECTED_TYPE(nullptr);

    bool declaring_type_set = false;
    if (declaring_tuple && declaring_tuple->is_kind(TYPE_TUPLE)) {
      auto info = declaring_tuple->info->as<TupleTypeInfo>();
      if (info->types.size() < type_index) {
        throw_error(std::format("too many expressions provided to tuple\ntuple type {}", declaring_tuple->to_string()),
                    value->span);
      }
      expected_type = info->types[type_index];
      declaring_type_set = true;
    }

    value->accept(this);

    if (declaring_type_set) {
      assert_types_can_cast_or_equal(value, expected_type, value->span,
                                     "tuple value was incapable of casting to expected tuple element type");
      value->resolved_type = expected_type;
    }

    types.push_back(value->resolved_type);
    type_index++;
  }

  TypeExtensions extensions;

  node->resolved_type = global_find_type_id(types, extensions);
}

void Typer::visit(ASTAlias *node) {
  node->source_node->accept(this);

  auto symbol = ctx.get_symbol(node->source_node);

  if (ctx.scope->symbols.contains(node->name)) {
    throw_error("redefinition in alias", node->span);
  }

  if (symbol && node->source_node->get_node_type() != AST_NODE_TYPE) {
    ctx.scope->create_reference(ctx.get_symbol_and_scope(node->source_node));
  } else {
    auto type = node->source_node->resolved_type;
    if (type == nullptr) {
      throw_error("cannot alias a non-existent type or symbol", node->span);
    }
    ctx.scope->create_type_alias(node->name, type, node);
  }
  return;
}

void Typer::visit(ASTDestructure *node) {
  node->right->accept(this);
  node->resolved_type = node->right->resolved_type;
  auto type = node->right->resolved_type;

  for (auto &element : node->elements) {
    auto symbol = ctx.scope->local_lookup(element.identifier);

    if (node->op == TType::ColonEquals) {
      if (symbol) {
        throw_error(
            "redefinition of a variable, tuple deconstruction with := doesn't allow redeclaration of any of "
            "the identifiers",
            node->span);
      }
      ctx.scope->insert_local_variable(element.identifier, Type::INVALID_TYPE, nullptr, element.mutability);
    } else {
      if (!symbol) {
        throw_error("use of an undeclared variable, tuple deconstruction with = requires all identifiers already exist",
                    node->span);
      }
      ctx.scope->insert_local_variable(element.identifier, Type::INVALID_TYPE, nullptr, element.mutability);
    }
  }

  if (type->has_extensions()) {
    throw_error("Cannot destructure pointer or array type.", node->span);
  }

  auto members = type->info->members;
  size_t i = 0;

  for (auto [name, type, _, __] : members) {
    if (i > node->elements.size()) break;
    auto &destructure = node->elements[i];
    if (is_pointer_semantic(destructure.semantic)) {
      type = type->take_pointer_to(MUT);
    }
    destructure.type = type;
    ctx.scope->insert_local_variable(destructure.identifier, type, nullptr, destructure.mutability);
    ++i;
  }
};

void Typer::visit(ASTImpl *node) {
  if (!node->generic_parameters.empty()) {
    auto symbol_nullable = ctx.get_symbol(node->target);

    if (symbol_nullable.is_null() || !symbol_nullable.get()->is_type) {
      throw_error("generic `impl![...]` can only be used on types.", node->span);
    }

    auto declaring_node = symbol_nullable.get()->type.declaration.get();
    auto node_as_decl = static_cast<ASTDeclaration *>(declaring_node);
    node_as_decl->impls.push_back(node);
    for (auto instantiations : node_as_decl->generic_instantiations) {
      visit_generic(node, instantiations.arguments, instantiations.declaration->span);
    }
  } else {
    visit_impl_declaration(node, false);
  }
  return;
}

void Typer::visit(ASTDefer *node) {
  node->statement->accept(this);
  return;
}

void Typer::visit(ASTCast *node) {
  node->expression->accept(this);
  auto expr_type = node->expression->resolved_type;
  node->target_type->accept(this);
  auto type = node->target_type->resolved_type;
  auto conversion = type_conversion_rule(expr_type, type);
  if (conversion == CONVERT_PROHIBITED) {
    throw_error(std::format("casting {} to {} is strictly prohibited.", expr_type->to_string(), type->to_string()), node->span);
  }
  node->resolved_type = type;
}

void Typer::visit(ASTTraitDeclaration *node) {
  if (!node->generic_parameters.empty()) {
    for (auto param : node->generic_parameters) {
      if (param.default_value) {
        param.default_value->accept(this);
      }
    }
    ctx.scope->create_trait_type(node->name, node->scope, {}, node);
  } else {
    visit_trait_declaration(node, false);
    ctx.scope->create_type_alias(node->name, node->resolved_type, node);
  }
  return;
}

void Typer::visit(ASTSize_Of *node) {
  node->target_type->accept(this);
  node->resolved_type = u64_type();
}

void Typer::visit_import_group(const ASTImport::Group &group, Scope *module_scope, Scope *import_scope, const Span &range) {
  ENTER_SCOPE(module_scope);
  import_scope->create_reference(ctx.get_symbol_and_scope(group.path));

  if (group.is_wildcard) {
    // Import all symbols from the module
    for (const auto &[name, sym] : module_scope->symbols) {
      import_scope->create_reference(name, module_scope);
    }
    return;
  }

  for (const auto &symbol : group.symbols) {
    if (symbol.is_group) {  // Group import
      Nullable<Symbol> submod_sym = ctx.get_symbol(symbol.group.path);
      if (!submod_sym || !submod_sym.get()->is_module) {
        throw_error("submodule or type '{}' not found in module", range);
      }
      auto submod = submod_sym.get();
      visit_import_group(symbol.group, submod->module.declaration->scope, import_scope, range);

    } else {  // Single symbol import
      Nullable<Symbol> sym = ctx.get_symbol(symbol.path);

      if (symbol.path->segments.back().tag != ASTPath::Segment::IDENTIFIER) {
        throw_error(
            "got a non-identifier path in an import, this is not valid. "
            "You don't need to apply generics or anything to imports",
            {});
      }

      InternedString name = symbol.path->segments.back().get_identifier();

      if (!sym) {
        throw_error(std::format("symbol: {} not found in module", name), range);
      }

      if (symbol.has_alias) {
        import_scope->create_reference(name, module_scope, symbol.alias);
      }

      import_scope->create_reference(name, module_scope);
    }
  }
}

void Typer::visit(ASTImport *node) {
  const Nullable<Symbol> the_module_nullable = ctx.get_symbol(node->root_group.path);

  if (!the_module_nullable) {
    for (const auto &[name, sym] : ctx.scope->symbols) {
      if (sym.is_module) {
        printf("local module: %s\n", name.c_str());
      }
    }
    for (const auto &[name, sym] : ctx.root_scope->symbols) {
      if (sym.is_module) {
        printf("global/imported module: %s\n", name.c_str());
      }
    }
    throw_error("unable to find module", node->span);
  }

  const Symbol *the_module = the_module_nullable.get();

  // This means that we imported a single symbol.
  if (!the_module->is_module) {
    ctx.scope->create_reference(ctx.get_symbol_and_scope(node->root_group.path));
    return;
  }

  Scope *module_scope = the_module->module.declaration->scope;
  // propagate the module itself, when we add symbols from it, so we can access it like
  /*
    `
      import api::{
        subapi::{
          ...
        }
      };
    `
    then we can do
      `api::subapi::function();`
    or whatever.
  */

  ctx.scope->symbols[the_module->name] = *the_module;

  visit_import_group(node->root_group, module_scope, ctx.scope, node->span);
}

void Typer::visit(ASTModule *node) {
// TODO: we need to remove all the symbol table insertions from the parser to make this work.
// otherwise we just get constant fake redefinitions.
// For now, redefinitions from module "appends" are untracked.
#if 0 
  {
    if (auto mod = ctx.scope->lookup(node->module_name)) {
      for (const auto &stmt : node->statements) {
        InternedString offender = {};
  
        if (auto struct_decl = dynamic_cast<ASTStructDeclaration *>(stmt)) {
          if (mod->scope->symbols.contains(struct_decl->name)) {
            offender = struct_decl->name;
            goto err;
          }
        }
        if (auto choice_decl = dynamic_cast<ASTChoiceDeclaration *>(stmt)) {
          if (mod->scope->symbols.contains(choice_decl->name)) {
            offender = choice_decl->name;
            goto err;
          }
        }
        if (auto enum_decl = dynamic_cast<ASTEnumDeclaration *>(stmt)) {
          if (mod->scope->symbols.contains(enum_decl->name)) {
            offender = enum_decl->name;
            goto err;
          }
        }
        if (auto func_decl = dynamic_cast<ASTFunctionDeclaration *>(stmt)) {
          auto sym = mod->scope->local_lookup(func_decl->name);
  
          if (!sym) {
            continue;
          }
          
          const ASTFunctionDeclaration *decl = sym->function.declaration;
  
          const bool new_declaration_is_fwd_or_extern = func_decl->is_forward_declared || func_decl->is_extern;
          const bool existing_declaration_is_fwd_or_extern = decl->is_extern || decl->is_forward_declared;
  
          if (!new_declaration_is_fwd_or_extern && existing_declaration_is_fwd_or_extern) {
            offender = func_decl->name;
            goto err;
          }
        }
        if (auto trait_decl = dynamic_cast<ASTTraitDeclaration *>(stmt)) {
          if (mod->scope->symbols.contains(trait_decl->name)) {
            offender = trait_decl->name;
            goto err;
          }
        }
        if (auto alias_decl = dynamic_cast<ASTAlias *>(stmt)) {
          if (mod->scope->symbols.contains(alias_decl->name)) {
            offender = alias_decl->name;
            goto err;
          }
        }
        if (auto variable_decl = dynamic_cast<ASTVariable *>(stmt)) {
          if (mod->scope->symbols.contains(variable_decl->name)) {
            offender = variable_decl->name;
            goto err;
          }
        }
  
        continue;
      err:
        throw_error(std::format("redefinition of '{}'", offender), stmt->span);
      }
    }
  }
#endif

  {
    ENTER_SCOPE(node->scope)
    node->scope->name = node->module_name;
    for (auto statement : node->statements) {
      statement->accept(this);
    }
  }

  if (Symbol *mod = ctx.scope->lookup(node->module_name)) {
    if (!mod->is_module) {
      throw_error(std::format("cannot create module '{}': an identifier exists in this scope with that name.", mod->name),
                  node->span);
    }
    auto scope = mod->module.declaration->scope;
    // See above: we need to check for redefinitions.
    for (auto &[name, sym] : node->scope->symbols) {
      scope->symbols[name] = sym;
    }
  } else {
    node->declaring_scope->create_module(node->module_name, node);
  }
}

void Typer::visit(ASTDyn_Of *node) {
  if (!node->trait_type) {
    auto type = expected_type;
    if (type && type->is_kind(TYPE_DYN)) {
      node->trait_type = ast_alloc<ASTType>();
      node->trait_type->resolved_type = type->info->as<DynTypeInfo>()->trait_type;
    } else {
      throw_error(
          "if a dyn type isn't already expected (via an argument, or an explicitly typed variable declaration, "
          "etc), you must pass the trait type as the second parameter to 'dynof'\nSo, if you wanted a "
          "'dyn Format', youd use 'dynof(my_instance, Format)'",
          node->span);
    }
  } else {
    node->trait_type->accept(this);
  }

  node->object->accept(this);

  auto object_type = node->object->resolved_type;

  if (!object_type->is_mut_pointer()) {
    throw_error(
        "'dynof' requires the first argument, the instance to create a dyn dispatch object for, must be a "
        "mutable pointer. eventually we'll have const dyn's",
        node->span);
  }

  auto type = node->trait_type->resolved_type;
  if (!type->is_kind(TYPE_TRAIT)) {
    throw_error("cannot use 'dynof(Type, $expr)' on types that aren't traits.", node->span);
  }

  auto element_type = object_type->get_element_type();
  if (!element_type->implements(node->trait_type->resolved_type)) {
    throw_error(std::format("cannot create 'dyn {}' from object of type '{}' because it does not implement the trait.",
                            type->to_string(), element_type->to_string()),
                node->span);
  }

  auto ty =
      ctx.scope->find_or_create_dyn_type_of(type->base_type == Type::INVALID_TYPE ? type : type->base_type, node->span, this);
  node->resolved_type = ty;
}

/*
  TODO: this shouldn't be on the scope. it's an absolute eye sore, and could certainly be tidied up and cleaned
*/
Type *Scope::find_or_create_dyn_type_of(Type *trait_type, Span range, Typer *typer) {
  for (size_t i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->is_kind(TYPE_DYN) && type_table[i]->info->as<DynTypeInfo>()->trait_type == trait_type) {
      return type_table[i];
    }
  }
  auto trait_name = "dyn$" + trait_type->to_string();
  auto dyn_info = new (type_info_alloc<DynTypeInfo>()) DynTypeInfo();
  dyn_info->trait_type = trait_type;

  // TODO: * determine whether 'dyn' should actually be in the type name itself. *
  auto ty = global_create_type(TYPE_DYN, trait_name, dyn_info);

  ty->traits.push_back(is_dyn_trait());

  dyn_info->members.push_back({.name = "instance", .type = void_type()->take_pointer_to(true)});

  dyn_info->scope->insert_variable("instance", global_find_type_id(void_type(), {{TYPE_EXT_POINTER_MUT}}), nullptr, MUT);

  ty->info->as<DynTypeInfo>()->trait_type = trait_type;

  auto trait_info = trait_type->info->as<TraitTypeInfo>();

  auto old_scope = typer->ctx.scope;
  typer->ctx.scope = trait_info->scope;
  Defer _defer([&] { typer->ctx.scope = old_scope; });
  const auto insert_function = [&](const InternedString &name, ASTFunctionDeclaration *declaration) {
    std::vector<Type *> parameters;
    bool has_self = false;
    for (auto param : declaration->params->params) {
      if (param->tag == ASTParamDecl::Self) {
        if (param->self.is_pointer) {
          parameters.push_back(global_find_type_id(void_type(), {{TYPE_EXT_POINTER_CONST}}));
        } else {
          throw_error(
              "cannot use 'dyn' on traits that take 'self' by value because that would be a zero-sized "
              "parameter, as we don't know the type of the 'self' at compile time definitively.",
              range);
        }
        has_self = true;
      } else {
        if (!has_self) {
          throw_error(
              "'dyn' can only be used with traits that do not have any associated functions, e.g functions "
              "that\n"
              "do not take a '*mut self', nor a '*const self' (in the case of 'dyn' self must always be a pointer)",
              range);
        }

        param->accept(typer);
        // There's an exception here for trait typed parameters.
        auto parameter_type = param->resolved_type;
        if (parameter_type->is_kind(TYPE_TRAIT)) {
          throw_error(
              "you cannot take a 'dyn' of an trait that uses other traits as parameter constraints.\n"
              "the parameters all must be concrete types, with the exception of '*const/mut self' params.",
              range);
        }

        parameters.push_back(param->resolved_type);
      }
    }

    if (declaration->return_type->kind == ASTType::SELF) {
      throw_error(
          "just as we can't take 'self' by value in a 'dyn' trait, you can't return '#self', even by pointer, "
          "because we would have to return it as a type erased *const void. return the concrete type.",
          range);
    }

    declaration->return_type->accept(typer);
    auto return_type = declaration->return_type->resolved_type;

    FunctionTypeInfo type_info;
    memcpy(type_info.parameter_types, parameters.data(), parameters.size() * sizeof(Type *));
    type_info.params_len = parameters.size();
    type_info.return_type = return_type;

    auto function_type = global_find_function_type_id(type_info, {{TYPE_EXT_POINTER_MUT}});
    dyn_info->members.push_back({
        .name = name,
        .type = function_type->take_pointer_to(),
    });
    dyn_info->methods.push_back({name.str(), function_type /* , declaration */});
    dyn_info->scope->insert_variable(name.str(), function_type, nullptr, MUT, nullptr);
  };

  for (const auto &[name, sym] : trait_info->scope->symbols) {
    if (sym.is_function && !sym.is_generic_function()) {
      insert_function(name, sym.function.declaration);
    }
  };

  if (trait_type->declaring_node) {
    auto declaration = (ASTTraitDeclaration *)trait_type->declaring_node.get();
    if (declaration->where_clause) {
      auto where = declaration->where_clause.get();
      for (const auto &constraint : where->constraints) {
        constraint.first->accept(typer);
        constraint.second->accept(typer);
        if (!type_is_valid(constraint.first->resolved_type) || !type_is_valid(constraint.second->resolved_type)) {
          continue;
        }
        if (constraint.first->get_node_type() != AST_NODE_TYPE) {
          continue;
        }
        auto type = (ASTType *)constraint.first;
        if (type->kind != ASTType::SELF) {
          continue;
        }
        if (!constraint.second->resolved_type->is_kind(TYPE_TRAIT)) {
          continue;
        }
        auto right_ty_info = constraint.second->resolved_type->info->as<TraitTypeInfo>();
        for (const auto &[name, sym] : right_ty_info->scope->symbols) {
          if (!sym.is_function || sym.is_generic_function()) {
            continue;
          }

          auto declaration = sym.function.declaration;

          // Only dyn applicable trait bounds methods are taken. this is far more selective than the other process.
          if (!declaration || !declaration->params->has_self || !declaration->params->params[0]->self.is_pointer) {
            continue;
          }

          insert_function(name, declaration);
        }
      }
    }
  }

  auto sym = Symbol::create_type(ty, trait_name, nullptr);
  // TODO: we have to fit this in modules or some stuff.
  sym.parent_scope = this;
  symbols.insert_or_assign(trait_name, sym);
  return ty;
}

Nullable<Symbol> Context::get_symbol(ASTNode *node) {
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
      Scope *scope = this->scope;
      size_t index = 0;
      for (auto &part : path->segments) {
        Symbol *symbol = nullptr;
        Type *type = nullptr;
        if (part.tag == ASTPath::Segment::IDENTIFIER) {
          symbol = scope->lookup(part.get_identifier());
          if (!symbol) {
            return nullptr;
          }
        } else if (part.tag == ASTPath::Segment::TYPE) {
          type = part.get_type()->resolved_type;
        } else {
          throw_error("INTERNAL COMPILER ERROR: got an invalid path segment, was neither identifier nor type", node->span);
        }

        if (index == path->length() - 1) {
          return symbol;
        }

        if (type) {
          scope = type->info->scope;
        } else if (!part.generic_arguments.empty()) {
          if (symbol->is_type) {
            auto decl = dynamic_cast<ASTDeclaration *>(symbol->type.declaration.get());

            if (!decl) {
              throw_error("Cannot apply generic arguments to that type", node->span);
            }

            auto instantiation = find_generic_instance(((ASTDeclaration *)symbol->type.declaration.get())->generic_instantiations,
                                                       part.get_resolved_generics());
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

      if (dotnode->member.tag != ASTPath::Segment::IDENTIFIER) {
        throw_error("cannot use a non-identifier path as the right hand side of a dot expression", node->span);
      }

      auto identifier = dotnode->member.get_identifier();
      auto symbol = type->info->scope->local_lookup(identifier);

      // Implicit dereference, we look at the base scope.
      if (!symbol && type->is_pointer()) {
        type = type->get_element_type();
        symbol = type->info->scope->local_lookup(identifier);
      }
      return symbol;
    }
    default:
      return nullptr;  // TODO: verify this isn't strange.
  }
  return nullptr;
}

Nullable<Scope> Context::get_scope(ASTNode *node) {
  switch (node->get_node_type()) {
    case AST_NODE_TYPE: {
      auto type = node->resolved_type;
      return type->info->scope;
    }
    case AST_NODE_PATH: {
      auto path = static_cast<ASTPath *>(node);
      Scope *scope = this->scope;
      size_t index = 0;
      for (auto &part : path->segments) {
        Symbol *symbol = nullptr;
        Type *type = nullptr;
        if (part.tag == ASTPath::Segment::IDENTIFIER) {
          symbol = scope->lookup(part.get_identifier());
          if (!symbol) {
            throw_error("INTERNAL COMPILER ERROR: symbol null in path", node->span);
          }
        } else if (part.tag == ASTPath::Segment::TYPE) {
          type = part.get_type()->resolved_type;
        } else {
          throw_error("INTERNAL COMPILER ERROR: got an invalid path segment, was neither identifier nor expression", node->span);
          return nullptr;
        }

        if (index == path->length() - 1) return symbol->parent_scope;

        if (type) {
          scope = type->info->scope;
        } else if (!part.generic_arguments.empty()) {
          if (symbol->is_type) {
            auto instantiation = find_generic_instance(((ASTDeclaration *)symbol->type.declaration.get())->generic_instantiations,
                                                       part.get_resolved_generics());
            auto type = instantiation->resolved_type;
            scope = type->info->scope;
          } else
            return nullptr;
        } else {
          if (symbol->is_module) {
            scope = symbol->module.declaration->scope;
          } else if (symbol->is_type) {
            auto resolved_type = symbol->resolved_type;
            scope = resolved_type->info->scope;
          } else {
            return nullptr;
          }
        }
        index++;
      }
    } break;
    default:
      return nullptr;  // TODO: verify this isn't strange.
  }
  return nullptr;
}

void Typer::visit(ASTMethodCall *node) {
  Type *type = nullptr;
  ASTFunctionDeclaration *func_decl = nullptr;
  node->callee->accept(this);
  auto func_sym = ctx.get_symbol(node->callee).get();

  auto old_expression_list = current_expression_list;
  current_expression_list = &node->arguments->arguments;
  Defer _defer([&] { current_expression_list = old_expression_list; });

  if (func_sym && func_sym->is_function) {
    type = func_sym->resolved_type;

    auto declaring_node = func_sym->function.declaration;
    if (declaring_node && declaring_node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      func_decl = static_cast<ASTFunctionDeclaration *>(declaring_node);

      // resolve a generic call.
      if (!func_decl->generic_parameters.empty()) {
        // doing this so self will get the right type when we call generic methods
        // TODO: handle this in the function decl itself, maybe insert self into symbol table

        auto old_type = type_context;
        Defer _([&] { type_context = old_type; });

        ASTType func_type_ast;
        if (func_decl->declaring_type != Type::INVALID_TYPE) {
          func_type_ast.resolved_type = func_decl->declaring_type;
          type_context = &func_type_ast;
        }

        func_decl =
            resolve_generic_function_call(func_decl, &node->callee->member.generic_arguments, node->arguments, node->span);
      }

      type = func_decl->resolved_type;
    }
  } else {
    // Implicitly pass the 'dyn.instance' when calling the function pointers
    // that the dyn thingy sets up.
    auto object = node->callee->base;
    auto obj_type = object->resolved_type;

    if (obj_type->is_kind(TYPE_DYN) && !node->inserted_dyn_arg) {
      auto &args = node->arguments->arguments;
      auto dot = ast_alloc<ASTDotExpr>();
      dot->base = object;
      dot->member = ASTPath::Segment::Identifier(DYN_INSTANCE_KEY);
      dot->resolved_type = global_find_type_id(void_type(), {{TYPE_EXT_POINTER_MUT}});
      args.insert(args.begin(), dot);
      node->inserted_dyn_arg = true;
      node->dyn_method_name = node->callee->member.get_identifier();
    }

    type = node->callee->resolved_type;
  }

  if (!type) {
    throw_error("use of undeclared function", node->span);
  }

  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(std::format("unable to call a non-function, got {}", type->to_string()), node->span);
  }

  auto info = type->info->as<FunctionTypeInfo>();

  // If we have the declaring node representing this function, type check it against the parameters in that definition.
  // else, use the type.
  if (func_decl) {
    if (!func_decl->params->has_self) {
      throw_error("Calling static methods with instance not allowed", node->span);
    }
    type_check_args_from_params(node->arguments, func_decl->params, func_decl, node->callee->base, func_sym->name == "destroy");
  } else {
    type_check_args_from_info(node->arguments, info);
  }

  node->resolved_type = info->return_type;
}

void Typer::visit(ASTPatternMatch *node) {
  // this just serves as a condition.
  // we should probably restrict this to being in control flow.
  node->resolved_type = bool_type();

  node->object->accept(this);
  node->target_type_path->accept(this);

  auto old_scope = ctx.scope;
  ctx.set_scope(node->target_block->scope);
  Defer _([&] { ctx.scope = old_scope; });

  auto target_type = node->target_type_path->resolved_type;
  auto info = target_type->info->as<ChoiceTypeInfo>();

  const auto segment = node->target_type_path->segments.back();
  auto variant_type = info->get_variant_type(segment.get_identifier());

  switch (node->pattern_tag) {
    case ASTPatternMatch::NONE:
      break;
    case ASTPatternMatch::STRUCT: {
      if (!variant_type->is_kind(TYPE_STRUCT)) {
        throw_error(
            "cannot use { $field: $var, ... } destructure on a non-struct-style choice variant.\nfor tuple "
            "style <Variant(), Variant(f32, s32)>,\nuse the tuple destructure syntax. <Choice::Tuple(x, y)>. "
            "for markers, such as <Variant>, dont use any destructure.",
            node->span);
      }

      auto info = variant_type->info->as<StructTypeInfo>();
      for (auto &part : node->struct_pattern.parts) {
        auto symbol = info->scope->local_lookup(part.field_name);
        if (!symbol) {
          throw_error(std::format("cannot destructure field {} of choice variant {} because it didn't have that field.",
                                  part.field_name, target_type->to_string()),
                      node->span);
        }

        auto type_id = symbol->resolved_type;
        if (part.semantic == PATTERN_MATCH_PTR_CONST) {
          type_id = global_find_type_id(type_id, {{TYPE_EXT_POINTER_CONST}});
        } else if (part.semantic == PATTERN_MATCH_PTR_MUT) {
          type_id = global_find_type_id(type_id, {{TYPE_EXT_POINTER_MUT}});
        }

        part.resolved_type = type_id;

        ctx.scope->insert_local_variable(part.var_name, type_id, nullptr, part.mutability);
      }
    } break;
    case ASTPatternMatch::TUPLE: {
      if (!variant_type->is_kind(TYPE_TUPLE)) {
        throw_error(
            "cannot use ($var, $var) destructure on a non-tuple-style choice variant.\nfor struct "
            "style <Variant {x: f32, y: f32} >,\nuse the struct destructure syntax. <Choice::Variant { x: x, "
            "y: mut y)>. "
            "for markers, such as <Variant>, dont use any destructure.",
            node->span);
      }
      auto info = variant_type->info->as<TupleTypeInfo>();
      if (node->tuple_pattern.parts.size() > info->types.size()) {
        throw_error("too many variables provided in choice type tuple destructure", node->span);
      }
      auto index = 0;
      for (auto &part : node->tuple_pattern.parts) {
        auto type_id = info->types[index];
        if (part.semantic == PATTERN_MATCH_PTR_CONST) {
          type_id = global_find_type_id(type_id, {{TYPE_EXT_POINTER_CONST}});
        } else if (part.semantic == PATTERN_MATCH_PTR_MUT) {
          type_id = global_find_type_id(type_id, {{TYPE_EXT_POINTER_MUT}});
        }
        part.resolved_type = type_id;
        ctx.scope->insert_local_variable(part.var_name, type_id, nullptr, part.mutability);
        index++;
      }
    } break;
  }
}

void Typer::visit_path(ASTPath *node, bool from_call) {
  Scope *scope = ctx.scope;
  size_t index = 0;
  Type *previous_type = nullptr;
  for (auto &segment : node->segments) {
    Symbol *symbol = nullptr;
    Type *type = nullptr;
    if (segment.tag == ASTPath::Segment::IDENTIFIER) {
      auto ident = segment.get_identifier();
      symbol = scope->lookup(ident);
      if (!symbol) {
        throw_error(std::format("use of undeclared identifier '{}'", segment.get_identifier()), node->span);
      }
    } else if (segment.tag == ASTPath::Segment::TYPE) {
      auto type_ast = segment.get_type();
      visit(type_ast);
      type = type_ast->resolved_type;
    } else {
      throw_error("INTERNAL COMPILER ERROR: path segment was neither type nor identifier", node->span);
    }
    if (previous_type && previous_type->is_kind(TYPE_CHOICE) && index == node->length() - 1 &&
        segment.tag == ASTPath::Segment::IDENTIFIER) {
      /* we need to return the parent type here? but we need to maintain the variant. hmm. */
      node->resolved_type = previous_type;
      auto symbol = previous_type->info->scope->lookup(segment.get_identifier());
      if (!symbol) {
        throw_error(
            std::format("unable to find varaint '{}' in choice type '{}'", segment.get_identifier(), previous_type->to_string()),
            node->span);
      }
      return;
    }

    scope = nullptr;

    if (type) {
      previous_type = type;
      scope = previous_type->info->scope;
      segment.resolved_type = type;
    } else if (!segment.generic_arguments.empty()) {
      std::vector<Type *> generic_args;
      for (auto &arg : segment.generic_arguments) {
        arg->accept(this);
        generic_args.push_back(arg->resolved_type);
      }
      ASTDeclaration *decl;
      if (symbol->is_type) {
        decl = (ASTDeclaration *)symbol->type.declaration.get();
      } else if (symbol->is_function) {
        decl = symbol->function.declaration;
      } else {
        throw_error("use of generic arguments only for types and functions currently.", node->span);
      }
      if (from_call && index == node->segments.size() - 1 && generic_args.size() != decl->generic_parameters.size()) {
        // dont try to instantiate unresolved generic function for calls so that call can handle it
        return;
      }
      auto instantiation = visit_generic(decl, generic_args, node->span);
      if (symbol->is_type) {
        auto type = instantiation->resolved_type;
        scope = type->info->scope;
        previous_type = type;
      } else if (symbol->is_function) {
        instantiation = visit_generic(symbol->function.declaration, generic_args, node->span);
      }
      segment.resolved_type = instantiation->resolved_type;
    } else {
      if (symbol->is_module) {
        scope = symbol->module.declaration->scope;
      } else if (symbol->is_type) {
        if (symbol->resolved_type == Type::UNRESOLVED_GENERIC) {
          throw_error(std::format("use of generic type {}, but no type arguments were provided.", symbol->name), node->span);
        }
        previous_type = symbol->resolved_type;
        scope = previous_type->info->scope;
      }
      segment.resolved_type = symbol->resolved_type;
    }
    if (!scope && index < node->segments.size() - 1) {
      if (segment.tag == ASTPath::Segment::IDENTIFIER) {
        throw_error(std::format("symbol {}'s scope could not be resolved in path", segment.get_identifier()), node->span);
      } else {
        throw_error("symbol {}'s scope could not be resolved in path", node->span);
      }
    }
    index++;
  }

  node->resolved_type = node->segments[node->segments.size() - 1].resolved_type;
}

void Typer::visit(ASTPath *node) { visit_path(node); }

bool Typer::visit_where_predicate_throws(Type *target_type, ASTExpr *predicate) {
  switch (predicate->get_node_type()) {
    case AST_NODE_BIN_EXPR: {
      auto bin = static_cast<ASTBinExpr *>(predicate);
      auto op = bin->op;
      if (op == TType::And) {
        return visit_where_predicate(target_type, bin->left) && visit_where_predicate(target_type, bin->right);
      } else if (op == TType::Or) {
        return visit_where_predicate(target_type, bin->left) || visit_where_predicate(target_type, bin->right);
        return true;
      } else {
        throw std::pair(
            "Invalid operator in 'where' clause predicate, only And/Or allowed: '&' / '|'.\nNote: these use "
            "'bitwise' operators for brevity, they're effectively '&&' and '||'.",
            bin->span);
      }
    } break;
    case AST_NODE_TYPE: {
      // make sure the type is fixed up.
      predicate->accept(this);
      // return whether this type implements this trait or not.
      // also can be used to assert whether it's equal to the type provided or not.
      return std::ranges::find(target_type->traits, predicate->resolved_type) != target_type->traits.end() ||
             target_type == predicate->resolved_type;
    } break;
    default:
      throw std::pair("Invalid node in 'where' clause predicate", predicate->span);
  }
}

bool Typer::visit_where_predicate(Type *target_type, ASTExpr *predicate) {
  try {
    return visit_where_predicate_throws(target_type, predicate);
  } catch (const std::pair<const char *, Span> &error) {
    throw_error(error.first, error.second);
    return false;
  }
}

void Typer::visit(ASTWhere *node) {
  for (auto &constraint : node->constraints) {
    auto [target_type, predicate] = constraint;
    target_type->accept(this);
    auto type = target_type->resolved_type;
    auto satisfied = visit_where_predicate(type, predicate);

    if (!satisfied) {
      throw_error(
          std::format("constraint \"{}\" not satisfied for {}", print_where_predicate(predicate), get_unmangled_name(type)),
          node->span);
    }
  }
}

void Typer::visit(ASTWhereStatement *node) {
  const auto visit_where_clause_no_error = [&](ASTWhere *where) -> bool {
    for (auto &constraint : where->constraints) {
      auto [target_type, predicate] = constraint;
      target_type->accept(this);
      auto type = target_type->resolved_type;
      try {
        return visit_where_predicate_throws(type, predicate);
      } catch (const std::pair<const char *, Span> &error) {
        return false;
      }
    }
    return false;
  };

  const auto visit_branch = [&](WhereBranch *branch) -> ControlFlow {
    if (branch->where_stmt.is_not_null()) {
      branch->where_stmt.get()->accept(this);
      return branch->where_stmt.get()->control_flow;
    } else {
      branch->block.get()->accept(this);
      return branch->block.get()->control_flow;
    }
  };

  auto value = visit_where_clause_no_error(node->where);

  auto &control_flow = node->control_flow;
  if (value || (!value && node->negative)) {
    node->should_compile = true;
    node->block->accept(this);
    control_flow = node->block->control_flow;
  } else if (node->branch.is_not_null()) {
    auto branch = node->branch.get();
    control_flow = visit_branch(branch);
  } else {
    control_flow = {};
    control_flow.flags |= BLOCK_FLAGS_FALL_THROUGH;
  }
}

/// TODO: this needs ot happen for tuples too, any tuple that contains a "Destroy"able object needs to forward
/// that declaration to the parent type, and this is where we steal therust nomenclature of "Drop Glue".
void Typer::implement_destroy_glue_for_choice_type(ASTChoiceDeclaration *node, const bool generic_instantiation,
                                                   const std::vector<Type *> generic_args) {
  return;
  // no-checkin
  if (generic_instantiation) {
    node = (ASTChoiceDeclaration *)find_generic_instance(node->generic_instantiations, generic_args);
  }
  Type *type = node->resolved_type;
  ChoiceTypeInfo *info = type->info->as<ChoiceTypeInfo>();

  bool needs_destroy = false;
  for (const auto &member : info->members) {
    if (member.type->implements(g_Destroy_trait_type)) {
      needs_destroy = true;
      break;
    }
  }

  if (!needs_destroy) {
    return;
  }

  type->traits.push_back(g_Destroy_trait_type);
  node->destroy_glue_compiler_implementation_needed = true;

  FunctionTypeInfo func_info;
  func_info.parameter_types[0] = node->resolved_type->take_pointer_to(true);
  func_info.parameter_types[1] = bool_type();
  func_info.params_len = 2;
  func_info.return_type = void_type();
  func_info.is_varargs = false;
  Type *destroy_method_type_id = global_find_function_type_id(func_info, {});

  ASTFunctionDeclaration *declaration = ast_alloc<ASTFunctionDeclaration>();
  declaration->block = nullptr;
  declaration->generic_arguments = {};
  declaration->declaring_type = type;
  declaration->resolved_type = destroy_method_type_id;
  declaration->params = ast_alloc<ASTParamsDecl>();

  {
    ASTParamDecl *first_param = ast_alloc<ASTParamDecl>();
    first_param->mutability = MUT;
    first_param->tag = ASTParamDecl::Self;
    first_param->self.is_pointer = true;
    declaration->params->params.push_back(first_param);
  }
  {
    ASTParamDecl *second_param = ast_alloc<ASTParamDecl>();
    second_param->mutability = CONST;
    second_param->tag = ASTParamDecl::Normal;
    {
      ASTLiteral *default_value = ast_alloc<ASTLiteral>();
      default_value->value = "true";
      default_value->tag = ASTLiteral::Bool;
      second_param->normal.default_value = default_value;
    }
    {
      second_param->normal.type = ast_alloc<ASTType>();
      second_param->normal.type->resolved_type = bool_type();
    }
    declaration->params->params.push_back(second_param);
  }

  info->scope->insert_function("destroy", destroy_method_type_id, declaration);
}

void Typer::visit(ASTUnpack *node) {
  ASTExpr *expr = node->expression;
  expr->accept(this);

  if (expr->resolved_type->is_kind(TYPE_TUPLE)) {
    node->resolved_type = void_type();

    TupleTypeInfo *info = expr->resolved_type->info->as<TupleTypeInfo>();

    static int temp_idx = 0;

    for (size_t i = 0; i < info->types.size(); ++i) {
      Type *type = info->types[i];
      auto element = ast_alloc<ASTUnpackElement>();
      element->tuple.source_temp_id = "$unpack" + std::to_string(temp_idx);
      element->span = node->span;
      element->tuple.source_tuple = node->expression;
      element->resolved_type = type;
      element->tuple.element_index = i;
      element->tag = ASTUnpackElement::TUPLE_ELEMENT;

      if (current_expression_list.is_null()) {
        throw_error("INTERNAL_COMPILER_ERROR: Used an unpack statement but the current expression list was null", node->span);
        return;
      }

      if (current_expression_list.get()->size() == 0) {
        new (current_expression_list.get()) std::vector<ASTExpr *>();
      }

      current_expression_list.get()->push_back(element);
    }

    temp_idx++;
  } else if (expr->get_node_type() == AST_NODE_RANGE) {
    ASTRange *range = (ASTRange *)expr;

    auto left = interpret_from_ast(range->left, ctx);
    auto right = interpret_from_ast(range->right, ctx);

    if (left->value_type != ValueType::INTEGER || right->value_type != ValueType::INTEGER) {
      throw_error("currently, you can only use integer literals in range unpack expressions", node->span);
    }

    auto left_integer = left->as<IntValue>()->value;
    auto right_integer = right->as<IntValue>()->value;

    for (size_t i = left_integer; left_integer > right_integer ? i < right_integer : i < left_integer;
         left_integer < right_integer ? ++i : i--) {
      ASTUnpackElement *element = ast_alloc<ASTUnpackElement>();
      element->span = node->span;
      ASTLiteral *literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Integer;
      literal->value = std::to_string(i);
      literal->span = node->span;
      literal->resolved_type = s32_type();
      element->range_literal_value = literal;
      element->tag = ASTUnpackElement::RANGE_ELEMENT;
      element->resolved_type = s32_type();

      if (current_expression_list.is_null()) {
        throw_error("INTERNAL_COMPILER_ERROR: Used an unpack statement but the current expression list was null", node->span);
        return;
      }

      current_expression_list.get()->push_back(element);
    }

  } else {
    throw_error(
        "currently, you can only use an unpack expression '...<expr>' on a tuples and ranges (literals only right "
        "now).",
        node->span);
  }
}

void Typer::visit(ASTUnpackElement *) {
  // Do nothing. this is essentially a placeholder, until emit time.
}

void Typer::visit(ASTRun *node) { node->node_to_run->accept(this); }

void Typer::visit(ASTForCStyle *node) {
  ENTER_SCOPE(node->scope);
  // the init is always implicitly mut, just so it's a little more ergonomic.
  if (node->initialization->mutability != MUT) {
    node->initialization->mutability = MUT;
  }
  node->initialization->accept(this);
  node->condition->accept(this);
  node->increment->accept(this);
  node->block->accept(this);
};