
#include <cassert>
#include <csetjmp>
#include <format>
#include <string>
#include <vector>

#include "ast.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"


/* 
  TODO: these require some more intense refactoring, due to the lack of Arguments/Parameters nodes now.
*/

void Typer::type_check_args_from_params(AST *call, AST *parameters, bool skip_first) {
  auto old_type = expected_type;
  Defer _([&]() { expected_type = old_type; });
  auto args_ct = node->arguments.size();
  auto params_ct = params->params.size();
  auto largest = args_ct > params_ct ? args_ct : params_ct;
  int param_index = skip_first ? 1 : 0;
  for (int arg_index = 0; arg_index < largest; ++arg_index, ++param_index) {
    if (param_index < params_ct) {
      if (arg_index < args_ct) {
        expected_type = params->params[param_index]->resolved_type;
        node->arguments[arg_index]->accept(this);
        assert_types_can_cast_or_equal(
            node->arguments[arg_index]->resolved_type, params->params[param_index]->resolved_type,
            node->arguments[arg_index]->source_range,
            std::format("unexpected argument type.. parameter #{} of function",
                        arg_index + 1)); // +1 here to make it 1 based indexing for user. more intuitive
      }
    } else {
      if (arg_index < args_ct) {
        expected_type = Type::INVALID_TYPE_ID;
        node->arguments[arg_index]->accept(this);
        if (!params->is_varargs) {
          throw_error("Too many arguments to function", node->source_range);
        }
      }
    }
  }
}

void Typer::type_check_args_from_info(AST *call, Function_Info *info) {
  auto old_type = expected_type;
  Defer _([&]() { expected_type = old_type; });
  auto args_ct = node->arguments.size();
  // TODO: rewrite this. this is so hard tor read.
  if ((args_ct > info->params_len && !info->is_varargs) || args_ct < info->params_len) {
    throw_error(
        std::format("Function call has incorrect number of arguments. Expected: {}, Found: {}... function type: {}",
                    info->params_len, args_ct, info->to_string()),
        node->source_range);
  }

  for (int i = 0; i < args_ct; ++i) {
    auto arg = node->arguments[i];
    expected_type = info->parameter_types[i];
    arg->accept(this);
    if (i < info->params_len) {
      assert_types_can_cast_or_equal(arg->resolved_type, info->parameter_types[i], arg->source_range,
                                     std::format("invalid argument type for parameter #{}", i + 1));
    }
  }
}

AST *Typer::resolve_generic_function_call(AST *call, AST *func) {
  std::vector<int> generic_args;
  if (call->generic_arguments.empty()) {
    call->arguments->accept(this);
    generic_args = call->arguments->resolved_argument_types;
    auto index = 0;
    for (auto generic_arg : generic_args) {
      auto type = ast_alloc<ASTType>();
      type->source_range = node->source_range;
      auto gen_t = global_get_type(generic_arg);

      /*
        * This is auto dereferencing an inferred generic argument when you have a parameter such as T*
        * We do this because it's strange to pass T as s32* if i do func(&s32);
        * it makes it hard to do certain things, and if you wanted to take T as s32*, you'd just not give it a T* in
        your
        * parameter signature.

        * I tried to mke it safer, not sure if i did.
      */

      if (gen_t->meta.is_pointer() && !func->params->params.empty()) {
        // if it == 1, then we skip zero. works out.
        int param_infer_index = func->params->params[0]->tag == ASTParamDecl::Self;
        if (param_infer_index < func->params->params.size() &&
            func->params->params[param_infer_index]->normal.type != nullptr &&
            !func->params->params[param_infer_index]->normal.type->extensions.empty()) {
          type->resolved_type = gen_t->get_element_type();
        } else {
          type->resolved_type = generic_arg;
        }
      } else {
        type->resolved_type = generic_arg;
      }
      node->generic_arguments.push_back(type);
      index++;
    }
  } else {
    generic_args = get_generic_arg_types(call->generic_arguments);
  }
  auto instantiation = visit_generic(&Typer::visit_function_header, func, generic_args);
  if (!instantiation) {
    throw_error("Template instantiation argument count mismatch", call->source_range);
  }
  instantiation->generic_arguments = generic_args;
  visit_function_body(static_cast<AST *>(instantiation));
  return instantiation;
}

void Typer::visit(ASTSubscript *node) {
  node->left->accept(this);
  node->subscript->accept(this);
  auto left_ty = global_get_type(node->left->resolved_type);
  auto subscript_ty = global_get_type(node->subscript->resolved_type);

  auto overload = find_operator_overload(Token_Type::LBrace, left_ty, OPERATION_SUBSCRIPT);
  if (overload != -1) {
    node->is_operator_overload = true;
    node->resolved_type = global_get_type(overload)->info->as<Function_Info>()->return_type;
    return;
  }

  // * Todo: reimplement operator overloads with interfaces.

  auto meta = left_ty->meta;

  if (!meta.is_fixed_sized_array() && !meta.is_pointer()) {
    throw_error(std::format("cannot index into non-array, non-pointer type that doesn't implement 'subscript :: "
                            "fn(self*, idx: u32)' method. {}",
                            left_ty->to_string()),
                node->source_range);
  }

  node->resolved_type = left_ty->get_element_type();
}

void Typer::visit(ASTSwitch *node) {
  node->target->accept(this);
  auto type_id = node->target->resolved_type;
  auto type = global_get_type(type_id);

  if (!type->is_kind(TYPE_SCALAR) && !type->is_kind(TYPE_ENUM) && !type->meta.is_pointer()) {
    auto operator_overload = find_operator_overload(Token_Type::EQ, type, OPERATION_BINARY);
    if (operator_overload == -1) {
      throw_error(
          std::format("Can't use a 'switch' statement/expression on a non-scalar, non-enum type that doesn't implement "
                      "Eq (== operator on #self)\ngot type '{}'",
                      type->to_string()),
          node->target->source_range);
    }
  }

  int return_type = void_type();
  int flags = BLOCK_FLAGS_FALL_THROUGH;

  for (const auto &_case : node->cases) {
    _case.expression->accept(this);
    auto expr_type = _case.expression->resolved_type;
    _case.block->accept(this);
    auto block_cf = _case.block->control_flow;
    flags |= block_cf.flags;
    if ((block_cf.flags & BLOCK_FLAGS_RETURN) != 0) {
      if (return_type != void_type()) {
        assert_return_type_is_valid(return_type, block_cf.type, node);
      }
      return_type = block_cf.type;
    }

    if (type_is_numerical(type)) {
      continue;
    } else {
      assert_types_can_cast_or_equal(expr_type, type_id, node->source_range, "Invalid switch case.");
    }
  }
  node->resolved_type = node->return_type = return_type;
  if (node->is_statement) {
    node->control_flow = Control_Flow{flags, return_type};
  } else {
    if ((flags & BLOCK_FLAGS_BREAK) != 0) {
      throw_warning(WarningSwitchBreak, "You do not need to break from switch cases.", node->source_range);
    } else if ((flags & BLOCK_FLAGS_CONTINUE) != 0) {
      throw_error("Cannot continue from a switch case: it is not a loop.", node->source_range);
    }
  }
}

