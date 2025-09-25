#include "constexpr.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "value.hpp"
#include "libffi.hpp"

Value *evaluate_constexpr(ASTExpr *node, Context &ctx) {
  CTInterpreter interpeter(ctx);
  return interpeter.visit(node);
}

Value *CTInterpreter::visit_path(ASTPath *node) {
  auto symbol = ctx.get_symbol(node).get();

  if (!symbol) {
    ENTER_SCOPE(current_scope);
    symbol = ctx.get_symbol(node).get();
  }

  if (!symbol) {
    return NullV();
  }

  if (symbol->value == nullptr) {
    if (symbol->is_variable && symbol->variable.initial_value.get()) {
      symbol->value = visit(symbol->variable.initial_value.get());
    }
    
    if (symbol->is_function && symbol->function.declaration) {
      if (symbol->function.declaration->is_extern) {
        symbol->value = try_link_extern_function(symbol);
      } else {
        symbol->value = visit(symbol->function.declaration);
      }
    }
  }

  if (symbol->value == nullptr) {
    return NullV();
  }

  return symbol->value;
}

Value *CTInterpreter::visit_block(ASTBlock *node) {
  ENTER_SCOPE(node->scope);
  for (const auto &stmt : node->statements) {
    auto result = visit(stmt);
    if (result->value_type == ValueType::RETURN) {
      auto value = result->as<ReturnValue>()->value;
      if (value) {
        return value.get();
      }
      return NullV();
    }
  }
  return NullV();
}

Value *CTInterpreter::visit_expr_statement(ASTExprStatement *stmt) { return visit(stmt->expression); }

Value *CTInterpreter::visit_bin_expr(ASTBinExpr *node) {
  auto left = visit(node->left);
  auto right = visit(node->right);

  auto is_int = [](Value *v) { return v->get_value_type() == ValueType::INTEGER; };
  auto is_float = [](Value *v) { return v->get_value_type() == ValueType::FLOATING; };
  auto is_bool = [](Value *v) { return v->get_value_type() == ValueType::BOOLEAN; };

  constexpr auto to_int = [](Value *v) -> long long {
    if (v->get_value_type() == ValueType::INTEGER) return ((IntValue *)v)->value;
    if (v->get_value_type() == ValueType::FLOATING) return (long long)((FloatValue *)v)->value;
    return 0;
  };

  constexpr auto to_double = [](Value *v) -> double {
    if (v->get_value_type() == ValueType::FLOATING) return ((FloatValue *)v)->value;
    if (v->get_value_type() == ValueType::INTEGER) return (double)((IntValue *)v)->value;
    return 0.0;
  };

  switch (node->op) {
    case TType::Add: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return FloatV(a + b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((size_t)(a + b));
    }
    case TType::Sub: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return FloatV(a - b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a - b));
    }
    case TType::Mul: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return FloatV(a * b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a * b));
    }
    case TType::Div: {
      if (is_float(left) || is_float(right)) {
        double b = to_double(right);
        double a = to_double(left);
        return FloatV(a / b);
      }
      long long b = to_int(right);
      long long a = to_int(left);
      return IntV((a / b));
    }
    case TType::Modulo: {
      long long b = to_int(right);
      long long a = to_int(left);
      return IntV((a % b));
    }
    case TType::Or: {
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a | b));
    }
    case TType::And: {
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a & b));
    }
    case TType::Xor: {
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a ^ b));
    }
    case TType::SHL: {
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a << b));
    }
    case TType::SHR: {
      long long a = to_int(left);
      long long b = to_int(right);
      return IntV((a >> b));
    }
    case TType::LogicalOr: {
      bool a = ((BoolValue *)left)->value;
      bool b = ((BoolValue *)right)->value;
      return BoolV(a || b);
    }
    case TType::LogicalAnd: {
      bool a = ((BoolValue *)left)->value;
      bool b = ((BoolValue *)right)->value;
      return BoolV(a && b);
    }
    case TType::LT: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a < b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a < b);
      }
      return NullV();
    }
    case TType::GT: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a > b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a > b);
      }
      return NullV();
    }
    case TType::LE: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a <= b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a <= b);
      }
      return NullV();
    }
    case TType::GE: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a >= b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a >= b);
      }
      return NullV();
    }
    case TType::EQ: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a == b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a == b);
      }
      if (is_bool(left) && is_bool(right)) {
        bool a = ((BoolValue *)left)->value;
        bool b = ((BoolValue *)right)->value;
        return BoolV(a == b);
      }
      return NullV();
    }
    case TType::NEQ: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return BoolV(a != b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return BoolV(a != b);
      }
      if (is_bool(left) && is_bool(right)) {
        bool a = ((BoolValue *)left)->value;
        bool b = ((BoolValue *)right)->value;
        return BoolV(a != b);
      }
      return NullV();
    }

    case TType::CompAdd:
    case TType::CompSub:
    case TType::CompMul:
    case TType::CompDiv:
    case TType::CompMod:
    case TType::CompAnd:
    case TType::CompOr:
    case TType::CompXor:
    case TType::CompSHL:
    case TType::CompSHR: {
      auto old_op = node->op;
      TType base_op = TType::Add;
      switch (old_op) {
        case TType::CompAdd:
          base_op = TType::Add;
          break;
        case TType::CompSub:
          base_op = TType::Sub;
          break;
        case TType::CompMul:
          base_op = TType::Mul;
          break;
        case TType::CompDiv:
          base_op = TType::Div;
          break;
        case TType::CompMod:
          base_op = TType::Modulo;
          break;
        case TType::CompAnd:
          base_op = TType::And;
          break;
        case TType::CompOr:
          base_op = TType::Or;
          break;
        case TType::CompXor:
          base_op = TType::Xor;
          break;
        case TType::CompSHL:
          base_op = TType::SHL;
          break;
        case TType::CompSHR:
          base_op = TType::SHR;
          break;
        default:
          base_op = old_op;
          break;
      }

      // temporarily replace op and evaluate as a normal binary expr
      node->op = base_op;
      Value *res = visit_bin_expr(node);
      node->op = old_op;

      if (node->left->get_node_type() == AST_NODE_PATH) {
        auto path = (ASTPath *)node->left;
        auto symbol = ctx.get_symbol(path).get();
        if (symbol) {
          symbol->value = res;
        }
      }

      return res;
    }
    default:
      break;
  }
  return NullV();
}

Value *CTInterpreter::visit_unary_expr(ASTUnaryExpr *node) {
  auto unary = (ASTUnaryExpr *)node;
  auto operand = evaluate_constexpr(unary->operand, ctx);

  switch (unary->op) {
    case TType::Sub: {
      if (operand->get_value_type() == ValueType::FLOATING) {
        double v = ((FloatValue *)operand)->value;
        return FloatV(-v);
      }
      if (operand->get_value_type() == ValueType::INTEGER) {
        long long v = ((IntValue *)operand)->value;
        return IntV((-v));
      }
      return NullV();
    }
    case TType::LogicalNot: {
      if (operand->get_value_type() == ValueType::BOOLEAN) {
        bool v = ((BoolValue *)operand)->value;
        return BoolV(!v);
      }
      return NullV();
    }
    case TType::Not: {
      if (operand->get_value_type() == ValueType::INTEGER) {
        long long v = ((IntValue *)operand)->value;
        return IntV((~v));
      }
      return NullV();
    }
    default:
      return NullV();
  }

  return NullV();
}

Value *CTInterpreter::visit_literal(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer:
      return IntV(node->value);
    case ASTLiteral::Float:
      return FloatV(node->value);
    case ASTLiteral::Bool:
      return BoolV(node->value);
    case ASTLiteral::Char:
      return CharV(node->value.get_str()[0]);
    case ASTLiteral::String:
    case ASTLiteral::MultiLineString:
      return StringV(node->value);
    default:
      return NullV();
  }
}

Value *CTInterpreter::visit_for(ASTFor *node) {
  InternedString loop_var_name;
  if (node->left_tag == ASTFor::IDENTIFIER) {
    loop_var_name = node->left.identifier;
  } else {
    return NullV();
  }

  ASTRange *range;
  if (node->right->get_node_type() == AST_NODE_RANGE) {
    range = (ASTRange *)node->right;
  } else {
    return NullV();
  }

  auto left_v = visit(range->left);
  auto right_v = visit(range->right);

  long long left = 0, right = 0;
  if (left_v->get_value_type() == ValueType::INTEGER) {
    left = ((IntValue *)left_v)->value;
  } else if (left_v->get_value_type() == ValueType::FLOATING) {
    left = (long long)((FloatValue *)left_v)->value;
  } else {
    return NullV();
  }

  if (right_v->get_value_type() == ValueType::INTEGER) {
    right = ((IntValue *)right_v)->value;
  } else if (right_v->get_value_type() == ValueType::FLOATING) {
    right = (long long)((FloatValue *)right_v)->value;
  } else {
    return NullV();
  }

  (void)left;
  (void)right;

  current_scope->insert_local_variable(loop_var_name, s32_type(), nullptr, MUT);

  for (int i = left; i < right; ++i) {
    set_value(loop_var_name, IntV(i));
    auto result = visit(node->block);
    if (result->get_value_type() == ValueType::RETURN) {
      auto return_v = result->as<ReturnValue>();
      if (return_v->value.is_not_null()) {
        return return_v->value.get();
      }
      return NullV();
    }
  }

  return NullV();
}

Value *CTInterpreter::visit_return(ASTReturn *node) {
  if (node->expression) {
    return ReturnV(visit(node->expression.get()));
  }
  return ReturnV();
}

Value *CTInterpreter::visit_struct_declaration(ASTStructDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_function_declaration(ASTFunctionDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_choice_declaration(ASTChoiceDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_enum_declaration(ASTEnumDeclaration *) { return NullV(); }

Value *CTInterpreter::visit_program(ASTProgram *) { return NullV(); }

Value *CTInterpreter::visit_call(ASTCall *call) {
  auto function = visit(call->callee);

  std::vector<Value *> evaluated_args;
  evaluated_args.reserve(call->arguments->arguments.size());

  for (const auto &arg : call->arguments->arguments) {
    evaluated_args.push_back(visit(arg));
  }

  if (function->get_value_type() == ValueType::EXTERN_FUNCTION) {
    auto extern_function = function->as<ExternFunctionValue>();
    auto result = compile_time_ffi_dispatch(extern_function->name, extern_function->info, evaluated_args);
    return result ? result: NullV();
  }

  return NullV();
}
Value *CTInterpreter::visit_method_call(ASTMethodCall *) { return NullV(); }

Value *CTInterpreter::visit_dot_expr(ASTDotExpr *) { return NullV(); }
Value *CTInterpreter::visit_index(ASTIndex *) { return NullV(); }
Value *CTInterpreter::visit_initializer_list(ASTInitializerList *) { return NullV(); }
Value *CTInterpreter::visit_range(ASTRange *) { return NullV(); }
Value *CTInterpreter::visit_tuple(ASTTuple *) { return NullV(); }
Value *CTInterpreter::visit_lambda(ASTLambda *) { return NullV(); }
Value *CTInterpreter::visit_cast(ASTCast *) { return NullV(); }
Value *CTInterpreter::visit_size_of(ASTSize_Of *) { return NullV(); }
Value *CTInterpreter::visit_dyn_of(ASTDyn_Of *) { return NullV(); }
Value *CTInterpreter::visit_type_of(ASTType_Of *) { return NullV(); }

Value *CTInterpreter::visit_pattern_match(ASTPatternMatch *) { return NullV(); }

Value *CTInterpreter::visit_variable(ASTVariable *variable) {
  current_scope->insert_local_variable(variable->name, variable->resolved_type, variable->value.get(),
                                       variable->mutability);
  return NullV();
}

Value *CTInterpreter::visit_tuple_deconstruction(ASTDestructure *) { return NullV(); }

Value *CTInterpreter::visit_defer(ASTDefer *) { return NullV(); }
Value *CTInterpreter::visit_switch(ASTSwitch *) { return NullV(); }
Value *CTInterpreter::visit_continue(ASTContinue *) { return NullV(); }
Value *CTInterpreter::visit_break(ASTBreak *) { return NullV(); }
Value *CTInterpreter::visit_if(ASTIf *) { return NullV(); }
Value *CTInterpreter::visit_else(ASTElse *) { return NullV(); }
Value *CTInterpreter::visit_while(ASTWhile *) { return NullV(); }

Value *CTInterpreter::visit_module(ASTModule *) { return NullV(); }
Value *CTInterpreter::visit_import(ASTImport *) { return NullV(); }
Value *CTInterpreter::visit_impl(ASTImpl *) { return NullV(); }

void CTInterpreter::set_value(InternedString &name, Value *value) {
  auto symbol = ctx.scope->lookup(name);
  if (symbol) {
    symbol->value = value;
  }
  symbol = current_scope->lookup(name);
  if (symbol) {
    symbol->value = value;
  }
}

Value *CTInterpreter::try_link_extern_function(Symbol *symbol) {
  auto function = symbol->function.declaration;
  auto fti = function->resolved_type->info->as<FunctionTypeInfo>();
  return value_arena_alloc<ExternFunctionValue>(function->name, fti);
}
