#include "constexpr.hpp"
#include <print>
#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "value.hpp"
#include "libffi.hpp"

Value *evaluate_constexpr(ASTExpr *node, Context &ctx) {
  CTInterpreter interpeter(ctx);
  return interpeter.visit(node);
}

Value *CTInterpreter::visit_path(ASTPath *node) {
  auto lvalue = get_lvalue(node);
  if (lvalue->kind == LValue::RAW) {
    auto raw = lvalue->raw;
    return raw;
  } else {
    return *lvalue->managed;
  }
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
      return null_value();
    }
  }
  return null_value();
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
        return new_float(a + b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((size_t)(a + b));
    }
    case TType::Sub: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_float(a - b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a - b));
    }
    case TType::Mul: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_float(a * b);
      }
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a * b));
    }
    case TType::Div: {
      if (is_float(left) || is_float(right)) {
        double b = to_double(right);
        double a = to_double(left);
        return new_float(a / b);
      }
      long long b = to_int(right);
      long long a = to_int(left);
      return new_int((a / b));
    }
    case TType::Modulo: {
      long long b = to_int(right);
      long long a = to_int(left);
      return new_int((a % b));
    }
    case TType::Or: {
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a | b));
    }
    case TType::And: {
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a & b));
    }
    case TType::Xor: {
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a ^ b));
    }
    case TType::SHL: {
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a << b));
    }
    case TType::SHR: {
      long long a = to_int(left);
      long long b = to_int(right);
      return new_int((a >> b));
    }
    case TType::LogicalOr: {
      bool a = ((BoolValue *)left)->value;
      bool b = ((BoolValue *)right)->value;
      return new_bool(a || b);
    }
    case TType::LogicalAnd: {
      bool a = ((BoolValue *)left)->value;
      bool b = ((BoolValue *)right)->value;
      return new_bool(a && b);
    }
    case TType::LT: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a < b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a < b);
      }
      return null_value();
    }
    case TType::GT: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a > b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a > b);
      }
      return null_value();
    }
    case TType::LE: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a <= b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a <= b);
      }
      return null_value();
    }
    case TType::GE: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a >= b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a >= b);
      }
      return null_value();
    }
    case TType::EQ: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a == b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a == b);
      }
      if (is_bool(left) && is_bool(right)) {
        bool a = ((BoolValue *)left)->value;
        bool b = ((BoolValue *)right)->value;
        return new_bool(a == b);
      }
      return null_value();
    }
    case TType::NEQ: {
      if (is_float(left) || is_float(right)) {
        double a = to_double(left);
        double b = to_double(right);
        return new_bool(a != b);
      }
      if (is_int(left) && is_int(right)) {
        long long a = to_int(left);
        long long b = to_int(right);
        return new_bool(a != b);
      }
      if (is_bool(left) && is_bool(right)) {
        bool a = ((BoolValue *)left)->value;
        bool b = ((BoolValue *)right)->value;
        return new_bool(a != b);
      }
      return null_value();
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

      // make it permanent
      auto lvalue = get_lvalue(node->left);
      switch (lvalue->kind) {
        case LValue::MANAGED:
          *lvalue->managed = right;
          break;
        case LValue::RAW:
          lvalue->raw->assign_from(right);
          break;
      }
      return res;
    }
    case TType::Assign: {
      auto lvalue = get_lvalue(node->left);
      switch (lvalue->kind) {
        case LValue::MANAGED:
          *lvalue->managed = right;
          break;
        case LValue::RAW:
          lvalue->raw->assign_from(right);
          break;
      }
      return lvalue;
    }
    default:
      break;
  }

  return null_value();
}

Value *CTInterpreter::visit_unary_expr(ASTUnaryExpr *node) {
  auto operand = visit(node->operand);
  switch (node->op) {
    case TType::Sub: {
      if (operand->get_value_type() == ValueType::FLOATING) {
        double v = ((FloatValue *)operand)->value;
        return new_float(-v);
      }
      if (operand->get_value_type() == ValueType::INTEGER) {
        long long v = ((IntValue *)operand)->value;
        return new_int((-v));
      }
      return null_value();
    }
    case TType::LogicalNot: {
      if (operand->get_value_type() == ValueType::BOOLEAN) {
        bool v = ((BoolValue *)operand)->value;
        return new_bool(!v);
      }
      return null_value();
    }
    case TType::Not: {
      if (operand->get_value_type() == ValueType::INTEGER) {
        long long v = ((IntValue *)operand)->value;
        return new_int((~v));
      }
      return null_value();
    }
    case TType::And: {
      auto lvalue = get_lvalue(node->operand);
      return new_pointer(lvalue->managed);
    }
    case TType::Mul: {
      bool is_pointer = operand->get_value_type() == ValueType::POINTER,
           is_raw_pointer = operand->get_value_type() == ValueType::RAW_POINTER;

      if (!is_pointer && !is_raw_pointer) {
        throw_error(
            std::format(
                "cannot dereference a non-pointer, somehow the compile time interpreter got this mixed up, got a {}",
                (int)operand->get_value_type()),
            node->source_range);
      }

      if (is_pointer) {
        PointerValue *pointer = operand->as<PointerValue>();
        return *pointer->ptr;
      } else if (is_raw_pointer) {
        RawPointerValue *pointer = operand->as<RawPointerValue>();
        return pointer->dereference();
      }
    }
    default:
      return null_value();
  }
  return null_value();
}

Value *CTInterpreter::visit_literal(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer:
      return new_int(node->value);
    case ASTLiteral::Float:
      return new_float(node->value);
    case ASTLiteral::Bool:
      return new_bool(node->value);
    case ASTLiteral::Char:
      return new_char(node->value.get_str()[0]);
    case ASTLiteral::String:
    case ASTLiteral::MultiLineString:
      return new_string(node->value);
    default:
      return null_value();
  }
}

// TODO: handle iterators.
Value *CTInterpreter::visit_for(ASTFor *node) {
  InternedString loop_var_name;

  if (node->left_tag == ASTFor::IDENTIFIER) {
    loop_var_name = node->left.identifier;
  } else {
    throw_error("loop destructures not implemented in compile time", node->source_range);
  }

  ASTRange *range;
  if (node->right->get_node_type() == AST_NODE_RANGE) {
    range = (ASTRange *)node->right;
  } else {
    return null_value();
  }

  auto left_v = visit(range->left);
  auto right_v = visit(range->right);

  long long left = 0, right = 0;
  if (left_v->get_value_type() == ValueType::INTEGER) {
    left = ((IntValue *)left_v)->value;
  } else if (left_v->get_value_type() == ValueType::FLOATING) {
    left = (long long)((FloatValue *)left_v)->value;
  } else {
    return null_value();
  }

  if (right_v->get_value_type() == ValueType::INTEGER) {
    right = ((IntValue *)right_v)->value;
  } else if (right_v->get_value_type() == ValueType::FLOATING) {
    right = (long long)((FloatValue *)right_v)->value;
  } else {
    return null_value();
  }

  (void)left;
  (void)right;

  current_scope->insert_local_variable(loop_var_name, s32_type(), nullptr, MUT);

  for (int i = left; i < right; ++i) {
    set_value(loop_var_name, new_int(i));
    auto result = visit(node->block);
    if (result->get_value_type() == ValueType::RETURN) {
      auto return_v = result->as<ReturnValue>();
      if (return_v->value.is_not_null()) {
        return return_v->value.get();
      }
      return null_value();
    }
  }

  return null_value();
}

Value *CTInterpreter::visit_continue(ASTContinue *) { return continue_value(); }

Value *CTInterpreter::visit_break(ASTBreak *) { return break_value(); }

Value *CTInterpreter::visit_return(ASTReturn *node) {
  if (node->expression) {
    return return_value(visit(node->expression.get()));
  }
  return return_value();
}

Value *CTInterpreter::visit_function_declaration(ASTFunctionDeclaration *node) {
  if (node->is_forward_declared) {
    return null_value();
  }

  auto function = value_arena_alloc<FunctionValue>();
  function->parameters = node->params;
  function->block = node->block.get();

  return function;
}

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
    return result ? result : null_value();
  } else if (function->get_value_type() == ValueType::FUNCTION) {
    return function->as<FunctionValue>()->call(this, evaluated_args);
  }
  if (call->callee->get_node_type() == AST_NODE_PATH && call->callee->resolved_type &&
      call->callee->resolved_type->is_kind(TYPE_CHOICE)) {
    const auto path = (ASTPath *)call->callee;
    const auto choice_type = path->resolved_type;
    const auto info = choice_type->info->as<ChoiceTypeInfo>();
    const auto last_segment = path->segments.back();
    const auto variant_name = last_segment.identifier;
    const auto variant_type = info->get_variant_type(variant_name);

    auto tuple = default_value_of_t(variant_type, this)->as<ObjectValue>();
    auto choice = default_value_of_t(choice_type, this)->as<ObjectValue>();
    choice->values["index"] = new_int(info->get_variant_discriminant(variant_name));
    for (size_t i = 0; i < evaluated_args.size(); ++i) {
      std::println("{} = {}", i, evaluated_args[i]->to_string());
      tuple->values[std::format("${}", i)] = evaluated_args[i];
    }
    choice->values[std::format("${}", variant_name)] = tuple;
    return choice;
  } else {
    throw_error("Unable to call non-function symbol", call->source_range);
  }
  return null_value();
}

Value *CTInterpreter::visit_dot_expr(ASTDotExpr *dot) {
  LValue *lvalue = get_dot_expr_lvalue(dot);
  if (lvalue->kind == LValue::MANAGED) {
    return *lvalue->managed;
  } else if (lvalue->kind == LValue::RAW) {
    return lvalue->raw->dereference();
  }
  return null_value();
}

Value *CTInterpreter::visit_index(ASTIndex *node) {
  LValue *lvalue = get_index_lvalue(node);
  if (lvalue->kind == LValue::MANAGED) {
    return *lvalue->managed;
  } else if (lvalue->kind == LValue::RAW) {
    return lvalue->raw->dereference();
  }
  return null_value();
}

LValue *CTInterpreter::get_dot_expr_lvalue(ASTDotExpr *dot) {
  Value *base = visit(dot->base);
  static Value *the_null_value = null_value();

  // Auto dereference
  if (base->get_value_type() == ValueType::POINTER) {
    base = *base->as<PointerValue>()->ptr;
  } else if (base->get_value_type() == ValueType::RAW_POINTER) {
    base = base->as<RawPointerValue>()->dereference();
  }

  if (base->get_value_type() != ValueType::OBJECT) {
    throw_error("cannot use '.' expression on a non-object at compile time", dot->source_range);
  }

  ObjectValue *object = (ObjectValue *)base;

  if (object->values.contains(dot->member.identifier)) {
    return new_lvalue(&object->values[dot->member.identifier]);
  } else {
    throw_warning(WARNING_COUNT,
                  std::format("Accessing a dot expression during compile time, but the object didn't have a property "
                              "{}, so it got ignored",
                              dot->member.identifier),
                  dot->source_range);
    return new_lvalue(&the_null_value);
  }
}

// This is just for assigning via a *x = 0 kinda dereference.
LValue *CTInterpreter::get_unary_lvalue(ASTUnaryExpr *node) {
  auto operand = visit(node->operand);

  if (node->op == TType::Mul) {
    bool is_pointer = operand->get_value_type() == ValueType::POINTER,
         is_raw_pointer = operand->get_value_type() == ValueType::RAW_POINTER;

    if (!is_pointer && !is_raw_pointer) {
      throw_error(
          std::format(
              "cannot dereference a non-pointer, somehow the compile time interpreter got this mixed up, got a {}",
              (int)operand->get_value_type()),
          node->source_range);
    }

    if (is_pointer) {
      PointerValue *pointer = operand->as<PointerValue>();
      return new_lvalue(pointer->ptr);
    } else if (is_raw_pointer) {
      RawPointerValue *pointer = operand->as<RawPointerValue>();
      return new_lvalue(pointer);
    }
  }
  static Value *the_null_value = null_value();
  return new_lvalue(&the_null_value);
}

LValue *CTInterpreter::get_index_lvalue(ASTIndex *node) {
  Value *base = visit(node->base);
  IntValue *index_v = (IntValue *)visit(node->index);
  size_t index = index_v->value;

  if (base->get_value_type() == ValueType::POINTER && node->is_pointer_subscript) {
    // This shouldn't ever happen.. but I guess? we'll assume it's valid. but this basically says this is legal:
    /*
      int x = 0;
      int *ptr = &x;
      ptr[0] = 10;
    */
    // Why would you ever do this?
    PointerValue *pv = base->as<PointerValue>();
    Value **p = &pv->ptr[index];
    return new_lvalue(p);
  } else if (base->get_value_type() == ValueType::RAW_POINTER && node->is_pointer_subscript) {
    RawPointerValue *raw = base->as<RawPointerValue>();
    const size_t sz = raw->type->base_type->size_in_bytes();
    char *p = raw->ptr + (sz * index);
    // TODO: see the RawPointerValue::dereference() to see why this happens, and why it shouldnt happen.
    return new_lvalue(new_raw_pointer(raw->type->base_type, p));
  }

  if (base->get_value_type() != ValueType::ARRAY) {
    throw_error("cannot index-- operator overloading not implemented at compile time", node->source_range);
  }

  ArrayValue *array = (ArrayValue *)base;
  return new_lvalue(&array->values[index]);
}

LValue *CTInterpreter::get_lvalue(ASTNode *node) {
  if (node->get_node_type() == AST_NODE_UNARY_EXPR) {
    return get_unary_lvalue((ASTUnaryExpr *)node);
  }
  if (node->get_node_type() == AST_NODE_DOT_EXPR) {
    return get_dot_expr_lvalue((ASTDotExpr *)node);
  }
  if (node->get_node_type() == AST_NODE_INDEX) {
    return get_index_lvalue((ASTIndex *)node);
  }

  static Value *the_null_value = null_value();

  Symbol *symbol;
  {
    ENTER_SCOPE(current_scope);
    symbol = ctx.get_symbol(node).get();
  }

  if (!symbol) {
    symbol = ctx.get_symbol(node).get();
  }

  if (!symbol) {
    return new_lvalue(&the_null_value);
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
    return new_lvalue(&the_null_value);
  }

  return new_lvalue(&symbol->value);
}

Value *CTInterpreter::visit_range(ASTRange *node) {
  ObjectValue *object = (ObjectValue *)default_value_of_t(node->resolved_type, this);
  object->values["begin"] = visit(node->left);
  object->values["end"] = visit(node->right);
  return object;
}

Value *CTInterpreter::visit_tuple(ASTTuple *node) {
  ObjectValue *object = (ObjectValue *)default_value_of_t(node->resolved_type, this);
  for (size_t i = 0; i < node->values.size(); ++i) {
    const auto &value = node->values[i];
    object->values[std::format("${}", i)] = visit(value);
  }
  return object;
}

Value *CTInterpreter::visit_method_call(ASTMethodCall *node) {
  auto func_symbol = ctx.get_symbol(node->callee).get();
  auto func_decl = func_symbol->function.declaration;
  auto function = visit(func_decl);

  if (function->get_value_type() != ValueType::FUNCTION) {
    throw_error("Tried to .call() a non-function during compile time", node->source_range);
  }

  auto fn = function->as<FunctionValue>();

  Value *self_val = visit(node->callee->base);

  std::vector<Value *> args;
  args.push_back(self_val);
  for (auto *arg_node : node->arguments->arguments) {
    args.push_back(visit(arg_node));
  }

  auto result = fn->call(this, args);

  {  // this should always be managed. 'self' will never be a raw pointer.
    auto self_ptr = get_lvalue(node->callee->base);
    *self_ptr->managed = self_val;
  }

  return result;
}

Value *CTInterpreter::visit_initializer_list(ASTInitializerList *ini) {
  if (ini->tag == ASTInitializerList::INIT_LIST_NAMED) {
    ObjectValue *object = (ObjectValue *)default_value_of_t(ini->resolved_type, this);
    for (const auto &[key, value] : ini->key_values) {
      object->values[key] = visit(value);
    }
    return object;
  } else {
    ArrayValue *array = new_array(ini->resolved_type);
    for (const auto &value : ini->values) {
      array->values.push_back(visit(value));
    }
    return array;
  }
}

Value *CTInterpreter::visit_lambda(ASTLambda *node) {
  // Might have to do more here.
  auto function = value_arena_alloc<FunctionValue>();
  function->block = node->block;
  function->parameters = node->params;
  return function;
}

Value *CTInterpreter::visit_cast(ASTCast *node) {
  // TODO: do we even have to do any casting here? probably float<->int and other scalars just to make sense?
  return visit(node->expression);
}

Value *CTInterpreter::visit_size_of(ASTSize_Of *node) {
  return new_int(node->target_type->resolved_type->size_in_bytes());
}

Value *CTInterpreter::visit_type_of(ASTType_Of *) {
  ObjectValue *object =
      (ObjectValue *)default_value_of_t(ctx.root_scope->find_type_id("Type", {{TYPE_EXT_POINTER_CONST}}), this);
  return object;
}

Value *CTInterpreter::visit_variable(ASTVariable *variable) {
  current_scope->insert_local_variable(variable->name, variable->type->resolved_type, nullptr, variable->mutability);

  auto symbol = current_scope->local_lookup(variable->name);

  if (variable->value) {
    symbol->value = visit(variable->value.get());
  } else {
    symbol->value = default_value_of_t(variable->type->resolved_type, this);
  }

  // TODO: handle other implicit casts.
  if (symbol->value) {
    symbol->value->type = variable->type->resolved_type;
  }

  return null_value();
}

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

Value *CTInterpreter::visit_impl(ASTImpl *node) {
  throw_error("creating \"impl\"'s in a #run or #eval directive is not allowed", node->source_range);
  return null_value();
}

Value *CTInterpreter::visit_if(ASTIf *node) {
  bool cond;

  if (node->condition->get_node_type() == AST_NODE_PATTERN_MATCH) {
    auto cond_value = visit(node->condition);
    cond = cond_value->is_truthy();

    if (cond) {
      // we already executed the then block.
      return null_value();
    }

  } else {
    auto cond_value = visit(node->condition);
    cond = cond_value->is_truthy();
  }

  if (cond) {
    return visit(node->block);
  } else if (node->_else) {
    return visit(node->_else.get());
  }

  return null_value();
}

Value *CTInterpreter::visit_while(ASTWhile *node) {
  const auto should_continue = [&]() {
    if (node->condition.is_not_null()) {
      return visit(node->condition.get())->is_truthy();
    }
    return true;
  };

  while (should_continue()) {
    auto result = visit(node->block);
    if (result->get_value_type() == ValueType::BREAK) {
      break;
    }
    if (result->get_value_type() == ValueType::RETURN) {
      return result;
    }
  }
  return null_value();
}

Value *CTInterpreter::visit_else(ASTElse *node) {
  if (node->_if) {
    return visit(node->_if.get());
  }
  if (node->block) {
    return visit(node->block.get());
  }
  return null_value();
}

Value *CTInterpreter::visit_pattern_match(ASTPatternMatch *node) {
  Value *object = visit(node->object);
  Type *target_type = node->target_type_path->resolved_type;

  if (object->type == target_type) {
    visit(node->target_block);
    return new_bool(false);
  }

  return new_bool(true);
}

Value *CTInterpreter::visit_unpack_element(ASTUnpackElement *) { return null_value(); }
Value *CTInterpreter::visit_unpack(ASTUnpack *) { return null_value(); }
Value *CTInterpreter::visit_tuple_deconstruction(ASTDestructure *) { return null_value(); }
Value *CTInterpreter::visit_defer(ASTDefer *) { return null_value(); }
Value *CTInterpreter::visit_switch(ASTSwitch *) { return null_value(); }
Value *CTInterpreter::visit_dyn_of(ASTDyn_Of *) { return null_value(); }