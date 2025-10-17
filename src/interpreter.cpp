#include <print>
#include "ast.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "libffi.hpp"
#include "thir_interpreter.hpp"
#include "thir.hpp"
#include "type.hpp"
#include "value.hpp"

Value *interpret_from_ast(ASTNode *node, Context &ctx) {
  THIRGen thir_gen{ctx, false};

  if (!node->resolved_type) {
    Typer typer{ctx};
    node->accept(&typer);
  }

  auto thir = thir_gen.visit_node(node);
  Interpreter interp{ctx};
  return interp.visit_node(thir);
}

Value *interpret(THIR *node, Context &ctx) {
  Interpreter interpeter(ctx);
  return interpeter.visit_node(node);
}

Value *Interpreter::try_link_extern_function(THIRFunction *function) {
  auto fti = function->type->info->as<FunctionTypeInfo>();
  return value_arena_alloc<ExternFunctionValue>(function->name, fti);
}

LValue *Interpreter::get_lvalue(THIR *node) {
  switch (node->get_node_type()) {
    case THIRNodeType::UnaryExpr:
      return get_unary_lvalue((THIRUnaryExpr *)node);
    case THIRNodeType::MemberAccess:
      return get_member_access_lvalue((THIRMemberAccess *)node);
    case THIRNodeType::Index:
      return get_index_lvalue((THIRIndex *)node);
    case THIRNodeType::Variable:
      return get_variable_lvalue((THIRVariable *)node);
    case THIRNodeType::Function: {
      // TODO: fix this
      static Value *function;
      function = visit_node(node);
      return new_lvalue(&function);
    }
    default:
      break;
  }
  return null_lvalue();
}

LValue *Interpreter::get_variable_lvalue(THIRVariable *node) {
  if (!node->value && !node->compile_time_value) {
    if (node->global_initializer_assignment) {
      node->compile_time_value = visit_node(node->global_initializer_assignment->right);
      node->use_compile_time_value_at_emit_time = false;
    } else {
      node->compile_time_value = null_value();
      node->use_compile_time_value_at_emit_time = false;
    }

    // printf("didnt have value nor ctval: %s, defaulting to %s\n", node->name.get_str().c_str(),
    // node->compile_time_value->to_string().c_str()); our first assignment has to have something to write into, so we
    // just give null.
    return new_lvalue(&node->compile_time_value);
  }

  if (!node->compile_time_value) {
    node->compile_time_value = visit_node(node->value);
  }

  // nocheckin
  // node->use_compile_time_value_at_emit_time = true;

  // printf("reading: %s, from %s\n", node->compile_time_value->to_string().c_str(), node->name.get_str().c_str());

  return new_lvalue(&node->compile_time_value);
}

LValue *Interpreter::get_member_access_lvalue(THIRMemberAccess *node) {
  Value *base = visit_node(node->base);

  // Auto dereference
  if (base->get_value_type() == ValueType::POINTER) {
    base = *base->as<PointerValue>()->ptr;
  } else if (base->get_value_type() == ValueType::RAW_POINTER) {
    base = base->as<RawPointerValue>()->dereference();
  }

  if (base->get_value_type() != ValueType::OBJECT) {
    throw_error("cannot use '.' expression on a non-object at compile time. this is likely a compiler bug.",
                node->source_range);
  }

  ObjectValue *object = (ObjectValue *)base;

  if (object->values.contains(node->member)) {
    return new_lvalue(&object->values[node->member]);
  } else {
    throw_error(std::format("A member access expression during compile time read a property that doesnt exist."
                            " member: '{}'",
                            node->member),
                node->source_range);
    return null_lvalue();
  }
}

LValue *Interpreter::get_index_lvalue(THIRIndex *node) {
  Value *base = visit_node(node->base);
  IntValue *index_v = (IntValue *)visit_node(node->index);
  size_t index = index_v->value;

  if (base->get_value_type() == ValueType::POINTER) {
    PointerValue *pv = base->as<PointerValue>();
    Value **p = &pv->ptr[index];
    return new_lvalue(p);
  } else if (base->get_value_type() == ValueType::RAW_POINTER) {
    RawPointerValue *raw = base->as<RawPointerValue>();
    const size_t sz = raw->type->base_type->size_in_bytes();
    char *p = raw->ptr + (sz * index);
    // TODO: see the RawPointerValue::dereference() to see why this happens, and why it shouldnt happen.
    return new_lvalue(new_raw_pointer(raw->type->base_type, p));
  }

  if (base->get_value_type() == ValueType::RAW_POINTER) {
    RawPointerValue *raw = base->as<RawPointerValue>();
    const size_t sz = raw->type->base_type->size_in_bytes();
    char *p = raw->ptr + (sz * index);
    return new_lvalue(new_raw_pointer(raw->type->base_type, p));
  } else if (base->get_value_type() == ValueType::POINTER) {
    PointerValue *pv = base->as<PointerValue>();
    Value **p = &pv->ptr[index];
    return new_lvalue(p);
  } else if (base->get_value_type() != ValueType::ARRAY) {
    throw_error("cannot index-- operator overloading not implemented at compile time", node->source_range);
  }

  ArrayValue *array = (ArrayValue *)base;
  return new_lvalue(&array->values[index]);
}

LValue *Interpreter::get_unary_lvalue(THIRUnaryExpr *node) {
  auto operand = visit_node(node->operand);

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

  return null_lvalue();
}

Value *Interpreter::visit_block(THIRBlock *node) {
  for (const auto &stmt : node->statements) {
    auto result = visit_node(stmt);
    if (result && result->value_type == ValueType::RETURN) {
      auto value = result->as<ReturnValue>()->value;
      if (value) {
        // Propagate returns to function
        return result;
      }
      return null_value();
    }
  }
  return null_value();
}

Value *Interpreter::visit_bin_expr(THIRBinExpr *node) {
  Value *left = visit_node(node->left);
  Value *right = visit_node(node->right);

  if (!left || !right) {
    bool left_null = !left, right_null = !right;
    std::println("left={}, right={}", node->left->source_range.ToString(), node->right->source_range.ToString());
    throw_error(std::format("[INTERPRETER] INTERNAL COMPILER ERROR: one or both binary operands null in expression left null?={}, right null?={}", left_null, right_null), node->source_range);
  }

  const auto vt = [](Value *v) { return v->get_value_type(); };
  const auto is_int = [&](Value *v) { return vt(v) == ValueType::INTEGER; };
  const auto is_float = [&](Value *v) { return vt(v) == ValueType::FLOATING; };
  const auto is_bool = [&](Value *v) { return vt(v) == ValueType::BOOLEAN; };
  const auto is_null = [&](Value *v) { return vt(v) == ValueType::NULLPTR; };
  const auto is_ptr = [&](Value *v) { return vt(v) == ValueType::POINTER || vt(v) == ValueType::RAW_POINTER; };
  const auto is_number = [&](Value *v) { return is_int(v) || is_float(v); };

  const auto to_int = [&](Value *v) -> long long {
    if (is_int(v)) return (long long)v->as<IntValue>()->value;
    if (is_float(v)) return (long long)v->as<FloatValue>()->value;
    return 0;
  };

  const auto to_double = [&](Value *v) -> double {
    if (is_float(v)) return v->as<FloatValue>()->value;
    if (is_int(v)) return (double)v->as<IntValue>()->value;
    return 0.0;
  };

  // Produce a numeric result following C usual arithmetic conversions:
  // - If either is float -> do double math, result is float
  // - Else integer math (wrap in host sized integer – same as current model)
  auto numeric_bin = [&](auto op_int, auto op_float) -> Value * {
    if (!is_number(left) || !is_number(right)) {
      return null_value();
    }

    if (is_float(left) || is_float(right)) {
      double a = to_double(left);
      double b = to_double(right);
      return new_float(op_float(a, b));
    }
    long long a = to_int(left);
    long long b = to_int(right);
    long long r = op_int(a, b);
    return new_int((size_t)r);
  };

  auto numeric_rel = [&](auto cmp_int, auto cmp_float) -> Value * {
    if (!is_number(left) || !is_number(right)) return null_value();
    if (is_float(left) || is_float(right)) {
      double a = to_double(left);
      double b = to_double(right);
      return new_bool(cmp_float(a, b));
    }
    long long a = to_int(left);
    long long b = to_int(right);
    return new_bool(cmp_int(a, b));
  };

  auto numeric_eq = [&]() -> Value * {
    if (!is_number(left) || !is_number(right)) return nullptr;
    if (is_float(left) || is_float(right)) {
      return new_bool(to_double(left) == to_double(right));
    }
    return new_bool(to_int(left) == to_int(right));
  };

  auto numeric_neq = [&]() -> Value * {
    if (!is_number(left) || !is_number(right)) return nullptr;
    if (is_float(left) || is_float(right)) {
      return new_bool(to_double(left) != to_double(right));
    }
    return new_bool(to_int(left) != to_int(right));
  };

  switch (node->op) {
    case TType::Add: {
      return numeric_bin([](long long a, long long b) { return (a + b); }, [](double a, double b) { return a + b; });
    }

    case TType::Sub: {
      return numeric_bin([](long long a, long long b) { return (a - b); }, [](double a, double b) { return a - b; });
    }

    case TType::Mul: {
      return numeric_bin([](long long a, long long b) { return (a * b); }, [](double a, double b) { return a * b; });
    }

    case TType::Div: {
      if (!is_number(left) || !is_number(right)) return null_value();
      if ((is_int(right) && to_int(right) == 0) || (is_float(right) && to_double(right) == 0.0)) {
        throw_error("division by zero in compile-time evaluation", node->source_range);
      }
      return numeric_bin([](long long a, long long b) { return (b == 0 ? 0 : a / b); },
                         [](double a, double b) { return a / b; });
    }

    case TType::Modulo: {
      if (!is_int(left) || !is_int(right)) return null_value();
      long long b = to_int(right);
      if (b == 0) {
        throw_error("modulo by zero in compile-time evaluation", node->source_range);
      }
      long long a = to_int(left);
      return new_int((size_t)(a % b));
    }

    case TType::Or:
      if (is_int(left) && is_int(right))
        return new_int((size_t)(to_int(left) | to_int(right)));
      else
        return null_value();
    case TType::And:
      if (is_int(left) && is_int(right))
        return new_int((size_t)(to_int(left) & to_int(right)));
      else
        return null_value();
    case TType::Xor:
      if (is_int(left) && is_int(right))
        return new_int((size_t)(to_int(left) ^ to_int(right)));
      else
        return null_value();
    case TType::SHL:
      if (is_int(left) && is_int(right))
        return new_int((size_t)(to_int(left) << to_int(right)));
      else
        return null_value();
    case TType::SHR:
      if (is_int(left) && is_int(right))
        return new_int((size_t)(to_int(left) >> to_int(right)));
      else
        return null_value();

    case TType::LogicalOr:
      if (is_bool(left) && is_bool(right))
        return new_bool(left->as<BoolValue>()->value || right->as<BoolValue>()->value);
      return null_value();

    case TType::LogicalAnd:
      if (is_bool(left) && is_bool(right))
        return new_bool(left->as<BoolValue>()->value && right->as<BoolValue>()->value);
      return null_value();

    case TType::LT:
      return numeric_rel([](long long a, long long b) { return a < b; }, [](double a, double b) { return a < b; });

    case TType::GT:
      return numeric_rel([](long long a, long long b) { return a > b; }, [](double a, double b) { return a > b; });

    case TType::LE:
      return numeric_rel([](long long a, long long b) { return a <= b; }, [](double a, double b) { return a <= b; });

    case TType::GE:
      return numeric_rel([](long long a, long long b) { return a >= b; }, [](double a, double b) { return a >= b; });

    case TType::EQ: {
      if (auto r = numeric_eq()) return r;

      if (is_bool(left) && is_bool(right)) {
        return new_bool(left->as<BoolValue>()->value == right->as<BoolValue>()->value);
      }

      if ((is_null(left) && is_null(right))) {
        return new_bool(true);
      }

      if ((is_null(left) && is_ptr(right)) || (is_ptr(left) && is_null(right))) {
        return new_bool(false);
      }

      if (is_ptr(left) && is_ptr(right)) {
        if (vt(left) == ValueType::POINTER && vt(right) == ValueType::POINTER) {
          auto lp = left->as<PointerValue>()->ptr;
          auto rp = right->as<PointerValue>()->ptr;
          return new_bool(lp == rp);
        } else if (vt(left) == ValueType::RAW_POINTER && vt(right) == ValueType::RAW_POINTER) {
          return new_bool(left->as<RawPointerValue>()->ptr == right->as<RawPointerValue>()->ptr);
        }
        return new_bool(false);
      }

      return null_value();
    }

    case TType::NEQ: {
      if (auto r = numeric_neq()) return r;

      if (is_bool(left) && is_bool(right))
        return new_bool(left->as<BoolValue>()->value != right->as<BoolValue>()->value);

      if ((is_null(left) && is_null(right))) return new_bool(false);
      if ((is_null(left) && is_ptr(right)) || (is_ptr(left) && is_null(right))) return new_bool(true);

      if (is_ptr(left) && is_ptr(right)) {
        if (vt(left) == ValueType::POINTER && vt(right) == ValueType::POINTER) {
          auto lp = left->as<PointerValue>()->ptr;
          auto rp = right->as<PointerValue>()->ptr;
          return new_bool(lp != rp);
        } else if (vt(left) == ValueType::RAW_POINTER && vt(right) == ValueType::RAW_POINTER) {
          return new_bool(left->as<RawPointerValue>()->ptr != right->as<RawPointerValue>()->ptr);
        }
        return new_bool(true);
      }

      return null_value();
    }

    /* Compound assignments */
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
      // Map compound to base
      TType base;
      switch (node->op) {
        case TType::CompAdd:
          base = TType::Add;
          break;
        case TType::CompSub:
          base = TType::Sub;
          break;
        case TType::CompMul:
          base = TType::Mul;
          break;
        case TType::CompDiv:
          base = TType::Div;
          break;
        case TType::CompMod:
          base = TType::Modulo;
          break;
        case TType::CompAnd:
          base = TType::And;
          break;
        case TType::CompOr:
          base = TType::Or;
          break;
        case TType::CompXor:
          base = TType::Xor;
          break;
        case TType::CompSHL:
          base = TType::SHL;
          break;
        case TType::CompSHR:
          base = TType::SHR;
          break;
        default:
          base = node->op;
          break;
      }
      auto saved = node->op;
      node->op = base;
      Value *res = visit_bin_expr(node);
      node->op = saved;

      auto lvalue = get_lvalue(node->left);
      write_to_lvalue(node->left, res);
      switch (lvalue->kind) {
        case LValue::MANAGED:
          *lvalue->managed = res;
          break;
        case LValue::RAW:
          lvalue->raw->assign_from(res);
          break;
      }
      return res;
    }

    case TType::Assign: {
      auto lvalue = get_lvalue(node->left);
      write_to_lvalue(node->left, right);
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
      return null_value();
  }
}

Value *Interpreter::visit_unary_expr(THIRUnaryExpr *node) {
  auto operand = visit_node(node->operand);
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

Value *Interpreter::visit_literal(THIRLiteral *node) {
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
Value *Interpreter::visit_continue(THIRContinue *) { return continue_value(); }
Value *Interpreter::visit_break(THIRBreak *) { return break_value(); }

Value *Interpreter::visit_return(THIRReturn *node) {
  if (node->expression) {
    return return_value(visit_node(node->expression));
  }
  return return_value();
}

Value *Interpreter::visit_member_access(THIRMemberAccess *node) {
  LValue *lvalue = get_member_access_lvalue(node);
  if (lvalue->kind == LValue::MANAGED) {
    return *lvalue->managed;
  } else if (lvalue->kind == LValue::RAW) {
    return lvalue->raw->dereference();
  }
  return null_value();
}

Value *Interpreter::visit_function(THIRFunction *node) {
  if (node->is_extern) {
    return try_link_extern_function(node);
  }
  auto function = value_arena_alloc<FunctionValue>();
  function->name = node->name;
  function->parameters = node->parameters;
  function->block = node->block;
  return function;
}

Value *Interpreter::visit_call(THIRCall *node) {
  auto function = visit_node(node->callee);
  std::vector<Value *> evaluated_args;
  evaluated_args.reserve(node->arguments.size());

  for (const auto &arg : node->arguments) {
    const auto evaluated = visit_node(arg);
    evaluated_args.push_back(evaluated);
  }

  if (node->is_dyn_call) {
    return FunctionValue::dyn_dispatch(node->dyn_method_name, this, evaluated_args);
  }

  if (function->get_value_type() == ValueType::EXTERN_FUNCTION) {
    auto extern_function = function->as<ExternFunctionValue>();
    auto result = compile_time_ffi_dispatch(extern_function->name, extern_function->info, evaluated_args);
    return result ? result : null_value();
  } else if (function->get_value_type() == ValueType::FUNCTION) {
    return function->as<FunctionValue>()->call(this, evaluated_args);
  } else {
    throw_error("Unable to call non-function symbol", node->source_range);
  }

  return null_value();
}

Value *Interpreter::visit_index(THIRIndex *node) {
  LValue *lvalue = get_index_lvalue(node);
  if (lvalue->kind == LValue::MANAGED) {
    return *lvalue->managed;
  } else if (lvalue->kind == LValue::RAW) {
    return lvalue->raw->dereference();
  }
  return null_value();
}

Value *Interpreter::visit_cast(THIRCast *node) {
  // TODO: do we even have to do any casting here? probably float<->int and other scalars just to make sense?
  auto result =  visit_node(node->operand);

  if (result->get_value_type() == ValueType::LVALUE) {
    auto lvalue = (LValue*)result;
    if (lvalue->managed) {
      return *lvalue->managed;
    } else {
      throw_error("unmanaged casts not implemented", node->source_range);
    }
  }

  return result;
}

Value *Interpreter::visit_variable(THIRVariable *node) {
  LValue *value = get_variable_lvalue(node);
  return *value->managed;
}

Value *Interpreter::visit_for(THIRFor *node) {
  Value *result = null_value();
  if (node->initialization) {
    visit_node(node->initialization);
  }

  while (true) {
    auto condition = visit_node(node->condition);

    if (!condition->is_truthy()) {
      break;
    }

    auto body_result = visit_node(node->block);
    if (body_result->get_value_type() == ValueType::BREAK) {
      break;
    }
    if (body_result->get_value_type() == ValueType::RETURN) {
      return body_result;
    }
    if (node->increment) {
      visit_node(node->increment);
    }
  }

  return result;
}

Value *Interpreter::visit_if(THIRIf *node) {
  auto cond_value = visit_node(node->condition);
  bool cond = cond_value->is_truthy();

  if (cond) {
    return visit_node(node->block);
  } else if (node->_else) {
    return visit_node(node->_else);
  }

  return null_value();
}

Value *Interpreter::visit_while(THIRWhile *node) {
  const auto should_continue = [&]() { return visit_node(node->condition)->is_truthy(); };

  while (should_continue()) {
    auto result = visit_node(node->block);
    if (result->get_value_type() == ValueType::BREAK) {
      break;
    }
    if (result->get_value_type() == ValueType::RETURN) {
      return result;
    }
  }

  return null_value();
}

Value *Interpreter::visit_expression_block(THIRExprBlock *) { return nullptr; }

Value *Interpreter::visit_aggregate_initializer(THIRAggregateInitializer *node) {
  ObjectValue *object = (ObjectValue *)default_value_of_t(node->type, this);

  const bool is_dyn = node->type->is_kind(TYPE_DYN);

  for (const auto &[key, value] : node->key_values) {
    if (is_dyn && key == "instance") {
      continue;
    }
    auto v = visit_node(value);
    object->values[key] = v;
  }

  if (is_dyn) {
    // We have to patch this back out
    object->values["instance"] = object;
  }

  return object;
}

Value *Interpreter::visit_collection_initializer(THIRCollectionInitializer *node) {
  ArrayValue *array = new_array(node->type);
  for (const auto &value : node->values) {
    array->values.push_back(visit_node(value));
  }
  return array;
}

void Interpreter::write_to_lvalue(THIR *left, Value *right) {
  if (left->get_node_type() == THIRNodeType::Variable) {
    auto var = (THIRVariable *)left;
    if (var->global_initializer_assignment) {
      // var->global_initializer_assignment->right = right->to_thir();
    }
    var->compile_time_value = right;
  }
}
