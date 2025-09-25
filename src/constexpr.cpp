#include "constexpr.hpp"
#include "ast.hpp"
#include "scope.hpp"

Value *evaluate_constexpr(ASTExpr *node, Context &ctx) {
  CTInterpreter interpeter(ctx);
  return interpeter.visit(node);
}

Value *CTInterpreter::visit_method_call(ASTMethodCall *) { return NullV(); }

Value *CTInterpreter::visit_path(ASTPath *node) {
  auto symbol = ctx.get_symbol(node).get();
  if (symbol->is_variable && symbol->variable.initial_value.get()) {
    symbol->value = visit(symbol->variable.initial_value.get());
  }
  if (symbol->is_function && symbol->function.declaration) {
    symbol->value = visit(symbol->function.declaration);
  }
  return symbol->value;
}

Value *CTInterpreter::visit_pattern_match(ASTPatternMatch *) { return NullV(); }
Value *CTInterpreter::visit_dyn_of(ASTDyn_Of *) { return NullV(); }
Value *CTInterpreter::visit_type_of(ASTType_Of *) { return NullV(); }
Value *CTInterpreter::visit_block(ASTBlock *) { return NullV(); }
Value *CTInterpreter::visit_expr_statement(ASTExprStatement *) { return NullV(); }

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
    default:
      return NullV();
  }
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

Value *CTInterpreter::visit_for(ASTFor *) { return NullV(); }

Value *CTInterpreter::visit_call(ASTCall *) { return NullV(); }
Value *CTInterpreter::visit_return(ASTReturn *) { return NullV(); }
Value *CTInterpreter::visit_dot_expr(ASTDotExpr *) { return NullV(); }
Value *CTInterpreter::visit_index(ASTIndex *) { return NullV(); }
Value *CTInterpreter::visit_initializer_list(ASTInitializerList *) { return NullV(); }
Value *CTInterpreter::visit_range(ASTRange *) { return NullV(); }
Value *CTInterpreter::visit_switch(ASTSwitch *) { return NullV(); }
Value *CTInterpreter::visit_tuple(ASTTuple *) { return NullV(); }
Value *CTInterpreter::visit_cast(ASTCast *) { return NullV(); }
Value *CTInterpreter::visit_lambda(ASTLambda *) { return NullV(); }
Value *CTInterpreter::visit_size_of(ASTSize_Of *) { return NullV(); }
Value *CTInterpreter::visit_struct_declaration(ASTStructDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_module(ASTModule *) { return NullV(); }
Value *CTInterpreter::visit_import(ASTImport *) { return NullV(); }
Value *CTInterpreter::visit_program(ASTProgram *) { return NullV(); }
Value *CTInterpreter::visit_function_declaration(ASTFunctionDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_variable(ASTVariable *) { return NullV(); }
Value *CTInterpreter::visit_continue(ASTContinue *) { return NullV(); }
Value *CTInterpreter::visit_break(ASTBreak *) { return NullV(); }
Value *CTInterpreter::visit_if(ASTIf *) { return NullV(); }
Value *CTInterpreter::visit_else(ASTElse *) { return NullV(); }
Value *CTInterpreter::visit_while(ASTWhile *) { return NullV(); }
Value *CTInterpreter::visit_enum_declaration(ASTEnumDeclaration *) { return NullV(); }
Value *CTInterpreter::visit_tuple_deconstruction(ASTDestructure *) { return NullV(); }
Value *CTInterpreter::visit_impl(ASTImpl *) { return NullV(); }
Value *CTInterpreter::visit_defer(ASTDefer *) { return NullV(); }
Value *CTInterpreter::visit_choice_declaration(ASTChoiceDeclaration *) { return NullV(); }
