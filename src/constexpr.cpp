#include "constexpr.hpp"
#include "ast.hpp"
#include "scope.hpp"

Value *evaluate_constexpr(ASTExpr *node, Context &ctx) {
  CTInterpreter interpeter(ctx);
  return interpeter.visit(node);
}

Value *CTInterpreter::visit_method_call(ASTMethodCall *) { return Value::Null(); }

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

Value *CTInterpreter::visit_pattern_match(ASTPatternMatch *) { return Value::Null(); }
Value *CTInterpreter::visit_dyn_of(ASTDyn_Of *) { return Value::Null(); }
Value *CTInterpreter::visit_type_of(ASTType_Of *) { return Value::Null(); }
Value *CTInterpreter::visit_block(ASTBlock *) { return Value::Null(); }
Value *CTInterpreter::visit_expr_statement(ASTExprStatement *) { return Value::Null(); }

Value *CTInterpreter::visit_bin_expr(ASTBinExpr *node) {
  auto left = visit(node->left);
  auto right = visit(node->right);

  (void)left;
  (void)right;

  // TODO: implement this;
  // switch (binary->op) {
  //   case TType::Add:
  //     return left + right;
  //   case TType::Sub:
  //     return left - right;
  //   case TType::Mul:
  //     return left * right;
  //   case TType::Div:
  //     if ((right == Value::Int(0)).is_truthy()) {
  //       throw_error("Division by zero in constant expression", node->source_range);
  //     }
  //     return left / right;
  //   case TType::Modulo:
  //     if ((right == Value::Int(0)).is_truthy()) {
  //       throw_error("Modulo by zero in constant expression", node->source_range);
  //     }
  //     return left % right;
  //   case TType::LogicalNot:
  //     return !left;
  //   case TType::Not:
  //     return ~left;
  //   case TType::Or:
  //     return left | right;
  //   case TType::And:
  //     return left & right;
  //   case TType::SHL:
  //     return left << right;
  //   case TType::SHR:
  //     return left >> right;
  //   case TType::Xor:
  //     return left ^ right;
  //   case TType::LogicalOr:
  //     return left || right;
  //   case TType::LogicalAnd:
  //     return left && right;
  //   case TType::LT:
  //     return left < right;
  //   case TType::GT:
  //     return left > right;
  //   case TType::EQ:
  //     return left == right;
  //   case TType::NEQ:
  //     return left != right;
  //   case TType::LE:
  //     return left <= right;
  //   case TType::GE:
  //     return left >= right;
  //   default:
  //     throw_error("Invalid binary operator in constant expression", node->source_range);
  // }
  
  return Value::Null();
}

Value *CTInterpreter::visit_unary_expr(ASTUnaryExpr *node) {
  auto unary = (ASTUnaryExpr *)node;
  auto operand = evaluate_constexpr(unary->operand, ctx);
  (void)operand;
  // TODO: implement this
  // switch (unary->op) {
  //   case TType::Sub:
  //     return -operand;
  //   case TType::LogicalNot:
  //     return !operand;
  //   case TType::Not:
  //     return ~operand;
  //   default:
  //     throw_error("Invalid unary operator in constant expression", node->source_range);
  // }
  return Value::Null();
}

Value *CTInterpreter::visit_literal(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer:
      return Value::Int(node->value);
    case ASTLiteral::Float:
      return Value::Float(node->value);
    case ASTLiteral::Bool:
      return Value::Bool(node->value);
    case ASTLiteral::Null:
      return Value::Null();
    case ASTLiteral::Char:
      return Value::Char(node->value.get_str()[0]);
    case ASTLiteral::String:
    case ASTLiteral::MultiLineString:
      return Value::String(node->value);
    default:
      throw_error("Invalid literal type in constant expression", node->source_range);
      return Value::Null();
  }
}

Value *CTInterpreter::visit_for(ASTFor *) { return Value::Null(); }

Value *CTInterpreter::visit_call(ASTCall *) { return Value::Null(); }
Value *CTInterpreter::visit_return(ASTReturn *) { return Value::Null(); }
Value *CTInterpreter::visit_dot_expr(ASTDotExpr *) { return Value::Null(); }
Value *CTInterpreter::visit_index(ASTIndex *) { return Value::Null(); }
Value *CTInterpreter::visit_initializer_list(ASTInitializerList *) { return Value::Null(); }
Value *CTInterpreter::visit_range(ASTRange *) { return Value::Null(); }
Value *CTInterpreter::visit_switch(ASTSwitch *) { return Value::Null(); }
Value *CTInterpreter::visit_tuple(ASTTuple *) { return Value::Null(); }
Value *CTInterpreter::visit_cast(ASTCast *) { return Value::Null(); }
Value *CTInterpreter::visit_lambda(ASTLambda *) { return Value::Null(); }
Value *CTInterpreter::visit_size_of(ASTSize_Of *) { return Value::Null(); }
Value *CTInterpreter::visit_struct_declaration(ASTStructDeclaration *) { return Value::Null(); }
Value *CTInterpreter::visit_module(ASTModule *) { return Value::Null(); }
Value *CTInterpreter::visit_import(ASTImport *) { return Value::Null(); }
Value *CTInterpreter::visit_program(ASTProgram *) { return Value::Null(); }
Value *CTInterpreter::visit_function_declaration(ASTFunctionDeclaration *) { return Value::Null(); }
Value *CTInterpreter::visit_variable(ASTVariable *) { return Value::Null(); }
Value *CTInterpreter::visit_continue(ASTContinue *) { return Value::Null(); }
Value *CTInterpreter::visit_break(ASTBreak *) { return Value::Null(); }
Value *CTInterpreter::visit_if(ASTIf *) { return Value::Null(); }
Value *CTInterpreter::visit_else(ASTElse *) { return Value::Null(); }
Value *CTInterpreter::visit_while(ASTWhile *) { return Value::Null(); }
Value *CTInterpreter::visit_enum_declaration(ASTEnumDeclaration *) { return Value::Null(); }
Value *CTInterpreter::visit_tuple_deconstruction(ASTDestructure *) { return Value::Null(); }
Value *CTInterpreter::visit_impl(ASTImpl *) { return Value::Null(); }
Value *CTInterpreter::visit_defer(ASTDefer *) { return Value::Null(); }
Value *CTInterpreter::visit_choice_declaration(ASTChoiceDeclaration *) { return Value::Null(); }
