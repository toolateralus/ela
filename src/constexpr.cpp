#include "constexpr.hpp"

#include "ast.hpp"

Value evaluate_constexpr(ASTExpr *node, Context &ctx) {
  switch (node->get_node_type()) {
    case AST_NODE_IDENTIFIER: {
      auto name = static_cast<ASTIdentifier*>(node);
      auto symbol = ctx.scope->lookup(name->value);
      
      if (!symbol || symbol->is_function()) {
        throw_error("Cannot evaluate non-variable, non-constant values at compile time currently.", node->source_range);
      }

      auto initial_value = symbol->variable.initial_value;
      if (!initial_value) {
        throw_error(std::format("Couldn't evaluate symbol {}, it didn't have a value. Make sure it's a constant, such as `CONSTANT :: 100`, or a default initialized runtime variable with a constant value.", name->value.get_str()), node->source_range);
      }

      return evaluate_constexpr(initial_value.get(), ctx);
    }
    case AST_NODE_BIN_EXPR: {
      auto binary = static_cast<ASTBinExpr *>(node);
      auto left = evaluate_constexpr(binary->left, ctx);
      auto right = evaluate_constexpr(binary->right, ctx);
      switch (binary->op.type) {
        case Token_Type::Add:
          return left + right;
        case Token_Type::Sub:
          return left - right;
        case Token_Type::Mul:
          return left * right;
        case Token_Type::Div:
          if ((right == Value::Int(0)).is_truthy()) {
            throw_error("Division by zero in constant expression", node->source_range);
          }
          return left / right;
        case Token_Type::Modulo:
          if ((right == Value::Int(0)).is_truthy()) {
            throw_error("Modulo by zero in constant expression", node->source_range);
          }
          return left % right;
        case Token_Type::LogicalNot:
          return !left;
        case Token_Type::Not:
          return ~left;
        case Token_Type::Or:
          return left | right;
        case Token_Type::And:
          return left & right;
        case Token_Type::SHL:
          return left << right;
        case Token_Type::SHR:
          return left >> right;
        case Token_Type::Xor:
          return left ^ right;
        case Token_Type::LogicalOr:
          return left || right;
        case Token_Type::LogicalAnd:
          return left && right;
        case Token_Type::LT:
          return left < right;
        case Token_Type::GT:
          return left > right;
        case Token_Type::EQ:
          return left == right;
        case Token_Type::NEQ:
          return left != right;
        case Token_Type::LE:
          return left <= right;
        case Token_Type::GE:
          return left >= right;
        default:
          throw_error("Invalid binary operator in constant expression", node->source_range);
      }
      auto unary = static_cast<ASTUnaryExpr *>(node);
    } break;
    case AST_NODE_UNARY_EXPR: {
      auto unary = static_cast<ASTUnaryExpr *>(node);
      auto operand = evaluate_constexpr(unary->operand, ctx);
      switch (unary->op.type) {
        case Token_Type::Sub:
          return -operand;
        case Token_Type::LogicalNot:
          return !operand;
        case Token_Type::Not:
          return ~operand;
        default:
          throw_error("Invalid unary operator in constant expression", node->source_range);
      }
    } break;
    case AST_NODE_LITERAL: {
      auto literal = static_cast<ASTLiteral *>(node);
      switch (literal->tag) {
        case ASTLiteral::Integer:
          return Value::Int(literal->value);
        case ASTLiteral::Float:
          return Value::Float(literal->value);
        case ASTLiteral::Bool:
          return Value::Bool(literal->value);
          break;
        default:
          throw_error("Invalid literal type in constant expression", node->source_range);
      }
    } break;
    default:
      throw_error("Invalid node in constant expression", node->source_range);
      return {};
  }
  return {};
}