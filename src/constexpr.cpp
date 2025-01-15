#include "constexpr.hpp"

#include "ast.hpp"

Value evaluate_constexpr(ASTExpr *node, Context &ctx) {
  switch (node->get_node_type()) {
    case AST_NODE_IDENTIFIER: {
      auto name = static_cast<ASTIdentifier*>(node);
      auto symbol = ctx.scope->lookup(name->value);
      if (symbol->declaring_node.is_not_null() && symbol->declaring_node.get()->is_expr()) {
        return evaluate_constexpr((ASTExpr*)symbol->declaring_node.get(), ctx);
      } else {
        throw_error("unable to evaluate value of symbol", node->source_range);
      }
    }
    case AST_NODE_BIN_EXPR: {
      auto binary = static_cast<ASTBinExpr *>(node);
      auto left = evaluate_constexpr(binary->left, ctx);
      auto right = evaluate_constexpr(binary->right, ctx);
      switch (binary->op.type) {
        case TType::Add:
          return left + right;
        case TType::Sub:
          return left - right;
        case TType::Mul:
          return left * right;
        case TType::Div:
          if ((right == Value::Int(0)).is_truthy()) {
            throw_error("Division by zero in constant expression", node->source_range);
          }
          return left / right;
        case TType::Modulo:
          if ((right == Value::Int(0)).is_truthy()) {
            throw_error("Modulo by zero in constant expression", node->source_range);
          }
          return left % right;
        case TType::LogicalNot:
          return !left;
        case TType::Not:
          return ~left;
        case TType::Or:
          return left | right;
        case TType::And:
          return left & right;
        case TType::SHL:
          return left << right;
        case TType::SHR:
          return left >> right;
        case TType::Xor:
          return left ^ right;
        case TType::LogicalOr:
          return left || right;
        case TType::LogicalAnd:
          return left && right;
        case TType::LT:
          return left < right;
        case TType::GT:
          return left > right;
        case TType::EQ:
          return left == right;
        case TType::NEQ:
          return left != right;
        case TType::LE:
          return left <= right;
        case TType::GE:
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
        case TType::Sub:
          return -operand;
        case TType::LogicalNot:
          return !operand;
        case TType::Not:
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
  }
}