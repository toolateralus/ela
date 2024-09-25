#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"

ASTNode *Parser::parse() {
  auto program = ast_alloc<ASTProgram>();
  while (tok) {
    program->statements.push(parse_expr());
    if (semicolon()) {
      eat();
    }
  }
  return program;
}


ASTExpr *Parser::parse_expr() {
  auto left = parse_primary();

  while (!semicolon() && not_eof() && peek().family == TFamily::Operator) {
    auto op = eat();
    
    auto right = parse_primary();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_assignment() {
  auto left = parse_logical_or();
  if (peek().type == TType::Assign) {
    auto op = eat();
    auto right = parse_assignment();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    return binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_logical_or() {
  auto left = parse_logical_and();
  while (peek().type == TType::LogicalOr) {
    auto op = eat();
    auto right = parse_logical_and();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_logical_and() {
  auto left = parse_bitwise_or();
  while (peek().type == TType::LogicalAnd) {
    auto op = eat();
    auto right = parse_bitwise_or();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_or() {
  auto left = parse_bitwise_xor();
  while (peek().type == TType::Or) {
    auto op = eat();
    auto right = parse_bitwise_xor();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_xor() {
  auto left = parse_bitwise_and();
  while (peek().type == TType::Xor) {
    auto op = eat();
    auto right = parse_bitwise_and();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_and() {
  auto left = parse_equality();
  while (peek().type == TType::And) {
    auto op = eat();
    auto right = parse_equality();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_equality() {
  auto left = parse_relational();
  while (peek().type == TType::EQ || peek().type == TType::NEQ) {
    auto op = eat();
    auto right = parse_relational();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_relational() {
  auto left = parse_shift();
  while (peek().type == TType::LT || peek().type == TType::GT ||
         peek().type == TType::LE || peek().type == TType::GE) {
    auto op = eat();
    auto right = parse_shift();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_shift() {
  auto left = parse_additive();
  while (peek().type == TType::SHL || peek().type == TType::SHR) {
    auto op = eat();
    auto right = parse_additive();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_additive() {
  auto left = parse_multiplicative();
  while (peek().type == TType::Add || peek().type == TType::Sub) {
    auto op = eat();
    auto right = parse_multiplicative();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_multiplicative() {
  auto left = parse_unary();
  while (peek().type == TType::Mul || peek().type == TType::Div || peek().type == TType::Modulo) {
    auto op = eat();
    auto right = parse_unary();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_unary() {
  if (peek().type == TType::Add || peek().type == TType::Sub || peek().type == TType::Not || peek().type == TType::BitwiseNot) {
    auto op = eat();
    auto expr = parse_unary();
    auto unaryexpr = ast_alloc<ASTUnaryExpr>();
    unaryexpr->op = op;
    unaryexpr->operand = expr;
    return unaryexpr;
  }
  return parse_primary();
}
ASTExpr *Parser::parse_primary() {
  auto tok = peek();

  switch (tok.type) {
    case TType::Identifier: {
      eat();
      auto iden = ast_alloc<ASTIden>();
      iden->value = tok.value;
      iden->type_id = get_type_unresolved();
      return iden;
    }
    case TType::Integer: {
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Integer;
      literal->value = tok.value;
      literal->type_id = find_type_id("i32");
      return literal;
    }
    case TType::Float: {
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Float;
      literal->value = tok.value;
      literal->type_id = find_type_id("f32");
      return literal;
    }
    case TType::String: {
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::String;
      literal->value = tok.value;
      literal->type_id = find_type_id("string");
      return literal;
    }
    case TType::LParen: {
      eat(); // consume '('
      auto expr = parse_expr();
      if (peek().type != TType::RParen) {
        throw_error({.message = "Expected ')'", .severity = ERROR_FAILURE});
      }
      eat(); // consume ')'
      return expr;
    }
    default: {
      char buffer[1024];
      snprintf(buffer, sizeof(buffer),
               "Invalid primary expression. Token: %s, Type: %s",
               tok.value.c_str(), TTypeToString(tok.type).c_str());
      throw_error({.message = buffer, .severity = ERROR_FAILURE});
      return nullptr;
    }
  }
}
