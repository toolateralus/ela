#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include <format>

ASTProgram *Parser::parse() {
  auto program = ast_alloc<ASTProgram>();
  while (tok) {
    program->statements.push(parse_statement());
    if (semicolon()) {
      eat();
    }
  }
  return program;
}
ASTStatement *Parser::parse_statement() {
  auto tok = peek();

  if (tok.type == TType::LCurly) {
    return parse_block();
  }  

  if (find_type_id(tok.value) != -1) {
    auto decl = parse_declaration();
    
    if (peek().type == TType::Semi) eat();
    
    return decl;
  }

  eat();
  if (peek().type == TType::DoubleColon) {
    eat();
    if (peek().type == TType::LParen) {
      return parse_function_declaration(tok);
    }
  } else if (peek().type == TType::Assign) {
    auto statement = ast_alloc<ASTExprStatement>();
    statement->expression = parse_assignment(&tok);
    if (semicolon()) eat();
    return statement;
  }

  throw_error(Error{
      .message =
          std::format("Unexpected token when parsing statement: {}", tok.value),
      .severity = ERROR_CRITICAL,
  });
}
ASTDecl *Parser::parse_declaration() {
  ASTDecl *decl = ast_alloc<ASTDecl>();
  decl->type = parse_type();
  auto iden = eat();
  decl->name = iden;

  if (peek().type == TType::Assign) {
    eat();
    auto expr = parse_expr();
    decl->value = expr;
  }
  
  context.current_scope->insert(iden.value, decl->type->type_info->owner_id);
  
  return decl;
}
ASTFuncDecl *Parser::parse_function_declaration(Token name) {
  auto function = ast_alloc<ASTFuncDecl>();
  function->params = parse_parameters();
  
  if (peek().type != TType::Arrow) {
    function->return_type = ASTType::get_void();
  } else {
    expect(TType::Arrow); 
    function->return_type = parse_type();
  }
  
  function->block = parse_block();
  return function;
}
ASTBlock *Parser::parse_block() {
  expect(TType::LCurly);
  ASTBlock *block = ast_alloc<ASTBlock>();
  context.enter_scope();
  while (not_eof() && peek().type != TType::RCurly) {
    block->statements.push(parse_statement());
  }
  expect(TType::RCurly);
  block->scope = context.exit_scope();
  return block;
}
ASTExpr *Parser::parse_expr() {
  return parse_logical_or();
}
ASTExpr *Parser::parse_assignment(Token *iden = nullptr) {
  ASTExpr *left;
  
  if (iden != nullptr)  {
    auto iden = ast_alloc<ASTIden>();
    iden->value = iden->value;
    left = iden;
  } else {
     left = parse_logical_or();  
  }
  
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
  while (peek().type == TType::Mul || peek().type == TType::Div ||
         peek().type == TType::Modulo) {
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
  if (peek().type == TType::Add || peek().type == TType::Sub ||
      peek().type == TType::Not || peek().type == TType::BitwiseNot) {
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
    return iden;
  }
  case TType::Integer: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Integer;
    literal->value = tok.value;
    return literal;
  }
  case TType::Float: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Float;
    literal->value = tok.value;
    return literal;
  }
  case TType::String: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::String;
    literal->value = tok.value;
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
    throw_error({.message = std::format(
                     "Invalid primary expression. Token: {}, Type: {}",
                     tok.value, TTypeToString(tok.type)),
                 .severity = ERROR_FAILURE});
    return nullptr;
  }
  }
}
ASTType *Parser::parse_type() {
  auto base = eat().value;
  jstl::Vector<int> array_dims;
  int ptr_depth = 0;
  
  while (true) {
    if (peek().type == TType::LBrace) {
      expect(TType::LBrace);
      if (peek().type == TType::Integer) {
        array_dims.push(std::stoi(eat().value));
      } else {
        array_dims.push(-1); // dynamic array
      }
      if (peek().type != TType::RBrace) {
        throw_error({.message = "Expected ']'", .severity = ERROR_FAILURE});
      }
      expect(TType::RBrace);
    } else if (peek().type == TType::Mul) {
      expect(TType::Mul);
      ptr_depth++;
    } else {
      break;
    }
  }
  
  auto node = ast_alloc<ASTType>();
  
  auto id = find_type_id(base, ptr_depth, array_dims);
  
  if (id != -1) {
    auto typeinfo = get_type_info(id);
    node->type_info = typeinfo;
    node->complete = true;
    return node;
  }
  
  auto typeinfo = ast_alloc<TypeInfo>();
  typeinfo->name = base;
  typeinfo->array_dims = array_dims;
  typeinfo->ptr_depth = ptr_depth;
  
  node->type_info = typeinfo;
  node->complete = false;
  
  return node;
}
ASTParamsDecl *Parser::parse_parameters() {
    ASTParamsDecl *params = ast_alloc<ASTParamsDecl>();
  expect(TType::LParen);
  
  while (peek().type != TType::RParen) {
    auto type = parse_type();
    auto name = expect(TType::Identifier).value;
    
    auto param = ast_alloc<ASTParamDecl>();
    param->type = type;
    param->name = name;
    
    if (peek().type == TType::Assign) {
      eat();
      param->default_value = parse_expr();
    }
    
    params->params.push(param);
    
    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    } else break;
  }
  
  expect(TType::RParen);
  return params;
}
