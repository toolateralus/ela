#pragma once

#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>

struct ASTNode {
  virtual ~ASTNode() = default;
};

struct ASTProgram: ASTNode {
  jstl::Vector<ASTNode*> statements;
};

struct ASTStatement : ASTNode {
};

struct ASTBlock : ASTStatement {
  // TODO: make this statements only, just using expressions atm.
  jstl::Vector<ASTNode*> statements;
};

struct ASTExpr : ASTNode {
  int type_id;
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  Token op;
};

struct ASTUnaryExpr: ASTExpr {
  ASTExpr *operand;
  Token op;  
};

struct ASTIden : ASTExpr {
  std::string value;
};

struct ASTLiteral : ASTExpr {
  enum Tag {
    Integer,
    Float,
    String,
  } tag;
  std::string value;
};

static jstl::Arena ast_arena{GB(1)};

template <class T> T *ast_alloc(size_t n = 1) {
  return (T *)ast_arena.allocate(sizeof(T) * n);
}

struct Parser {
  Parser(const std::string &contents, const std::string &filename, Context &context)
      : state(Lexer::State::from_file(contents, filename)), context(context) {
    tok = lexer.get_token(state);
  }
  
  Context &context;
  
  const Token &peek() const {
    return tok;
  }
  
  inline Token eat() {
    auto tok = this->tok;
    this->tok = lexer.get_token(state);
    return tok;
  }
  
  inline bool not_eof() const {
    return !tok.is_eof();
  }
  inline bool eof() const {
    return tok.is_eof();
  }
  
  inline bool semicolon() const {
    return tok.type == TType::Semi;
  }
  
  inline Token expect(TType type) {
    if (peek().type != type) {
      char buf[128]; // our token types are very short.
      snprintf(buf, sizeof(buf), "Expected %s, got %s", TTypeToString(type).c_str(), TTypeToString(peek().type).c_str());
      throw_error(Error {
        .message = buf,
        .severity = ERROR_CRITICAL,
      });
    }
    return eat();
  }

  Token tok = Token::Eof();
  Lexer lexer {};
  Lexer::State state;

  ASTNode *parse();
  ASTExpr *parse_expr();
  ASTExpr *parse_assignment();
  ASTExpr *parse_logical_or();
  ASTExpr *parse_logical_and();
  ASTExpr *parse_bitwise_or();
  ASTExpr *parse_bitwise_xor();
  ASTExpr *parse_bitwise_and();
  ASTExpr *parse_equality();
  ASTExpr *parse_relational();
  ASTExpr *parse_shift();
  ASTExpr *parse_additive();
  ASTExpr *parse_multiplicative();
  ASTExpr *parse_unary();
  ASTExpr *parse_primary();
};
