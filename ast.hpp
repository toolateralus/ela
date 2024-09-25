#pragma once

#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include <format>
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>
#include "nullable.hpp"

extern jstl::Arena ast_arena;

template <class T> T *ast_alloc(size_t n = 1) {
  return (T *)ast_arena.allocate(sizeof(T) * n);
}

struct ASTNode {
  virtual ~ASTNode() = default;
};

struct ASTStatement : ASTNode {};
struct ASTBlock : ASTStatement {
  Scope *scope;
  jstl::Vector<ASTNode *> statements;
};
struct ASTProgram : ASTNode {
  jstl::Vector<ASTStatement *> statements;
};

struct ASTType : ASTNode {
  std::string base;
  int ptr_depth;
  jstl::Vector<int> array_dims;

  static ASTType *get_void() {
    static ASTType *type = [] {
      ASTType *type = ast_alloc<ASTType>();
      type->base = "void";
      return type;
    }();
    return type;
  }
};

struct ASTExpr : ASTNode {};

struct ASTExprStatement : ASTStatement {
  ASTExpr *expression;
};

struct ASTDeclaration : ASTStatement {
  Token name;
  ASTType *type;
  Nullable<ASTExpr> value;
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  Token op;
};
struct ASTUnaryExpr : ASTExpr {
  ASTExpr *operand;
  Token op;
};
struct ASTIdentifier : ASTExpr {
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

struct ASTParamDecl : ASTNode {
  ASTType *type;
  // nullable
  Nullable<ASTExpr> default_value;
  std::string name;
};
struct ASTParamsDecl : ASTStatement {
  jstl::Vector<ASTParamDecl *> params;
};
struct ASTFuncDecl : ASTStatement {
  ASTParamsDecl *params;
  ASTBlock *block;
  Token name;
  ASTType *return_type;
};

struct Parser {
  Parser(const std::string &contents, const std::string &filename,
         Context &context)
      : state(Lexer::State::from_file(contents, filename)), context(context) {
    tok = lexer.get_token(state);
  }

  Context &context;

  const Token &peek() const { return tok; }

  inline Token eat() {
    auto tok = this->tok;
    this->tok = lexer.get_token(state);
    return tok;
  }

  inline bool not_eof() const { return !tok.is_eof(); }
  inline bool eof() const { return tok.is_eof(); }

  inline bool semicolon() const { return tok.type == TType::Semi; }

  inline Token expect(TType type) {
    if (peek().type != type) {
      throw_error(Error{
          .message = std::format("Expected {}, got {}", TTypeToString(type),
                                 TTypeToString(peek().type)),
          .severity = ERROR_CRITICAL,
      });
    }
    return eat();
  }

  Token tok = Token::Eof();
  Lexer lexer{};
  Lexer::State state;
  ASTType *parse_type();

  ASTProgram *parse();

  ASTStatement *parse_statement();

  ASTDeclaration *parse_declaration();
  ASTFuncDecl *parse_function_declaration(Token);
  ASTParamsDecl *parse_parameters();
  ASTBlock *parse_block();

  ASTExpr *parse_expr();
  ASTExpr *parse_assignment(Token *);
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
