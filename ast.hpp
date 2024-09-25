#pragma once

#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include <any>
#include <format>
#include <jstl/memory/arena.hpp>
#include <jstl/containers/vector.hpp>

extern jstl::Arena ast_arena;

template <class T> T *ast_alloc(size_t n = 1) {
  return new (ast_arena.allocate(sizeof(T) * n)) T();
}

struct VisitorBase;

struct ASTNode {
  virtual ~ASTNode() = default;
  virtual std::any accept(VisitorBase *visitor) = 0;
};

struct ASTStatement : ASTNode {};

struct ASTBlock : ASTStatement {
  Scope *scope;
  jstl::Vector<ASTNode *> statements;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTProgram : ASTNode {
  jstl::Vector<ASTStatement *> statements;
  std::any accept(VisitorBase *visitor) override;
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

  std::any accept(VisitorBase *visitor) override;
};

struct ASTExpr : ASTNode {};

struct ASTExprStatement : ASTStatement {
  ASTExpr *expression;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTDeclaration : ASTStatement {
  Token name;
  ASTType *type;
  Nullable<ASTExpr> value;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  Token op;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTUnaryExpr : ASTExpr {
  ASTExpr *operand;
  Token op;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTIdentifier : ASTExpr {
  std::string value;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTLiteral : ASTExpr {
  enum Tag {
    Integer,
    Float,
    String,
  } tag;
  std::string value;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTParamDecl : ASTNode {
  ASTType *type;
  Nullable<ASTExpr> default_value;
  std::string name;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTParamsDecl : ASTStatement {
  jstl::Vector<ASTParamDecl *> params;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTFuncDecl : ASTStatement {
  ASTParamsDecl *params;
  ASTBlock *block;
  Token name;
  ASTType *return_type;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTArguments: ASTNode {
  jstl::Vector<ASTExpr*> arguments;

  std::any accept(VisitorBase *visitor) override;
};

struct ASTCall : ASTExpr {
  Token name;
  ASTArguments* arguments;

  std::any accept(VisitorBase *visitor) override;
};

// Use this only for implementing the methods, so you can use the IDE to expand
// it.
#define DECLARE_VISIT_METHODS()                                                \
  std::any visit(ASTProgram *node) override {}                                 \
  std::any visit(ASTBlock *node) override {}                                   \
  std::any visit(ASTFuncDecl *node) override {}                                \
  std::any visit(ASTParamsDecl *node) override {}                              \
  std::any visit(ASTParamDecl *node) override {}                               \
  std::any visit(ASTDeclaration *node) override {}                             \
  std::any visit(ASTExprStatement *node) override {}                           \
  std::any visit(ASTBinExpr *node) override {}                                 \
  std::any visit(ASTUnaryExpr *node) override {}                               \
  std::any visit(ASTIdentifier *node) override {}                              \
  std::any visit(ASTLiteral *node) override {}                                 \
  std::any visit(ASTType *node) override {}                                    \
  std::any visit(ASTCall *node) override {}                                    \
  std::any visit(ASTArguments *node) override {}                               \
  
  
  

#define DECLARE_VISIT_BASE_METHODS()                                           \
  virtual std::any visit(ASTProgram *node) = 0;                                \
  virtual std::any visit(ASTBlock *node) = 0;                                  \
  virtual std::any visit(ASTFuncDecl *node) = 0;                               \
  virtual std::any visit(ASTParamsDecl *node) = 0;                             \
  virtual std::any visit(ASTParamDecl *node) = 0;                              \
  virtual std::any visit(ASTDeclaration *node) = 0;                            \
  virtual std::any visit(ASTExprStatement *node) = 0;                          \
  virtual std::any visit(ASTBinExpr *node) = 0;                                \
  virtual std::any visit(ASTUnaryExpr *node) = 0;                              \
  virtual std::any visit(ASTIdentifier *node) = 0;                             \
  virtual std::any visit(ASTLiteral *node) = 0;                                \
  virtual std::any visit(ASTType *node) = 0;                                   \
  virtual std::any visit(ASTCall *node) = 0;                                   \
  virtual std::any visit(ASTArguments *node) = 0;                               \

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
  ASTStatement *parse_call_statement(Token);
  ASTArguments *parse_arguments();

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
