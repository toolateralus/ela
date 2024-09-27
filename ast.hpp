#pragma once

#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <any>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>
#include <sstream>
#include <vector>

extern jstl::Arena ast_arena;

template <class T> T *ast_alloc(size_t n = 1) {
  return new (ast_arena.allocate(sizeof(T) * n)) T();
}

struct VisitorBase;

struct ASTNode {
  std::vector<Token> source_tokens {};
  virtual ~ASTNode() = default;
  virtual std::any accept(VisitorBase *visitor) = 0;
};

struct ASTStatement : ASTNode {};

enum BlockFlags {
  BLOCK_FLAGS_FALL_THROUGH = 1 << 0,
  BLOCK_FLAGS_RETURN = 1 << 1,
  BLOCK_FLAGS_CONTINUE = 1 << 2,
  BLOCK_FLAGS_BREAK = 1 << 3,
};

struct ControlFlow {
  int flags;
  int type;
};

#define BLOCK_FLAG_TO_STRING(flag)                                             \
  if (flags & flag)                                                            \
    result += #flag " ";

inline static std::string block_flags_to_string(int flags) {
  std::string result;
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_FALL_THROUGH)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_RETURN)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_CONTINUE)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_BREAK)
  return result;
}

struct ASTBlock : ASTStatement {
  int flags = BLOCK_FLAGS_FALL_THROUGH;
  int return_type = Type::invalid_id;
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
  TypeExtensionInfo extension_info{};
  
  int resolved_type = Type::invalid_id;
  static ASTType *get_void() {
    static ASTType *type = [] {
      ASTType *type = ast_alloc<ASTType>();
      type->base = "void";
      type->resolved_type = find_type_id("void", {});
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

struct ASTCompAssign : ASTStatement {
  Token name;
  Token op;
  ASTExpr *expr;

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
  Token value;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTLiteral : ASTExpr {
  enum Tag {
    Integer,
    Float,
    String,
    Bool,
    Null,
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

struct ASTArguments : ASTNode {
  jstl::Vector<ASTExpr *> arguments;

  std::any accept(VisitorBase *visitor) override;
};

struct ASTCall : ASTExpr {
  Token name;
  ASTArguments *arguments;
  int type = Type::invalid_id;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTReturn : ASTStatement {
  Nullable<ASTExpr> expression;
  std::any accept(VisitorBase *visitor) override;
};
struct ASTBreak : ASTStatement {
  std::any accept(VisitorBase *visitor) override;
};
struct ASTContinue : ASTStatement {
  std::any accept(VisitorBase *visitor) override;
};

struct ASTFor : ASTStatement {
  enum ForType {
    RangeBased,
    CStyle,
  } tag;

  union {
    struct {
      // TODO: add a way to use 'for v, idx in collection'
      // the identifier
      ASTExpr *target;
      ASTExpr *collection;
    } range_based;
    struct {
      ASTDeclaration *decl;
      ASTExpr *condition;
      ASTExpr *increment;
    } c_style;
  } value;

  ASTBlock *block;

  std::any accept(VisitorBase *visitor) override;
};

struct ASTElse;
struct ASTIf : ASTStatement {
  ASTExpr *condition;
  ASTBlock *block;
  Nullable<ASTElse> _else; // just an else.
  std::any accept(VisitorBase *visitor) override;
};

struct ASTElse : ASTStatement {
  Nullable<ASTIf> _if; // conditional else.
  Nullable<ASTBlock> block;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTWhile : ASTStatement {
  Nullable<ASTExpr> condition;
  ASTBlock *block;
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
  std::any visit(ASTReturn *node) override {}                                  \
  std::any visit(ASTContinue *node) override {}                                \
  std::any visit(ASTBreak *node) override {}                                   \
  std::any visit(ASTFor *node) override {}                                     \
  std::any visit(ASTIf *node) override {}                                      \
  std::any visit(ASTElse *node) override {}                                    \
  std::any visit(ASTWhile *node) override {}                                   \
  std::any visit(ASTCompAssign *node) override {}

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
  virtual std::any visit(ASTArguments *node) = 0;                              \
  virtual std::any visit(ASTReturn *node) = 0;                                 \
  virtual std::any visit(ASTContinue *node) = 0;                               \
  virtual std::any visit(ASTBreak *node) = 0;                                  \
  virtual std::any visit(ASTFor *node) = 0;                                    \
  virtual std::any visit(ASTIf *node) = 0;                                     \
  virtual std::any visit(ASTElse *node) = 0;                                   \
  virtual std::any visit(ASTWhile *node) = 0;                                  \
  virtual std::any visit(ASTCompAssign *node) = 0;



struct Parser {
  Parser(const std::string &contents, const std::string &filename,
         Context &context)
      : states({Lexer::State::from_file(contents, filename)}), context(context) {
    init_directive_routines();
    tok = lexer.get_token(states.back());
  }

  Context &context;
 
  const Token &peek() const { return tok; }

  inline Token eat() {
    auto tok = this->tok;
    token_frames.back().push_back(tok);
    this->tok = lexer.get_token(states.back());
    
    // pop the state stack and move into the next stream.
    if (this->tok.is_eof() && states.size() > 1) {
      states.pop_back();
      this->tok = lexer.get_token(states.back());
    }
    
    return tok;
  }

  inline bool not_eof() const { return !tok.is_eof(); }
  inline bool eof() const { return tok.is_eof(); }

  inline bool semicolon() const { return tok.type == TType::Semi; }

  inline Token expect(TType type) {
    if (peek().type != type) {
      throw_error(
          std::format("Expected {}, got {}", TTypeToString(type),
                                 TTypeToString(peek().type)),
          ERROR_CRITICAL,
          token_frames.back()
      );
    }
    return eat();
  }

  Token tok = Token::Eof();
  Lexer lexer{};
  
  std::vector<Lexer::State> states;
  
  std::vector<std::vector<Token>> token_frames = {};
  
  inline void begin_token_frame() {
    token_frames.push_back({});
  }
  inline void end_token_frame(ASTNode *node) {
    if (node) node->source_tokens = token_frames.back();
    token_frames.pop_back();
  }
  
  
void process_directive(const std::string &identifier);

struct DirectiveRoutine {
  std::string identifier;
  std::function<void(Parser *parser)> run;
};

jstl::Vector<DirectiveRoutine> directive_routines;

inline void init_directive_routines() {
  directive_routines.push(DirectiveRoutine {
    .identifier = "include",
    .run = [](Parser *parser) static {
      auto filename = parser->expect(TType::String).value;
      if (!std::filesystem::exists(filename)) {
        throw_error(std::format("Couldn't find included file: {}", filename), ERROR_CRITICAL, {});
      }
      std::stringstream ss;
      std::ifstream isftr(filename);
      ss << isftr.rdbuf();
      parser->states.push_back(Lexer::State::from_file(ss.str(), filename));
    }
  });
}
  
  ASTType *parse_type();

  ASTProgram *parse();

  ASTStatement *parse_statement();
  ASTStatement *parse_call_statement(Token);
  ASTArguments *parse_arguments();

  ASTDeclaration *parse_declaration();
  ASTFuncDecl *parse_function_declaration(Token);
  ASTParamsDecl *parse_parameters();
  ASTBlock *parse_block();

  ASTCall *parse_call(const Token&);

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
  ASTExpr *parse_postfix();
  ASTExpr *parse_primary();
};