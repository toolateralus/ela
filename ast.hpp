#pragma once

#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <any>
#include <cstdio>
#include <deque>
#include <format>
#include <functional>
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>
#include <vector>

extern jstl::Arena ast_arena;

template <class T> T *ast_alloc(size_t n = 1) {
  return new (ast_arena.allocate(sizeof(T) * n)) T();
}

struct VisitorBase;

// TODO: add an enum member in the base that says what type this node is,
// so we can be more performant and just static_cast<T*> instead of
// dynamic_cast<T*>;
// TODO: add a set of common ASTNode flags like unused, unresolved_symbol, etc.
// this way we can use things before theyre defined, prune unused code, etc.
struct ASTNode {
  // TODO: use a more data oriented approach here, instead of copying vectors
  // around everywhere. I am sure we can have a UID for ASTNode, and just lookup
  // our source tokens range in that table with a int src_info_start, int
  // src_info_end, and capture that info in a similar way in the parser. Also,
  // we can definitely improve the way that we capture stuff in the parser.
  std::vector<Token> source_tokens{};
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

// TODO: rename me, and add this stuff to visiting While, For, etc.
// Probably create another visitor to do this, the type visitor is getting too
// large and cumbersome. Maybe even have a base class for ASTNode's that have
// control flow attributes?
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
  std::vector<ASTNode *> statements;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTProgram : ASTNode {
  std::vector<ASTStatement *> statements;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTExpr : ASTNode {};

struct ASTType : ASTExpr {
  std::string base;
  TypeExt extension_info{};

  int resolved_type = Type::invalid_id;
  static ASTType *get_void() {
    static ASTType *type = [] {
      ASTType *type = ast_alloc<ASTType>();
      type->base = "void";
      type->resolved_type = void_type();
      return type;
    }();
    return type;
  }

  std::any accept(VisitorBase *visitor) override;
};

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
  int resolved_type;
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
    RawString,
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
  std::vector<ASTParamDecl *> params;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTFuncDecl : ASTStatement {
  int flags = 0;
  // extern, normal etc.
  FunctionMetaType meta_type = FunctionMetaType::FUNCTION_TYPE_NORMAL;
  ASTParamsDecl *params;
  Nullable<ASTBlock> block;
  Token name;
  ASTType *return_type;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTArguments : ASTNode {
  std::vector<ASTExpr *> arguments;

  std::any accept(VisitorBase *visitor) override;
};

struct ASTCall : ASTExpr {
  Token name;
  ASTArguments *arguments;
  int type = Type::invalid_id;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTDotExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  ASTType *type;
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

struct ASTSubscript: ASTExpr {
  ASTExpr *left;
  ASTExpr *subscript;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTStructDeclaration : ASTStatement {
  Scope *scope;
  ASTType *type;
  std::vector<ASTDeclaration *> declarations;
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
  std::any visit(ASTCompAssign *node) override {}                              \
  std::any visit(ASTStructDeclaration *node) override {}                       \
  std::any visit(ASTDotExpr *node) override {} \
  std::any visit(ASTSubscript *node) override {} \

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
  virtual std::any visit(ASTCompAssign *node) = 0;                             \
  virtual std::any visit(ASTStructDeclaration *node) = 0;                      \
  virtual std::any visit(ASTDotExpr *node) = 0; \
  virtual std::any visit(ASTSubscript *node) = 0; \

enum DirectiveKind {
  DIRECTIVE_KIND_STATEMENT,
  DIRECTIVE_KIND_EXPRESSION,
};

struct Parser;
struct DirectiveRoutine {
  ~DirectiveRoutine() = default;
  std::string identifier;
  DirectiveKind kind;
  std::function<Nullable<ASTNode>(Parser *parser)> run;
};

struct Parser {
  
  inline Token peek() const {
    return states.back().lookahead_buffer.front();
  }

  #define lookahead_buf() \
    states.back().lookahead_buffer
    
  Parser(const std::string &contents, const std::string &filename,
         Context &context)
      : states({Lexer::State::from_file(contents, filename)}),
        context(context) {
    init_directive_routines();
    for (int i = lookahead_buf().size(); i < 8; ++i) {
      lexer.get_token(states.back());
    }
  }

  Context &context;

  inline void fill_buffer_if_needed(){
    while (states.back().lookahead_buffer.size() < 8) {
      lexer.get_token(states.back());
    }
  }

  inline Token eat() {
    fill_buffer_if_needed();
    auto tok = peek();
    
    if (tok.is_eof() && states.size() > 1) {
      states.pop_back();
      fill_buffer_if_needed();
      return peek();
    } 
    
    lookahead_buf().pop_front();
    lexer.get_token(states.back());
    
    token_frames.back().push_back(tok);
    return tok;
  }

  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == TType::Semi; }

  inline Token expect(TType type) {
    fill_buffer_if_needed();
    if (peek().type != type) {
      throw_error(std::format("Expected {}, got {} : {}", TTypeToString(type),
                              TTypeToString(peek().type), peek().value),
                  ERROR_CRITICAL, token_frames.back());
    }
    return eat();
  }

  Lexer lexer{};

  std::vector<Lexer::State> states;

  std::vector<std::vector<Token>> token_frames = {};

  inline void begin_token_frame() { token_frames.push_back({}); }
  inline void end_token_frame(ASTNode *node) {
    if (node)
      node->source_tokens = token_frames.back();
    token_frames.pop_back();
  }

  Nullable<ASTNode> process_directive(DirectiveKind kind,
                                      const std::string &identifier);

  std::vector<DirectiveRoutine> directive_routines;

  void init_directive_routines();

  Nullable<ASTExpr> try_parse_directive_expr() {
    if (peek().type == TType::Directive) {
      eat();
      auto identifier = expect(TType::Identifier);
      Nullable<ASTNode> node =
          process_directive(DIRECTIVE_KIND_EXPRESSION, identifier.value);

      auto expr = Nullable<ASTExpr>(dynamic_cast<ASTExpr *>(node.get()));
      if (expr.is_not_null()) {
        return expr;
      } else {
        throw_error("Invalid directive in expression: directives in "
                    "expressions must return a value.",
                    ERROR_FAILURE, token_frames.back());
      }
    }
    return nullptr;
  }

  ASTType *parse_type();

  ASTProgram *parse();

  ASTStatement *parse_statement();
  ASTStatement *parse_call_statement(Token);
  ASTArguments *parse_arguments();

  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTExprStatement *parse_dot_statement(Token);
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
  ASTExpr *parse_postfix();
  ASTExpr *parse_primary();
  ASTCall *parse_call(const Token &);
};