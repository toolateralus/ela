#pragma once

#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <algorithm>
#include <any>
#include <cstdint>
#include <cstdio>
#include <deque>
#include <format>
#include <functional>
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>
#include <vector>
#include <cmath>

enum {
  ASTTYPE_EMIT_OBJECT,
};

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
  SourceRange source_range{};
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

// CLEANUP(Josh) Probably rename me 9/30/2024, 10:18:10 AM
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
  int flags = -1;
  Nullable<ASTType> pointing_to;
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

// BUG(Josh) cannot do array[i] += n. use non-identifiers for comp assign 9/30/2024, 10:31:56 AM
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

struct ASTFunctionDeclaration : ASTStatement {
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

// we'll use this node for several things,
// to reduce ast amount
enum ASTMakeKind {
  MAKE_CTOR,
  MAKE_COPY_CTOR,
  MAKE_CAST,
};

struct ASTMake : ASTExpr {
  int kind = MAKE_CTOR;
  ASTType *type_arg;
  ASTArguments *arguments;
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

// BUG(Josh) bug : for i ; arr  assumes i is the array's type, not the element type. 9/30/2024, 10:32:26 AM
struct ASTFor : ASTStatement {
  enum ForType {
    RangeBased,
    CStyle,
  } tag;

  union {
    struct {
      // TODO: add a way to use 'for v, idx in collection'
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

struct ASTSubscript : ASTExpr {
  ASTExpr *left;
  ASTExpr *subscript;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTStructDeclaration : ASTStatement {
  ASTType *type;
  Scope *scope;

  std::vector<ASTDeclaration *> fields;
  std::vector<ASTFunctionDeclaration *> methods;

  std::any accept(VisitorBase *visitor) override;
};

struct ASTInitializerList : ASTExpr {
  ASTType *type;
  std::vector<ASTExpr*> expressions;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTEnumDeclaration : ASTStatement {
  bool is_flags = false;
  ASTType* type;
  std::vector<std::pair<std::string, Nullable<ASTExpr>>> key_values;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTNoop : ASTStatement {
  std::any accept(VisitorBase *visitor) override;
};

// Use this only for implementing the methods, so you can use the IDE to expand
// it.
#define DECLARE_VISIT_METHODS()                                                \
  std::any visit(ASTProgram *node) override {}                                 \
  std::any visit(ASTBlock *node) override {}                                   \
  std::any visit(ASTFunctionDeclaration *node) override {}                                \
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
  std::any visit(ASTDotExpr *node) override {}                                 \
  std::any visit(ASTSubscript *node) override {}                               \
  std::any visit(ASTMake *node) override {}                                    \
  std::any visit(ASTInitializerList *node) override {}                         \
  std::any visit(ASTEnumDeclaration *node) override {}                         \
  


#define DECLARE_VISIT_BASE_METHODS()                                           \
  virtual std::any visit(ASTProgram *node) = 0;                                \
  virtual std::any visit(ASTBlock *node) = 0;                                  \
  virtual std::any visit(ASTFunctionDeclaration *node) = 0;                               \
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
  virtual std::any visit(ASTDotExpr *node) = 0;                                \
  virtual std::any visit(ASTSubscript *node) = 0;                              \
  virtual std::any visit(ASTMake *node) = 0;                                   \
  virtual std::any visit(ASTInitializerList *node) = 0;                        \
  virtual std::any visit(ASTEnumDeclaration *node) = 0;                        \

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
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;

  inline Token peek() const {
    if (states.empty()) {
      return Token::Eof();
    }
    return states.back().lookahead_buffer.front();
  }

#define lookahead_buf() states.back().lookahead_buffer

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

  inline void fill_buffer_if_needed() {
    while (states.back().lookahead_buffer.size() < 8) {
      lexer.get_token(states.back());
    }
  }

  inline Token eat() {
    all_tokens.push_back(peek());
    token_idx++;
    
    fill_buffer_if_needed();

    if (peek().is_eof() && states.size() > 1) {
      states.pop_back();
      fill_buffer_if_needed();
      return peek();
    }
    auto tok = peek();
    lookahead_buf().pop_front();
    lexer.get_token(states.back());
    return tok;
  }

  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == TType::Semi; }

  inline Token expect(TType type) {
    fill_buffer_if_needed();
    if (peek().type != type) {
      SourceRange range = {std::max(token_idx - 5, int64_t()), token_idx + 5};
      throw_error(std::format("Expected {}, got {} : {}", TTypeToString(type),
                              TTypeToString(peek().type), peek().value),
                  ERROR_CRITICAL, range);
    }
    return eat();
  }

  Lexer lexer{};

  std::vector<Lexer::State> states;

  int64_t token_idx{};

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
                    ERROR_FAILURE, {std::max(token_idx - 5, int64_t()), std::max(token_idx + 5, int64_t())});
      }
    }
    return nullptr;
  }

  SourceRange begin_node() {
    return SourceRange{.begin = token_idx, .begin_loc = (int64_t)peek().location.line};
  }
  
  void end_node(ASTNode *node, SourceRange &range) {
    range.end = token_idx;
    range.end_loc = peek().location.line;
    if (node)
      node->source_range = range;
  }

  ASTType *parse_type();
  ASTProgram *parse();
  ASTStatement *parse_statement();
  ASTArguments *parse_arguments();
  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTDeclaration *parse_declaration();
  ASTFunctionDeclaration *parse_function_declaration(Token);
  ASTParamsDecl *parse_parameters();
  ASTEnumDeclaration *parse_enum_declaration(Token);

  ASTBlock *parse_block();
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
  ASTExpr *parse_postfix();
  ASTExpr *parse_primary();
  ASTCall *parse_call(const Token &);
  
  
  
};