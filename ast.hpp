#pragma once

#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

#include <any>
#include <cstdint>
#include <cstdio>
#include <deque>
#include <functional>
#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>
#include <unordered_set>
#include <vector>

enum {
  ASTTYPE_EMIT_OBJECT,
};

extern jstl::Arena ast_arena;

template <class T> T *ast_alloc(size_t n = 1) {
  return new (ast_arena.allocate(sizeof(T) * n)) T();
}

struct VisitorBase;
extern std::unordered_set<std::string> import_set;

// TODO: add an enum member in the base that says what type this node is,
// so we can be more performant and just static_cast<T*> instead of
// dynamic_cast<T*>;
// TODO: add a set of common ASTNode flags like unused, unresolved_symbol, etc.
// this way we can use functions and type declarations before theyre defined, prune unused code, etc.
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
  Nullable<ASTExpr> pointing_to;
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

// All of our declarations could inherit from a base declaration. I am not sure if that would be useful.
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
    InterpolatedString,
    Bool,
    Null,
  } tag;
  std::vector<ASTExpr*> interpolated_values {};
  std::string value;
  std::any accept(VisitorBase *visitor) override;
};

// CLEANUP(Josh) 10/1/2024, 10:31:35 AM This node is entirely unneccesary, and pruning any node we don't need from the AST significantly reduces code complexity.
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

// CLEANUP(Josh) 10/1/2024, 10:32:12 AM
// Remove these 3 nodes and have a generic ASTControlFlowChanger or something like that
// that just uses an enum tag and has an expr field.
// it would simplify our stuff.
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

enum ValueSemantic {
  VALUE_SEMANTIC_COPY,
  VALUE_SEMANTIC_POINTER,
  VALUE_SEMANTIC_MOVE, // todo: add a way to do this.
}; 

struct ASTFor : ASTStatement {
  enum ForType {
    RangeBased,
    CStyle,
  } tag;

  union {
    struct {
      ValueSemantic value_semantic;
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

// TODO(Josh) 10/1/2024, 10:33:12 AM Refactor the way this works to allow better typing and more flexibility with this. It really only works 
// for a couple types right now.
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


struct Sum {
  enum Tags {
    SUM_TAG_1,
    SUM_TAG_0,
  } tag;
  union Data {
    int one;
    int zero;
  } data;
};

struct ASTUnionDeclaration : ASTStatement {
  Scope *scope;
  Token name;
  ASTType* type;
  UnionKind kind = UNION_IS_NORMAL;
  std::vector<ASTDeclaration*> fields;
  std::vector<ASTFunctionDeclaration*> methods;
  std::vector<ASTStructDeclaration*> structs;
  std::any accept(VisitorBase *visitor) override;
};

struct ASTAllocate : ASTExpr {
  Nullable<ASTType> type;
  Nullable<ASTArguments> arguments;  
  
  enum Kind {
    New,
    Delete,
  } kind;
  std::any accept(VisitorBase *visitor) override;
};

struct AllocationInfo {
  Symbol *symbol;
  ASTAllocate* allocation;
};

extern std::vector<ASTAllocate*> allocation_info;

void insert_allocation(ASTAllocate *allocation);
void erase_allocation(ASTAllocate *allocation);

bool report_unfreed_allocations();
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
  std::any visit(ASTStructDeclaration *node) override {}                       \
  std::any visit(ASTDotExpr *node) override {}                                 \
  std::any visit(ASTSubscript *node) override {}                               \
  std::any visit(ASTMake *node) override {}                                    \
  std::any visit(ASTInitializerList *node) override {}                         \
  std::any visit(ASTEnumDeclaration *node) override {}                         \
  std::any visit(ASTUnionDeclaration *node) override {}                        \
  std::any visit(ASTAllocate *node) override {};                        \
  


#define DECLARE_VISIT_BASE_METHODS()                                           \
  std::any visit(ASTNoop *noop) { return {}; }                                 \
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
  virtual std::any visit(ASTStructDeclaration *node) = 0;                      \
  virtual std::any visit(ASTDotExpr *node) = 0;                                \
  virtual std::any visit(ASTSubscript *node) = 0;                              \
  virtual std::any visit(ASTMake *node) = 0;                                   \
  virtual std::any visit(ASTInitializerList *node) = 0;                        \
  virtual std::any visit(ASTEnumDeclaration *node) = 0;                        \
  virtual std::any visit(ASTUnionDeclaration *node) = 0;                        \
  virtual std::any visit(ASTAllocate *node) = 0;                        \

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


enum Precedence {
  PRECEDENCE_LOWEST,
  PRECEDENCE_ASSIGNMENT,    // =, :=
  PRECEDENCE_LOGICALOR,     // ||
  PRECEDENCE_LOGICALAND,    // &&
  PRECEDENCE_BITWISEOR,     // |
  PRECEDENCE_BITWISEXOR,    // ^
  PRECEDENCE_BITWISEAND,    // &
  PRECEDENCE_EQUALITY,      // ==, !=
  PRECEDENCE_RELATIONAL,    // <, >, <=, >=
  PRECEDENCE_SHIFT,         // <<, >>
  PRECEDENCE_ADDITIVE,      // +, -
  PRECEDENCE_MULTIPLICATIVE // *, /, %
};

static inline Precedence get_operator_precedence(Token token) {
  if (token.is_comp_assign()) {
    return PRECEDENCE_ASSIGNMENT;
  }
  auto type = token.type;
  switch (type) {
    case TType::Assign:
    case TType::ColonEquals:
      return PRECEDENCE_ASSIGNMENT;
    case TType::LogicalOr:
      return PRECEDENCE_LOGICALOR;
    case TType::LogicalAnd:
      return PRECEDENCE_LOGICALAND;
    case TType::Or:
      return PRECEDENCE_BITWISEOR;
    case TType::Xor:
      return PRECEDENCE_BITWISEXOR;
    case TType::And:
      return PRECEDENCE_BITWISEAND;
    case TType::EQ:
    case TType::NEQ:
      return PRECEDENCE_EQUALITY;
    case TType::LT:
    case TType::GT:
    case TType::LE:
    case TType::GE:
      return PRECEDENCE_RELATIONAL;
    case TType::SHL:
    case TType::SHR:
      return PRECEDENCE_SHIFT;
    case TType::Add:
    case TType::Sub:
      return PRECEDENCE_ADDITIVE;
    case TType::Mul:
    case TType::Div:
    case TType::Modulo:
      return PRECEDENCE_MULTIPLICATIVE;
    default:
      return PRECEDENCE_LOWEST;
  }
}

struct Parser {
  bool allow_function_type_parsing = true;
  ASTProgram *parse();
  ASTStatement *parse_statement();
  ASTArguments *parse_arguments();
  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTDeclaration *parse_declaration();
  ASTFunctionDeclaration *parse_function_declaration(Token);
  ASTUnionDeclaration *parse_union_declaration(Token);
  ASTParamsDecl *parse_parameters();
  ASTEnumDeclaration *parse_enum_declaration(Token);
  ASTBlock *parse_block();
  ASTExpr *parse_expr(Precedence = PRECEDENCE_LOWEST);
  ASTExpr *parse_unary();
  ASTExpr *parse_postfix();
  ASTExpr *parse_primary();
  ASTCall *parse_call(const Token &);
  
  // ASTType* parsing routines
  
  ASTType *parse_type();
  std::vector<ASTType *> parse_parameter_types() {
    std::vector<ASTType *> param_types;
    expect(TType::LParen);
    while (peek().type != TType::RParen) {
      auto param_type = parse_type();
      param_types.push_back(param_type);
      if (peek().type == TType::Comma) {
        expect(TType::Comma);
      } else {
        break;
      }
    }
    expect(TType::RParen);
    return param_types;
  }
  void parse_type_extensions(ASTType *type) {
    while (peek().type == TType::Mul || peek().type == TType::LBrace) {
      if (peek().type == TType::Mul) {
        eat();
        type->extension_info.extensions.push_back(TYPE_EXT_POINTER);
      } else if (peek().type == TType::LBrace) {
        type->extension_info.extensions.push_back(TYPE_EXT_ARRAY);
        expect(TType::LBrace);
        if (peek().type == TType::Integer) {
          auto integer = expect(TType::Integer);
          type->extension_info.array_sizes.push_back(std::stoi(integer.value));
        } else {
          type->extension_info.array_sizes.push_back(-1);
        }
        expect(TType::RBrace);
      }
    }
  }
  
  ASTType *parse_function_type(const std::string &base, TypeExt extension_info) {
    auto return_type = ast_alloc<ASTType>();
    return_type->base = base;
    return_type->extension_info = extension_info;
    
    FunctionTypeInfo info{};
    info.return_type = ctx.scope->find_type_id(return_type->base, return_type->extension_info);

    auto param_types = parse_parameter_types();
    std::ostringstream ss;
    
    // convert parameter types to a string
    {
      ss << "(";
      for (size_t i = 0; i < param_types.size(); ++i) {
        info.parameter_types[i] = ctx.scope->find_type_id(param_types[i]->base, param_types[i]->extension_info);
        ss << ctx.scope->get_type(info.parameter_types[i])->to_string();
        if (i != param_types.size() - 1) {
          ss << ", ";
        }
      }
      ss << ")";
    }
    
    auto type_name = ctx.scope->get_type(info.return_type)->to_string() + ss.str();
    return_type->resolved_type = ctx.scope->find_type_id(type_name, info, {});
    return_type->base = type_name;
    return_type->extension_info = {};

    parse_type_extensions(return_type);

    return return_type;
  }
  
  
  Nullable<ASTNode> process_directive(DirectiveKind kind,
                                      const std::string &identifier);
  void init_directive_routines();
  Nullable<ASTExpr> try_parse_directive_expr();
  
  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == TType::Semi; }

  inline std::deque<Token>& lookahead_buf() { return states.back().lookahead_buffer; }
  
  



  Token eat();
  Token expect(TType type);
  Token peek() const;
  
  void fill_buffer_if_needed();
  SourceRange begin_node();
  void end_node(ASTNode *node, SourceRange &range);

  Parser(const std::string &contents, const std::string &filename,
         Context &context)
      : states({Lexer::State::from_file(contents, filename)}),
        ctx(context) {
    init_directive_routines();
    fill_buffer_if_needed();
    // for (int i = lookahead_buf().size(); i < 8; ++i) {
    //   lexer.get_token(states.back());
    // }
  }
  
  Context &ctx;
  Lexer lexer{};
  std::vector<Lexer::State> states;
  Nullable<ASTUnionDeclaration> current_union_decl = nullptr;
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;
  std::vector<DirectiveRoutine> directive_routines;
  int64_t token_idx{};
};