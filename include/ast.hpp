#pragma once

#include <any>
#include <cstdint>
#include <cstdio>
#include <deque>
#include <functional>
#include <vector>

#include "arena.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

extern jstl::Arena ast_arena;

struct VisitorBase;

// used to prevent double includes.
extern std::unordered_set<InternedString> import_set;

enum ASTNodeType {
  AST_NODE_PROGRAM,
  AST_NODE_BLOCK,
  AST_NODE_FUNCTION_DECLARATION,
  AST_NODE_PARAMS_DECL,
  AST_NODE_PARAM_DECL,
  AST_NODE_DECLARATION,
  AST_NODE_EXPR_STATEMENT,
  AST_NODE_BIN_EXPR,
  AST_NODE_UNARY_EXPR,
  AST_NODE_IDENTIFIER,
  AST_NODE_LITERAL,
  AST_NODE_TYPE,
  AST_NODE_TUPLE,
  AST_NODE_CALL,
  AST_NODE_ARGUMENTS,
  AST_NODE_RETURN,
  AST_NODE_CONTINUE,
  AST_NODE_BREAK,
  AST_NODE_FOR,
  AST_NODE_IF,
  AST_NODE_ELSE,
  AST_NODE_WHILE,
  AST_NODE_STRUCT_DECLARATION,
  AST_NODE_DOT_EXPR,
  AST_NODE_SCOPE_RESOLUTION,
  AST_NODE_SUBSCRIPT,
  AST_NODE_MAKE,
  AST_NODE_INITIALIZER_LIST,
  AST_NODE_ENUM_DECLARATION,
  AST_NODE_UNION_DECLARATION,
  AST_NODE_TAGGED_UNION_DECLARATION,
  AST_NODE_NOOP,
  AST_NODE_ALIAS,
  AST_NODE_IMPL,
  AST_NODE_DEFER,
  AST_NODE_RANGE,
  AST_NODE_SWITCH,
  AST_NODE_TUPLE_DECONSTRUCTION,
  AST_NODE_STATEMENT_LIST, // Used just to return a bunch of statments from a single directive.s
};

struct ASTBlock;
struct ASTNode {
  Nullable<ASTBlock> declaring_block;
  SourceRange source_range{};
  int resolved_type = Type::invalid_id;
  virtual ~ASTNode() = default;
  virtual std::any accept(VisitorBase *visitor) = 0;
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTStatement : ASTNode {
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTStatementList : ASTStatement {
  std::vector<ASTNode *> statements;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_STATEMENT_LIST; }
};

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

#define BLOCK_FLAG_TO_STRING(flag)                                                                                     \
  if (flags & flag)                                                                                                    \
    result += #flag " ";

inline static std::string block_flags_to_string(int flags) {
  std::string result;
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_FALL_THROUGH)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_RETURN)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_CONTINUE)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_BREAK)
  return result;
}

struct DestructorCall {
  InternedString variable_name;
  InternedString destructor_fn_name;
  int type_id;
  int statement_idx;
};

struct ASTBlock : ASTStatement {
  ASTNode *parent;
  // List of identifiers to call destructors on.
  // types associate with them too, so we don't have to look it up later.
  // also, we have the statement idx, so we know how many destructors to acll in an early return
  // where there maybe more decarations after it.
  std::vector<DestructorCall> identifiers_to_destruct_on_block_exit;

  int flags = BLOCK_FLAGS_FALL_THROUGH;
  bool has_defer = false;
  int defer_count = 0;
  int return_type = Type::invalid_id;
  Scope *scope;
  std::vector<ASTNode *> statements;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BLOCK; }
};

struct ASTProgram : ASTNode {
  std::vector<ASTStatement *> statements;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PROGRAM; }
};

struct ASTExpr : ASTNode {
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTTypeExtension {
  TypeExtEnum type;
  ASTExpr *expression;
};

struct ASTType : ASTExpr {
  enum Kind {
    NORMAL,
    REFLECTION,
    TUPLE,
    FUNCTION,
  } kind = NORMAL;
  
  union {
    struct {
      InternedString base;
      std::vector<ASTType *> generic_arguments;
    } normal;
    struct {
      std::vector<ASTType *> parameter_types;
      Nullable<ASTType> return_type;
    } function;
    // special info for tuple types.
    std::vector<ASTType *> tuple_types;
  };

  ASTType() {}

  ASTType(const ASTType &other) {
    extensions = other.extensions;
    kind = other.kind;
    pointing_to = other.pointing_to;
    switch (kind) {
      case NORMAL:
      case REFLECTION:
        normal = decltype(normal)(other.normal);
        break;
      case TUPLE:
        tuple_types = decltype(tuple_types)(other.tuple_types);
        break;
      case FUNCTION:
        function = decltype(function)(other.function);
        break;
    }
  }

  ~ASTType() {}
  // [], *, [string] etc.
  std::vector<ASTTypeExtension> extensions;
  // special info for reflection
  Nullable<ASTExpr> pointing_to;

  ASTNodeType get_node_type() const override { return AST_NODE_TYPE; }
  static ASTType *get_void();
  std::any accept(VisitorBase *visitor) override;
};
struct ASTExprStatement : ASTStatement {
  ASTExpr *expression;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_EXPR_STATEMENT; }
};

// All of our declarations could inherit from a base declaration. I am not sure
// if that would be useful.
struct ASTDeclaration : ASTStatement {
  InternedString name;
  InternedString bitsize;

  // This isn't nullable, even though it can be null for part of compilation.
  // That's because if it ever was null, when it's done typing it will have been created.
  // It creates too much friction later on down the line if it's not.
  ASTType *type = nullptr;
  Nullable<ASTExpr> value;

  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DECLARATION; }

  bool is_constexpr = false;
  bool is_static = false;
  bool is_bitfield = false;
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  Token op;

  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BIN_EXPR; }
};
struct ASTUnaryExpr : ASTExpr {
  ASTExpr *operand;
  Token op;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_UNARY_EXPR; }
};
struct ASTIdentifier : ASTExpr {
  InternedString value;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_IDENTIFIER; }
};
struct ASTLiteral : ASTExpr {
  enum Tag {
    Integer,
    Float,
    String,
    RawString,
    InterpolatedString,
    Char,
    Bool,
    Null,
  } tag;
  std::vector<ASTExpr *> interpolated_values{};
  InternedString value;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_LITERAL; }
};

struct ASTTupleDeconstruction : ASTStatement {
  std::vector<ASTIdentifier *> idens;
  ASTExpr *right;

  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE_DECONSTRUCTION; }
};

struct ASTTuple : ASTExpr {
  ASTType *type;
  std::vector<ASTExpr *> values;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE; }
};

struct ASTParamDecl : ASTNode {
  ASTType *type;
  Nullable<ASTExpr> default_value;
  InternedString name;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PARAM_DECL; }
};

struct ASTParamsDecl : ASTStatement {
  std::vector<ASTParamDecl *> params;
  bool has_self = false;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PARAMS_DECL; }
};

struct GenericInstance {
  std::vector<int> arguments;
  ASTStatement *node;
  int type;
};

struct ASTFunctionDeclaration : ASTStatement {
  size_t flags = 0;
  bool has_defer = false;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;

  Scope *scope;
  ASTParamsDecl *params;
  Nullable<ASTBlock> block;
  InternedString name;
  ASTType *return_type;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_FUNCTION_DECLARATION; }
};

struct ASTArguments : ASTNode {
  std::vector<ASTExpr *> arguments;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ARGUMENTS; }
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
  ASTNodeType get_node_type() const override { return AST_NODE_MAKE; }
};
struct ASTCall : ASTExpr {
  ASTExpr *function;
  ASTArguments *arguments;
  std::vector<ASTType *> generic_arguments;
  int type = Type::invalid_id;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CALL; }
};
struct ASTDotExpr : ASTExpr {
  ASTExpr *base;
  InternedString member_name;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DOT_EXPR; }
};

struct ASTScopeResolution : ASTExpr {
  ASTExpr *base;
  InternedString member_name;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SCOPE_RESOLUTION; }
};

struct ASTReturn : ASTStatement {
  Nullable<ASTExpr> expression;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_RETURN; }
};
struct ASTBreak : ASTStatement {
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BREAK; }
};
struct ASTContinue : ASTStatement {
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CONTINUE; }
};

enum ValueSemantic {
  VALUE_SEMANTIC_COPY,
  VALUE_SEMANTIC_POINTER,
};

struct ASTRange : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_RANGE; }
};

struct ASTFor : ASTStatement {
  ASTExpr *iden;
  ASTExpr *range;
  ValueSemantic value_semantic;
  ASTBlock *block;

  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_FOR; }
};

struct ASTElse;
struct ASTIf : ASTStatement {
  ASTExpr *condition;
  ASTBlock *block;
  Nullable<ASTElse> _else; // just an else.
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_IF; }
};

struct ASTElse : ASTStatement {
  Nullable<ASTIf> _if; // conditional else.
  Nullable<ASTBlock> block;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ELSE; }
};

struct ASTWhile : ASTStatement {
  Nullable<ASTExpr> condition;
  ASTBlock *block;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_WHILE; }
};

struct ASTSubscript : ASTExpr {
  ASTExpr *left;
  ASTExpr *subscript;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SUBSCRIPT; }
};

struct ASTUnionDeclaration;

struct ASTStructDeclaration : ASTStatement {
  Scope *scope;
  InternedString name;

  bool is_fwd_decl = false;
  bool is_extern = false;

  std::vector<ASTDeclaration *> fields;
  std::vector<ASTUnionDeclaration *> unions;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;

  std::any accept(VisitorBase *visitor) override;

  ASTNodeType get_node_type() const override { return AST_NODE_STRUCT_DECLARATION; }
};

struct ASTInitializerList : ASTExpr {
  ASTInitializerList() {}
  ~ASTInitializerList() {}

  ASTInitializerList(const ASTInitializerList &other) {
    tag = other.tag;
    switch (tag) {
      case INIT_LIST_NAMED:
        new (&key_values) std::vector<std::pair<InternedString, ASTExpr *>>(other.key_values);
        break;
      case INIT_LIST_COLLECTION:
        new (&values) std::vector<ASTExpr *>(other.values);
        break;
      case INIT_LIST_EMPTY:
        break;
    }
  }

  // Key values.
  union {
    std::vector<std::pair<InternedString, ASTExpr *>> key_values;
    std::vector<ASTExpr *> values;
  };

  enum {
    INIT_LIST_EMPTY,
    INIT_LIST_NAMED,
    INIT_LIST_COLLECTION,
  } tag;

  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_INITIALIZER_LIST; }
};

struct ASTEnumDeclaration : ASTStatement {
  bool is_flags = false;
  int element_type;
  InternedString name;
  std::vector<std::pair<InternedString, Nullable<ASTExpr>>> key_values;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ENUM_DECLARATION; }
};

struct ASTUnionDeclaration : ASTStatement {
  Scope *scope;
  InternedString name;
  int kind = UNION_IS_NORMAL;
  bool is_fwd_decl = false;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTDeclaration *> fields;
  std::vector<ASTStructDeclaration *> structs;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_UNION_DECLARATION; }
};

struct ASTTaggedUnionDeclaration : ASTStatement {
  InternedString name;
  Scope *scope;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTNode *> members;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_UNION_DECLARATION; }
};

struct SwitchCase {
  ASTExpr *expression;
  ASTBlock *block;
};

struct ASTSwitch : ASTExpr {
  bool is_statement = false;
  int return_type = void_type();
  ASTExpr *target;
  std::vector<SwitchCase> cases;
  std::any accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SWITCH; }
};

struct ASTNoop : ASTStatement {
  ASTNodeType get_node_type() const override { return AST_NODE_NOOP; }
  std::any accept(VisitorBase *visitor) override;
};

struct ASTAlias : ASTStatement {
  InternedString name;
  ASTType *type;
  ASTNodeType get_node_type() const override { return AST_NODE_ALIAS; }
  std::any accept(VisitorBase *visitor) override;
};

// TODO: add interface field, once we have interfaces lol.
struct ASTImpl : ASTStatement {
  // impl 'target' or impl *interface for 'target'
  ASTType *target;
  // methods / static methods this is implementing for the type.
  std::vector<ASTFunctionDeclaration *> methods;
  ASTNodeType get_node_type() const override { return AST_NODE_IMPL; }
  std::any accept(VisitorBase *visitor) override;
};

struct ASTDefer : ASTStatement {
  ASTNode *statement;
  ASTNodeType get_node_type() const override { return AST_NODE_DEFER; }
  std::any accept(VisitorBase *visitor) override;
};

// Use this only for implementing the methods, so you can use the IDE to expand
// it.
#define DECLARE_VISIT_METHODS()                                                                                        \
  std::any visit(ASTProgram *node) override {}                                                                         \
  std::any visit(ASTBlock *node) override {}                                                                           \
  std::any visit(ASTFunctionDeclaration *node) override {}                                                             \
  std::any visit(ASTParamsDecl *node) override {}                                                                      \
  std::any visit(ASTParamDecl *node) override {}                                                                       \
  std::any visit(ASTDeclaration *node) override {}                                                                     \
  std::any visit(ASTExprStatement *node) override {}                                                                   \
  std::any visit(ASTBinExpr *node) override {}                                                                         \
  std::any visit(ASTUnaryExpr *node) override {}                                                                       \
  std::any visit(ASTIdentifier *node) override {}                                                                      \
  std::any visit(ASTLiteral *node) override {}                                                                         \
  std::any visit(ASTType *node) override {}                                                                            \
  std::any visit(ASTCall *node) override {}                                                                            \
  std::any visit(ASTArguments *node) override {}                                                                       \
  std::any visit(ASTReturn *node) override {}                                                                          \
  std::any visit(ASTContinue *node) override {}                                                                        \
  std::any visit(ASTBreak *node) override {}                                                                           \
  std::any visit(ASTFor *node) override {}                                                                             \
  std::any visit(ASTIf *node) override {}                                                                              \
  std::any visit(ASTElse *node) override {}                                                                            \
  std::any visit(ASTWhile *node) override {}                                                                           \
  std::any visit(ASTStructDeclaration *node) override {}                                                               \
  std::any visit(ASTDotExpr *node) override {}                                                                         \
  std::any visit(ASTSubscript *node) override {}                                                                       \
  std::any visit(ASTMake *node) override {}                                                                            \
  std::any visit(ASTInitializerList *node) override {}                                                                 \
  std::any visit(ASTEnumDeclaration *node) override {}                                                                 \
  std::any visit(ASTUnionDeclaration *node) override {}                                                                \
  std::any visit(ASTRange *node) override {};                                                                          \
  std::any visit(ASTSwitch *node) override {};                                                                         \
  std::any visit(ASTTuple *node) override {};                                                                          \
  std::any visit(ASTAlias *node) override {};                                                                          \
  std::any visit(ASTTupleDeconstruction *node) override {};                                                            \
  std::any visit(ASTDefer *node) override {};\
  std::any visit(ASTTaggedUnionDeclaration *node) override {};

#define DECLARE_VISIT_BASE_METHODS()                                                                                   \
  std::any visit(ASTNoop *noop) { return {}; }                                                                         \
  virtual std::any visit(ASTScopeResolution *node) = 0;                                                                \
  virtual std::any visit(ASTProgram *node) = 0;                                                                        \
  virtual std::any visit(ASTBlock *node) = 0;                                                                          \
  virtual std::any visit(ASTFunctionDeclaration *node) = 0;                                                            \
  virtual std::any visit(ASTParamsDecl *node) = 0;                                                                     \
  virtual std::any visit(ASTParamDecl *node) = 0;                                                                      \
  virtual std::any visit(ASTDeclaration *node) = 0;                                                                    \
  virtual std::any visit(ASTExprStatement *node) = 0;                                                                  \
  virtual std::any visit(ASTBinExpr *node) = 0;                                                                        \
  virtual std::any visit(ASTUnaryExpr *node) = 0;                                                                      \
  virtual std::any visit(ASTIdentifier *node) = 0;                                                                     \
  virtual std::any visit(ASTLiteral *node) = 0;                                                                        \
  virtual std::any visit(ASTType *node) = 0;                                                                           \
  virtual std::any visit(ASTCall *node) = 0;                                                                           \
  virtual std::any visit(ASTArguments *node) = 0;                                                                      \
  virtual std::any visit(ASTReturn *node) = 0;                                                                         \
  virtual std::any visit(ASTContinue *node) = 0;                                                                       \
  virtual std::any visit(ASTBreak *node) = 0;                                                                          \
  virtual std::any visit(ASTFor *node) = 0;                                                                            \
  virtual std::any visit(ASTIf *node) = 0;                                                                             \
  virtual std::any visit(ASTElse *node) = 0;                                                                           \
  virtual std::any visit(ASTWhile *node) = 0;                                                                          \
  virtual std::any visit(ASTStructDeclaration *node) = 0;                                                              \
  virtual std::any visit(ASTDotExpr *node) = 0;                                                                        \
  virtual std::any visit(ASTSubscript *node) = 0;                                                                      \
  virtual std::any visit(ASTMake *node) = 0;                                                                           \
  virtual std::any visit(ASTInitializerList *node) = 0;                                                                \
  virtual std::any visit(ASTEnumDeclaration *node) = 0;                                                                \
  virtual std::any visit(ASTUnionDeclaration *node) = 0;                                                               \
  virtual std::any visit(ASTRange *node) = 0;                                                                          \
  virtual std::any visit(ASTSwitch *node) = 0;                                                                         \
  virtual std::any visit(ASTTuple *node) = 0;                                                                          \
  virtual std::any visit(ASTAlias *node) = 0;                                                                          \
  virtual std::any visit(ASTImpl *node) = 0;                                                                           \
  virtual std::any visit(ASTTupleDeconstruction *node) = 0;                                                            \
  virtual std::any visit(ASTDefer *node) = 0;                                                                          \
  virtual std::any visit(ASTTaggedUnionDeclaration *node) = 0;

enum DirectiveKind {
  DIRECTIVE_KIND_STATEMENT,
  DIRECTIVE_KIND_EXPRESSION,
  DIRECTIVE_KIND_DONT_CARE,
};

struct Parser;
struct DirectiveRoutine {
  ~DirectiveRoutine() = default;
  InternedString identifier;
  DirectiveKind kind;
  std::function<Nullable<ASTNode>(Parser *parser)> run;
};

// VERIFY(Josh) 10/5/2024, 10:33:32 AM
// make sure that this precedence scheme makes sense.
enum Precedence {
  PRECEDENCE_LOWEST,
  PRECEDENCE_ASSIGNMENT,    // =, :=
  PRECEDENCE_LOGICALOR,     // ||
  PRECEDENCE_LOGICALAND,    // &&
  PRECEDENCE_EQUALITY,      // ==, !=
  PRECEDENCE_RELATIONAL,    // <, >, <=, >=
  PRECEDENCE_BITWISEOR,     // |
  PRECEDENCE_BITWISEXOR,    // ^
  PRECEDENCE_BITWISEAND,    // &
  PRECEDENCE_SHIFT,         // <<, >>
  PRECEDENCE_ADDITIVE,      // +, -
  PRECEDENCE_MULTIPLICATIVE // *, /, %
  // these aren't handled here, but this is the continuation
  // unary
  // posfix
};

static Precedence get_operator_precedence(Token token);

struct Typer;

struct Parser {
  Typer *typer;
  static Nullable<ASTBlock> current_block;
  ASTProgram *parse();
  ASTStatement *parse_statement();
  ASTArguments *parse_arguments();
  ASTTupleDeconstruction *parse_multiple_asssignment();
  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTDeclaration *parse_declaration();
  ASTFunctionDeclaration *parse_function_declaration(Token);
  std::vector<GenericParameter> parse_generic_parameters();
  std::vector<ASTType *> parse_generic_arguments();
  ASTUnionDeclaration *parse_union_declaration(Token);
  ASTTaggedUnionDeclaration *parse_tagged_union_declaration(Token name);
  ASTParamsDecl *parse_parameters(std::vector<GenericParameter> params = {});
  void visit_struct_statements(ASTStructDeclaration *decl, const std::vector<ASTNode *> &statements);
  ASTEnumDeclaration *parse_enum_declaration(Token);
  ASTBlock *parse_block(Scope *scope = nullptr);
  ASTExpr *parse_expr(Precedence = PRECEDENCE_LOWEST);
  ASTExpr *parse_unary();
  ASTExpr *parse_postfix();
  ASTExpr *parse_primary();
  ASTCall *parse_call(ASTExpr *function);
  ASTImpl *parse_impl();

  // ASTType* parsing routines
  ASTType *parse_type();
  std::vector<ASTType *> parse_parameter_types();
  void append_type_extensions(ASTType *type);

  ASTType *parse_function_type();

  ASTDefer *parse_defer();

  Nullable<ASTNode> process_directive(DirectiveKind kind, const InternedString &identifier);
  Nullable<ASTExpr> try_parse_directive_expr();

  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == TType::Semi; }
  InternedString type_name(ASTExpr *node);

  inline std::deque<Token> &lookahead_buf() { return states.back().lookahead_buffer; }

  Token eat();
  Token expect(TType type);
  Token peek() const;

  void fill_buffer_if_needed();
  SourceRange begin_node();
  void end_node(ASTNode *node, SourceRange &range);

  Parser(const std::string &filename, Context &context);
  ~Parser();

  Context &ctx;
  Lexer lexer{};
  std::vector<Lexer::State> states;
  Nullable<ASTUnionDeclaration> current_union_decl = nullptr;
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;
  Nullable<ASTImpl> current_impl = nullptr;

  static std::vector<DirectiveRoutine> directive_routines;
  int64_t token_idx{};
};

template <class T> static inline T *ast_alloc(size_t n = 1) {
  auto node = new (ast_arena.allocate(sizeof(T) * n)) T();
  node->declaring_block = Parser::current_block;
  return node;
}