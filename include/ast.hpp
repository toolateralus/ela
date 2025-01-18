#pragma once


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
  AST_NODE_SELF_TYPE,
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
  AST_NODE_INITIALIZER_LIST,
  AST_NODE_ENUM_DECLARATION,
  AST_NODE_TAGGED_UNION_DECLARATION,
  AST_NODE_NOOP,
  AST_NODE_ALIAS,
  AST_NODE_IMPL,
  AST_NODE_INTERFACE_DECLARATION,
  AST_NODE_DEFER,
  AST_NODE_CAST,
  AST_NODE_LAMBDA,
  AST_NODE_RANGE,
  AST_NODE_SWITCH,
  AST_NODE_TUPLE_DECONSTRUCTION,
  AST_NODE_STATEMENT_LIST, // Used just to return a bunch of statments from a single directive.s
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

struct ASTBlock;
struct ASTNode {
  ControlFlow control_flow = {
    .flags = BLOCK_FLAGS_FALL_THROUGH,
    .type = -1,
  };
  Nullable<ASTBlock> declaring_block;
  SourceRange source_range{};
  int resolved_type = Type::invalid_id;
  virtual ~ASTNode() = default;
  virtual void accept(VisitorBase *visitor) = 0;
  virtual ASTNodeType get_node_type() const = 0;

  inline bool is_expr() {
    switch (get_node_type()) {
      case AST_NODE_BIN_EXPR:
      case AST_NODE_UNARY_EXPR:
      case AST_NODE_IDENTIFIER:
      case AST_NODE_LITERAL:
      case AST_NODE_TYPE:
      case AST_NODE_TUPLE:
      case AST_NODE_CALL:
      case AST_NODE_ARGUMENTS:
      case AST_NODE_SELF_TYPE:
      case AST_NODE_DOT_EXPR:
      case AST_NODE_SCOPE_RESOLUTION:
      case AST_NODE_SUBSCRIPT:
      case AST_NODE_INITIALIZER_LIST:
      case AST_NODE_CAST:
      case AST_NODE_RANGE:
      case AST_NODE_SWITCH:
      return true;
      default:
      return false;
    }
  }
};

struct ASTStatement : ASTNode {
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTStatementList : ASTStatement {
  std::vector<ASTNode *> statements;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_STATEMENT_LIST; }
};


inline static std::string block_flags_to_string(int flags) {
  std::string result;
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_FALL_THROUGH)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_RETURN)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_CONTINUE)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_BREAK)
  return result;
}

struct ASTBlock : ASTStatement {
  ASTNode *parent;
  int flags = BLOCK_FLAGS_FALL_THROUGH;
  bool has_defer = false;
  int defer_count = 0;
  int return_type = Type::invalid_id;
  Scope *scope;
  std::vector<ASTNode *> statements;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BLOCK; }
};

struct ASTProgram : ASTNode {
  std::vector<ASTStatement *> statements;
  void accept(VisitorBase *visitor) override;
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
    SELF,
  } kind = NORMAL;

  union {
    struct {
      ASTExpr *base;
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
      case SELF:
      case REFLECTION:
        normal = decltype(normal)(other.normal);
        break;
      case TUPLE:
        tuple_types = decltype(tuple_types)(other.tuple_types);
        break;
      case FUNCTION:
        function = decltype(function)(other.function);
        break;
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
  void accept(VisitorBase *visitor) override;
};

struct ASTExprStatement : ASTStatement {
  ASTExpr *expression;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_EXPR_STATEMENT; }
};

struct ASTDeclaration : ASTStatement {
  InternedString name;
  InternedString bitsize;

  // This isn't nullable, even though it can be null for part of compilation.
  // That's because if it ever was null, when it's done typing it will have been created.
  // It creates too much friction later on down the line if it's not.
  ASTType *type = nullptr;
  Nullable<ASTExpr> value;

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DECLARATION; }

  bool is_constexpr = false;
  bool is_static = false;
  bool is_bitfield = false;
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  Token op;
  bool is_operator_overload = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BIN_EXPR; }
};
struct ASTUnaryExpr : ASTExpr {
  ASTExpr *operand;
  Token op;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_UNARY_EXPR; }
};
struct ASTIdentifier : ASTExpr {
  ASTIdentifier() {}
  ASTIdentifier(const InternedString &value) : value(value) {}
  InternedString value;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_IDENTIFIER; }
};

struct InterpolatedStringSegment {
  InternedString             prefix;
  ASTExpr                   *expression;
  InterpolatedStringSegment *next = nullptr;
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

  ~ASTLiteral() {
    if (tag == InterpolatedString) {
      delete(interpolated_string_root);
    }
  }
  InterpolatedStringSegment *interpolated_string_root;
  InternedString value;
  bool is_c_string = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_LITERAL; }
};


struct ASTTupleDeconstruction : ASTStatement {
  std::vector<ASTIdentifier *> idens;
  ASTExpr *right;
  TType op;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE_DECONSTRUCTION; }
};

struct ASTTuple : ASTExpr {
  ASTType *type;
  std::vector<ASTExpr *> values;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE; }
};

struct ASTParamDecl : ASTNode {
  enum {
    Normal,
    Self,
  } tag;
  union {
    struct {
      ASTType *type;
      Nullable<ASTExpr> default_value;
      InternedString name;
    } normal;
    struct {
      bool is_pointer;
    } self;
  };
  ~ASTParamDecl() {}
  ASTParamDecl() {}
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PARAM_DECL; }
};

struct ASTParamsDecl : ASTStatement {
  std::vector<ASTParamDecl *> params;
  bool has_self = false;
  bool is_varargs = false;
  void accept(VisitorBase *visitor) override;
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
  std::vector<int> generic_arguments;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;

  Scope *scope;
  ASTParamsDecl *params;
  Nullable<ASTBlock> block;
  InternedString name;
  ASTType *return_type;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_FUNCTION_DECLARATION; }
};

struct ASTArguments : ASTNode {
  std::vector<ASTExpr *> arguments;
  std::vector<int> resolved_argument_types;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ARGUMENTS; }
};

struct ASTCall : ASTExpr {
  ASTExpr *function;
  ASTArguments *arguments;
  std::vector<ASTType *> generic_arguments;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CALL; }
};
struct ASTDotExpr : ASTExpr {
  ASTExpr *base;
  InternedString member_name;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DOT_EXPR; }
};

struct ASTScopeResolution : ASTExpr {
  ASTExpr *base;
  InternedString member_name;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SCOPE_RESOLUTION; }
};

struct ASTReturn : ASTStatement {
  Nullable<ASTExpr> expression;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_RETURN; }
};
struct ASTBreak : ASTStatement {
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BREAK; }
};
struct ASTContinue : ASTStatement {
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CONTINUE; }
};

enum ValueSemantic {
  VALUE_SEMANTIC_COPY,
  VALUE_SEMANTIC_POINTER,
};

struct ASTRange : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_RANGE; }
};

struct ASTFor : ASTStatement {
  ASTExpr *iden;
  ASTExpr *range;
  ValueSemantic value_semantic;
  ASTBlock *block;

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_FOR; }
};

struct ASTElse;
struct ASTIf : ASTStatement {
  ASTExpr *condition;
  ASTBlock *block;
  Nullable<ASTElse> _else; // just an else.
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_IF; }
};

struct ASTElse : ASTStatement {
  Nullable<ASTIf> _if; // conditional else.
  Nullable<ASTBlock> block;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ELSE; }
};

struct ASTWhile : ASTStatement {
  Nullable<ASTExpr> condition;
  ASTBlock *block;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_WHILE; }
};

struct ASTSubscript : ASTExpr {
  ASTExpr *left;
  ASTExpr *subscript;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SUBSCRIPT; }
};

struct ASTImpl;

struct ASTStructDeclaration : ASTStatement {
  Scope *scope;
  InternedString name;
  bool is_fwd_decl = false;
  bool is_extern = false;
  bool is_union = false;
  std::vector<ASTDeclaration *> fields;
  std::vector<ASTStructDeclaration *> subtypes; // Right now this is only for '#anon :: struct // #anon :: union'
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTImpl *> impls;
  void accept(VisitorBase *visitor) override;
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

  Nullable<ASTType> target_type;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_INITIALIZER_LIST; }
};

struct ASTEnumDeclaration : ASTStatement {
  bool is_flags = false;
  int element_type;
  InternedString name;
  std::vector<std::pair<InternedString, ASTExpr*>> key_values;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ENUM_DECLARATION; }
};

struct ASTTaggedUnionDeclaration : ASTStatement {
  InternedString name;
  Scope *scope;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTNode *> members;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TAGGED_UNION_DECLARATION; }
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
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SWITCH; }
};

struct ASTNoop : ASTStatement {
  ASTNodeType get_node_type() const override { return AST_NODE_NOOP; }
  void accept(VisitorBase *visitor) override;
};

struct ASTAlias : ASTStatement {
  InternedString name;
  ASTType *type;
  ASTNodeType get_node_type() const override { return AST_NODE_ALIAS; }
  void accept(VisitorBase *visitor) override;
};

struct ASTInterfaceDeclaration : ASTStatement {
  InternedString name;
  Scope *scope;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTFunctionDeclaration *> methods;
  ASTNodeType get_node_type() const override { return AST_NODE_INTERFACE_DECLARATION; }
  void accept(VisitorBase *visitor) override;
};

// TODO: add interface field, once we have interfaces lol.
struct ASTImpl : ASTStatement {
  // impl 'target' or impl *interface for 'target'
  ASTType *target;
  Nullable<ASTType> interface;
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  Scope *scope;
  // methods / static methods this is implementing for the type.
  std::vector<ASTFunctionDeclaration *> methods;
  ASTNodeType get_node_type() const override { return AST_NODE_IMPL; }
  void accept(VisitorBase *visitor) override;
};

struct ASTDefer : ASTStatement {
  ASTNode *statement;
  ASTNodeType get_node_type() const override { return AST_NODE_DEFER; }
  void accept(VisitorBase *visitor) override;
};

struct ASTCast : ASTExpr {
  ASTExpr *expression;
  ASTType *target_type;
  ASTNodeType get_node_type() const override { return AST_NODE_CAST; }
  void accept(VisitorBase *visitor) override;
};

// These do not support closures!!
struct ASTLambda : ASTExpr {
  ASTParamsDecl* params;
  ASTType* return_type;
  ASTBlock* block;
  ASTNodeType get_node_type() const override { return AST_NODE_LAMBDA; }
  void accept(VisitorBase *visitor) override;
};

// Use this only for implementing the methods, so you can use the IDE to expand
// it.
#define DECLARE_VISIT_METHODS()                                                                                        \
  void visit(ASTProgram *node) override {}                                                                         \
  void visit(ASTLambda *node) override {}                                                                         \
  void visit(ASTBlock *node) override {}                                                                           \
  void visit(ASTFunctionDeclaration *node) override {}                                                             \
  void visit(ASTParamsDecl *node) override {}                                                                      \
  void visit(ASTParamDecl *node) override {}                                                                       \
  void visit(ASTDeclaration *node) override {}                                                                     \
  void visit(ASTExprStatement *node) override {}                                                                   \
  void visit(ASTBinExpr *node) override {}                                                                         \
  void visit(ASTUnaryExpr *node) override {}                                                                       \
  void visit(ASTIdentifier *node) override {}                                                                      \
  void visit(ASTLiteral *node) override {}                                                                         \
  void visit(ASTType *node) override {}                                                                            \
  void visit(ASTCall *node) override {}                                                                            \
  void visit(ASTArguments *node) override {}                                                                       \
  void visit(ASTReturn *node) override {}                                                                          \
  void visit(ASTContinue *node) override {}                                                                        \
  void visit(ASTBreak *node) override {}                                                                           \
  void visit(ASTFor *node) override {}                                                                             \
  void visit(ASTIf *node) override {}                                                                              \
  void visit(ASTElse *node) override {}                                                                            \
  void visit(ASTWhile *node) override {}                                                                           \
  void visit(ASTStructDeclaration *node) override {}                                                               \
  void visit(ASTDotExpr *node) override {}                                                                         \
  void visit(ASTSubscript *node) override {}                                                                       \
  void visit(ASTInitializerList *node) override {}                                                                 \
  void visit(ASTEnumDeclaration *node) override {}                                                                 \
  void visit(ASTRange *node) override {};                                                                          \
  void visit(ASTSwitch *node) override {};                                                                         \
  void visit(ASTTuple *node) override {};                                                                          \
  void visit(ASTAlias *node) override {};                                                                          \
  void visit(ASTTupleDeconstruction *node) override {};                                                            \
  void visit(ASTDefer *node) override {};                                                                          \
  void visit(ASTCast *node) override {};                                                                           \
  void visit(ASTTaggedUnionDeclaration *node) override {};                                                         \
  void visit(ASTInterfaceDeclaration *node) override {};

#define DECLARE_VISIT_BASE_METHODS()                                                                                   \
  void visit(ASTNoop *noop) { return; }                                                                         \
  virtual void visit(ASTScopeResolution *node) = 0;                                                                \
  virtual void visit(ASTCast *node) = 0;                                                                           \
  virtual void visit(ASTLambda *node) = 0;                                                                           \
  virtual void visit(ASTProgram *node) = 0;                                                                        \
  virtual void visit(ASTBlock *node) = 0;                                                                          \
  virtual void visit(ASTFunctionDeclaration *node) = 0;                                                            \
  virtual void visit(ASTParamsDecl *node) = 0;                                                                     \
  virtual void visit(ASTParamDecl *node) = 0;                                                                      \
  virtual void visit(ASTDeclaration *node) = 0;                                                                    \
  virtual void visit(ASTExprStatement *node) = 0;                                                                  \
  virtual void visit(ASTBinExpr *node) = 0;                                                                        \
  virtual void visit(ASTUnaryExpr *node) = 0;                                                                      \
  virtual void visit(ASTIdentifier *node) = 0;                                                                     \
  virtual void visit(ASTLiteral *node) = 0;                                                                        \
  virtual void visit(ASTType *node) = 0;                                                                           \
  virtual void visit(ASTCall *node) = 0;                                                                           \
  virtual void visit(ASTArguments *node) = 0;                                                                      \
  virtual void visit(ASTReturn *node) = 0;                                                                         \
  virtual void visit(ASTContinue *node) = 0;                                                                       \
  virtual void visit(ASTBreak *node) = 0;                                                                          \
  virtual void visit(ASTFor *node) = 0;                                                                            \
  virtual void visit(ASTIf *node) = 0;                                                                             \
  virtual void visit(ASTElse *node) = 0;                                                                           \
  virtual void visit(ASTWhile *node) = 0;                                                                          \
  virtual void visit(ASTStructDeclaration *node) = 0;                                                              \
  virtual void visit(ASTDotExpr *node) = 0;                                                                        \
  virtual void visit(ASTSubscript *node) = 0;                                                                      \
  virtual void visit(ASTInitializerList *node) = 0;                                                                \
  virtual void visit(ASTEnumDeclaration *node) = 0;                                                                \
  virtual void visit(ASTRange *node) = 0;                                                                          \
  virtual void visit(ASTSwitch *node) = 0;                                                                         \
  virtual void visit(ASTTuple *node) = 0;                                                                          \
  virtual void visit(ASTAlias *node) = 0;                                                                          \
  virtual void visit(ASTImpl *node) = 0;                                                                           \
  virtual void visit(ASTTupleDeconstruction *node) = 0;                                                            \
  virtual void visit(ASTDefer *node) = 0;                                                                          \
  virtual void visit(ASTInterfaceDeclaration *node) = 0;                                                           \
  virtual void visit(ASTTaggedUnionDeclaration *node) = 0;

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

  ASTInterfaceDeclaration *parse_interface_declaration(Token);
  ASTTupleDeconstruction *parse_multiple_asssignment();
  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTDeclaration *parse_declaration();
  ASTFunctionDeclaration *parse_function_declaration(Token);
  std::vector<GenericParameter> parse_generic_parameters();
  std::vector<ASTType *> parse_generic_arguments();
  ASTTaggedUnionDeclaration *parse_tagged_union_declaration(Token name);
  ASTParamsDecl *parse_parameters(std::vector<GenericParameter> params = {});
  void visit_struct_statements(ASTStructDeclaration *decl, const std::vector<ASTNode *> &statements);
  ASTEnumDeclaration *parse_enum_declaration(Token);
  ASTLambda *parse_lambda();
  ASTBlock *parse_block(Scope *scope = nullptr);
  ASTExpr *parse_expr(Precedence = PRECEDENCE_LOWEST);
  ASTExpr *parse_unary();
  ASTExpr *parse_postfix();
  ASTExpr *parse_interpolated_string();
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
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;
  Nullable<ASTImpl> current_impl_decl = nullptr;
  Nullable<ASTInterfaceDeclaration> current_interface_decl = nullptr;

  static std::vector<DirectiveRoutine> directive_routines;
  int64_t token_idx{};
};

template <class T> static inline T *ast_alloc(size_t n = 1) {
  auto node = new (ast_arena.allocate(sizeof(T) * n)) T();
  node->declaring_block = Parser::current_block;
  return node;
}



#define NODE_ALLOC(type, node, range, defer, parser)                                                                   \
  type *node = ast_alloc<type>();                                                                                      \
  auto range = parser->begin_node();                                                                                   \
  Defer defer([&] { parser->end_node(node, range); });

#define NODE_ALLOC_EXTRA_DEFER(type, node, range, defer, parser, deferred)                                             \
  type *node = ast_alloc<type>();                                                                                      \
  auto range = parser->begin_node();                                                                                   \
  Defer defer([&] {                                                                                                    \
    parser->end_node(node, range);                                                                                     \
    deferred;                                                                                                          \
  });
