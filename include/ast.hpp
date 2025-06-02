#pragma once

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

extern size_t lambda_unique_id;
extern jstl::Arena ast_arena;
struct VisitorBase;
extern std::unordered_map<InternedString, Scope *> import_map;
extern std::unordered_set<InternedString> include_set;

enum ASTNodeType {
  AST_NODE_PROGRAM,
  AST_NODE_BLOCK,
  AST_NODE_FUNCTION_DECLARATION,
  AST_NODE_NOOP,
  AST_NODE_ALIAS,
  AST_NODE_IMPL,
  AST_NODE_IMPORT,
  AST_NODE_MODULE,
  AST_NODE_RETURN,
  AST_NODE_CONTINUE,
  AST_NODE_BREAK,
  AST_NODE_FOR,
  AST_NODE_IF,
  AST_NODE_ELSE,
  AST_NODE_WHILE,
  AST_NODE_STRUCT_DECLARATION,
  AST_NODE_ENUM_DECLARATION,
  AST_NODE_CHOICE_DECLARATION,
  AST_NODE_TRAIT_DECLARATION,

  AST_NODE_PARAMS_DECL,
  AST_NODE_PARAM_DECL,
  AST_NODE_VARIABLE,

  AST_NODE_EXPR_STATEMENT,
  AST_NODE_BIN_EXPR,
  AST_NODE_UNARY_EXPR,

  AST_NODE_LITERAL,

  AST_NODE_PATH,
  AST_NODE_TYPE,
  AST_NODE_TUPLE,

  AST_NODE_CALL,
  AST_NODE_METHOD_CALL,
  AST_NODE_ARGUMENTS,

  AST_NODE_DOT_EXPR,
  AST_NODE_INDEX,
  AST_NODE_INITIALIZER_LIST,
  AST_NODE_SIZE_OF,
  AST_NODE_TYPE_OF,
  AST_NODE_DYN_OF,
  AST_NODE_DEFER,
  AST_NODE_CAST,
  AST_NODE_LAMBDA,
  AST_NODE_RANGE,
  AST_NODE_SWITCH,
  AST_NODE_TUPLE_DECONSTRUCTION,
  AST_NODE_WHERE,
  AST_NODE_PATTERN_MATCH,
  AST_NODE_STATEMENT_LIST, // Used just to return a bunch of statments from a single directive.s

  AST_NODE_WHERE_STATEMENT,
};

enum BlockFlags {
  BLOCK_FLAGS_FALL_THROUGH = 1 << 0,
  BLOCK_FLAGS_RETURN = 1 << 1,
  BLOCK_FLAGS_CONTINUE = 1 << 2,
  BLOCK_FLAGS_BREAK = 1 << 3,
};

struct ControlFlow {
  int flags;
  Type *type;
};

struct ASTBlock;

struct ASTNode {
  ControlFlow control_flow = {
      .flags = BLOCK_FLAGS_FALL_THROUGH,
      .type = Type::INVALID_TYPE,
  };
  Nullable<ASTBlock> declaring_block;
  SourceRange source_range{};
  Type *resolved_type = Type::INVALID_TYPE;
  bool is_emitted = false;
  virtual ~ASTNode() = default;
  virtual void accept(VisitorBase *visitor) = 0;
  virtual ASTNodeType get_node_type() const = 0;

  inline bool is_expr() {
    switch (get_node_type()) {
      case AST_NODE_BIN_EXPR:
      case AST_NODE_UNARY_EXPR:
      case AST_NODE_LITERAL:
      case AST_NODE_TYPE:
      case AST_NODE_TUPLE:
      case AST_NODE_CALL:
      case AST_NODE_ARGUMENTS:
      case AST_NODE_DOT_EXPR:
      case AST_NODE_INDEX:
      case AST_NODE_INITIALIZER_LIST:
      case AST_NODE_CAST:
      case AST_NODE_RANGE:
      case AST_NODE_SWITCH:
      case AST_NODE_PATH:
        return true;
      default:
        return false;
    }
  }
};

enum AttributeTag {
  //  / @[inline]                           : inlined function
  ATTRIBUTE_INLINE,
  // @[entry]                               : program entry point
  ATTRIBUTE_ENTRY,
  // @[impl(Clone, Format, ...)]            : auto implement trait on type declaration
  ATTRIBUTE_IMPL,
  // @[const]                               : make a function/struct compile-time-compatible
  ATTRIBUTE_CONST,
  // @[pub]                                 : make a symbol publicly visible to all importing modules
  ATTRIBUTE_PUB,
  // @[no_mangle]                           : don't mangle a symbols name ever.
  ATTRIBUTE_NO_MANGLE,
  // @[no_return],                          : don't throw errors when control flow analyzer isnt satisfied. i.e, this function will never return control flow back to the caller, when called.
  ATTRIBUTE_NO_RETURN,
};

struct Attribute {
  AttributeTag tag;
  std::vector<ASTExpr *> arguments;
};

struct ASTStatement : ASTNode {
  std::vector<Attribute> attributes;
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTStatementList : ASTStatement {
  std::vector<ASTNode *> statements;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_STATEMENT_LIST; }
};

inline static std::string block_flags_to_string(int flags) {
  std::string result;
#define X(flag)                                                                                                        \
  if (flags & flag)                                                                                                    \
    result += #flag " ";
  X(BLOCK_FLAGS_FALL_THROUGH)
  X(BLOCK_FLAGS_RETURN)
  X(BLOCK_FLAGS_CONTINUE)
  X(BLOCK_FLAGS_BREAK)
  return result;
#undef X
}

struct ASTModule : ASTStatement {
  InternedString module_name;
  std::vector<ASTStatement *> statements;
  Scope *scope;
  ASTNodeType get_node_type() const override { return AST_NODE_MODULE; }
  void accept(VisitorBase *visitor) override;
};

struct ASTImport : ASTModule {
  // We need to have a more complex way to represent these 'symbols', which could be any of these,
  // where we could be ::{}, ::*, or ::identifier.
  /*
    import fs::{
      file::*,
      directory::prober::*,
      directory::Directory::open,
    };

    import fs::file::*;

    import fs::*;
  */

  std::vector<InternedString> symbols;

  enum {
    IMPORT_NORMAL,
    IMPORT_ALL,
    IMPORT_NAMED,
  } tag = IMPORT_NAMED;

  ASTNodeType get_node_type() const override { return AST_NODE_IMPORT; }
  void accept(VisitorBase *visitor) override;
};

struct ASTBlock : ASTStatement {
  ASTNode *parent;
  int flags = BLOCK_FLAGS_FALL_THROUGH;

  // used for tuple destructures.
  size_t temp_iden_idx = 0;

  // of course used for defer.
  bool has_defer = false;
  int defer_count = 0;

  Scope *scope;
  std::vector<ASTNode *> statements;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BLOCK; }
};

struct ASTProgram : ASTNode {
  size_t end_of_bootstrap_index = -1;
  std::vector<ASTStatement *> statements;
  Scope *scope;
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

struct ASTPath : ASTExpr {
  struct Segment {
    InternedString identifier{};
    /*
      we use optional here to avoid unneccesary initialization for basic stuff
      like identifiers.
    */
    std::vector<ASTExpr *> generic_arguments;

    Type *resolved_type = Type::INVALID_TYPE;

    inline std::vector<Type *> get_resolved_generics() {
      std::vector<Type *> generics;
      for (auto arg : generic_arguments) {
        generics.push_back(arg->resolved_type);
      }
      return generics;
    }
  };

  std::vector<ASTExpr *> *get_last_segments_generics() { return &segments.back().generic_arguments; }

  /*
    Path parts are typically identifiers, and they may contain their own generic arguments.

    "collections::List!<s32>" -> {
      parts: [
        { "collections" },
        { "List", [ s32 ] }
      ]
    }
  */
  std::vector<Segment> segments{};

  ASTNodeType get_node_type() const override { return AST_NODE_PATH; }

  void accept(VisitorBase *visitor) override;
  inline size_t length() const { return segments.size(); }

  inline void push_segment(InternedString identifier) { segments.emplace_back(identifier); }

  inline void push_segment(InternedString identifier, std::vector<ASTExpr *> generic_arguments) {
    segments.emplace_back(identifier, generic_arguments);
  }
};

struct ASTType : ASTExpr {
  enum Kind {
    NORMAL,
    TUPLE,
    FUNCTION,
    SELF,
  } kind = NORMAL;

  union {
    struct {
      ASTPath *path;
      bool is_dyn = false;
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
    source_range = other.source_range;
    extensions = other.extensions;
    kind = other.kind;
    switch (kind) {
      case NORMAL:
      case SELF:
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

  // [100], *, etc.
  std::vector<ASTTypeExtension> extensions;

  ASTNodeType get_node_type() const override { return AST_NODE_TYPE; }
  static ASTType *get_void();
  void accept(VisitorBase *visitor) override;
};

struct ASTExprStatement : ASTStatement {
  ASTExpr *expression;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_EXPR_STATEMENT; }
};

struct ASTVariable : ASTStatement {
  InternedString name;
  InternedString bitsize;

  // This isn't nullable, even though it can be null for part of compilation.
  // That's because if it ever was null, when it's done typing it will have been created.
  // It creates too much friction later on down the line if it's not.
  ASTType *type = nullptr;
  Nullable<ASTExpr> value;

  Mutability mutability = CONST;

  bool is_local = false;
  bool is_extern = false;
  bool is_constexpr = false;
  bool is_static = false;
  bool is_bitfield = false;

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_VARIABLE; }
};

struct ASTBinExpr : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  TType op;
  bool is_operator_overload = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BIN_EXPR; }
};

struct ASTUnaryExpr : ASTExpr {
  bool is_operator_overload = false;
  Mutability mutability = CONST;
  ASTExpr *operand;
  TType op;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_UNARY_EXPR; }
};

struct ASTLiteral : ASTExpr {
  enum Tag {
    Integer,
    Float,
    String,
    Char,
    Bool,
    Null,
  } tag;
  bool is_c_string = false;
  InternedString value;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_LITERAL; }
};

enum ValueSemantic {
  VALUE_SEMANTIC_COPY,
  VALUE_SEMANTIC_POINTER,
};

struct DestructureElement {
  ValueSemantic semantic;
  InternedString identifier;
  Mutability mutability;
};

struct ASTDestructure : ASTStatement {
  std::vector<DestructureElement> elements;
  ASTExpr *right;
  TType op;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE_DECONSTRUCTION; }
};

struct ASTTuple : ASTExpr {
  std::vector<ASTExpr *> values;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TUPLE; }
};

struct ASTParamDecl : ASTNode {
  enum {
    Normal,
    Self,
  } tag;
  Mutability mutability;
  union {
    struct {
      ASTType *type;
      InternedString name;
      Nullable<ASTExpr> default_value;
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
  bool self_is_pointer = false;

  bool is_varargs = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PARAMS_DECL; }
};

struct ASTGenericParameter {
  InternedString identifier;
  ASTType *default_value = nullptr;
};

struct ASTDeclaration;

struct GenericInstance {
  std::vector<Type *> arguments;
  ASTDeclaration *declaration;
};

struct ASTImpl;

struct ASTDeclaration : ASTStatement {
  std::vector<ASTGenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTImpl *> impls;
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTWhere;
struct ASTLambda;

struct ASTFunctionDeclaration : ASTDeclaration {
  unsigned int flags;
  bool has_defer = false;
  bool is_declared = false;
  Type *declaring_type = Type::INVALID_TYPE;
  Nullable<ASTWhere> where_clause;
  std::vector<Type *> generic_arguments;
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
  std::vector<Type *> resolved_argument_types;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ARGUMENTS; }
};

struct ASTCall : ASTExpr {
  ASTExpr *callee;
  ASTArguments *arguments;

  bool has_generics() {
    if (auto path = dynamic_cast<ASTPath *>(callee)) {
      return path->get_last_segments_generics() != nullptr;
    }
    return false;
  }

  Nullable<std::vector<ASTExpr *>> get_generic_arguments() {
    if (auto path = dynamic_cast<ASTPath *>(callee)) {
      return path->get_last_segments_generics();
    }
    return nullptr;
  }

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CALL; }
};

struct ASTDotExpr : ASTExpr {
  ASTExpr *base;
  /* Generic arguments are stored in the segment. */
  ASTPath::Segment member;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DOT_EXPR; }
};

struct ASTMethodCall : ASTExpr {
  /*
    сперма хранится в яйцах.
    (aka the generic arguments are stored in the base)
  */
  ASTDotExpr *callee;
  ASTArguments *arguments;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_METHOD_CALL; }
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

struct ASTRange : ASTExpr {
  ASTExpr *left;
  ASTExpr *right;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_RANGE; }
};

struct ASTFor : ASTStatement {
  enum {
    // implicitly pulled an iter() off a type that implements Iterable![T]
    ITERABLE,
    // got an Iter![T] object directly
    ITERATOR,
  } iteration_kind;

  // This is the type of the container/sequence, whatever implements .iter() / .enumerator()
  Type *iterable_type = Type::INVALID_TYPE;
  // This is either the type of that implements Enumerator![T], or is Iter![T];
  Type *iterator_type = Type::INVALID_TYPE;
  // This is the 'i' part of 'for i in...', the type of the whatchamacallit.
  Type *identifier_type = Type::INVALID_TYPE;
  union Left {
    Left() {}
    ~Left() {}
    InternedString identifier;
    std::vector<DestructureElement> destructure;
  } left;

  enum {
    IDENTIFIER,
    DESTRUCTURE,
  } left_tag = IDENTIFIER;

  // this is the '0..100' or any thing on the right hand side of the 'in'
  // `for i in 0..100`
  //           ^^^^^^^ <- this is the `range`
  ASTExpr *right;
  ASTBlock *block;

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_FOR; }
  ASTFor() {}
  ~ASTFor() {}
  ASTFor(const ASTFor &other) {
    source_range = other.source_range;
    iteration_kind = other.iteration_kind;
    iterable_type = other.iterable_type;
    iterator_type = other.iterator_type;
    identifier_type = other.identifier_type;
    left_tag = other.left_tag;
    right = other.right;
    block = other.block;
    switch (left_tag) {
      case IDENTIFIER:
        left.identifier = other.left.identifier;
        break;
      case DESTRUCTURE:
        left.destructure = other.left.destructure;
        break;
    }
  }
};

struct ASTElse;
struct ASTIf : ASTExpr {
  bool is_expression = false;
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

struct ASTIndex : ASTExpr {
  bool is_operator_overload = false;
  ASTExpr *base;
  ASTExpr *index;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_INDEX; }
};

struct ASTImpl;
struct ASTWhere;

struct ASTStructMember {
  bool is_bitfield = false;
  InternedString bitsize;
  InternedString name;
  ASTType *type;
  Nullable<ASTExpr> default_value;
};

struct ASTAlias;

struct ASTStructDeclaration : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  InternedString name;
  Scope *scope;

  bool is_fwd_decl = false;
  bool is_union = false;
  bool is_anonymous = false;

  std::vector<ASTStructDeclaration *> subtypes; // Right now this is only for '#anon :: struct // #anon :: union'
  std::vector<ASTStructMember> members;

  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_STRUCT_DECLARATION; }
};

struct ASTSize_Of : ASTExpr {
  ASTType *target_type;
  ASTNodeType get_node_type() const override { return AST_NODE_SIZE_OF; }
  void accept(VisitorBase *visitor) override;
};

struct ASTDyn_Of : ASTExpr {
  ASTType *trait_type;
  ASTExpr *object;
  ASTNodeType get_node_type() const override { return AST_NODE_DYN_OF; }
  void accept(VisitorBase *visitor) override;
};

struct ASTType_Of : ASTExpr {
  ASTExpr *target;
  ASTNodeType get_node_type() const override { return AST_NODE_TYPE_OF; }
  void accept(VisitorBase *visitor) override;
};

struct ASTTraitDeclaration : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  InternedString name;
  Scope *scope;
  std::vector<ASTFunctionDeclaration *> methods;
  ASTNodeType get_node_type() const override { return AST_NODE_TRAIT_DECLARATION; }
  void accept(VisitorBase *visitor) override;
};

struct ASTEnumDeclaration : ASTStatement {
  bool is_flags = false;
  Type *element_type;
  InternedString name;
  std::vector<std::pair<InternedString, ASTExpr *>> key_values;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ENUM_DECLARATION; }
};

struct ASTChoiceVariant {
  enum Kind {
    NORMAL,
    TUPLE,
    STRUCT,
  } kind;
  ASTType *tuple;
  std::vector<ASTVariable *> struct_declarations;
  InternedString name;
};

struct ASTChoiceDeclaration : ASTDeclaration {
  InternedString name;
  Nullable<ASTWhere> where_clause;
  Scope *scope;
  std::vector<ASTChoiceVariant> variants;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CHOICE_DECLARATION; }
};

struct ASTInitializerList : ASTExpr {
  ASTInitializerList() {}
  ~ASTInitializerList() {}
  ASTInitializerList(const ASTInitializerList &other) {
    source_range = other.source_range;
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

struct SwitchCase {
  ASTExpr *expression;
  ASTBlock *block;
};

struct ASTSwitch : ASTExpr {
  bool is_statement = false;
  Type *return_type = void_type();
  bool is_pattern_match = false;
  ASTExpr *target;
  Nullable<ASTBlock> default_case;
  std::vector<SwitchCase> cases;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SWITCH; }
};

struct ASTNoop : ASTStatement {
  ASTNodeType get_node_type() const override { return AST_NODE_NOOP; }
  void accept(VisitorBase *visitor) override;
};

struct ASTAlias : ASTStatement { // TODO: Implement where clauses for generic aliases?
  InternedString name;
  ASTNode *source_node;
  std::vector<ASTExpr *> generic_arguments;
  std::vector<ASTGenericParameter> generic_parameters;
  ASTNodeType get_node_type() const override { return AST_NODE_ALIAS; }
  void accept(VisitorBase *visitor) override;
};

struct ASTImpl : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  // impl 'target' or impl *trait for 'target'
  ASTType *target;
  Nullable<ASTType> trait;
  Scope *scope;

  // methods / static methods this is implementing for the type.
  std::vector<ASTFunctionDeclaration *> methods;

  // aliases.
  std::vector<ASTAlias *> aliases;

  // constants.
  std::vector<ASTVariable *> constants;

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
  InternedString unique_identifier;
  ASTParamsDecl *params;
  ASTType *return_type;
  ASTBlock *block;
  ASTNodeType get_node_type() const override { return AST_NODE_LAMBDA; }
  void accept(VisitorBase *visitor) override;
};

// The first value is the target type.
// The second value is the condition/constraint being applied.
using Constraint = std::pair<ASTExpr *, ASTExpr *>;
struct ASTWhere : ASTExpr {
  // instead of just having one, we can have several.
  /*
    such as
    * fn!<T, T1>() where T: s64, T1: AsSlice {}
  */
  std::vector<Constraint> constraints;
  ASTNodeType get_node_type() const override { return AST_NODE_WHERE; }
  void accept(VisitorBase *visitor) override;
};

enum PatternMatchPointerSemantic { PTRN_MTCH_PTR_NONE, PTRN_MTCH_PTR_MUT, PTR_MTCH_PTR_CONST };

struct TuplePattern {
  struct Part {
    Mutability mutability = CONST;
    InternedString var_name;
    PatternMatchPointerSemantic semantic = PTRN_MTCH_PTR_NONE;
    Type *resolved_type;
  };
  std::vector<Part> parts;
};

struct StructPattern {
  struct Part {
    InternedString field_name;
    InternedString var_name;
    Mutability mutability = CONST;
    PatternMatchPointerSemantic semantic = PTRN_MTCH_PTR_NONE;
    Type *resolved_type;
  };
  std::vector<Part> parts;
};

/*
  * note: the new variables created by the pattern match's destructure
  * are denoted by $name

    tuple style. never has names, only new var names.
  if x is Choice::Variant($v1, $v2) { ... }

    can be mut
  if x is Choice::Variant(mut $v1, mut $v2) { ... }

    struct style, for now, has to have the property names and the new var name.
  if x is Choice::Variant{ v: $v, y: $y } { ... }

    can be mut too.
  if x is Choice::Variant{ v: mut $v, y: mut $y } { ... }
*/
struct ASTPatternMatch : ASTExpr {
  /* This needs a scope to store possibly destructured variables. */
  Scope *scope;
  ~ASTPatternMatch() {}
  ASTPatternMatch() {}
  ASTPatternMatch(const ASTPatternMatch &other) {
    source_range = other.source_range;
    object = other.object;
    target_type_path = other.target_type_path;
    pattern_tag = other.pattern_tag;
    switch (pattern_tag) {
      case NONE:
        break;
      case STRUCT:
        struct_pattern = other.struct_pattern;
        break;
      case TUPLE:
        tuple_pattern = other.tuple_pattern;
        break;
    }
  }
  /*
    the left hand side of the 'is', e.g { if x is ... }
                                             ^<- object.
  */
  ASTExpr *object;
  /*
    the type thats being matched
    e.g
      if x is Choice::Variant {... }
              ^^^^^^^^^^^^^^^<- the target type.
  */
  ASTPath *target_type_path;

  enum {
    /*
      this (NONE) is for when there's no fields being destructured,
      such as:
        if x is Choice::None {

        }
    */
    NONE,
    STRUCT,
    TUPLE,
  } pattern_tag;

  union {
    StructPattern struct_pattern;
    TuplePattern tuple_pattern;
  };

  ASTNodeType get_node_type() const override { return AST_NODE_PATTERN_MATCH; }
  void accept(VisitorBase *visitor) override;
};

struct WhereBranch;
struct ASTWhereStatement: ASTStatement {
  /*
    Allow for negative checks for early returns.
    where! $Type: $Trait {
      return;
    }
  */
  bool negative = false;

  // The actual clause.
  ASTWhere *where;

  // the actual block to be executed.
  ASTBlock *block;

  // for caching the success/failure of the where.
  bool should_compile = false;

  // if this has any else clauses, this will be occupied.
  Nullable<WhereBranch> branch;

  ASTNodeType get_node_type() const override {
    return AST_NODE_WHERE_STATEMENT;
  }

  virtual void accept(VisitorBase *visitor) override;
};

struct WhereBranch {
  // If this has a else where, this will be the occupied field.
  Nullable<ASTWhereStatement> where_stmt;
  // if this is just an else {} this will be the occupied field.
  Nullable<ASTBlock> block;
};

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

static inline Precedence get_operator_precedence(Token token);

struct Typer;

struct Parser {
  ASTIf *parse_if();
  ASTProgram *parse_program();
  ASTStatement *parse_statement();
  ASTArguments *parse_arguments();
  ASTPath::Segment parse_path_segment();

  ASTTraitDeclaration *parse_trait_declaration();
  ASTStructDeclaration *parse_struct_body(InternedString name, SourceRange range, ASTStructDeclaration *node);
  ASTStructDeclaration *parse_struct_declaration();
  ASTFunctionDeclaration *parse_function_declaration();
  ASTChoiceDeclaration *parse_choice_declaration();
  ASTEnumDeclaration *parse_enum_declaration();

  ASTDestructure *parse_destructure();
  ASTVariable *parse_variable();
  std::vector<ASTGenericParameter> parse_generic_parameters();
  std::vector<ASTExpr *> parse_generic_arguments();

  ASTParamsDecl *parse_parameters();

  ASTLambda *parse_lambda();
  ASTBlock *parse_block(Scope *scope = nullptr);
  ASTExpr *parse_expr(Precedence = PRECEDENCE_LOWEST);
  ASTExpr *parse_unary();
  ASTExpr *parse_postfix();
  ASTPath *parse_path();
  ASTExpr *parse_primary();
  ASTImpl *parse_impl();

  ASTWhere *parse_where_clause();
  ASTWhereStatement *parse_where_statement();

  ASTType *parse_type();
  ASTDefer *parse_defer();

  void parse_pointer_extensions(ASTType *type);
  std::vector<ASTType *> parse_parameter_types();
  void append_type_extensions(ASTType *&type);
  ASTType *parse_function_type();

  Nullable<ASTNode> process_directive(DirectiveKind kind, const InternedString &identifier);
  Nullable<ASTExpr> try_parse_directive_expr();
  bool import(InternedString name, Scope **scope);
  inline std::deque<Token> &lookahead_buf() { return states.back().lookahead_buffer; }

  Token eat();
  Token expect(TType type);
  Token peek() const;
  void fill_buffer_if_needed();

  SourceRange begin_node();
  void end_node(ASTNode *node, SourceRange &range);
  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == TType::Semi; }

  Parser(const std::string &filename, Context &context);
  ~Parser();

  Typer *typer;
  Context &ctx;
  Lexer lexer{};

  std::vector<Lexer::State> states;
  Nullable<ASTStructDeclaration> current_struct_decl = nullptr;
  Nullable<ASTFunctionDeclaration> current_func_decl = nullptr;
  Nullable<ASTImpl> current_impl_decl = nullptr;
  Nullable<ASTTraitDeclaration> current_trait_decl = nullptr;

  static Nullable<ASTBlock> current_block;
  static std::vector<DirectiveRoutine> directive_routines;
};

template <class T> static inline T *ast_alloc(size_t n = 1) {
  auto node = new (ast_arena.allocate(sizeof(T) * n)) T();
  node->declaring_block = Parser::current_block;
  return node;
}

ASTDeclaration *find_generic_instance(std::vector<GenericInstance> instantiations, const std::vector<Type *> &gen_args);

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
