#pragma once

#include <cstdint>
#include <cstdio>
#include <deque>
#include <functional>
#include <optional>
#include <vector>

#include "arena.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

extern size_t LAMBDA_UNIQUE_ID;
extern jstl::Arena ast_arena;
struct VisitorBase;
extern std::unordered_map<InternedString, Scope *> import_map;
extern std::unordered_set<InternedString> include_set;

enum ASTNodeType {
  AST_NODE_PROGRAM,
  AST_NODE_BLOCK,
  AST_NODE_FUNCTION_DECLARATION,
  AST_NODE_PARAMS_DECL,
  AST_NODE_PARAM_DECL,
  AST_NODE_VARIABLE,
  AST_NODE_EXPR_STATEMENT,
  AST_NODE_BIN_EXPR,
  AST_NODE_UNARY_EXPR,
  AST_NODE_PATH,
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
  AST_NODE_IMPORT,
  AST_NODE_MODULE,
  AST_NODE_INTERFACE_DECLARATION,
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

struct ASTBlock;

struct ASTNode {
  ControlFlow control_flow = {
      .flags = BLOCK_FLAGS_FALL_THROUGH,
      .type = -1,
  };
  Nullable<ASTBlock> declaring_block;
  SourceRange source_range{};
  int resolved_type = Type::INVALID_TYPE_ID;
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

enum AttributeTag {
  //  / @[inline]                           : inlined function
  ATTRIBUTE_INLINE,
  // @[foreign(external_fn_name)]           : extern function import
  ATTRIBUTE_FOREIGN,
  // @[entry]                               : program entry point
  ATTRIBUTE_ENTRY,
  // @[impl(Clone, Format, ...)]            : auto implement interface on type declaration
  ATTRIBUTE_IMPL,
  // @[const]                               : make a function/struct compile-time-compatible
  ATTRIBUTE_CONST,
  // @[pub]                                 : make a symbol publicly visible to all importing modules
  ATTRIBUTE_PUB,
  // @[export]                              : make a symbol visible to any translation unit, even assembly / C code
  ATTRIBUTE_EXPORT,
  // @[no_mangle]                           : don't mangle a symbols name ever.
  ATTRIBUTE_NO_MANGLE,
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

struct ImplicitConversion {
  int to = Type::INVALID_TYPE_ID;
  int from = Type::INVALID_TYPE_ID;
  enum {
    /*
      this is for things like:
        `n : any = 100;`

      where we need to construct & reflect to create the instance.
    */
    TO_ANY,
    /*
      this is when we do like:

        `n : Option = Option::Some(x);`

      we are convertion Option::Some to Option technically.
    */
    VARIANT_TO_TAGGED_UNION,
  } kind;
};

struct ASTExpr : ASTNode {
  virtual ASTNodeType get_node_type() const = 0;
  std::optional<ImplicitConversion> conversion = std::nullopt;
};

struct ASTTypeExtension {
  TypeExtEnum type;
  ASTExpr *expression;
};

struct ASTPath: ASTExpr {
  struct Part {
    InternedString value {};
    /* 
      we use optional here to avoid unneccesary initialization for basic stuff
      like identifiers.
    */
    std::optional<std::vector<ASTExpr*>> generic_arguments = std::nullopt;

    inline std::vector<int> get_resolved_generics() {
      std::vector<int> generics;
      for (auto arg: *generic_arguments) {
        generics.push_back(arg->resolved_type);
      }
      return generics;
    }
    
  };

  /*
    Path parts are typically identifiers, and they may contain their own generic arguments.

    "collections::List!<s32>" -> {
      parts: [
        { "collections" },
        { "List", [ s32 ] }
      ]
    }
  */
  std::vector<Part> parts;
  ASTNodeType get_node_type() const {
    return AST_NODE_PATH;
  }

  inline size_t length() const {
    return parts.size();
  }

  inline void push_part(InternedString identifier) {
    parts.emplace_back(identifier);
  }

  inline void push_part(InternedString identifier, std::vector<ASTExpr*> generic_arguments) {
    parts.emplace_back(identifier, std::make_optional(generic_arguments));
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
      ASTExpr *base;
      std::vector<ASTExpr *> generic_arguments;
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
  Token op;
  bool is_operator_overload = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_BIN_EXPR; }
};

struct ASTUnaryExpr : ASTExpr {
  bool is_operator_overload = false;
  Mutability mutability = CONST;
  ASTExpr *operand;
  Token op;
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

struct Destructure {
  ValueSemantic semantic;
  InternedString identifier;
  Mutability mutability;
};

struct ASTTupleDeconstruction : ASTStatement {
  std::vector<Destructure> elements;
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
    Себя,
  } tag;
  Mutability mutability;
  union {
    struct {
      ASTType *type;
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
  bool self_is_pointer = false;

  bool is_varargs = false;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_PARAMS_DECL; }
};

struct ASTDeclaration;

struct GenericInstance {
  std::vector<int> arguments;
  ASTDeclaration *declaration;
};

struct ASTImpl;

struct ASTDeclaration : ASTStatement {
  std::vector<GenericParameter> generic_parameters;
  std::vector<GenericInstance> generic_instantiations;
  std::vector<ASTImpl *> impls;
  virtual ASTNodeType get_node_type() const = 0;
};

struct ASTWhere;
struct ASTLambda;

struct ASTFunctionDeclaration : ASTDeclaration {
  size_t flags = 0;
  bool has_defer = false;
  bool is_declared = false;
  int declaring_type = Type::INVALID_TYPE_ID;
  Nullable<ASTWhere> where_clause;
  std::vector<int> generic_arguments;
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
  std::vector<ASTExpr *> generic_arguments;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_CALL; }
};

struct ASTDotExpr : ASTExpr {
  ASTExpr *base;
  InternedString member_name;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_DOT_EXPR; }
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
  int iterable_type = Type::INVALID_TYPE_ID;
  // This is either the type of that implements Enumerator![T], or is Iter![T];
  int iterator_type = Type::INVALID_TYPE_ID;
  // This is the 'i' part of 'for i in...', the type of the whatchamacallit.
  int identifier_type = Type::INVALID_TYPE_ID;
  union Left {
    Left() {}
    ~Left() {}
    InternedString identifier;
    std::vector<Destructure> destructure;
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
  bool is_operator_overload = false;
  ASTExpr *left;
  ASTExpr *subscript;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_SUBSCRIPT; }
};

struct ASTImpl;
struct ASTWhere;

struct ASTStructMember {
  bool is_bitfield = false;
  InternedString bitsize;
  InternedString name;
  ASTType *type;
};

struct ASTAlias;

struct ASTStructDeclaration : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  Scope *scope;
  InternedString name;
  bool is_fwd_decl = false;
  bool is_extern = false;
  bool is_union = false;

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
  ASTType *interface_type;
  ASTExpr *object;
  ASTNodeType get_node_type() const override { return AST_NODE_DYN_OF; }
  void accept(VisitorBase *visitor) override;
};

struct ASTType_Of : ASTExpr {
  ASTExpr *target;
  ASTNodeType get_node_type() const override { return AST_NODE_TYPE_OF; }
  void accept(VisitorBase *visitor) override;
};

struct ASTInterfaceDeclaration : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  InternedString name;
  Scope *scope;
  std::vector<ASTFunctionDeclaration *> methods;
  ASTNodeType get_node_type() const override { return AST_NODE_INTERFACE_DECLARATION; }
  void accept(VisitorBase *visitor) override;
};

struct ASTEnumDeclaration : ASTStatement {
  bool is_flags = false;
  int element_type;
  InternedString name;
  std::vector<std::pair<InternedString, ASTExpr *>> key_values;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_ENUM_DECLARATION; }
};

struct ASTTaggedUnionVariant {
  enum Kind {
    NORMAL,
    TUPLE,
    STRUCT,
  } kind;
  ASTType *tuple;
  std::vector<ASTVariable *> struct_declarations;
  InternedString name;
};

struct ASTTaggedUnionDeclaration : ASTDeclaration {
  InternedString name;
  Nullable<ASTWhere> where_clause;
  Scope *scope;
  std::vector<ASTTaggedUnionVariant> variants;
  void accept(VisitorBase *visitor) override;
  ASTNodeType get_node_type() const override { return AST_NODE_TAGGED_UNION_DECLARATION; }
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
  int return_type = void_type();
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
  std::vector<GenericParameter> generic_parameters;
  ASTNodeType get_node_type() const override { return AST_NODE_ALIAS; }
  void accept(VisitorBase *visitor) override;
};

// TODO: add interface field, once we have interfaces lol.
struct ASTImpl : ASTDeclaration {
  Nullable<ASTWhere> where_clause;
  // impl 'target' or impl *interface for 'target'
  ASTType *target;
  Nullable<ASTType> interface;
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

using Constraint = std::pair<ASTType *, ASTExpr *>;
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

struct TuplePattern {
  struct Part {
    Mutability mutability = CONST;
    InternedString var_name;
  };
  std::vector<Part> parts;
};

struct StructPattern {
  struct Part {
    InternedString field_name;
    InternedString var_name;
    Mutability mutability = CONST;
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
  ~ASTPatternMatch() {}
  ASTPatternMatch() {}
  ASTPatternMatch(const ASTPatternMatch &other) {
    source_range = other.source_range;
    object = other.object;
    target_type = other.target_type;
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
  ASTType *target_type;

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
  ASTProgram *parse_program();
  ASTStatement *parse_statement();
  ASTArguments *parse_arguments();

  ASTInterfaceDeclaration *parse_interface_declaration(Token);
  ASTTupleDeconstruction *parse_multiple_asssignment();
  ASTStructDeclaration *parse_struct_declaration(Token);
  ASTVariable *parse_variable();
  ASTFunctionDeclaration *parse_function_declaration(Token);
  std::vector<GenericParameter> parse_generic_parameters();
  std::vector<ASTExpr *> parse_generic_arguments();
  ASTTaggedUnionDeclaration *parse_tagged_union_declaration(Token name);
  ASTParamsDecl *parse_parameters(std::vector<GenericParameter> params = {});
  ASTEnumDeclaration *parse_enum_declaration(Token);
  ASTLambda *parse_lambda();
  ASTBlock *parse_block(Scope *scope = nullptr);
  ASTExpr *parse_expr(Precedence = PRECEDENCE_LOWEST);
  ASTExpr *parse_unary();
  ASTExpr *parse_postfix();
  ASTPath *parse_path();
  ASTExpr *parse_primary();
  ASTCall *parse_call(ASTExpr *function);
  ASTImpl *parse_impl();
  ASTWhere *parse_where_clause();

  // ASTType* parsing routines
  ASTType *parse_type();

  void parse_pointer_extensions(ASTType *type);

  std::vector<ASTType *> parse_parameter_types();
  void append_type_extensions(ASTType *&type);

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

  // returns true if successful, false if already included
  bool import(InternedString name, Scope **scope);

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

ASTDeclaration *find_generic_instance(std::vector<GenericInstance> instantiations, const std::vector<int> &gen_args);

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
