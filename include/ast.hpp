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

enum AST_Literal_Tag {
  LITERAL_INTEGER,
  LITERAL_FLOAT,
  LITERAL_STRING,
  LITERAL_CHAR,
  LITERAL_BOOL,
  LITERAL_NULL,
};

extern size_t LAMBDA_UNIQUE_ID;

extern jstl::Arena ast_arena;

struct VisitorBase;

// used to prevent double includes.
extern std::unordered_set<InternedString> import_set;

#include "core.hpp"
#include <string>
#include "type.hpp"

enum AST_Node_Type {
  AST_PROGRAM,
  AST_BLOCK,
  AST_FUNCTION,
  AST_DECLARATION,
  AST_EXPR_STATEMENT,
  AST_BIN_EXPR,
  AST_UNARY_EXPR,
  AST_IDENTIFIER,
  AST_LITERAL,
  AST_TYPE,
  AST_TUPLE,
  AST_CALL,
  AST_RETURN,
  AST_CONTINUE,
  AST_BREAK,
  AST_FOR,
  AST_IF,
  AST_ELSE,
  AST_WHILE,
  AST_STRUCT,
  AST_DOT_EXPR,
  AST_SCOPE_RESOLUTION,
  AST_SUBSCRIPT,
  AST_INITIALIZER,
  AST_ENUM,
  AST_NOOP,
  AST_ALIAS,
  AST_IMPL,
  AST_INTERFACE,
  AST_SIZE_OF,
  AST_DEFER,
  AST_CAST,
  AST_LAMBDA,
  AST_RANGE,
  AST_SWITCH,
  AST_TUPLE_DECONSTRUCTION,
  AST_WHERE,
  AST_STATEMENT_LIST, // Used just to return a bunch of statments from a single directive.s
};
enum BlockFlags {
  BLOCK_FLAGS_FALL_THROUGH = 1 << 0,
  BLOCK_FLAGS_RETURN = 1 << 1,
  BLOCK_FLAGS_CONTINUE = 1 << 2,
  BLOCK_FLAGS_BREAK = 1 << 3,
};
struct Control_Flow {
  int flags;
  int type;
};

#define BLOCK_FLAG_TO_STRING(flag)                                                                                     \
  if (flags & flag)                                                                                                    \
    result += #flag " ";

constexpr const inline static auto block_flags_to_string(const auto flags) {
  static std::string result;
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_FALL_THROUGH)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_RETURN)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_CONTINUE)
  BLOCK_FLAG_TO_STRING(BLOCK_FLAGS_BREAK)
  return result;
}

struct AST_Type_Extension {
  TypeExtEnum type;
  AST *expression;
};

struct AST;
enum AST_Parameter_Tag {
  AST_PARAM_NORMAL,
  AST_PARAM_SELF,
};
struct AST_Parameter_Declaration {
  AST_Parameter_Tag tag;
  union {
    struct {
      AST *type;
      InternedString name;
    } normal;
    struct {
      bool is_pointer : 1;
    } self;
  };
};

struct GenericInstance {
  std::vector<int> arguments;
  AST *node;
};

// for the for loops,
// copy is `for i in ...`
// pointer is `for *v in ...`
enum ValueSemantic {
  VALUE_SEMANTIC_COPY,
  VALUE_SEMANTIC_POINTER,
};

struct ASTStructMember {
  bool is_bitfield : 1 = false;
  InternedString bitsize;
  InternedString name;
  AST *type;
};

struct SwitchCase {
  AST *expression;
  AST *block;
};

enum AST_Type_Kind {
  AST_TYPE_NORMAL,
  AST_TYPE_REFLECTION,
  AST_TYPE_TUPLE,
  AST_TYPE_FUNCTION,
  AST_TYPE_SELF,
};

enum AST_Initializer_Tag {
  INITIALIZER_EMPTY,
  INITIALIZER_NAMED,
  INITIALIZER_COLLECTION,
};

struct AST {
  AST(AST_Node_Type node_type) : node_type(node_type) {}
  ~AST() {}

  const AST_Node_Type node_type;

  AST *parent;
  SymbolTable symbol_table;

  Control_Flow control_flow = {
      .flags = BLOCK_FLAGS_FALL_THROUGH,
      .type = Type::INVALID_TYPE_ID,
  };

  Nullable<AST> declaring_block;

  Source_Range source_range{};
  int resolved_type = Type::INVALID_TYPE_ID;
  bool is_emitted = false;

  inline bool is_expr() const {
    switch (node_type) {
      case AST_BIN_EXPR:
      case AST_UNARY_EXPR:
      case AST_IDENTIFIER:
      case AST_LITERAL:
      case AST_TYPE:
      case AST_TUPLE:
      case AST_CALL:
      case AST_DOT_EXPR:
      case AST_SCOPE_RESOLUTION:
      case AST_SUBSCRIPT:
      case AST_INITIALIZER:
      case AST_CAST:
      case AST_RANGE:
      case AST_SWITCH:
        return true;
      default:
        return false;
    }
  }

  union {
    std::vector<AST *> statements;
    struct {
      AST_Type_Kind kind = AST_TYPE_NORMAL;
      union {
        struct {
          AST *base;
          std::vector<AST *> generic_arguments;
        } normal;
        struct {
          std::vector<AST *> parameter_types;
          Nullable<AST> return_type;
        } function;
        // special info for tuple types.
        std::vector<AST *> tuple_types;
      };
      // [], *, [string] etc.
      std::vector<AST_Type_Extension> extensions;
      // special info for reflection
      Nullable<AST> pointing_to;
    } type;
    struct {
      size_t temp_iden_idx = 0;
      AST *parent;
      int flags = BLOCK_FLAGS_FALL_THROUGH;
      bool has_defer = false;
      int defer_count = 0;
      int return_type = Type::INVALID_TYPE_ID;
    } block;
    AST *expression_statement;

    struct {
      InternedString name;
      InternedString bitsize;
      // This isn't nullable, even though it can be null for part of compilation.
      // That's because if it ever was null, when it's done typing it will have been created.
      // It creates too much friction later on down the line if it's not.
      AST *type = nullptr;
      Nullable<AST> value;
      bool is_extern : 1 = false;
      bool is_constexpr : 1 = false;
      bool is_static : 1 = false;
      bool is_bitfield : 1 = false;
    } declaration;

    struct {
      AST *left;
      AST *right;
      Token_Type op;
      bool is_operator_overload : 1 = false;
    } binary;
    struct {
      AST *operand;
      Token_Type op;
      bool is_operator_overload : 1 = false;
    } unary;
    InternedString identifier;
    struct {
      AST_Literal_Tag tag;
      bool is_c_string : 1 = false;
      InternedString value;
    } literal;

    struct {
      std::vector<AST *> idens; // TODO: make it so you can take it by pointer, like *a, *b := some_struct;
      AST *right;
      Token_Type op;
    } tuple_deconstruction; // TODO: rename to multiple assignment.
    std::vector<AST *> tuple;

    struct {
      // various flags.
      int16_t flags = 0;
      // has a self/self* parameter.
      bool has_self : 1 = false;
      // params have varargs.
      bool is_varargs : 1 = false;
      // body contains defer.
      bool has_defer : 1 = false;
      // for methods.
      int declaring_type = Type::INVALID_TYPE_ID;

      Nullable<AST> where_clause; // where T: ... type constraint.
      std::vector<int> generic_arguments;
      std::vector<GenericParameter> generic_parameters;
      std::vector<GenericInstance> generic_instantiations;
      InternedString name;
      std::vector<AST_Parameter_Declaration> parameters;
      Nullable<AST> return_type;
      Nullable<AST> block;
    } function;

    struct {
      std::vector<AST *> generic_arguments;
      std::vector<AST *> arguments;
      AST *function;
    } call;

    struct {
      AST *base;
      InternedString member_name;
    } dot;

    struct {
      AST *base;
      InternedString member_name;
    } scope_resolution;

    Nullable<AST> $return;

    struct {
      AST *left;
      AST *right;
    } range;

    struct {
      enum {
        // implicitly pulled an iter() off a type that implements Iterable![T]
        ITERABLE,
        // implicitly pulled an enumerator() off a type that implements Enumerable![T]
        ENUMERABLE,
        // got an Enumerator![T] object directly
        ENUMERATOR,
        // got an Iter![T] object directly
        ITERATOR,
      } iteration_kind; // This is the type of the container/sequence, whatever implements .iter() / .enumerator()
      int range_type = Type::INVALID_TYPE_ID;
      // This is either the type of that implements Enumerator![T], or is Iter![T];
      int iterable_type = Type::INVALID_TYPE_ID;
      // This is the 'i' part of 'for i in...', the type of the whatchamacallit.
      int identifier_type = Type::INVALID_TYPE_ID;

      // this is the 'i' in `for i in 0..100`
      AST *iter_identifier;
      // this is the '0..100' or any thing on the right hand side of the 'in'
      // `for i in 0..100`
      //           ^^^^^^^ <- this is the `range`
      AST *range;
      AST *block;
      ValueSemantic value_semantic;
    } $for;

    struct {
      AST *condition;
      AST *block;
      Nullable<AST> $else;
    } $if;

    struct {
      Nullable<AST> elseif; // conditional else.
      Nullable<AST> block;
    } $else;

    struct {
      Nullable<AST> condition;
      AST *block;
    } $while;

    struct {
      bool is_operator_overload : 1 = false;
      AST *left;
      AST *index_expression;
    } subscript;

    struct {
      Nullable<AST> where_clause;
      InternedString name;
      bool is_fwd_decl : 1 = false;
      bool is_extern : 1 = false;
      bool is_union : 1 = false;
      std::vector<AST *> subtypes; // Right now this is only for '#anon :: struct // #anon :: union'
      std::vector<AST *> aliases;  // Right now this is only for '#anon :: struct // #anon :: union'
      std::vector<ASTStructMember> members;
      std::vector<GenericParameter> generic_parameters;
      std::vector<GenericInstance> generic_instantiations;
      std::vector<AST *> impls;
    } $struct;

    AST *size_of;

    struct {
      Nullable<AST> where_clause;
      InternedString name;
      std::vector<GenericParameter> generic_parameters;
      std::vector<GenericInstance> generic_instantiations;
      std::vector<AST *> methods;
    } interface;

    struct {
      bool is_flags : 1 = false;
      int element_type;
      InternedString name;
      std::vector<std::pair<InternedString, AST *>> key_values;
    } $enum;

    struct {
      union {
        std::vector<std::pair<InternedString, AST *>> key_values;
        std::vector<AST *> values;
      };
      AST_Initializer_Tag tag;
      Nullable<AST> target_type;
    } initializer;

    struct {
      bool is_statement : 1 = false;
      int return_type = void_type();
      AST *target;
      std::vector<SwitchCase> cases;
    } $switch;

    struct {
      InternedString name;
      AST *type;
    } alias;

    struct {
      Nullable<AST> where_clause;
      // impl 'target' or impl *interface for 'target'
      AST *target;
      Nullable<AST> interface;
      std::vector<GenericParameter> generic_parameters;
      std::vector<GenericInstance> generic_instantiations;
      // methods / static methods this is implementing for the type.
      std::vector<AST *> methods;
    } impl;

    AST *defer;

    struct {
      AST *expression;
      AST *target_type;
    } cast;

    struct {
      InternedString unique_identifier;
      std::vector<AST_Parameter_Declaration> parameters;
      AST *return_type;
      AST *block;
    } lambda;

    struct {
      AST *target_type;
      AST *predicate;
    } where;
  };

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto field_ct = 0;
    for (auto sym = symbol_table.head; sym; sym = sym->next) {
      if (!sym->is_function() && sym->is_type())
        field_ct++;
    }
    return field_ct;
  }

  void insert_variable(const InternedString &name, int type_id, AST *initial_value, AST *decl = nullptr) {
    symbol_table.insert(Symbol::create_variable(name, type_id, initial_value, decl));
  }

  void insert_function(const InternedString &name, const int type_id, AST *declaration,
                       SymbolFlags flags = SYMBOL_IS_FUNCTION) {
    symbol_table.insert(Symbol::create_function(name, type_id, declaration, flags));
  }

  void insert_type(const int type_id, const InternedString &name, TypeKind kind, AST *declaration) {
    symbol_table.insert(Symbol::create_type(type_id, name, kind, declaration));
  }

  Symbol *lookup(const InternedString &name) {
    if (auto sym = symbol_table.local_lookup(name)) {
      return sym;
    }
    if (parent) {
      return parent->lookup(name);
    }
    return nullptr;
  }

  // TODO: should this traverse upward to erase?

  void declare_interface(const InternedString &name, AST *node);

  int create_interface_type(const InternedString &name, const std::vector<int> &generic_args, AST *declaration) {
    auto id = global_create_interface_type(name, &declaration->symbol_table, generic_args);
    insert(Symbol::create_type(id, name, TYPE_INTERFACE, declaration));
    return id;
  }

  int create_struct_type(const InternedString &name, AST *declaration) {
    auto id = global_create_struct_type(name, &declaration->symbol_table);
    insert(Symbol::create_type(id, name, TYPE_STRUCT, declaration));
    return id;
  }

  void create_type_alias(const InternedString &name, int type_id, TypeKind kind, AST *declaring_node) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = type_id;
    symbol.type.kind = kind;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.declaration = declaring_node;
    symbol_table.insert(symbol);
  }

  void forward_declare_type(const InternedString &name, int default_id) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = default_id;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol_table.insert(symbol);
  }

  int create_enum_type(const InternedString &name, bool flags, AST *declaration) {
    auto id = global_create_enum_type(name, &declaration->symbol_table, flags);
    insert(Symbol::create_type(id, name, TYPE_STRUCT, declaration));
    return id;
  }

  int create_tuple_type(const std::vector<int> &types) {
    auto id = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
    symbol_table.insert(Symbol::create_type(id, name, TYPE_STRUCT, nullptr));
    return id;
  }

  int find_type_id(const InternedString &name, const TypeExtensions &ext) {
    auto symbol = lookup(name);
    if (!symbol || !symbol->is_type()) {
      return Type::INVALID_TYPE_ID;
    }
    return global_find_type_id(symbol->type_id, ext);
  }
};

extern AST *GLOBAL_NOOP;

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
  std::function<Nullable<AST>(Parser *parser)> run;
};

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
  Context &ctx;
  Lexer lexer{};
  std::vector<Lexer::State> states;
  Nullable<AST> current_struct_decl = nullptr;
  Nullable<AST> current_func_decl = nullptr;
  Nullable<AST> current_impl_decl = nullptr;
  Nullable<AST> current_interface_decl = nullptr;
  static std::vector<DirectiveRoutine> directive_routines;
  int64_t token_idx{};
  AST *last_parent;
  static Nullable<AST> current_block;

  void parse_parameters(const std::vector<GenericParameter> &generic_parameters, AST *node);

  Defer set_last_parent(AST *parent) {
    auto old = last_parent;
    last_parent = parent;
    return Defer([&] { last_parent = old; });
  }

  AST *parse();
  AST *parse_statement();
  void parse_arguments(AST *call);
  AST *parse_interface_declaration(Token);
  AST *parse_multiple_asssignment();
  AST *parse_struct_declaration(Token);
  AST *parse_declaration();
  AST *parse_enum_declaration(Token);
  AST *parse_lambda();
  AST *parse_block(Scope *scope = nullptr);
  AST *parse_expr(Precedence = PRECEDENCE_LOWEST);
  AST *parse_unary();
  AST *parse_postfix();
  AST *parse_primary();
  AST *parse_call(AST *function);
  AST *parse_impl();
  AST *parse_where_clause();
  AST *parse_type();
  AST *parse_function_declaration(Token);
  AST *parse_function_type();
  AST *parse_defer();

  void append_type_extensions(AST *&type);
  std::vector<AST_Parameter_Declaration> parse_parameters(std::vector<GenericParameter> params = {});
  Nullable<AST> process_directive(DirectiveKind kind, const InternedString &identifier);
  Nullable<AST> try_parse_directive_expr();

  std::vector<GenericParameter> parse_generic_parameters();
  std::vector<AST *> parse_generic_arguments();
  std::vector<AST *> parse_parameter_types();

  inline bool not_eof() const { return !peek().is_eof(); }
  inline bool eof() const { return peek().is_eof(); }
  inline bool semicolon() const { return peek().type == Token_Type::Semi; }
  InternedString type_name(AST *node);

  inline std::deque<Token> &lookahead_buf() { return states.back().lookahead_buffer; }

  Token eat();
  Token expect(Token_Type type);
  Token peek() const;

  void fill_buffer_if_needed();
  Source_Range begin_node();
  void end_node(AST *node, Source_Range &range);

  // returns true if successful, false if already included
  void import(InternedString name);

  Parser(const std::string &filename, Context &context);
  ~Parser();
};

static inline AST *ast_alloc(AST_Node_Type type, AST *parent) {
  auto node = new (ast_arena.allocate(sizeof(AST))) AST(type);
  node->parent = parent;
  node->declaring_block = Parser::current_block;
  return node;
}

static inline AST *find_generic_instance(std::vector<GenericInstance> instantiations,
                                         const std::vector<int> &gen_args) {
  for (auto &instantiation : instantiations) {
    if (instantiation.arguments == gen_args) {
      return instantiation.node;
    }
  }
  return nullptr;
}

#define NODE_ALLOC(type, node, range, parser, defer)                                                                   \
  AST *node = ast_alloc(type, parser->last_parent);                                                                    \
  auto range = parser->begin_node();                                                                                   \
  Defer defer([&] { parser->end_node(node, range); });

#define NODE_ALLOC_EXTRA_DEFER(type, node, range, defer, parser, deferred)                                             \
  AST *node = ast_alloc(type, parser->last_parent);                                                                    \
  auto range = parser->begin_node();                                                                                   \
  Defer defer([&] {                                                                                                    \
    parser->end_node(node, range);                                                                                     \
    deferred;                                                                                                          \
  });

#define DEFINE_VISITOR(node_name) void visit_##node_name(AST *node);

#define DEFINE_VISITORS()                                                                                              \
  DEFINE_VISITOR(program)                                                                                              \
  DEFINE_VISITOR(block)                                                                                                \
  DEFINE_VISITOR(function_declaration)                                                                                 \
  DEFINE_VISITOR(params_decl)                                                                                          \
  DEFINE_VISITOR(param_decl)                                                                                           \
  DEFINE_VISITOR(declaration)                                                                                          \
  DEFINE_VISITOR(expr_statement)                                                                                       \
  DEFINE_VISITOR(bin_expr)                                                                                             \
  DEFINE_VISITOR(unary_expr)                                                                                           \
  DEFINE_VISITOR(identifier)                                                                                           \
  DEFINE_VISITOR(literal)                                                                                              \
  DEFINE_VISITOR(type)                                                                                                 \
  DEFINE_VISITOR(tuple)                                                                                                \
  DEFINE_VISITOR(call)                                                                                                 \
  DEFINE_VISITOR(arguments)                                                                                            \
  DEFINE_VISITOR(return)                                                                                               \
  DEFINE_VISITOR(continue)                                                                                             \
  DEFINE_VISITOR(break)                                                                                                \
  DEFINE_VISITOR(for)                                                                                                  \
  DEFINE_VISITOR(if)                                                                                                   \
  DEFINE_VISITOR(else)                                                                                                 \
  DEFINE_VISITOR(while)                                                                                                \
  DEFINE_VISITOR(struct_declaration)                                                                                   \
  DEFINE_VISITOR(dot_expr)                                                                                             \
  DEFINE_VISITOR(scope_resolution)                                                                                     \
  DEFINE_VISITOR(subscript)                                                                                            \
  DEFINE_VISITOR(initializer_list)                                                                                     \
  DEFINE_VISITOR(enum_declaration)                                                                                     \
  DEFINE_VISITOR(tagged_union_declaration)                                                                             \
  DEFINE_VISITOR(noop)                                                                                                 \
  DEFINE_VISITOR(alias)                                                                                                \
  DEFINE_VISITOR(impl)                                                                                                 \
  DEFINE_VISITOR(interface_declaration)                                                                                \
  DEFINE_VISITOR(size_of)                                                                                              \
  DEFINE_VISITOR(defer)                                                                                                \
  DEFINE_VISITOR(cast)                                                                                                 \
  DEFINE_VISITOR(lambda)                                                                                               \
  DEFINE_VISITOR(range)                                                                                                \
  DEFINE_VISITOR(switch)                                                                                       \
  DEFINE_VISITOR(tuple_deconstruction)                                                                         \
  DEFINE_VISITOR(where)

#define DECLARE_VISITOR_METHODS(type)                                                                                  \
  void type::visit_program(AST *node) {}                                                                               \
  void type::visit_block(AST *node) {}                                                                                 \
  void type::visit_function_declaration(AST *node) {}                                                                  \
  void type::visit_params_decl(AST *node) {}                                                                           \
  void type::visit_param_decl(AST *node) {}                                                                            \
  void type::visit_declaration(AST *node) {}                                                                           \
  void type::visit_expr_statement(AST *node) {}                                                                        \
  void type::visit_bin_expr(AST *node) {}                                                                              \
  void type::visit_unary_expr(AST *node) {}                                                                            \
  void type::visit_identifier(AST *node) {}                                                                            \
  void type::visit_literal(AST *node) {}                                                                               \
  void type::visit_type(AST *node) {}                                                                                  \
  void type::visit_tuple(AST *node) {}                                                                                 \
  void type::visit_call(AST *node) {}                                                                                  \
  void type::visit_arguments(AST *node) {}                                                                             \
  void type::visit_return(AST *node) {}                                                                                \
  void type::visit_continue(AST *node) {}                                                                              \
  void type::visit_break(AST *node) {}                                                                                 \
  void type::visit_for(AST *node) {}                                                                                   \
  void type::visit_if(AST *node) {}                                                                                    \
  void type::visit_else(AST *node) {}                                                                                  \
  void type::visit_while(AST *node) {}                                                                                 \
  void type::visit_struct_declaration(AST *node) {}                                                                    \
  void type::visit_dot_expr(AST *node) {}                                                                              \
  void type::visit_scope_resolution(AST *node) {}                                                                      \
  void type::visit_subscript(AST *node) {}                                                                             \
  void type::visit_initializer_list(AST *node) {}                                                                      \
  void type::visit_enum_declaration(AST *node) {}                                                                      \
  void type::visit_tagged_union_declaration(AST *node) {}                                                              \
  void type::visit_noop(AST *node) {}                                                                                  \
  void type::visit_alias(AST *node) {}                                                                                 \
  void type::visit_impl(AST *node) {}                                                                                  \
  void type::visit_interface_declaration(AST *node) {}                                                                 \
  void type::visit_size_of(AST *node) {}                                                                               \
  void type::visit_defer(AST *node) {}                                                                                 \
  void type::visit_cast(AST *node) {}                                                                                  \
  void type::visit_lambda(AST *node) {}                                                                                \
  void type::visit_range(AST *node) {}                                                                                 \
  void type::visit_switch(AST *node) {}                                                                                \
  void type::visit_tuple_deconstruction(AST *node) {}                                                                  \
  void type::visit_where(AST *node) {}
Symbol *SymbolTable::local_lookup(const InternedString &name) {
  if (head == nullptr) {
    return nullptr;
  }
  auto sym = head;
  while (sym) {
    if (sym->name == name) {
      return sym;
    }
    sym = sym->next;
  }
  return nullptr;
}
void SymbolTable::insert(const Symbol &symbol) {
  Symbol **sym = &head;
  while (*sym) {
    sym = &(*sym)->next;
  }
  *sym = (Symbol *)symbol_arena.allocate(sizeof(Symbol));
  **sym = symbol;
}
bool SymbolTable::erase(const InternedString &name) {
  Symbol **sym = &head;
  while (*sym) {
    if ((*sym)->name == name) {
      *sym = (*sym)->next;
      return true;
    }
    sym = &(*sym)->next;
  }
  return false;
}
