#pragma once

#include "arena.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "ast.hpp"
#include "visitor.hpp"
#include <map>
#include <unordered_map>
#include <vector>

enum struct THIRNodeType : unsigned char {
  // Statements
  Program,
  Block,
  Variable,

  Function,
  Type,

  // Expressions
  ExpressionBlock,
  BinExpr,
  UnaryExpr,
  Literal,
  Call,
  MemberAccess,
  Cast,
  Index,
  AggregateInitializer,
  CollectionInitializer,
  EmptyInitializer,

  // Control Flow
  Return,
  Break,
  Continue,
  For,
  If,
  While,

  Noop,  // Some things just return nothing, but need to return something
};


#define THIR_NODE_TYPE_LIST \
  X(Program) \
  X(Block) \
  X(Variable) \
  X(Function) \
  X(Type) \
  X(ExpressionBlock) \
  X(BinExpr) \
  X(UnaryExpr) \
  X(Literal) \
  X(Call) \
  X(MemberAccess) \
  X(Cast) \
  X(Index) \
  X(AggregateInitializer) \
  X(CollectionInitializer) \
  X(EmptyInitializer) \
  X(Return) \
  X(Break) \
  X(Continue) \
  X(For) \
  X(If) \
  X(While) \
  X(Noop)

inline const char* node_type_to_string(THIRNodeType type) {
  switch (type) {
#define X(name) case THIRNodeType::name: return #name;
    THIR_NODE_TYPE_LIST
#undef X
    default: return "<unknown>";
  }
}

struct THIR {
  // This is purely used to handle putting semicolons after expression statements, without needing an 'expression
  // statement' node This is surely irrelevant to anything but a C backend, but I can't think of an easier nor cheaper
  // way to do this it is a bit messy, but it's far preferable to THIRStmt/THIRExpr && ThirExprStatement.
  bool is_statement = false;

  bool deprecated;
  Attribute deprecated_attr;

  // We default this to void so we never get any bad reads; full confidence this field cannot be null.
  // statements are considered void nodeessions anyway in the THIR.
  Type *type = void_type();
  Span span;

  virtual ~THIR() {}
  virtual THIRNodeType get_node_type() const = 0;

  bool is_temporary_value() const {
    switch (get_node_type()) {
      case THIRNodeType::Literal:
      case THIRNodeType::Call:
      case THIRNodeType::Cast:
      case THIRNodeType::ExpressionBlock:
        return true;
      // These are technically rvalues, but C doesn't care, so for now, we ignore them.
      // it would be best to not do this.
      case THIRNodeType::AggregateInitializer:
      case THIRNodeType::CollectionInitializer:
      case THIRNodeType::EmptyInitializer:
      default:
        return false;
    }
  }
};

struct THIRNoop : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Noop; }
  static THIRNoop *shared() {
    static THIRNoop noop;
    return &noop;
  }
};

struct THIRProgram : THIR {
  std::vector<THIR *> statements;
  THIRNodeType get_node_type() const override { return THIRNodeType::Program; }
};

struct Value;
struct THIRBinExpr;
struct THIRVariable : THIR {
  InternedString name;
  THIR *value;
  // Global variables will always have ".value" stored in here as .right, and this is what will get
  // read from and mutated during compile time.
  THIRBinExpr *global_initializer_assignment;
  Value *compile_time_value;
  Symbol *symbol;

  bool use_compile_time_value_at_emit_time : 1 = false;
  bool is_static : 1 = false;
  bool is_global : 1 = false;
  bool is_constexpr : 1 = false;
  bool is_extern : 1 = false;
  THIRNodeType get_node_type() const override { return THIRNodeType::Variable; }
};

/*
  We can use this to emit various types,
  'struct'
  'union'
  'choice'
  'dyn ...'
  tuple types.

  as long as we structure it in a desugared, literal way.
*/
struct THIRType : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Type; }
};

// this is just a block that can return a value, and can be used as an expression.
// used primarily for 
// x := if true { 1 } else { false };
struct THIRExprBlock : THIR {
  std::vector<THIR *> statements;
  THIRVariable *return_register;
  THIRNodeType get_node_type() const override { return THIRNodeType::ExpressionBlock; }
};

struct THIRBlock : THIR {
  std::vector<THIR *> statements;
  THIRNodeType get_node_type() const override { return THIRNodeType::Block; }
};

struct THIRParameter {
  Mutability mutability;
  InternedString name;
  THIR *default_value;
  THIRVariable *associated_variable;
};

struct THIRFunction : THIR {
  // this is for builtins, often ones that are just C macros.
  bool is_extern : 1 = false;
  bool is_inline : 1 = false;
  bool is_exported : 1 = false;
  bool is_test : 1 = false;
  bool is_varargs : 1 = false;
  bool is_entry : 1 = false;
  bool is_no_return : 1 = false;
  bool is_no_mangle : 1 = false;
  bool is_macro : 1 = false;

  /* 
    0 == not a constructor.
    1 == run in the global initializer function, after all globals have run. 
    2 == use clang __attribute__(constructor) (or an equivalent)
  */
  uint8_t constructor_index : 2;

  InternedString name;
  std::vector<THIRParameter> parameters;

  THIRBlock *block;
  THIRNodeType get_node_type() const override { return THIRNodeType::Function; }
};

struct THIRBinExpr : THIR {
  THIR *left;
  THIR *right;
  TType op;
  THIRNodeType get_node_type() const override { return THIRNodeType::BinExpr; }
};

struct THIRUnaryExpr : THIR {
  THIR *operand;
  TType op;
  THIRNodeType get_node_type() const override { return THIRNodeType::UnaryExpr; }
};

struct THIRLiteral : THIR {
  ASTLiteral::Tag tag;
  InternedString value;
  bool is_c_string = false;
  THIRNodeType get_node_type() const override { return THIRNodeType::Literal; }
};

struct THIRCall : THIR {
  struct {
    bool is_dyn_call = false;
    InternedString dyn_method_name = "";
  };
  THIR *callee;
  std::vector<THIR *> arguments;
  THIRNodeType get_node_type() const override { return THIRNodeType::Call; }
};

struct THIRMemberAccess : THIR {
  THIR *base;
  InternedString member;
  THIRNodeType get_node_type() const override { return THIRNodeType::MemberAccess; }
};

struct THIRCast : THIR {
  THIR *operand;
  THIRNodeType get_node_type() const override { return THIRNodeType::Cast; }
};

struct THIRIndex : THIR {
  THIR *base;
  THIR *index;
  THIRNodeType get_node_type() const override { return THIRNodeType::Index; }
};

struct THIRReturn : THIR {
  THIR *expression;
  THIRNodeType get_node_type() const override { return THIRNodeType::Return; }
};

struct THIRBreak : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Break; }
};

struct THIRContinue : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Continue; }
};

struct THIRFor : THIR {
  THIR *initialization;
  THIR *condition;
  THIR *increment;
  THIR *block;
  THIRNodeType get_node_type() const override { return THIRNodeType::For; }
};

struct THIRIf : THIR {
  THIR *condition;
  THIRBlock *block;
  THIR *_else;
  THIRNodeType get_node_type() const override { return THIRNodeType::If; }
};

struct THIRWhile : THIR {
  THIR *condition;
  THIRBlock *block;
  THIRNodeType get_node_type() const override { return THIRNodeType::While; }
};

struct THIREmptyInitializer : THIR {
  bool is_uninitialized = false; // TODO: add --- ast to indicate we don't want to initialize a variable.
  THIRNodeType get_node_type() const override { return THIRNodeType::EmptyInitializer; }
};

struct THIRAggregateInitializer : THIR {
  std::vector<std::pair<InternedString, THIR *>> key_values;
  THIRNodeType get_node_type() const override { return THIRNodeType::AggregateInitializer; }
};

struct THIRCollectionInitializer : THIR {
  bool is_variable_length_array = false;
  std::vector<THIR *> values;
  THIRNodeType get_node_type() const override { return THIRNodeType::CollectionInitializer; }
};

extern jstl::Arena thir_arena;

template <class T>
static inline T *thir_alloc() {
  return new (thir_arena.allocate(sizeof(T))) T();
}

#define THIR_ALLOC(__type, __name, ast)                                                \
  static_assert(std::is_base_of<THIR, __type>::value, "__type must derive from THIR"); \
  __type *__name = thir_alloc<__type>();                                               \
  __name->span = ast->span;                                            \
  __name->type = ast->resolved_type;

#define THIR_ALLOC_NO_SRC_RANGE(__type, __name)                                        \
  static_assert(std::is_base_of<THIR, __type>::value, "__type must derive from THIR"); \
  __type *__name = thir_alloc<__type>();

struct ReflectionInfo {
  // const Type type_info_for_this_type = {...}; global variable.
  THIRVariable *definition;
  // &type_info_of_this_type, unary address of expression getting a pointer to that stable memory.
  THIRUnaryExpr *reference;

  bool created = false;
  bool has_been_created() const { return created; }
};

enum struct DeferBoundary {
  FUNCTION,
  LOOP,
  BLOCK,
};

struct DeferFrame {
  DeferBoundary boundary;
  // Each defer may contain multiple statements; store as groups so that when we
  // execute defers in LIFO order we keep the statements inside each defer in
  // their original order.
  std::vector<THIR *> defers;
};

struct THIRGen {
  
  THIRGen(Context &ctx, bool for_emitter = true);
  std::vector<THIR *> *current_expression_list;
  bool is_making_call = false;
  Context &ctx;
  
  std::vector<THIRFunction *> constructors;
  std::vector<THIRFunction *> test_functions;
  
  THIRCall *global_initializer_call;
  THIRFunction *global_initializer_function;
  
  std::vector<DeferFrame> defer_stack;
  
  void enter_defer_boundary(DeferBoundary boundary);
  void exit_defer_boundary();

  THIR *option_some(THIR *value, Type *interior_type);
  THIR *option_none(Type *interior_type);

  void check_for_deprecation(Span call_site, THIR *thir);
  void format_and_print_deprecated_warning(Span call_site, THIR *thir, const Attribute &attr);
  void convert_function_attributes(THIRFunction *reciever, const std::vector<Attribute> &attrs);
  
  Symbol *get_symbol(ASTNode *);

  // remove frames up to `boundary` and return defers in execution order
  std::vector<THIR *> collect_defers_up_to(DeferBoundary boundary);

  std::map<Symbol *, THIR *> symbol_map;
  std::map<ASTNode *, THIR *> ast_map;

  inline void bind(ASTNode *ast, THIR *thir) {
    if (!ast) {
      throw_error("Bound a null AST to a thir", thir->span);
    }
    if (!thir) {
      throw_error("Bound a null thir to an AST", ast->span);
    }
    ast_map[ast] = thir;
  }

  inline void bind(Symbol *sym, THIR *thir) {
    if (!thir) {
      throw_error("Bound a null thir to a symbol", {});
    }
    if (!sym) {
      throw_error("Bound a null symbol to a thir", thir->span);
    }

    symbol_map[sym] = thir;
  }

  inline THIR *get_thir(ASTNode *ast) { return ast_map[ast]; }

  inline THIR *get_thir(Symbol *sym) { return symbol_map[sym]; }

  THIR *entry_point;
  THIRProgram *program;

  // The "return override register" is used to capture the result of a block or function,
  // mainly for things like defer, early returns, or blocks that yield values. Instead of returning
  // directly, we write the result to this register (almost always a variable), so the correct value is
  // available for the final return or whatever comes next.
  Nullable<THIRVariable> return_override_register;
  size_t return_override_register_index = 0;

  std::unordered_map<const Type *, ReflectionInfo> reflected_upon_types;

  Type *type_ptr_list = nullptr;
  Type *method_list = nullptr;
  Type *field_list = nullptr;

  void set_reflection_types(Typer &typer);

  inline Type *iterator_trait() const {
    static Type *iter_id = ctx.scope->lookup("Iterator")->resolved_type;
    return iter_id;
  }

  inline Type *iterable_trait() const {
    static Type *iterable_id = ctx.scope->lookup("Iterable")->resolved_type;
    return iterable_id;
  }

  inline THIRVariable *init_override_register(ASTNode *node) {
    THIR_ALLOC(THIRVariable, override_register, node)
    override_register->name = get_temporary_variable();
    THIR_ALLOC(THIREmptyInitializer, empty_init, node)
    override_register->value = empty_init;
    override_register->type = node->resolved_type;
    return_override_register = {override_register};
    return override_register;
  }

  void convert_parameters(ASTFunctionDeclaration *&ast, THIRFunction *&thir);

  // This will either point to the entire THIRProgram, or, it will point to a function, either being `fn main()` or any
  // `@[entry]` tagged function.
  inline THIR *get_entry_point() const { return entry_point; }

  // This is always a program, module, or block. it's for visit functions that need to return many nodes, from one call.
  // typically these will just return void.
  std::vector<THIR *> *current_statement_list;

  THIR *visit_for_c_style(ASTForCStyle *ast);

  THIR *visit_unpack_element(ASTUnpackElement *ast);

  THIR *try_deref_or_take_ptr_to_if_needed(ASTExpr *const base, THIR *self, const bool requires_self_ptr);
  THIR *visit_method_call(ASTMethodCall *node);
  THIR *visit_path(ASTPath *node);

  void make_destructure_for_pattern_match(ASTPatternMatch *ast, THIR *object, Scope *block_scope,
                                          std::vector<THIR *> &statements, Type *variant_type,
                                          const InternedString &variant_name);

  THIR *visit_pattern_match_condition(ASTPatternMatch *ast, THIR *cached_object, const size_t discriminant);
  THIR *visit_pattern_match(ASTPatternMatch *node, Scope *scope, std::vector<THIR *> &statements);

  THIR *make_structural_typing_bitcast(Type *to, THIR *expr);

  THIR *visit_dyn_of(ASTDyn_Of *node);
  THIR *visit_type_of(ASTType_Of *node);
  THIR *visit_block(ASTBlock *node);
  THIR *visit_bin_expr(ASTBinExpr *node);
  THIR *visit_unary_expr(ASTUnaryExpr *node);
  THIR *visit_literal(ASTLiteral *node);

  void extract_arguments_desugar_defaults(const THIR *callee, const ASTArguments *in_args,
                                          std::vector<THIR *> &out_args, Nullable<THIR> self = nullptr);

  void extract_thir_values_for_type_members(Type *type);

  THIR *visit_call(ASTCall *node);
  THIR *visit_return(ASTReturn *node);
  THIR *visit_dot_expr(ASTDotExpr *node);
  THIR *visit_index(ASTIndex *node);
  THIR *visit_initializer_list(ASTInitializerList *node);
  THIR *visit_range(ASTRange *node);
  THIR *visit_switch(ASTSwitch *node);
  THIR *visit_tuple(ASTTuple *node);
  THIR *visit_cast(ASTCast *node);
  THIR *visit_lambda(ASTLambda *node);
  THIR *visit_size_of(ASTSize_Of *node);
  THIR *visit_struct_declaration(ASTStructDeclaration *node);
  THIR *initialize(const Span &span, Type *type,
                   std::vector<std::pair<InternedString, ASTExpr *>> key_values);
  THIR *visit_program(ASTProgram *node);

  void mangle_function_name_for_thir(ASTFunctionDeclaration *&ast, THIRFunction *&thir);
  THIR *visit_function_declaration(ASTFunctionDeclaration *node);
  THIR *visit_variable(ASTVariable *node);
  THIR *visit_continue(ASTContinue *node);
  THIR *visit_break(ASTBreak *node);
  THIR *visit_for(ASTFor *node);
  THIR *visit_if(ASTIf *node);
  THIR *visit_else(ASTElse *node);
  THIR *visit_while(ASTWhile *node);
  THIR *visit_enum_declaration(ASTEnumDeclaration *node);
  THIR *visit_defer(ASTDefer *node);
  THIR *visit_choice_declaration(ASTChoiceDeclaration *node);
  THIR *visit_expr_statement(ASTExprStatement *node);

  THIR *take_address_of(THIR *node, ASTNode *ast);
  THIRVariable *make_variable(const InternedString &name, THIR *value, ASTNode *ast, bool is_global = false);
  THIR *make_cast(THIR *operand, Type *type);
  THIR *make_str(const InternedString &value, const Span &src_range);
  THIR *make_literal(const InternedString &value, const Span &src_range, Type *type, ASTLiteral::Tag tag);

  THIR *make_member_access(const Span &range, THIR *base, std::deque<std::pair<Type *, InternedString>> parts);

  void visit_module(ASTModule *node);
  void visit_import(ASTImport *node);
  void visit_impl(ASTImpl *node);
  void visit_destructure(ASTDestructure *node);
  void visit_where_statement(ASTWhereStatement *node);

  THIR *visit_run(ASTRun *);

  // instantiate conversions bool is needed because we recurse on these to set the
  // operand of a cast when capturing otherwise implicit casts as explicit casts.
  THIR *visit_node(ASTNode *node, bool instantiate_conversions = true);

  THIR *get_field_struct_list(Type *type);
  THIR *get_methods_list(Type *type);
  THIR *get_traits_list(Type *type);
  THIR *get_generic_args_list(Type *type);

  THIR *get_method_struct(const std::string &name, Type *type);
  THIR *get_field_struct(const std::string &name, Type *type, Type *parent_type);
  ReflectionInfo create_reflection_type_struct(Type *type);
  THIR *to_reflection_type_struct(Type *type);
  void emit_destructure_for_pattern_match(ASTPatternMatch *pattern_match, std::vector<THIR *> &statements);

  void visit_where_branch(const WhereBranch *branch);
  THIRFunction *emit_runtime_entry_point();

  void make_global_initializer(const Type *, THIRVariable *thir, Nullable<ASTExpr> value);
  void setup__all_tests();
};