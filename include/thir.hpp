#pragma once

#include "arena.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "ast.hpp"
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

  Size_Of,
  Offset_Of,
  // Control Flow
  Return,
  Break,
  Continue,
  For,
  If,
  While,
};

struct THIR {
  // This is purely used to handle putting semicolons after expression statements, without needing an 'expression
  // statement' node This is surely irrelevant to anything but a C backend, but I can't think of an easier nor cheaper
  // way to do this it is a bit messy, but it's far preferable to THIRStmt/THIRExpr && ThirExprStatement.
  bool is_statement = false;

  // We default this to void so we never get any bad reads; full confidence this field cannot be null.
  // statements are considered void nodeessions anyway in the THIR.
  Type *type = void_type();
  SourceRange source_range;

  virtual ~THIR() {}
  virtual THIRNodeType get_node_type() const = 0;

  bool is_temporary_value() const {
    switch (get_node_type()) {
      case THIRNodeType::Literal:
      case THIRNodeType::Call:
      case THIRNodeType::Cast:
      case THIRNodeType::ExpressionBlock:
      case THIRNodeType::Size_Of:
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

struct THIRProgram : THIR {
  std::vector<THIR *> statements;
  THIRNodeType get_node_type() const override { return THIRNodeType::Program; }
};

struct THIRVariable : THIR {
  InternedString name;
  THIR *value;
  bool is_global;
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
// this distinction is important in C, but llvm ir too probably.
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
  InternedString name;
  THIR *default_value;
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
  InternedString value;
  THIRNodeType get_node_type() const override { return THIRNodeType::Literal; }
};

struct THIRCall : THIR {
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

struct THIRSizeOf : THIR {
  Type *target_type;
  THIRNodeType get_node_type() const override { return THIRNodeType::Size_Of; }
};

struct THIROffsetOf : THIR {
  Type *target_type;
  InternedString target_field;
  THIRNodeType get_node_type() const override { return THIRNodeType::Offset_Of; }
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
  THIRNodeType get_node_type() const override { return THIRNodeType::EmptyInitializer; }
};

struct THIRAggregateInitializer : THIR {
  std::vector<std::pair<InternedString, THIR *>> key_values;
  THIRNodeType get_node_type() const override { return THIRNodeType::AggregateInitializer; }
};

struct THIRCollectionInitializer : THIR {
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
  __name->source_range = ast->source_range;                                            \
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

struct THIRGen {
  THIRGen(Context &ctx) : ctx(ctx) {}
  Context &ctx;
  // We use this for some temporary AST generation, primarily used during desugaring things like For loops.
  std::map<Symbol *, THIR *> symbol_map;
  THIR *entry_point;

  // The "return override register" is used to capture the result of a block or function,
  // mainly for things like defer, early returns, or blocks that yield values. Instead of returning
  // directly, we write the result to this register (almost always a variable), so the correct value is
  // available for the final return or whatever comes next.
  Nullable<THIRVariable> return_override_register;
  size_t return_override_register_index = 0;

  // Here, the variable is the original, and the unary expression is the address-of you can use to refer to the type.
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
    override_register->name = std::format(THIR_RETURN_OVERRIDE_REGISTER_KEY_FORMAT, return_override_register_index++);
    THIR_ALLOC(THIREmptyInitializer, empty_init, node)
    override_register->value = empty_init;
    override_register->type = node->resolved_type;
    return_override_register = {override_register};
    return override_register;
  }

  // This will either point to the entire THIRProgram, or, it will point to a function, either being `fn main()` or any
  // `@[entry]` tagged function.
  inline THIR *get_entry_point() const { return entry_point; }

  // This is always a program, module, or block. it's for visit functions that need to return many nodes, from one call.
  // typically these will just return void.
  std::vector<THIR *> *current_statement_list;

  THIR *visit_method_call(ASTMethodCall *node);
  THIR *visit_path(ASTPath *node);

  void make_destructure_for_pattern_match(ASTPatternMatch *ast, THIR *object, Scope *block_scope,
                                          std::vector<THIR *> &statements, Type *variant_type,
                                          const InternedString &variant_name);
  THIR *visit_pattern_match_condition(ASTPatternMatch *ast, THIR *cached_object, const size_t discriminant);
  THIR *visit_pattern_match(ASTPatternMatch *node, Scope *scope, std::vector<THIR *> &statements);

  THIR *visit_dyn_of(ASTDyn_Of *node);
  THIR *visit_type_of(ASTType_Of *node);
  THIR *visit_block(ASTBlock *node);
  THIR *visit_bin_expr(ASTBinExpr *node);
  THIR *visit_unary_expr(ASTUnaryExpr *node);
  THIR *visit_literal(ASTLiteral *node);

  void extract_arguments_desugar_defaults(const THIR *callee, const ASTArguments *in_args,
                                          std::vector<THIR *> &out_args);

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
  THIR *initialize(const SourceRange &source_range, Type *type,
                   const std::vector<std::pair<InternedString, ASTExpr *>> &key_values);
  THIR *visit_program(ASTProgram *node);
  THIR *visit_function_declaration_via_symbol(Symbol *symbol);
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
  THIR *make_str(const InternedString &value, const SourceRange &src_range);
  THIR *make_literal(const InternedString &value, const SourceRange &src_range, Type *type);

  THIR *make_member_access(const SourceRange &range, THIR *base, std::deque<std::pair<Type *, InternedString>> parts);

  void visit_module(ASTModule *node);
  void visit_import(ASTImport *node);
  void visit_impl(ASTImpl *node);
  void visit_destructure(ASTDestructure *node);
  void visit_where_statement(ASTWhereStatement *node);

  THIR *visit_node(ASTNode *node) {
    switch (node->get_node_type()) {
      case AST_NODE_STATEMENT_LIST: {
        for (const auto &ast_stmt : ((ASTStatementList *)node)->statements) {
          if (auto thir = visit_node(ast_stmt)) {
            current_statement_list->push_back(thir);
          }
        }
        return nullptr;
      }

      // These nodes can return many nodes, so they always return void, and push the nodes manually.
      case AST_NODE_TUPLE_DECONSTRUCTION: {
        visit_destructure((ASTDestructure *)node);
        return nullptr;
      }
      case AST_NODE_WHERE_STATEMENT: {
        visit_where_statement((ASTWhereStatement *)node);
        return nullptr;
      }
      case AST_NODE_IMPL: {
        visit_impl((ASTImpl *)node);
        return nullptr;
      }
      case AST_NODE_IMPORT: {
        visit_import((ASTImport *)node);
        return nullptr;
      }
      case AST_NODE_MODULE: {
        visit_module((ASTModule *)node);
        return nullptr;
      }

      // Actual nodes.
      case AST_NODE_IF:
        return visit_if((ASTIf *)node);
      case AST_NODE_LAMBDA:
        return visit_lambda((ASTLambda *)node);
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr((ASTBinExpr *)node);
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr((ASTUnaryExpr *)node);
      case AST_NODE_LITERAL:
        return visit_literal((ASTLiteral *)node);
      case AST_NODE_PATH:
        return visit_path((ASTPath *)node);
      case AST_NODE_TUPLE:
        return visit_tuple((ASTTuple *)node);
      case AST_NODE_CALL:
        return visit_call((ASTCall *)node);
      case AST_NODE_METHOD_CALL:
        return visit_method_call((ASTMethodCall *)node);
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr((ASTDotExpr *)node);
      case AST_NODE_INDEX:
        return visit_index((ASTIndex *)node);
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list((ASTInitializerList *)node);
      case AST_NODE_SIZE_OF:
        return visit_size_of((ASTSize_Of *)node);
      case AST_NODE_TYPE_OF:
        return visit_type_of((ASTType_Of *)node);
      case AST_NODE_DYN_OF:
        return visit_dyn_of((ASTDyn_Of *)node);
      case AST_NODE_CAST:
        return visit_cast((ASTCast *)node);
      case AST_NODE_RANGE:
        return visit_range((ASTRange *)node);
      case AST_NODE_SWITCH:
        return visit_switch((ASTSwitch *)node);
      case AST_NODE_PATTERN_MATCH:
        throw_error(
            "INTERNAL COMPILER ERROR:THIR :: you cannot visit a PatternMatch without explicitly calling into it, with "
            "the list of statements it needs to unload into.",
            node->source_range);
        return nullptr;
      // Statement nodes
      case AST_NODE_BLOCK:
        return visit_block((ASTBlock *)node);
      case AST_NODE_FUNCTION_DECLARATION:
        return visit_function_declaration((ASTFunctionDeclaration *)node);
      case AST_NODE_RETURN:
        return visit_return((ASTReturn *)node);
      case AST_NODE_CONTINUE:
        return visit_continue((ASTContinue *)node);
      case AST_NODE_BREAK:
        return visit_break((ASTBreak *)node);
      case AST_NODE_FOR:
        return visit_for((ASTFor *)node);
      case AST_NODE_ELSE:
        return visit_else((ASTElse *)node);
      case AST_NODE_WHILE:
        return visit_while((ASTWhile *)node);
      case AST_NODE_STRUCT_DECLARATION:
        return visit_struct_declaration((ASTStructDeclaration *)node);
      case AST_NODE_ENUM_DECLARATION:
        return visit_enum_declaration((ASTEnumDeclaration *)node);
      case AST_NODE_CHOICE_DECLARATION:
        return visit_choice_declaration((ASTChoiceDeclaration *)node);
      case AST_NODE_VARIABLE:
        return visit_variable((ASTVariable *)node);
      case AST_NODE_EXPR_STATEMENT:
        return visit_expr_statement((ASTExprStatement *)node);
      case AST_NODE_DEFER:
        return visit_defer((ASTDefer *)node);
      case AST_NODE_PROGRAM:
        return visit_program((ASTProgram *)node);

      // Ignored nodes.
      case AST_NODE_NOOP:
      case AST_NODE_ALIAS:
      case AST_NODE_TRAIT_DECLARATION:
        return nullptr;

      case AST_NODE_PARAMS_DECL:
      case AST_NODE_TYPE:
      case AST_NODE_PARAM_DECL:
      case AST_NODE_ARGUMENTS:
      case AST_NODE_WHERE:
        throw_error(
            "INTERNAL COMPILER ERROR: ast node not supported by thir gen. it's likely it just needs to be moved to the "
            "ignored cases",
            node->source_range);
        return nullptr;
    }
  }

  THIR *get_field_struct_list(Type *type);
  THIR *get_methods_list(Type *type);
  THIR *get_traits_list(Type *type);
  THIR *get_generic_args_list(Type *type);

  THIR *get_method_struct(const std::string &name, Type *type);
  THIR *get_field_struct(const std::string &name, Type *type, Type *parent_type);
  ReflectionInfo create_reflection_type_struct(Type *type);
  THIR *to_reflection_type_struct(Type *type);
  void emit_destructure_for_pattern_match(ASTPatternMatch *pattern_match, std::vector<THIR *> &statements);
};