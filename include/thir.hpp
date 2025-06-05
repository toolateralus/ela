#pragma once

#include "arena.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "ast.hpp"
#include <map>
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
  bool is_extern : 1;
  bool is_inline : 1;
  bool is_exported : 1;
  bool is_test : 1;
  bool is_varargs : 1;
  bool is_entry : 1;
  bool is_no_return : 1;
  bool is_no_mangle : 1;

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
  Type *target;
  THIRNodeType get_node_type() const override { return THIRNodeType::Size_Of; }
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

struct THIRGen {
  THIRGen(Typer &typer, Context &ctx) : ctx(ctx), typer(typer) {}
  Context &ctx;
  // We use this for some temporary AST generation, primarily used during desugaring things like For loops.
  Typer &typer;
  std::map<Symbol *, THIR *> symbol_map;
  THIR *entry_point;

  // The "return override register" is used to capture the result of a block or function,
  // mainly for things like defer, early returns, or blocks that yield values. Instead of returning
  // directly, we write the result to this register (almost always a variable), so the correct value is
  // available for the final return or whatever comes next.
  Nullable<THIRVariable> return_override_register;
  size_t return_override_register_index = 0;

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
  THIR *visit_pattern_match(ASTPatternMatch *node);
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
  THIR *initialize(const SourceRange &source_range, Type *type, const std::vector<std::pair<InternedString, ASTExpr *>> &key_values);
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

  void visit_module(ASTModule *node);
  void visit_import(ASTImport *node);
  void visit_impl(ASTImpl *node);
  void visit_tuple_deconstruction(ASTDestructure *node);
  void visit_where_statement(ASTWhereStatement *node);

  THIR *visit_node(ASTNode *node) {
    switch (node->get_node_type()) {
      case AST_NODE_STATEMENT_LIST: {
        for (const auto &ast_stmt : ((ASTStatementList *)node)->statements) {
          visit_node(ast_stmt);
        }
        return nullptr;
      }

      // These nodes can return many nodes, so they always return void, and push the nodes manually.
      case AST_NODE_TUPLE_DECONSTRUCTION: {
        visit_tuple_deconstruction((ASTDestructure *)node);
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
        return visit_pattern_match((ASTPatternMatch *)node);
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
};