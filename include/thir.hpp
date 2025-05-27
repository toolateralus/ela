#pragma once

#include "arena.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "ast.hpp"
#include <variant>
#include <vector>

enum struct THIRNodeType : unsigned char {
  Path,
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

  Return,
  Break,
  Continue,
  For,
  If,
  While,
  Switch,
  Variable,
  Block,
};

struct THIR {
  THIR() {}
  // We default this to void so we never get any bad reads; full confidence this field cannot be null.
  // statements are considered void expressions anyway in the THIR.
  Type *type = void_type();
  SourceRange source_range;
  virtual ~THIR() {}
  virtual THIRNodeType get_node_type() const = 0;
};

struct THIRBlock : THIR {
  std::vector<THIR *> statements;
  THIRNodeType get_node_type() const override { return THIRNodeType::Block; }
};

struct THIRFunction {
  InternedString name;
  // includes parameters and return type of course.
  Type *type;
  // Where the meat of the executable portion of the program resides.
  THIRBlock block;

  size_t id;
};

struct THIRVariable : THIR {
  InternedString name;
  THIR *value;
  THIRNodeType get_node_type() const override { return THIRNodeType::Variable; }
};

struct THIRContext {
  std::vector<THIRFunction> functions;
  std::vector<THIRVariable> global_variables;
  inline size_t insert_function(const THIRFunction &fn) {
    size_t id = functions.size();
    functions.push_back(std::move(fn));
    return id;
  }
};

struct THIRPath : THIR {
  enum Kind { Function, Variable } tag;
  union {
    THIRFunction *function;
    THIRVariable *variable;
  };
  THIRNodeType get_node_type() const override { return THIRNodeType::Path; }
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
  THIRBlock *block;
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

struct THIRSwitch : THIR {
  THIR *target;
  std::vector<std::pair<THIR *, THIRBlock *>> branches;
  THIRNodeType get_node_type() const override { return THIRNodeType::Switch; }
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

template <class T> static inline T *thir_alloc() { return (T *)thir_arena.allocate(sizeof(T)); }

#define THIR_ALLOC_EXPR(__type, __name, ast)                                                                           \
  static_assert(std::is_base_of<THIR, __type>::value, "__type must derive from THIR");                                 \
  __type *__name = thir_alloc<__type>();                                                                               \
  __name->source_range = ast->source_range;                                                                            \
  __name->type = ast->resolved_type;

#define THIR_ALLOC_STMT(__type, __name, ast)                                                                           \
  static_assert(std::is_base_of<THIR, __type>::value, "__type must derive from THIR");                                 \
  __type *__name = thir_alloc<__type>();                                                                               \
  __name->source_range = ast->source_range;

#define THIR_ALLOC_NO_SRC_RANGE(__type, __name)                                                                        \
  static_assert(std::is_base_of<THIR, __type>::value, "__type must derive from THIR");                                 \
  __type *__name = thir_alloc<__type>();

struct THIRVisitor {
  THIRVisitor(Context &ctx) : ctx(ctx) {}
  Context &ctx;
  THIR *visit_method_call(ASTMethodCall *node);
  THIR *visit_path(ASTPath *node);
  THIR *visit_pattern_match(ASTPatternMatch *node);
  THIR *visit_dyn_of(ASTDyn_Of *node);
  THIR *visit_type_of(ASTType_Of *node);
  THIR *visit_block(ASTBlock *node);
  THIR *visit_expr_statement(ASTExprStatement *node);
  THIR *visit_bin_expr(ASTBinExpr *node);
  THIR *visit_unary_expr(ASTUnaryExpr *node);
  THIR *visit_literal(ASTLiteral *node);
  THIR *visit_call(ASTCall *node);
  THIR *visit_return(ASTReturn *node);
  THIR *visit_dot_expr(ASTDotExpr *node);
  THIR *visit_index(ASTIndex *node);
  THIR *visit_initializer_list(ASTInitializerList *node);
  THIR *visit_range(ASTRange *node);
  THIR *visit_switch(ASTSwitch *node);
  THIR *visit_tuple(ASTTuple *node);
  THIR *load_value(ASTNode *node, THIR *expr); // What exactly is this supposed to do?
  THIR *visit_cast(ASTCast *node);
  THIR *visit_lambda(ASTLambda *node);
  THIR *visit_size_of(ASTSize_Of *node);
  THIR *visit_struct_declaration(ASTStructDeclaration *node);
  THIR *visit_module(ASTModule *node);
  THIR *visit_import(ASTImport *node);
  THIR *visit_program(ASTProgram *node);
  THIR *visit_function_declaration(ASTFunctionDeclaration *node);
  THIR *visit_variable(ASTVariable *node);
  THIR *visit_continue(ASTContinue *node);
  THIR *visit_break(ASTBreak *node);
  THIR *visit_for(ASTFor *node);
  THIR *visit_if(ASTIf *node);
  THIR *visit_else(ASTElse *node);
  THIR *visit_while(ASTWhile *node);
  THIR *visit_enum_declaration(ASTEnumDeclaration *node);
  THIR *visit_tuple_deconstruction(ASTDestructure *node);
  THIR *visit_impl(ASTImpl *node);
  THIR *visit_defer(ASTDefer *node);
  THIR *visit_choice_declaration(ASTChoiceDeclaration *node);

  THIR *visit_statement(ASTStatement *statement) {
    switch (statement->get_node_type()) {
      case AST_NODE_BLOCK:
        return visit_block(static_cast<ASTBlock *>(statement));
      case AST_NODE_FUNCTION_DECLARATION:
        return visit_function_declaration(static_cast<ASTFunctionDeclaration *>(statement));
      case AST_NODE_IMPL:
        return visit_impl(static_cast<ASTImpl *>(statement));
      case AST_NODE_IMPORT:
        return visit_import(static_cast<ASTImport *>(statement));
      case AST_NODE_MODULE:
        return visit_module(static_cast<ASTModule *>(statement));
      case AST_NODE_RETURN:
        return visit_return(static_cast<ASTReturn *>(statement));
      case AST_NODE_CONTINUE:
        return visit_continue(static_cast<ASTContinue *>(statement));
      case AST_NODE_BREAK:
        return visit_break(static_cast<ASTBreak *>(statement));
      case AST_NODE_FOR:
        return visit_for(static_cast<ASTFor *>(statement));

      case AST_NODE_ELSE:
        return visit_else(static_cast<ASTElse *>(statement));
      case AST_NODE_WHILE:
        return visit_while(static_cast<ASTWhile *>(statement));
      case AST_NODE_STRUCT_DECLARATION:
        return visit_struct_declaration(static_cast<ASTStructDeclaration *>(statement));
      case AST_NODE_ENUM_DECLARATION:
        return visit_enum_declaration(static_cast<ASTEnumDeclaration *>(statement));
      case AST_NODE_CHOICE_DECLARATION:
        return visit_choice_declaration(static_cast<ASTChoiceDeclaration *>(statement));
      case AST_NODE_VARIABLE:
        return visit_variable(static_cast<ASTVariable *>(statement));
      case AST_NODE_EXPR_STATEMENT:
        return visit_expr_statement(static_cast<ASTExprStatement *>(statement));
      case AST_NODE_DEFER:
        return visit_defer(static_cast<ASTDefer *>(statement));
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return visit_tuple_deconstruction(static_cast<ASTDestructure *>(statement));
      default:
        return nullptr;
    }
  }

  THIR *visit_expr(ASTExpr *expr) {
    switch (expr->get_node_type()) {
      case AST_NODE_IF:
        return visit_if(static_cast<ASTIf *>(expr));
      case AST_NODE_LAMBDA:
        return visit_lambda(static_cast<ASTLambda *>(expr));
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr(static_cast<ASTBinExpr *>(expr));
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr(static_cast<ASTUnaryExpr *>(expr));
      case AST_NODE_LITERAL:
        return visit_literal(static_cast<ASTLiteral *>(expr));
      case AST_NODE_PATH:
        return visit_path(static_cast<ASTPath *>(expr));
      case AST_NODE_TUPLE:
        return visit_tuple(static_cast<ASTTuple *>(expr));
      case AST_NODE_CALL:
        return visit_call(static_cast<ASTCall *>(expr));
      case AST_NODE_METHOD_CALL:
        return visit_method_call(static_cast<ASTMethodCall *>(expr));
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr(static_cast<ASTDotExpr *>(expr));
      case AST_NODE_INDEX:
        return visit_index(static_cast<ASTIndex *>(expr));
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list(static_cast<ASTInitializerList *>(expr));
      case AST_NODE_SIZE_OF:
        return visit_size_of(static_cast<ASTSize_Of *>(expr));
      case AST_NODE_TYPE_OF:
        return visit_type_of(static_cast<ASTType_Of *>(expr));
      case AST_NODE_DYN_OF:
        return visit_dyn_of(static_cast<ASTDyn_Of *>(expr));
      case AST_NODE_CAST:
        return visit_cast(static_cast<ASTCast *>(expr));
      case AST_NODE_RANGE:
        return visit_range(static_cast<ASTRange *>(expr));
      case AST_NODE_SWITCH:
        return visit_switch(static_cast<ASTSwitch *>(expr));
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match(static_cast<ASTPatternMatch *>(expr));
      default:
        return nullptr;
    }
  }

  std::variant<THIR *, std::nullptr_t> visit_node(ASTNode *node) {
    switch (node->get_node_type()) {
      // Expression nodes
      case AST_NODE_LAMBDA:
        return visit_lambda(static_cast<ASTLambda *>(node));
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr(static_cast<ASTBinExpr *>(node));
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr(static_cast<ASTUnaryExpr *>(node));
      case AST_NODE_LITERAL:
        return visit_literal(static_cast<ASTLiteral *>(node));
      case AST_NODE_PATH:
        return visit_path(static_cast<ASTPath *>(node));
      case AST_NODE_TUPLE:
        return visit_tuple(static_cast<ASTTuple *>(node));
      case AST_NODE_CALL:
        return visit_call(static_cast<ASTCall *>(node));
      case AST_NODE_METHOD_CALL:
        return visit_method_call(static_cast<ASTMethodCall *>(node));
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr(static_cast<ASTDotExpr *>(node));
      case AST_NODE_INDEX:
        return visit_index(static_cast<ASTIndex *>(node));
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list(static_cast<ASTInitializerList *>(node));
      case AST_NODE_SIZE_OF:
        return visit_size_of(static_cast<ASTSize_Of *>(node));
      case AST_NODE_TYPE_OF:
        return visit_type_of(static_cast<ASTType_Of *>(node));
      case AST_NODE_DYN_OF:
        return visit_dyn_of(static_cast<ASTDyn_Of *>(node));
      case AST_NODE_CAST:
        return visit_cast(static_cast<ASTCast *>(node));
      case AST_NODE_RANGE:
        return visit_range(static_cast<ASTRange *>(node));
      case AST_NODE_SWITCH:
        return visit_switch(static_cast<ASTSwitch *>(node));
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match(static_cast<ASTPatternMatch *>(node));
      // Statement nodes
      case AST_NODE_BLOCK:
        return visit_block(static_cast<ASTBlock *>(node));
      case AST_NODE_FUNCTION_DECLARATION:
        return visit_function_declaration(static_cast<ASTFunctionDeclaration *>(node));
      case AST_NODE_IMPL:
        return visit_impl(static_cast<ASTImpl *>(node));
      case AST_NODE_IMPORT:
        return visit_import(static_cast<ASTImport *>(node));
      case AST_NODE_MODULE:
        return visit_module(static_cast<ASTModule *>(node));
      case AST_NODE_RETURN:
        return visit_return(static_cast<ASTReturn *>(node));
      case AST_NODE_CONTINUE:
        return visit_continue(static_cast<ASTContinue *>(node));
      case AST_NODE_BREAK:
        return visit_break(static_cast<ASTBreak *>(node));
      case AST_NODE_FOR:
        return visit_for(static_cast<ASTFor *>(node));
      case AST_NODE_IF:
        return visit_if(static_cast<ASTIf *>(node));
      case AST_NODE_ELSE:
        return visit_else(static_cast<ASTElse *>(node));
      case AST_NODE_WHILE:
        return visit_while(static_cast<ASTWhile *>(node));
      case AST_NODE_STRUCT_DECLARATION:
        return visit_struct_declaration(static_cast<ASTStructDeclaration *>(node));
      case AST_NODE_ENUM_DECLARATION:
        return visit_enum_declaration(static_cast<ASTEnumDeclaration *>(node));
      case AST_NODE_CHOICE_DECLARATION:
        return visit_choice_declaration(static_cast<ASTChoiceDeclaration *>(node));
      case AST_NODE_VARIABLE:
        return visit_variable(static_cast<ASTVariable *>(node));
      case AST_NODE_EXPR_STATEMENT:
        return visit_expr_statement(static_cast<ASTExprStatement *>(node));
      case AST_NODE_DEFER:
        return visit_defer(static_cast<ASTDefer *>(node));
      case AST_NODE_TUPLE_DECONSTRUCTION:
        return visit_tuple_deconstruction(static_cast<ASTDestructure *>(node));
      default:
        return nullptr;
    }
  }
};