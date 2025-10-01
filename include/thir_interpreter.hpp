#pragma once
#include <unordered_map>
#include "interned_string.hpp"
#include "type.hpp"
#include "thir.hpp"
#include "value.hpp"

struct Context;
struct ASTExpr;

struct ASTNode;
Value *interpret(THIR *node, Context &ctx);
Value *interpret_from_ast(ASTNode *node, Context &ctx);

struct Interpreter {
  std::unordered_map<InternedString, ExternFunctionValue> extern_functions{};
  Scope *root_scope;
  Scope *scope;
  Context &ctx;

  Interpreter(Context &context) : ctx(context) {
    root_scope = create_child(ctx.scope);
    scope = root_scope;
  }

  Value *try_link_extern_function(THIRFunction *function);
  LValue *get_lvalue(THIR *);
  LValue *get_member_access_lvalue(THIRMemberAccess *);
  LValue *get_index_lvalue(THIRIndex *);
  LValue *get_unary_lvalue(THIRUnaryExpr *);
  LValue *get_variable_lvalue(THIRVariable *);
  Value *visit_block(THIRBlock *);
  Value *visit_bin_expr(THIRBinExpr *);
  Value *visit_unary_expr(THIRUnaryExpr *);
  Value *visit_literal(THIRLiteral *);
  Value *visit_call(THIRCall *);
  Value *visit_return(THIRReturn *);
  Value *visit_index(THIRIndex *);
  Value *visit_cast(THIRCast *);
  Value *visit_function(THIRFunction *);
  Value *visit_variable(THIRVariable *);
  Value *visit_continue(THIRContinue *);
  Value *visit_break(THIRBreak *);
  Value *visit_for(THIRFor *);
  Value *visit_if(THIRIf *);
  Value *visit_while(THIRWhile *);
  
  Value *visit_expression_block(THIRExprBlock *);
  Value *visit_member_access(THIRMemberAccess *);
  Value *visit_aggregate_initializer(THIRAggregateInitializer *);
  Value *visit_collection_initializer(THIRCollectionInitializer *);

  Value *visit_empty_initializer(THIREmptyInitializer *node) {
    if (node->type->is_fixed_sized_array()) {
      return new_array(node->type);
    }
    return new_object(node->type);
  }

  Value *visit_offset_of(THIROffsetOf *node) {
    auto type = node->target_type;
    auto field = node->target_field;
    return new_int(type->offset_in_bytes(field));
  }

  // don't need to declare types.
  Value *visit_type_node(THIRType *) {
    return null_value();
  }

  Value *visit_noop(THIRNoop *) {
    return null_value();
  };

  Value *visit_node(THIR *node) {
    switch (node->get_node_type()) {
      case THIRNodeType::Program:
        throw_error("compile time interpreter got a program node.. this is certainly a bug.", node->source_range);
        return nullptr;
      case THIRNodeType::Block:
        return visit_block(static_cast<THIRBlock *>(node));
      case THIRNodeType::Variable:
        return visit_variable(static_cast<THIRVariable *>(node));
      case THIRNodeType::Function:
        return visit_function(static_cast<THIRFunction *>(node));
      case THIRNodeType::Type:
        return visit_type_node(static_cast<THIRType *>(node));
      case THIRNodeType::ExpressionBlock:
        return visit_expression_block(static_cast<THIRExprBlock *>(node));
      case THIRNodeType::BinExpr:
        return visit_bin_expr(static_cast<THIRBinExpr *>(node));
      case THIRNodeType::UnaryExpr:
        return visit_unary_expr(static_cast<THIRUnaryExpr *>(node));
      case THIRNodeType::Literal:
        return visit_literal(static_cast<THIRLiteral *>(node));
      case THIRNodeType::Call:
        return visit_call(static_cast<THIRCall *>(node));
      case THIRNodeType::MemberAccess:
        return visit_member_access(static_cast<THIRMemberAccess *>(node));
      case THIRNodeType::Cast:
        return visit_cast(static_cast<THIRCast *>(node));
      case THIRNodeType::Index:
        return visit_index(static_cast<THIRIndex *>(node));
      case THIRNodeType::AggregateInitializer:
        return visit_aggregate_initializer(static_cast<THIRAggregateInitializer *>(node));
      case THIRNodeType::CollectionInitializer:
        return visit_collection_initializer(static_cast<THIRCollectionInitializer *>(node));
      case THIRNodeType::EmptyInitializer:
        return visit_empty_initializer(static_cast<THIREmptyInitializer *>(node));
      case THIRNodeType::Offset_Of:
        return visit_offset_of(static_cast<THIROffsetOf *>(node));
      case THIRNodeType::Return:
        return visit_return(static_cast<THIRReturn *>(node));
      case THIRNodeType::Break:
        return visit_break(static_cast<THIRBreak *>(node));
      case THIRNodeType::Continue:
        return visit_continue(static_cast<THIRContinue *>(node));
      case THIRNodeType::For:
        return visit_for(static_cast<THIRFor *>(node));
      case THIRNodeType::If:
        return visit_if(static_cast<THIRIf *>(node));
      case THIRNodeType::While:
        return visit_while(static_cast<THIRWhile *>(node));
      case THIRNodeType::Noop:
        return visit_noop(static_cast<THIRNoop *>(node));
    }
  }

};
