#include "mir.hpp"
#include "thir.hpp"
#include "type.hpp"

namespace Mir {
void generate_module(const THIRFunction *entry_point, Module &m) { generate_function(entry_point, m); }

Operand generate_function(const THIRFunction *node, Module &m) {
  uint32_t index;
  Function *f = m.create_function(node->name, node->type, index);
  m.enter_function(f);
  generate_block(node->block, m);
  Basic_Block *insert_block = f->get_insert_block();
  if (f->type_info->return_type == void_type() && insert_block->back_opcode() != OP_RET_VOID) {
    EMIT_OP(OP_RET_VOID);
  }
  m.leave_function();
  return Operand::Temp(index, node->type);
}

void generate_block(const THIRBlock *node, Module &m) {
  m.create_basic_block();
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
  }
}

Operand load_variable(const THIRVariable *node, Module &m) {
  auto it = m.variables.find(node);
  assert(it != m.variables.end() && "variable not declared");
  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, it->second);
  return result;
}

void generate_variable(const THIRVariable *node, Module &m) {
  Operand dest = m.current_function->create_temporary(node->type);
  EMIT_ALLOCA(dest, Operand::Ty(node->type));
  EMIT_STORE(dest, generate_expr(node->value, m));
  m.variables[node] = dest;
}

Operand generate_lvalue_addr(const THIR *node, Module &m) {
  switch (node->get_node_type()) {
    case THIRNodeType::Variable: {
      const THIRVariable *var = (const THIRVariable *)node;
      auto it = m.variables.find(var);
      assert(it != m.variables.end() && "variable not declared");
      return it->second;  // This is already the address (from alloca)
    }
    case THIRNodeType::MemberAccess: {
      return generate_member_access_addr((const THIRMemberAccess *)node, m);
    }
    case THIRNodeType::Index: {
      return generate_index_addr((const THIRIndex *)node, m);
    }
    case THIRNodeType::UnaryExpr: {
      const THIRUnaryExpr *unary = (const THIRUnaryExpr *)node;
      if (unary->op == TType::Mul) {
        return generate_expr(unary->operand, m);
      }
    }
    default:
      assert(false && "not an lvalue");
      return Operand::Null();
  }
}

Operand generate_member_access_addr(const THIRMemberAccess *node, Module &m) {
  Operand base_addr = generate_lvalue_addr(node->base, m);
  Operand result = m.current_function->create_temporary(node->type->take_pointer_to());
  Type *base = node->base->type;
  uint32_t index = base->info->index_of_member(node->member);
  Operand field_index = Operand::Imm(new_int(index), u32_type());

  EMIT_GEP(result, base_addr, field_index);
  return result;
}

Operand generate_index_addr(const THIRIndex *node, Module &m) {
  Operand base_addr = generate_lvalue_addr(node->base, m);
  Operand index = generate_expr(node->index, m);
  Operand result = m.current_function->create_temporary(node->type->take_pointer_to(true));
  EMIT_GEP(result, base_addr, index);
  return result;
}

Operand generate_bin_expr(const THIRBinExpr *node, Module &m) {
  if (node->op == TType::Assign || node->op == TType::CompAdd || node->op == TType::CompSub || node->op == TType::CompMul ||
      node->op == TType::CompDiv || node->op == TType::CompMod || node->op == TType::CompAnd || node->op == TType::CompOr ||
      node->op == TType::CompXor || node->op == TType::CompSHL || node->op == TType::CompSHR) {
    Operand lvalue_addr = generate_lvalue_addr(node->left, m);
    Operand rvalue = generate_expr(node->right, m);

    if (node->op == TType::Assign) {
      EMIT_STORE(lvalue_addr, rvalue);
      return rvalue;
    } else {
      Operand current_val = m.current_function->create_temporary(node->left->type);
      EMIT_LOAD(current_val, lvalue_addr);
      Operand result = m.current_function->create_temporary(node->type);

      Op_Code op;
      switch (node->op) {
        case TType::CompAdd:
          op = OP_ADD;
          break;
        case TType::CompSub:
          op = OP_SUB;
          break;
        case TType::CompMul:
          op = OP_MUL;
          break;
        case TType::CompDiv:
          op = OP_DIV;
          break;
        case TType::CompMod:
          op = OP_MOD;
          break;
        case TType::CompAnd:
          op = OP_AND;
          break;
        case TType::CompOr:
          op = OP_OR;
          break;
        case TType::CompXor:
          op = OP_XOR;
          break;
        case TType::CompSHL:
          op = OP_SHL;
          break;
        case TType::CompSHR:
          op = OP_SHR;
          break;
        default:
          assert(false && "unknown compound assignment");
      }

      EMIT_BINOP(op, result, current_val, rvalue);
      EMIT_STORE(lvalue_addr, result);
      return result;
    }
  }

  Operand left = generate_expr(node->left, m);
  Operand right = generate_expr(node->right, m);
  Operand result = m.current_function->create_temporary(node->type);

  Op_Code op;
  switch (node->op) {
    case TType::Add:
      op = OP_ADD;
      break;
    case TType::Sub:
      op = OP_SUB;
      break;
    case TType::Mul:
      op = OP_MUL;
      break;
    case TType::Div:
      op = OP_DIV;
      break;
    case TType::Modulo:
      op = OP_MOD;
      break;
    case TType::And:
      op = OP_AND;
      break;
    case TType::Or:
      op = OP_OR;
      break;
    case TType::Xor:
      op = OP_XOR;
      break;
    case TType::SHL:
      op = OP_SHL;
      break;
    case TType::SHR:
      op = OP_SHR;
      break;
    case TType::LogicalAnd:
      op = OP_LOGICAL_AND;
      break;
    case TType::LogicalOr:
      op = OP_LOGICAL_OR;
      break;
    case TType::EQ:
      op = OP_EQ;
      break;
    case TType::NEQ:
      op = OP_NE;
      break;
    case TType::LT:
      op = OP_LT;
      break;
    case TType::LE:
      op = OP_LE;
      break;
    case TType::GT:
      op = OP_GT;
      break;
    case TType::GE:
      op = OP_GE;
      break;
    default:
      assert(false && "unknown binary operator");
  }

  EMIT_BINOP(op, result, left, right);
  return result;
}

Operand generate_unary_expr(const THIRUnaryExpr *node, Module &m) {
  Op_Code op;
  switch (node->op) {
    case TType::Not:
      op = OP_NOT;
      break;
    case TType::LogicalNot:
      op = OP_LOGICAL_NOT;
      break;
    case TType::Sub:
      op = OP_NEG;
      break;
    case TType::And: {
      return generate_lvalue_addr(node->operand, m);
    }
    case TType::Mul: {
      Operand ptr = generate_expr(node->operand, m);
      Operand result = m.current_function->create_temporary(node->type);
      EMIT_LOAD(result, ptr);
      return result;
    }
    default:
      assert(false && "unknown unary operator");
  }

  Operand operand = generate_expr(node->operand, m);
  Operand result = m.current_function->create_temporary(node->type);
  EMIT_UNOP(op, result, operand);
  return result;
}

Operand generate_expr_block(const THIRExprBlock *node, Module &m) {
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
  }
  return load_variable(node->return_register, m);
}

Operand generate_literal(const THIRLiteral *node, Module &) {
  Value *value;
  switch (node->tag) {
    case ASTLiteral::Integer:
      value = new_int(node->value);
      break;
    case ASTLiteral::Float:
      value = new_float(node->value);
      break;
    case ASTLiteral::Bool:
      value = new_bool(node->value);
      break;
    case ASTLiteral::Char:
      value = new_char(node->value.get_str()[0]);
      break;
    case ASTLiteral::String:
    case ASTLiteral::MultiLineString:
      value = new_string(node->value);
      break;
    default:
      assert(false && "invalid literal");
      break;
  }
  return Operand::Imm(value, node->type);
}

Operand generate_call(const THIRCall *node, Module &m) {
  for (const THIR *arg : node->arguments) {
    Operand arg_operand = generate_expr(arg, m);
    EMIT_PUSH_ARG(arg_operand);
  }
  
  Operand result = m.current_function->create_temporary(node->type);
  Operand fn_operand = generate_expr(node->callee, m);
  Operand arg_count = Operand::Imm(new_int(node->arguments.size()), u32_type());

  EMIT_CALL(result, fn_operand, arg_count);
  return result;
}

Operand generate_member_access(const THIRMemberAccess *node, Module &m) {
  Operand addr = generate_member_access_addr(node, m);
  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, addr);
  return result;
}

Operand generate_cast(const THIRCast *node, Module &m) {
  Operand value = generate_expr(node->operand, m);
  Operand result = m.current_function->create_temporary(node->type);
  Operand type_operand = Operand::Ty(node->type);

  EMIT_CAST(result, value, type_operand);
  return result;
}

Operand generate_index(const THIRIndex *node, Module &m) {
  Operand addr = generate_index_addr(node, m);
  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, addr);
  return result;
}

Operand generate_aggregate_initializer(const THIRAggregateInitializer *node, Module &m) {
  Operand dest = m.current_function->create_temporary(node->type);
  EMIT_ALLOCA(dest, Operand::Ty(node->type));

  for (const auto &kv : node->key_values) {
    Type *base = node->type;
    uint32_t field_index = base->info->index_of_member(kv.first);

    Operand field_addr = m.current_function->create_temporary(kv.second->type->take_pointer_to());
    Operand field_index_op = Operand::Imm(new_int(field_index), u32_type());
    EMIT_GEP(field_addr, dest, field_index_op);

    Operand field_value = generate_expr(kv.second, m);
    EMIT_STORE(field_addr, field_value);
  }

  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
}

Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m) {
  Operand dest = m.current_function->create_temporary(node->type);
  EMIT_ALLOCA(dest, Operand::Ty(node->type));

  for (size_t i = 0; i < node->values.size(); i++) {
    Operand element_addr = m.current_function->create_temporary(node->values[i]->type->take_pointer_to());
    Operand element_index = Operand::Imm(new_int(i), u32_type());
    EMIT_GEP(element_addr, dest, element_index);

    Operand element_value = generate_expr(node->values[i], m);
    EMIT_STORE(element_addr, element_value);
  }

  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
}

Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m) {
  Operand dest = m.current_function->create_temporary(node->type);
  EMIT_ALLOCA(dest, Operand::Ty(node->type));

  Operand result = m.current_function->create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
}

void generate_return(const THIRReturn *, Module &) {}
void generate_break(const THIRBreak *, Module &) {}
void generate_continue(const THIRContinue *, Module &) {}
void generate_for(const THIRFor *, Module &) {}
void generate_if(const THIRIf *, Module &) {}
void generate_while(const THIRWhile *, Module &) {}
void generate_noop(const THIRNoop *, Module &) {}

}  // namespace Mir