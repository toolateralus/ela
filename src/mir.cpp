#include "mir.hpp"
#include <cstdio>
#include "error.hpp"
#include "strings.hpp"
#include "thir.hpp"
#include "type.hpp"

namespace Mir {
static std::vector<Basic_Block *> g_break_targets;
static std::vector<Basic_Block *> g_continue_targets;

static bool block_only_contains_noop(const THIRBlock *b) {
  size_t count = 0;
  for (const auto *stmt : b->statements) {
    if (stmt->get_node_type() == THIRNodeType::Noop) {
      count += 1;
    } else {
      break;
    }
  }
  return count == b->statements.size();
}

void generate_module(const THIRFunction *entry_point, Module &m) { generate_function(entry_point, m); }

void convert_function_flags(const THIRFunction *t, Function *f) {
  uint8_t flags = Function::FUNCTION_FLAGS_NONE;
  if (t->is_inline) {
    flags |= Function::FUNCTION_FLAGS_IS_INLINE;
  }
  if (t->is_varargs) {
    flags |= Function::FUNCTION_FLAGS_IS_VAR_ARGS;
  }
  if (t->is_extern) {
    flags |= Function::FUNCTION_FLAGS_IS_EXTERN;
  }
  if (t->is_entry) {
    flags |= Function::FUNCTION_FLAGS_IS_ENTRY_POINT;
  }
  if (t->is_exported) {
    flags |= Function::FUNCTION_FLAGS_IS_EXPORTED;
  }
  if (t->is_test) {
    flags |= Function::FUNCTION_FLAGS_IS_TEST;
  }
  f->flags = flags;
}

Operand generate_function(const THIRFunction *node, Module &m) {
  auto it = m.function_table.find(node->name);

  if (it != m.function_table.end()) {
    return Operand::Make_Temp(it->second->index, it->second->type);
  }

  uint32_t index;
  Function *f = m.create_function(node, index);

  convert_function_flags(node, f);

  // TODO: figure out how we're going to do externs.
  // probably just need an extern section
  if (node->is_extern) {
    return Operand::Make_Temp(index, node->type);
  }
  m.enter_function(f);

  m.create_basic_block("entry");
  generate_block(node->block, m);
  Basic_Block *insert_block = f->get_insert_block();
  if (f->type_info->return_type == void_type() && (!insert_block->code.size() || insert_block->back_opcode() != OP_RET_VOID)) {
    EMIT_OP(OP_RET_VOID);
  }
  m.leave_function();
  return Operand::Make_Temp(index, node->type);
}

void generate_block(const THIRBlock *node, Module &m) {
  if (node->statements.empty() || block_only_contains_noop(node)) {
    return;
  }
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
  }
}

Operand load_variable(const THIRVariable *node, Module &m) {
  Operand result = m.create_temporary(node->type);

  if (node->is_global || node->is_static) {
    auto g_var = m.global_variable_table[node];
    EMIT_LOAD(result, Operand::Make_Global_Ref(g_var));
    return result;
  }

  auto it = m.variables.find(node);
  if (it == m.variables.end()) {
    throw_error(std::format("variable '{}' not declared", node->name.get_str()), node->span);
  }
  EMIT_LOAD(result, it->second);
  return result;
}

void generate_variable(const THIRVariable *node, Module &m) {
  // static locals is just syntax sugar for a scoped global
  if (node->is_global || node->is_static) {  
    Global_Variable global = {.name = node->name, .type = node->type, .has_external_linkage = node->is_global};
    Global_Variable *gv = mir_arena.construct<Global_Variable>(global);
    m.global_variables.push_back(gv);
    m.global_variable_table[node] = gv;
    // TODO; figure out how the fuck we're going to make global initializers.
    return;
  }

  Operand dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));

  Operand *old_alloca = m.current_alloca;
  m.current_alloca = &dest;

  Operand value_temp = generate_expr(node->value, m);

  // If we took advantage of the pre-existing alloca thing with m.current alloca,
  // we wrote directly into the variables storage, so a store here would be redundant
  if (value_temp.tag != Operand::OPERAND_TEMP || value_temp.temp != dest.temp) {
    EMIT_STORE(dest, value_temp);
  }

  m.variables[node] = dest;
  m.current_alloca = old_alloca;
}

Operand generate_lvalue_addr(const THIR *node, Module &m) {
  switch (node->get_node_type()) {
    case THIRNodeType::Variable: {
      const THIRVariable *var = (const THIRVariable *)node;

      if (var->is_global || var->is_static) {
        auto it = m.global_variable_table.find(var);
        if (it == m.global_variable_table.end()) {
          // lazily visit global variables.
          generate_variable(var, m);
          it = m.global_variable_table.find(var);
          if (it == m.global_variable_table.end()) {
            throw_error(std::format("global variable '{}' failed to lazy init", var->name.get_str()), var->span);
          }
        }
        Operand result = m.create_temporary(var->type->take_pointer_to());
        EMIT_LOAD(result, Operand::Make_Global_Ref(it->second));
        return result;
      }

      auto it = m.variables.find(var);
      if (it == m.variables.end()) {
        throw_error(std::format("variable '{}' not declared", var->name.get_str()), var->span);
      }
      return it->second;
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
    case THIRNodeType::Function: {
      // we have to assume this is a function pointer, otherwise it wouldn't have made it here.
      Operand temp = m.create_temporary(node->type->take_pointer_to());
      Operand fn_index = generate_function((THIRFunction *)node, m);
      EMIT_LOAD_FN_PTR(temp, fn_index);
      return temp;
    }
    default:
      throw_error(std::format("not an lvalue (node type {})", node_type_to_string(node->get_node_type())), node->span);
      return Operand::MakeNull();
  }
}

Operand generate_member_access_addr(const THIRMemberAccess *node, Module &m) {
  Operand base_addr = generate_lvalue_addr(node->base, m);
  Operand result = m.create_temporary(node->type->take_pointer_to());
  Type *base = node->base->type;

  size_t index = 0;

  if (node->member != CHOICE_TYPE_DISCRIMINANT_KEY) {
    if (!base->try_get_index_of_member(node->member, index)) {
      throw_error(std::format("unable to find index of member: {}", node->member.get_str()), node->span);
    }
  }

  Operand field_index = Operand::Make_Imm(Constant::Int(index), u32_type());

  EMIT_GEP(result, base_addr, field_index);
  return result;
}

Operand generate_index_addr(const THIRIndex *node, Module &m) {
  Operand base_addr = generate_lvalue_addr(node->base, m);
  Operand index = generate_expr(node->index, m);
  Operand result = m.create_temporary(node->type->take_pointer_to(true));
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
      Operand current_val = m.create_temporary(node->left->type);
      EMIT_LOAD(current_val, lvalue_addr);
      Operand result = m.create_temporary(node->type);

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
          throw_error(std::format("unknown compound assignment operator {}", (int)node->op), node->span);
          exit(1);
      }

      EMIT_BINOP(op, result, current_val, rvalue);
      EMIT_STORE(lvalue_addr, result);
      return result;
    }
  }

  Operand left = generate_expr(node->left, m);
  Operand right = generate_expr(node->right, m);
  Operand result = m.create_temporary(node->type);

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
      throw_error(std::format("unknown binary operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
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
      Operand result = m.create_temporary(node->type);
      EMIT_LOAD(result, ptr);
      return result;
    }
    case TType::Increment: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand current_val = m.create_temporary(node->type);
      EMIT_LOAD(current_val, addr);
      Operand result = m.create_temporary(node->type);
      EMIT_BINOP(OP_ADD, result, current_val, Operand::Make_Imm(Constant::Int(1), node->type));
      EMIT_STORE(addr, result);
      return result;
    }
    case TType::Decrement: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand current_val = m.create_temporary(node->type);
      EMIT_LOAD(current_val, addr);
      Operand result = m.create_temporary(node->type);
      EMIT_BINOP(OP_SUB, result, current_val, Operand::Make_Imm(Constant::Int(1), node->type));
      EMIT_STORE(addr, result);
      return result;
    }
    default:
      throw_error(std::format("unknown unary operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
  }

  Operand operand = generate_expr(node->operand, m);
  Operand result = m.create_temporary(node->type);
  EMIT_UNARY(op, result, operand);
  return result;
}

Operand generate_expr_block(const THIRExprBlock *node, Module &m) {
  generate_variable(node->return_register, m);
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
  }
  return load_variable(node->return_register, m);
}

Operand generate_literal(const THIRLiteral *node, Module &) {
  Constant value;
  switch (node->tag) {
    case ASTLiteral::Integer:
      value = Constant::Int(atoll(node->value.get_str().c_str()));
      break;
    case ASTLiteral::Float:
      value = Constant::Float(atof(node->value.get_str().c_str()));
      break;
    case ASTLiteral::Bool:
      value = Constant::Bool(node->value == "true" ? true : false);
      break;
    case ASTLiteral::Char:
      value = Constant::Char(node->value.get_str()[0]);  // TODO: handle utf8
      break;
    case ASTLiteral::String:
    case ASTLiteral::MultiLineString:
      value = Constant::String(node->value);
      break;
    case ASTLiteral::Null:
      value = Constant::Int(0);
      break;
  }
  return Operand::Make_Imm(value, node->type);
}

Operand generate_call(const THIRCall *node, Module &m) {
  for (const THIR *arg : node->arguments) {
    Operand arg_operand = generate_expr(arg, m);
    EMIT_PUSH_ARG(arg_operand);
  }

  Operand result = m.create_temporary(node->type);
  Operand fn_operand = generate_expr(node->callee, m);
  Operand arg_count = Operand::Make_Imm(Constant::Int(node->arguments.size()), u32_type());

  if (node->callee->type->is_pointer()) {
    EMIT_CALL_PTR(result, fn_operand, arg_count);
  } else {
    EMIT_CALL(result, fn_operand, arg_count);
  }

  return result;
}

Operand generate_member_access(const THIRMemberAccess *node, Module &m) {
  Operand addr = generate_member_access_addr(node, m);
  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, addr);
  return result;
}

Operand generate_cast(const THIRCast *node, Module &m) {
  Operand value = generate_expr(node->operand, m);
  Operand result = m.create_temporary(node->type);
  Operand type_operand = Operand::Make_Type_Ref(node->type);
  EMIT_CAST(result, value, type_operand);
  return result;
}

Operand generate_index(const THIRIndex *node, Module &m) {
  Operand addr = generate_index_addr(node, m);
  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, addr);
  return result;
}

Operand generate_aggregate_initializer(const THIRAggregateInitializer *node, Module &m) {
  Operand dest = Operand::MakeNull();
  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  } else {
    // Reuse a variables alloca so we don't have to double allocate.
    used_pre_existing_alloca = true;
    dest = *m.current_alloca;
  }

  for (const auto &[key, value] : node->key_values) {
    Type *base = node->type;
    size_t field_index = 0;
    if (key != CHOICE_TYPE_DISCRIMINANT_KEY) {
      if (!base->try_get_index_of_member(key, field_index)) {
        throw_error(std::format("unable to find index of member: {}", key.get_str()), node->span);
      }
    }

    Operand field_addr = m.create_temporary(value->type->take_pointer_to());
    Operand field_index_op = Operand::Make_Imm(Constant::Int(field_index), u32_type());
    EMIT_GEP(field_addr, dest, field_index_op);

    Operand field_value = generate_expr(value, m);
    EMIT_STORE(field_addr, field_value);
  }

  // The consumer of the pre existing alloca will load, this prevents an unneccesary double load.
  if (!used_pre_existing_alloca) {
    Operand result = m.create_temporary(node->type);
    EMIT_LOAD(result, dest);
  }
  return dest;
}

Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m) {
  Operand dest = Operand::MakeNull();
  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  } else {
    used_pre_existing_alloca = true;
    dest = *m.current_alloca;
  }

  for (size_t i = 0; i < node->values.size(); i++) {
    Operand element_addr = m.create_temporary(node->values[i]->type->take_pointer_to());
    Operand element_index = Operand::Make_Imm(Constant::Int(i), u32_type());
    EMIT_GEP(element_addr, dest, element_index);

    Operand element_value = generate_expr(node->values[i], m);
    EMIT_STORE(element_addr, element_value);
  }

  Operand result = m.create_temporary(node->type);

  if (!used_pre_existing_alloca) {
    EMIT_LOAD(result, dest);
  }

  return result;
}

Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m) {
  Operand dest = Operand::MakeNull();

  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  } else {
    used_pre_existing_alloca = true;
    dest = *m.current_alloca;
  }

  Operand result = m.create_temporary(node->type);

  if (!used_pre_existing_alloca) {
    EMIT_LOAD(result, dest);
  }
  return result;
}

void generate_return(const THIRReturn *node, Module &m) {
  if (!node->expression || node->expression == THIRNoop::shared()) {
    EMIT_RET_VOID();
    return;
  }
  Operand val = generate_expr(node->expression, m);
  EMIT_RET(val);
}

void generate_break(const THIRBreak *node, Module &m) {
  if (g_break_targets.empty()) {
    throw_error("break used outside of loop", node->span);
  }
  Basic_Block *target = g_break_targets.back();
  m.current_function->get_insert_block()->push(Instruction{OP_JMP, Operand(), Operand ::Make_BB(target), .span = node->span});
}

void generate_continue(const THIRContinue *node, Module &m) {
  if (g_continue_targets.empty()) {
    throw_error("continue used outside of loop", node->span);
  }
  Basic_Block *target = g_continue_targets.back();
  EMIT_JUMP(target);
}

void generate_for(const THIRFor *node, Module &m) {
  generate(node->initialization, m);

  // remember the block where initialization finished
  Basic_Block *orig_bb = m.get_insert_block();

  Basic_Block *cond_bb = m.create_basic_block("for");
  Basic_Block *body_bb = m.create_basic_block("do");
  Basic_Block *incr_bb = m.create_basic_block("incr");
  Basic_Block *after_bb = m.create_basic_block("done");

  // jump from original to cond
  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(cond_bb);

  // cond
  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(body_bb, cond);
  EMIT_JUMP(after_bb);

  // body
  m.current_function->set_insert_block(body_bb);
  g_break_targets.push_back(after_bb);
  g_continue_targets.push_back(incr_bb);

  generate(node->block, m);

  g_break_targets.pop_back();
  g_continue_targets.pop_back();
  EMIT_JUMP(incr_bb);

  // incr
  m.current_function->set_insert_block(incr_bb);
  generate(node->increment, m);

  EMIT_JUMP(cond_bb);

  // after
  m.current_function->set_insert_block(after_bb);
}

void generate_if(const THIRIf *node, Module &m) {
  Operand cond = generate_expr(node->condition, m);

  Basic_Block *cond_bb = m.get_insert_block();
  Basic_Block *then_bb = m.create_basic_block("then");
  Basic_Block *else_bb = node->_else ? m.create_basic_block("else") : nullptr;
  Basic_Block *after_bb = m.create_basic_block("end");

  m.current_function->set_insert_block(cond_bb);
  EMIT_JUMP_TRUE(then_bb, cond);
  if (else_bb) {
    EMIT_JUMP(else_bb);
  } else {
    EMIT_JUMP(after_bb);
  }

  m.current_function->set_insert_block(then_bb);
  generate(node->block, m);
  EMIT_JUMP(after_bb);

  if (else_bb) {
    m.current_function->set_insert_block(else_bb);
    generate(node->_else, m);
    EMIT_JUMP(after_bb);
  }

  m.current_function->set_insert_block(after_bb);
}

void generate_while(const THIRWhile *node, Module &m) {
  Basic_Block *orig_bb = m.get_insert_block();
  Basic_Block *cond_bb = m.create_basic_block("while");
  Basic_Block *body_bb = m.create_basic_block("do");
  Basic_Block *after_bb = m.create_basic_block("done");

  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(body_bb, cond);
  EMIT_JUMP(after_bb);

  m.current_function->set_insert_block(body_bb);
  g_break_targets.push_back(after_bb);
  g_continue_targets.push_back(cond_bb);
  generate(node->block, m);
  g_break_targets.pop_back();
  g_continue_targets.pop_back();
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(after_bb);
}

Operand Operand::Make_Global_Ref(Global_Variable *gv_ref) {
  Operand o;
  o.gv = gv_ref;
  o.type = gv_ref->type;
  o.tag = OPERAND_GLOBAL_VARIABLE_REFERENCE;
  return o;
}
}  // namespace Mir