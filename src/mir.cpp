#include "mir.hpp"
#include <cstdio>
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
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
  Function *f = mir_arena.construct<Function>();
  index = (uint32_t)m.functions.size();
  f->name = node->name;
  f->type_info = node->type->info->as<FunctionTypeInfo>();
  f->type = node->type;
  f->index = index;
  f->span = node->span;

  m.enter_function(f);
  Defer _defer([&] { 
    if (m.function_stack.size()) { // we never push the first function
      m.leave_function(); 
    }
  });

  if (!node->is_extern) {
    f->create_and_enter_basic_block("entry");
  }

  for (const auto &param : node->parameters) {
    Operand parameter_temp = m.create_temporary(param.associated_variable->type);
    f->parameter_temps.push_back({
        .name = std::format("t{}: {}", parameter_temp.temp, param.name),
        .type = parameter_temp.type,
        .index = parameter_temp.temp
    });
    Type *type = param.associated_variable->type;
    if (!node->is_extern) {
      // We just make allocas and store the initial values of parameters for all parameters,
      // this way we can take the address of a parameter, and mutate them.
      Operand alloca_temp = m.create_temporary(type->take_pointer_to());
      EMIT_ALLOCA(alloca_temp, Operand ::Make_Type_Ref(type));
      EMIT_STORE(alloca_temp, parameter_temp);
      m.variables[param.associated_variable] = alloca_temp;
    } else {
      m.variables[param.associated_variable] = parameter_temp;
    }
  }

  m.functions.push_back(f);
  m.function_table[node->name] = f;

  convert_function_flags(node, f);

  if (node->is_extern) {
    return Operand::Make_Temp(index, node->type);
  }

  generate_block(node->block, m);

  Basic_Block *end = f->basic_blocks.back();
  if (f->type_info->return_type == void_type() && (end->code.empty() || end->back_opcode() != OP_RET_VOID)) {
    m.current_function->set_insert_block(end);
    EMIT_OP(OP_RET_VOID);
  }
  return Operand::Make_Temp(index, node->type);
}

Operand load_variable(const THIRVariable *node, Module &m) {
  if (node->is_global || node->is_static) {
    Operand result = m.create_temporary(node->type);

    auto it = m.global_variable_table.find(node);

    if (it == m.global_variable_table.end()) {
      generate_variable(node, m);
      it = m.global_variable_table.find(node);
      assert(it != m.global_variable_table.end());
    }

    Global_Variable *g_var = it->second;
    EMIT_LOAD(result, Operand::Make_Global_Ref(g_var));
    return result;
  }

  auto it = m.variables.find(node);
  if (it == m.variables.end()) {
    throw_error(std::format("variable '{}' not declared", node->name.str()), node->span);
  }

  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, it->second);
  return result;
}

Operand generate_variable(const THIRVariable *node, Module &m) {
  // static locals is just syntax sugar for a scoped global
  if (node->is_global || node->is_static) {
    // we take a pointer to it because we have to load from this.
    Global_Variable global = {.name = node->name, .type = node->type->take_pointer_to(), .has_external_linkage = node->is_global};
    Global_Variable *gv = mir_arena.construct<Global_Variable>(global);
    m.global_variables.push_back(gv);
    m.global_variable_table[node] = gv;
    return Operand::MakeNull();
  }

  Operand dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));

  Operand value_temp = generate_expr(node->value, m);

  // If we took advantage of the pre-existing alloca thing with m.current alloca,
  // we wrote directly into the variables storage, so a store here would be redundant
  if (value_temp.tag != Operand::OPERAND_TEMP || value_temp.temp != dest.temp) {
    EMIT_STORE(dest, value_temp);
  }

  m.variables[node] = dest;

  return dest;
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
            throw_error(std::format("global variable '{}' failed to lazy init", var->name.str()), var->span);
          }
        }
        return Operand::Make_Global_Ref(it->second);
      }

      auto it = m.variables.find(var);
      if (it == m.variables.end()) {
        throw_error(std::format("variable '{}' not declared", var->name.str()), var->span);
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
  Operand base_addr;
  if (node->base->type->is_pointer()) {
    base_addr = generate_expr(node->base, m);
  } else {
    base_addr = generate_lvalue_addr(node->base, m);
  }

  Operand result = m.create_temporary(node->type->take_pointer_to());
  Type *base = node->base->type;

  size_t index = 0;  // choice discriminant is always offset 0

  if (node->member != CHOICE_TYPE_DISCRIMINANT_KEY) {
    if (!base->try_get_index_of_member(node->member, index)) {
      throw_error(std::format("unable to find index of member: {}", node->member.str()), node->span);
    }
  }

  const Operand member_idx = Operand::Make_Imm(Constant::Int(index), u32_type());

  EMIT_GEP(result, base_addr, member_idx);
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
  if (node->op == TType::Assign || ttype_is_comp_assign(node->op)) {
    Operand lvalue_addr = generate_lvalue_addr(node->left, m);
    Operand rvalue = generate_expr(node->right, m);

    if (node->op == TType::Assign) {
      EMIT_STORE(lvalue_addr, rvalue);
      return rvalue;
    } else {
      Operand lvalue_addr = generate_lvalue_addr(node->left, m);
      Operand rvalue = generate_expr(node->right, m);

      if (node->op == TType::Assign) {
        EMIT_STORE(lvalue_addr, rvalue);
        return rvalue;
      } else {
        Operand current_val;
        current_val = m.create_temporary(node->left->type);
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
            op = (Op_Code)-1;
            throw_error(std::format("unknown compound assignment operator {}", (int)node->op), node->span);
        }

        EMIT_BINOP(op, result, current_val, rvalue);
        EMIT_STORE(lvalue_addr, result);
        return result;
      }
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
    case TType::And: {  // address of just doesn't load the reference to this
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
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
  }
  return load_variable(node->return_register, m);
}

Operand generate_literal(const THIRLiteral *node, Module &) {
  Constant value;
  switch (node->tag) {
    case ASTLiteral::Integer:
      value = Constant::Int(atoll(node->value.c_str()));
      break;
    case ASTLiteral::Float:
      value = Constant::Float(atof(node->value.c_str()));
      break;
    case ASTLiteral::Bool:
      value = Constant::Bool(node->value == "true" ? true : false);
      break;
    case ASTLiteral::Char:
      value = Constant::Char(node->value.str()[0]);  // TODO: handle utf8
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

  Operand result = Operand::MakeNull();
  if (node->type != void_type()) {
    result = m.create_temporary(node->type);
  }
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
  // Tiny optimization: avoid redundant casts.
  // This is a hack-- we shouldn't be doing this
  if (node->operand->type == node->type) {
    return value;
  }
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
  // bool used_pre_existing_alloca = false;
  // if (m.current_alloca_stack.empty()) {
  dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  // } else {
  // Reuse a variables alloca so we don't have to double allocate.
  // used_pre_existing_alloca = true;
  // dest = m.current_alloca_stack.top();
  // m.current_alloca_stack.pop();
  // }

  for (const auto &[key, value] : node->key_values) {
    Type *base = node->type;
    size_t field_index = 0;
    if (key != CHOICE_TYPE_DISCRIMINANT_KEY) {
      if (!base->try_get_index_of_member(key, field_index)) {
        throw_error(std::format("unable to find index of member: {}", key.str()), node->span);
      }
    }

    Operand field_addr = m.create_temporary(value->type->take_pointer_to());
    Operand field_index_op = Operand::Make_Imm(Constant::Int(field_index), u32_type());
    EMIT_GEP(field_addr, dest, field_index_op);

    Operand field_value = generate_expr(value, m);
    EMIT_STORE(field_addr, field_value);
  }

  // The consumer of the pre existing alloca will load, this prevents an unneccesary double load.
  // if (!used_pre_existing_alloca) {
  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
  // }
  // return dest;
}

Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m) {
  Operand dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));

  for (size_t i = 0; i < node->values.size(); i++) {
    Operand element_addr = m.create_temporary(node->values[i]->type->take_pointer_to());
    Operand element_index = Operand::Make_Imm(Constant::Int(i), u32_type());
    EMIT_GEP(element_addr, dest, element_index);

    Operand element_value = generate_expr(node->values[i], m);
    EMIT_STORE(element_addr, element_value);
  }

  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
}

Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m) {
  Operand ptr = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(ptr, Operand::Make_Type_Ref(node->type));
  auto result = m.create_temporary(node->type);
  // TODO: this is completely fucking
  // this reuses a temp for no reason
  EMIT_ZERO_INIT(result, ptr, Operand::Make_Type_Ref(node->type));
  EMIT_LOAD(result, ptr);
  return result;
}

void generate_return(const THIRReturn *node, Module &m) {
  if (!node->expression) {
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

void generate_block(const THIRBlock *node, Module &m) {
  if (node->statements.empty() || block_only_contains_noop(node)) {
    return;
  }
  for (const THIR *stmt : node->statements) {
    generate(stmt, m);
    if (stmt->get_node_type() == THIRNodeType::Return) {
      // We terminate this basic block on return. to emit intructions after this
      // point would be illegal.
      return;
    }
  }
}

void generate_for(const THIRFor *node, Module &m) {
  if (node->get_node_type() == THIRNodeType::Variable) {
    generate_variable((THIRVariable *)node->initialization, m);
  } else {
    generate(node->initialization, m);
  }

  Basic_Block *orig_bb = m.get_insert_block();

  Basic_Block *cond_bb = m.create_and_enter_basic_block("for");
  Basic_Block *body_bb = m.create_and_enter_basic_block("do");
  Basic_Block *incr_bb = m.create_and_enter_basic_block("incr");
  Basic_Block *after_bb = m.create_and_enter_basic_block("done");

  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(body_bb, after_bb, cond);

  m.current_function->set_insert_block(body_bb);
  g_break_targets.push_back(after_bb);
  g_continue_targets.push_back(incr_bb);

  generate(node->block, m);

  g_break_targets.pop_back();
  g_continue_targets.pop_back();
  EMIT_JUMP(incr_bb);

  m.current_function->set_insert_block(incr_bb);
  generate(node->increment, m);

  EMIT_JUMP(cond_bb);
  m.current_function->set_insert_block(after_bb);
}

void generate_if(const THIRIf *node, Module &m) {
  Operand cond = generate_expr(node->condition, m);

  Basic_Block *if_bb = m.get_insert_block();
  Basic_Block *then_bb = m.create_and_enter_basic_block("then");
  Basic_Block *else_bb = node->_else ? m.create_and_enter_basic_block("else") : nullptr;
  Basic_Block *end_bb = m.create_and_enter_basic_block("end");
  m.current_function->set_insert_block(if_bb);

  if (else_bb) {
    EMIT_JUMP_TRUE(then_bb, else_bb, cond);
  } else {
    EMIT_JUMP_TRUE(then_bb, end_bb, cond);
  }

  m.current_function->set_insert_block(then_bb);
  generate_block(node->block, m);

  if (!then_bb->ends_with_terminator()) {
    auto insert_block = m.get_insert_block();
    m.current_function->set_insert_block(then_bb);
    EMIT_JUMP(end_bb);
    m.current_function->set_insert_block(insert_block);
  }

  if (else_bb) {
    m.current_function->set_insert_block(else_bb);
    generate(node->_else, m);
    if (!else_bb->ends_with_terminator()) {
      auto insert_block = m.get_insert_block();
      m.current_function->set_insert_block(else_bb);
      EMIT_JUMP(end_bb);
      m.current_function->set_insert_block(insert_block);
    }
  }

  m.current_function->set_insert_block(end_bb);
}

void generate_while(const THIRWhile *node, Module &m) {
  Basic_Block *orig_bb = m.get_insert_block();
  Basic_Block *cond_bb = m.create_and_enter_basic_block("while");
  Basic_Block *body_bb = m.create_and_enter_basic_block("do");
  Basic_Block *after_bb = m.create_and_enter_basic_block("done");

  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(body_bb, after_bb, cond);

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

void Basic_Block::finalize(Function *f) const {
  if (!ends_with_terminator()) {
    Op_Code opcode = code.size() ? code.back().opcode : (Op_Code)-1;
    printf(
        "in function: %s, basic block: %s, opcode: %d\n"
        "malformed basic block: every basic block must end with one of the following instructions:\n[OP_JMP, OP_JMP_TRUE, "
        "OP_RET, OP_RET_VOID]\n",
        f->name.c_str(), label.c_str(), opcode);
  }
}

Operand generate_ptr_bin_expr(const THIRPtrBinExpr *node, Module &m) {
  // handle compound-assignment forms first (they need lvalue handling)
  if (ttype_is_comp_assign(node->op)) {
    // only += and -= make sense for pointers
    if (!(node->op == TType::CompAdd || node->op == TType::CompSub)) {
      throw_error(std::format("unsupported compound pointer operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
    }

    Operand lvalue_addr = generate_lvalue_addr(node->left, m);
    Operand rvalue = generate_expr(node->right, m);

    // disallow ptr += ptr
    if (node->right->type->is_pointer()) {
      throw_error("pointer compound assign with pointer RHS is not allowed", node->span);
      return Operand::MakeNull();
    }

    // load current pointer value (or use register value if already a value)
    Operand current_val;
    current_val = m.create_temporary(node->left->type);
    EMIT_LOAD(current_val, lvalue_addr);

    Operand result = m.create_temporary(node->left->type);

    if (node->op == TType::CompAdd) {
      // ptr += int  -> GEP(ptr, int)
      EMIT_GEP(result, current_val, rvalue);
    } else {  // CompSub
      // ptr -= int -> GEP(ptr, -int)
      Operand neg = m.create_temporary(rvalue.type);
      Operand zero = Operand::Make_Imm(Constant::Int(0), rvalue.type);
      EMIT_BINOP(OP_SUB, neg, zero, rvalue);  // neg = 0 - rvalue
      EMIT_GEP(result, current_val, neg);
    }

    EMIT_STORE(lvalue_addr, result);
    return result;
  }

  Operand left = generate_expr(node->left, m);
  Operand right = generate_expr(node->right, m);
  Operand result;

  switch (node->op) {
    case TType::Add: {
      Operand base, index;
      if (node->left->type->is_pointer()) {
        base = left;
        index = right;
      } else if (node->right->type->is_pointer()) {
        base = right;
        index = left;
      } else {
        throw_error("add on non-pointer types reached pointer generator", node->span);
        return Operand::MakeNull();
      }

      result = m.create_temporary(node->type);
      EMIT_GEP(result, base, index);
      return result;
    }

    case TType::Sub: {
      if (node->left->type->is_pointer() && node->right->type->is_integer()) {
        Operand neg = m.create_temporary(right.type);
        Operand zero = Operand::Make_Imm(Constant::Int(0), right.type);
        EMIT_BINOP(OP_SUB, neg, zero, right);  // neg = 0 - right
        result = m.create_temporary(node->type);
        EMIT_GEP(result, left, neg);
        return result;
      } else if (node->left->type->is_pointer() && node->right->type->is_pointer()) {
        Operand lhs_int = m.create_temporary(u64_type());
        Operand rhs_int = m.create_temporary(u64_type());
        EMIT_BITCAST(lhs_int, left, Operand::Make_Type_Ref(u64_type()));
        EMIT_BITCAST(rhs_int, right, Operand::Make_Type_Ref(u64_type()));
        result = m.create_temporary(node->type);
        EMIT_BINOP(OP_SUB, result, lhs_int, rhs_int);
        return result;
      } else {
        throw_error("invalid pointer subtraction", node->span);
        return Operand::MakeNull();
      }
    }

    // pointer comparisons (==, !=, <, <=, >, >=)
    case TType::EQ:
    case TType::NEQ:
    case TType::LT:
    case TType::LE:
    case TType::GT:
    case TType::GE: {
      Op_Code cmp_op;
      switch (node->op) {
        case TType::EQ:
          cmp_op = OP_EQ;
          break;
        case TType::NEQ:
          cmp_op = OP_NE;
          break;
        case TType::LT:
          cmp_op = OP_LT;
          break;
        case TType::LE:
          cmp_op = OP_LE;
          break;
        case TType::GT:
          cmp_op = OP_GT;
          break;
        case TType::GE:
          cmp_op = OP_GE;
          break;
        default:
          cmp_op = (Op_Code)-1;
          break;
      }

      result = m.create_temporary(node->type);
      EMIT_BINOP(cmp_op, result, left, right);
      return result;
    }

    default:
      throw_error(std::format("unsupported pointer operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
  }

  return Operand::MakeNull();
}

Operand generate_ptr_unary_expr(const THIRPtrUnaryExpr *node, Module &m) {
  switch (node->op) {
    case TType::Increment: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand current_val = m.create_temporary(node->operand->type);
      EMIT_LOAD(current_val, addr);
      Operand one = Operand::Make_Imm(Constant::Int(1), u64_type());
      Operand next = m.create_temporary(node->operand->type);
      EMIT_GEP(next, current_val, one);
      EMIT_STORE(addr, next);
      return next;
    }
    case TType::Decrement: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand current_val = m.create_temporary(node->operand->type);
      EMIT_LOAD(current_val, addr);
      Operand neg_one = Operand::Make_Imm(Constant::Int(-1), u64_type());
      Operand next = m.create_temporary(node->operand->type);
      EMIT_GEP(next, current_val, neg_one);  // ptr - 1
      EMIT_STORE(addr, next);
      return next;
    }
    case TType::LogicalNot: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand loaded = m.create_temporary(node->operand->type);
      EMIT_LOAD(loaded, addr);
      auto dest = m.create_temporary(bool_type());
      EMIT_BINOP(OP_EQ, dest, loaded, Operand::Make_Imm(Constant::Int(0), u64_type()));
      return dest;
    }
    default:
      throw_error(std::format("unsupported pointer unary operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
  }
}

}  // namespace Mir