#include "mir.hpp"
#include <cstdio>
#include "core.hpp"
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
    return Operand::Temp(it->second->index, it->second->type);
  }

  uint32_t index;
  Function *f = m.create_function(node, index);

  convert_function_flags(node, f);

  // TODO: figure out how we're going to do externs.
  // probably just need an extern section
  if (node->is_extern) {
    return Operand::Temp(index, node->type);
  }
  m.enter_function(f);

  m.create_basic_block("entry");
  generate_block(node->block, m);
  Basic_Block *insert_block = f->get_insert_block();
  if (f->type_info->return_type == void_type() && (!insert_block->code.size() || insert_block->back_opcode() != OP_RET_VOID)) {
    EMIT_OP(OP_RET_VOID);
  }
  m.leave_function();
  return Operand::Temp(index, node->type);
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

  if (node->is_global) {
    auto g_var = m.global_variable_table[node];
    EMIT_LOAD_GLOBAL(result, Operand::Imm(Constant::Int(g_var->idx), node->type));
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
  if (node->is_global) {
    uint32_t idx = (uint32_t)m.global_variables.size();

    if (!type_is_valid(node->type)) {
      throw_error("invalid type in global var\n", node->span);
    }

    Global_Variable *v =
        mir_arena.construct<Global_Variable>(Global_Variable{.name = node->name, .type = node->type, .idx = idx});

    m.global_variables.push_back(v);
    m.global_variable_table[node] = v;

    if (node->value && node->value != THIRNoop::shared()) {
      if (m.current_function) {
        Operand init_val = generate_expr(node->value, m);
        Operand gidx = Operand::Imm(Constant::Int(idx), u32_type());
        EMIT_STORE_GLOBAL(gidx, init_val);
      }
    }
    return;
  }

  Operand dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Ty(node->type));

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

      if (var->is_global) {
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
        Operand gidx = Operand::Imm(Constant::Int(it->second->idx), u32_type());
        EMIT_LOAD_GLOBAL_PTR(result, gidx);
        // TODO: we actually have to take the address of the global here.
        // globals aren't stored with an "alloca" per-se, so we'll probably need a
        // REF_GLOBAL or something like that, or need to fix the GLOBAL_LOAD/STORE stuff
        // to not be so stupid.
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
      return Operand::Null();
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

  Operand field_index = Operand::Imm(Constant::Int(index), u32_type());

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
      return Operand::Null();
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
      EMIT_BINOP(OP_ADD, result, current_val, Operand::Imm(Constant::Int(1), node->type));
      EMIT_STORE(addr, result);
      return result;
    }
    case TType::Decrement: {
      Operand addr = generate_lvalue_addr(node->operand, m);
      Operand current_val = m.create_temporary(node->type);
      EMIT_LOAD(current_val, addr);
      Operand result = m.create_temporary(node->type);
      EMIT_BINOP(OP_SUB, result, current_val, Operand::Imm(Constant::Int(1), node->type));
      EMIT_STORE(addr, result);
      return result;
    }
    default:
      throw_error(std::format("unknown unary operator {}", (int)node->op), node->span);
      return Operand::Null();
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
  return Operand::Imm(value, node->type);
}

Operand generate_call(const THIRCall *node, Module &m) {
  for (const THIR *arg : node->arguments) {
    Operand arg_operand = generate_expr(arg, m);
    EMIT_PUSH_ARG(arg_operand);
  }

  Operand result = m.create_temporary(node->type);
  Operand fn_operand = generate_expr(node->callee, m);
  Operand arg_count = Operand::Imm(Constant::Int(node->arguments.size()), u32_type());

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
  Operand type_operand = Operand::Ty(node->type);

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
  Operand dest = Operand::Null();
  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Ty(node->type));
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
    Operand field_index_op = Operand::Imm(Constant::Int(field_index), u32_type());
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
  Operand dest = Operand::Null();
  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Ty(node->type));
  } else {
    used_pre_existing_alloca = true;
    dest = *m.current_alloca;
  }

  for (size_t i = 0; i < node->values.size(); i++) {
    Operand element_addr = m.create_temporary(node->values[i]->type->take_pointer_to());
    Operand element_index = Operand::Imm(Constant::Int(i), u32_type());
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
  Operand dest = Operand::Null();

  bool used_pre_existing_alloca = false;
  if (!m.current_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Ty(node->type));
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
  m.current_function->get_insert_block()->push(Instruction{OP_JMP, Operand(), Operand ::BB(target), .span = node->span});
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

const Type *get_base_type(const Type *t) {
  while (t->has_extensions()) {
    t = t->base_type;
  }
  return t;
}

std::string format_type_ref(const Type *t) {
  if (!t) {
    fprintf(stderr, "got a null type in format_type_ref\n");
    return "";
  }

  // If this type has pointer/array extensions, print a compact reference
  // that encodes the base type uid and the extension. Examples:
  //   pointer -> [*<base_uid>]
  //   pointer depth 2 -> [**<base_uid>]
  //   fixed array -> <[<base_uid>]; <size>>
  if (t->has_extensions()) {
    if (type_extensions_is_back_array(t->extensions)) {
      auto ext = t->extensions.back();
      const Type *base = get_base_type(t);
      return "<" + std::to_string(base->uid) + "; " + std::to_string(ext.array_size) + ">";
    }

    // pointer(s) at the back
    int depth = t->pointer_depth();
    if (depth > 0) {
      const Type *base = get_base_type(t);
      std::string stars(depth, '*');
      return "(" + stars + std::to_string(base->uid) + ")";
    }
  }
  return "(" + std::to_string(t->uid) + ")";
}

void collect_dependencies(const Type *t, std::unordered_set<const Type *> &visited, std::vector<const Type *> &ordered_types) {
  if (visited.count(t)) {
    return;
  }
  visited.insert(t);

  if (t->has_extensions() && t->kind != TYPE_FUNCTION &&
      (type_extensions_is_back_pointer(t->extensions) || type_extensions_is_back_array(t->extensions))) {
    const Type *base = get_base_type(t);
    collect_dependencies(base, visited, ordered_types);
    return;
  }
  switch (t->kind) {
    case TYPE_SCALAR:
      break;
    case TYPE_FUNCTION: {
      auto info = t->info->as<FunctionTypeInfo>();
      for (size_t i = 0; i < info->params_len; ++i) {
        collect_dependencies(info->parameter_types[i], visited, ordered_types);
      }
      if (info->return_type && info->return_type != void_type()) {
        collect_dependencies(info->return_type, visited, ordered_types);
      }
      break;
    }
    case TYPE_DYN: {
      auto info = t->info->as<DynTypeInfo>();
      for (const auto &method : info->methods) {
        collect_dependencies(method.second, visited, ordered_types);
      }
      break;
    }
    case TYPE_STRUCT: {
      for (const auto &member : t->info->members) {
        collect_dependencies(member.type, visited, ordered_types);
      }
      break;
    }
    case TYPE_ENUM: {
      auto info = t->info->as<EnumTypeInfo>();
      collect_dependencies(info->underlying_type, visited, ordered_types);
      break;
    }
    case TYPE_TUPLE: {
      auto info = t->info->as<TupleTypeInfo>();
      for (size_t i = 0; i < info->types.size(); ++i) {
        collect_dependencies(info->types[i], visited, ordered_types);
      }
      break;
    }
    case TYPE_CHOICE: {
      for (const auto &variant : t->info->members) {
        if (variant.type != void_type()) {
          collect_dependencies(variant.type, visited, ordered_types);
        }
      }
      break;
    }
    case TYPE_TRAIT:
      // Traits should not be emitted directly
      break;
  }
  ordered_types.push_back(t);
}

void print_type(FILE *f, const Type *t, int indent = 0) {
  auto print_indent = [f, indent]() {
    for (int i = 0; i < indent; ++i) fprintf(f, " ");
  };

  switch (t->kind) {
    case TYPE_SCALAR:
      print_indent();
      fprintf(f, "(%zu): %s\n", t->uid, t->to_string().c_str());
      break;
    case TYPE_FUNCTION: {
      auto info = t->info->as<FunctionTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): fn (", t->uid);
      for (size_t i = 0; i < info->params_len; ++i) {
        fprintf(f, "%s", format_type_ref(info->parameter_types[i]).c_str());
        if (i + 1 < info->params_len) fprintf(f, ", ");
      }
      fprintf(f, ")");
      if (info->return_type && info->return_type != void_type()) {
        fprintf(f, " -> %s", format_type_ref(info->return_type).c_str());
      }
      fprintf(f, "\n");
      break;
    }
    case TYPE_DYN: {
      auto info = t->info->as<DynTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): struct dyn %s {\n", t->uid, info->trait_type->basename.get_str().c_str());

      for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
      fprintf(f, "instance: %s\n", format_type_ref(void_type()->take_pointer_to()).c_str());

      for (const auto &method : info->methods) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s: %s\n", method.first.get_str().c_str(), format_type_ref(method.second).c_str());
      }

      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_STRUCT: {
      auto info = t->info->as<StructTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): %sstruct %s {\n", t->uid, info->is_union ? "union " : "", t->basename.get_str().c_str());
      for (const auto &member : t->info->members) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s: %s\n", member.name.get_str().c_str(), format_type_ref(member.type).c_str());
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_ENUM: {
      auto info = t->info->as<EnumTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): enum %s : %s {\n", t->uid, t->basename.get_str().c_str(),
              format_type_ref(info->underlying_type).c_str());
      for (const auto &member : t->info->members) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s\n", member.name.get_str().c_str());
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_TUPLE: {
      auto info = t->info->as<TupleTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): tuple (", t->uid);
      for (size_t i = 0; i < info->types.size(); ++i) {
        fprintf(f, "%s", format_type_ref(info->types[i]).c_str());
        if (i + 1 < info->types.size()) fprintf(f, ", ");
      }
      fprintf(f, ")\n");
      break;
    }
    case TYPE_CHOICE: {
      print_indent();
      fprintf(f, "(%zu): choice %s {\n", t->uid, t->basename.get_str().c_str());
      for (size_t i = 0; i < t->info->members.size(); ++i) {
        const auto &variant = t->info->members[i];
        for (int j = 0; j < indent + 2; ++j) fprintf(f, " ");
        fprintf(f, "%s: %zu", variant.name.get_str().c_str(), i);

        if (variant.type == void_type()) {
          fprintf(f, ",\n");
        } else if (variant.type->is_kind(TYPE_TUPLE)) {
          auto tuple_info = variant.type->info->as<TupleTypeInfo>();
          if (tuple_info->types.size() == 1) {
            fprintf(f, "(%s),\n", format_type_ref(tuple_info->types[0]).c_str());
          } else {
            fprintf(f, "(");
            for (size_t j = 0; j < tuple_info->types.size(); ++j) {
              fprintf(f, "%s", format_type_ref(tuple_info->types[j]).c_str());
              if (j + 1 < tuple_info->types.size()) fprintf(f, ", ");
            }
            fprintf(f, "),\n");
          }
        } else if (variant.type->is_kind(TYPE_STRUCT)) {
          fprintf(f, " {\n");
          for (const auto &member : variant.type->info->members) {
            for (int k = 0; k < indent + 4; ++k) fprintf(f, " ");
            fprintf(f, "%s: %s,\n", member.name.get_str().c_str(), format_type_ref(member.type).c_str());
          }
          for (int k = 0; k < indent + 2; ++k) fprintf(f, " ");
          fprintf(f, "},\n");
        } else {
          fprintf(f, ": %s,\n", format_type_ref(variant.type).c_str());
        }
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_TRAIT:
      throw_error("somehow we tried to emit a trait type", {});
      break;
  }
}

void Module::print(FILE *f) const {
  fprintf(f, "Types: {\n");

  // Collect all dependencies in proper order
  std::unordered_set<const Type *> visited;
  std::vector<const Type *> ordered_types;

  for (const auto *t : used_types) {
    collect_dependencies(t, visited, ordered_types);
  }

  // Print types in dependency order
  for (const auto *t : ordered_types) {
    print_type(f, t, 2);
  }

  fprintf(f, "}\n");

  for (const auto &fn : functions) {
    fn->print(f, (Module &)*this);
  }
}

void Instruction::print(FILE *f, Module &m) const {
  const char *opcode_name;

  switch (opcode) {
    case OP_NOOP:
      opcode_name = "NOP";
      break;
    case OP_ADD:
      opcode_name = "ADD";
      break;
    case OP_SUB:
      opcode_name = "SUB";
      break;
    case OP_MUL:
      opcode_name = "MUL";
      break;
    case OP_DIV:
      opcode_name = "DIV";
      break;
    case OP_MOD:
      opcode_name = "MOD";
      break;
    case OP_AND:
      opcode_name = "AND";
      break;
    case OP_OR:
      opcode_name = "OR";
      break;
    case OP_XOR:
      opcode_name = "XOR";
      break;
    case OP_SHL:
      opcode_name = "SHL";
      break;
    case OP_SHR:
      opcode_name = "SHR";
      break;
    case OP_EQ:
      opcode_name = "EQ";
      break;
    case OP_NE:
      opcode_name = "NE";
      break;
    case OP_LT:
      opcode_name = "LT";
      break;
    case OP_LE:
      opcode_name = "LE";
      break;
    case OP_GT:
      opcode_name = "GT";
      break;
    case OP_GE:
      opcode_name = "GE";
      break;
    case OP_LOGICAL_AND:
      opcode_name = "LOGICAL_AND";
      break;
    case OP_LOGICAL_OR:
      opcode_name = "LOGICAL_OR";
      break;
    case OP_NOT:
      opcode_name = "NOT";
      break;
    case OP_LOGICAL_NOT:
      opcode_name = "LOGICAL_NOT";
      break;
    case OP_NEG:
      opcode_name = "NEG";
      break;
    case OP_LOAD:
      opcode_name = "LOAD";
      break;
    case OP_STORE:
      opcode_name = "STORE";
      break;
    case OP_ALLOCA:
      opcode_name = "ALLOCA";
      break;
    case OP_GEP:
      opcode_name = "GEP";
      break;
    case OP_CAST:
      opcode_name = "CAST";
      break;
    case OP_LOAD_GLOBAL:
      opcode_name = "LOAD_GLOBAL";
      break;
    case OP_STORE_GLOBAL:
      opcode_name = "STORE_GLOBAL";
      break;
    case OP_LOAD_GLOBAL_PTR:
      opcode_name = "LOAD_GLOBAL_PTR";
      break;
    case OP_LOAD_FN_PTR:
      opcode_name = "LOAD_FN_PTR";
      break;
    case OP_PUSH_ARG:
      opcode_name = "PUSH_ARG";
      break;
    case OP_CALL:
      opcode_name = "CALL";
      break;
    case OP_RET:
      opcode_name = "RET";
      break;
    case OP_RET_VOID:
      opcode_name = "RET_VOID";
      break;
    case OP_JMP:
      opcode_name = "JMP";
      break;
    case OP_JMP_TRUE:
      opcode_name = "JMP_TRUE";
      break;
    case OP_JMP_FALSE:
      opcode_name = "JMP_FALSE";
      break;
    case OP_BITCAST:
      opcode_name = "BITCAST";
      break;
    case OP_CALL_PTR:
      opcode_name = "CALL_PTR";
      break;
  }

  // Build the instruction text into a string so we can compute its length and pad.
  std::string line;

  auto append_constant = [](const Constant &c) -> std::string {
    switch (c.tag) {
      case Constant::CONST_INVALID:
        return "invalid";
      case Constant::CONST_INT:
        return std::to_string(c.int_lit);
      case Constant::CONST_STRING:
        return std::string("\"") + c.string_lit.get_str() + "\"";
      case Constant::CONST_FLOAT:
        return std::to_string(c.float_lit);
      case Constant::CONST_BOOL:
        return c.bool_lit ? "true" : "false";
      case Constant::CONST_CHAR: {
        char buf[8] = {'\'', (char)c.char_lit, '\'', 0};
        return std::string(buf);
      }
    }
    return "";
  };

  auto print_operand_to_string = [&append_constant](const Operand &op, bool is_destination = false) -> std::string {
    switch (op.tag) {
      case Operand::OPERAND_NULL:
        return "null";
      case Operand::OPERAND_TEMP: {
        std::string s = "t" + std::to_string(op.temp);
        // we don't double print the type of temps because
        // it clutters the format and it's already known by the consumer
        // of the IR what type that local is (via its declaration)
        if (is_destination) {
          s += " ";
          s += format_type_ref(op.type);
          s += " = ";
        }
        return s;
      }
      case Operand::OPERAND_CONSTANT: {
        std::string s = "const(";
        s += append_constant(op.constant);
        s += ", ";
        s += format_type_ref(op.constant.type);
        s += ")";
        return s;
      }
      case Operand::OPERAND_IMMEDIATE_VALUE: {
        std::string s = "imm(";
        s += append_constant(op.immediate);
        s += ", ";
        s += format_type_ref(op.type);
        s += ")";
        return s;
      }
      case Operand::OPERAND_BASIC_BLOCK:
        return std::string("bb(") + op.bb->label.get_str() + ")";
      case Operand::OPERAND_TYPE:
        return format_type_ref(op.type);
    }
    return "";
  };

  std::vector<const Operand *> ops;

  if (dest.tag != Operand::OPERAND_NULL) {
    line += print_operand_to_string(dest, true);
  }

  line += opcode_name;

  if (left.tag != Operand::OPERAND_NULL) {
    if (opcode == OP_CALL) {
      line += " " + m.functions[left.temp]->name.get_str();
    } else {
      ops.push_back(&left);
    }
  }

  if (right.tag != Operand::OPERAND_NULL) {
    ops.push_back(&right);
  }

  if (!ops.empty()) {
    line += " ";
    for (size_t i = 0; i < ops.size(); ++i) {
      if (i) line += ", ";
      line += print_operand_to_string(*ops[i]);
    }
  }

  if (!compile_command.has_flag("release")) {
    const int DEBUG_COMMENT_COLUMN = 80;
    int printed_len = 2 + (int)line.size();
    int pad = DEBUG_COMMENT_COLUMN - printed_len;
    if (pad < 1) {
      pad = 1;
    }

    fprintf(f, "  %s", line.c_str());

    for (int i = 0; i < pad; ++i) {
      fputc(' ', f);
    }

    fprintf(f, "; ");
    std::string source = get_source_line_from_span(span);
    fprintf(f, "\"%s\" :: '%s'\n", source.c_str(), span.ToString().c_str());

  } else {
    fprintf(f, "  %s\n", line.c_str());
  }
}

void Basic_Block::print(FILE *f, Module &m) const {
  fprintf(f, "%s:", label.get_str().c_str());
  fprintf(f, "\n");
  for (const auto &instruction : code) {
    instruction.print(f, m);
  }
}

void Function::print(FILE *f, Module &m) const {
  if (HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXPORTED) || HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXTERN)) {
    fprintf(f, "extern ");
  }
  fprintf(f, "fn %s(", name.get_str().c_str());
  for (size_t i = 0; i < type_info->params_len; ++i) {
    fprintf(f, "t%zu: %s", i, format_type_ref(type_info->parameter_types[i]).c_str());
    if (i + 1 < type_info->params_len) {
      fprintf(f, ", ");
    }
  }
  fprintf(f, ")");
  if (type_info->return_type && type_info->return_type != void_type()) {
    fprintf(f, " -> %s", format_type_ref(type_info->return_type).c_str());
  }

  if (HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXPORTED) || HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXTERN)) {
    fprintf(f, " :: [flags: 0x%02x]", flags);
    fprintf(f, ";\n");
    return;
  } else {
    fprintf(f, " :: [flags: 0x%02x, stack: %zu bytes]", flags, stack_size_needed_in_bytes);
    fprintf(f, " {\n");
  }

  for (const auto &bb : basic_blocks) {
    bb->print(f, m);
  }

  fprintf(f, "}\n");
}

void Constant::print(FILE *f) const {
  switch (tag) {
    case CONST_INVALID:
      fprintf(f, "invalid");
      break;
    case CONST_INT:
      fprintf(f, "%zu", int_lit);
      break;
    case CONST_STRING:
      fprintf(f, "\"%s\"", string_lit.get_str().c_str());
      break;
    case CONST_FLOAT:
      fprintf(f, "%f", float_lit);
      break;
    case CONST_BOOL:
      fprintf(f, "%s", bool_lit ? "true" : "false");
      break;
    case CONST_CHAR:
      fprintf(f, "'%c'", (char)char_lit);
      break;
  }
}

}  // namespace Mir