#include "mir.hpp"
#include <cstdio>
#include <functional>
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "thir.hpp"
#include "type.hpp"

namespace mir {
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
  EMIT_JUMP(target);
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
    if (m.current_function->insert_block->ends_with_terminator()) {
      return;
    }
  }
}

void generate_for(const THIRFor *node, Module &m) {
  Basic_Block *orig_bb = m.current_function->get_insert_block();
  Basic_Block *for_bb = m.create_and_enter_basic_block("for");
  Basic_Block *cond_bb = m.create_and_enter_basic_block("for.cond");
  Basic_Block *do_bb = m.create_and_enter_basic_block("do");
  Basic_Block *incr_bb = m.create_and_enter_basic_block("incr");
  Basic_Block *after_bb = m.create_and_enter_basic_block("done");

  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(for_bb);

  m.current_function->set_insert_block(for_bb);
  generate(node->initialization, m);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(do_bb, after_bb, cond);

  m.current_function->set_insert_block(do_bb);
  g_break_targets.push_back(after_bb);
  g_continue_targets.push_back(incr_bb);

  generate_block((const THIRBlock *)node->block, m);

  g_break_targets.pop_back();
  g_continue_targets.pop_back();

  if (!m.current_function->get_insert_block()->ends_with_terminator()) {
    // again, we may have some code like a break or continue
    // that might already perform a jump right here, and in that case,
    // this cannot be emitted.
    EMIT_JUMP(incr_bb);
  }

  m.current_function->set_insert_block(incr_bb);
  generate(node->increment, m);

  if (!m.current_function->get_insert_block()->ends_with_terminator()) {
    // This is less possible to ever be something other than true.
    // Im not sure how youd get a jump in the increment of a for loop.

    // This will surely ALWAYS get hit.
    EMIT_JUMP(cond_bb);
  }

  m.current_function->set_insert_block(after_bb);
}

void generate_if(const THIRIf *node, Module &m) {
  Basic_Block *if_bb = m.current_function->get_insert_block();
  Basic_Block *cond_bb = m.create_and_enter_basic_block("if.cond");
  Basic_Block *then_bb = m.create_and_enter_basic_block("then");
  Basic_Block *else_bb = node->_else ? m.create_and_enter_basic_block("else") : nullptr;
  Basic_Block *end_bb = m.create_and_enter_basic_block("end");

  m.current_function->set_insert_block(if_bb);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  if (else_bb) {
    EMIT_JUMP_TRUE(then_bb, else_bb, cond);
  } else {
    EMIT_JUMP_TRUE(then_bb, end_bb, cond);
  }

  m.current_function->set_insert_block(then_bb);
  generate_block(node->block, m);

  if (!m.current_function->get_insert_block()->ends_with_terminator()) {
    EMIT_JUMP(end_bb);
  }

  if (else_bb) {
    m.current_function->set_insert_block(else_bb);
    generate(node->_else, m);
    if (!m.current_function->get_insert_block()->ends_with_terminator()) {
      EMIT_JUMP(end_bb);
    }
  }

  m.current_function->set_insert_block(end_bb);
}

void generate_while(const THIRWhile *node, Module &m) {
  Basic_Block *orig_bb = m.current_function->get_insert_block();
  Basic_Block *cond_bb = m.create_and_enter_basic_block("while");
  Basic_Block *do_bb = m.create_and_enter_basic_block("do");
  Basic_Block *done_bb = m.create_and_enter_basic_block("done");

  m.current_function->set_insert_block(orig_bb);
  EMIT_JUMP(cond_bb);

  m.current_function->set_insert_block(cond_bb);
  Operand cond = generate_expr(node->condition, m);
  EMIT_JUMP_TRUE(do_bb, done_bb, cond);

  m.current_function->set_insert_block(do_bb);
  g_break_targets.push_back(done_bb);
  g_continue_targets.push_back(cond_bb);
  generate(node->block, m);
  g_break_targets.pop_back();
  g_continue_targets.pop_back();

  if (!m.current_function->get_insert_block()->ends_with_terminator()) {
    // We may just have a single break in a loop.
    // We _should_ optimize this shit out, but for now,
    // we have to stop doing this.

    // We could also just ignore everything in a basic block past a jump, ret, unreachable, etc
    // in the LLVM backend, but that's just sloppy ass MIR.
    EMIT_JUMP(cond_bb);
  }

  m.current_function->set_insert_block(done_bb);
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
  if (t->is_exported) {
    flags |= Function::FUNCTION_FLAGS_IS_EXPORTED;
  }
  if (t->is_no_return) {
    flags |= Function::FUNCTION_FLAGS_IS_NO_RETURN;
  }

  // either CONSTRUCTOR_0 or CONSTRUCTOR_1, which gives it priority for getting called
  // either BEFORE        or AFTER          global initializers run.
  if (t->constructor_index != 0) {
    flags |= Function::FUNCTION_FLAGS_IS_CONSTRUCTOR_0 + (t->constructor_index - 1);
  }

  f->flags = flags;
}

void insert_ret_void_if_missing(const THIRFunction *node, Module &m, Function *f, Basic_Block *end) {
  if (f->type_info->return_type == void_type() &&
      (end->code.empty() || (end->back_opcode() != OP_RET_VOID && end->back_opcode() != OP_UNREACHABLE))) {
    m.current_function->set_insert_block(end);
    EMIT_OP(OP_RET_VOID);
  }
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
  convert_function_flags(node, f);

  m.enter_function(f);
  Defer _defer([&] {
    if (m.function_stack.size()) {  // we never push the first function
      m.leave_function();
    }
  });

  if (!node->is_extern) {
    f->create_and_enter_basic_block("entry");
  }

  for (const auto &param : node->parameters) {
    Operand parameter_temp = m.create_temporary(param.associated_variable->type);
    f->parameter_temps.push_back({.name = std::format("t{}: {}", parameter_temp.temp, param.name),
                                  .type = parameter_temp.type,
                                  .index = parameter_temp.temp});
    Type *type = param.associated_variable->type;
    if (!node->is_extern) {
      // We just make allocas and store the initial values of parameters for all parameters,
      // this way we can take the address of a parameter, and mutate them.
      Operand alloca_temp = m.create_temporary(type->take_pointer_to(), param.name);
      EMIT_ALLOCA(alloca_temp, Operand ::Make_Type_Ref(type));
      EMIT_STORE(alloca_temp, parameter_temp);
      m.variables[param.associated_variable] = alloca_temp;
    } else {
      m.variables[param.associated_variable] = parameter_temp;
    }
  }

  m.functions.push_back(f);
  m.function_table[node->name] = f;

  if (node->is_extern) {
    return Operand::Make_Temp(index, node->type);
  }

  generate_block(node->block, m);

  insert_ret_void_if_missing(node, m, f, f->insert_block);

  size_t num_blocks_ending_with_non_divergent_terminator = 0;
  for (const auto &bb : f->basic_blocks) {
    num_blocks_ending_with_non_divergent_terminator += bb->ends_with_terminator();
  }

  if (num_blocks_ending_with_non_divergent_terminator == f->basic_blocks.size() - 1 &&
      !f->basic_blocks.back()->ends_with_terminator()) {
    f->basic_blocks.back()->push({OP_UNREACHABLE});
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
  if (node->is_from_enum_declaration && it == m.variables.end()) {
    for (const auto &var : node->enum_type->enum_members) {
      generate(var, m);
    }
    it = m.variables.find(node);
  }

  if (it == m.variables.end()) {
    throw_error(std::format("variable '{}' not declared", node->name.str()), node->span);
  }

  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, it->second);
  return result;
}

Operand generate_variable(const THIRVariable *node, Module &m) {
  // static locals are just syntax sugar for a scoped global.
  if (node->is_global || node->is_static) {
    // we take a pointer to it because we have to load from this.
    Global_Variable global = {.name = node->name, .type = node->type, .has_external_linkage = node->is_global};
    Global_Variable *gv = mir_arena.construct<Global_Variable>(global);
    m.global_variables.push_back(gv);
    m.global_variable_table[node] = gv;
    return Operand::MakeNull();
  }

  Operand dest = m.create_temporary(node->type->take_pointer_to(), node->name);
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));

  if (!node->is_uninitialized) {
    Operand value_temp = generate_expr(node->value, m, &dest);

    // Is this even applicable? it's kind of a leftover from a previous system,
    // not sure.
    if (value_temp.tag != Operand::OPERAND_TEMP || value_temp.temp != dest.temp) {
      EMIT_STORE(dest, value_temp);
    }
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
    case THIRNodeType::MemberAccess:
      return generate_member_access_addr((const THIRMemberAccess *)node, m);
    case THIRNodeType::Index:
      return generate_index_addr((const THIRIndex *)node, m);
    case THIRNodeType::UnaryExpr: {
      const THIRUnaryExpr *unary = (const THIRUnaryExpr *)node;
      if (unary->op == TType::Mul) {
        return generate_expr(unary->operand, m);
      }
      break;
    }
    case THIRNodeType::Function: {
      Operand temp = m.create_temporary(node->type->take_pointer_to());
      Operand fn_index = generate_function((THIRFunction *)node, m);
      EMIT_LOAD_FN_PTR(temp, fn_index);
      return temp;
    }
    // Add these cases for rvalues:
    case THIRNodeType::Call:
    case THIRNodeType::Cast:
    case THIRNodeType::Literal:
    case THIRNodeType::AggregateInitializer:
    case THIRNodeType::CollectionInitializer:
    case THIRNodeType::EmptyInitializer:
    case THIRNodeType::ExpressionBlock:
    default:
      break;
  }

  // Fallback: make an addressable temporary for rvalues.
  Operand value = generate_expr(node, m);
  Operand dest = m.create_temporary(node->type->take_pointer_to());
  EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  EMIT_STORE(dest, value);
  return dest;
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
  Operand base_addr;
  if (node->base->type->is_pointer()) {
    base_addr = generate_expr(node->base, m);
  } else {
    base_addr = generate_lvalue_addr(node->base, m);
  }
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
    case ASTLiteral::Integer: {
      std::string s = node->value.str();
      int base = 10;
      const char *str = s.c_str();
      if (str[0] == '0') {
        if (str[1] == 'x' || str[1] == 'X') {
          base = 16;
          str += 2;
        } else if (str[1] == 'b' || str[1] == 'B') {
          base = 2;
          str += 2;
        } else if (str[1] == 'o' || str[1] == 'O') {
          base = 8;
          str += 2;
        } else if (isdigit(str[1])) {
          base = 8;
          str += 1;
        }
      }
      unsigned long long val = strtoull(str, nullptr, base);
      value = Constant::Int(val);
    } break;
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
      THIRLiteral *literal = (THIRLiteral *)node;
      literal->type = u8_ptr_type();
      value = Constant::Nullptr(u8_ptr_type());
      break;
  }
  return Operand::Make_Imm(value, node->type);
}

Operand generate_call(const THIRCall *node, Module &m) {
  Operand result = Operand::MakeNull();
  if (node->type != void_type()) {
    result = m.create_temporary(node->type);
  }
  Operand fn_operand = generate_expr(node->callee, m);
  Operand arg_count = Operand::Make_Imm(Constant::Int(node->arguments.size()), u32_type());

  THIRFunction *callee = nullptr;
  if (node->callee->get_node_type() == THIRNodeType::Function) {
    callee = (THIRFunction *)node->callee;
  }

  for (const THIR *arg : node->arguments) {
    Operand arg_operand = generate_expr(arg, m);
    if (callee && callee->is_varargs) {
      if (arg_operand.type->is_float() && arg_operand.type != f64_type()) {
        Operand casted = m.create_temporary(f64_type(), "varargs_float_to_f64");
        EMIT_CAST(casted, arg_operand, Operand::Make_Type_Ref(f64_type()));
        arg_operand = casted;
      }
    }
    EMIT_PUSH_ARG(arg_operand);
  }

  if (node->callee->type->is_pointer()) {
    EMIT_CALL_PTR(result, fn_operand, arg_count);
  } else {
    EMIT_CALL(result, fn_operand, arg_count);
  }

  if (node->callee->get_node_type() == THIRNodeType::Function) {
    THIRFunction *f = (THIRFunction *)node->callee;
    if (f->is_no_return) {
      EMIT_UNREACHABLE();
    }
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
  // Casting an array to a pointer is just a secret &arr[0].
  if (node->type->is_pointer() && node->operand->type->is_fixed_sized_array()) {
    Operand result = m.create_temporary(node->type->take_pointer_to());
    Operand value = generate_lvalue_addr(node->operand, m);
    EMIT_GEP(result, value, Operand::Make_Imm(Constant::Int(0), u64_type()));
    Operand loaded = m.create_temporary(node->type);
    EMIT_LOAD(loaded, result);
    return result;
  }

  Operand value = generate_expr(node->operand, m);
  // Tiny optimization: avoid redundant casts.
  // This is a hack-- we shouldn't be doing this in the first place.
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

Operand generate_aggregate_initializer(const THIRAggregateInitializer *node, Module &m, Nullable<Operand> existing_alloca) {
  Operand dest = Operand::MakeNull();

  if (!existing_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  } else {
    dest = existing_alloca.deref();
  }
  EMIT_ZERO_INIT(dest, Operand::Make_Type_Ref(node->type));

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

  Operand result = m.create_temporary(node->type);
  EMIT_LOAD(result, dest);
  return result;
}

Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m, Nullable<Operand> existing_alloca) {
  Operand dest;

  if (!existing_alloca) {
    dest = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(dest, Operand::Make_Type_Ref(node->type));
  } else {
    dest = existing_alloca.deref();
  }

  // Zero initialize to ensure every element that we don't emit here is also zeroed.
  // This may be redundant.
  EMIT_ZERO_INIT(dest, Operand::Make_Type_Ref(node->type));

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

Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m, Nullable<Operand> existing_alloca) {
  Operand ptr;

  if (!existing_alloca) {
    ptr = m.create_temporary(node->type->take_pointer_to());
    EMIT_ALLOCA(ptr, Operand::Make_Type_Ref(node->type));
  } else {
    ptr = *existing_alloca.get();
  }

  if (node->type->has_no_extensions() && node->type->is_integer()) {
    EMIT_STORE(ptr, Operand::Make_Imm(Constant::Int(0, node->type), node->type));
    return ptr;
  }

  if (node->type->has_no_extensions() && node->type->is_float()) {
    EMIT_STORE(ptr, Operand::Make_Imm(Constant::Float(0.0f, node->type), node->type));
    return ptr;
  }

  if (node->type->is_pointer()) {
    EMIT_STORE(ptr, Operand::Make_Imm(Constant::Nullptr(node->type), node->type));
    return ptr;
  }

  EMIT_ZERO_INIT(ptr, Operand::Make_Type_Ref(node->type));
  return ptr;
}

Operand Operand::Make_Global_Ref(Global_Variable *gv_ref) {
  Operand o;
  o.gv = gv_ref;
  o.type = gv_ref->type;
  o.tag = OPERAND_GLOBAL_VARIABLE_REFERENCE;
  return o;
}

void Basic_Block::finalize(Function *f) const {
  if (!ends_with_terminator() && DOESNT_HAVE_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_NO_RETURN)) {
    const Op_Code last_opcode = code.size() ? code.back().opcode : (Op_Code)-1;

    // Accept OP_UNREACHABLE as a valid terminator
    bool valid_terminator = last_opcode == OP_JMP || last_opcode == OP_JMP_TRUE || last_opcode == OP_RET ||
                            last_opcode == OP_RET_VOID || last_opcode == OP_UNREACHABLE;

    if (!valid_terminator) {
      printf(
          "in function: %s, basic block: %s, opcode: %s\n"
          "malformed basic block: every basic block must end with one of the following instructions:\n"
          "[OP_JMP, OP_JMP_TRUE, OP_RET, OP_RET_VOID, OP_UNREACHABLE]\n",
          f->name.c_str(), label.c_str(), opcode_to_string(last_opcode));
    }
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
    case TType::And: {
      return generate_lvalue_addr(node->operand, m);
    }
    default:
      throw_error(std::format("unsupported pointer unary operator {}", (int)node->op), node->span);
      return Operand::MakeNull();
  }
}

void compile(const THIR *entry_point, Module &m, const std::vector<THIRFunction *> &constructors,
             const std::vector<THIRFunction *> &test_functions, const THIRFunction *global_initializer) {
  if (compile_command.has_flag("test")) {
    for (const auto &f : test_functions) {
      generate(f, m);
    }
  }

  /*
    These are disabled because of bugs I can't currently solve.
    TODO: fixme
  */
  if (false) {
    for (const auto &ctor : constructors) {
      generate(ctor, m);
    }
  }

  if (true) {
    generate(global_initializer, m);
  }

  generate(entry_point, m);
}
}  // namespace mir
