#include "llvm_emit.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Value.h>
#include "mir.hpp"
#include "type.hpp"

llvm::Value *LLVM_Emitter::binary_signed(llvm::Value *left, llvm::Value *right, Op_Code op) {
  switch (op) {
    case Mir::OP_ADD:
      return builder.CreateAdd(left, right, "addtmp");
    case Mir::OP_SUB:
      return builder.CreateSub(left, right, "subtmp");
    case Mir::OP_MUL:
      return builder.CreateMul(left, right, "multmp");
    case Mir::OP_DIV:
      return builder.CreateSDiv(left, right, "divtmp");
    case Mir::OP_MOD:
      return builder.CreateSRem(left, right, "modtmp");
    case Mir::OP_LT:
      return builder.CreateICmpSLT(left, right, "lttmp");
    case Mir::OP_GT:
      return builder.CreateICmpSGT(left, right, "gttmp");
    case Mir::OP_LE:
      return builder.CreateICmpSLE(left, right, "letmp");
    case Mir::OP_GE:
      return builder.CreateICmpSGE(left, right, "getmp");
    case Mir::OP_EQ:
      return builder.CreateICmpEQ(left, right, "eqtmp");
    case Mir::OP_NE:
      return builder.CreateICmpNE(left, right, "neqtmp");
    case Mir::OP_AND:
      return builder.CreateAnd(left, right, "andtmp");
    case Mir::OP_OR:
      return builder.CreateOr(left, right, "ortmp");
    case Mir::OP_XOR:
      return builder.CreateXor(left, right, "xortmp");
    case Mir::OP_SHL:
      return builder.CreateShl(left, right, "shltmp");
    case Mir::OP_SHR:
      return builder.CreateAShr(left, right, "shrtmp");  // Arithmetic shift
    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVM_Emitter::binary_unsigned(llvm::Value *left, llvm::Value *right, Op_Code op) {
  switch (op) {
    case Mir::OP_ADD:
      return builder.CreateAdd(left, right, "addtmp");
    case Mir::OP_SUB:
      return builder.CreateSub(left, right, "subtmp");
    case Mir::OP_MUL:
      return builder.CreateMul(left, right, "multmp");
    case Mir::OP_DIV:
      return builder.CreateUDiv(left, right, "divtmp");
    case Mir::OP_MOD:
      return builder.CreateURem(left, right, "modtmp");
    case Mir::OP_LT:
      return builder.CreateICmpULT(left, right, "lttmp");
    case Mir::OP_GT:
      return builder.CreateICmpUGT(left, right, "gttmp");
    case Mir::OP_LE:
      return builder.CreateICmpULE(left, right, "letmp");
    case Mir::OP_GE:
      return builder.CreateICmpUGE(left, right, "getmp");
    case Mir::OP_EQ:
      return builder.CreateICmpEQ(left, right, "eqtmp");
    case Mir::OP_NE:
      return builder.CreateICmpNE(left, right, "neqtmp");
    case Mir::OP_AND:
      return builder.CreateAnd(left, right, "andtmp");
    case Mir::OP_OR:
      return builder.CreateOr(left, right, "ortmp");
    case Mir::OP_XOR:
      return builder.CreateXor(left, right, "xortmp");
    case Mir::OP_SHL:
      return builder.CreateShl(left, right, "shltmp");
    case Mir::OP_SHR:
      return builder.CreateLShr(left, right, "shrtmp");  // Logical shift
    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVM_Emitter::binary_fp(llvm::Value *left, llvm::Value *right, Op_Code op) {
  switch (op) {
    case Mir::OP_ADD:
      return builder.CreateFAdd(left, right, "addtmp");
    case Mir::OP_SUB:
      return builder.CreateFSub(left, right, "subtmp");
    case Mir::OP_MUL:
      return builder.CreateFMul(left, right, "multmp");
    case Mir::OP_DIV:
      return builder.CreateFDiv(left, right, "divtmp");
    case Mir::OP_MOD:
      return builder.CreateFRem(left, right, "modtmp");
    case Mir::OP_LT:
      return builder.CreateFCmpOLT(left, right, "lttmp");
    case Mir::OP_GT:
      return builder.CreateFCmpOGT(left, right, "gttmp");
    case Mir::OP_LE:
      return builder.CreateFCmpOLE(left, right, "letmp");
    case Mir::OP_GE:
      return builder.CreateFCmpOGE(left, right, "getmp");
    case Mir::OP_EQ:
      return builder.CreateFCmpOEQ(left, right, "eqtmp");
    case Mir::OP_NE:
      return builder.CreateFCmpONE(left, right, "neqtmp");
    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVM_Emitter::cast_scalar(llvm::Value *value, Type *from, Type *to) {
  if (from == to) {
    return value;
  }

  auto from_info = from->info->as<ScalarTypeInfo>();
  auto to_info = to->info->as<ScalarTypeInfo>();
  auto llvm_to = llvm_typeof(to);

  if (to->is_pointer()) {
    if (from->is_pointer()) {
      return builder.CreateBitCast(value, llvm_to, "bitcasttmp");
    }
  } else if (to_info->scalar_type == TYPE_BOOL) {
    if (from_info->is_integral) {
      return builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0), "booltmp");
    } else {
      return builder.CreateFCmpONE(value, llvm::ConstantFP::get(value->getType(), 0.0), "booltmp");
    }
  } else if (to_info->is_integral) {
    if (from_info->scalar_type == TYPE_BOOL) {
      return builder.CreateZExt(value, llvm_to, "zexttmp");
    } else if (from_info->is_integral) {
      if (value->getType()->getIntegerBitWidth() < llvm_to->getIntegerBitWidth()) {
        if (from_info->is_signed()) {
          return builder.CreateSExt(value, llvm_to, "sexttmp");
        } else {
          return builder.CreateZExt(value, llvm_to, "zexttmp");
        }
      } else {
        return builder.CreateTrunc(value, llvm_to, "trunctmp");
      }
    } else {
      if (to_info->is_signed()) {
        return builder.CreateFPToSI(value, llvm_to, "fptositmp");
      } else {
        return builder.CreateFPToUI(value, llvm_to, "fptouitmp");
      }
    }
  } else {
    if (from_info->scalar_type == TYPE_BOOL) {
      return builder.CreateUIToFP(value, llvm_to, "uitofptmp");
    } else if (from_info->is_integral) {
      if (from_info->is_signed()) {
        return builder.CreateSIToFP(value, llvm_to, "sitofptmp");
      } else {
        return builder.CreateUIToFP(value, llvm_to, "uitofptmp");
      }
    } else {
      if (value->getType()->getPrimitiveSizeInBits() < llvm_to->getPrimitiveSizeInBits()) {
        return builder.CreateFPExt(value, llvm_to, "fpexttmp");
      } else if (value->getType()->getPrimitiveSizeInBits() > llvm_to->getPrimitiveSizeInBits()) {
        return builder.CreateFPTrunc(value, llvm_to, "fptrunctmp");
      }
    }
  }

  return nullptr;
}

void LLVM_Emitter::emit_module() {
  // Forward declare all functions by header so we can refer to them in other functions with no worry
  dbg.enter_file_scope({});
  for (const auto &f : m.functions) {
    llvm::FunctionType *func_type = (llvm::FunctionType *)llvm_typeof(f->type);
    llvm::Function *llvm_f =
        llvm::Function::Create(func_type, llvm::GlobalValue::ExternalLinkage, f->name.get_str(), llvm_module.get());
    function_table[f] = llvm_f;
  }

  for (const auto &f : m.functions) {
    if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_EXTERN)) {
      continue;
    }
    emit_function(f, function_table[f]);
  }
  dbg.pop_scope();
}

void LLVM_Emitter::emit_function(Mir::Function *f, llvm::Function *ir_f) {
  auto subprogram = dbg.enter_function_scope(dbg.current_scope(), ir_f, f->name.get_str(), f->span);
  ir_f->setSubprogram(subprogram);

  bb_table.reserve(f->basic_blocks.size());

  for (auto *bb : f->basic_blocks) {  // pre-create the basic blocks so we can easily do jumps and flips on it and shit
    llvm::BasicBlock *ir_bb = llvm::BasicBlock::Create(llvm_ctx, bb->label.get_str(), ir_f);
    bb_table[bb] = ir_bb;
  }

  // Declare the parameters and store them in the temps
  temps.reserve(f->temps.size());
  for (size_t i = 0; i < f->type_info->params_len; ++i) {
    llvm::Argument *llvm_param = ir_f->getArg(i);
    temps[i] = llvm_param;
  }

  for (const auto &bb : f->basic_blocks) {
    builder.SetInsertPoint(bb_table[bb]);
    emit_basic_block(bb, f);
  }

  temps.clear();
  bb_table.clear();
  dbg.pop_scope();
  builder.ClearInsertionPoint();
}

void LLVM_Emitter::emit_basic_block(Mir::Basic_Block *bb, Mir::Function *f) {
  for (const auto &instr : bb->code) {
    switch (instr.opcode) {
      case Mir::OP_NOOP:
        continue;
      case Mir::OP_ADD:
      case Mir::OP_SUB:
      case Mir::OP_MUL:
      case Mir::OP_DIV:
      case Mir::OP_MOD:
      case Mir::OP_AND:
      case Mir::OP_OR:
      case Mir::OP_XOR:
      case Mir::OP_SHL:
      case Mir::OP_SHR:
      case Mir::OP_LOGICAL_AND:
      case Mir::OP_LOGICAL_OR:
      case Mir::OP_EQ:
      case Mir::OP_NE:
      case Mir::OP_LT:
      case Mir::OP_LE:
      case Mir::OP_GT:
      case Mir::OP_GE: {
        llvm::Value *left = visit_operand(instr.left, true);
        llvm::Value *right = visit_operand(instr.right, true);

        if (instr.left.type->is_pointer() || instr.right.type->is_pointer()) {
          llvm::Value *result = pointer_binary(left, right, instr);
          builder.CreateStore(result, temps[instr.dest.temp]);
          break;
        }

        ScalarTypeInfo *left_info = instr.left.type->info->as<ScalarTypeInfo>();
        ScalarTypeInfo *right_info = instr.left.type->info->as<ScalarTypeInfo>();
        llvm::Value *result = nullptr;

        right = cast_scalar(right, instr.right.type, instr.left.type);

        if (left_info->is_signed() && right_info->is_signed()) {
          result = binary_signed(left, right, instr.opcode);
        } else if (left_info->is_integral && right_info->is_integral) {
          result = binary_unsigned(left, right, instr.opcode);
        } else if (left_info->is_float() && right_info->is_float()) {
          result = binary_fp(left, right, instr.opcode);
        } else {
          std::string msg = std::format("Unsupported operand types for binary operation: left type '{}', right type '{}'",
                                        instr.left.type->to_string(), instr.right.type->to_string());
          throw_error(msg, instr.span);
          return;
        }

        builder.CreateStore(result, temps[instr.dest.temp]);
      } break;

      case Mir::OP_LOGICAL_NOT: {
        // dest = !left
        llvm::Value *v = visit_operand(instr.left, true);
        Type *bool_mir = bool_type();
        v = cast_scalar(v, instr.left.type, bool_mir);

        if (!v->getType()->isIntegerTy(1)) {
          v = builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv");
        }

        llvm::Value *not_val = builder.CreateNot(v, "nottmp");
        builder.CreateStore(not_val, temps[instr.dest.temp]);
      } break;

      case Mir::OP_NOT: {
        llvm::Value *v = visit_operand(instr.left, true);
        temps[instr.dest.temp] = builder.CreateNot(v, "bittmp");
      } break;

      case Mir::OP_LOAD: {
        temps[instr.dest.temp] = visit_operand(instr.left, true);
      } break;

      case Mir::OP_NEG: {
        llvm::Value *v = visit_operand(instr.left, true);

        if (instr.left.type->is_pointer()) {
          llvm::Value *res = pointer_unary(v, instr);
          builder.CreateStore(res, temps[instr.dest.temp]);
          break;
        }

        auto info = instr.left.type->info->as<ScalarTypeInfo>();
        llvm::Value *res = nullptr;
        if (info->is_float()) {
          res = builder.CreateFNeg(v, "fnegtmp");
        } else {
          res = builder.CreateNeg(v, "negtmp");
        }
        builder.CreateStore(res, temps[instr.dest.temp]);
      } break;

      case Mir::OP_STORE: {
        // store right -> *left
        llvm::Value *ptr = visit_operand(instr.left, false);
        llvm::Value *val = visit_operand(instr.right, true);
        builder.CreateStore(val, ptr);
      } break;

      case Mir::OP_ALLOCA: {
        uint32_t index = instr.dest.temp;
        Temporary &temp = f->temps[index];
        temps[index] = builder.CreateAlloca(llvm_typeof(temp.type), nullptr, temp.name.get_str());
      } break;

      case Mir::OP_LOAD_GLOBAL: {
        Global_Variable *gvar = m.global_variables[instr.right.temp];
        llvm::Value *l_gvar = global_variables[gvar];
        llvm::Value *val = builder.CreateLoad(llvm_typeof(gvar->type), l_gvar, "loadglobaltmp");
        builder.CreateStore(val, temps[instr.dest.temp]);
      } break;

      case Mir::OP_STORE_GLOBAL: {
        Global_Variable *gvar = m.global_variables[instr.right.temp];
        llvm::Value *l_gvar = global_variables[gvar];
        llvm::Value *val = visit_operand(instr.right, true);
        builder.CreateStore(val, l_gvar);
      } break;

      case Mir::OP_LOAD_GLOBAL_PTR: {
        Global_Variable *gvar = m.global_variables[instr.right.temp];
        llvm::Value *l_gvar = global_variables[gvar];
        builder.CreateStore(l_gvar, temps[instr.dest.temp]);
      } break;

      case Mir::OP_LOAD_FN_PTR: {  // this is a stupid instruction, get rid of it
        llvm::Value *fnptr = visit_operand(instr.right, false);
        builder.CreateStore(fnptr, temps[instr.dest.temp]);
      } break;

      case Mir::OP_JMP: {
        Mir::Basic_Block *target_mb = instr.left.bb;
        auto it = bb_table.find(target_mb);
        if (it == bb_table.end()) {
          throw_error("Unknown target basic block", instr.span);
        }
        llvm::BasicBlock *target_bb = it->second;
        builder.CreateBr(target_bb);
      } break;

      case Mir::OP_JMP_TRUE: {
        Mir::Basic_Block *target_mb = instr.left.bb;
        llvm::Value *cond = visit_operand(instr.right, true);
        Type *bool_mir = bool_type();
        cond = cast_scalar(cond, instr.right.type, bool_mir);

        if (!cond->getType()->isIntegerTy(1)) {
          cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "boolconv");
        }

        auto it = bb_table.find(target_mb);
        if (it == bb_table.end()) throw_error("Unknown target basic block", instr.span);
        llvm::BasicBlock *target_bb = it->second;

        // fallthrough block (next in function); create if missing
        llvm::BasicBlock *parent_bb = builder.GetInsertBlock();
        llvm::Function *cur_f = parent_bb->getParent();
        llvm::BasicBlock *fallthrough_bb = nullptr;
        for (auto itb = cur_f->begin(); itb != cur_f->end(); ++itb) {
          if (&*itb == parent_bb) {
            auto next = std::next(itb);
            if (next != cur_f->end()) {
              fallthrough_bb = &*next;
            }
            break;
          }
        }
        if (!fallthrough_bb) {
          fallthrough_bb = llvm::BasicBlock::Create(llvm_ctx, parent_bb->getName() + ".cont", cur_f);
        }

        builder.CreateCondBr(cond, target_bb, fallthrough_bb);
      } break;

      case Mir::OP_JMP_FALSE: {
        Mir::Basic_Block *target_mb = instr.left.bb;
        llvm::Value *cond = visit_operand(instr.right, true);

        Type *bool_mir = bool_type();
        cond = cast_scalar(cond, instr.right.type, bool_mir);
        if (!cond->getType()->isIntegerTy(1)) {
          cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "boolconv");
        }
        cond = builder.CreateNot(cond, "notcond");

        auto it = bb_table.find(target_mb);
        if (it == bb_table.end()) {
          throw_error("Unknown target basic block", instr.span);
        }
        llvm::BasicBlock *target_bb = it->second;

        llvm::BasicBlock *parent_bb = builder.GetInsertBlock();
        llvm::Function *cur_f = parent_bb->getParent();
        llvm::BasicBlock *fallthrough_bb = nullptr;
        for (auto itb = cur_f->begin(); itb != cur_f->end(); ++itb) {
          if (&*itb == parent_bb) {
            auto next = std::next(itb);
            if (next != cur_f->end()) fallthrough_bb = &*next;
            break;
          }
        }
        if (!fallthrough_bb) {
          fallthrough_bb = llvm::BasicBlock::Create(llvm_ctx, parent_bb->getName() + ".cont", cur_f);
        }

        builder.CreateCondBr(cond, target_bb, fallthrough_bb);
      } break;

      case Mir::OP_PUSH_ARG: {
        llvm::Value *arg = visit_operand(instr.left, true);
        arg_stack.push_back(arg);
      } break;

      case Mir::OP_CALL: {
        Mir::Function *mir_fn = m.functions[instr.left.temp];
        llvm::Value *fnval = function_table[mir_fn];
        uint32_t nargs = 0;
        if (instr.right.tag == Mir::Operand::OPERAND_IMMEDIATE_VALUE && instr.right.immediate.tag == Mir::Constant::CONST_INT) {
          nargs = (uint32_t)instr.right.immediate.int_lit;
        } else if (instr.right.tag == Mir::Operand::OPERAND_CONSTANT && instr.right.constant.tag == Mir::Constant::CONST_INT) {
          nargs = (uint32_t)instr.right.constant.int_lit;
        } else {
          throw_error("CALL missing arg count", instr.span);
        }

        if (nargs > arg_stack.size()) throw_error("Not enough arguments on stack for CALL", instr.span);

        std::vector<llvm::Value *> call_args;
        call_args.reserve(nargs);
        size_t start = arg_stack.size() - nargs;

        for (size_t i = start; i < arg_stack.size(); ++i) {
          call_args.push_back(arg_stack[i]);
        }

        arg_stack.erase(arg_stack.begin() + start, arg_stack.end());

        llvm::CallInst *call = nullptr;
        call = builder.CreateCall((llvm::FunctionType *)llvm_typeof(mir_fn->type), fnval, call_args);

        if (call && !call->getType()->isVoidTy()) {
          temps[instr.dest.temp] = call;
        }
      } break;

      case Mir::OP_CALL_PTR: {
        llvm::Value *f = temps[instr.left.temp];
        uint32_t nargs = 0;
        if (instr.right.tag == Mir::Operand::OPERAND_IMMEDIATE_VALUE && instr.right.immediate.tag == Mir::Constant::CONST_INT) {
          nargs = (uint32_t)instr.right.immediate.int_lit;
        } else if (instr.right.tag == Mir::Operand::OPERAND_CONSTANT && instr.right.constant.tag == Mir::Constant::CONST_INT) {
          nargs = (uint32_t)instr.right.constant.int_lit;
        } else {
          throw_error("CALL_PTR missing arg count", instr.span);
        }

        if (nargs > arg_stack.size()) {
          throw_error("Not enough arguments on stack for CALL_PTR", instr.span);
        }

        std::vector<llvm::Value *> call_args;
        call_args.reserve(nargs);
        size_t start = arg_stack.size() - nargs;
        for (size_t i = start; i < arg_stack.size(); ++i) call_args.push_back(arg_stack[i]);
        arg_stack.erase(arg_stack.begin() + start, arg_stack.end());

        llvm::CallInst *call = builder.CreateCall((llvm::FunctionType *)llvm_typeof(instr.left.type), f, call_args);

        if (call && !call->getType()->isVoidTy()) {
          builder.CreateStore(call, temps[instr.dest.temp]);
        }
      } break;

      case Mir::OP_RET: {
        // left = value
        llvm::Value *val = visit_operand(instr.left, true);
        builder.CreateRet(val);
      } break;

      case Mir::OP_RET_VOID: {
        builder.CreateRetVoid();
      } break;

      case Mir::OP_CAST: {
        // dest=result, left=value, right=type_index
        llvm::Value *v = visit_operand(instr.left, true);
        llvm::Value *casted = cast_scalar(v, instr.left.type, instr.right.type);
        if (!casted) {
          throw_error("Unsupported cast", instr.span);
          break;
        }
        temps[instr.dest.temp] = casted;
      } break;

      case Mir::OP_BITCAST: {
        // bitcast value to type (no reinterpretation of bits)
        llvm::Value *v = visit_operand(instr.left, true);
        llvm::Type *to_ty = llvm_typeof(instr.right.type);
        llvm::Value *bc = builder.CreateBitCast(v, to_ty, "bitcasttmp");
        temps[instr.dest.temp] = bc;
      } break;

      case Mir::OP_GEP: {
        llvm::Value *base = visit_operand(instr.left, false);
        llvm::Value *index = visit_operand(instr.right, true);
        Temporary &temp = f->temps[instr.dest.temp];
        llvm::Type *pointee = llvm_typeof(temp.type->get_element_type());
        temps[instr.dest.temp] = builder.CreateGEP(pointee, base, index, f->temps[instr.dest.temp].name.get_str());
      } break;
    }
  }
}

llvm::Value *LLVM_Emitter::visit_operand(Operand o, bool do_load = true) {
  switch (o.tag) {
    // ignored
    case Mir::Operand::OPERAND_BASIC_BLOCK:  // only for jumps.
    case Mir::Operand::OPERAND_TYPE:         // only for special instructions. see mir.hpp
    case Mir::Operand::OPERAND_NULL:         // unused operand.
      return nullptr;                        // TODO: figure out if this is valid

    case Mir::Operand::OPERAND_TEMP: {
      if (o.temp >= temps.size() || temps[o.temp] == nullptr) {
        throw_error(std::format("INTERNAL COMPILER ERROR: temp t{} not allocated", o.temp), {});
        return nullptr;
      }
      // TODO: figure out why we're ever trying to load function pointers into non-pointer types
      if (do_load) {
        return builder.CreateLoad(llvm_typeof(o.type), temps[o.temp]);
      } else {
        return temps[o.temp];
      }
    }
    case Mir::Operand::OPERAND_CONSTANT: {
      switch (o.constant.tag) {
        case Mir::Constant::CONST_INT:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.constant.int_lit);
        case Mir::Constant::CONST_FLOAT:
          return llvm::ConstantFP::get(llvm_typeof(o.type), o.constant.float_lit);
        case Mir::Constant::CONST_BOOL:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.constant.bool_lit);
        case Mir::Constant::CONST_CHAR:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.constant.char_lit);
        case Mir::Constant::CONST_STRING:
          return builder.CreateGlobalString(o.constant.string_lit.get_str());
        default:
          return nullptr;
      }
    }
    case Mir::Operand::OPERAND_IMMEDIATE_VALUE:
      switch (o.immediate.tag) {
        case Mir::Constant::CONST_INT:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.immediate.int_lit);
        case Mir::Constant::CONST_FLOAT:
          return llvm::ConstantFP::get(llvm_typeof(o.type), o.immediate.float_lit);
        case Mir::Constant::CONST_BOOL:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.immediate.bool_lit);
        case Mir::Constant::CONST_CHAR:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.immediate.char_lit);
        case Mir::Constant::CONST_STRING:
          return builder.CreateGlobalString(o.immediate.string_lit.get_str());
        default:
          return nullptr;
      }
      break;
  }
}

llvm::Value *LLVM_Emitter::pointer_binary(llvm::Value *left, llvm::Value *right, const Instruction &instr) {
  llvm::Value *result = nullptr;
  llvm::Type *elem_ty = nullptr;
  if (instr.left.type && instr.left.type->is_pointer()) {
    elem_ty = llvm_typeof(instr.left.type->get_element_type());
  } else if (instr.right.type && instr.right.type->is_pointer()) {
    elem_ty = llvm_typeof(instr.right.type->get_element_type());
  }

  if ((instr.opcode == Mir::OP_LOGICAL_NOT || instr.opcode == Mir::OP_NOT) && !right) {
    Type *bool_mir = bool_type();
    llvm::Value *v = cast_scalar(left, instr.left.type, bool_mir);
    if (!v) {
      throw_error("Pointer logical-not: failed to cast pointer to bool via MIR type system", instr.span);
      return nullptr;
    }
    if (!v->getType()->isIntegerTy(1)) {
      v = builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv");
    }
    result = builder.CreateNot(v, "ptr_not");
    return result;
  }

  if (!right) {
    throw_error("Pointer arithmetic: missing right operand", instr.span);
    return nullptr;
  }

  if (instr.opcode == Mir::OP_EQ || instr.opcode == Mir::OP_NE) {
    if (left->getType() == right->getType() && left->getType()->isPointerTy()) {
      result =
          builder.CreateICmp(instr.opcode == Mir::OP_EQ ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE, left, right, "ptrcmp");
      return result;
    }

    llvm::Type *i8ptr = llvm::PointerType::get(llvm::Type::getInt8Ty(llvm_ctx), 0);
    llvm::Value *lbc = builder.CreateBitCast(left, i8ptr, "ptrcmp_lcast");
    llvm::Value *rbc = builder.CreateBitCast(right, i8ptr, "ptrcmp_rcast");
    result = builder.CreateICmp(instr.opcode == Mir::OP_EQ ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE, lbc, rbc, "ptrcmp");
    return result;
  }

  if (instr.opcode == Mir::OP_LT || instr.opcode == Mir::OP_LE || instr.opcode == Mir::OP_GT || instr.opcode == Mir::OP_GE) {
    unsigned bits = data_layout.getPointerSizeInBits(0);
    llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
    llvm::Value *li = builder.CreatePtrToInt(left, intptr_ty, "ptr2int_l");
    llvm::Value *ri = builder.CreatePtrToInt(right, intptr_ty, "ptr2int_r");

    llvm::CmpInst::Predicate pred;
    if (instr.opcode == Mir::OP_LT)
      pred = llvm::CmpInst::ICMP_ULT;
    else if (instr.opcode == Mir::OP_LE)
      pred = llvm::CmpInst::ICMP_ULE;
    else if (instr.opcode == Mir::OP_GT)
      pred = llvm::CmpInst::ICMP_UGT;
    else
      pred = llvm::CmpInst::ICMP_UGE;

    result = builder.CreateICmp(pred, li, ri, "ptrcmp");
    return result;
  }

  if (instr.opcode == Mir::OP_SUB) {
    if (!elem_ty) {
      throw_error("Pointer subtraction: missing pointee type in MIR", instr.span);
      return nullptr;
    }
    result = builder.CreatePtrDiff(elem_ty, left, right, "ptrsub");
    return result;
  }

  throw_error("Unsupported operation on pointers", instr.span);
  return nullptr;
}

llvm::Value *LLVM_Emitter::pointer_unary(llvm::Value *operand, const Instruction &instr) {
  if (instr.opcode == Mir::OP_LOGICAL_NOT) {
    Type *bool_mir = bool_type();
    llvm::Value *v = cast_scalar(operand, instr.left.type, bool_mir);
    if (!v) {
      throw_error("Pointer logical-not: failed to cast pointer to bool via MIR type system", instr.span);
      return nullptr;
    }
    if (!v->getType()->isIntegerTy(1)) {
      v = builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv");
    }
    return builder.CreateNot(v, "ptr_lnot");
  }

  if (instr.opcode == Mir::OP_NOT) {
    if (!operand->getType()->isPointerTy()) {
      throw_error("Bitwise-not on non-pointer operand", instr.span);
      return nullptr;
    }
    unsigned bits = data_layout.getPointerSizeInBits(0);
    llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
    llvm::Value *i = builder.CreatePtrToInt(operand, intptr_ty, "ptr2int");
    llvm::Value *noti = builder.CreateNot(i, "ptr_not_int");
    return builder.CreateIntToPtr(noti, operand->getType(), "ptr_not");
  }

  if (instr.opcode == Mir::OP_NEG) {
    if (!operand->getType()->isPointerTy()) {
      throw_error("Unary minus on non-pointer operand", instr.span);
      return nullptr;
    }
    unsigned bits = data_layout.getPointerSizeInBits(0);
    llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
    llvm::Value *i = builder.CreatePtrToInt(operand, intptr_ty, "ptr2int");
    llvm::Value *zero = llvm::ConstantInt::get(intptr_ty, 0);
    llvm::Value *neg = builder.CreateSub(zero, i, "ptr_neg_int");
    return builder.CreateIntToPtr(neg, operand->getType(), "ptr_neg");
  }

  throw_error("Unsupported unary operation on pointer", instr.span);
  return nullptr;
}