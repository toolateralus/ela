#include "llvm_emit.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>
#include <system_error>
#include "mir.hpp"
#include "type.hpp"

llvm::Value *LLVM_Emitter::pointer_binary(llvm::Value *left, llvm::Value *right, const Instruction &instr) {
  llvm::Value *result = nullptr;
  llvm::Type *elem_ty = nullptr;

  if (instr.left.type && instr.left.type->is_pointer()) {
    elem_ty = llvm_typeof(instr.left.type->get_element_type());
  } else if (instr.right.type && instr.right.type->is_pointer()) {
    elem_ty = llvm_typeof(instr.right.type->get_element_type());
  }

  // Unary NOT handled separately
  if (!right) {
    if (instr.opcode == Mir::OP_LOGICAL_NOT || instr.opcode == Mir::OP_NOT) {
      Type *unused = nullptr;
      llvm::Value *v = cast_scalar(left, instr.left.type, bool_type(), &unused);
      if (!v) throw_error("Pointer logical-not: failed to cast pointer to bool via MIR type system", instr.span);
      if (!v->getType()->isIntegerTy(1)) v = builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv");
      return (instr.opcode == Mir::OP_LOGICAL_NOT) ? builder.CreateNot(v, "ptr_lnot") : builder.CreateNot(v, "ptr_not");
    }
    throw_error("Pointer arithmetic: missing right operand", instr.span);
    return nullptr;
  }

  // Pointer comparisons
  if (instr.opcode == Mir::OP_EQ || instr.opcode == Mir::OP_NE || instr.opcode == Mir::OP_LT || instr.opcode == Mir::OP_LE ||
      instr.opcode == Mir::OP_GT || instr.opcode == Mir::OP_GE) {
    llvm::Value *l = left;
    llvm::Value *r = right;

    if (l->getType() != r->getType()) {
      llvm::Type *i8ptr = llvm::PointerType::get(llvm::Type::getInt8Ty(llvm_ctx), 0);
      l = builder.CreateBitCast(left, i8ptr, "ptrcmp_lcast");
      r = builder.CreateBitCast(right, i8ptr, "ptrcmp_rcast");
    }

    if (instr.opcode == Mir::OP_EQ || instr.opcode == Mir::OP_NE) {
      result = builder.CreateICmp(instr.opcode == Mir::OP_EQ ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE, l, r, "ptrcmp");
    } else {
      unsigned bits = data_layout.getPointerSizeInBits(0);
      llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
      llvm::Value *li = builder.CreatePtrToInt(l, intptr_ty, "ptr2int_l");
      llvm::Value *ri = builder.CreatePtrToInt(r, intptr_ty, "ptr2int_r");
      llvm::CmpInst::Predicate pred;
      switch (instr.opcode) {
        case Mir::OP_LT:
          pred = llvm::CmpInst::ICMP_ULT;
          break;
        case Mir::OP_LE:
          pred = llvm::CmpInst::ICMP_ULE;
          break;
        case Mir::OP_GT:
          pred = llvm::CmpInst::ICMP_UGT;
          break;
        case Mir::OP_GE:
          pred = llvm::CmpInst::ICMP_UGE;
          break;
        default:
          throw_error("Invalid pointer comparison", instr.span);
          return nullptr;
      }
      result = builder.CreateICmp(pred, li, ri, "ptrcmp");
    }
    return result;
  }

  // Pointer subtraction (ptr - ptr) or (ptr - int)
  if (instr.opcode == Mir::OP_SUB) {
    if (!elem_ty) throw_error("Pointer subtraction: missing pointee type in MIR", instr.span);

    if (left->getType()->isPointerTy() && right->getType()->isPointerTy()) {
      return builder.CreatePtrDiff(elem_ty, left, right, "ptrsub");
    } else if (left->getType()->isPointerTy()) {
      return builder.CreateGEP(elem_ty, left, builder.CreateNeg(right, "neg_offset"), "ptr_sub_int");
    } else if (right->getType()->isPointerTy()) {
      return builder.CreateGEP(elem_ty, right, left, "ptr_add_int");  // swap for int - ptr
    } else {
      throw_error("Pointer subtraction requires at least one pointer", instr.span);
      return nullptr;
    }
  }

  // Pointer addition (ptr + int or int + ptr)
  if (instr.opcode == Mir::OP_ADD) {
    if (!elem_ty) throw_error("Pointer addition: missing pointee type in MIR", instr.span);

    if (left->getType()->isPointerTy()) {
      result = builder.CreateGEP(elem_ty, left, right, "ptradd");
    } else if (right->getType()->isPointerTy()) {
      result = builder.CreateGEP(elem_ty, right, left, "ptradd");  // commutative
    } else {
      throw_error("Pointer addition requires at least one pointer", instr.span);
      return nullptr;
    }
    return result;
  }

  // MUL, DIV, MOD (pointer → integer)
  if (instr.opcode == Mir::OP_MUL || instr.opcode == Mir::OP_DIV || instr.opcode == Mir::OP_MOD) {
    unsigned bits = data_layout.getPointerSizeInBits(0);
    llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
    llvm::Value *li = left->getType()->isPointerTy() ? builder.CreatePtrToInt(left, intptr_ty, "ptr2int_l") : left;
    llvm::Value *ri = right->getType()->isPointerTy() ? builder.CreatePtrToInt(right, intptr_ty, "ptr2int_r") : right;

    switch (instr.opcode) {
      case Mir::OP_MUL:
        result = builder.CreateMul(li, ri, "ptr_mul");
        break;
      case Mir::OP_DIV:
        result = builder.CreateUDiv(li, ri, "ptr_div");
        break;
      case Mir::OP_MOD:
        result = builder.CreateURem(li, ri, "ptr_mod");
        break;
      default:
        break;
    }

    if (left->getType()->isPointerTy() || right->getType()->isPointerTy()) {
      result = builder.CreateIntToPtr(result, elem_ty, "ptr_back");
    }
    return result;
  }

  throw_error(std::format("Unsupported operation on pointer: op = {}", (uint8_t)instr.opcode), instr.span);
  return nullptr;
}

llvm::Value *LLVM_Emitter::pointer_unary(llvm::Value *operand, const Instruction &instr) {
  if (instr.opcode == Mir::OP_LOGICAL_NOT) {
    Type *unused = nullptr;
    llvm::Value *v = cast_scalar(operand, instr.left.type, bool_type(), &unused);
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

llvm::Value *LLVM_Emitter::cast_scalar(llvm::Value *value, Type *from, Type *to, Type **new_type) {
  if (from == to) {
    return value;
  }

  llvm::Type *llvm_to = llvm_typeof(to);

  if (to->is_pointer()) {
    if (from->is_pointer()) {
      return builder.CreateBitCast(value, llvm_to, "bitcasttmp");
    }
  }

  ScalarTypeInfo *from_info = from->info->as<ScalarTypeInfo>();
  ScalarTypeInfo *to_info = to->info->as<ScalarTypeInfo>();

  *new_type = to;

  if (to_info->scalar_type == TYPE_BOOL) {
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

  for (const Global_Variable *gv : m.global_variables) {
    llvm::Type *gv_type = llvm_typeof(gv->type);
    llvm::Constant *initializer = nullptr;
    llvm::GlobalVariable *llvm_gv = new llvm::GlobalVariable(*llvm_module, gv_type, false, llvm::GlobalValue::InternalLinkage,
                                                             initializer, gv->name.get_str());
    global_variables[gv] = llvm_gv;
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
    insert_temp(i, f, llvm_param);
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
  for (auto &instr : bb->code) {
    switch (instr.opcode) {
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
        llvm::Value *left = visit_operand(instr.left, instr.span);
        llvm::Value *right = visit_operand(instr.right, instr.span);
        
        if (instr.left.type->is_pointer() || instr.right.type->is_pointer()) {
          llvm::Value *result = pointer_binary(left, right, instr);
          insert_temp(instr.dest.temp, f, result);
          break;
        }

        ScalarTypeInfo *left_info = instr.left.type->info->as<ScalarTypeInfo>();
        ScalarTypeInfo *right_info = instr.right.type->info->as<ScalarTypeInfo>();
        llvm::Value *result = nullptr;

        Type *new_type = nullptr;
        right = cast_scalar(right, instr.right.type, instr.left.type, &new_type);

        // We performed a cast so now we have to update to the new type so the below logic is correct
        if (new_type) {
          instr.right.type = new_type;
          right_info = new_type->info->as<ScalarTypeInfo>();
        }

        if (false) {
          std::error_code ec;
          llvm::raw_fd_ostream stream("-", ec);
          left->getType()->print(stream);
          stream.write('\n');
          right->getType()->print(stream);
          stream.write('\n');
        }

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

        insert_temp(instr.dest.temp, f, result);
      } break;

      case Mir::OP_LOGICAL_NOT: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        Type *unused = nullptr;
        v = cast_scalar(v, instr.left.type, bool_type(), &unused);

        if (!v->getType()->isIntegerTy(1)) {
          v = builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv");
        }

        llvm::Value *not_val = builder.CreateNot(v, "nottmp");
        insert_temp(instr.dest.temp, f, not_val);
      } break;

      case Mir::OP_NOT: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        llvm::Value *res = builder.CreateNot(v, "nottmp");
        insert_temp(instr.dest.temp, f, res);
      } break;

      case Mir::OP_LOAD: {
        llvm::Value *val = visit_operand(instr.left, instr.span);
        auto loaded = builder.CreateLoad(llvm_typeof(instr.left.type->get_element_type()), val);
        insert_temp(instr.dest.temp, f, loaded);
      } break;

      case Mir::OP_NEG: {
        if (instr.left.type->is_pointer()) {
          llvm::Value *v = visit_operand(instr.left, instr.span);
          llvm::Value *res = pointer_unary(v, instr);
          insert_temp(instr.dest.temp, f, res);
          break;
        }
        llvm::Value *v = visit_operand(instr.left, instr.span);
        auto info = instr.left.type->info->as<ScalarTypeInfo>();
        llvm::Value *res = info->is_float() ? builder.CreateFNeg(v, "fnegtmp") : builder.CreateNeg(v, "negtmp");
        insert_temp(instr.dest.temp, f, res);
      } break;

      case Mir::OP_STORE: {
        llvm::Value *val = visit_operand(instr.right, instr.span);
        llvm::Value *ptr = temps[instr.left.temp].value;
        builder.CreateStore(val, ptr);
      } break;

      case Mir::OP_ALLOCA: {
        uint32_t index = instr.dest.temp;
        Temporary &temp = f->temps[index];
        llvm::Value *ai = builder.CreateAlloca(llvm_typeof(temp.type->get_element_type()), nullptr, temp.name.get_str());
        insert_temp(index, f, ai);
      } break;

      case Mir::OP_LOAD_FN_PTR: {
        llvm::Value *fnptr = visit_operand(instr.right, instr.span);
        insert_temp(instr.dest.temp, f, fnptr);
      } break;

      case Mir::OP_JMP: {
        Mir::Basic_Block *target_mb = instr.left.bb;
        auto it = bb_table.find(target_mb);
        if (it == bb_table.end()) {
          throw_error("Unknown target basic block", instr.span);
        }
        builder.CreateBr(it->second);
      } break;

      case Mir::OP_JMP_TRUE: {
        Mir::Basic_Block *target_mir_bb = instr.left.bb_pair.target;
        Mir::Basic_Block *fallthrough_bb = instr.left.bb_pair.fallthrough;

        llvm::Value *cond = visit_operand(instr.right, instr.span);
        Type *unused = nullptr;
        cond = cast_scalar(cond, instr.right.type, bool_type(), &unused);

        if (!cond->getType()->isIntegerTy(1)) {
          cond = builder.CreateICmpEQ(cond, llvm::ConstantInt::get(cond->getType(), 1), "boolconv");
        }

        auto it = bb_table.find(target_mir_bb);
        if (it == bb_table.end()) {
          throw_error("Unknown target basic block", instr.span);
        }
        llvm::BasicBlock *target_bb = it->second;

        auto fallthrough_it = bb_table.find(fallthrough_bb);
        if (fallthrough_it == bb_table.end()) {
          throw_error("Unknown fallthrough basic block", instr.span);
        }
        builder.CreateCondBr(cond, target_bb, fallthrough_it->second);
      } break;

      case Mir::OP_PUSH_ARG: {
        llvm::Value *arg = visit_operand(instr.left, instr.span);
        arg_stack.push_back(arg);
      } break;

      case Mir::OP_CALL: {
        Mir::Function *mir_fn = m.functions[instr.left.temp];
        llvm::Value *fnval = function_table[mir_fn];
        uint32_t nargs = (uint32_t)instr.right.imm.int_lit;

        auto start = arg_stack.end() - nargs;
        std::vector<llvm::Value *> call_args(std::make_move_iterator(start), std::make_move_iterator(arg_stack.end()));
        arg_stack.erase(start, arg_stack.end());
        llvm::CallInst *call = builder.CreateCall(llvm_fn_typeof(mir_fn->type), fnval, call_args);
        if (mir_fn->type_info->return_type != void_type()) {
          insert_temp(instr.dest.temp, f, call);
        }
      } break;

      case Mir::OP_CALL_PTR: {
        llvm::Value *fn = visit_operand(instr.left, instr.span);
        const uint32_t n_args = (uint32_t)instr.right.imm.int_lit;
        const auto start = arg_stack.end() - n_args;
        const std::vector<llvm::Value *> call_args(std::make_move_iterator(start), std::make_move_iterator(arg_stack.end()));
        arg_stack.erase(start, arg_stack.end());
        llvm::CallInst *call = builder.CreateCall(llvm_fn_typeof(instr.left.type), fn, call_args);
        if (call && !call->getType()->isVoidTy()) {
          insert_temp(instr.dest.temp, f, call);
        }
      } break;

      case Mir::OP_RET: {
        llvm::Value *val = visit_operand(instr.left, instr.span);
        builder.CreateRet(val);
        return;
      } break;

      case Mir::OP_RET_VOID: {
        builder.CreateRetVoid();
      } break;

      case Mir::OP_CAST: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        Type *new_type = nullptr;
        llvm::Value *casted = cast_scalar(v, instr.left.type, instr.right.type, &new_type);
        if (new_type) {
          instr.right.type = new_type;
        }
        insert_temp(instr.dest.temp, f, casted);
      } break;

      case Mir::OP_BITCAST: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        llvm::Type *to_ty = llvm_typeof(instr.right.type);
        llvm::Value *bc = builder.CreateBitCast(v, to_ty, "bitcasttmp");
        insert_temp(instr.dest.temp, f, bc);
      } break;

      case Mir::OP_GEP: {
        llvm::Value *base = visit_operand(instr.left, instr.span);
        llvm::Value *index = visit_operand(instr.right, instr.span);
        Temporary &temp = f->temps[instr.dest.temp];
        llvm::Type *pointee = llvm_typeof(temp.type->get_element_type());
        llvm::Value *gep = builder.CreateGEP(pointee, base, index, f->temps[instr.dest.temp].name.get_str());
        insert_temp(instr.dest.temp, f, gep);
      } break;
      case Mir::OP_ZERO_INIT: {
        // crap we need.
        static llvm::Type *i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
        static llvm::Type *i8_ptr_ty = llvm::PointerType::get(i8_ty, 0);
        static llvm::Type *i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
        static llvm::Type *i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
        static llvm::Value *zero = llvm::ConstantInt::get(i8_ty, 0);
        static llvm::Value *is_volatile = llvm::ConstantInt::get(i1_ty, 0);
        static std::vector<llvm::Type *> memset_arg_types = {i8_ptr_ty, i64_ty};

        // PTR is a pointer to the memory to zero, TY is the type (non-pointer) to zero-init
        llvm::Value *ptr = visit_operand(instr.left, instr.span);
        Type *ty = instr.right.type;  // Non-pointer type ref
        uint64_t size = data_layout.getTypeAllocSize(llvm_typeof(ty));
        llvm::Value *cast_ptr = builder.CreateBitCast(ptr, llvm::PointerType::get(i8_ty, 0), "memset_ptr");
        llvm::Value *size_val = llvm::ConstantInt::get(i64_ty, size);
        llvm::Function *memset_fn =
            llvm::Intrinsic::getOrInsertDeclaration(llvm_module.get(), llvm::Intrinsic::memset, memset_arg_types);

        builder.CreateCall(memset_fn, {cast_ptr, zero, size_val, is_volatile});
        builder.CreateCall(memset_fn, {cast_ptr, zero, size_val, is_volatile});
        llvm::Value *val = builder.CreateLoad(llvm_typeof(ty), ptr);
        insert_temp(instr.dest.temp, f, val);
        break;
      }
    }
  }
}

llvm::Value *LLVM_Emitter::visit_operand(Operand o, Span span) {
  switch (o.tag) {
    // ignored
    case Mir::Operand::OPERAND_BASIC_BLOCK:  // only for jumps.
    case Mir::Operand::OPERAND_TYPE:         // only for special instructions. see mir.hpp
    case Mir::Operand::OPERAND_NULL:         // unused operand.
    case Mir::Operand::OPERAND_BASIC_BLOCK_PAIR:
      return nullptr;  // TODO: figure out if this is valid

    case Mir::Operand::OPERAND_TEMP: {
      if (o.temp >= temps.size()) {
        throw_error("use of undeclared temp", span);
      }
      return temps[o.temp].value;
    }
    case Mir::Operand::OPERAND_GLOBAL_VARIABLE_REFERENCE: {
      llvm::GlobalVariable *gv = global_variables[o.gv];
      if (!gv) {
        throw_error("use of undeclared global variable", span);
      }
      return gv;
    }
    case Mir::Operand::OPERAND_IMMEDIATE_VALUE:
      switch (o.imm.tag) {
        case Mir::Constant::CONST_INT:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.imm.int_lit);
        case Mir::Constant::CONST_FLOAT:
          return llvm::ConstantFP::get(llvm_typeof(o.type), o.imm.float_lit);
        case Mir::Constant::CONST_BOOL:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.imm.bool_lit);
        case Mir::Constant::CONST_CHAR:
          return llvm::ConstantInt::get(llvm_typeof(o.type), o.imm.char_lit);
        case Mir::Constant::CONST_STRING:
          return builder.CreateGlobalString(unescape_string_lit(o.imm.string_lit.get_str()));
        default:
          return nullptr;
      }
      break;
  }
}
