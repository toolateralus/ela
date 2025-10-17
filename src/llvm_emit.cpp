#include "llvm_emit.hpp"

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

    // NOTE: compound-assignment lowering should be done earlier into load/add/store sequences
    // in MIR. MIR doesn't define separate compound opcodes, so we don't handle them here.

    // Assignment (store into memory)
    case Mir::OP_STORE:
      builder.CreateStore(right, left);
      return right;

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

    // Assignment (store into memory)
    case Mir::OP_STORE:
      builder.CreateStore(right, left);
      return right;

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
    case Mir::OP_STORE:
      builder.CreateStore(right, left);
      return right;

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