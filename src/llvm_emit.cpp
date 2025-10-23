#include "llvm_emit.hpp"
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>
#include "core.hpp"
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
      llvm::Value *v = perform_cast(left, instr.left.type, bool_type(), &unused);
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
          exit(1);
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
    }
  }

  // Pointer addition (ptr + int or int + ptr)
  if (instr.opcode == Mir::OP_ADD) {
    if (!elem_ty) throw_error("Pointer addition: missing pointee type in MIR", instr.span);

    if (left->getType()->isPointerTy()) {
      result = builder.CreateGEP(elem_ty, left, right, "ptradd");
    } else if (right->getType()->isPointerTy()) {
      result = builder.CreateGEP(elem_ty, right, left, "ptradd");
    } else {
      throw_error("Pointer addition requires at least one pointer", instr.span);
    }
    return result;
  }

  // Logical AND / OR (for pointer truthiness)
  if (instr.opcode == Mir::OP_LOGICAL_AND || instr.opcode == Mir::OP_LOGICAL_OR) {
    llvm::Value *l =
        builder.CreateICmpNE(left, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(left->getType())), "ptr_truth_l");
    llvm::Value *r = builder.CreateICmpNE(right, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(right->getType())),
                                          "ptr_truth_r");
    return (instr.opcode == Mir::OP_LOGICAL_AND) ? builder.CreateAnd(l, r, "ptr_land") : builder.CreateOr(l, r, "ptr_lor");
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

    if (left->getType()->isPointerTy() || right->getType()->isPointerTy())
      result = builder.CreateIntToPtr(result, elem_ty, "ptr_back");

    return result;
  }

  throw_error(std::format("Unsupported operation on pointer: op = {}", (uint8_t)instr.opcode), instr.span);
  return nullptr;
}

llvm::Value *LLVM_Emitter::pointer_unary(llvm::Value *operand, const Instruction &instr) {
  if (instr.opcode == Mir::OP_LOGICAL_NOT) {
    llvm::Value *cmp = builder.CreateICmpEQ(
        operand, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(operand->getType())), "ptr_is_null");
    return builder.CreateZExt(cmp, llvm::Type::getInt1Ty(llvm_ctx), "ptr_lnot");
  }

  if (instr.opcode == Mir::OP_NOT) {
    unsigned bits = data_layout.getPointerSizeInBits(0);
    llvm::Type *intptr_ty = llvm::Type::getIntNTy(llvm_ctx, bits);
    llvm::Value *i = builder.CreatePtrToInt(operand, intptr_ty, "ptr2int");
    llvm::Value *noti = builder.CreateNot(i, "ptr_not_int");
    return builder.CreateIntToPtr(noti, operand->getType(), "ptr_not");
  }

  if (instr.opcode == Mir::OP_NEG) {
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
      return builder.CreateAShr(left, right, "shrtmp");

    case Mir::OP_LOGICAL_AND:
      return builder.CreateAnd(builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0)),
                               builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0)), "landtmp");
    case Mir::OP_LOGICAL_OR:
      return builder.CreateOr(builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0)),
                              builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0)), "lortmp");
    default:
      return nullptr;
  }
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
      return builder.CreateLShr(left, right, "shrtmp");

    case Mir::OP_LOGICAL_AND:
      return builder.CreateAnd(builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0)),
                               builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0)), "landtmp");
    case Mir::OP_LOGICAL_OR:
      return builder.CreateOr(builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0)),
                              builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0)), "lortmp");
    default:
      return nullptr;
  }
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

    case Mir::OP_LOGICAL_AND:
      return builder.CreateAnd(builder.CreateFCmpONE(left, llvm::ConstantFP::get(left->getType(), 0.0)),
                               builder.CreateFCmpONE(right, llvm::ConstantFP::get(right->getType(), 0.0)), "flandtmp");
    case Mir::OP_LOGICAL_OR:
      return builder.CreateOr(builder.CreateFCmpONE(left, llvm::ConstantFP::get(left->getType(), 0.0)),
                              builder.CreateFCmpONE(right, llvm::ConstantFP::get(right->getType(), 0.0)), "flortmp");
    default:
      return nullptr;
  }
}
llvm::Value *LLVM_Emitter::perform_cast(llvm::Value *value, Type *from, Type *to, Type **new_type) {
  if (from == to) {
    *new_type = to;
    return value;
  }

  *new_type = to;
  llvm::Type *llvm_to = llvm_typeof(to);
  
  const auto cast_integer = [&](llvm::Value *val, llvm::Type *from_ty, llvm::Type *to_ty, bool from_signed) -> llvm::Value * {
    unsigned from_bits = from_ty->getIntegerBitWidth();
    unsigned to_bits = to_ty->getIntegerBitWidth();

    if (from_bits == to_bits) {
      return val;
    } else if (from_bits < to_bits) {
      if (from_signed) {
        return builder.CreateSExt(val, to_ty, "sexttmp");
      } else {
        return builder.CreateZExt(val, to_ty, "zexttmp");
      }
    } else {
      return builder.CreateTrunc(val, to_ty, "trunctmp");
    }
  };

  // --- Pointer casting ---
  if (from->is_pointer() && to->is_pointer()) {
    value = builder.CreateBitCast(value, llvm_to, "bitcasttmp");
  } else if (from->is_pointer() && to->is_integer()) {
    value = builder.CreatePtrToInt(value, llvm_to, "ptr2int_cast");
  } else {
    // --- Enum handling ---
    if (from->kind == TYPE_ENUM) {
      from = from->info->as<EnumTypeInfo>()->underlying_type;
    }
    if (to->kind == TYPE_ENUM) {
      to = to->info->as<EnumTypeInfo>()->underlying_type;
    }

    if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR) {
      throw_error("unable to cast non-scalar type at this point", {});
    }

    ScalarTypeInfo *from_info = from->info->as<ScalarTypeInfo>();
    ScalarTypeInfo *to_info = to->info->as<ScalarTypeInfo>();

    // Boolean casting block 
    if (to_info->scalar_type == TYPE_BOOL) {
      if (from_info->is_integral) {
        value = builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0), "booltmp");
      } else {
        value = builder.CreateFCmpONE(value, llvm::ConstantFP::get(value->getType(), 0.0), "booltmp");
      }
    }
    // Integer casting block
    else if (to_info->is_integral) {
      if (from_info->scalar_type == TYPE_BOOL) {
        value = builder.CreateZExt(value, llvm_to, "zexttmp");
      } else if (from_info->is_integral) {
        value = cast_integer(value, value->getType(), llvm_to, from_info->is_signed());
      } else {
        if (to_info->is_signed()) {
          value = builder.CreateFPToSI(value, llvm_to, "fptositmp");
        } else {
          value = builder.CreateFPToUI(value, llvm_to, "fptouitmp");
        }
      }
    }
    // Floating-point casting block
    else {
      if (from_info->scalar_type == TYPE_BOOL) {
        value = builder.CreateUIToFP(value, llvm_to, "uitofptmp");
      } else if (from_info->is_integral) {
        if (from_info->is_signed()) {
          value = builder.CreateSIToFP(value, llvm_to, "sitofptmp");
        } else {
          value = builder.CreateUIToFP(value, llvm_to, "uitofptmp");
        }
      } else {
        unsigned from_bits = value->getType()->getPrimitiveSizeInBits();
        unsigned to_bits = llvm_to->getPrimitiveSizeInBits();
        if (from_bits < to_bits) {
          value = builder.CreateFPExt(value, llvm_to, "fpexttmp");
        } else if (from_bits > to_bits) {
          value = builder.CreateFPTrunc(value, llvm_to, "fptrunctmp");
        }
      }
    }
  }

  return value;
}

void LLVM_Emitter::emit_module() {
  file = dbg.get_file_scope(m.functions[0]->span);

  for (const auto &f : m.functions) {
    llvm::FunctionType *func_type = (llvm::FunctionType *)llvm_typeof(f->type);
    llvm::Function *llvm_f =
        llvm::Function::Create(func_type, llvm::GlobalValue::ExternalLinkage, f->name.str(), llvm_module.get());
    function_table[f] = llvm_f;
  }

  for (const Global_Variable *gv : m.global_variables) {
    llvm::Type *gv_type = llvm_typeof(gv->type);
    llvm::Constant *initializer = llvm::Constant::getNullValue(gv_type);
    llvm::GlobalVariable *llvm_gv =
        new llvm::GlobalVariable(*llvm_module, gv_type, false, llvm::GlobalValue::InternalLinkage, initializer, gv->name.str());
    global_variables[gv] = llvm_gv;
  }

  for (const auto &f : m.functions) {
    if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_EXTERN)) {
      continue;
    }
    emit_function(f, function_table[f]);
  }

  dbg.pop_scope(DIManager::Scope::CU);
}

void LLVM_Emitter::register_constructor(llvm::Function *func, uint32_t priority) {
  llvm::LLVMContext &ctx = llvm_module->getContext();

  // Type of ctor entry: { i32, void ()*, i8* } (i8* for stupid C++ legacy ABI stuff)
  llvm::StructType *ctor_struct_ty =
      llvm::StructType::get(llvm::Type::getInt32Ty(ctx), func->getType(), llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0));

  llvm::Constant *ctor_entry =
      llvm::ConstantStruct::get(ctor_struct_ty, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), priority), func,
                                llvm::ConstantPointerNull::get(llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0)));

  llvm::GlobalVariable *global_ctors = llvm_module->getNamedGlobal("llvm.global_ctors");
  if (!global_ctors) {
    llvm::ArrayType *array_ty = llvm::ArrayType::get(ctor_struct_ty, 1);
    global_ctors = new llvm::GlobalVariable(*llvm_module, array_ty, false, llvm::GlobalValue::AppendingLinkage,
                                            llvm::ConstantArray::get(array_ty, {ctor_entry}), "llvm.global_ctors");
  } else {
    auto *old_array = llvm::cast<llvm::ConstantArray>(global_ctors->getInitializer());
    std::vector<llvm::Constant *> new_elems;
    for (unsigned i = 0, e = old_array->getNumOperands(); i != e; ++i) {
      new_elems.push_back(llvm::cast<llvm::Constant>(old_array->getOperand(i)));
    }
    new_elems.push_back(ctor_entry);
    llvm::ArrayType *new_array_ty = llvm::ArrayType::get(ctor_struct_ty, new_elems.size());
    global_ctors->setInitializer(llvm::ConstantArray::get(new_array_ty, new_elems));
  }
}

void convert_function_flags(Mir::Function *f, llvm::Function *ir_f) {
  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_NO_RETURN)) {
    ir_f->addFnAttr(llvm::Attribute::NoReturn);
  }
  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_INLINE)) {
    ir_f->addFnAttr(llvm::Attribute::AlwaysInline);
  }
  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_CONSTRUCTOR_0)) {
  }
  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_CONSTRUCTOR_1)) {
  }
}

void LLVM_Emitter::emit_function(Mir::Function *f, llvm::Function *ir_f) {
  convert_function_flags(f, ir_f);
  if (!compile_command.has_flag("nl")) {
    dbg.enter_function_scope(f->type, this, ir_f, f->name.str(), f->span);
  }

  bb_table.reserve(f->basic_blocks.size());

  llvm::BasicBlock *entry_bb = nullptr;

  for (auto *bb : f->basic_blocks) {  // pre-create the basic blocks so we can easily do jumps and flips on it and shit
    llvm::BasicBlock *ir_bb = llvm::BasicBlock::Create(llvm_ctx, bb->label.str(), ir_f);
    if (!entry_bb) {
      entry_bb = ir_bb;
    }
    bb_table[bb] = ir_bb;
  }

  // Declare the parameters and store them in the temps
  temps.reserve(f->temps.size());

  size_t i = 0;
  for (const auto &param_temp : f->parameter_temps) {
    llvm::Argument *llvm_param = ir_f->getArg(i);
    insert_temp(param_temp.index, f, llvm_param);
    ++i;
  }

  for (const auto &bb : f->basic_blocks) {
    builder.SetInsertPoint(bb_table[bb]);
    emit_basic_block(bb, f, entry_bb);
  }

  temps.clear();
  bb_table.clear();
  builder.ClearInsertionPoint();

  if (!compile_command.has_flag("nl")) {
    dbg.pop_scope(DIManager::Scope::Subroutine);
  }

  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_CONSTRUCTOR_0)) {
    register_constructor(ir_f, 0);
  }

  if (HAS_FLAG(f->flags, Function::FUNCTION_FLAGS_IS_CONSTRUCTOR_1)) {
    register_constructor(ir_f, 1);
  }
}

void LLVM_Emitter::emit_basic_block(Mir::Basic_Block *bb, Mir::Function *f, llvm::BasicBlock *entry_bb) {
  if (!compile_command.has_flag("nl")) {
    dbg.enter_lexical_scope(dbg.current_scope(DIManager::Scope::Subroutine), bb->code.front().span);
  }

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

        Type *left_ty = instr.left.type;
        Type *right_ty = instr.right.type;

        if (left_ty->kind == TYPE_ENUM) {
          Type *enum_underlying = left_ty->info->as<EnumTypeInfo>()->underlying_type;
          left = perform_cast(left, left_ty, enum_underlying, &left_ty);
        }
        if (right_ty->kind == TYPE_ENUM) {
          Type *enum_underlying = right_ty->info->as<EnumTypeInfo>()->underlying_type;
          right = perform_cast(right, right_ty, enum_underlying, &right_ty);
        }

        if (instr.left.type->is_pointer() || instr.right.type->is_pointer()) {
          llvm::Value *result = create_dbg(pointer_binary(left, right, instr), instr.span);
          insert_temp(instr.dest.temp, f, result);
          break;
        }

        ScalarTypeInfo *left_info = left_ty->info->as<ScalarTypeInfo>();
        ScalarTypeInfo *right_info = right_ty->info->as<ScalarTypeInfo>();
        Type *new_type = nullptr;
        right = perform_cast(right, right_ty, left_ty, &new_type);
        if (new_type) {
          right_ty = new_type;
          right_info = new_type->info->as<ScalarTypeInfo>();
        }

        llvm::Value *result = nullptr;
        if (left_info->is_signed() && right_info->is_signed()) {
          result = create_dbg(binary_signed(left, right, instr.opcode), instr.span);
        } else if (left_info->is_integral && right_info->is_integral) {
          result = create_dbg(binary_unsigned(left, right, instr.opcode), instr.span);
        } else if (left_info->is_float() && right_info->is_float()) {
          result = create_dbg(binary_fp(left, right, instr.opcode), instr.span);
        } else {
          throw_error(
              std::format("Unsupported operand types for binary op: '{}' '{}'", left_ty->to_string(), right_ty->to_string()),
              instr.span);
        }

        insert_temp(instr.dest.temp, f, result);
      } break;

      case Mir::OP_LOGICAL_NOT: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        Type *unused = nullptr;
        v = perform_cast(v, instr.left.type, bool_type(), &unused);
        if (!v->getType()->isIntegerTy(1))
          v = create_dbg(builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "boolconv"), instr.span);
        llvm::Value *not_val = create_dbg(builder.CreateNot(v, "nottmp"), instr.span);
        insert_temp(instr.dest.temp, f, not_val);
      } break;

      case Mir::OP_NOT: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        llvm::Value *res = create_dbg(builder.CreateNot(v, "nottmp"), instr.span);
        insert_temp(instr.dest.temp, f, res);
      } break;

      case Mir::OP_LOAD: {
        llvm::Value *val = visit_operand(instr.left, instr.span);
        llvm::Value *loaded = create_dbg(
            builder.CreateLoad(llvm_typeof(instr.left.type->get_element_type()), val, f->temps[instr.dest.temp].name.str()),
            instr.span);
        insert_temp(instr.dest.temp, f, loaded);
      } break;

      case Mir::OP_NEG: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        llvm::Value *res;
        if (instr.left.type->is_pointer())
          res = create_dbg(pointer_unary(v, instr), instr.span);
        else {
          auto info = instr.left.type->info->as<ScalarTypeInfo>();
          res = create_dbg(info->is_float() ? builder.CreateFNeg(v, "fnegtmp") : builder.CreateNeg(v, "negtmp"), instr.span);
        }
        insert_temp(instr.dest.temp, f, res);
      } break;

      case Mir::OP_STORE: {
        llvm::Value *val = visit_operand(instr.right, instr.span);

        if (instr.left.tag == Mir::Operand::OPERAND_GLOBAL_VARIABLE_REFERENCE) {
          auto it = global_variables.find(instr.left.gv);
          if (it != global_variables.end()) {
            create_dbg(builder.CreateStore(val, it->second), instr.span);
          } else {
            throw_error("somehow a global variable wasn't declared in LLVM backend", instr.span);
          }

        } else {
          assert(is_temporary_valid(instr.left.temp) && "Invalid temporary in STORE");
          llvm::Value *ptr = temps[instr.left.temp].value;
          create_dbg(builder.CreateStore(val, ptr), instr.span);
        }

        break;
      }

      case Mir::OP_ALLOCA: {
        uint32_t index = instr.dest.temp;
        Temporary &temp = f->temps[index];
        auto [alloc_ty, alloc_ty_di] = llvm_typeof_impl(temp.type->get_element_type());
        llvm::IRBuilder<> tmp_builder(entry_bb, entry_bb->begin());
        llvm::AllocaInst *ai = tmp_builder.CreateAlloca(alloc_ty, nullptr, temp.name.str());

        if (!compile_command.has_flag("nl")) {
          dbg.create_variable(dbg.current_scope(DIManager::Scope::Lexical), temp.name.str(), instr.span, alloc_ty_di, ai, builder,
                              llvm_ctx);
        }

        insert_temp(index, f, ai);
      } break;

      case Mir::OP_LOAD_FN_PTR: {
        llvm::Value *fnptr = function_table[m.functions[instr.right.temp]];
        insert_temp(instr.dest.temp, f, fnptr);
      } break;

      case Mir::OP_JMP: {
        auto it = bb_table.find(instr.left.bb);
        if (it == bb_table.end()) throw_error("Unknown target basic block", instr.span);
        create_dbg(builder.CreateBr(it->second), instr.span);
      } break;

      case Mir::OP_JMP_TRUE: {
        llvm::Value *cond = visit_operand(instr.right, instr.span);
        Type *unused = nullptr;
        cond = perform_cast(cond, instr.right.type, bool_type(), &unused);
        if (!cond->getType()->isIntegerTy(1))
          cond = create_dbg(builder.CreateICmpEQ(cond, llvm::ConstantInt::get(cond->getType(), 1), "boolconv"), instr.span);

        auto target_it = bb_table.find(instr.left.bb_pair.target);
        auto fallthrough_it = bb_table.find(instr.left.bb_pair.fallthrough);
        if (target_it == bb_table.end() || fallthrough_it == bb_table.end()) throw_error("Unknown basic block", instr.span);

        create_dbg(builder.CreateCondBr(cond, target_it->second, fallthrough_it->second), instr.span);
      } break;

      case Mir::OP_PUSH_ARG:
        arg_stack.push_back(visit_operand(instr.left, instr.span));
        break;

      case Mir::OP_CALL: {
        Mir::Function *mir_fn = m.functions[instr.left.temp];
        llvm::Value *fnval = function_table[mir_fn];
        uint32_t nargs = instr.right.imm.int_lit;

        auto start = arg_stack.end() - nargs;
        std::vector<llvm::Value *> call_args(std::make_move_iterator(start), std::make_move_iterator(arg_stack.end()));
        arg_stack.erase(start, arg_stack.end());

        llvm::CallInst *call;
        if (mir_fn->type_info->return_type != void_type()) {
          call =
              create_dbg(builder.CreateCall(llvm_fn_typeof(mir_fn->type), fnval, call_args, f->temps[instr.dest.temp].name.str()),
                         instr.span);
          insert_temp(instr.dest.temp, f, call);
        } else {
          call = create_dbg(builder.CreateCall(llvm_fn_typeof(mir_fn->type), fnval, call_args), instr.span);
        }

      } break;

      case Mir::OP_CALL_PTR: {
        // Get the function value from the operand
        llvm::Value *val = visit_operand(instr.left, instr.span);

        llvm::FunctionCallee fn;

        if (auto *func = llvm::dyn_cast<llvm::Function>(val)) {
          fn = llvm::FunctionCallee(func);
        } else {
          auto *funcTy = llvm::cast<llvm::FunctionType>(llvm_typeof(instr.left.type->get_element_type()));
          fn = llvm::FunctionCallee(funcTy, val);
        }

        // Prepare arguments
        uint32_t nargs = instr.right.imm.int_lit;
        auto start = arg_stack.end() - nargs;
        std::vector<llvm::Value *> call_args(std::make_move_iterator(start), std::make_move_iterator(arg_stack.end()));
        arg_stack.erase(start, arg_stack.end());

        // Emit the call with debug info
        llvm::CallInst *call = create_dbg(builder.CreateCall(fn, call_args), instr.span);

        if (call && !call->getType()->isVoidTy()) {
          insert_temp(instr.dest.temp, f, call);
        }
      } break;

      case Mir::OP_RET: {
        llvm::Value *val = visit_operand(instr.left, instr.span);
        create_dbg(builder.CreateRet(val), instr.span);
        return;
      } break;

      case Mir::OP_RET_VOID:
        create_dbg(builder.CreateRetVoid(), instr.span);
        break;

      case Mir::OP_CAST: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        Type *new_type = nullptr;
        llvm::Value *casted = create_dbg(perform_cast(v, instr.left.type, instr.right.type, &new_type), instr.span);
        if (new_type) instr.right.type = new_type;
        insert_temp(instr.dest.temp, f, casted);
      } break;

      case Mir::OP_BITCAST: {
        llvm::Value *v = visit_operand(instr.left, instr.span);
        llvm::Type *to_ty = llvm_typeof(instr.right.type);
        llvm::Value *bc = create_dbg(builder.CreateBitCast(v, to_ty, "bitcasttmp"), instr.span);
        insert_temp(instr.dest.temp, f, bc);
      } break;

      case Mir::OP_GEP: {
        llvm::Value *base = visit_operand(instr.left, instr.span);
        llvm::Value *index = visit_operand(instr.right, instr.span);
        Temporary &temp = f->temps[instr.dest.temp];

        Type *element_type = temp.type->get_element_type();

        if (element_type == void_type()) {
          throw_error("tried to GEP into a void", instr.span);
        }

        llvm::Type *pointee = llvm_typeof(element_type);
        llvm::Value *gep = create_dbg(builder.CreateGEP(pointee, base, index, f->temps[instr.dest.temp].name.str()), instr.span);
        insert_temp(instr.dest.temp, f, gep);
      } break;
      case Mir::OP_UNREACHABLE: {
        builder.CreateUnreachable();
      } break;
      case Mir::OP_ZERO_INIT: {
        static llvm::Type *i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
        static llvm::Type *i8_ptr_ty = llvm::PointerType::get(i8_ty, 0);
        static llvm::Type *i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
        static llvm::Type *i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

        static llvm::Value *zero = llvm::ConstantInt::get(i8_ty, 0);
        static llvm::Value *is_volatile = llvm::ConstantInt::get(i1_ty, 0);
        static std::vector<llvm::Type *> memset_arg_types = {i8_ptr_ty, i64_ty};

        llvm::Value *ptr = visit_operand(instr.left, instr.span);
        Type *ty = instr.right.type;
        uint64_t size = data_layout.getTypeAllocSize(llvm_typeof(ty));

        llvm::Value *cast_ptr =
            create_dbg(builder.CreateBitCast(ptr, llvm::PointerType::get(i8_ty, 0), "memset_ptr"), instr.span);

        llvm::Value *size_val = llvm::ConstantInt::get(i64_ty, size);

        llvm::Function *memset_fn =
            llvm::Intrinsic::getOrInsertDeclaration(llvm_module.get(), llvm::Intrinsic::memset, memset_arg_types);

        create_dbg(builder.CreateCall(memset_fn, {cast_ptr, zero, size_val, is_volatile}), instr.span);
      } break;
    }
  }

  if (!compile_command.has_flag("nl")) {
    dbg.pop_scope(DIManager::Scope::Lexical);
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
      if (!is_temporary_valid(o.temp)) {
        for (auto [temp, _] : temps) {
          printf("existing: %u\n", temp);
        }

        throw_error(std::format("use of undeclared temp '{}'", o.temp), span);
      }
      auto val = temps[o.temp].value;
      assert(val);
      return val;
    }
    case Mir::Operand::OPERAND_GLOBAL_VARIABLE_REFERENCE: {
      llvm::GlobalVariable *gv = global_variables[o.gv];
      if (!gv) {
        throw_error(std::format("use of undeclared global variable: {}", o.gv->name.str()), span);
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
          return builder.CreateGlobalString(unescape_string_lit(o.imm.string_lit.str()));
        case Mir::Constant::CONST_NULLPTR:
          return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(llvm_typeof(o.type)));
        case Mir::Constant::CONST_INVALID:
          throw_error("invalid constant", {});
          exit(1);
          break;
      }
      break;
  }
}

llvm::DISubprogram *DIManager::enter_function_scope(const Type *type, LLVM_Emitter *emitter, llvm::Function *function,
                                                    const std::string &name, const Span &span) {
  auto [basename, dirpath, line, column] = extract_span(span);
  llvm::DIFile *file = get_file_scope(span);

  std::vector<llvm::Metadata *> di_types;

  auto [ret_llvm_ty, ret_di_ty] = emitter->llvm_typeof_impl(type->info->as<FunctionTypeInfo>()->return_type);
  di_types.push_back(ret_di_ty);

  FunctionTypeInfo *fty_info = type->info->as<FunctionTypeInfo>();
  for (size_t i = 0; i < fty_info->params_len; ++i) {
    auto [p_llvm_ty, p_di_ty] = emitter->llvm_typeof_impl(fty_info->parameter_types[i]);
    di_types.push_back(p_di_ty);
  }

  llvm::DISubroutineType *func_type = di_builder->createSubroutineType(di_builder->getOrCreateTypeArray(di_types));

  llvm::DIScope *scope = cu;
  llvm::DISubprogram *subprogram = di_builder->createFunction(scope,                   // scope
                                                              name,                    // Name
                                                              name,                    // Linkage name
                                                              file,                    // File
                                                              line,                    // LineNo
                                                              func_type,               // DISubroutineType*
                                                              line,                    // ScopeLine
                                                              llvm::DINode::FlagZero,  // Flags
                                                              llvm::DISubprogram::SPFlagDefinition);

  function->setSubprogram(subprogram);
  push_scope(subprogram, Scope::Subroutine);
  return subprogram;
}
