#include "llvm.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

void LLVMEmitter::visit_program(ASTProgram *node) {
  size_t index = 0;

  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
      this->module->setSourceFileName(node->source_range.begin_location.filename());
      dbg.enter_file_scope(node->source_range);
    }
    visit_node(statement);
    index++;
  }

  dbg.pop_scope();
}

static inline std::string get_mangled_name(Symbol *symbol) {
  if (symbol->is_local() ||
      (symbol->is_function() && HAS_FLAG(symbol->function.declaration->flags, FUNCTION_IS_FOREIGN))) {
    return symbol->name.get_str();
  }

  auto full_name = symbol->scope->full_name();
  if (!full_name.empty()) {
    full_name += "$";
  }
  full_name += symbol->name.get_str();
  return full_name;
};

void LLVMEmitter::visit_function_declaration(ASTFunctionDeclaration *node) {
  if (node->generic_parameters.size()) {
    return;
  }
  if (node->is_emitted) {
    return;
  } else {
    node->is_emitted = true;
  }

  std::printf("emitting function %s\n", node->name.get_str().c_str());

  auto name = get_mangled_name(ctx.scope->lookup(node->name));
  auto return_type = global_get_type(node->return_type->resolved_type);

  std::vector<llvm::Type *> param_types;
  for (const auto &param : node->params->params) {
    auto param_type = global_get_type(param->resolved_type);
    param_types.push_back(llvm_typeof(param_type));
  }

  auto func_type = llvm::FunctionType::get(llvm_typeof(return_type), param_types, node->params->is_varargs);
  auto func = llvm::Function::Create(func_type, llvm::Function::LinkageTypes::ExternalLinkage, name, module.get());

  auto function_symbol = ctx.scope->local_lookup(name);
  function_symbol->llvm_value = func;

  if (HAS_FLAG(node->flags, FUNCTION_IS_FOREIGN)) {
    return;
  } else if (HAS_FLAG(node->flags, FUNCTION_IS_FORWARD_DECLARED)) {
    return;
  } else if (HAS_FLAG(node->flags, FUNCTION_IS_TEST)) {
    return;
  }

  auto entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", func);
  builder.SetInsertPoint(entry_block);

  auto old_scope = ctx.scope;
  ctx.set_scope(node->block.get()->scope);
  dbg.enter_function_scope(dbg.current_scope(), func, name, node->source_range);

  auto index = 0;
  for (auto &param : func->args()) {
    auto ast_param = node->params->params[index];
    if (ast_param->tag == ASTParamDecl::Normal) {
      Symbol *symbol = ctx.scope->local_lookup(ast_param->normal.name);
      symbol->llvm_value = &param;
    } else {
      Symbol *symbol = ctx.scope->local_lookup("self");
      symbol->llvm_value = &param;
    }
    index++;
  }

  visit_node(node->block.get());

  if (!builder.GetInsertBlock()->getTerminator()) {
    if (return_type->id == void_type()) {
      builder.CreateRetVoid();
    } else {
      builder.CreateRet(llvm::Constant::getNullValue(llvm_typeof(return_type)));
    }
  }
  dbg.pop_scope();

  ctx.scope = old_scope;
}

llvm::Value *LLVMEmitter::visit_block(ASTBlock *node) {
  auto old_scope = ctx.scope;
  ctx.scope = node->scope;

  for (const auto &statement : node->statements) {
    visit_node(statement);
  }

  ctx.scope = old_scope;
  return nullptr;
}

llvm::Value *LLVMEmitter::visit_return(ASTReturn *node) {
  if (node->expression.get()) {
    return builder.CreateRet(visit_expr(node->expression.get()));
  } else {
    return builder.CreateRetVoid();
  }
}

llvm::Value *LLVMEmitter::visit_literal(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer: {
      auto info = global_get_type(node->resolved_type)->get_info()->as<ScalarTypeInfo>();
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(info->size * 8, std::stoll(node->value.get_str()), true));
    }
    case ASTLiteral::Float: {
      return llvm::ConstantFP::get(llvm_ctx, llvm::APFloat(std::stod(node->value.get_str())));
    }
    case ASTLiteral::String: {
      return builder.CreateGlobalString(node->value.get_str());
    }
    case ASTLiteral::Char: {
      auto info = global_get_type(node->resolved_type)->get_info()->as<ScalarTypeInfo>();
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(info->size * 8, node->value.get_str()[0], false));
    }
    case ASTLiteral::Bool: {
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(1, node->value.get_str() == "true" ? 1 : 0, false));
    }
    case ASTLiteral::Null: {
      static auto ptr_ty = builder.getPtrTy(0);
      return llvm::ConstantPointerNull::get(ptr_ty);
    }
    default:
      return nullptr;
  }
}

llvm::Value *LLVMEmitter::visit_bin_expr(ASTBinExpr *node) {
  auto left_ty = global_get_type(node->left->resolved_type);
  auto right_ty = global_get_type(node->right->resolved_type);
  auto expr_ty = global_get_type(node->resolved_type);

  if (node->is_operator_overload) {
    /// TODO:
    /// call the fricken funkchtyun
    return nullptr;
  }

  if (left_ty->get_ext().has_extensions() || right_ty->get_ext().has_extensions()) {
    /// TODO:
    /// pointer arithmetic.
    return nullptr;
  }

  // We can assume that if we aren't an operator overload at this point,
  // that we have 2 scalars;
  auto left_info = left_ty->get_info()->as<ScalarTypeInfo>();
  auto right_info = right_ty->get_info()->as<ScalarTypeInfo>();

  auto left = visit_expr(node->left);
  auto right = visit_expr(node->right);

  return binary_scalars(&builder, left, right, node->op.type, expr_ty, left_info, right_info);
}

llvm::Value *LLVMEmitter::visit_unary_expr(ASTUnaryExpr *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_method_call(ASTMethodCall *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_path(ASTPath *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_pattern_match(ASTPatternMatch *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_dyn_of(ASTDyn_Of *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_type_of(ASTType_Of *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_type(ASTType *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_call(ASTCall *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_expr_statement(ASTExprStatement *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_dot_expr(ASTDotExpr *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_subscript(ASTSubscript *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_initializer_list(ASTInitializerList *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_range(ASTRange *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_switch(ASTSwitch *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_tuple(ASTTuple *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_cast(ASTCast *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_lambda(ASTLambda *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_size_of(ASTSize_Of *node) {
  auto type = global_get_type(node->resolved_type);
  auto llvm_type = llvm_typeof(type);
  auto data_layout = module->getDataLayout();
  auto size = data_layout.getTypeStoreSize(llvm_type);
  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm_ctx), size);
}

void LLVMEmitter::visit_struct_declaration(ASTStructDeclaration *node) {}
void LLVMEmitter::visit_module(ASTModule *node) {}
void LLVMEmitter::visit_import(ASTImport *node) {}

void LLVMEmitter::visit_params_decl(ASTParamsDecl *node) {}
void LLVMEmitter::visit_param_decl(ASTParamDecl *node) {}
void LLVMEmitter::visit_variable(ASTVariable *node) {}
void LLVMEmitter::visit_arguments(ASTArguments *node) {}
void LLVMEmitter::visit_continue(ASTContinue *node) {}
void LLVMEmitter::visit_break(ASTBreak *node) {}
void LLVMEmitter::visit_for(ASTFor *node) {}
void LLVMEmitter::visit_if(ASTIf *node) {}
void LLVMEmitter::visit_else(ASTElse *node) {}
void LLVMEmitter::visit_while(ASTWhile *node) {}
void LLVMEmitter::visit_enum_declaration(ASTEnumDeclaration *node) {}
void LLVMEmitter::visit_tuple_deconstruction(ASTTupleDeconstruction *node) {}
void LLVMEmitter::visit_alias(ASTAlias *node) {}
void LLVMEmitter::visit_impl(ASTImpl *node) {}
void LLVMEmitter::visit_defer(ASTDefer *node) {}
void LLVMEmitter::visit_choice_declaration(ASTChoiceDeclaration *node) {}
void LLVMEmitter::visit_interface_declaration(ASTInterfaceDeclaration *node) {}
void LLVMEmitter::visit_where(ASTWhere *node) {}

llvm::Value *LLVMEmitter::binary_signed(llvm::IRBuilder<> *builder, llvm::Value *left, llvm::Value *right, TType op,
                                        llvm::Type *expr_type) {
  switch (op) {
    case TType::Add:
      return builder->CreateAdd(left, right, "addtmp");
    case TType::Sub:
      return builder->CreateSub(left, right, "subtmp");
    case TType::Mul:
      return builder->CreateMul(left, right, "multmp");
    case TType::Div:
      return builder->CreateSDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder->CreateSRem(left, right, "modtmp");
    case TType::LT:
      return builder->CreateICmpSLT(left, right, "lttmp");
    case TType::GT:
      return builder->CreateICmpSGT(left, right, "gttmp");
    case TType::LE:
      return builder->CreateICmpSLE(left, right, "letmp");
    case TType::GE:
      return builder->CreateICmpSGE(left, right, "getmp");
    case TType::EQ:
      return builder->CreateICmpEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder->CreateICmpNE(left, right, "neqtmp");
    case TType::And:
      return builder->CreateAnd(left, right, "andtmp");
    case TType::Or:
      return builder->CreateOr(left, right, "ortmp");
    case TType::Xor:
      return builder->CreateXor(left, right, "xortmp");
    case TType::SHL:
      return builder->CreateShl(left, right, "shltmp");
    case TType::SHR:
      return builder->CreateAShr(left, right, "shrtmp"); // Arithmetic shift

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateAdd(loadedLeft, right, "compaddtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateSub(loadedLeft, right, "compsubtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateMul(loadedLeft, right, "compmultmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateSDiv(loadedLeft, right, "compdivtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateSRem(loadedLeft, right, "compmodtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompAnd: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateAnd(loadedLeft, right, "compandtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompOr: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateOr(loadedLeft, right, "comportmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompXor: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateXor(loadedLeft, right, "compxortmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSHL: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateShl(loadedLeft, right, "compshltmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSHR: {
      auto loadedLeft = builder->CreateLoad(expr_type, left, "loadtmp");
      auto result = builder->CreateAShr(loadedLeft, right, "compshrtmp");
      builder->CreateStore(result, left);
      return result;
    }

    // Assignment
    case TType::Assign:
      builder->CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::binary_unsigned(llvm::IRBuilder<> *builder, llvm::Value *left, llvm::Value *right, TType op,
                                          llvm::Type *left_type) {
  switch (op) {
    case TType::Add:
      return builder->CreateAdd(left, right, "addtmp");
    case TType::Sub:
      return builder->CreateSub(left, right, "subtmp");
    case TType::Mul:
      return builder->CreateMul(left, right, "multmp");
    case TType::Div:
      return builder->CreateUDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder->CreateURem(left, right, "modtmp");
    case TType::LT:
      return builder->CreateICmpULT(left, right, "lttmp");
    case TType::GT:
      return builder->CreateICmpUGT(left, right, "gttmp");
    case TType::LE:
      return builder->CreateICmpULE(left, right, "letmp");
    case TType::GE:
      return builder->CreateICmpUGE(left, right, "getmp");
    case TType::EQ:
      return builder->CreateICmpEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder->CreateICmpNE(left, right, "neqtmp");
    case TType::And:
      return builder->CreateAnd(left, right, "andtmp");
    case TType::Or:
      return builder->CreateOr(left, right, "ortmp");
    case TType::Xor:
      return builder->CreateXor(left, right, "xortmp");
    case TType::SHL:
      return builder->CreateShl(left, right, "shltmp");
    case TType::SHR:
      return builder->CreateLShr(left, right, "shrtmp"); // Logical shift

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateAdd(loadedLeft, right, "compaddtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateSub(loadedLeft, right, "compsubtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateMul(loadedLeft, right, "compmultmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateUDiv(loadedLeft, right, "compdivtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateURem(loadedLeft, right, "compmodtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompAnd: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateAnd(loadedLeft, right, "compandtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompOr: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateOr(loadedLeft, right, "comportmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompXor: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateXor(loadedLeft, right, "compxortmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSHL: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateShl(loadedLeft, right, "compshltmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSHR: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateLShr(loadedLeft, right, "compshrtmp");
      builder->CreateStore(result, left);
      return result;
    }

    // Assignment
    case TType::Assign:
      builder->CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::binary_fp(llvm::IRBuilder<> *builder, llvm::Value *left, llvm::Value *right, TType op,
                                    llvm::Type *left_type) {
  switch (op) {
    case TType::Add:
      return builder->CreateFAdd(left, right, "addtmp");
    case TType::Sub:
      return builder->CreateFSub(left, right, "subtmp");
    case TType::Mul:
      return builder->CreateFMul(left, right, "multmp");
    case TType::Div:
      return builder->CreateFDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder->CreateFRem(left, right, "modtmp");
    case TType::LT:
      return builder->CreateFCmpOLT(left, right, "lttmp");
    case TType::GT:
      return builder->CreateFCmpOGT(left, right, "gttmp");
    case TType::LE:
      return builder->CreateFCmpOLE(left, right, "letmp");
    case TType::GE:
      return builder->CreateFCmpOGE(left, right, "getmp");
    case TType::EQ:
      return builder->CreateFCmpOEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder->CreateFCmpONE(left, right, "neqtmp");

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateFAdd(loadedLeft, right, "compaddtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateFSub(loadedLeft, right, "compsubtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateFMul(loadedLeft, right, "compmultmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateFDiv(loadedLeft, right, "compdivtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateFRem(loadedLeft, right, "compmodtmp");
      builder->CreateStore(result, left);
      return result;
    }
    // Assignment
    case TType::Assign:
      builder->CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *cast_scalar(llvm::IRBuilder<> *builder, llvm::Value *value, llvm::Type *type, bool from_signed,
                         bool to_signed) {
  if (value->getType() == type) {
    return value;
  }

  if (value->getType()->isIntegerTy() && type->isIntegerTy()) {
    if (value->getType()->getIntegerBitWidth() < type->getIntegerBitWidth()) {
      if (from_signed) {
        return builder->CreateSExt(value, type, "sexttmp");
      } else {
        return builder->CreateZExt(value, type, "zexttmp");
      }
    } else if (value->getType()->getIntegerBitWidth() > type->getIntegerBitWidth()) {
      return builder->CreateTrunc(value, type, "trunctmp");
    }
  } else if (value->getType()->isFloatingPointTy() && type->isFloatingPointTy()) {
    if (value->getType()->getPrimitiveSizeInBits() < type->getPrimitiveSizeInBits()) {
      return builder->CreateFPExt(value, type, "fpexttmp");
    } else if (value->getType()->getPrimitiveSizeInBits() > type->getPrimitiveSizeInBits()) {
      return builder->CreateFPTrunc(value, type, "fptrunctmp");
    }
  } else if (value->getType()->isIntegerTy() && type->isFloatingPointTy()) {
    if (from_signed) {
      return builder->CreateSIToFP(value, type, "sitofptmp");
    } else {
      return builder->CreateUIToFP(value, type, "uitofptmp");
    }
  } else if (value->getType()->isFloatingPointTy() && type->isIntegerTy()) {
    if (to_signed) {
      return builder->CreateFPToSI(value, type, "fptositmp");
    } else {
      return builder->CreateFPToUI(value, type, "fptouitmp");
    }
  } else if (value->getType()->isPointerTy() && type->isPointerTy()) {
    return builder->CreateBitCast(value, type, "bitcasttmp");
  }

  return nullptr;
}

llvm::Value *LLVMEmitter::binary_scalars(llvm::IRBuilder<> *builder, llvm::Value *left, llvm::Value *right, TType op,
                                         Type *expr_ty, ScalarTypeInfo *left_info, ScalarTypeInfo *right_info) {
  auto expr_ty_info = expr_ty->get_info()->as<ScalarTypeInfo>();

  Token temp_token = {};
  temp_token.type = op;
  const bool is_assignment = temp_token.is_comp_assign() || op == TType::Assign;

  auto type = llvm_typeof(expr_ty);
  right = cast_scalar(builder, right, type, right_info->is_signed(), expr_ty_info->is_signed());
  if (!is_assignment) {
    left = cast_scalar(builder, left, type, left_info->is_signed(), expr_ty_info->is_signed());
  }

  switch (expr_ty_info->scalar_type) {
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
      return binary_signed(builder, left, right, op, type);
    case TYPE_CHAR:
    case TYPE_BOOL:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
      return binary_unsigned(builder, left, right, op, type);
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
      return binary_fp(builder, left, right, op, type);
      break;
    case TYPE_VOID:
      break;
  }

  return nullptr;
}
