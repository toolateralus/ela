#include "llvm.hpp"
#include "ast.hpp"
#include "core.hpp"
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

  auto left_llvm_ty = llvm_typeof(left_ty);
  auto right_llvm_ty = llvm_typeof(right_ty);

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

  return nullptr;
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
                                        llvm::Type *left_type) {
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
      auto result = builder->CreateSDiv(loadedLeft, right, "compdivtmp");
      builder->CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder->CreateLoad(left_type, left, "loadtmp");
      auto result = builder->CreateSRem(loadedLeft, right, "compmodtmp");
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

    // Assignment
    case TType::Assign:
      builder->CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::binary_scalars(llvm::IRBuilder<> *builder, llvm::Value *left, llvm::Value *right, TType op,
                                         Type *left_ty, Type *right_ty, ScalarTypeInfo *left_info,
                                         ScalarTypeInfo *right_info) {
  Token temp_token = {};
  temp_token.type = op;
  const auto is_assignment = temp_token.is_comp_assign();

  if (is_assignment) {
    switch (left_info->scalar_type) {
      case TYPE_S8:
      case TYPE_S16:
      case TYPE_S32:
      case TYPE_S64:
        binary_signed(builder, left, right, op, LLVMEmitter::llvm_typeof(left_ty));
      case TYPE_U8:
      case TYPE_U16:
      case TYPE_U32:
      case TYPE_U64:
      case TYPE_FLOAT:
      case TYPE_DOUBLE:
      case TYPE_CHAR:
      case TYPE_BOOL:
        break;
    }
  }

  // switch (left_info->scalar_type) {
  //   case TYPE_S8: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_S16: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_S32: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_S64: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_U8: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_U16: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_U32: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_U64: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_FLOAT: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_DOUBLE: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_CHAR: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   case TYPE_BOOL: {
  //     switch (right_info->scalar_type) {
  //       case TYPE_S8:
  //       case TYPE_S16:
  //       case TYPE_S32:
  //       case TYPE_S64:
  //       case TYPE_U8:
  //       case TYPE_U16:
  //       case TYPE_U32:
  //       case TYPE_U64:
  //       case TYPE_FLOAT:
  //       case TYPE_DOUBLE:
  //       case TYPE_CHAR:
  //       case TYPE_BOOL:
  //         break;
  //       default:
  //         return nullptr;
  //     }
  //   } break;
  //   default:
  //     break;
  // }
}
