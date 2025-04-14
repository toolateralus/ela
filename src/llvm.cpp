#include "llvm.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <alloca.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Triple.h>

void LLVMEmitter::visit_program(ASTProgram *node) {
  this->module->setSourceFileName(node->source_range.begin_location.filename());
  file = dbg.enter_file_scope(node->source_range);
  di_builder->createCompileUnit(llvm::dwarf::DW_LANG_C, file, "0.01", false, "", 0);

  size_t index = 0;
  for (auto &statement : node->statements) {
    if (index == node->end_of_bootstrap_index) {
      ctx.set_scope(node->scope);
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

  /*
    I'm not sure why we have to do this, but if we don't the symbol often comes up
    null and crashes out.
  */
  Symbol *sym;
  if (node->declaring_type != Type::INVALID_TYPE_ID) {
    sym = global_get_type(node->declaring_type)->get_info()->scope->local_lookup(node->name);
  } else {
    sym = ctx.scope->lookup(node->name);
  }

  auto name = get_mangled_name(sym);
  auto return_type = global_get_type(node->return_type->resolved_type);

  std::vector<llvm::Type *> param_types;
  for (const auto &param : node->params->params) {
    auto param_type = global_get_type(param->resolved_type);
    param_types.push_back(llvm_typeof(param_type));
  }

  auto func_type = llvm::FunctionType::get(llvm_typeof(return_type), param_types, node->params->is_varargs);
  auto func = llvm::Function::Create(func_type, llvm::Function::LinkageTypes::ExternalLinkage, name, module.get());

  sym->llvm_value = func;

  if (HAS_FLAG(node->flags, FUNCTION_IS_FOREIGN) || HAS_FLAG(node->flags, FUNCTION_IS_TEST)) {
    return;
  } else if (HAS_FLAG(node->flags, FUNCTION_IS_FORWARD_DECLARED)) {
    return;
  } else if (HAS_FLAG(node->flags, FUNCTION_IS_INLINE)) {
    func->setLinkage(llvm::GlobalValue::LinkOnceAnyLinkage);
  } else if (HAS_FLAG(node->flags, FUNCTION_IS_STATIC)) {
    func->setLinkage(llvm::GlobalValue::InternalLinkage);
  }

  auto entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", func);
  builder.SetInsertPoint(entry_block);

  auto old_scope = ctx.scope;
  ctx.set_scope(node->scope);

  auto subprogram = dbg.enter_function_scope(dbg.current_scope(), func, name, node->source_range);
  func->setSubprogram(subprogram);

  auto index = 0;
  for (auto param = func->arg_begin(); param != func->arg_end(); ++param) {
    auto ast_param = node->params->params[index];
    if (ast_param->tag == ASTParamDecl::Normal) {
      Symbol *symbol = ctx.scope->local_lookup(ast_param->normal.name);
      symbol->llvm_value = param;
    } else {
      Symbol *symbol = ctx.scope->local_lookup("self");
      symbol->llvm_value = param;
    }
    index++;
  }

  visit_block(node->block.get());

  if (!builder.GetInsertBlock()->getTerminator()) {
    if (return_type->id == void_type()) {
      builder.CreateRetVoid();
    } else {
      builder.CreateRet(llvm::Constant::getNullValue(llvm_typeof(return_type)));
    }
  }
  dbg.pop_scope();

  ctx.scope = old_scope;

  // Make sure we're not going to insert in this function anymore.
  // if global functions or something comes after this, they'd instead be dropped into here.
  builder.ClearInsertionPoint();
}

llvm::Value *LLVMEmitter::visit_call(ASTCall *node) {
  llvm::Value *callee = visit_expr(node->function);
  std::vector<llvm::Value *> args = visit_arguments(node->arguments);

  // normal named function.
  if (auto *func = llvm::dyn_cast<llvm::Function>(callee)) {
    return builder.CreateCall(func, args);
  }

  // call a function pointer.
  // the fn ptr type
  auto fn_ptr_ty = global_get_type(node->function->resolved_type);
  auto fn_ty = global_get_type(fn_ptr_ty->get_element_type());
  // convert to llvm
  auto llvm_fn_ty = llvm::dyn_cast<llvm::FunctionType>(llvm_typeof(fn_ty));
  return builder.CreateCall(llvm_fn_ty, callee, args);
}

llvm::Value *LLVMEmitter::visit_method_call(ASTMethodCall *node) {
  auto function_symbol = ctx.get_symbol(node->dot).get();
  auto arguments = visit_arguments(node->arguments);

  if (function_symbol->is_variable()) {
    /*
      TODO: handle calling a function pointer via a 'dyn' interface object.
    */
    return nullptr;
  }

  auto decl = function_symbol->function.declaration;
  auto &self_param = decl->params->params[0]->self;

  auto dot_type = global_get_type(node->dot->base->resolved_type);
  auto dot_ext = dot_type->get_ext();

  auto dot_llvm_type = llvm_typeof(dot_type);

  llvm::Value *self_argument = visit_expr(node->dot->base); // Evaluate the `dot` expression

  if (self_param.is_pointer && !dot_ext.is_pointer() && !self_argument->getType()->isPointerTy()) {
    // TODO:{}
    // Do we need to do this?
    // we can probably only do this when we pass a value that's not a pointer to a *mut/*const self function.
    // but if it's already an alloca or a reference to some memory,
    // we don't need to do this.
    auto alloca_inst = builder.CreateAlloca(dot_llvm_type, nullptr, "self_ref");
    builder.CreateStore(self_argument, alloca_inst);
    self_argument = alloca_inst;
  } else if (!self_param.is_pointer) {
    self_argument = builder.CreateLoad(dot_llvm_type, self_argument, "self_deref");
  }

  arguments.insert(arguments.begin(), self_argument);
  auto *function = llvm::dyn_cast<llvm::Function>(function_symbol->llvm_value);
  auto inst = builder.CreateCall(function, arguments);
  dbg.attach_debug_info(inst, node->source_range);
  return inst;
}

llvm::Value *LLVMEmitter::visit_block(ASTBlock *node) {
  auto old_scope = ctx.scope;
  ctx.scope = node->scope;
  dbg.enter_lexical_scope(dbg.current_scope(), node->source_range);

  for (const auto &statement : node->statements) {
    visit_node(statement);
  }

  dbg.pop_scope();
  ctx.scope = old_scope;
  return nullptr;
}

llvm::Value *LLVMEmitter::visit_return(ASTReturn *node) {
  if (node->expression) {
    auto expr_ast = node->expression.get();
    auto expr_val = load_value(expr_ast, visit_expr(expr_ast));
    return builder.CreateRet(expr_val);
  } else {
    return builder.CreateRetVoid();
  }
}

llvm::Value *LLVMEmitter::visit_literal(ASTLiteral *node) {
  switch (node->tag) {
    case ASTLiteral::Integer: {
      auto info = global_get_type(node->resolved_type)->get_info()->as<ScalarTypeInfo>();
      std::string value_str = node->value.get_str();
      int64_t value = 0;

      if (value_str.size() > 2 && value_str[0] == '0' && (value_str[1] == 'x' || value_str[1] == 'X')) {
        value = std::stoll(value_str, nullptr, 16);
      } else if (value_str.size() > 2 && value_str[0] == '0' && (value_str[1] == 'b' || value_str[1] == 'B')) {
        value = std::stoll(value_str.substr(2), nullptr, 2);
      } else {
        value = std::stoull(value_str);
      }

      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(info->size * 8, value, true));
    }
    case ASTLiteral::Float: {
      auto info = global_get_type(node->resolved_type)->get_info()->as<ScalarTypeInfo>();
      if (info->size == 8) {
        return llvm::ConstantFP::get(llvm_ctx, llvm::APFloat(std::stod(node->value.get_str())));
      } else if (info->size == 4) {
        return llvm::ConstantFP::get(llvm_ctx, llvm::APFloat(std::stof(node->value.get_str())));
      }
    }
    /*
      TODO: we need to be creating an instance of the 'str' struct
      if this node has that resolved type. right now we arent compiling any of the
      stdlib so this isn't quite possible yet.
    */
    case ASTLiteral::String: {
      return builder.CreateGlobalString(unescape_string_lit(node->value.get_str()));
    }
    case ASTLiteral::Char: {
      /*
        TODO:
        we can't just do this like this, 0th character in the string.
        we support utf8 characters, so we'll have to do something better than this
      */
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

  auto result = binary_scalars(node->left, node->right, node->op.type, expr_ty);
  /*
    TODO: attach debug info.
  */
  return result;
}

llvm::Value *LLVMEmitter::visit_path(ASTPath *node) {
  auto symbol = ctx.get_symbol(node);
  auto type = global_get_type(symbol.get()->type_id);
  return symbol.get()->llvm_value;
}

llvm::Value *LLVMEmitter::visit_expr_statement(ASTExprStatement *node) {
  visit_expr(node->expression);
  return nullptr;
}

llvm::Value *LLVMEmitter::visit_initializer_list(ASTInitializerList *node) {
  auto type = global_get_type(node->resolved_type);
  switch (node->tag) {
    case ASTInitializerList::INIT_LIST_EMPTY: {
      auto llvm_type = llvm_typeof(type);
      auto alloca_inst = builder.CreateAlloca(llvm_type);
      builder.CreateStore(llvm::ConstantAggregateZero::get(llvm_type), alloca_inst);
      return alloca_inst;
    }
    case ASTInitializerList::INIT_LIST_NAMED: {
      auto struct_type = llvm::cast<llvm::StructType>(llvm_typeof(type));
      auto alloca_inst = builder.CreateAlloca(struct_type);
      auto info = type->get_info()->as<StructTypeInfo>();

      for (const auto &[key, value] : node->key_values) {
        const auto field_index = info->get_llvm_field_index(key);
        const auto field_type = global_get_type(value->resolved_type);
        auto field_value = visit_expr(value);
        field_value = load_value(value, field_value);
        auto field_ptr = builder.CreateStructGEP(struct_type, alloca_inst, field_index);
        builder.CreateStore(field_value, field_ptr);
      }

      return alloca_inst;
    }
    case ASTInitializerList::INIT_LIST_COLLECTION: {
      auto element_type = global_get_type(type->generic_args[0]);
      auto llvm_element_type = llvm_typeof(element_type);

      auto array_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm_ctx), node->values.size());
      auto array_alloca = builder.CreateAlloca(llvm_element_type, array_size);

      for (size_t i = 0; i < node->values.size(); ++i) {
        auto value = visit_expr(node->values[i]);
        value = load_value(node->values[i], value);

        auto element_ptr = builder.CreateGEP(llvm_element_type, array_alloca,
                                             llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm_ctx), i));
        builder.CreateStore(value, element_ptr);
      }

      if (type->get_base().get_str().starts_with("InitList$")) {
        auto struct_type = llvm::cast<llvm::StructType>(llvm_typeof(type));
        auto alloca_inst = builder.CreateAlloca(struct_type);
        auto array_ptr = builder.CreateStructGEP(struct_type, alloca_inst, 0);
        builder.CreateStore(array_alloca, array_ptr);
        auto length_ptr = builder.CreateStructGEP(struct_type, alloca_inst, 1);
        builder.CreateStore(array_size, length_ptr);
        return alloca_inst;
      }

      return array_alloca;
    }
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::visit_range(ASTRange *node) {
  const auto type = global_get_type(node->resolved_type);
  const auto llvm_type = llvm_typeof(type);
  auto alloca_inst = builder.CreateAlloca(llvm_type, nullptr, "range_init");
  auto gep = builder.CreateStructGEP(llvm_type, alloca_inst, 0);
  auto left_value = visit_expr(node->left);
  builder.CreateStore(left_value, gep);

  gep = builder.CreateStructGEP(llvm_type, alloca_inst, 1);
  auto right_value = visit_expr(node->left);
  builder.CreateStore(right_value, gep);

  return alloca_inst;
}

llvm::Value *LLVMEmitter::visit_tuple(ASTTuple *node) {
  const auto type = global_get_type(node->resolved_type);
  const auto llvm_type = llvm_typeof(type);
  auto alloca_inst = builder.CreateAlloca(llvm_type, nullptr, "tuple_init");
  size_t index = 0;
  for (const auto &value : node->values) {
    auto gep = builder.CreateStructGEP(llvm_type, alloca_inst, index);
    auto field_value = visit_expr(value);
    builder.CreateStore(field_value, gep);
    index++;
  }
  return alloca_inst;
}

llvm::Value *LLVMEmitter::visit_dot_expr(ASTDotExpr *node) {
  auto base_ty = global_get_type(node->base->resolved_type);
  auto base = visit_expr(node->base);

  auto struct_info = base_ty->get_info()->as<StructTypeInfo>();
  auto base_llvm_type = llvm_typeof(base_ty);

  auto member_index = struct_info->get_llvm_field_index(node->member.identifier);

  // We take a pointer to the member type because both ExtractValue and GEP require
  // to take the extracted value by pointer.
  auto member_ty = global_get_type(global_get_type(node->resolved_type)->take_pointer_to(false));

  auto member_ptr = builder.CreateStructGEP(base_llvm_type, base, member_index, "gep_dot_member");

  /*
    I am not sure if we always want to load this,
    but if I dont when doing things like printing `printf("...", x.y)`
    i just get a junk pointer.
  */
  return builder.CreateLoad(llvm_typeof(member_ty), member_ptr, "load_dot_member");
}

llvm::Value *LLVMEmitter::visit_pattern_match(ASTPatternMatch *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_dyn_of(ASTDyn_Of *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_type_of(ASTType_Of *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_subscript(ASTSubscript *node) { return nullptr; }

/*
  TODO: verify that this won't go completely unused? it doesn't have the same purpose as it did in the old emitter,
  and simply using llvm_typeof() is probably sufficient enough for 90% of nodes.
*/
llvm::Value *LLVMEmitter::visit_type(ASTType *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_lambda(ASTLambda *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_size_of(ASTSize_Of *node) {
  auto type = global_get_type(node->target_type->resolved_type);
  auto llvm_type = llvm_typeof(type);
  auto data_layout = module->getDataLayout();
  auto bitsize = data_layout.getTypeSizeInBits(llvm_type);
  auto size = bitsize > 1 ? bitsize / 8 : bitsize;
  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm_ctx), size);
}

/*
  This only loads values when neccesary.
*/
llvm::Value *LLVMEmitter::load_value(ASTNode *node, llvm::Value *expr) {
  // We have to fetch the value of the global variable if and when we get one,
  // otherwise we get a pointer, and loading a global variable is completely invalid.
  if (auto gv = llvm::dyn_cast<llvm::GlobalVariable>(expr)) {
    // this is a hack to get strings to work, but I have no idea if this is a proper thing to do.
    // if we don't do this, we get an array of chars, instead a pointer to the strings data.
    static const auto c_string_type = global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER_CONST}}});
    if (node->resolved_type == c_string_type) {
      return gv;
    }

    return gv->getOperand(0);
  }

  if (auto symbol = ctx.get_symbol(node)) {
    if (!symbol.get()->is_param()) {
      auto type = global_get_type(node->resolved_type);
      expr = builder.CreateLoad(llvm_typeof(type), expr);
    }
  }
  return expr;
}

llvm::Value *LLVMEmitter::visit_cast(ASTCast *node) {
  auto target = global_get_type(node->target_type->resolved_type);
  auto from = global_get_type(node->expression->resolved_type);
  auto expr = visit_expr(node->expression);
  expr = load_value(node->expression, expr);
  return cast_scalar(expr, from, target);
}

void LLVMEmitter::visit_variable(ASTVariable *node) {
  auto var_type = global_get_type(node->type->resolved_type);
  auto llvm_var_type = llvm_typeof(var_type);

  /*
    The constants need a lot of work.
    We'll need to implement a 'global static initializer' kind of thing, like C++,
    so we can have constant values of arbitrary contents.
  */
  if (!builder.GetInsertBlock() || node->is_constexpr) {
    auto init_value = visit_expr(node->value.get());
    if (auto constant = llvm::dyn_cast<llvm::Constant>(init_value)) {
      auto gv =
          new llvm::GlobalVariable(llvm_var_type, node->is_constexpr, llvm::GlobalValue::ExternalLinkage, constant);
      ctx.scope->local_lookup(node->name)->llvm_value = gv;
      module->insertGlobalVariable(gv);
    } else {
      throw_error("for now, we can only have statically compile-able constant variables. this is limited to numerical "
                  "types mostly",
                  node->source_range);
    }
  } else {
    // All local variables.
    llvm::Value *alloca_inst = builder.CreateAlloca(llvm_var_type, nullptr, node->name.get_str());
    ctx.scope->local_lookup(node->name)->llvm_value = alloca_inst;

    if (node->value) {
      auto init_value = visit_expr(node->value.get());
      builder.CreateStore(init_value, alloca_inst);
    } else {
      builder.CreateStore(llvm::Constant::getNullValue(llvm_var_type), alloca_inst);
    }
  }
}

void LLVMEmitter::visit_struct_declaration(ASTStructDeclaration *node) {
  if (node->is_emitted)
    return;
  if (node->generic_parameters.size())
    return;
  node->is_emitted = true;
  llvm_typeof(global_get_type(node->resolved_type));
}

void LLVMEmitter::visit_enum_declaration(ASTEnumDeclaration *node) {
  node->is_emitted = true;
  llvm_typeof(global_get_type(node->resolved_type));
}

void LLVMEmitter::visit_choice_declaration(ASTChoiceDeclaration *node) {
  if (node->generic_parameters.size())
    return;
  if (node->is_emitted)
    return;
  node->is_emitted = true;
  llvm_typeof(global_get_type(node->resolved_type));
}

void LLVMEmitter::visit_tuple_deconstruction(ASTTupleDeconstruction *node) {}

std::vector<llvm::Value *> LLVMEmitter::visit_arguments(ASTArguments *node) {
  std::vector<llvm::Value *> args;
  for (const auto &value : node->arguments) {
    args.push_back(load_value(value, visit_expr(value)));
  }
  return args;
}

void LLVMEmitter::visit_impl(ASTImpl *node) {
  if (node->generic_parameters.size())
    return;
  if (node->is_emitted)
    return;
  node->is_emitted = true;

  auto old_scope = ctx.scope;
  ctx.scope = node->scope;
  Defer _([&] { ctx.scope = old_scope; });

  for (const auto &method : node->methods) {
    visit_function_declaration(method);
  }
}
void LLVMEmitter::visit_module(ASTModule *node) {}
void LLVMEmitter::visit_import(ASTImport *node) {}

void LLVMEmitter::visit_for(ASTFor *node) {}

llvm::Value *LLVMEmitter::visit_switch(ASTSwitch *node) { return nullptr; }
void LLVMEmitter::visit_continue(ASTContinue *node) {}
void LLVMEmitter::visit_break(ASTBreak *node) {}
void LLVMEmitter::visit_if(ASTIf *node) {
  llvm::Function *function = builder.GetInsertBlock()->getParent();

  // decalare blocks
  auto *ifBlock = llvm::BasicBlock::Create(llvm_ctx, "if");
  auto *mergeBlock = llvm::BasicBlock::Create(llvm_ctx, "ifcont");

  // if condition
  auto cond_result = visit_expr(node->condition);
  builder.CreateCondBr(cond_result, ifBlock, mergeBlock);

  // if block
  function->insert(function->end(), ifBlock);
  builder.SetInsertPoint(ifBlock);

  visit_block(node->block);
  if (HAS_FLAG(node->block->control_flow.flags, BLOCK_FLAGS_FALL_THROUGH)) {
    builder.CreateBr(mergeBlock);
  }

  // merge block
  function->insert(function->end(), mergeBlock);
  builder.SetInsertPoint(mergeBlock);
}

void LLVMEmitter::visit_else(ASTElse *node) {}
void LLVMEmitter::visit_while(ASTWhile *node) {}
void LLVMEmitter::visit_defer(ASTDefer *node) {}

llvm::Value *LLVMEmitter::visit_unary_expr(ASTUnaryExpr *node) {
  auto operand = visit_expr(node->operand);
  auto type = global_get_type(node->operand->resolved_type);
  auto llvm_type = llvm_typeof(type);

  /*
    TODO: we need to handle casting and typing better here.
    very wrong.
  */

  if (node->op.type == TType::Increment) {
    auto loadedOperand = builder.CreateLoad(llvm_type, operand, "loadtmp");
    auto incremented = builder.CreateAdd(loadedOperand, llvm::ConstantInt::get(llvm_type, 1), "inctmp");
    builder.CreateStore(incremented, operand);
    return incremented;
  }

  if (node->op.type == TType::Decrement) {
    auto loadedOperand = builder.CreateLoad(llvm_type, operand, "loadtmp");
    auto decremented = builder.CreateSub(loadedOperand, llvm::ConstantInt::get(llvm_type, 1), "dectmp");
    builder.CreateStore(decremented, operand);
    return decremented;
  }

  operand = load_value(node->operand, operand);

  switch (node->op.type) {
    case TType::LogicalNot: {
      auto zero = llvm::ConstantInt::get(operand->getType(), 0);
      return builder.CreateICmpEQ(operand, zero, "nottmp");
    }
    case TType::Not:
      return builder.CreateNot(operand, "nottmp");
    case TType::Sub:
      if (llvm_type->isFloatingPointTy()) {
        return builder.CreateFNeg(operand, "negtmp");
      } else {
        return builder.CreateNeg(operand, "negtmp");
      }
    case TType::Mul: {
      auto element_type = llvm_typeof(global_get_type(node->resolved_type));
      return builder.CreateLoad(element_type, operand);
    }
    case TType::And: {
      if (!operand->getType()->isPointerTy()) {
        auto alloca = builder.CreateAlloca(operand->getType());
        builder.CreateStore(operand, alloca);
        return alloca;
      }
      return operand;
    }
    default:
      return nullptr;
  }
}

llvm::Value *LLVMEmitter::binary_signed(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *expr_type) {
  switch (op) {
    case TType::Add:
      return builder.CreateAdd(left, right, "addtmp");
    case TType::Sub:
      return builder.CreateSub(left, right, "subtmp");
    case TType::Mul:
      return builder.CreateMul(left, right, "multmp");
    case TType::Div:
      return builder.CreateSDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder.CreateSRem(left, right, "modtmp");
    case TType::LT:
      return builder.CreateICmpSLT(left, right, "lttmp");
    case TType::GT:
      return builder.CreateICmpSGT(left, right, "gttmp");
    case TType::LE:
      return builder.CreateICmpSLE(left, right, "letmp");
    case TType::GE:
      return builder.CreateICmpSGE(left, right, "getmp");
    case TType::EQ:
      return builder.CreateICmpEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder.CreateICmpNE(left, right, "neqtmp");
    case TType::And:
      return builder.CreateAnd(left, right, "andtmp");
    case TType::Or:
      return builder.CreateOr(left, right, "ortmp");
    case TType::Xor:
      return builder.CreateXor(left, right, "xortmp");
    case TType::SHL:
      return builder.CreateShl(left, right, "shltmp");
    case TType::SHR:
      return builder.CreateAShr(left, right, "shrtmp"); // Arithmetic shift

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateAdd(loadedLeft, right, "compaddtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateSub(loadedLeft, right, "compsubtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateMul(loadedLeft, right, "compmultmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateSDiv(loadedLeft, right, "compdivtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateSRem(loadedLeft, right, "compmodtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompAnd: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateAnd(loadedLeft, right, "compandtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompOr: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateOr(loadedLeft, right, "comportmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompXor: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateXor(loadedLeft, right, "compxortmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSHL: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateShl(loadedLeft, right, "compshltmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSHR: {
      auto loadedLeft = builder.CreateLoad(expr_type, left, "loadtmp");
      auto result = builder.CreateAShr(loadedLeft, right, "compshrtmp");
      builder.CreateStore(result, left);
      return result;
    }

    // Assignment
    case TType::Assign:
      builder.CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::binary_unsigned(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *left_type) {
  switch (op) {
    case TType::Add:
      return builder.CreateAdd(left, right, "addtmp");
    case TType::Sub:
      return builder.CreateSub(left, right, "subtmp");
    case TType::Mul:
      return builder.CreateMul(left, right, "multmp");
    case TType::Div:
      return builder.CreateUDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder.CreateURem(left, right, "modtmp");
    case TType::LT:
      return builder.CreateICmpULT(left, right, "lttmp");
    case TType::GT:
      return builder.CreateICmpUGT(left, right, "gttmp");
    case TType::LE:
      return builder.CreateICmpULE(left, right, "letmp");
    case TType::GE:
      return builder.CreateICmpUGE(left, right, "getmp");
    case TType::EQ:
      return builder.CreateICmpEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder.CreateICmpNE(left, right, "neqtmp");
    case TType::And:
      return builder.CreateAnd(left, right, "andtmp");
    case TType::Or:
      return builder.CreateOr(left, right, "ortmp");
    case TType::Xor:
      return builder.CreateXor(left, right, "xortmp");
    case TType::SHL:
      return builder.CreateShl(left, right, "shltmp");
    case TType::SHR:
      return builder.CreateLShr(left, right, "shrtmp"); // Logical shift

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateAdd(loadedLeft, right, "compaddtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateSub(loadedLeft, right, "compsubtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateMul(loadedLeft, right, "compmultmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateUDiv(loadedLeft, right, "compdivtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateURem(loadedLeft, right, "compmodtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompAnd: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateAnd(loadedLeft, right, "compandtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompOr: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateOr(loadedLeft, right, "comportmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompXor: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateXor(loadedLeft, right, "compxortmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSHL: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateShl(loadedLeft, right, "compshltmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSHR: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateLShr(loadedLeft, right, "compshrtmp");
      builder.CreateStore(result, left);
      return result;
    }

    // Assignment
    case TType::Assign:
      builder.CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::binary_fp(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *left_type) {
  switch (op) {
    case TType::Add:
      return builder.CreateFAdd(left, right, "addtmp");
    case TType::Sub:
      return builder.CreateFSub(left, right, "subtmp");
    case TType::Mul:
      return builder.CreateFMul(left, right, "multmp");
    case TType::Div:
      return builder.CreateFDiv(left, right, "divtmp");
    case TType::Modulo:
      return builder.CreateFRem(left, right, "modtmp");
    case TType::LT:
      return builder.CreateFCmpOLT(left, right, "lttmp");
    case TType::GT:
      return builder.CreateFCmpOGT(left, right, "gttmp");
    case TType::LE:
      return builder.CreateFCmpOLE(left, right, "letmp");
    case TType::GE:
      return builder.CreateFCmpOGE(left, right, "getmp");
    case TType::EQ:
      return builder.CreateFCmpOEQ(left, right, "eqtmp");
    case TType::NEQ:
      return builder.CreateFCmpONE(left, right, "neqtmp");

    // Compound assignments
    case TType::CompAdd: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateFAdd(loadedLeft, right, "compaddtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompSub: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateFSub(loadedLeft, right, "compsubtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMul: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateFMul(loadedLeft, right, "compmultmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompDiv: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateFDiv(loadedLeft, right, "compdivtmp");
      builder.CreateStore(result, left);
      return result;
    }
    case TType::CompMod: {
      auto loadedLeft = builder.CreateLoad(left_type, left, "loadtmp");
      auto result = builder.CreateFRem(loadedLeft, right, "compmodtmp");
      builder.CreateStore(result, left);
      return result;
    }
    // Assignment
    case TType::Assign:
      builder.CreateStore(right, left);
      return right;

    default:
      return nullptr;
  }
  return nullptr;
}

llvm::Value *LLVMEmitter::cast_scalar(llvm::Value *value, Type *from, Type *to) {
  if (from == to) {
    return value;
  }

  auto from_info = from->get_info()->as<ScalarTypeInfo>();
  auto to_info = to->get_info()->as<ScalarTypeInfo>();
  auto llvm_to = llvm_typeof(to);

  if (to->get_ext().is_pointer()) {
    if (from->get_ext().is_pointer()) {
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

llvm::Value *LLVMEmitter::binary_scalars(ASTExpr *left_ast, ASTExpr *right_ast, TType op, Type *expr_ty) {
  auto expr_ty_info = expr_ty->get_info()->as<ScalarTypeInfo>();

  auto left = visit_expr(left_ast);
  auto right = visit_expr(right_ast);

  Token temp_token = {};
  temp_token.type = op;
  const bool is_assignment = temp_token.is_comp_assign() || op == TType::Assign;

  auto right_ty = global_get_type(right_ast->resolved_type);
  auto left_ty = global_get_type(left_ast->resolved_type);
  auto llvm_left_ty = llvm_typeof(left_ty);
  auto right_info = right_ty->get_info()->as<ScalarTypeInfo>();
  auto left_info = left_ty->get_info()->as<ScalarTypeInfo>();

  right = load_value(right_ast, right);
  right = cast_scalar(right, right_ty, left_ty);

  if (!is_assignment) {
    left = load_value(left_ast, left);
  }

  auto type = llvm_typeof(expr_ty);

  switch (expr_ty_info->scalar_type) {
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
      return binary_signed(left, right, op, type);
    case TYPE_CHAR:
    case TYPE_BOOL:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
      return binary_unsigned(left, right, op, type);
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
      return binary_fp(left, right, op, type);
      break;
    case TYPE_VOID:
      break;
  }

  return nullptr;
}
