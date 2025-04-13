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
#include <print>


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

  std::println("emitting function {}", node->name.get_str());

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

  for (const auto &statement: node->statements) {
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
    case ASTLiteral::Integer:
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(32, std::stoll(node->value.get_str()), true));
    case ASTLiteral::Float:
      return llvm::ConstantFP::get(llvm_ctx, llvm::APFloat(std::stod(node->value.get_str())));
    case ASTLiteral::String:
      return builder.CreateGlobalStringPtr(node->value.get_str());
    case ASTLiteral::Char:
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(8, node->value.get_str()[0], false));
    case ASTLiteral::Bool:
      return llvm::ConstantInt::get(llvm_ctx, llvm::APInt(1, node->value.get_str() == "true" ? 1 : 0, false));
    case ASTLiteral::Null:
      static auto ptr_ty = builder.getPtrTy(0);
      return llvm::ConstantPointerNull::get(ptr_ty);
    default:
      return nullptr;
  }
}

llvm::Value *LLVMEmitter::visit_method_call(ASTMethodCall *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_path(ASTPath *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_pattern_match(ASTPatternMatch *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_dyn_of(ASTDyn_Of *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_type_of(ASTType_Of *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_expr_statement(ASTExprStatement *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_bin_expr(ASTBinExpr *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_unary_expr(ASTUnaryExpr *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_type(ASTType *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_call(ASTCall *node) { return nullptr; }

llvm::Value *LLVMEmitter::visit_dot_expr(ASTDotExpr *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_subscript(ASTSubscript *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_initializer_list(ASTInitializerList *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_range(ASTRange *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_switch(ASTSwitch *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_tuple(ASTTuple *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_cast(ASTCast *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_lambda(ASTLambda *node) { return nullptr; }
llvm::Value *LLVMEmitter::visit_size_of(ASTSize_Of *node) { return nullptr; }

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
