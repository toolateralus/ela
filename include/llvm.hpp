#pragma once
#include "core.hpp"
#include "visitor.hpp"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DIBuilder.h"
#include <llvm/IR/DebugInfoMetadata.h>
#include <memory>
#include <stack>

using namespace llvm;

struct ScopeManager {
  std::shared_ptr<DIBuilder> di_builder;
  std::stack<DIScope *> scope_stack;

  explicit ScopeManager(std::shared_ptr<DIBuilder> &builder) : di_builder(builder) {}

  struct ScopeDropStub {
    ScopeManager *manager;
    ScopeDropStub() = delete;
    explicit ScopeDropStub(ScopeManager *manager) : manager(manager) {}
    ScopeDropStub(const ScopeDropStub &) = delete;
    ScopeDropStub &operator=(const ScopeDropStub &) = delete;
    ScopeDropStub(ScopeDropStub &&other) noexcept : manager(other.manager) {
      other.manager = nullptr;
    }

    ScopeDropStub &operator=(ScopeDropStub &&other) noexcept {
      if (this != &other) {
        manager = other.manager;
        other.manager = nullptr;
      }
      return *this;
    }
    ~ScopeDropStub() {
      manager->pop_scope();
    }
  };

  ScopeDropStub push_scope(DIScope *scope) {
    scope_stack.push(scope);
    return ScopeDropStub(this);
  }

  void pop_scope() {
    if (!scope_stack.empty()) {
      scope_stack.pop();
    }
  }

  DIScope *current() const { return scope_stack.empty() ? nullptr : scope_stack.top(); }

  std::tuple<std::string, std::string, unsigned, unsigned> extract_source_range(SourceRange range) {
    auto location = range.begin_location;
    auto line = location.line, column = location.column;
    std::filesystem::path abs_path = location.files()[location.file];

    auto basename = abs_path.filename().string();
    auto dirpath = abs_path.parent_path().string();
    return {basename, dirpath, line, column};
  }

  std::pair<DIFile *, ScopeDropStub> enter_file_scope(const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *file = di_builder->createFile(basename, dirpath);
    auto stub = push_scope(file);
    return {file, std::move(stub)};
  }

  std::pair<DISubprogram *, ScopeDropStub> enter_function_scope(DIScope *parent, Function *function, const std::string &name, const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *file = dyn_cast<DIFile>(parent);
    auto *func_type = di_builder->createSubroutineType(di_builder->getOrCreateTypeArray({}));
    auto *subprogram = di_builder->createFunction(file, name, name, file, line, func_type, line, DINode::FlagZero,
                                                  DISubprogram::SPFlagDefinition);
    function->setSubprogram(subprogram);
    auto stub = push_scope(subprogram);
    return {subprogram, std::move(stub)};
  }

  std::pair<DILexicalBlock *, ScopeDropStub> enter_lexical_scope(DIScope *parent, const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *block = di_builder->createLexicalBlock(parent, di_builder->createFile(basename, dirpath), line, column);
    auto stub = push_scope(block);
    return {block, std::move(stub)};
  }

  DIVariable *create_variable(DIScope *scope, const std::string &name, DIFile *file, unsigned line, DIType *type) {
    return di_builder->createAutoVariable(scope, name, file, line, type, true);
  }

  void attach_debug_info(Instruction *instruction, DIVariable *variable, unsigned line, unsigned column) {
    auto *debug_loc = DILocation::get(instruction->getContext(), line, column, variable->getScope());
    instruction->setDebugLoc(debug_loc);
  }

  DIType *create_basic_type(const std::string &name, uint64_t size_in_bits, unsigned encoding) {
    return di_builder->createBasicType(name, size_in_bits, encoding);
  }

  DIType *create_pointer_type(DIType *pointee_type, uint64_t size_in_bits) {
    return di_builder->createPointerType(pointee_type, size_in_bits);
  }

  DIType *create_function_type(ArrayRef<Metadata *> param_types) {
    return di_builder->createSubroutineType(di_builder->getOrCreateTypeArray(param_types));
  }

  DIType *create_struct_type(DIScope *scope, const std::string &name, DIFile *file, unsigned line, uint64_t size_in_bits, uint64_t align_in_bits, DINode::DIFlags flags, ArrayRef<Metadata *> elements) {
    return di_builder->createStructType(scope, name, file, line, size_in_bits, align_in_bits, flags, nullptr, di_builder->getOrCreateArray(elements));
  }

  DIType *create_array_type(uint64_t size, uint64_t align_in_bits, DIType *element_type, ArrayRef<Metadata *> subscripts) {
    return di_builder->createArrayType(size, align_in_bits, element_type, di_builder->getOrCreateArray(subscripts));
  }
};

struct LLVMEmitter : VisitorBase {
  Context &ctx;
  LLVMContext llvm_ctx;
  IRBuilder<> builder;
  std::unique_ptr<Module> module;
  std::shared_ptr<DIBuilder> di_builder;
  DataLayout *data_layout;
  bool dont_load;
  ScopeManager scope_manager;

  LLVMEmitter(Context &ctx)
      : ctx(ctx), llvm_ctx(), builder(llvm_ctx), module(std::make_unique<Module>("module", llvm_ctx)),
        di_builder(std::make_shared<DIBuilder>(*module)), data_layout(nullptr), dont_load(false), scope_manager(di_builder) {}

  void visit(ASTModule *node) override;
  void visit(ASTImport *node) override;
  void visit(ASTType_Of *node) override;
  void visit(ASTStructDeclaration *node) override;
  void visit(ASTProgram *node) override;
  void visit(ASTBlock *node) override;
  void visit(ASTFunctionDeclaration *node) override;
  void visit(ASTParamsDecl *node) override;
  void visit(ASTParamDecl *node) override;
  void visit(ASTVariable *node) override;
  void visit(ASTExprStatement *node) override;
  void visit(ASTBinExpr *node) override;
  void visit(ASTUnaryExpr *node) override;
  void visit(ASTIdentifier *node) override;
  void visit(ASTLiteral *node) override;
  void visit(ASTType *node) override;
  void visit(ASTCall *node) override;
  void visit(ASTArguments *node) override;
  void visit(ASTReturn *node) override;
  void visit(ASTContinue *node) override;
  void visit(ASTBreak *node) override;
  void visit(ASTFor *node) override;
  void visit(ASTIf *node) override;
  void visit(ASTElse *node) override;
  void visit(ASTWhile *node) override;
  void visit(ASTDotExpr *node) override;
  void visit(ASTSubscript *node) override;
  void visit(ASTInitializerList *node) override;
  void visit(ASTEnumDeclaration *node) override;
  void visit(ASTRange *node) override;
  void visit(ASTSwitch *node) override;
  void visit(ASTTuple *node) override;
  void visit(ASTTupleDeconstruction *node) override;
  void visit(ASTSize_Of *node) override;
  void visit(ASTScopeResolution *node) override;
  void visit(ASTAlias *node) override;
  void visit(ASTImpl *node) override;
  void visit(ASTDefer *node) override;
  void visit(ASTTaggedUnionDeclaration *node) override;
  void visit(ASTCast *node) override;
  void visit(ASTInterfaceDeclaration *node) override;
  void visit(ASTLambda *node) override;
  void visit(ASTWhere *node) override;

  void visit(ASTStatementList *node) override {
    for (const auto &stmt : node->statements) {
      stmt->accept(this);
    }
  };
};