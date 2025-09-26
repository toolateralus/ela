#pragma once

#include <stdint.h>
#include "ast.hpp"
#include "scope.hpp"
#include "type.hpp"

struct Binding {
  Type *type;
  uint64_t uid;
  ASTNode *node;

  virtual ~Binding() {}

  template <class T>
  bool is() const {
    return dynamic_cast<T const *>(this);
  }
  template <class T>
  T *as() const {
    return static_cast<T *>(this);
  }
};

struct FunctionBinding : Binding {
  ASTFunctionDeclaration const *ast() const { return (ASTFunctionDeclaration *)this->node; }
};

struct GlobalBinding : Binding {
  ASTVariable const *ast() const { return (ASTVariable *)this->node; }
};

struct LocalBinding : Binding {
  ASTVariable const *ast() const { return (ASTVariable *)this->node; }
};

extern jstl::Arena binding_arena;

struct Binder {
  std::vector<Binding *> bindings;
  std::vector<uint64_t> bind(ASTFunctionDeclaration *);
  uint64_t bind(ASTVariable *);
};