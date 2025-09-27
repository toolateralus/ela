#pragma once

#include <stdint.h>
#include <cstdint>
#include "scope.hpp"
#include "type.hpp"

struct Binding {
  Type *type;
  uint64_t uid;
  ASTNode *node;
  THIR *thir;

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
using FunctionGenerator = std::function<THIR *(ASTFunctionDeclaration *)>;

struct Binder {
  std::vector<Binding *> bindings;
  void bind(ASTFunctionDeclaration *, FunctionGenerator);
  void bind(ASTFunctionDeclaration *, THIR *);
  void bind(ASTVariable *v, THIR *thir);
  void bind(ASTVariable *v);

  inline Binding *get(uint64_t index) const {
    if (index < bindings.size())
      return bindings[index];
    return nullptr;
  }
};

static inline bool valid_binding(uint64_t binding) {
  return binding != UINT64_MAX;
}