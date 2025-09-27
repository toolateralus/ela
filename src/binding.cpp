#include "binding.hpp"
#include <vector>
#include "ast.hpp"
#include "thir.hpp"

template <class T>
static inline T *binding_alloc() {
  return new (binding_arena.allocate(sizeof(T))) T();
}

// This only works for one known-concrete function
void Binder::bind(ASTFunctionDeclaration *f, THIR *thir) {
  FunctionBinding *binding = binding_alloc<FunctionBinding>();
  binding->type = f->resolved_type;
  binding->uid = bindings.size();

  {
    binding->node = f;
    f->binding = binding->uid;
  }

  if (thir) {
    thir->binding = binding->uid;
    binding->thir = thir;
  }

  bindings.push_back(binding);
}

// If it has generic instantiations, we only dump the monomorphizations, and discard the template.
void Binder::bind(ASTFunctionDeclaration *f, FunctionGenerator thir_generator) {
  if (f->generic_instantiations.size()) {
    for (auto &instantiation : f->generic_instantiations) {
      bind((ASTFunctionDeclaration *)instantiation.declaration, thir_generator);
    }
    return;
  }
  bind(f, thir_generator(f));
}

void Binder::bind(ASTVariable *v, THIR *thir) {
  const auto bind = [&](auto binding) {
    binding->uid = bindings.size();
    binding->type = v->resolved_type;
    binding->node = v;
    v->binding = binding->uid;
    binding->node = v;
    if (thir) {
      thir->binding = binding->uid;
      binding->thir = thir;
    }
    bindings.push_back(binding);
  };

  if (v->is_local) {
    LocalBinding *binding = binding_alloc<LocalBinding>();
    bind(binding);
  } else {
    GlobalBinding *binding = binding_alloc<GlobalBinding>();
    bind(binding);
  }
}

void Binder::bind(ASTVariable *v) { return bind(v, nullptr); }