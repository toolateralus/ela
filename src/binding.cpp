#include "binding.hpp"
#include <vector>
#include "ast.hpp"

template <class T>
T *binding_alloc() {
  return new (binding_arena.allocate(sizeof(T))) T();
}

std::vector<uint64_t> Binder::bind(ASTFunctionDeclaration *f) {
  // If it has generic instantiations, we only dump the monomorphizations, and discard the template.
  if (f->generic_instantiations.size()) {
    std::vector<uint64_t> result{};
    for (auto &instantiation : f->generic_instantiations) {
      const auto inner_result = bind((ASTFunctionDeclaration *)instantiation.declaration);
      // This _should always_ be of one length, the inner result.
      // but this is safer, and easier than a check.
      result.append_range(inner_result);
    }
    return result;
  }

  FunctionBinding *binding = binding_alloc<FunctionBinding>();
  binding->type = f->resolved_type;
  binding->node = f;
  binding->uid = bindings.size();
  return {binding->uid};
}

uint64_t Binder::bind(ASTVariable *v) {
  if (v->is_local) {
    LocalBinding *binding = binding_alloc<LocalBinding>();
    binding->type = v->resolved_type;
    binding->node = v;
    binding->uid = bindings.size();
    return binding->uid;
  } else {
    GlobalBinding *binding = binding_alloc<GlobalBinding>();
    binding->type = v->resolved_type;
    binding->node = v;
    binding->uid = bindings.size();
    return binding->uid;
  }
}
