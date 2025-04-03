#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"

Context::Context() {
  scope = create_child(nullptr);
  root_scope = scope;

#if defined(__linux)
  scope->defines().insert("PLATFORM_LINUX");
#elif defined(_WIN32)
  scope->defines().insert("PLATFORM_WINDOWS");
#elif defined(__APPLE__)
  scope->defines().insert("PLATFORM_MACOS");
#elif defined(__ANDROID__)
  scope->defines().insert("PLATFORM_ANDROID");
#elif defined(__unix__)
  scope->defines().insert("PLATFORM_UNIX");
#elif defined(__FreeBSD__)
  scope->defines().insert("PLATFORM_FREEBSD");
#endif

  if (compile_command.has_flag("freestanding"))
    Scope::defines().insert("FREESTANDING");

  if (compile_command.has_flag("test")) {
    Scope::add_def("TESTING");
  }

  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->get_info()->scope) {
      type_table[i]->get_info()->scope->parent = scope;
    }
    scope->create_type_alias(type_table[i]->get_base(), i, type_table[i]->kind, nullptr);
  }
}

Symbol *Scope::lookup(const InternedString &name) {
  if (symbols.find(name) != symbols.end()) {
    return &symbols[name];
  } else if (parent) {
    return parent->lookup(name);
  }
  return nullptr;
}

void Scope::erase(const InternedString &name) {
  symbols.erase(name);
  ordered_symbols.erase(std::remove(ordered_symbols.begin(), ordered_symbols.end(), name), ordered_symbols.end());
}

void Scope::declare_interface(const InternedString &name, ASTInterfaceDeclaration *node) {
  auto sym = Symbol::create_type(-1, name, TYPE_INTERFACE, node);
  sym.scope = this;
  symbols.insert({name, sym});
}

bool Symbol::is_generic_function() const {
  if (!is_function())
    return false;
  
  auto declaration = this->function.declaration;

  if (declaration->generic_parameters.size() != 0 || 
      declaration->generic_arguments.size() != 0  ||
      declaration->generic_instantiations.size() != 0) {
    return true;
  }

  return false;
}