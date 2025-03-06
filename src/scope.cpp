#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"
#include <format>

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
    if (type_table[i]->kind == TYPE_FUNCTION) {
      continue;
    }
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
  symbols.insert({name, Symbol::create_type(-1, name, TYPE_INTERFACE, node)});
}
