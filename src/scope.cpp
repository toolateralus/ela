#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"
#include <format>

Context::Context() {
  root_scope = scope;

#if defined(__linux)
  root_scope->defines().insert("PLATFORM_LINUX");
#elif defined(_WIN32)
  root_scope->defines().insert("PLATFORM_WINDOWS");
#elif defined(__APPLE__)
  root_scope->defines().insert("PLATFORM_MACOS");
#elif defined(__ANDROID__)
  root_scope->defines().insert("PLATFORM_ANDROID");
#elif defined(__unix__)
  root_scope->defines().insert("PLATFORM_UNIX");
#elif defined(__FreeBSD__)
  root_scope->defines().insert("PLATFORM_FREEBSD");
#endif

  if (compile_command.has_flag("freestanding"))
    root_scope->defines().insert("FREESTANDING");

  if (compile_command.has_flag("test")) {
    root_scope->add_def("TESTING");
  }

  { // For this compiler intrinsic operator,
    // We have to do this. However, in the future, we will implement our own sizer,
    // and we won't have this problem.
    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = u32_type();
    sizeof_info.is_varargs = true;
    scope->insert_function("sizeof", nullptr);
    scope->symbols["sizeof"].type_id = global_find_function_type_id(sizeof_info, {});
  }

  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->get_info()->scope) {
      type_table[i]->get_info()->scope->parent = root_scope;
    }
    std::cout << std::format("creating alias for scalar/builtin {} :: {}\n", type_table[i]->get_base(), i);
    root_scope->create_type_alias(type_table[i]->get_base(), i, type_table[i]->kind);
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
