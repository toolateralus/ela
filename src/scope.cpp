#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"

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
    scope->insert("sizeof", global_find_function_type_id(sizeof_info, {}), nullptr, SYMBOL_IS_FUNCTION);
  }

  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->get_info()->scope) {
      type_table[i]->get_info()->scope->parent = root_scope;
    }
    root_scope->create_type_alias(type_table[i]->get_base(), i, type_table[i]->kind);
  }
}


// We probably want to just alltogether get rid of this entire function.
// We should be creating symbols explicitly based on their symbol kind, not reading the flags.

void Scope::insert(const InternedString &name, int type_id, ASTNode *declaring_node, int flags) {
  if ((flags & SYMBOL_IS_VARIABLE) != 0) {
    symbols.insert({name, Symbol::create_variable(name, (ASTExpr*)declaring_node)});
  } else if ((flags & SYMBOL_IS_FUNCTION) != 0) {
    symbols.insert({name, Symbol::create_function(name, (ASTFunctionDeclaration*)declaring_node, (SymbolFlags)flags)});
  } else if ((flags & SYMBOL_IS_TYPE) != 0) {
    auto type = global_get_type(type_id);
    if (type) {
      symbols.insert({name, Symbol::create_type(type_id, name, type->kind, declaring_node)});
    } else {
      symbols.insert({name, Symbol::create_type(type_id, name, (TypeKind)0, declaring_node)});
    }
  }
  symbols[name].flags |= flags;
  ordered_symbols.push_back(name);
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
