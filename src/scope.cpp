#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "type.hpp"
#include <format>

// Context::Context() {
//   root_scope = scope;

// #if defined(__linux)
//   root_scope->defines().insert("PLATFORM_LINUX");
// #elif defined(_WIN32)
//   root_scope->defines().insert("PLATFORM_WINDOWS");
// #elif defined(__APPLE__)
//   root_scope->defines().insert("PLATFORM_MACOS");
// #elif defined(__ANDROID__)
//   root_scope->defines().insert("PLATFORM_ANDROID");
// #elif defined(__unix__)
//   root_scope->defines().insert("PLATFORM_UNIX");
// #elif defined(__FreeBSD__)
//   root_scope->defines().insert("PLATFORM_FREEBSD");
// #endif

//   if (compile_command.has_flag("freestanding"))
//     root_scope->defines().insert("FREESTANDING");

//   if (compile_command.has_flag("test")) {
//     root_scope->add_def("TESTING");
//   }

//   { // For this compiler intrinsic operator,
//     // We have to do this. However, in the future, we will implement our own sizer,
//     // and we won't have this problem.
//     FunctionTypeInfo sizeof_info{};
//     sizeof_info.return_type = u32_type();
//     sizeof_info.is_varargs = true;
//     scope->insert_function("sizeof", global_find_function_type_id(sizeof_info, {}), nullptr);
//   }

//   for (int i = 0; i < type_table.size(); ++i) {
//     if (type_table[i]->kind == TYPE_FUNCTION) {
//       continue;
//     }
//     if (type_table[i]->get_info()->scope) {
//       type_table[i]->get_info()->scope->parent = root_scope;
//     }

//     root_scope->create_type_alias(type_table[i]->get_base(), i, type_table[i]->kind, nullptr);
//   }
// }


// void Scope::declare_interface(const InternedString &name, ASTInterfaceDeclaration *node) {
//   symbols.insert({name, Symbol::create_type(-1, name, TYPE_INTERFACE, node)});
// }

Symbol *Scope::lookup(const Interned_String &name) {
  if (head == nullptr) {
    return nullptr;
  }
  auto sym = head;
  while (sym) {
    if (sym->name == name) {
      return sym;
    }
    sym = sym->next;
  }
  return nullptr;
}


void Scope::insert(const Symbol &symbol) {
  Symbol **sym = &head;
  while (*sym) {
    // Overwrite if we find a symbol with a matching name.
    if ((*sym)->name == symbol.name) {
      **sym = symbol;
      return;
    }
    sym = &(*sym)->next;
  }
  // Insert the new symbol at the end of the list
  *sym = (Symbol *)symbol_arena.allocate(sizeof(Symbol));
  **sym = symbol;
}


bool Scope::erase(const Interned_String &name) {
  Symbol **sym = &head;
  while (*sym) {
    if ((*sym)->name == name) {
      *sym = (*sym)->next;
      return true;
    }
    sym = &(*sym)->next;
  }
  return false;
}
