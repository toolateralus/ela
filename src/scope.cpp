#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "type.hpp"


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

int Scope::create_interface_type(const Interned_String &name, const std::vector<int> &generic_args, AST *declaration,
                                 Scope scope) {
  auto id = global_create_interface_type(name, scope, generic_args);
  insert(Symbol::create_type(id, name, TYPE_INTERFACE, declaration));
  return id;
}

int Scope::create_struct_type(const Interned_String &name, AST *declaration, Scope scope) {
  auto id = global_create_struct_type(name, scope);
  insert(Symbol::create_type(id, name, TYPE_STRUCT, declaration));
  return id;
}

void Scope::create_type_alias(const Interned_String &name, int type_id, Type_Kind kind, AST *declaring_node) {
  Symbol symbol;
  symbol.name = name;
  symbol.type_id = type_id;
  symbol.type.kind = kind;
  symbol.flags = SYMBOL_IS_TYPE;
  symbol.type.declaration = declaring_node;
  insert(symbol);
}

void Scope::forward_declare_type(const Interned_String &name, int default_id) {
  Symbol symbol;
  symbol.name = name;
  symbol.type_id = default_id;
  symbol.flags = SYMBOL_IS_TYPE;
  insert(symbol);
}

int Scope::create_enum_type(const Interned_String &name, bool flags, AST *declaration, Scope scope) {
  auto id = global_create_enum_type(name, scope, flags);
  insert(Symbol::create_type(id, name, TYPE_STRUCT, declaration));
  return id;
}

int Scope::create_tuple_type(const std::vector<int> &types) {
  auto id = global_create_tuple_type(types);
  auto name = get_tuple_type_name(types);
  // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
  insert(Symbol::create_type(id, name, TYPE_STRUCT, nullptr));
  return id;
}

void Scope::insert_variable(const Interned_String &name, int type_id, AST *initial_value, AST *decl) {
  insert(Symbol::create_variable(name, type_id, initial_value, decl));
}

void Scope::insert_function(const Interned_String &name, const int type_id, AST *declaration, SymbolFlags flags) {
  insert(Symbol::create_function(name, type_id, declaration, flags));
}

void Scope::insert_type(const int type_id, const Interned_String &name, Type_Kind kind, AST *declaration) {
  insert(Symbol::create_type(type_id, name, kind, declaration));
}
