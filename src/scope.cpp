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
  Symbol *sym = head;
  Symbol *prev = nullptr;

  while (sym) {
    if (sym->name == symbol.name) {
      *sym = symbol;
      return;
    }
    prev = sym;
    sym = sym->next;
  }

  Symbol *new_sym = new (symbol_arena.allocate(sizeof(Symbol))) Symbol(symbol);
  if (prev) {
    prev->next = new_sym;
  } else {
    head = new_sym;
  }
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
  auto id = global_create_interface_type(name, scope, declaration, generic_args);
  insert(Symbol(name, id, declaration, SYMBOL_IS_TYPE));
  return id;
}

int Scope::create_struct_type(const Interned_String &name, AST *declaration, Scope scope) {
  auto id = global_create_struct_type(name, scope, declaration);
  insert(Symbol(name, id, declaration, SYMBOL_IS_TYPE));
  return id;
}

void Scope::create_type_alias(const Interned_String &name, int type_id, AST *declaring_node) {
  insert(Symbol(name, type_id, declaring_node, SYMBOL_IS_TYPE));
}

void Scope::forward_declare_type(const Interned_String &name, int default_id) {
  insert(Symbol(name, default_id, nullptr, SYMBOL_IS_TYPE));
}

int Scope::create_enum_type(const Interned_String &name, bool flags, AST *declaration, Scope scope) {
  auto id = global_create_enum_type(name, scope, declaration, flags);
  insert(Symbol(name, id, declaration, SYMBOL_IS_TYPE));
  return id;
}

int Scope::create_tuple_type(const std::vector<int> &types) {
  auto id = global_create_tuple_type(types);
  auto name = get_tuple_type_name(types);
  // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
  insert(Symbol(name, id, nullptr, SYMBOL_IS_TYPE));
  return id;
}

void Scope::insert_variable(const Interned_String &name, int type_id, AST *initial_value, AST *decl) {
  insert(Symbol(name, type_id, decl, SYMBOL_IS_VARIABLE, initial_value));
}

void Scope::insert_function(const Interned_String &name, const int type_id, AST *declaration, SymbolFlags flags) {
  insert(Symbol(name, type_id, declaration, flags));
}

void Scope::insert_type(const int type_id, const Interned_String &name, AST *declaration) {
  insert(Symbol(name, type_id, declaration, SYMBOL_IS_TYPE));
}
