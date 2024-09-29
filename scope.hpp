#pragma once


#include "lex.hpp"
#include "type.hpp"
#include <jstl/memory/arena.hpp>
#include <string>
#include <unordered_map>

// TODO(Josh) Add scope flags? so we can tell stuff like executing, declaring,
// possibly returns, breaks, continues, etc.
// TODO(cont.) executing would be an executing body like if, func, declaring
// would be things like struct declarations, etc.
// Would this even help?

extern jstl::Arena scope_arena;

struct Symbol {
  std::string name;
  int type_id;
};

struct Scope {
  bool is_struct_scope = false;
  
  std::unordered_map<std::string, Symbol> symbols;
  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : parent(parent), symbols({}) {
  }
  inline void insert(const std::string &name, int type_id) {
    symbols[name] = Symbol{name, type_id};
  }
  inline Symbol *lookup(const std::string &name) {
    if (symbols.find(name) != symbols.end()) {
      return &symbols[name];
    } else if (parent) {
      return parent->lookup(name);
    }
    return nullptr;
  }
  inline void erase(const std::string &name) {
    symbols.erase(name);
  }
  
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}

struct Context {
  
  Scope *current_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  Scope *root_scope;
  Context();

  inline void enter_scope(Scope *scope = nullptr) {
    if (!scope) {
      scope = create_child(current_scope);
    }
    current_scope = scope;
  }
  inline Scope *exit_scope() {
    auto scope = current_scope;
    if (current_scope) {
      current_scope = current_scope->parent;
    }
    return scope;
  }
};