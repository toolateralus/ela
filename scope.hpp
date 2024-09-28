#pragma once

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
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}

struct Context {
  
  Scope *current_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  Scope *root_scope;
  Context() {
    FunctionTypeInfo printf_info {};
    printf_info.return_type = void_type();
    printf_info.is_varargs = true;
    current_scope->insert("printf", find_type_id("", printf_info, {}));
    
    FunctionTypeInfo assert_info {};
    assert_info.return_type = void_type();
    assert_info.parameter_types[0] = string_type();
    assert_info.parameter_types[1] = bool_type();
    assert_info.params_len = 2;
    current_scope->insert("assert", find_type_id("", assert_info, {}));
    
    FunctionTypeInfo sizeof_info {};
    sizeof_info.return_type = find_type_id("s64", {});
    sizeof_info.is_varargs = true;
    current_scope->insert("sizeof", find_type_id("", sizeof_info, {}));
    root_scope = current_scope;
  }
  
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