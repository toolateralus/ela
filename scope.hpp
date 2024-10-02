#pragma once

#include "error.hpp"
#include "type.hpp"
#include <jstl/memory/arena.hpp>
#include <set>
#include <string>
#include <unordered_map>

// TODO(Josh) Add scope flags? so we can tell stuff like executing, declaring,
// possibly returns, breaks, continues, etc.
// TODO(cont.) executing would be an executing body like if, func, declaring
// would be things like struct declarations, etc.
// Would this even help?

extern jstl::Arena scope_arena;

struct Symbol {
  // the identifier.
  std::string name;
  // if this is a type alias, this
  // is the type that this identifier points to.
  int type_id; 
  // is this a type alias?
  bool type_alias;
};


struct Scope {
  // TODO(Josh) 10/1/2024, 1:03:34 PM Replace this with a set of flagsor something.
  bool is_struct_or_union_scope = false;
  std::unordered_map<std::string, Symbol> symbols;
  std::set<int> types;
  inline int find_type_id(const std::string &name, const TypeExt &ext) {
    auto base_id = global_find_type_id(name, {});
    auto id = global_find_type_id(name, ext);
    
    if (!types.contains(id) && types.contains(base_id)) {
      // a type with new extensions got created, so we just add it to this scope.
      types.insert(id);
      return id;
    }
    
    if (types.contains(id)) return id;
    
    if (parent) {
      auto id = parent->find_type_id(name, ext);
      if (id != -1)
        return id;
    }
    
    return -1;
  }
  inline int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExt &ext) {
    auto base_id = global_find_type_id(name, info, {});
    auto id = global_find_type_id(name, info, ext);
     if (!types.contains(id) && types.contains(base_id)) {
      // a type with new extensions got created, so we just add it to this scope.
      types.insert(id);
      return id;
    }
    
    if (types.contains(id)) return id;
    
    if (parent) {
      auto id = parent->find_type_id(name, ext);
      if (id != -1)
        return id;
    }
    
    if (types.contains(id)) return id;
    return -1;
  }
  inline Type* get_type(int id) const {
    if (types.contains(id))
      return global_get_type(id);
    
    if (parent) {
      return parent->get_type(id);
    }
    // TODO: we should probably throw more information. this is basically nothing and you'd never be able to find where this happened
    throw_error(std::format("Use of undeclared type"), ERROR_FAILURE, {});
  }
  
  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : parent(parent), symbols({}) {
  }
  inline void insert(const std::string &name, int type_id, bool is_type_alias = false) {
    symbols[name] = Symbol{name, type_id, is_type_alias};
    if (is_type_alias) {
      global_type_aliases.insert({name, type_id});
    }
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
  
  inline void on_scope_enter() {
    for (const auto &[id, sym]: symbols) {
      if (sym.type_alias) {
        global_type_aliases.insert({id, sym.type_id});
      }
    }
  }
  inline void on_scope_exit() {
    for (const auto &[id, sym]: symbols) {
      if (sym.type_alias) {
        auto pointed_to = global_get_type(sym.type_id);
        auto it = global_type_aliases.find(sym.name);
        if (it != global_type_aliases.end())
          global_type_aliases.erase(it);
      }
    }
  }
  
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}

struct Context {
  // TODO: there's probably a much much better way to do this that doesn't intermix the entire
  // TODO(cont.): type system, ast, and emitting system so much
  // used by the type system to build type info instantiation code 
  // to be emitted to cpp.
  std::vector<std::string> type_info_strings;
  
  Scope *current_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  Scope *root_scope;
  Context();

  inline void set_scope(Scope *scope = nullptr) {
    if (!scope) {
      scope = create_child(current_scope);
    }
    current_scope = scope;
    scope->on_scope_enter();
    //printf("entering scope: %p\n", current_scope);
  }
  inline Scope *exit_scope() {
    auto scope = current_scope;
    if (current_scope) {
      current_scope->on_scope_exit();
      current_scope = current_scope->parent;
    }
    return scope;
  }
};