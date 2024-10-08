#pragma once

#include "type.hpp"
#include <jstl/memory/arena.hpp>
#include <string>
#include <unordered_map>

extern jstl::Arena scope_arena;
enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_HAS_OVERLOADS = 1 << 3,
  SYMBOL_WAS_MUTATED = 1 << 4,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 5,
};

struct ASTNode;

struct Symbol {
  std::string name;
  int type_id;
  int flags = SYMBOL_IS_VARIABLE;
  std::vector<int> function_overload_types;
  Nullable<ASTNode> declaring_node;
  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
};


struct ASTFunctionDeclaration;
extern Scope *root_scope;
struct Scope {

  void report_symbol_mutated(const std::string &name) {
    if (symbols.contains(name))
      symbols[name].flags |= SYMBOL_WAS_MUTATED;
    else if (parent) {
      parent->report_symbol_mutated(name);
    }
  }

  bool is_struct_or_union_scope = false;
  
  std::vector<std::string> ordered_symbols;
  std::unordered_map<std::string, Symbol> symbols;

  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : parent(parent), symbols({}) {}

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function())
        fields++;
    }
    return fields;
  }

  void insert(const std::string &name, int type_id,
              int flags = SYMBOL_IS_VARIABLE);
  Symbol *lookup(const std::string &name);
  
  Symbol *local_lookup(const std::string &name) {
    if (symbols.contains(name)) {
      return &symbols[name];
    }
    return nullptr;
  }
  
  void erase(const std::string &name);
  
  std::vector<int> aliases;
  int create_type_alias(int aliased, const std::string& name) {
    auto id = global_create_type_alias(aliased, name);
    aliases.push_back(id);
    return id;
  }
  void on_scope_enter() {
    for (const auto &alias: aliases) {
      auto type = global_get_type(alias);
      type_alias_map[type->get_base()] = alias;
    }
  }
  void on_scope_exit() {
    for (const auto &alias: aliases) {
      auto type = global_get_type(alias);
      type_alias_map.erase(type->get_base());
    }
  }
  
};
static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}
struct Context {
  
  // TODO: clean this system up so we don't have to generate c++ code in the type system.
  // Would be much more preferable to have something that's flexible to various backends.
  std::vector<std::string> type_info_strings;
  Scope *scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  Context();
  inline void set_scope(Scope *in_scope = nullptr) {
    if (!in_scope) {
      in_scope = create_child(scope);
    }
    scope = in_scope;
    scope->on_scope_enter();
  }
  inline Scope *exit_scope() {
    auto old_scope = scope;
    if (scope) {
      scope = scope->parent;
    }
    scope->on_scope_exit();
    return old_scope;
  }
};