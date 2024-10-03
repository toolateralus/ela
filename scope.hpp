#pragma once

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

enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_IS_TYPE_ALIAS = 1 << 2,
};

struct Symbol {
  // the identifier.
  std::string name;
  // if this is a type alias, this
  // is the type that this identifier points to.
  int type_id;
  int flags = SYMBOL_IS_VARIABLE;
  
  std::vector<std::vector<int>> parameter_signatures;
  
  bool is_type_alias() const {
    return (flags & SYMBOL_IS_TYPE_ALIAS) != 0;
  }
  
  bool is_function() const {
    return (flags & SYMBOL_IS_FUNCTION) != 0;
  }
  
};

struct ASTFunctionDeclaration;

struct Scope {
  // TODO(Josh) 10/1/2024, 1:03:34 PM Replace this with a set of flags or something.
  bool is_struct_or_union_scope = false;
  std::unordered_map<std::string, Symbol> symbols;
  std::set<int> types;
  
  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : parent(parent), symbols({}) {}
  
  inline void create_type_alias(const std::string &alias, int type) {
    insert(alias, type, SYMBOL_IS_TYPE_ALIAS);
    global_type_aliases[alias] = type;
  }
  
  // TODO: verify we need a copy of this
  std::string get_function_typename(ASTFunctionDeclaration *decl);
  int find_alias(const std::string name, const TypeExt &ext);
  int find_type_id(const std::string &name, const TypeExt &ext);
  int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                   const TypeExt &ext);
                   
  Type *get_type(int id) const;
  int create_struct_type(const std::string &name, Scope *scope);
  int create_union_type(const std::string &name, Scope *scope, UnionKind kind);
  int create_enum_type(const std::string &name,
                       const std::vector<std::string> &keys, bool is_flags);
  
  
  int create_type(TypeKind kind, const std::string &name, TypeInfo *info,
                  const TypeExt &extensions);
  
  void insert(const std::string &name, int type_id, int flags = SYMBOL_IS_VARIABLE);
  Symbol *lookup(const std::string &name);
  void erase(const std::string &name);

  void on_scope_enter();
  void on_scope_exit();
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}

struct Context {
  // TODO: there's probably a much much better way to do this that doesn't
  // intermix the entire
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
    // printf("entering scope: %p\n", current_scope);
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