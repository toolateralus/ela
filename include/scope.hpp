#pragma once

#include <string>
#include <unordered_map>
#include <unordered_set>

#include "arena.hpp"
#include "constexpr.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "type.hpp"

extern jstl::Arena scope_arena;
enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_HAS_OVERLOADS = 1 << 3,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 5,
};

struct ASTNode;

struct Symbol {
  InternedString name;
  int type_id;
  int flags = SYMBOL_IS_VARIABLE;
  std::vector<int> function_overload_types;
  Nullable<ASTNode> declaring_node;
  Value value;
  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
};

struct ASTFunctionDeclaration;
extern Scope *root_scope;
struct Scope {
  bool is_struct_or_union_scope = false;
  std::vector<InternedString> ordered_symbols;
  std::unordered_map<InternedString, Symbol> symbols;
  std::unordered_map<InternedString, int> types;

  static std::unordered_set<InternedString> &defines() {
    static std::unordered_set<InternedString> defines;
    return defines;
  };

  bool add_def(const InternedString &define) { return defines().insert(define).second; }

  bool has_def(const InternedString &define) const {
    if (defines().contains(define)) {
      return true;
    }
    return false;
  }

  void undef(const InternedString &define) { defines().erase(define); }

  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : symbols({}), parent(parent) {}

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function() && name != "this") fields++;
    }
    return fields;
  }

  void insert(const InternedString &name, int type_id, int flags = SYMBOL_IS_VARIABLE);

  Symbol *lookup(const InternedString &name);

  Symbol *local_lookup(const InternedString &name) {
    if (symbols.contains(name)) {
      return &symbols[name];
    }
    return nullptr;
  }

  void erase(const InternedString &name);

  int create_type(TypeKind kind, const InternedString &name, TypeInfo *info = nullptr, const TypeExtensions &ext = {}) {
    auto id = global_create_type(kind, name, info, ext);
    types[name] = id;
    return id;
  }

  int create_struct_type(const InternedString &name, Scope *scope) {
    auto id = global_create_struct_type(name, scope);
    types[name] = id;
    return id;
  }

  int create_enum_type(const InternedString &name, const std::vector<InternedString> &fields, bool flags) {
    auto id = global_create_enum_type(name, fields, flags);
    types[name] = id;
    return id;
  }

  int create_tuple_type(const std::vector<int> &types, const TypeExtensions &ext) {
    auto id = global_create_tuple_type(types, ext);
    this->types[get_tuple_type_name(types)] = id;
    return id;
  }

  int create_union_type(const InternedString &name, Scope *scope, UnionFlags kind) {
    auto id = global_create_union_type(name, scope, kind);
    types[name] = id;
    return id;
  }

  int find_type_id(const InternedString &name, const TypeExtensions &ext) {
    if (!types.contains(name)) {
      if (parent) {
        return parent->find_type_id(name, ext);
      } else {
        return Type::invalid_id;
      }
    }
    return global_find_type_id(types[name], ext);
  }
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
  return scope;
}

struct Context {
  // CLEANUP(Josh) 10/14/2024, 10:07:07 AM
  // This type_info_strings field should be in the emit visitor.
  // That's the only place it's used anyway.
  std::vector<InternedString> type_info_strings;

  Scope *scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  Context();
  inline void set_scope(Scope *in_scope = nullptr) {
    if (!in_scope) {
      in_scope = create_child(scope);
    }
    scope = in_scope;
  }
  inline Scope *exit_scope() {
    if (scope == root_scope) {
      throw_error("Internal Compiler Error: attempted to exit the global scope.", {});
    }
    auto old_scope = scope;
    if (scope) {
      scope = scope->parent;
    }
    return old_scope;
  }
};
