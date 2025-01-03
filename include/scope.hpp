#pragma once

#include "arena.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "type.hpp"
#include <string>
#include <unordered_map>
#include <unordered_set>

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
  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
};

struct ASTFunctionDeclaration;
extern Scope *root_scope;
struct Scope {
  bool is_struct_or_union_scope = false;
  std::vector<InternedString> ordered_symbols;
  std::unordered_map<InternedString, Symbol> symbols;
  std::vector<int> aliases;
  std::unordered_set<int> types;

  static std::unordered_set<InternedString> &defines() {
    static std::unordered_set<InternedString> defines;
    return defines;
  };

  bool add_def(const InternedString &define) {
    return defines().insert(define).second;
  }

  bool has_def(const InternedString &define) const {
    if (defines().contains(define)) {
      return true;
    }
    return false;
  }

  void undef(const InternedString &define) {
    defines().erase(define);
  }

  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : symbols({}), parent(parent) {}

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function() && name != "this")
        fields++;
    }
    return fields;
  }

  void insert(const InternedString &name, int type_id,
              int flags = SYMBOL_IS_VARIABLE);

  Symbol *lookup(const InternedString &name);

  Symbol *local_lookup(const InternedString &name) {
    if (symbols.contains(name)) {
      return &symbols[name];
    }
    return nullptr;
  }

  void erase(const InternedString &name);

  void on_scope_enter() {
    for (const auto &alias : aliases) {
      auto type = global_get_type(alias);
      type_alias_map[type->get_base()] = alias;
    }
  }

  void on_scope_exit() {
    for (const auto &alias : aliases) {
      auto type = global_get_type(alias);
      type_alias_map.erase(type->get_base());
    }
  }

  /*  Type interactions  */

  int create_type_alias(int aliased, const InternedString &name) {
    auto id = global_create_type_alias(aliased, name);
    aliases.push_back(id);
    types.insert(id);
    if (this == root_scope) {
      type_alias_map[name] = id;
    }
    return id;
  }

  inline bool is_ancestor(Scope *ancestor) {
    Scope *current = this;
    while (current != nullptr) {
      if (current == ancestor) {
        return true;
      }
      current = current->parent;
    }
    return false;
  }

  Type *get_type(const int id) {
    if (types.contains(id)) {
      return global_get_type(id);
    }

    for (auto alias : aliases) {
      if (alias == id) {
        return global_get_type(id);
      }
    }

    if (parent)
      return parent->get_type(id);
    else {
      //! BUG remove this hack. This should not be neccessary.
      auto type = global_get_type(id);
      if (type && type->is_kind(TYPE_FUNCTION)) {
        return type;
      }
    }

    return nullptr;
  }

  int create_type(TypeKind kind, const InternedString &name,
                  TypeInfo *info = nullptr, const TypeExt &ext = {}) {
    auto id = global_create_type(kind, name, info, ext);
    types.insert(id);
    return id;
  }

  int create_struct_type(const InternedString &name, Scope *scope) {
    auto id = global_create_struct_type(name, scope);
    types.insert(id);
    return id;
  }

  int create_enum_type(const InternedString &name,
                       const std::vector<InternedString> &fields, bool flags) {
    auto id = global_create_enum_type(name, fields, flags);
    types.insert(id);
    return id;
  }

  int create_tuple_type(const std::vector<int> &types, const TypeExt &ext) {
    auto id = global_create_tuple_type(types, ext);
    this->types.insert(id);
    return id;
  }

  int create_union_type(const InternedString &name, Scope *scope,
                        UnionFlags kind) {
    auto id = global_create_union_type(name, scope, kind);
    types.insert(id);
    return id;
  }

  int find_function_type_id(const InternedString &name,
                            const FunctionTypeInfo &info, const TypeExt &ext) {
    // We leave function types as global so that we don't have to recreate
    // things like void(int) over and over. This may inadvertently allow you to
    // access types that are in your scope, so
    // TODO: verify this isn't terrible.
    auto id = global_find_function_type_id(name, info, ext);
    ;
    types.insert(id);
    root_scope->types.insert(id);
    return id;
  }
  int find_type_id(std::vector<int> &tuple_types,
                   const TypeExt &type_extensions, bool was_created = false) {
    auto num = type_table.size();
    auto id = global_find_type_id(tuple_types, type_extensions);

    // if we extended a type, or if we created a new tuple,
    // but we have access to all of thee types within it,
    // we just add the type to our table.
    if (type_table.size() > num || was_created) {
      // search for all the types within the tuple.
      for (auto t : tuple_types) {
        if (!types.contains(t)) {
          was_created = true;
          goto try_find_in_parent;
        }
      }
      types.insert(id);
      return id;
    }

    if (types.contains(id))
      return id;

  try_find_in_parent:
    if (parent) {
      return parent->find_type_id(tuple_types, type_extensions, was_created);
    }
    return -1;
  }

  int find_type_id(const InternedString &name, const TypeExt &ext) {
    auto id = global_find_type_id(name, ext);
    auto base_id = global_find_type_id(name, {});

    // type does not exist globally.
    if (id == -1 && base_id == -1) {
      return -1;
    }

    if (types.contains(id)) {
      return id;
    }

    bool has_base = types.contains(base_id);

    if (has_base) {
      types.insert(id);
      return id;
    }

    if (parent) {
      return parent->find_type_id(name, ext);
    }
    return -1;
  }

  int get_pointer_to_type(int base) {
    auto type = get_type(base);
    auto extensions = type->get_ext();
    extensions.extensions.push_back({TYPE_EXT_POINTER});
    auto id = find_type_id(type->get_base(), extensions);
    if (id == -1) {
      throw_error("Failed to get pointer to type", {});
    }
    return id;
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
    scope->on_scope_enter();
  }
  inline Scope *exit_scope() {
    if (scope == root_scope) {
      throw_error(
          "Internal Compiler Error: attempted to exit the global scope.", {});
    }
    auto old_scope = scope;
    if (scope) {
      scope->on_scope_exit();
      scope = scope->parent;
    }
    return old_scope;
  }
};
