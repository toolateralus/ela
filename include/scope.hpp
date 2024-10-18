#pragma once

#include "error.hpp"
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
  std::vector<int> aliases;
  std::vector<int> types;
  
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
  
  /*  Type interactions  */
  
  int create_type_alias(int aliased, const std::string& name) {
    auto id = global_create_type_alias(aliased, name);
    aliases.push_back(id);
    types.push_back(id);
    if (this == root_scope) {
      type_alias_map[name] = id;
    }
    return id;
  }
  
  Type *get_type(const int id) {
    for (auto ty: types) 
      if (id == ty) 
        return global_get_type(id);
    
    if (parent) return parent->get_type(id);
    else {
      //! BUG remove this hack. This should not be neccessary.
      auto type = global_get_type(id);
      if (type && type->is_kind(TYPE_FUNCTION)) {
        return type;
      }
    }
    
    return nullptr;
  }

  int create_type(TypeKind kind, const std::string &name, TypeInfo * info = nullptr, const TypeExt &ext = {}) {
    auto id = global_create_type(kind, name, info, ext);
    types.push_back(id);
    return id;
  }

  int create_struct_type(const std::string &name, Scope *scope) {
    auto id = global_create_struct_type(name, scope);
    types.push_back(id);
    return id;
  }

  int create_enum_type(const std::string &name, const std::vector<std::string> &fields, bool flags) {
    auto id = global_create_enum_type(name, fields, flags);
    types.push_back(id);
    return id;
  }

  int create_tuple_type(const std::vector<int> &types, const TypeExt& ext) {
    auto id = global_create_tuple_type(types, ext);
    this->types.push_back(id);
    return id;
  }

  int create_union_type(const std::string &name, Scope *scope, UnionFlags kind) {
    auto id = global_create_union_type(name, scope, kind);
    types.push_back(id);
    return id;
  }
  
  int find_function_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExt &ext) {
    // We leave function types as global so that we don't have to recreate things like
    // void(int) over and over. This may inadvertently allow you to access types that are in your scope,
    // so
    // TODO: verify this isn't terrible.
    return global_find_function_type_id(name, info, ext);
  }

  int find_type_id(std::vector<int> &tuple_types, const TypeExt &type_extensions) {
    auto num = num_types;
    auto id = global_find_type_id(tuple_types, type_extensions);
    
    // if we extended a type, or if we created a new tuple,
    // but we have access to all of thee types within it,
    // we just add the type to our table.
    if (num_types > num) {
      // search for all the types within the tuple.
      for (auto t: tuple_types) {
        bool found = false;
        for (auto my_t : types) {
          if (my_t == t) {
            found =true;
            break;
          }
        }
        if (!found) {
          if (parent) {
            return parent->find_type_id(tuple_types, type_extensions);
          }
          return -1;
        }
      }
      types.push_back(id);
      return id;
    }
    for (const auto ty: types) {
      if (ty == id) {
        return id;
      }
    }
    if (parent) {
      return parent->find_type_id(tuple_types, type_extensions);
    }
    return -1;
  }

  int find_type_id(const std::string &name, const TypeExt &ext) {
    auto id = global_find_type_id(name, ext);
    auto base_id = global_find_type_id(name, {});
    
    // type does not exist globally.
    if (id == -1 && base_id == -1) {
      return -1;
    }
    
    bool has_base = false;
    
    for (const auto ty: types) {
      if (ty == id) {
        return ty;
      }
      if (ty == base_id) {
        has_base = true;
      }
    }
    
    if (!has_base) {
      if (parent) {
        return parent->find_type_id(name, ext);
      } else {
        return -1;
      }
    }
    
    for (const auto ty: types) {
      // We own this type, but a new type was created to add extensions.
      if (ty == base_id) {
        types.push_back(id);
        return id;
      }
    }
    if (parent) {
      return parent->find_type_id(name, ext);
    }
    return -1;
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
    if (scope == root_scope) {
      throw_error("Internal Compiler Error: attempted to exit the global scope.", {});
    }
    auto old_scope = scope;
    if (scope) {
      scope->on_scope_exit();
      scope = scope->parent;
    }
    return old_scope;
  }
};