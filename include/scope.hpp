#pragma once

#include <set>
#include <unordered_map>
#include <unordered_set>

#include "arena.hpp"
#include "interned_string.hpp"
#include "type.hpp"
#include "value.hpp"

extern jstl::Arena scope_arena;
extern jstl::Arena symbol_arena;

struct ASTNode;
struct THIR;

enum Mutability : char {
  CONST,
  MUT,
};

struct Key {
  // the bare name of the symbol
  InternedString name;
  // the generic arguments this type or function was created with, to differentiate instantiations from templates
  // and create unique symbols per instantiation

  // *PERFORMANCE: use a pointer to a set of interned generic arguments, so we don't have to frequently copy and compare
  // *entire vectors everywhere. we can use this too for type extensions.
  std::vector<Type *> generics;

  bool is_generic_template;  // is this a generic-argument-free template of a generic type or symbol?

  inline bool operator==(const Key &other) const {
    return name == other.name && generics == other.generics && is_generic_template == other.is_generic_template;
  }

  struct Hash {
    std::size_t operator()(const Key &k) const {
      std::size_t h = std::hash<InternedString>{}(k.name);
      for (auto *t : k.generics) {
        h ^= std::hash<Type *>{}(t) + 0x9e3779b9 + (h << 6) + (h >> 2);
      }
      h ^= std::hash<bool>{}(k.is_generic_template) + 0x9e3779b9 + (h << 6) + (h >> 2);
      return h;
    }
  };

  static inline Key from(const InternedString &name, const std::vector<Type *> &generics = {},
                         bool is_generic_template = false) {
    return {name, generics, is_generic_template};
  }
};

extern size_t global_num_symbols_declared;

struct Symbol {
  size_t index = global_num_symbols_declared++;
  Scope *parent_scope;
  Type *type = Type::INVALID_TYPE;
  Mutability mutability = CONST;
  InternedString name = "";  // defaulted to "" to avoid annoying bugs.

  Nullable<ASTNode> ast = nullptr;
  Nullable<THIR> thir = nullptr;
  // used only by the ct interpreter and thir generator respectively for compile time execution
  Value *value = nullptr;
  Key key;  // the key that this is declared with in it's parent scope.

  inline bool is_const() const { return mutability == CONST; }
  inline bool is_mut() const { return mutability == MUT; }
};

// Used for aliasing.
struct Reference {
  Scope *scope;
  InternedString original_name;
  InternedString alias_name;
  bool operator==(const Reference &other) const { return original_name == other.original_name; }
  bool operator<(const Reference &other) const { return original_name < other.original_name; }
};

struct SymbolScopePair {
  Symbol *symbol;
  Scope *scope;
  bool has_value = false;
};

struct Scope final {
  InternedString name = "";
  bool is_module_scope = false;
  Scope *parent = nullptr;

  std::set<Reference> references;
  std::unordered_map<Key, Symbol *, Key::Hash> symbols = {};
  std::unordered_map<Key, Type *, Key::Hash> types;

  Scope(Scope *parent) : parent(parent) {}

  inline void insert(const InternedString &name, Type *type, Mutability mutability, ASTNode *ast,
                     const std::vector<Type *> &generics, bool is_generic_template) {
    Symbol *sym = new (symbol_arena.allocate(sizeof(Symbol))) Symbol();
    sym->parent_scope = this;
    sym->type = type;
    sym->mutability = mutability;
    sym->name = name;
    sym->ast = ast;
    sym->thir = nullptr;
    sym->value = nullptr;
    symbols.insert_or_assign(Key::from(name, generics, is_generic_template), sym);
  }

  // Your type should already have declaring_node set if applicable by now, we don't need to duplicate it in the scope
  // table.
  inline void insert_type(const InternedString &name, Type *type, const std::vector<Type *> &generics = {},
                          bool is_generic_template = false) {
    types.insert_or_assign(Key::from(name, generics, is_generic_template), type);
  }

  // use is_generic_template with an empty array for generics to find the actual template instead of a specific
  // instantiation of a generic.
  inline Symbol *find(const InternedString &name, const std::vector<Type *> generics = {},
                      bool is_generic_template = false) {
    Key key = Key::from(name, generics, is_generic_template);
    auto it = symbols.find(key);
    if (it != symbols.end()) {
      return it->second;
    }

    // Try to look up a reference from another scope.
    for (auto it = references.begin(); it != references.end(); ++it) {
      if (it->original_name == name || it->alias_name == name) {
        return it->scope->local_find(it->original_name, generics, is_generic_template);
      }
    }

    // if we can, lookup in parent.
    if (parent) {
      return parent->find(name, generics, is_generic_template);
    }

    // no symbol found.
    return nullptr;
  }

  // same as find, but we won't traverse into parents to try to find the symbol, it must exist
  // here as a concrete symbol or a reference to a symbol must exist here.
  inline Symbol *local_find(const InternedString &name, const std::vector<Type *> generics = {},
                            bool is_generic_template = false) {
    Key key = Key::from(name, generics, is_generic_template);
    auto it = symbols.find(key);
    if (it != symbols.end()) {
      return it->second;
    }

    // local lookup will still check references since this scope does in fact own the reference,
    // even if we don't own the symbol technically
    for (auto it = references.begin(); it != references.end(); ++it) {
      if (it->original_name == name || it->alias_name == name) {
        return it->scope->local_find(it->original_name, generics, is_generic_template);
      }
    }

    return nullptr;
  }

  // CLEANUP: why would we ever need to erase a symbol?
  // this seems bad, try to get rid of whatever logic is doing this.
  inline void erase(const InternedString &name, const std::vector<Type *> &generics, bool is_generic_template) {
    symbols.erase(Key::from(name, generics, is_generic_template));
  }

  Type *create_choice_type(const InternedString &name, Scope *scope, ASTNode *declaration,
                           const std::vector<Type *> &generics, bool is_generic_template) {
    auto type = global_create_choice_type(name, scope, generics);
    this->insert(name, type, CONST, (ASTNode *)declaration, generics, is_generic_template);
    return type;
  }

  Type *create_trait_type(const InternedString &name, Scope *scope, ASTNode *declaration,
                          const std::vector<Type *> &generics, bool is_generic_template) {
    auto type = global_create_trait_type(name, scope, generics);
    this->insert(name, type, CONST, (ASTNode *)declaration, generics, is_generic_template);
    return type;
  }

  Type *create_struct_type(const InternedString &name, Scope *scope, ASTNode *declaration,
                           const std::vector<Type *> &generics, bool is_generic_template) {
    auto type = global_create_struct_type(name, scope, generics);
    this->insert(name, type, CONST, (ASTNode *)declaration, generics, is_generic_template);
    return type;
  }

  void create_type_alias(const InternedString &name, Type *type, ASTNode *declaring_node,
                         const std::vector<Type *> &generics, bool is_generic_template) {
    this->insert(name, type, CONST, declaring_node, generics, is_generic_template);
  }

  Type *create_enum_type(const InternedString &name, Scope *scope, bool flags, ASTNode *declaration) {
    auto type = global_create_enum_type(name, scope, flags);
    // enums can't have generics, nor are they a template.
    this->insert(name, type, CONST, (ASTNode *)declaration, {}, false);
    return type;
  }

  Type *create_tuple_type(const std::vector<Type *> &types) {
    auto type = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // tuple types don't have associated AST
    // they can't have generics, nor are they a template.
    this->insert(name, type, CONST, nullptr, {}, false);
    return type;
  }

  void create_module(const InternedString &name, ASTNode *declaration) {
    // modules  can't have generics, nor are they a template.
    this->insert(name, nullptr, CONST, (ASTNode *)declaration, {}, false);
  }

  inline Type *find_type(const InternedString &name, const std::vector<Type *> &generics, bool is_generic_template,
                         const TypeExtensions &ext) {
    Key key = Key::from(name, generics, is_generic_template);

    auto it = types.find(key);
    if (it != types.end()) {
      Type *type = it->second;

      // if it already equals, we don't need to do the expensive global_find_type_id, just return it.
      if (type->extensions_equals(ext)) {
        return type;
      }

      if (type->has_extensions()) {  // don't double compound extensions up.
        type = type->base_type;
      }

      return global_find_type_id(type->base_type, ext);
    }

    if (parent) {
      return parent->find_type(name, generics, is_generic_template, ext);
    }

    return nullptr;
  }

  // CLEANUP: this should not be in the scope.
  // the typer should do this, we should simply use find_type to find a dyn type.
  Type *find_or_instantiate_dyn_type(Type *trait, SourceRange range, void *typer);

  inline std::string full_name() const {
    if (parent) {
      auto parent_name = parent->full_name();
      if (!parent_name.empty()) {
        return parent->full_name() + "$" + name.get_str();
      }
    }
    return name.get_str();
  }

  inline void create_reference(const InternedString &name, Scope *original_scope) {
    references.insert({original_scope, name});
  }

  inline void create_reference(const InternedString &original_name, Scope *original_scope,
                        const InternedString &aliased_name) {
    references.insert({.scope = original_scope, .original_name = original_name, .alias_name = aliased_name});
  }

  inline static std::unordered_set<InternedString> &defines() {
    static std::unordered_set<InternedString> defines;
    return defines;
  };

  inline static bool add_def(const InternedString &define) { return defines().insert(define).second; }

  inline static bool has_def(const InternedString &define) {
    if (defines().contains(define)) {
      return true;
    }
    return false;
  }

  inline static void undef(const InternedString &define) { defines().erase(define); }
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope(parent);
  return scope;
}

struct Context {
  Scope *root_scope = nullptr;
  Scope *scope = nullptr;
  Context();

  inline void enter_scope_or_create_and_enter_new_child_scope(Scope *in = nullptr) {
    if (!in) {
      in = create_child(scope);
    }
    scope = in;
  }

  Nullable<Symbol> get_symbol(ASTNode *node);
  Nullable<Scope> get_scope(ASTNode *node);

  SymbolScopePair get_symbol_and_scope(ASTNode *node) {
    const auto symbol = get_symbol(node);
    const auto scope = get_scope(node);
    if (!symbol || !scope) {
      return {};
    }
    return {
        .symbol = symbol.get(),
        .scope = scope.get(),
        .has_value = true,
    };
  }
};
