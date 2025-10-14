#pragma once

#include <set>
#include <unordered_map>
#include <unordered_set>

#include "arena.hpp"
#include "interned_string.hpp"
#include "type.hpp"
#include "value.hpp"

extern jstl::Arena scope_arena;

struct ASTNode;
struct ASTStructDeclaration;
struct ASTFunctionDeclaration;
struct ASTChoiceDeclaration;
struct ASTEnumDeclaration;
struct ASTModule;

enum Mutability : char {
  CONST,
  MUT,
};

struct Scope;
struct THIR;
struct ASTNode;

struct Key {
  // the bare name of the symbol
  InternedString name;
  // the generic arguments this type or function was created with, to differentiate instantiations from templates
  // and create unique symbols per instantiation
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
  std::unordered_map<Key, Symbol, Key::Hash> symbols = {};
  std::unordered_map<Key, Type *, Key::Hash> types;

  Scope(Scope *parent) : parent(parent) {}

  inline void insert(const InternedString &name, Type *type, Mutability mutability, ASTNode *ast,
                     const std::vector<Type *> &generics, bool is_generic_template) {
    Symbol sym = {
        .parent_scope = this,
        .type = type,
        .mutability = mutability,
        .name = name,
        .ast = ast,
        .thir = nullptr,
        .value = nullptr,
    };
    symbols.insert_or_assign(Key::from(name, generics, is_generic_template), sym);
  }

  // use is_generic_template with an empty array for generics to find the actual template instead of a specific
  // instantiation of a generic.
  inline Symbol *find(const InternedString &name, const std::vector<Type *> generics = {},
                      bool is_generic_template = false) {
    Key key = Key::from(name, generics, is_generic_template);
    if (Symbol *symbol = &symbols[key]) {
      return symbol;
    } else {
      // stupid c++, access == insertion.
      // still preferred over .contains which guarantees 2 hashes, here we might get lucky and only hash once.
      symbols.erase(key);
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
    if (Symbol *symbol = &symbols[key]) {
      return symbol;
    } else {
      // see lookup to understand why we erase.
      symbols.erase(key);
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

  Type *create_choice_type(const InternedString &name, Scope *scope, ASTChoiceDeclaration *declaration,
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

  void create_module(const InternedString &name, ASTModule *declaration) {
    // modules  can't have generics, nor are they a template.
    this->insert(name, nullptr, CONST, (ASTNode *)declaration, {}, false);
  }

  Type *create_tuple_type(const std::vector<Type *> &types) {
    auto type = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // tuple types don't have associated AST
    // they can't have generics, nor are they a template.
    this->insert(name, type, CONST, nullptr, {}, false);  
    return type;
  }

  Type *find_type(const InternedString &name, const TypeExtensions &ext) {
    auto symbol = find(name);

    if (!symbol) {
      if (parent) {
        return parent->find_type(name, ext);
      } else {
        return Type::INVALID_TYPE;
      }
    }

    // TODO: this shouldn't happen, if you can't find the symbol in the scope, you don't have access to it.
    return global_find_type_id(symbol->type, ext);
  }

  // CLEANUP: this should not be in the scope.
  // the typer should do this, we should simply use find_type to find a dyn type.
  Type *find_or_instantiate_dyn_type(Type *trait, SourceRange range, Typer *typer);

  inline std::string full_name() const {
    if (parent) {
      auto parent_name = parent->full_name();
      if (!parent_name.empty()) {
        return parent->full_name() + "$" + name.get_str();
      }
    }
    return name.get_str();
  }

  void create_reference(SymbolScopePair pair);
  inline void create_reference(const InternedString &name, Scope *original_scope) {
    references.insert({original_scope, name});
  }
  void create_reference(const InternedString &original_name, Scope *original_scope,
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
  inline void set_scope(Scope *in_scope = nullptr) {
    if (!in_scope) {
      in_scope = create_child(scope);
    }
    scope = in_scope;
  }

  // !This should only be used really in the parser.

  // ONLY use this for exiting a scope you JUST created.
  // just store the scope you left in any other case,
  // and return in the appropriate place.
  // this has adverse effects in many places.
  inline Scope *exit_scope() {
    auto old_scope = scope;
    if (scope) {
      scope = scope->parent;
    }
    return old_scope;
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
