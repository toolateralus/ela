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
struct Symbol {
  InternedString name;
  Type *resolved_type = Type::INVALID_TYPE;
  Mutability mutability = CONST;
  Scope *parent_scope;

  THIR *thir;

  bool is_variable : 1 = false;
  bool is_function : 1 = false;
  bool is_forward_declared : 1 = false;
  bool is_type : 1 = false;
  bool is_module : 1 = false;
  bool is_local : 1 = false;

  bool is_const() const { return mutability == CONST; }
  bool is_mut() const { return mutability == MUT; }

  Value* value = nullptr;

  union {
    struct {
      Nullable<ASTNode> declaration;
      // This is null for almost every variable besides constant declarations,
      // i.e `CONSTANT_DECLARATION :: 100 * 2` or whatever
      Nullable<ASTExpr> initial_value;
    } variable;
    struct {
      ASTFunctionDeclaration *declaration;
    } function;
    struct {
      ASTModule *declaration;
    } module;
    struct {
      // ? Why is this here? Just to confirm the type is a child of a choice type?
      Nullable<ASTChoiceDeclaration> choice;
      // This is nullable purely because `tuple` types do not have a declaring node!
      // Otherwise, all other nodes have this property, and must.
      Nullable<ASTNode> declaration;
    } type;
  };

  Symbol() {}
  ~Symbol() {}

  static Symbol create_variable(const InternedString &name, Type *type, ASTExpr *initial_value, ASTNode *decl,
                                Mutability mutability) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = type;
    symbol.is_variable = true;
    symbol.variable.initial_value = initial_value;
    symbol.variable.declaration = decl;
    symbol.mutability = mutability;
    return symbol;
  }

  static Symbol create_function(const InternedString &name, Type *type, ASTFunctionDeclaration *declaration) {
    Symbol symbol;
    symbol.resolved_type = type;
    symbol.name = name;
    symbol.is_function = true;
    symbol.function.declaration = declaration;
    return symbol;
  }

  static Symbol create_type(Type *type, const InternedString &name, ASTNode *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.is_type = true;
    symbol.type.declaration = declaration;
    symbol.resolved_type = type;
    return symbol;
  }

  static Symbol create_module(const InternedString &name, ASTModule *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.is_module = true;
    symbol.module.declaration = declaration;
    return symbol;
  }

  bool is_generic_function() const;
};

struct Typer;
struct ASTFunctionDeclaration;
struct ASTTraitDeclaration;

struct SymbolReference {
  Scope *scope;
  InternedString original_name;
  InternedString alias_name;
  bool operator==(const SymbolReference &other) const { return original_name == other.original_name; }

  bool operator<(const SymbolReference &other) const { return original_name < other.original_name; }
};

struct SymbolScopePair {
  Symbol *symbol;
  Scope *scope;
  bool has_value = false;
};

struct Scope {
  std::set<SymbolReference> references;
  std::unordered_map<InternedString, Symbol> symbols = {};
  InternedString name = "";
  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : symbols({}), parent(parent) {}
  void insert_local_variable(const InternedString &name, Type *type_id, ASTExpr *initial_value, Mutability mutability,
                             ASTNode *decl = nullptr) {
    auto sym = Symbol::create_variable(name, type_id, initial_value, decl, mutability);
    sym.is_local = true;
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  void insert_variable(const InternedString &name, Type *type_id, ASTExpr *initial_value, Mutability mutability,
                       ASTNode *decl = nullptr) {
    auto sym = Symbol::create_variable(name, type_id, initial_value, decl, mutability);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  void forward_declare_function(const InternedString &name, Type *type_id, ASTFunctionDeclaration *declaration) {
    auto sym = Symbol::create_function(name, type_id, declaration);
    sym.is_forward_declared = true;
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  void insert_function(const InternedString &name, Type *type_id, ASTFunctionDeclaration *declaration) {
    auto sym = Symbol::create_function(name, type_id, declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  void insert_type(Type *type_id, const InternedString &name, ASTNode *declaration) {
    auto sym = Symbol::create_type(type_id, name, declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  Symbol *lookup(const InternedString &name);
  Symbol *local_lookup(const InternedString &name);
  void erase(const InternedString &name);
  Type *create_tagged_union(const InternedString &name, Scope *scope, ASTChoiceDeclaration *declaration) {
    auto type = global_create_choice_type(name, scope, {});
    auto sym = Symbol::create_type(type, name, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }
  Type *create_trait_type(const InternedString &name, Scope *scope, const std::vector<Type *> &generic_args,
                          ASTTraitDeclaration *declaration) {
    auto type = global_create_trait_type(name, scope, generic_args);
    auto sym = Symbol::create_type(type, name, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }
  Type *create_struct_type(const InternedString &name, Scope *scope, ASTStructDeclaration *declaration) {
    auto type = global_create_struct_type(name, scope);
    auto sym = Symbol::create_type(type, name, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);

    return type;
  }
  void create_type_alias(const InternedString &name, Type *type_id, ASTNode *declaring_node) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = type_id;
    symbol.is_type = true;
    symbol.type.declaration = declaring_node;
    symbols.erase(name);
    symbol.parent_scope = this;
    symbols.insert_or_assign(name, symbol);
  }
  void forward_declare_type(const InternedString &name, Type *default_id) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = default_id;
    symbol.is_type = true;
    symbol.parent_scope = this;
    symbols.insert_or_assign(name, symbol);
  }
  Type *create_enum_type(const InternedString &name, Scope *scope, bool flags, ASTEnumDeclaration *declaration) {
    auto type = global_create_enum_type(name, scope, flags);
    auto sym = Symbol::create_type(type, name, (ASTNode *)declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }
  void create_module(const InternedString &name, ASTModule *declaration) {
    auto sym = Symbol::create_module(name, declaration);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
  }
  Type *create_tuple_type(const std::vector<Type *> &types) {
    auto type = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
    auto sym = Symbol::create_type(type, name, nullptr);
    sym.parent_scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }
  Type *find_type_id(const InternedString &name, const TypeExtensions &ext) {
    auto symbol = lookup(name);
    if (!symbol || !symbol->is_type) {
      if (parent) {
        return parent->find_type_id(name, ext);
      } else {
        return Type::INVALID_TYPE;
      }
    }
    return global_find_type_id(symbol->resolved_type, ext);
  }
  Type *find_or_create_dyn_type_of(Type *trait, SourceRange range, Typer *typer);
  // get the count of non-function variables in this scope.
  inline size_t fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function && !sym.is_type) fields++;
    }
    return fields;
  }
  inline std::string full_name() const {
    if (parent) {
      auto parent_name = parent->full_name();
      if (!parent_name.empty()) {
        return parent->full_name() + "$" + name.get_str();
      }
    }
    return name.get_str();
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
  size_t methods_count() const;

  void create_reference(SymbolScopePair pair);
  inline void create_reference(const InternedString &name, Scope *original_scope) {
    references.insert({original_scope, name});
  }

  void create_reference(const InternedString &original_name, Scope *original_scope,
                        const InternedString &aliased_name) {
    references.insert({.scope = original_scope, .original_name = original_name, .alias_name = aliased_name});
  }
};

static Scope *create_child(Scope *parent) {
  auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  scope->parent = parent;
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
