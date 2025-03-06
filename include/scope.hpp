#pragma once

#include <unordered_map>
#include <unordered_set>

#include "arena.hpp"
#include "constexpr.hpp"
#include "interned_string.hpp"
#include "type.hpp"

extern jstl::Arena scope_arena;

enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 2,
  SYMBOL_IS_TYPE = 1 << 3,
  SYMBOL_IS_MODULE = 1 << 4,
};

struct ASTNode;
struct ASTStructDeclaration;
struct ASTFunctionDeclaration;
struct ASTTaggedUnionDeclaration;
struct ASTEnumDeclaration;
struct ASTModule;

enum Mutability {
  CONST,
  MUT,
};

struct Symbol {
  InternedString name;
  int type_id = -1;
  int flags = SYMBOL_IS_VARIABLE;

  Mutability mutability = CONST;

  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
  bool is_variable() const { return (flags & SYMBOL_IS_VARIABLE) != 0; }
  bool is_type() const { return (flags & SYMBOL_IS_TYPE) != 0; }
  bool is_module() const { return (flags & SYMBOL_IS_MODULE) != 0; }
  bool is_forward_declared() const { return (flags & SYMBOL_IS_FORWARD_DECLARED) != 0; }

  union {
    struct {
      Nullable<ASTNode> declaration;
      Value value;
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
      // This is nullable purely because `tuple` types do not have a declaring node!
      // Otherwise, all other nodes have this property, and must.
      Nullable<ASTNode> declaration;
      TypeKind kind;
    } type;
  };

  Symbol() {}
  ~Symbol() {}

  static Symbol create_variable(const InternedString &name, int type_id, ASTExpr *initial_value, ASTNode* decl, Mutability mutability) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = type_id;
    symbol.flags = SYMBOL_IS_VARIABLE;
    symbol.variable.initial_value = initial_value;
    symbol.variable.declaration = decl;
    symbol.mutability = mutability;
    return symbol;
  }

  static Symbol create_function(const InternedString &name, const int type_id, ASTFunctionDeclaration *declaration, SymbolFlags flags) {
    Symbol symbol;
    symbol.type_id = type_id;
    symbol.name = name;
    symbol.flags = flags;
    symbol.function.declaration = declaration;
    return symbol;
  }

  static Symbol create_type(const int type_id, const InternedString &name, TypeKind kind, ASTNode *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.kind = kind;
    symbol.type.declaration = declaration;
    symbol.type_id = type_id;
    return symbol;
  }

  static Symbol create_module(const InternedString &name, ASTModule *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.flags = SYMBOL_IS_MODULE;
    symbol.module.declaration = declaration;
    return symbol;
  }
};

struct ASTFunctionDeclaration;
struct ASTInterfaceDeclaration;


struct Scope {
  std::vector<InternedString> ordered_symbols = {};
  std::unordered_map<InternedString, Symbol> symbols = {};

  static std::unordered_set<InternedString> &defines() {
    static std::unordered_set<InternedString> defines;
    return defines;
  };

  static bool add_def(const InternedString &define) { return defines().insert(define).second; }
  static bool has_def(const InternedString &define) {
    if (defines().contains(define)) {
      return true;
    }
    return false;
  }
  static void undef(const InternedString &define) { defines().erase(define); }

  Scope *parent = nullptr;
  Scope(Scope *parent = nullptr) : symbols({}), parent(parent) {}

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function())
        fields++;
    }
    return fields;
  }

  void insert_variable(const InternedString &name, int type_id, ASTExpr *initial_value, Mutability mutability, ASTNode* decl = nullptr) {
    symbols.insert_or_assign(name, Symbol::create_variable(name, type_id, initial_value, decl, mutability));
    ordered_symbols.push_back(name);
  }

  void insert_function(const InternedString &name, const int type_id, ASTFunctionDeclaration *declaration, SymbolFlags flags = SYMBOL_IS_FUNCTION) {
    symbols.insert_or_assign(name, Symbol::create_function(name, type_id, declaration, flags));
    ordered_symbols.push_back(name);
  }

  void insert_type(const int type_id, const InternedString &name, TypeKind kind, ASTNode *declaration) {
    symbols.insert_or_assign(name, Symbol::create_type(type_id, name, kind, declaration));
    ordered_symbols.push_back(name);
  }


  Symbol *lookup(const InternedString &name);

  Symbol *local_lookup(const InternedString &name) {
    if (symbols.contains(name)) {
      return &symbols[name];
    }
    return nullptr;
  }

  void erase(const InternedString &name);

  void declare_interface(const InternedString &name, ASTInterfaceDeclaration *node);

  int create_tagged_union(const InternedString &name, Scope *scope, ASTTaggedUnionDeclaration *declaration) {
    auto id = global_create_tagged_union_type(name, scope, {});
    symbols.insert_or_assign(name, Symbol::create_type(id, name, TYPE_TAGGED_UNION, (ASTNode*)declaration));
    return id;
  }

  int create_interface_type(const InternedString &name, Scope *scope, const std::vector<int> &generic_args, ASTInterfaceDeclaration *declaration) {
    auto id = global_create_interface_type(name, scope, generic_args);
    symbols.insert_or_assign(name, Symbol::create_type(id, name, TYPE_INTERFACE, (ASTNode*)declaration));
    return id;
  }

  int create_struct_type(const InternedString &name, Scope *scope, ASTStructDeclaration *declaration) {
    auto id = global_create_struct_type(name, scope);
    symbols.insert_or_assign(name, Symbol::create_type(id, name, TYPE_STRUCT, (ASTNode*)declaration));
    return id;
  }

  void create_type_alias(const InternedString &name, int type_id, TypeKind kind, ASTNode *declaring_node) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = type_id;
    symbol.type.kind = kind;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.declaration = declaring_node;
    symbols.erase(name);
    symbols.insert_or_assign(name, symbol);
  }

  void forward_declare_type(const InternedString &name, int default_id) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = default_id;
    symbol.flags = SYMBOL_IS_TYPE;
    symbols.insert_or_assign(name, symbol);
  }

  int create_enum_type(const InternedString &name, Scope *scope, bool flags, ASTEnumDeclaration *declaration) {
    auto id = global_create_enum_type(name, scope, flags);
    symbols.insert_or_assign(name, Symbol::create_type(id, name, TYPE_STRUCT, (ASTNode*)declaration));
    return id;
  }

  void create_module(const InternedString &name, ASTModule *declaration) {
    symbols.insert_or_assign(name, Symbol::create_module(name, declaration));
  }

  int create_tuple_type(const std::vector<int> &types) {
    auto id = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
    symbols.insert_or_assign(name, Symbol::create_type(id, name, TYPE_STRUCT, nullptr));
    return id;
  }

  int find_type_id(const InternedString &name, const TypeExtensions &ext) {
    auto symbol = lookup(name);
    if (!symbol || !symbol->is_type()) {
      if (parent) {
        return parent->find_type_id(name, ext);
      } else {
        return Type::INVALID_TYPE_ID;
      }
    }
    return global_find_type_id(symbol->type_id, ext);
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
  std::vector<InternedString> type_info_strings {};
  Scope *root_scope = nullptr;
  Scope *scope = nullptr;
  Context();
  inline void set_scope(Scope *in_scope = nullptr) {
    if (!in_scope) {
      in_scope = create_child(scope);
    }
    scope = in_scope;
  }
  inline Scope *exit_scope() {
    auto old_scope = scope;
    if (scope) {
      scope = scope->parent;
    }
    return old_scope;
  }
  Nullable<Symbol> get_symbol(ASTNode *node);
  Nullable<Scope> get_scope(ASTNode *node);
};
