#pragma once

#include <unordered_set>

#include "arena.hpp"
#include "constexpr.hpp"
#include "interned_string.hpp"

extern jstl::Arena symbol_arena;

enum TypeKind {
  TYPE_SCALAR,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_ENUM,
  TYPE_TUPLE,
  TYPE_INTERFACE,
};


enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 2,
  SYMBOL_IS_TYPE = 1 << 3,
};

struct AST;
struct ASTStructDeclaration;
struct AST;
struct ASTEnumDeclaration;

struct Symbol {
  Symbol *next = nullptr;
  InternedString name;
  int type_id = -1;
  int flags = SYMBOL_IS_VARIABLE;
  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
  bool is_variable() const { return (flags & SYMBOL_IS_VARIABLE) != 0; }
  bool is_type() const { return (flags & SYMBOL_IS_TYPE) != 0; }
  bool is_forward_declared() const { return (flags & SYMBOL_IS_FORWARD_DECLARED) != 0; }

  union {
    struct {
      Nullable<AST> declaration;
      Value value;
      // This is null for almost every variable besides constant declarations,
      // i.e `CONSTANT_DECLARATION :: 100 * 2` or whatever
      Nullable<AST> initial_value;
    } variable;
    struct {
      AST *declaration;
    } function;
    struct {
      // This is nullable purely because `tuple` types do not have a declaring node!
      // Otherwise, all other nodes have this property, and must.
      Nullable<AST> declaration;
      TypeKind kind;
    } type;
  };

  Symbol() {}
  ~Symbol() {}

  static Symbol create_variable(const InternedString &name, int type_id, AST *initial_value, AST* decl) {
    Symbol symbol;
    symbol.name = name;
    symbol.type_id = type_id;
    symbol.flags = SYMBOL_IS_VARIABLE;
    symbol.variable.initial_value = initial_value;
    symbol.variable.declaration = decl;
    return symbol;
  }

  static Symbol create_function(const InternedString &name, const int type_id, AST *declaration, SymbolFlags flags) {
    Symbol symbol;
    symbol.type_id = type_id;
    symbol.name = name;
    symbol.flags = flags;
    symbol.function.declaration = declaration;
    return symbol;
  }

  static Symbol create_type(const int type_id, const InternedString &name, TypeKind kind, AST *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.kind = kind;
    symbol.type.declaration = declaration;
    symbol.type_id = type_id;
    return symbol;
  }

};

struct Scope {
  Symbol *head;
  Symbol *lookup(const InternedString &name);
  bool erase(const InternedString &name);
  void insert(const Symbol &symbol);

  // TODO:
  // One problem with this way of doing scope, is that we're using an arena for symbols.
  // So, when we do this, we basically just leak everything that had been, and cannot ever reclaim
  // that memory. This is fine, because this is done very rarely, and can probably be negated in another way
  // however, it's something to think about.
  void clear() {
    head = nullptr;
  }
};

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

