#pragma once

#include <unordered_set>
#include <vector>

#include "arena.hpp"
#include "constexpr.hpp"
#include "interned_string.hpp"

extern jstl::Arena symbol_arena;

enum Type_Kind : unsigned char {
  TYPE_SCALAR,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_ENUM,
  TYPE_TUPLE,
  TYPE_INTERFACE,
};

enum SymbolFlags : unsigned char {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 2,
  SYMBOL_IS_TYPE = 1 << 3,
};

struct AST;

struct GenericInstance {
  std::vector<int> arguments;
  AST *node;
};

struct Symbol {
  Symbol *next = nullptr;
  Interned_String name;
  int type_id = -1;
  int flags = SYMBOL_IS_VARIABLE;
  bool is_forward_declared() const { return (flags & SYMBOL_IS_FORWARD_DECLARED) != 0; }
  std::vector<GenericInstance> generic_instantiations;
  // This is nullable purely because `tuple` types do not have a declaring node!
  // Otherwise, all other nodes have this property, and must.
  Nullable<AST> declaration;
  // This is null for almost every variable besides constant declarations,
  // i.e `CONSTANT_DECLARATION :: 100 * 2` or whatever
  Nullable<AST> initial_value;


  bool is_function() const { return (flags & SYMBOL_IS_FUNCTION) != 0; }
  bool is_variable() const { return (flags & SYMBOL_IS_VARIABLE) != 0; }
  bool is_type() const { return (flags & SYMBOL_IS_TYPE) != 0; }

  Symbol(const Interned_String &name, int type_id, AST *decl, SymbolFlags flags, AST *initial_value = nullptr) {
    this->name = name;
    this->type_id = type_id;
    this->flags = flags;
    this->initial_value = initial_value;
    this->declaration = decl;
  }
  ~Symbol() {}
};

struct Scope {
  bool empty() { return head == nullptr; }
  Symbol *head = nullptr;
  Symbol *lookup(const Interned_String &name);
  bool erase(const Interned_String &name);
  void insert(const Symbol &symbol);

  int create_interface_type(const Interned_String &name, const std::vector<int> &generic_args, AST *declaration,
                            Scope scope);
  int create_struct_type(const Interned_String &name, AST *declaration, Scope scope);
  void create_type_alias(const Interned_String &name, int type_id, AST *declaring_node);
  void forward_declare_type(const Interned_String &name, int default_id);
  int create_enum_type(const Interned_String &name, bool flags, AST *declaration, Scope scope);
  int create_tuple_type(const std::vector<int> &types);
  void insert_variable(const Interned_String &name, int type_id, AST *initial_value, AST *decl = nullptr);
  void insert_function(const Interned_String &name, const int type_id, AST *declaration,
                       SymbolFlags flags = SYMBOL_IS_FUNCTION);
  void insert_type(const int type_id, const Interned_String &name, AST *declaration);

  // get the count of non-function variables in this scope.
  inline int fields_count() const {
    auto field_ct = 0;
    for (auto sym = head; sym; sym = sym->next) {
      if (!sym->is_function() && sym->is_type())
        field_ct++;
    }
    return field_ct;
  }
  
  // TODO:
  // One problem with this way of doing scope, is that we're using an arena for symbols.
  // So, when we do this, we basically just leak everything that had been, and cannot ever reclaim
  // that memory. This is fine, because this is done very rarely, and can probably be negated in another way
  // however, it's something to think about.
  void clear() { head = nullptr; }

  class Iterator {
  public:
    Iterator(Symbol *symbol) : current(symbol) {}
    Symbol &operator*() { return *current; }
    Iterator &operator++() {
      current = current->next;
      return *this;
    }
    bool operator!=(const Iterator &other) const { return current != other.current; }

  private:
    Symbol *current;
  };
  Iterator begin() { return Iterator(head); }
  Iterator end() { return Iterator(nullptr); }
};

static std::unordered_set<Interned_String> &defines() {
  static std::unordered_set<Interned_String> defines;
  return defines;
};
static bool add_def(const Interned_String &define) { return defines().insert(define).second; }

static bool has_def(const Interned_String &define) {
  if (defines().contains(define)) {
    return true;
  }
  return false;
}
static void undef(const Interned_String &define) { defines().erase(define); }
