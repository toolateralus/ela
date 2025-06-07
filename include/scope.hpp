#pragma once

#include <unordered_map>
#include <unordered_set>

#include "arena.hpp"

#include "interned_string.hpp"
#include "type.hpp"

extern jstl::Arena scope_arena;

struct Value {
  enum {
    INTEGER,
    FLOATING,
    BOOLEAN,
  } tag;

  struct {
    int integer;
    float floating;
    bool boolean;
  };

  bool is_truthy() {
    switch (tag) {
      case INTEGER:
        return integer;
      case FLOATING:
        return floating;
      case BOOLEAN:
        return boolean;
        break;
    }
  }

  static Value Int(const InternedString &str) {
    Value val;
    val.tag = INTEGER;
    val.integer = std::stoll(str.get_str());
    return val;
  }

  static Value Float(const InternedString &str) {
    Value val;
    val.tag = FLOATING;
    val.floating = std::stod(str.get_str());
    return val;
  }

  static Value Bool(const InternedString &str) {
    Value val;
    val.tag = BOOLEAN;
    val.boolean = (str.get_str() == "true");
    return val;
  }

  Value operator-() const {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = -integer};
    } else if (tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = -floating};
    }
    throw_error("Invalid type for unary minus", {});
    return {};
  }

  Value operator!() const {
    if (tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = !boolean};
    }
    throw_error("Invalid type for logical not", {});
    return {};
  }

  Value operator~() const {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = ~integer};
    }
    throw_error("Invalid type for bitwise not", {});
    return {};
  }

  Value operator+(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer + other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) +
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for addition", {});
    return {};
  }

  Value operator-(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer - other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) -
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for subtraction", {});
    return {};
  }

  Value operator*(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer * other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) *
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for multiplication", {});
    return {};
  }

  Value operator/(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer / other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) /
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for division", {});
    return {};
  }

  Value operator%(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer % other.integer};
    }
    throw_error("Invalid types for modulo", {});
    return {};
  }

  Value operator|(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer | other.integer};
    }
    throw_error("Invalid types for bitwise or", {});
    return {};
  }

  Value operator&(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer & other.integer};
    }
    throw_error("Invalid types for bitwise and", {});
    return {};
  }

  Value operator<<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer << other.integer};
    }
    throw_error("Invalid types for shift left", {});
    return {};
  }

  Value operator>>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer >> other.integer};
    }
    throw_error("Invalid types for shift right", {});
    return {};
  }

  Value operator^(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer ^ other.integer};
    }
    throw_error("Invalid types for bitwise xor", {});
    return {};
  }

  Value operator||(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean || other.boolean};
    }
    throw_error("Invalid types for logical or", {});
    return {};
  }

  Value operator&&(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean && other.boolean};
    }
    throw_error("Invalid types for logical and", {});
    return {};
  }

  Value operator<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer < other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{
          .tag = BOOLEAN,
          .boolean = (tag == FLOATING ? floating : integer) < (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for less than comparison", {});
    return {};
  }

  Value operator>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer > other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{
          .tag = BOOLEAN,
          .boolean = (tag == FLOATING ? floating : integer) > (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for greater than comparison", {});
    return {};
  }

  Value operator==(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer == other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) ==
                              (other.tag == FLOATING ? other.floating : other.integer)};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean == other.boolean};
    }
    throw_error("Invalid types for equality comparison", {});
    return {};
  }

  Value operator!=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer != other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) !=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean != other.boolean};
    }
    throw_error("Invalid types for inequality comparison", {});
    return {};
  }

  Value operator<=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer <= other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) <=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for less than or equal comparison", {});
    return {};
  }

  Value operator>=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer >= other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) >=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for greater than or equal comparison", {});
    return {};
  }
};

enum SymbolFlags {
  SYMBOL_IS_VARIABLE = 1 << 0,
  SYMBOL_IS_FUNCTION = 1 << 1,
  SYMBOL_IS_FORWARD_DECLARED = 1 << 2,
  SYMBOL_IS_TYPE = 1 << 3,
  SYMBOL_IS_MODULE = 1 << 4,
  SYMBOL_IS_LOCAL = 1 << 5,
};

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

struct Symbol {
  InternedString name;
  Type *resolved_type = Type::INVALID_TYPE;
  int flags = SYMBOL_IS_VARIABLE;

  Mutability mutability = CONST;
  Scope *scope;

  bool is_function() const { return HAS_FLAG(flags, SYMBOL_IS_FUNCTION); }
  bool is_variable() const { return HAS_FLAG(flags, SYMBOL_IS_VARIABLE); }
  bool is_type() const { return HAS_FLAG(flags, SYMBOL_IS_TYPE); }
  bool is_module() const { return HAS_FLAG(flags, SYMBOL_IS_MODULE); }
  bool is_forward_declared() const { return HAS_FLAG(flags, SYMBOL_IS_FORWARD_DECLARED); }
  bool is_local() const { return HAS_FLAG(flags, SYMBOL_IS_LOCAL); }

  bool is_const() const { return mutability == CONST; }
  bool is_mut() const { return mutability == MUT; }

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
      // ? Why is this here? Just to confirm the type is a child of a choice type?
      Nullable<ASTChoiceDeclaration> choice;
      // This is nullable purely because `tuple` types do not have a declaring node!
      // Otherwise, all other nodes have this property, and must.
      Nullable<ASTNode> declaration;
      TypeKind kind;
    } type;
  };

  Symbol() {}
  ~Symbol() {}

  static Symbol create_variable(const InternedString &name, Type *type, ASTExpr *initial_value, ASTNode *decl,
                                Mutability mutability) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = type;
    symbol.flags = SYMBOL_IS_VARIABLE;
    symbol.variable.initial_value = initial_value;
    symbol.variable.declaration = decl;
    symbol.mutability = mutability;
    return symbol;
  }

  static Symbol create_function(const InternedString &name, Type *type, ASTFunctionDeclaration *declaration,
                                SymbolFlags flags) {
    Symbol symbol;
    symbol.resolved_type = type;
    symbol.name = name;
    symbol.flags = flags;
    symbol.function.declaration = declaration;
    return symbol;
  }

  static Symbol create_type(Type *type, const InternedString &name, TypeKind kind, ASTNode *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.kind = kind;
    symbol.type.declaration = declaration;
    symbol.resolved_type = type;
    return symbol;
  }

  static Symbol create_module(const InternedString &name, ASTModule *declaration) {
    Symbol symbol;
    symbol.name = name;
    symbol.flags = SYMBOL_IS_MODULE;
    symbol.module.declaration = declaration;
    return symbol;
  }

  bool is_generic_function() const;
};

struct Typer;
struct ASTFunctionDeclaration;
struct ASTTraitDeclaration;

struct Scope {
  std::unordered_map<InternedString, Symbol> symbols = {};
  InternedString name = "";

  std::string full_name() {
    if (parent) {
      auto parent_name = parent->full_name();
      if (!parent_name.empty()) {
        return parent->full_name() + "$" + name.get_str();
      }
    }
    return name.get_str();
  }

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
  inline size_t fields_count() const {
    auto fields = 0;
    for (const auto &[name, sym] : symbols) {
      if (!sym.is_function() && !sym.is_type()) fields++;
    }
    return fields;
  }

  void insert_local_variable(const InternedString &name, Type *type_id, ASTExpr *initial_value, Mutability mutability,
                             ASTNode *decl = nullptr) {
    auto sym = Symbol::create_variable(name, type_id, initial_value, decl, mutability);
    sym.flags |= SYMBOL_IS_LOCAL;
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
  }

  void insert_variable(const InternedString &name, Type *type_id, ASTExpr *initial_value, Mutability mutability,
                       ASTNode *decl = nullptr) {
    auto sym = Symbol::create_variable(name, type_id, initial_value, decl, mutability);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
  }

  void insert_function(const InternedString &name, Type *type_id, ASTFunctionDeclaration *declaration,
                       SymbolFlags flags = SYMBOL_IS_FUNCTION) {
    auto sym = Symbol::create_function(name, type_id, declaration, flags);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
  }

  void insert_type(Type *type_id, const InternedString &name, TypeKind kind, ASTNode *declaration) {
    auto sym = Symbol::create_type(type_id, name, kind, declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
  }

  Symbol *lookup(const InternedString &name);

  Symbol *local_lookup(const InternedString &name);

  void erase(const InternedString &name);

  Type *create_tagged_union(const InternedString &name, Scope *scope, ASTChoiceDeclaration *declaration) {
    auto type = global_create_choice_type(name, scope, {});
    auto sym = Symbol::create_type(type, name, TYPE_CHOICE, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }

  Type *create_trait_type(const InternedString &name, Scope *scope, const std::vector<Type *> &generic_args,
                          ASTTraitDeclaration *declaration) {
    auto type = global_create_trait_type(name, scope, generic_args);
    auto sym = Symbol::create_type(type, name, TYPE_TRAIT, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }

  Type *create_struct_type(const InternedString &name, Scope *scope, ASTStructDeclaration *declaration) {
    auto type = global_create_struct_type(name, scope);
    auto sym = Symbol::create_type(type, name, TYPE_STRUCT, (ASTNode *)declaration);
    type->declaring_node.set((ASTNode *)declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);

    return type;
  }

  void create_type_alias(const InternedString &name, Type *type_id, TypeKind kind, ASTNode *declaring_node) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = type_id;
    symbol.type.kind = kind;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.type.declaration = declaring_node;
    symbols.erase(name);
    symbol.scope = this;
    symbols.insert_or_assign(name, symbol);
  }

  void forward_declare_type(const InternedString &name, Type *default_id) {
    Symbol symbol;
    symbol.name = name;
    symbol.resolved_type = default_id;
    symbol.flags = SYMBOL_IS_TYPE;
    symbol.scope = this;
    symbols.insert_or_assign(name, symbol);
  }

  Type *create_enum_type(const InternedString &name, Scope *scope, bool flags, ASTEnumDeclaration *declaration) {
    auto type = global_create_enum_type(name, scope, flags);
    auto sym = Symbol::create_type(type, name, TYPE_STRUCT, (ASTNode *)declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }

  void create_module(const InternedString &name, ASTModule *declaration) {
    auto sym = Symbol::create_module(name, declaration);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
  }

  Type *create_tuple_type(const std::vector<Type *> &types) {
    auto type = global_create_tuple_type(types);
    auto name = get_tuple_type_name(types);
    // Tuples don't have a declaration node, so we pass nullptr here. Something to be aware of!
    auto sym = Symbol::create_type(type, name, TYPE_STRUCT, nullptr);
    sym.scope = this;
    symbols.insert_or_assign(name, sym);
    return type;
  }

  Type *find_type_id(const InternedString &name, const TypeExtensions &ext) {
    auto symbol = lookup(name);
    if (!symbol || !symbol->is_type()) {
      if (parent) {
        return parent->find_type_id(name, ext);
      } else {
        return Type::INVALID_TYPE;
      }
    }
    return global_find_type_id(symbol->resolved_type, ext);
  }

  Type *find_or_create_dyn_type_of(Type *trait, SourceRange range, Typer *typer);
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
};
