
#pragma once

#include "core.hpp"
#include "lex.hpp"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <jstl/memory/arena.hpp>

#ifndef MAX_NUM_TYPES
#define MAX_NUM_TYPES 1000
#endif

// fwd
struct Type;
struct ASTDeclaration;
struct Scope;
struct Context;

extern Type **type_table;
extern int num_types;
extern jstl::Arena type_arena;

// used for emitting typedefs

extern std::unordered_map<std::string, int> type_alias_map;


enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

Token get_unique_identifier();

enum StructTypeFlags {
  STRUCT_FLAG_FORWARD_DECLARED = 1 << 0,
  STRUCT_FLAG_IS_ANONYMOUS = 1 << 1,
};

enum ScalarType {
  TYPE_VOID,
  TYPE_S8,
  TYPE_S16,
  TYPE_S32,
  TYPE_S64,
  TYPE_U8,
  TYPE_U16,
  TYPE_U32,
  TYPE_U64,
  TYPE_FLOAT,
  TYPE_DOUBLE,
  TYPE_STRING,
  TYPE_CHAR,
  TYPE_BOOL,
};

enum TypeKind {
  TYPE_SCALAR,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_ENUM,
  TYPE_UNION,
  TYPE_TUPLE,
};

enum TypeExtEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
  TYPE_EXT_MAP,
};

enum FunctionInstanceFlags {
  FUNCTION_NORMAL = 0,
  FUNCTION_IS_TEST = 1 << 1,
  FUNCTION_IS_METHOD = 1 << 2,
  FUNCTION_IS_CTOR = 1 << 3,
  FUNCTION_IS_DTOR = 1 << 4,
  FUNCTION_IS_VARARGS = 1 << 5,
  FUNCTION_IS_OPERATOR = 1 << 6,
  FUNCTION_IS_EXPORTED= 1 << 7,
  FUNCTION_IS_GENERIC = 1 << 8,
  FUNCTION_IS_MUTATING = 1 << 9,
  FUNCTION_IS_FORWARD_DECLARED = 1 << 10,
};

enum struct FunctionMetaType {
  FUNCTION_TYPE_NORMAL,
  FUNCTION_TYPE_FOREIGN,
};

enum UnionFlags {
  UNION_IS_NORMAL = 1 << 0, // this and the next flag should never be present at the same itme.
  UNION_IS_SUM_TYPE = 1 << 1,
  UNION_IS_FORWARD_DECLARED = 1 << 2,
};

struct ASTExpr;

struct TypeExt {
  // this stores things like * and [], [20] etc.
  std::vector<TypeExtEnum> extensions{};
  // for each type extension that is [], nullptr == dynamic array, [non-nullptr] == fixed array size.
  std::vector<Nullable<ASTExpr>> array_sizes{};
  
  // This is a key type of a map.
  // so int with a MAP extension and a key type of string,
  // would be like int[string];
  int key_type = -1;
  
  inline bool is_map() const {
    for (const auto ext: extensions) {
      if (ext == TYPE_EXT_MAP) {
        return true;
      }
    }
    return false;
  }
  
  inline bool is_pointer(int depth = -1) const {
    if (depth == -1) {
      return std::find(extensions.rbegin(), extensions.rend(), TYPE_EXT_POINTER) != extensions.rend();
    }
    if (depth > extensions.size()) {
      return false;
    }
    return std::all_of(extensions.rbegin(), extensions.rbegin() + depth, [](TypeExtEnum ext) {
      return ext == TYPE_EXT_POINTER;
    });
  }
  
  inline bool operator==(const TypeExt &other) const { return equals(other); }
  
  bool equals(const TypeExt &other) const;
  
  inline bool has_no_extensions() const {
    return extensions.empty();
  }
  
  inline bool has_extensions() const {
    return !has_no_extensions();
  }
  
  TypeExt append(const TypeExt& to_append) const {
    auto these = *this;
    int sizes_i = 0;
    
    for (auto ext : to_append.extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        these.array_sizes.push_back(to_append.array_sizes[sizes_i]);
        sizes_i++;
      }
      
      if (ext == TYPE_EXT_MAP) {
        these.key_type = to_append.key_type;
      }
      
      these.extensions.push_back(ext);
    }
    return these;
  }  
  TypeExt without_back() const {
    auto these = *this;
    if (these.extensions.back() == TYPE_EXT_ARRAY) {
      these.array_sizes.pop_back();
    }
    if (these.extensions.back() == TYPE_EXT_MAP) {
      these.key_type = -1;
    }
    
    these.extensions.pop_back();
    return these;
  }
  
  inline bool is_array() const { return !array_sizes.empty(); }

  bool is_fixed_sized_array() const;

  std::string to_string() const;
};

struct TypeInfo {
  virtual ~TypeInfo() = default;
  virtual std::string to_string() const { return "Abstract TypeInfo base."; }
};

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() { 
    memset(parameter_types, -1, 256 * sizeof(int));
  }
  FunctionMetaType meta_type = FunctionMetaType::FUNCTION_TYPE_NORMAL;
  int return_type = -1;
  int parameter_types[256]; // max no of params in c++.
  int params_len = 0;
  int default_params = 0; // number of default params, always trailing.
  bool is_varargs = false;

  bool is_generic = false;

  // defined in cpp file
  virtual std::string to_string() const override;
};

struct ScalarTypeInfo : TypeInfo {
  bool is_integral = false;
  size_t size = 0;
  ScalarType scalar_type;
  virtual std::string to_string() const override { return ""; }
};

struct EnumTypeInfo : TypeInfo {
  bool is_flags = false;
  std::vector<std::string> keys;
};

struct UnionTypeInfo : TypeInfo {
  int flags;
  Scope *scope;
};

struct StructTypeInfo : TypeInfo {
  int flags;
  Scope *scope;

  std::vector<int> implicit_cast_table;
  std::vector<int> explicit_cast_table;

  virtual std::string to_string() const override { return ""; }
};

struct TupleTypeInfo : TypeInfo {
  std::vector<int> types;
};

Type *global_get_type(const int id);

std::string get_tuple_type_name(const std::vector<int> &types);

int global_create_type(TypeKind, const std::string &, TypeInfo * = nullptr,
                const TypeExt & = {});

int global_create_struct_type(const std::string &, Scope *);

int global_create_enum_type(const std::string &, const std::vector<std::string> &,
                     bool = false);

int global_create_tuple_type(const std::vector<int> &types, const TypeExt& ext);

int global_create_union_type(const std::string &name, Scope *scope, UnionFlags kind);

ConversionRule type_conversion_rule(const Type *, const Type *);

// helpers to get scalar types for fast comparison
int voidptr_type();
int char_type();
int bool_type();
int void_type();
int s8_type();
int s16_type();
int s32_type();
int s64_type();
int int_type();
int u8_type();
int u16_type();
int u32_type();
int u64_type();
int float64_type();
int float_type();
int float32_type();

// char *
int charptr_type();

int global_find_function_type_id(const std::string &, const FunctionTypeInfo &,
                 const TypeExt &);

int global_find_type_id(std::vector<int> &tuple_types, const TypeExt &type_extensions);

int global_find_type_id(const std::string &, const TypeExt &);

struct Token;

int remove_one_pointer_ext(int, const SourceRange &);

void init_type_system();

bool type_is_numerical(const Type *t);
constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to);

// returns false for failure, else true and passed param signature as out.
bool get_function_type_parameter_signature(Type *type, std::vector<int> &out);

void emit_warnings_or_errors_for_operator_overloads(const TType type, SourceRange &range);

int get_pointer_to_type(int base);

int global_create_type_alias(int aliased_type, const std::string &name);

int get_map_value_type(Type *map_type);

struct Type {
  const int id = invalid_id;

  // if this is an alias or something just get the actual real true type.
  int get_true_type() const {
    auto base_no_ext = id;
    
    if (extensions.has_extensions()) {
      base_no_ext = global_find_type_id(get_base(), {});
    }
    
    auto base_type = global_get_type(base_no_ext);
    Type* type = (Type*)this;
    while (type && type->is_alias || base_type->is_alias) {
      base_type = global_get_type(base_type->alias_id);
      auto old_ext = base_type->get_ext().append(get_ext_no_compound());
      auto new_id = global_find_type_id(base_type->get_base(), old_ext);
      type = global_get_type(new_id);
    }
    return type->id;
  }

  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;

  // this type is aliasing another type
  bool is_alias = false;
  // this is the type that this type aliases.
  int alias_id;
  
  // this type has other types that refer to me as an alias.
  bool has_aliases = false;
  // these are the types that refer to me as an alias
  std::vector<int> aliases;
  
  bool has_alias(const std::string &name) const {
    if (!has_aliases) return false;
    for (const auto &alias: aliases) {
      auto type = global_get_type(alias);
      if (type->get_base() == name) {
        return true;
      }
    }
    return false;
  }

  void set_base(const std::string &base) {
    this->base = base;
  }
  void set_ext(const TypeExt &ext) {
    this->extensions = ext;
  }
  void set_info(TypeInfo *info) {
    this->info = info;
  }
  std::string const get_base() const {
    return base;
  }
  TypeExt const get_ext() const {
    if (is_alias) {
      auto type = global_get_type(alias_id);
      auto exts = type->extensions;
      return exts.append(extensions);
    }
    return extensions;
  }
  TypeExt const get_ext_no_compound() const {
    return extensions;
  }

  TypeInfo *get_info() const {
    return info;
  }

  

  private:
  std::string base;
  TypeExt extensions;
  TypeInfo *info;
  public:

  bool equals(const std::string &name, const TypeExt &type_extensions, std::unordered_set<const Type*> &visited) const;

  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;
  Type(){};

  Type(const int id, const TypeKind kind) : id(id), kind(kind) {}
  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;
  Type(Type &&) = delete;
  Type &operator=(Type &&) = delete;
  bool operator==(const Type &type) const;
  bool is_kind(const TypeKind kind) const { return this->kind == kind; }
  std::string to_string() const;
  

  // returns -1 for non-arrays. use 'remove_one_pointer_depth' for pointers.
  int get_element_type() const;
  constexpr static int invalid_id = -1;
};

struct ASTFunctionDeclaration;
std::string global_get_function_typename(ASTFunctionDeclaration *);

template <class T> T *type_alloc(size_t n = 1) {
  auto mem = type_arena.allocate(sizeof(T) * n);
  return new (mem) T();
}
