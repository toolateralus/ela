
#pragma once

#include "core.hpp"
#include "lex.hpp"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <vector>

#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>


// TODO: we need a way to declare and use function types from in language.
// if we want to have first class functions and function pointers.

#ifndef MAX_NUM_TYPES
#define MAX_NUM_TYPES 1000
#endif

// fwd
struct Type;
struct ASTDeclaration;
struct Scope;
struct Context;

extern Type *type_table[MAX_NUM_TYPES];
extern int num_types;
extern jstl::Arena type_arena;

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

Token get_anonymous_struct_name();

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
};

enum TypeExtEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
};

enum FunctionInstanceFlags {
  FUNCTION_NORMAL = 0,
  FUNCTION_IS_TEST = 1 << 1,
  FUNCTION_IS_METHOD = 1 << 2,
  FUNCTION_IS_CTOR = 1 << 3,
  FUNCTION_IS_DTOR = 1 << 4,
  FUNCTION_IS_VARARGS = 1 << 5,
};

enum struct FunctionMetaType {
  FUNCTION_TYPE_NORMAL,
  FUNCTION_TYPE_FOREIGN,
};

enum UnionKind {
  UNION_IS_NORMAL = 0,
  UNION_IS_SUM_TYPE = 1,
};

// TODO: make it so array types are not scalars. It makes no darn sense.
struct TypeExt {
  // this stores things like * and [], [20] etc.
  std::vector<TypeExtEnum> extensions{};
  // for each type extension that is [], -1 == dynamic array, [n > 0] == fixed
  // array size.
  std::vector<int> array_sizes{};
  inline bool is_pointer(int depth = -1) const {
    if (depth == -1) {
      for (const auto ext : extensions) {
        if (ext == TYPE_EXT_POINTER)
          return true;
      }
      return false;
    }

    bool eq = false;
    int n = 0;
    for (const auto &ext : extensions) {
      if (n >= depth || ext != TYPE_EXT_POINTER) {
        return false;
      }
      n++;
    }
    return n == depth;
  }
  inline bool operator==(const TypeExt &other) const { return equals(other); }
  bool equals(const TypeExt &other) const;
  inline bool has_no_extensions() const {
    return extensions.size() == 0 && array_sizes.size() == 0;
  }

  inline bool is_array() const { return !array_sizes.empty(); }

  inline bool is_fixed_sized_array() const {
    for (const auto &ext : array_sizes) {
      if (ext != -1) {
        return true;
      }
    }
    return false;
  }

  inline std::string to_string() const {
    std::stringstream ss;
    std::vector<int> array_sizes = this->array_sizes;
    for (const auto ext : extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        auto size = array_sizes.back();
        array_sizes.pop_back();
        if (size == -1)
          ss << "[]";
        else {
          ss << "[" << size << "]";
        }
      }
      if (ext == TYPE_EXT_POINTER) {
        ss << "*";
      }
    }
    return ss.str();
  }
  inline std::string to_cpp_string(const std::string &base) const {
    std::vector<int> array_sizes = this->array_sizes;
    std::stringstream ss;
    ss << base;

    for (const auto ext : extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        auto size = array_sizes.back();
        array_sizes.pop_back();
        if (size == -1) {
          std::string current = ss.str();
          ss.str("_array<" + current + ">");
        } else {
          ss << "[" << size << "]";
        }
      }
      if (ext == TYPE_EXT_POINTER) {
        ss << "*";
      }
    }
    return ss.str();
  }
};

struct TypeInfo {
  virtual ~TypeInfo() = default;
  virtual std::string to_string() const { return "Abstract TypeInfo base."; }
};

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() { memset(parameter_types, -1, 256); }
  FunctionMetaType meta_type = FunctionMetaType::FUNCTION_TYPE_NORMAL;
  int return_type = -1;
  int parameter_types[256]; // max no of params in c++.
  int params_len = 0;
  int default_params = 0; // number of default params, always trailing.
  bool is_varargs = false;

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
  UnionKind kind;
  Scope *scope;
};

struct StructTypeInfo : TypeInfo {
  int flags;
  Scope *scope;
  
  std::vector<int> implicit_cast_table;
  std::vector<int> explicit_cast_table;
  
  virtual std::string to_string() const override { return ""; }
};

// active aliases. alias -> pointed to type_id
extern std::unordered_map<std::string, int> type_aliases;

struct Type {
  const int id = invalid_id;

  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;
  std::string base;
  TypeInfo *info;
  TypeExt extensions;

  bool equals(const std::string &name, const TypeExt &type_extensions) const;

  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;

  inline bool names_match_or_alias(const std::string &name) const {
    return has_alias(name) || this->base == name;
  }

  inline bool has_alias(const std::string &in_alias) const {
    return type_aliases.contains(in_alias) && type_aliases[in_alias] == id;
  }

  Type(){};
  Type(const int id, const TypeKind kind) : id(id), kind(kind) {}
  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;
  Type(Type &&) = delete;
  Type &operator=(Type &&) = delete;
  bool operator==(const Type &type) const;
  bool is_kind(const TypeKind kind) const { return this->kind == kind; }
  std::string to_string() const;
  std::string to_cpp_string() const;
  std::string to_type_struct(Context &context);

  // returns -1 for non-arrays. use 'remove_one_pointer_depth' for pointers.
  int get_element_type() const;
  constexpr static int invalid_id = -1;
};

struct ASTFunctionDeclaration;
std::string get_function_type_name(ASTFunctionDeclaration *);

template <class T> T *type_alloc(size_t n = 1) {
  auto mem = type_arena.allocate(sizeof(T) * n);
  return new (mem) T();
}

static Type *get_type(const int id) {
  [[unlikely]] if (id < 0)
    return nullptr;
  return type_table[id];
}

int create_type(TypeKind, const std::string &, TypeInfo * = nullptr,
                const TypeExt & = {});

int create_struct_type(const std::string &, Scope *);

int create_enum_type(const std::string &, const std::vector<std::string> &,
                     bool = false);

int create_union_type(const std::string &name, Scope *scope, UnionKind kind);

ConversionRule type_conversion_rule(const Type *, const Type *);

int voidptr_type();
int bool_type();
int void_type();
int s8_type();
int s16_type();
int s32_type();
int s64_type();
int float32_type();

// char *
int charptr_type();

int find_type_id(const std::string &, const FunctionTypeInfo &,
                 const TypeExt &);

int find_type_id(const std::string &, const TypeExt &);

std::string get_cpp_scalar_type(int);

struct Token;
int remove_one_pointer_ext(int, const SourceRange &);

void init_type_system();
