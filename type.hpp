
#pragma once

#include "core.hpp"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>

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
};

// TODO: we need a way to declare and use function types from in language.
// if we want to have first class functions and function pointers.

#ifndef MAX_NUM_TYPES
#define MAX_NUM_TYPES 1000
#endif

struct Type;

extern Type *type_table[MAX_NUM_TYPES];
extern int num_types;
extern jstl::Arena type_arena;

static Type *get_type(const int id) {
  [[unlikely]] if (id < 0)
    return nullptr;
  return type_table[id];
}

enum TypeExtEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
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
        if (ext == TYPE_EXT_POINTER) return true;
      }
      return false;
    }
    
    bool eq = false;
    int n = 0;
    for (const auto &ext: extensions) {
      if (n >= depth || ext != TYPE_EXT_POINTER) {
        return false;
      }
      n++;
    }
    return n == depth;
  }
  inline bool operator==(const TypeExt &other) const {
    return equals(other);
  }
  bool equals(const TypeExt &other) const;
  inline bool has_no_extensions() const {
    return extensions.size() == 0 && array_sizes.size() == 0;
  }
  
  inline bool is_array() const {
    return !array_sizes.empty();
  }

  inline bool is_fixed_sized_array() const {
    for (const auto &ext: array_sizes) {
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

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() {
    memset(parameter_types, -1, 256);
  }
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

struct ASTDeclaration;
struct Scope;

enum StructTypeFlags {
  STRUCT_FLAG_FORWARD_DECLARED = 1 << 0,
};

struct StructTypeInfo : TypeInfo {
  int flags;
  Scope *scope;
  virtual std::string to_string() const override { return ""; }
  mutable int m_size = -1;
};

struct Context;

struct Type {
  const int id = invalid_id;
  
  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;
  std::string base;
  TypeInfo *info;
  TypeExt extensions;
  
  bool equals(const std::string &name,
              const TypeExt &type_extensions) const;
  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;
  
  std::vector<std::string> aliases {};
  
  inline bool names_match_or_alias(const std::string &name) const {
    return has_alias(name) || this->base == name;
  }
  
  inline bool has_alias(const std::string &in_alias) const {
    for (const auto &alias: aliases) {
      if (alias == in_alias) return true;
    }
    return false;
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

template <class T> T *type_alloc(size_t n = 1) {
  auto mem = type_arena.allocate(sizeof(T) * n);
  return new (mem) T();
}

int create_type(TypeKind kind, const std::string &name,
                TypeInfo *info = nullptr,
                const TypeExt &extensions = {});

int create_struct_type(const std::string &name,
                       Scope *scope);

int create_enum_type(const std::string &name,
                     const std::vector<std::string> &keys,
                     bool is_keys = false);

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

ConversionRule type_conversion_rule(const Type *from, const Type *to);

int voidptr_type();
int bool_type();
int void_type();
int s8_type();
int s16_type();
int s32_type();
int s64_type();
int f32_type();

// char *
int string_type();

int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExt &ext);

int find_type_id(const std::string &name,
                 const TypeExt &type_extensions);

std::string get_cpp_scalar_type(int);
                 
struct Token;
int remove_one_pointer_ext(int operand_ty,
                           const SourceRange &source_range);

void init_type_system();
