
#pragma once

#include "nullable.hpp"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

struct TypeExt {
  // this stores things like * and [], [20] etc.
  jstl::Vector<TypeExtEnum> extensions{};
  // for each type extension that is [], -1 == dynamic array, [n > 0] == fixed
  // array size.
  jstl::Vector<int> array_sizes{};
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
  inline std::string to_string() const {
    std::stringstream ss;
    jstl::Vector<int> array_sizes = this->array_sizes;
    for (const auto ext : extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        auto size = array_sizes.pop();
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
    jstl::Vector<int> array_sizes = this->array_sizes;
    std::stringstream ss;
    ss << base;
    for (const auto ext : extensions) {
      if (ext == TYPE_EXT_ARRAY) {
        auto size = array_sizes.pop();
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

enum FunctionTypeFlags {
  FUNCTION_NORMAL = 1 << 0,
  FUNCTION_TEST = 1 << 1,
  FUNCTION_FOREIGN = 1 << 2,
};

struct FunctionTypeInfo : TypeInfo {
  int flags = FUNCTION_NORMAL;
  int return_type = -1;
  int parameter_types[256]; // max no of params in c++.
  int params_len = 0;
  int default_params = 0; // number of default params, always trailing.
  
  // TODO: add a way to declare varargs for externs only. Right now, we only use this for printf;
  // if this is set to true, on emit we will add a ..., and not check for too many arguments to calling this.
  bool is_varargs = false;
  
  // defined in cpp file
  virtual std::string to_string() const override;
};

// TODO(josh): add scalar casting info for each type here.
// TODO(cont.) builtins can just have a table of types they can implicitly explicitly and cant cast to.
struct ScalarTypeInfo : TypeInfo {
  bool is_integral = false;
  size_t size = 0;
  ScalarType scalar_type;
  virtual std::string to_string() const override { return ""; }
};

struct ASTDeclaration;
struct Scope;

struct StructTypeInfo : TypeInfo {
  Scope *scope;
  jstl::Vector<ASTDeclaration*> fields;
  virtual std::string to_string() const override { return ""; }
};

struct Type {
  const int id = -1;
  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;
  std::string base;
  Nullable<TypeInfo> info = nullptr;
  TypeExt extensions;
  bool equals(const std::string &name,
              const TypeExt &type_extensions) const;
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
  std::string to_cpp_string() const;

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
                       const jstl::Vector<ASTDeclaration*> &fields);

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
// u8 **
int string_type();

int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExt &ext);

int find_type_id(const std::string &name,
                 const TypeExt &type_extensions);

std::string get_cpp_scalar_type(int);
                 
struct Token;
int remove_one_pointer_ext(int operand_ty,
                           const std::vector<Token> &source_tokens);

void init_type_system();
