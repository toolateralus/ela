
#pragma once

#include "nullable.hpp"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>

enum TypeKind {
  TYPE_SCALAR,
  TYPE_FUNCTION,
  TYPE_STRUCT,
};

// TODO: probably use a better way to do this, and not a fix number of max
// types. However for now, this is reasonable, and if we need more that can be
// passed in to the compiler with a -D flag or something

#ifndef MAX_NUM_TYPES
#define MAX_NUM_TYPES 1000
#endif

struct Type;

// static storage is zero init I guess? so these are all nullptr

extern Type *type_table[MAX_NUM_TYPES];

static Type *get_type(const int id) {
  [[unlikely]] if (id < 0)
    return nullptr;

  return type_table[id];
}

extern int num_types;
extern jstl::Arena type_arena;

enum TypeExtensionEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
};

struct TypeExtensionInfo {
  // this stores things like * and [], [20] etc.
  jstl::Vector<TypeExtensionEnum> extensions{};
  // for each type extension that is [], -1 == dynamic array, [n > 0] == fixed
  // array size.
  jstl::Vector<int> array_sizes{};

  inline bool is_pointer(int depth) const {
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

  inline bool operator==(const TypeExtensionInfo &other) const {
    return equals(other);
  }

  bool equals(const TypeExtensionInfo &other) const;

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

enum FunctionFlags {
  FUNCTION_NORMAL,
  FUNCTION_TEST,
  FUNCTION_FOREIGN,
};

struct FunctionTypeInfo : TypeInfo {
  int flags = FUNCTION_NORMAL;
  int return_type = -1;
  int parameter_types[256]; // max no of params in c++.
  int params_len = 0;
  int default_params = 0; // number of default params, always trailing.
  
  // if this is set to true, on emit we will add a ..., and not check for too many arguments to calling this.
  bool is_varargs = false;
  // defined in cpp file
  virtual std::string to_string() const override;
};
struct ScalarTypeInfo : TypeInfo {
  virtual std::string to_string() const override { return ""; }
};
struct StructTypeInfo : TypeInfo {
  virtual std::string to_string() const override { return ""; }
};

struct Type {
  const int id = -1;
  const TypeKind kind = TypeKind::TYPE_SCALAR;
  std::string base;
  Nullable<TypeInfo> info = nullptr;
  TypeExtensionInfo extensions;
  bool equals(const std::string &name,
              const TypeExtensionInfo &type_extensions) const;
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
                const TypeExtensionInfo &extensions = {});

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

ConversionRule type_conversion_rule(const Type *from, const Type *to);

int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExtensionInfo &ext);

int find_type_id(const std::string &name,
                 const TypeExtensionInfo &type_extensions);

std::string get_cpp_scalar_type(int);
                 
                 
#include <vector>

struct Token;
int remove_one_pointer_ext(int operand_ty,
                           const std::vector<Token> &source_tokens);

void init_type_system();
