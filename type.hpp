
#pragma once

#include "error.hpp"
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <jstl/containers/vector.hpp>
#include <jstl/memory/arena.hpp>

enum TypeFlags {
  TYPE_FLAGS_NONE = 0 << 0,
  TYPE_FLAGS_IS_POINTER = 1 << 0,
  TYPE_FLAGS_IS_ARRAY = 1 << 1,
};

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
struct TypeInfo;

// static storage is zero init I guess? so these are all nullptr

static Type *type_table[MAX_NUM_TYPES]{};
static TypeInfo *type_info_table[MAX_NUM_TYPES]{};

static int num_types = 0;

struct TypeInfo {
  // TODO: add more flags as needed.
  // TODO: perhaps we'll even inherit from this for more complex type info.
  // functions, structs, etc.
  int owner_id;
  std::string name;
  size_t size;

  // int ** has a ptr depth of 2;
  const int ptr_depth = 0;

  // the length of this vector is the amount of array modifiers this has,
  // and the integer value (if > 0) is the fixed length.
  // int v[10][20]; would make this = { 10, 20 };
  // int *v = new int[100]; would make this { -1 } indicating a dynamic array
  const jstl::Vector<int> array_dims = {};

  inline bool equals(const std::string &name, int ptr_depth,
              jstl::Vector<int> &array_dims) const {
    if (name != this->name) {
      return false;
    }
    if (ptr_depth != this->ptr_depth) {
      return false;
    }
    if (array_dims.size() != this->array_dims.size()) {
      return false;
    }
    for (int i = 0; i < array_dims.size(); ++i) {
      if (array_dims[i] != this->array_dims[i]) {
        return false;
      }
    }
    return true;
  }
};

struct Type {
  const int id;
  const int flags;
  const int kind;

  Type(const int id, const int kind, const int flags)
      : id(id), kind(kind), flags(flags) {}

  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;
  Type(Type &&) = delete;
  Type &operator=(Type &&) = delete;

  bool has_flag(const TypeFlags flag) const { return (flags & flag) != 0; }
  bool is_kind(const TypeKind kind) const { return this->kind == kind; }
  constexpr static int invalid_id = -1;
};

static jstl::Arena type_arena{(sizeof(Type) * MAX_NUM_TYPES) +
                              (sizeof(TypeInfo) * MAX_NUM_TYPES)};

template <class T> T *type_alloc(size_t n = 1) {
  return (T *)type_arena.allocate(sizeof(T) * n);
}

static int create_type(TypeKind kind, TypeFlags flags, TypeInfo &&info) {
  Type *type = new (type_alloc<Type>()) Type(num_types++, kind, flags);
  info.owner_id = type->id;

  if (type->id > MAX_NUM_TYPES) {
    throw_error(Error{
        .message = "Max types exceeded",
    });
  }
  if (type_info_table[type->id] || type_table[type->id]) {
    printf("type system created a type with the same ID twice\n");
    exit(1);
  }
  type_info_table[type->id] = new (type_alloc<TypeInfo>()) TypeInfo(info);
  type_table[type->id] = type;
  return type->id;
}

static Type *get_type(int id) { return type_table[id]; }

static TypeInfo *get_type_info(int id) { return type_info_table[id]; }

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

static ConversionRule type_conversion_rule(const Type *from, const Type *to) {
  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  // TODO: have more thorough type conversion rules defined in the typeinfo or
  // something.
  if (from->is_kind(TYPE_SCALAR) && to->is_kind(TYPE_SCALAR))
    return CONVERT_EXPLICIT;

  return CONVERT_PROHIBITED;
}

// Returns -1 if not found.
static int find_type_id(const std::string &name, int ptr_depth = 0,
                        jstl::Vector<int> array_dims = {}) {
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_info_table[i];
    if (tinfo->equals(name, ptr_depth, array_dims))
      return tinfo->owner_id; // this should always equal i but whatever just return the actual value.
  }
  return -1;
}

static void init_type_system() {

  // Signed integers
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "i64", .size = sizeof(int64_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "i32", .size = sizeof(int32_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "i16", .size = sizeof(int16_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "i8", .size = sizeof(int8_t)});
  }

  // Unsigned integers
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "u64", .size = sizeof(uint64_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "u32", .size = sizeof(uint32_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "u16", .size = sizeof(uint16_t)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "u8", .size = sizeof(uint8_t)});
  }

  // Floats
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "f32", .size = sizeof(float)});
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
                TypeInfo{.name = "f64", .size = sizeof(double)});
  }

  // Other
  create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
              TypeInfo{.name = "string", .size = sizeof(const char *)});
  create_type(TYPE_SCALAR, TYPE_FLAGS_NONE,
              TypeInfo{.name = "bool", .size = sizeof(bool)});
}

// used as a marker for the type visitor that we need to resolve this type at
// visit time.
static int get_type_unresolved() { return Type::invalid_id; }