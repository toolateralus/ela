
#pragma once

#include "error.hpp"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <jstl/memory/arena.hpp>
#include <jstl/containers/vector.hpp>

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

// static storage is zero init I guess? so these are all nullptr

extern Type *type_table[MAX_NUM_TYPES];

extern int num_types;
extern jstl::Arena type_arena;


enum  TypeExtensionEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
};

struct TypeExtensions {
  // this stores things like * and [], [20] etc.
  jstl::Vector<TypeExtensionEnum> extensions{};
  // for each type extension that is [], -1 == dynamic array, [n > 0] == fixed array size.
  jstl::Vector<int> array_sizes{};
  
  inline bool operator ==(const TypeExtensions &other) const {
    return equals(other);
  }
  
  inline bool equals(const TypeExtensions &other) const {
    if (extensions.size() != other.extensions.size()) return false; 
    if (array_sizes.size() != other.array_sizes.size()) return false;
    for (int i = 0; i < extensions.size(); ++i) 
      if (extensions[i] != other.extensions[i]) 
        return false;
    for (int i = 0; i < array_sizes.size(); ++i) 
      if (array_sizes[i] != other.array_sizes[i]) return false;
    return true;
  }
  
  inline bool has_no_extensions() const {
    return extensions.size() == 0 && array_sizes.size() == 0;
  }
};


struct Type {
  const int id = -1;
  const int flags = -1;
  const int kind = -1;
  
  // nameof(T)
  std::string name;
  
  TypeExtensions type_extensions;

  inline bool equals(const std::string &name, const TypeExtensions &type_extensions) const {
    if (name != this->name)  return false;
    return type_extensions == this->type_extensions;
  }
  
  Type() {};
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

template <class T> T *type_alloc(size_t n = 1) {
  auto mem = type_arena.allocate(sizeof(T) * n);
  return new (mem) T();
}

static int create_type(TypeKind kind, TypeFlags flags, const std::string &name, const TypeExtensions &extensions = {}) {
  Type *type = new (type_alloc<Type>()) Type(num_types, kind, flags);

  type->type_extensions = extensions;
  
  type->name = name;

  if (type->id > MAX_NUM_TYPES) {
    throw_error({
        .message = "Max types exceeded",
    });
  }
  if (type_table[type->id]) {
    printf("type system created a type with the same ID twice\n");
    exit(1);
  }
  
  type_table[type->id] = type;
  num_types += 1;
  return type->id;
}

static Type *get_type(int id) { return type_table[id]; }

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
static int find_type_id(const std::string &name, const TypeExtensions &type_extensions) {
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->equals(name, type_extensions))
      return tinfo->id;
  }
  int base_id = -1;
  
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->name == name && tinfo->type_extensions.has_no_extensions()) {
      base_id = tinfo->id;
      break;
    }
  }
  if (base_id != -1) {
    auto t = get_type(base_id);
    return create_type((TypeKind)t->kind, (TypeFlags)t->flags, name, type_extensions);
  }
  return -1;
}
static void init_type_system() {
  // Signed integers
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "i64");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "i32");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "i16");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "i8");
  }

  // Unsigned integers
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "u64");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "u32");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "u16");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "u8");
  }

  // Floats
  {
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "f32");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "f64");
  }
  
  // Other
  {
    // Other
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "string");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "bool");
    create_type(TYPE_SCALAR, TYPE_FLAGS_NONE, "void");
  }
}

// used as a marker for the type visitor that we need to resolve this type at
// visit time.
static int get_type_unresolved() { return Type::invalid_id; }