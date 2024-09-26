
#pragma once

#include "error.hpp"
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

static Type *get_type(int id) { return type_table[id]; }

extern int num_types;
extern jstl::Arena type_arena;

enum TypeExtensionEnum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
};

struct TypeExtensionInfo {
  TypeExtensionInfo() = default;
  // this stores things like * and [], [20] etc.
  jstl::Vector<TypeExtensionEnum> extensions{};
  // for each type extension that is [], -1 == dynamic array, [n > 0] == fixed
  // array size.
  jstl::Vector<int> array_sizes{};

  inline bool operator==(const TypeExtensionInfo &other) const {
    return equals(other);
  }

  inline bool equals(const TypeExtensionInfo &other) const {
    if (extensions.size() != other.extensions.size())
      return false;
    if (array_sizes.size() != other.array_sizes.size())
      return false;
    for (int i = 0; i < extensions.size(); ++i)
      if (extensions[i] != other.extensions[i])
        return false;
    for (int i = 0; i < array_sizes.size(); ++i)
      if (array_sizes[i] != other.array_sizes[i])
        return false;
    return true;
  }

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
};

struct TypeInfo {
  virtual ~TypeInfo() = default;
  virtual std::string to_string() const {
    return "Abstract TypeInfo base.";
  }
};
struct FunctionTypeInfo : TypeInfo {
  int return_type;
  int parameter_types[256]; // max no of params in c++.
  int params_len;
  // defined in cpp file
  virtual std::string to_string() const override;
};
struct ScalarTypeInfo : TypeInfo {
  virtual std::string to_string() const override {
    return "";
  }
};
struct StructTypeInfo : TypeInfo {
  virtual std::string to_string() const override {
    return ""; 
  }
};

struct Type {
  const int id = -1;
  const TypeKind kind = TypeKind::TYPE_SCALAR;

  // nameof(T)
  std::string name;

  TypeExtensionInfo extensions;

  inline bool equals(const std::string &name,
                     const TypeExtensionInfo &type_extensions) const {
    if (name != this->name)
      return false;
    return type_extensions == this->extensions;
  }
  
  Nullable<TypeInfo> info = nullptr;

  bool type_info_equals(const TypeInfo *info, TypeKind kind) const {
    if (!this->info && info) {
      return false;
    }
    if (this->kind != kind)
      return false;
    if (kind == TypeKind::TYPE_FUNCTION) {
      auto finfo = static_cast<const FunctionTypeInfo *>(info);
      auto sinfo = static_cast<const FunctionTypeInfo *>(this->info.get());
      bool params_eq = finfo->params_len == sinfo->params_len;
      
      if (!params_eq) return false;
      
      for (int i = 0; i < finfo->params_len; ++i) 
        if (finfo->parameter_types[i] != sinfo->parameter_types[i]) {
          params_eq = false;
          break;
        }
      return finfo->return_type == sinfo->return_type && params_eq;
    }
    return false;
  }

  Type(){};
  Type(const int id, const TypeKind kind)
      : id(id), kind(kind) {}

  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;
  Type(Type &&) = delete;
  Type &operator=(Type &&) = delete;

  bool operator==(const Type &type) const {
    for (int i = 0; i < num_types; ++i) {
      auto tinfo = type_table[i];
      
      if (tinfo->equals(name, extensions) && type.info.is_not_null() && type_info_equals(type.info.get(), type.kind))
        return true;
    }
    return false;
  }

  bool is_kind(const TypeKind kind) const { return this->kind == kind; }
  
  std::string to_string() const  {
    
    switch (kind) {
    case TYPE_SCALAR:
      return name + extensions.to_string();
    case TYPE_FUNCTION:
      if (info.is_not_null())
        return info.get()->to_string();
      else return "invalid function type";
    case TYPE_STRUCT:
      return "struct NYI";
      break;
    }
  }
  
  constexpr static int invalid_id = -1;
};

template <class T> T *type_alloc(size_t n = 1) {
  auto mem = type_arena.allocate(sizeof(T) * n);
  return new (mem) T();
}

static int create_type(TypeKind kind, const std::string &name,
                       TypeInfo *info = nullptr, const TypeExtensionInfo &extensions = {}) {
  Type *type = new (type_alloc<Type>()) Type(num_types, kind);
  type->info = info;
  type->extensions = extensions;

  type->name = name;

  if (type->id > MAX_NUM_TYPES) {
    throw_error({
      .message = "Max types exceeded",
    });
  }
  if (type_table[type->id]) {
    throw_error({
      .message = "type system created a type with the same ID twice",
      .severity = ERROR_CRITICAL
    });
  }

  type_table[type->id] = type;
  num_types += 1;
  return type->id;
}

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
  // if (from->is_kind(TYPE_SCALAR) && to->is_kind(TYPE_SCALAR))
  //   return CONVERT_EXPLICIT;

  return CONVERT_PROHIBITED;
}


static int find_type_id(const std::string &name, const FunctionTypeInfo &info, const TypeExtensionInfo &ext) {
  for (int i = 0; i < num_types; ++i) {
    if (type_table[i]->kind != TYPE_FUNCTION) 
      continue;
    const Type *type = type_table[i];
    if (name == type->name && 
        type->type_info_equals(&info, TYPE_FUNCTION) &&
         ext.equals(type->extensions)) {
      return type->id;
    }
  }
  auto info_ptr = new (type_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);
  return create_type(TYPE_FUNCTION, name, info_ptr, ext);
}

static int find_type_id(const std::string &name,
                        const TypeExtensionInfo &type_extensions) {

  for (int i = 0; i < num_types; ++i) {
    auto type = type_table[i];
    if (type_extensions.has_no_extensions()) {
      if (type->name == name && type->extensions.has_no_extensions()) {
        return type->id;
      }
    }
    if (type->equals(name, type_extensions))
      return type->id;
  }
  
  // BELOW IS JUST FOR CREATING TYPES WITH NEW EXTENSIONS. NEW FUNCTION TYPES MUST BE CREATED MANUALLY
  int base_id = -1;

  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->name == name && tinfo->extensions.has_no_extensions()) {
      base_id = tinfo->id;
      break;
    }
  }
  
  if (base_id != -1) {
    auto t = get_type(base_id);
    printf("creating type: %s\n", t->name.c_str());
    return create_type((TypeKind)t->kind, name,
                       nullptr, type_extensions);
  }
  
  return -1;
}
static void init_type_system() {
  // Signed integers
  {
    create_type(TYPE_SCALAR, "s64");
    create_type(TYPE_SCALAR, "s32");
    create_type(TYPE_SCALAR, "s16");
    create_type(TYPE_SCALAR, "s8");
  }

  // Unsigned integers
  {
    create_type(TYPE_SCALAR, "u64");
    create_type(TYPE_SCALAR, "u32");
    create_type(TYPE_SCALAR, "u16");
    create_type(TYPE_SCALAR, "u8");
  }

  // Floats
  {
    create_type(TYPE_SCALAR, "f32");
    create_type(TYPE_SCALAR, "f64");
  }

  // Other
  {
    // Other
    create_type(TYPE_SCALAR, "float");
    create_type(TYPE_SCALAR, "int");
    create_type(TYPE_SCALAR, "char");
    create_type(TYPE_SCALAR, "string");
    create_type(TYPE_SCALAR, "bool");
    create_type(TYPE_SCALAR, "void");
  }
}

// used as a marker for the type visitor that we need to resolve this type at
// visit time.
static int get_type_unresolved() { return Type::invalid_id; }