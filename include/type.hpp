
#pragma once

#include <cstddef>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <vector>

#include "arena.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"

// fwd
struct Type;
struct ASTDeclaration;
struct Scope;
struct Context;

extern std::vector<Type> type_table;
extern jstl::Arena type_info_arena;

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
  TYPE_EXT_INVALID,
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
  TYPE_EXT_FIXED_ARRAY,
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
  FUNCTION_IS_EXPORTED = 1 << 7,
  FUNCTION_IS_MUTATING = 1 << 9,
  FUNCTION_IS_FORWARD_DECLARED = 1 << 10,
  FUNCTION_IS_STATIC = 1 << 11,
};

enum struct FunctionMetaType {
  FUNCTION_TYPE_NORMAL,
  FUNCTION_TYPE_FOREIGN,
};

enum UnionFlags {
  // this and UNION_IS_SUM_TYPE should never be present at the same itme.
  UNION_IS_NORMAL = 1 << 0,
  UNION_IS_SUM_TYPE = 1 << 1,
  UNION_IS_FORWARD_DECLARED = 1 << 2,
};

struct ASTExpr;

struct TypeExtension {
  TypeExtEnum type;
  union {
    size_t array_size;
    int key_type;
  };
  bool operator==(const TypeExtension &other) const {
    if (type != other.type)
      return false;
    if (type == TYPE_EXT_FIXED_ARRAY) {
      return array_size == other.array_size;
    }
    if (type == TYPE_EXT_MAP) {
      return key_type == other.key_type;
    }
    return true;
  }
};
struct TypeExtensions {
  // this stores things like * and [], [20] etc.
  // for each type extension that is [], -1 == dynamic array, every other value is fixed array size.
  std::vector<TypeExtension> extensions{};

  TypeExtEnum back_type() const {
    if (extensions.empty()) {
      return TYPE_EXT_INVALID;
    } else {
      return extensions.back().type;
    }
  }

  inline bool is_array() const { return back_type() == TYPE_EXT_ARRAY; }

  inline bool is_fixed_sized_array() const { return back_type() == TYPE_EXT_FIXED_ARRAY; }

  inline bool is_map() const { return back_type() == TYPE_EXT_MAP; }

  inline bool is_pointer() const { return back_type() == TYPE_EXT_POINTER; }

  inline bool operator==(const TypeExtensions &other) const { return equals(other); }

  bool equals(const TypeExtensions &other) const;

  inline bool has_no_extensions() const { return extensions.empty(); }

  inline bool has_extensions() const { return !has_no_extensions(); }

  inline TypeExtensions append(const TypeExtensions &to_append) const {
    auto these = *this;
    these.extensions.insert(these.extensions.end(), to_append.extensions.begin(), to_append.extensions.end());
    return these;
  }

  inline TypeExtensions without_back() const {
    TypeExtensions these = *this;
    these.extensions.pop_back();
    return these;
  }

  std::string to_string() const;
};

using GenericParameter = InternedString;

struct TypeInfo {
  std::vector<int> implicit_cast_table;
  std::vector<int> explicit_cast_table;

  TypeInfo() {}

  // Use this instead of the clunky static casts everywhere.
  template <class T>
    requires std::derived_from<T, TypeInfo>
  inline T *as() {
    return static_cast<T *>(this);
  }

  virtual ~TypeInfo() = default;
  virtual std::string to_string() const { return "Abstract TypeInfo base."; }
};

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() { memset(parameter_types, -1, 256 * sizeof(int)); }
  FunctionMetaType meta_type = FunctionMetaType::FUNCTION_TYPE_NORMAL;
  int return_type = -1;
  int parameter_types[256]; // max no of params in c++.
  int params_len = 0;
  int default_params = 0; // number of default params, always trailing.
  bool is_varargs = false;
  // defined in cpp file
  virtual std::string to_string() const override;
  std::string to_string(const TypeExtensions &ext) const;
};

struct ScalarTypeInfo : TypeInfo {
  ScalarTypeInfo() {}
  bool is_integral = false;
  size_t size = 0;
  ScalarType scalar_type;
  virtual std::string to_string() const override { return ""; }
};

struct EnumTypeInfo : TypeInfo {
  int element_type = 0;
  bool is_flags = false;
  std::vector<InternedString> keys;
  EnumTypeInfo() {};
};

struct UnionTypeInfo : TypeInfo {
  int flags;
  Scope *scope;
  UnionTypeInfo() {}
};

struct StructTypeInfo : TypeInfo {
  int flags;
  Scope *scope;

  virtual std::string to_string() const override { return ""; }

  StructTypeInfo() {}
};

struct TupleTypeInfo : TypeInfo {
  std::vector<int> types;
};

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

// These 3 type getters are assigned by the Context constructor.
int &c_string_type();
int &string_type();
int &range_type();

Type *global_get_type(const int id);
InternedString get_tuple_type_name(const std::vector<int> &types);
int global_create_type(TypeKind, const InternedString &, TypeInfo * = nullptr, const TypeExtensions & = {},
                       const int = -1);
int global_create_struct_type(const InternedString &, Scope *);
int global_create_enum_type(const InternedString &, const std::vector<InternedString> &, bool = false,
                            size_t element_type = s32_type());
int global_create_tuple_type(const std::vector<int> &types, const TypeExtensions &ext);
int global_create_union_type(const InternedString &name, Scope *scope, UnionFlags kind);
ConversionRule type_conversion_rule(const Type *, const Type *, const SourceRange & = {});
// char *
int charptr_type();
int global_find_function_type_id(const FunctionTypeInfo &, const TypeExtensions &);
int global_find_type_id(std::vector<int> &tuple_types, const TypeExtensions &type_extensions);
int global_find_type_id(const int, const TypeExtensions &);
struct Token;
void init_type_system();
bool type_is_numerical(const Type *t);
constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to);
// returns false for failure, else true and passed param signature as out.
bool get_function_type_parameter_signature(Type *type, std::vector<int> &out);
void emit_warnings_or_errors_for_operator_overloads(const TType type, SourceRange &range);

struct Type {
  int id = invalid_id;
  int base_id = invalid_id;
  // if this is an alias or something just get the actual real true type.
  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;

  inline void set_base(const InternedString &base) { this->base = base; }
  inline void set_ext(const TypeExtensions &ext) { this->extensions = ext; }
  inline void set_info(TypeInfo *info) { this->info = info; }
  inline InternedString const get_base() const { return base; }
  TypeExtensions const get_ext() const { return extensions; }
  TypeExtensions const get_ext_no_compound() const { return extensions; }

  TypeInfo *get_info() const { return info; }

private:
  TypeInfo *info;
  InternedString base{};
  TypeExtensions extensions{};

public:
  bool equals(const int base, const TypeExtensions &type_extensions) const;
  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;

  Type() = default;
  Type(const int id, const TypeKind kind) : id(id), kind(kind) {}
  bool is_kind(const TypeKind kind) const { return this->kind == kind; }
  std::string to_string() const;

  // returns -1 for non-arrays. use 'remove_one_pointer_depth' for pointers.
  int get_element_type() const;
  int take_pointer_to() const;

  constexpr static int invalid_id = -1;
};

struct ASTFunctionDeclaration;
InternedString get_function_typename(ASTFunctionDeclaration *);
template <class T> static inline T *type_info_alloc() { return new (type_info_arena.allocate(sizeof(T))) T(); }
