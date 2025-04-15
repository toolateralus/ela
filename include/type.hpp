
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
struct ASTVariable;
struct Scope;
struct Context;

extern std::vector<Type *> type_table;
extern jstl::Arena type_info_arena;

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

struct Token;
Token get_unique_identifier();

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
  TYPE_TUPLE,
  TYPE_CHOICE,
  TYPE_INTERFACE,
  TYPE_DYN,
};

enum TypeExtEnum {
  TYPE_EXT_POINTER_CONST,
  TYPE_EXT_POINTER_MUT,
  TYPE_EXT_ARRAY,
};

enum FunctionInstanceFlags : size_t {
  FUNCTION_NORMAL = 0,
  FUNCTION_IS_TEST = 1 << 1,
  FUNCTION_IS_METHOD = 1 << 2,
  FUNCTION_IS_VARARGS = 1 << 3,
  FUNCTION_IS_EXPORTED = 1 << 4,
  FUNCTION_IS_FORWARD_DECLARED = 1 << 5,
  FUNCTION_IS_STATIC = 1 << 6,
  FUNCTION_IS_FOREIGN = 1 << 7,
  FUNCTION_IS_INLINE = 1 << 8,
};

enum StructTypeFlags {
  STRUCT_FLAG_FORWARD_DECLARED = 1 << 0,
  STRUCT_FLAG_IS_ANONYMOUS = 1 << 1,
  STRUCT_FLAG_IS_UNION = 1 << 2,
};

struct ASTExpr;

std::string mangled_type_args(const std::vector<Type *> &args);

struct TypeExtension {
  TypeExtEnum type;
  size_t array_size;
  bool operator==(const TypeExtension &other) const {
    if (type != other.type)
      return false;
    if (type == TYPE_EXT_ARRAY) {
      return array_size == other.array_size;
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
      return (TypeExtEnum)-1;
    } else {
      return extensions.back().type;
    }
  }

  inline bool is_fixed_sized_array() const { return back_type() == TYPE_EXT_ARRAY; }

  inline bool is_pointer() const {
    return back_type() == TYPE_EXT_POINTER_CONST || back_type() == TYPE_EXT_POINTER_MUT;
  }

  // this returns the number of pointers at the BACK of the extensions,
  // so int*[]* would return 1;
  inline int pointer_depth() const {
    auto temp = extensions;
    int depth = 0;
    while (!temp.empty() && (temp.back().type == TYPE_EXT_POINTER_CONST || temp.back().type == TYPE_EXT_POINTER_MUT)) {
      depth++;
      temp.pop_back();
    }
    return depth;
  }

  inline bool is_const_pointer() const { return back_type() == TYPE_EXT_POINTER_CONST; }

  inline bool is_mut_pointer() const { return back_type() == TYPE_EXT_POINTER_MUT; }

  inline bool operator==(const TypeExtensions &other) const { return equals(other); }

  inline bool equals(const TypeExtensions &other) const {
    if (extensions != other.extensions)
      return false;
    return true;
  }

  inline bool has_no_extensions() const { return extensions.empty(); }

  inline bool has_extensions() const { return !has_no_extensions(); }

  inline TypeExtensions append(const TypeExtensions &to_append) const {
    auto these = *this;
    for (const auto &ext : to_append.extensions) {
      these.extensions.push_back({ext});
    }
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
  Scope *scope = nullptr;

  TypeInfo() {}

  template <class T>
    requires std::derived_from<T, TypeInfo>
  inline T *as() {
    return static_cast<T *>(this);
  }

  virtual ~TypeInfo() = default;
  virtual std::string to_string() const { return "Abstract TypeInfo base."; }
};

struct InterfaceTypeInfo : TypeInfo {
  InternedString name;
};

struct ChoiceVariant {
  InternedString name;
  Type *type;
};

struct ChoiceTypeInfo : TypeInfo {
  std::vector<ChoiceVariant> variants;
  int get_variant_index(const InternedString &variant_name) const;
  Type *get_variant_type(const InternedString &variant_name) const;
};

struct ASTFunctionDeclaration;

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() { memset(parameter_types, 0, 256 * sizeof(Type *)); }
  Type *return_type = nullptr;
  Type *parameter_types[256]; // max no of params in c++.
  size_t params_len = 0;
  bool is_varargs = false;
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
  Type *element_type = nullptr;
  bool is_flags = false;
  EnumTypeInfo() {};
};

struct StructTypeInfo : TypeInfo {
  int flags;
  virtual std::string to_string() const override { return ""; }
  StructTypeInfo() {}
};

struct TupleTypeInfo : TypeInfo {
  std::vector<Type *> types;
};

struct DynTypeInfo : TypeInfo {
  Type *interface_type;
  std::vector<std::pair<InternedString, Type *>> methods;
};

// helpers to get scalar types for fast comparison
Type *bool_type();
Type *void_type();
Type *s8_type();
Type *s16_type();
Type *s32_type();
Type *s64_type();
Type *u8_type();
Type *u16_type();
Type *u32_type();
Type *u64_type();
Type *f64_type();
Type *f32_type();

Type *is_tuple_interface();
Type *is_array_interface();
Type *is_pointer_interface();
Type *is_mut_pointer_interface();
Type *is_const_pointer_interface();

InternedString get_tuple_type_name(const std::vector<Type *> &types);

Type *global_create_type(TypeKind, const InternedString &, TypeInfo * = nullptr, const TypeExtensions & = {},
                         Type * = nullptr);

Type *global_create_struct_type(const InternedString &, Scope *, std::vector<Type *> generic_args = {});

Type *global_create_interface_type(const InternedString &name, Scope *scope, std::vector<Type *> generic_args);

Type *global_create_tagged_union_type(const InternedString &name, Scope *scope,
                                      const std::vector<Type *> &generic_args);
Type *global_create_enum_type(const InternedString &, Scope *, bool = false, Type *element_type = s32_type());

Type *global_create_tuple_type(const std::vector<Type *> &types);

ConversionRule type_conversion_rule(const Type *from, const Type *to, const SourceRange & = {});

Type *global_find_function_type_id(const FunctionTypeInfo &, const TypeExtensions &);
Type *global_find_type_id(std::vector<Type *> &tuple_types, const TypeExtensions &type_extensions);
Type *global_find_type_id(Type *, const TypeExtensions &);
void init_type_system();
bool type_is_numerical(const Type *t);
constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to);
// returns false for failure, else true and passed param signature as out.

struct ASTNode;

struct Type {
  // used for mangling and stuff, not used for any comparisons, nor lookups anymore.
  size_t uid;
  Type *base_type = INVALID_TYPE;
  Type *generic_base_type = INVALID_TYPE;
  std::vector<Type *> generic_args{};
  std::vector<Type *> interfaces{};
  Nullable<ASTNode> declaring_node;

  bool dyn_emitted = false;
  bool fwd_decl_is_emitted = false;
  bool tuple_is_emitted = false;
  // if this is an alias or something just get the actual real true type.
  // probably have a better default than this.
  const TypeKind kind = TYPE_SCALAR;

  inline void set_base(const InternedString &base) { this->basename = base; }
  inline void set_ext(const TypeExtensions &ext) { this->extensions = ext; }
  inline void set_info(TypeInfo *info) { this->info = info; }

  bool implements(const Type *interface);

  // TODO: move a lot of the querying methods from *info into here too.
  TypeInfo *info;

  InternedString basename{};

  // TODO: refactor the way type extensions work.
  // most of this should just be on the type itself,
  // especially the querying methods, it's a pain to get the extensions everywhere.
  TypeExtensions extensions{};

  // TODO: also remove me. this can be hand inlined to the one place this is used.
  inline bool equals(const Type *base, const TypeExtensions &type_extensions) const {
    return base_type == base && type_extensions.extensions == extensions.extensions;
  }

  /*
    TODO: remove me. this is used in one place.
  */
  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;

  inline Type() = default;

  inline Type(size_t uid, const TypeKind kind) : uid(uid), kind(kind) {
    if (kind == TYPE_TUPLE) {
      interfaces.push_back(is_tuple_interface());
    }
  }

  /*
    check if the 'kind' is of a certain value.
    note that 'kind' has nothing to do with pointers nor array extensions
    this is effectively checking the 'base type' if this is a pointer.
    use the 'extensions' and it's utilities for more thorough query.
  */
  inline bool is_kind(const TypeKind kind) const { return this->kind == kind; }

  /* convert it to the in-language recognizable representation of the type's name.
      - fn *(s32, s32) -> *mut void;
      - s32[10];
      - List!<(s32, s32)>
    and so on.
  */
  std::string to_string() const;

  // Get the element type of a pointer, or an array.
  Type *get_element_type() const;

  // take a mut/const pointer to this type.
  Type *take_pointer_to(bool) const;

  // To have a null, yet identifyable unresolved generic type,
  // we just reinterpret cast 1 to a Type *. this won't be 'nullptr',
  // but will still effectively be a poison value.
  static Type *UNRESOLVED_GENERIC_TYPE_ID;
  constexpr static Type *INVALID_TYPE = nullptr;
};

static inline constexpr bool type_is_valid(Type *type) {
  return type != Type::UNRESOLVED_GENERIC_TYPE_ID && type != Type::INVALID_TYPE;
}

struct ASTFunctionDeclaration;

InternedString get_function_typename(ASTFunctionDeclaration *);

template <class T> static inline T *type_info_alloc() { return new (type_info_arena.allocate(sizeof(T))) T(); }

enum OperationKind {
  OPERATION_BINARY,
  OPERATION_UNARY,
  OPERATION_SUBSCRIPT,
};

Type *find_operator_overload(int mutability, Type *left_ty, TType op, OperationKind kind);

std::string get_operator_overload_name(TType op, OperationKind kind);

static std::string get_unmangled_name(const Type *type) {
  std::string base = type->basename.get_str();
  auto first = base.find("$");
  if (first != std::string::npos) {
    base = base.substr(0, first);
  }

  if (!type->generic_args.empty()) {
    base += "!<";
    auto it = 0;
    for (auto id : type->generic_args) {
      base += get_unmangled_name(id);
      if (it != type->generic_args.size() - 1) {
        base += ", ";
      }
      it++;
    }
    base += ">";
  }

  auto output = type->extensions.to_string();
  if (!output.empty()) {
    output += " ";
  }
  output += base;

  return output;
}
