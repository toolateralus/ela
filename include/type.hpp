
#pragma once

#include <cstddef>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <vector>

#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"

// fwd
struct Type;
struct Scope;

extern std::vector<Type *> type_table;

enum ConversionRule {
  CONVERT_PROHIBITED,
  CONVERT_NONE_NEEDED,
  CONVERT_IMPLICIT,
  CONVERT_EXPLICIT,
};

Token get_unique_identifier();

enum ScalarType: unsigned char {
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

enum TypeExtEnum: unsigned char {
  TYPE_EXT_INVALID,
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
};

enum Function_Instance_Flags : unsigned char {
  FUNCTION_NORMAL = 0,
  FUNCTION_IS_TEST = 1 << 1,
  FUNCTION_IS_METHOD = 1 << 2,
  FUNCTION_IS_VARARGS = 1 << 3,
  FUNCTION_IS_EXPORTED = 1 << 4,
  FUNCTION_IS_FORWARD_DECLARED = 1 << 5,
  FUNCTION_IS_STATIC = 1 << 6,
  FUNCTION_IS_FOREIGN = 1 << 7,
};

enum StructTypeFlags: unsigned char {
  STRUCT_FLAG_FORWARD_DECLARED = 1 << 0,
  STRUCT_FLAG_IS_ANONYMOUS = 1 << 1,
  STRUCT_FLAG_IS_UNION = 1 << 2,
};


std::string mangled_type_args(const std::vector<int> &args);

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

struct Type_Metadata {
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

  inline bool is_pointer() const { return back_type() == TYPE_EXT_POINTER; }

  inline bool operator==(const Type_Metadata &other) const { return equals(other); }

  bool equals(const Type_Metadata &other) const;

  inline bool has_no_extensions() const { return extensions.empty(); }

  inline bool has_extensions() const { return !has_no_extensions(); }

  inline Type_Metadata append(const Type_Metadata &to_append) const {
    auto these = *this;
    for (const auto &meta : to_append.extensions) {
      these.extensions.push_back({meta});
    }
    return these;
  }

  inline Type_Metadata without_back() const {
    Type_Metadata these = *this;
    these.extensions.pop_back();
    return these;
  }

  std::string to_string() const;
};

using GenericParameter = Interned_String;

struct Symbol;
struct Function_Info {
  int return_type = -1;
  // We used to store a fixed array here, but the struct was like 1kb, and it's rarely neccesary.
  // I'd rather have a slightly slower allocator than that.
  std::vector<int> parameter_types;
  bool is_varargs = false;

  std::string to_string(const Type_Metadata &meta) const;
  std::string to_string() const;
};

struct Interface_Info {
  Interned_String name;
  // <method_name, type_signature>.
  std::vector<std::pair<Interned_String, int>> methods;
};

struct Scalar_Info {
  bool is_integral = false;
  size_t size = 0;
  ScalarType scalar_type;
};

struct Enum_Info {
  int element_type = 0;
  bool is_flags = false;
};

struct Struct_Info {
  int flags;
};

struct Tuple_Info {
  std::vector<int> types;
};

template <typename T>
struct Type_Name {
    consteval static std::string get() {
        return typeid(T).name();
    }
};


struct Type_Info {
  Scope scope;
  std::vector<int> implemented_interfaces;
  Type_Kind kind;

  template <class Info> Type_Info(Info info) {
    if constexpr (std::is_same_v<Info, Function_Info>) {
      kind = TYPE_FUNCTION;
      function = info;
    } else if constexpr (std::is_same_v<Info, Interface_Info>) {
      kind = TYPE_INTERFACE;
      interface = info;
    } else if constexpr (std::is_same_v<Info, Scalar_Info>) {
      kind = TYPE_SCALAR;
      scalar = info;
    } else if constexpr (std::is_same_v<Info, Enum_Info>) {
      kind = TYPE_ENUM;
      $enum = info;
    } else if constexpr (std::is_same_v<Info, Struct_Info>) {
      kind = TYPE_STRUCT;
      $struct = info;
    } else if constexpr (std::is_same_v<Info, Tuple_Info>) {
      kind = TYPE_TUPLE;
      tuple = info;
    } else {
      static_assert(false, "unsupported");
    }
  }

  union {
    Function_Info function;
    Interface_Info interface;
    Scalar_Info scalar;
    Enum_Info $enum;
    Struct_Info $struct;
    Tuple_Info tuple;
  };

  Type_Info(const Type_Info &info) {
    if (this != &info) {
      kind = info.kind;
      switch (kind) {
        case TYPE_FUNCTION:
          function.~Function_Info();
          new (&function) Function_Info(info.function);
          break;
        case TYPE_INTERFACE:
          interface.~Interface_Info();
          new (&interface) Interface_Info(info.interface);
          break;
        case TYPE_SCALAR:
          scalar.~Scalar_Info();
          new (&scalar) Scalar_Info(info.scalar);
          break;
        case TYPE_ENUM:
          $enum.~Enum_Info();
          new (&$enum) Enum_Info(info.$enum);
          break;
        case TYPE_STRUCT:
          $struct.~Struct_Info();
          new (&$struct) Struct_Info(info.$struct);
          break;
        case TYPE_TUPLE:
          tuple.~Tuple_Info();
          new (&tuple) Tuple_Info(info.tuple);
          break;
        default:
          break;
      }
    }
  }

  Type_Info(Type_Kind kind) : kind(kind) {}
  ~Type_Info() {}
};

// helpers to get scalar types for fast comparison
int voidptr_type();
int bool_type();
int void_type();
int s8_type();
int s16_type();
int s32_type();
int s64_type();
int u8_type();
int u16_type();
int u32_type();
int u64_type();
int f64_type();
int f32_type();

Type *global_get_type(const int id);
Interned_String get_tuple_type_name(const std::vector<int> &types);
int global_create_type(Type_Kind, const Interned_String &, Type_Info info, const Type_Metadata & = {},
                       const int base_id = -1);
int global_create_struct_type(const Interned_String &, Scope scope, std::vector<int> generic_args = {});

int global_create_interface_type(const Interned_String &name, Scope scope, std::vector<int> generic_args);

int global_create_enum_type(const Interned_String &, Scope, bool = false, size_t element_type = s32_type());
int global_create_tuple_type(const std::vector<int> &types);
ConversionRule type_conversion_rule(const Type *from, const Type *to, const Source_Range & = {});
// char *
int global_find_function_type_id(const Function_Info &, const Type_Metadata &);
int global_find_type_id(std::vector<int> &tuple_types, const Type_Metadata &type_extensions);
int global_find_type_id(const int, const Type_Metadata &);
struct Token;
void init_type_system();
bool type_is_numerical(const Type *t);
constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to);
// returns false for failure, else true and passed param signature as out.
bool get_function_type_parameter_signature(Type *type, std::vector<int> &out);
void emit_warnings_or_errors_for_operator_overloads(const Token_Type type, Source_Range &range);

struct AST;

struct Type {
  int id = INVALID_TYPE_ID;
  int base_id = INVALID_TYPE_ID;
  std::vector<int> generic_args{};
  std::vector<int> interfaces{};
  Nullable<AST> declaring_node;
  // Why are these on the type?
  // These should be on a symbol or something.
  // At the very least, put them in the particular info that they pertain to
  bool emitted_forward_declaration : 1 = false;
  bool emitted_tuple : 1 = false;
  // probably have a better default than this.
  const Type_Kind kind = TYPE_SCALAR;
  Type_Info info;
  Interned_String base{};
  Type_Metadata meta{};

  ~Type() {}

  Type(Interned_String base, const Type_Kind kind, const Type_Info &&info,
       const std::vector<int> &generic_args = {})
      : base(base), id(type_table.size()), kind(kind), info(std::move(info)), generic_args(generic_args) {}

  // You must check .is_pointer() or .is_fixed_array() on the meta first.
  int get_element_type() const;
  int take_pointer_to() const;
  bool is_kind(const Type_Kind kind) const { return this->kind == kind; }
  Type_Metadata const get_ext_no_compound() const { return meta; }
  bool implements(const Interned_String &interface);
  bool equals(const int base, const Type_Metadata &type_extensions) const;
  bool type_info_equals(const Type_Info *info, Type_Kind kind) const;
  std::string to_string() const;
  constexpr static int UNRESOLVED_GENERIC_TYPE_ID = -2;
  constexpr static int INVALID_TYPE_ID = -1;
};

Interned_String get_function_typename(AST *);

enum OperationKind {
  OPERATION_BINARY,
  OPERATION_UNARY,
  OPERATION_SUBSCRIPT,
};

int find_operator_overload(Token_Type op, Type *left_ty, OperationKind kind);
std::string get_operator_overload_name(Token_Type op, OperationKind kind);

static std::string get_unmangled_name(const Type *type) {
  std::string base = type->base.get_str();
  auto first = base.find("$");
  if (first != std::string::npos) {
    base = base.substr(0, first);
  }
  if (!type->generic_args.empty()) {
    base += "![";
    auto it = 0;
    for (auto id : type->generic_args) {
      base += get_unmangled_name(global_get_type(id));
      if (it != type->generic_args.size() - 1) {
        base += ", ";
      }
      it++;
    }
    base += "]";
  }
  base += type->meta.to_string();
  return base;
}
