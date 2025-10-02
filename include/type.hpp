
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
#include "strings.hpp"

// fwd
struct Type;
struct ASTVariable;
struct Scope;
struct Context;

extern std::vector<Type *> type_table;
extern std::vector<Type *> structural_type_table;
extern std::vector<Type *> function_type_table;

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
  TYPE_TRAIT,
  TYPE_DYN,
};

enum TypeExtEnum {
  TYPE_EXT_POINTER_CONST,
  TYPE_EXT_POINTER_MUT,
  TYPE_EXT_ARRAY,
};

struct ASTExpr;

std::string mangled_type_args(const std::vector<Type *> &args);

struct TypeExtension {
  TypeExtEnum type;
  size_t array_size;
  bool operator==(const TypeExtension &other) const {
    if (type != other.type) return false;
    if (type == TYPE_EXT_ARRAY) {
      return array_size == other.array_size;
    }
    return true;
  }
};

using TypeExtensions = std::vector<TypeExtension>;

std::string inline extensions_to_string(const TypeExtensions &extensions) {
  std::stringstream ss;
  for (const auto ext : extensions) {
    switch (ext.type) {
      case TYPE_EXT_POINTER_MUT:
        ss << "*mut";
        break;
      case TYPE_EXT_POINTER_CONST:
        ss << "*const";
        break;
      case TYPE_EXT_ARRAY:
        ss << "[" << ext.array_size << "]";
        break;
    }
  }
  return ss.str();
}

inline bool type_extensions_is_back_pointer(const TypeExtensions &extensions) {
  return extensions.size() &&
         (extensions.back().type == TYPE_EXT_POINTER_CONST || extensions.back().type == TYPE_EXT_POINTER_MUT);
}

inline bool type_extensions_is_back_const_pointer(const TypeExtensions &extensions) {
  return extensions.size() && extensions.back().type == TYPE_EXT_POINTER_CONST;
}

inline bool type_extensions_is_back_mut_pointer(const TypeExtensions &extensions) {
  return extensions.size() && extensions.back().type == TYPE_EXT_POINTER_MUT;
}

inline bool type_extensions_is_back_array(const TypeExtensions &extensions) {
  return extensions.size() && (extensions.back().type == TYPE_EXT_ARRAY);
}

using GenericParameter = InternedString;

struct THIR;
struct TypeMember {
  InternedString name;
  Type *type;
  Nullable<ASTExpr> default_value;
  Nullable<THIR> thir_value;
};

struct TypeInfo {
  Scope *scope = nullptr;

  std::vector<TypeMember> members;
  TypeInfo() {}

  template <class T>
    requires std::derived_from<T, TypeInfo>
  inline T *as() {
    return static_cast<T *>(this);
  }

  virtual ~TypeInfo() = default;
  virtual std::string to_string() const { return "Abstract TypeInfo base."; }

  virtual size_t size_in_bytes() const = 0;

  inline TypeMember const *find_member(const InternedString &name) const {
    for (auto member = members.begin(); member != members.end(); ++member) {
      if (member->name == name) {
        return member.base();
      }
    }
    return nullptr;
  }
};

struct TraitTypeInfo : TypeInfo {
  InternedString name;
  bool is_forward_declared = false;
  
  // Zero size type.
  size_t size_in_bytes() const override { return 0; }
};

struct ChoiceTypeInfo : TypeInfo {
  int get_variant_discriminant(const InternedString &variant_name) const;
  Type *get_variant_type(const InternedString &variant_name) const;

  size_t size_in_bytes() const override;
};

struct ASTFunctionDeclaration;

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo() { memset(parameter_types, 0, 256 * sizeof(Type *)); }
  Type *return_type = nullptr;
  Type *parameter_types[256];  // max no of params in c++.
  size_t params_len = 0;
  bool is_varargs = false;
  virtual std::string to_string() const override;
  std::string to_string(const TypeExtensions &ext) const;
  size_t size_in_bytes() const override {
    return sizeof(void *);  // This is only ever sized as a function pointer.
  }
};

struct ScalarTypeInfo : TypeInfo {
  ScalarTypeInfo() {}
  bool is_integral = false;
  size_t size = 0;
  ScalarType scalar_type;
  virtual std::string to_string() const override { return ""; }

  size_t size_in_bytes() const override {
    // TODO: why do we even use 'size' when we switch over this anyway?
    // I'm just afraid that size is wrong xD
    switch (scalar_type) {
      case TYPE_S8:
        return 1;
      case TYPE_U8:
        return 1;
      case TYPE_S16:
        return 2;
      case TYPE_U16:
        return 2;
      case TYPE_S32:
        return 4;
      case TYPE_U32:
        return 4;
      case TYPE_S64:
        return 8;
      case TYPE_U64:
        return 8;
      case TYPE_FLOAT:
        return 4;
      case TYPE_DOUBLE:
        return 8;
      case TYPE_CHAR:
        return 1;
      case TYPE_BOOL:
        return 1;
      case TYPE_VOID:
        return 0;
      default:
        return sizeof(void *);
    }
  }
};

struct EnumTypeInfo : TypeInfo {
  Type *underlying_type = nullptr;
  bool is_flags = false;
  EnumTypeInfo() {};
  size_t size_in_bytes() const override;
};

struct StructTypeInfo : TypeInfo {
  bool is_forward_declared : 1 = false;
  bool is_anonymous : 1 = false;   // this is for anonymous substructs!!! not for structural, unnamed types!
  bool is_structural : 1 = false;  // _this_ is for independent unnamed structs!
  bool is_union : 1 = false;
  virtual std::string to_string() const override { return ""; }
  StructTypeInfo() {}
  size_t size_in_bytes() const override;
  inline bool structural_match(std::vector<Type *> types) const {
    if (types.size() != members.size()) return false;
    for (size_t i = 0; i < types.size(); ++i) {
      if (types[i] != members[i].type) {
        return false;
      }
    }
    return true;
  }
};

struct TupleTypeInfo : TypeInfo {
  std::vector<Type *> types;
  size_t size_in_bytes() const override;
};

struct DynTypeInfo : TypeInfo {
  Type *trait_type;
  std::vector<std::pair<InternedString, Type *>> methods;

  size_t size_in_bytes() const override {
    size_t method_ptrs_size = methods.size() * sizeof(void *);
    size_t instance_size = sizeof(void *);
    return method_ptrs_size + instance_size;
  }
};

// helpers to get scalar types for fast comparison
Type *bool_type();
Type *void_type();
Type *unit_type();
Type *s8_type();
Type *s16_type();
Type *s32_type();
Type *s64_type();
Type *u8_type();
Type *u8_ptr_type();
Type *u16_type();
Type *u32_type();
Type *u64_type();
Type *f64_type();
Type *f32_type();

Type *char_ptr_type();
Type *char_type();

Type *is_fn_trait();

Type *is_fn_ptr_trait();
Type *is_tuple_trait();
Type *is_array_trait();

Type *is_struct_trait();
Type *is_enum_trait();
Type *is_choice_trait();
Type *is_dyn_trait();
Type *is_union_trait();

Type *is_pointer_trait();
Type *is_mut_pointer_trait();
Type *is_const_pointer_trait();
Type *is_slice_trait();
Type *is_slice_mut_trait();

// Kind of a specific trait to be compiler implemented,
// but it will greatly improve compile time reflection capabilities
// when writing serialization/ transmittion libraries.
Type *blittable_trait();

void assess_and_try_add_blittable_trait(Type *type);

InternedString get_tuple_type_name(const std::vector<Type *> &types);

Type *global_create_type(TypeKind, const InternedString &, TypeInfo * = nullptr, const TypeExtensions & = {},
                         Type * = nullptr);

Type *global_create_struct_type(const InternedString &, Scope *, std::vector<Type *> generic_args = {});

Type *global_create_trait_type(const InternedString &name, Scope *scope, std::vector<Type *> generic_args);

Type *global_create_choice_type(const InternedString &name, Scope *scope, const std::vector<Type *> &generic_args);
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

  // 'element type' of pointer types & array types.
  Type *base_type = INVALID_TYPE;

  // the generic that was monomorphized to create this concrete type.
  Type *generic_base_type = INVALID_TYPE;

  // the arguments that were used to monomorphize the above generic to create this concrete type.
  std::vector<Type *> generic_args{};

  // the traits that this type implements.
  std::vector<Type *> traits{};

  // the AST node that was used to declare this type, if not a built-in.
  Nullable<ASTNode> declaring_node;

  // the 'kind' of this type. note that this doesn't describe anything about it being a pointer,
  // this will always be the kind of the base type. so a `**SomeStruct` is still `KIND_STRUCT`.
  const TypeKind kind = TYPE_SCALAR;

  // the parent choice type that declared this subvariant. more than often null, so TODO: this should be nullable.
  Type *choice_parent = nullptr;

  // TODO: move a lot of the querying methods from *info into here too.
  // a specialized polymorphic type info, used for kind-specific attributes. use the 'as' method for easy casting.
  TypeInfo *info;

  // the actual name of the type, without extensions and generics.
  InternedString basename{};

  bool fwd_decl_is_emitted = false;
  bool tuple_is_emitted = false;
  bool dyn_emitted = false;

  TypeExtensions extensions{};

  inline TypeExtEnum back_ext_type() const {
    if (extensions.empty()) {
      return (TypeExtEnum)-1;
    } else {
      return extensions.back().type;
    }
  }

  inline bool is_fixed_sized_array() const { return back_ext_type() == TYPE_EXT_ARRAY; }

  inline bool has_fixed_array_ext_anywhere() const {
    for (const auto &ext : extensions) {
      if (ext.type == TYPE_EXT_ARRAY && ext.array_size != (size_t)-1) {
        return true;
      }
    }
    return false;
  }

  inline bool is_pointer() const {
    return back_ext_type() == TYPE_EXT_POINTER_CONST || back_ext_type() == TYPE_EXT_POINTER_MUT;
  }

  // this returns the number of pointers at the BACK of the extensions,
  // so *List!<*int> would return 1;
  inline int pointer_depth() const {
    auto temp = extensions;
    int depth = 0;
    while (!temp.empty() && (temp.back().type == TYPE_EXT_POINTER_CONST || temp.back().type == TYPE_EXT_POINTER_MUT)) {
      depth++;
      temp.pop_back();
    }
    return depth;
  }

  inline bool is_const_pointer() const { return back_ext_type() == TYPE_EXT_POINTER_CONST; }
  inline bool is_mut_pointer() const { return back_ext_type() == TYPE_EXT_POINTER_MUT; }

  inline bool extensions_equals(const TypeExtensions &other) const {
    if (other.size() != extensions.size()) return false;
    for (size_t i = 0; i < other.size(); ++i) {
      if (other[i] != extensions[i]) {
        return false;
      }
    }
    return true;
  }

  inline bool has_no_extensions() const { return extensions.empty(); }
  inline bool has_extensions() const { return !has_no_extensions(); }

  // This doesn't actually modify the extensions so it shouldn't be called append.
  // it just returns a new set.
  inline TypeExtensions append_extension(const TypeExtensions &to_append) const {
    auto these = this->extensions;
    these.append_range(to_append);
    return these;
  }

  inline TypeExtensions extensions_without_back() const {
    TypeExtensions these = extensions;
    these.pop_back();
    return these;
  }

  // TODO: remove these useless methods, these only existed because these 3 fields were private for a long time, an
  // artifact of a refactor.
  inline void set_base(const InternedString &base) { this->basename = base; }
  inline void set_ext(const TypeExtensions &ext) { this->extensions = ext; }
  inline void set_info(TypeInfo *info) { this->info = info; }

  inline bool is_child_of_choice_type() const { return choice_parent != nullptr; }

  bool implements(const Type *trait);

  /*
    TODO: remove me. this is used in one place.
    Or, at least move it out of the type.
    this is also just for function type info. we don't need this at all, just extract the contents to the one place its
    used or whatever.
  */
  bool type_info_equals(const TypeInfo *info, TypeKind kind) const;

  inline Type() = default;

  inline Type(size_t uid, const TypeKind kind) : uid(uid), kind(kind) {
    if (kind == TYPE_TUPLE) {
      traits.push_back(is_tuple_trait());
    }
    if (kind == TYPE_FUNCTION) {
      traits.push_back(is_fn_trait());
    }
  }

  // helper.
  inline bool is_kind(const TypeKind kind) const { return this->kind == kind; }

  /* convert it to the in-language recognizable representation of the type's name.
      - fn(s32, s32) -> *mut void;
      - s32[10];
      - List!<(s32, s32)>
    and so on.
  */
  std::string to_string() const;

  // Get the element type of a pointer, or an array.
  Type *get_element_type() const;

  // take a mut/const pointer to this type.
  Type *take_pointer_to(bool) const;

  Type *make_array_of(size_t size) const {
    return global_find_type_id((Type *)this, {{.type = TYPE_EXT_ARRAY, .array_size = size}});
  }

  // To have a null, yet identifyable unresolved generic type,
  // we just reinterpret cast 1 to a Type *. this won't be 'nullptr',
  // but will still effectively be a poison/invalid, but distinct and comparable value.
  // use `type_is_valid()` to check for this as well as null, instead of `type != nullptr`
  static Type *UNRESOLVED_GENERIC;
  constexpr static Type *INVALID_TYPE = nullptr;

  size_t size_in_bytes() const;
  size_t alignment_in_bytes() const;

  bool has_dependencies() const;
  size_t offset_in_bytes(const InternedString &field) const;
};

static inline constexpr bool type_is_valid(Type *type) {
  return type != Type::UNRESOLVED_GENERIC && type != Type::INVALID_TYPE;
}

struct ASTFunctionDeclaration;

template <class T>
static inline T *type_info_alloc() {
  return new (type_info_arena.allocate(sizeof(T))) T();
}

enum OperationKind {
  OPERATION_BINARY,
  OPERATION_UNARY,
  OPERATION_INDEX,
};

Type *find_operator_overload(int mutability, Type *left_ty, TType op, OperationKind kind);

std::string get_operator_overload_name(TType op, OperationKind kind);

static inline std::string get_unmangled_name(const Type *type) {
  std::string base = type->basename.get_str();
  auto first = base.find("$");
  if (first != std::string::npos) {
    base = base.substr(0, first);
  }

  if (!type->generic_args.empty()) {
    base += "!<";
    size_t idx = 0;
    for (auto id : type->generic_args) {
      base += get_unmangled_name(id);
      if (idx != type->generic_args.size() - 1) {
        base += ", ";
      }
      idx++;
    }
    base += ">";
  }

  auto output = extensions_to_string(type->extensions);
  if (!output.empty()) {
    output += " ";
  }
  output += base;

  return output;
}

static inline constexpr size_t get_reflection_type_flags(Type *type) {
  int kind_flags = 0;
  switch (type->kind) {
    case TYPE_SCALAR: {
      auto sint =
          type == s32_type() || type == s8_type() || type == s16_type() || type == s32_type() || type == s64_type();
      auto uint = type == u8_type() || type == u16_type() || type == u32_type() || type == u64_type();
      auto floating_pt = type == f32_type() || type == f64_type();
      if (sint) {
        kind_flags |= TYPE_FLAGS_SIGNED;
      } else if (uint) {
        kind_flags |= TYPE_FLAGS_UNSIGNED;
      }
      if (sint || uint) {
        kind_flags |= TYPE_FLAGS_INTEGER;
      } else if (floating_pt) {
        kind_flags |= TYPE_FLAGS_FLOAT;
      } else if (type == bool_type()) {
        kind_flags |= TYPE_FLAGS_BOOL;
      }
    } break;
    case TYPE_FUNCTION: {
      kind_flags = TYPE_FLAGS_FUNCTION;
    } break;
    case TYPE_STRUCT: {
      kind_flags = TYPE_FLAGS_STRUCT;
      auto info = type->info->as<StructTypeInfo>();
      if (info->is_union) {
        kind_flags = TYPE_FLAGS_UNION;
      }
    } break;
    case TYPE_ENUM: {
      kind_flags = TYPE_FLAGS_ENUM;
    } break;
    case TYPE_TUPLE: {
      kind_flags = TYPE_FLAGS_TUPLE;
    } break;
    case TYPE_CHOICE: {
      kind_flags = TYPE_FLAGS_CHOICE;
    } break;
    case TYPE_TRAIT: {
      kind_flags = TYPE_FLAGS_TRAIT;
    } break;
    case TYPE_DYN: {
      kind_flags = TYPE_FLAGS_DYN;
    } break;
  }

  // TODO: shouldn't this only account for the back extension?
  // if i have *const u8[1], an array of pointers, we shouldn't get both flags.
  for (const auto &ext : type->extensions) {
    switch (ext.type) {
      case TYPE_EXT_POINTER_MUT:
      case TYPE_EXT_POINTER_CONST:
        // TODO: add specific type flags for mut / const pointers?
        kind_flags |= TYPE_FLAGS_POINTER;
        break;
      case TYPE_EXT_ARRAY:
        kind_flags |= TYPE_FLAGS_ARRAY;
        break;
    }
  }

  return kind_flags;
}
