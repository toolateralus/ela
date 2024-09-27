#include "type.hpp"
#include "error.hpp"
#include <format>
#include <sstream>

std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << get_type(return_type)->base;
  ss << "(";
  for (int i = 0; i < params_len; ++i) {
    auto t = get_type(parameter_types[i]);
    ss << t->to_string();
    if (i < params_len - 1) {
      ss << ", ";
    }
  }

  if (is_varargs)
    ss << ", ...)";
  else
    ss << ')';

  return ss.str();
}
int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExt &ext) {
  for (int i = 0; i < num_types; ++i) {
    if (type_table[i]->kind != TYPE_FUNCTION)
      continue;
    const Type *type = type_table[i];
    if (name == type->base && type->type_info_equals(&info, TYPE_FUNCTION) &&
        ext.equals(type->extensions)) {
      return type->id;
    }
  }
  auto info_ptr = new (type_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);
  return create_type(TYPE_FUNCTION, name, info_ptr, ext);
}
int find_type_id(const std::string &name,
                 const TypeExt &type_extensions) {

  for (int i = 0; i < num_types; ++i) {
    auto type = type_table[i];
    if (type_extensions.has_no_extensions()) {
      if (type->base == name && type->extensions.has_no_extensions()) {
        return type->id;
      }
    }
    if (type->equals(name, type_extensions))
      return type->id;
  }

  // BELOW IS JUST FOR CREATING TYPES WITH NEW EXTENSIONS. NEW FUNCTION TYPES
  // MUST BE CREATED MANUALLY
  int base_id = -1;

  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->base == name && tinfo->extensions.has_no_extensions()) {
      base_id = tinfo->id;
      break;
    }
  }

  if (base_id != -1) {
    auto t = get_type(base_id);
    return create_type((TypeKind)t->kind, name, nullptr, type_extensions);
  }

  return -1;
}

std::string get_cpp_scalar_type(int id) {
  auto type = get_type(id);
  std::string name = "";
  if (type->base == "s64")
    name = "size_t";
  else if (type->base == "s32")
    name = "int32_t";
  else if (type->base == "s16")
    name = "int16_t";
  else if (type->base == "s8")
    name = "int8_t";
  else if (type->base == "u64")
    name = "uint64_t";
  else if (type->base == "u32")
    name = "uint32_t";
  else if (type->base == "u16")
    name = "uint16_t";
  else if (type->base == "u8")
    name = "uint8_t";
  else if (type->base == "f32")
    name = "float";
  else if (type->base == "f64")
    name = "double";
  else if (type->base == "float")
    name = "float";
  else if (type->base == "int")
    name = "int";
  else if (type->base == "char")
    name = "char";
  else if (type->base == "string")
    name = "const char *";
  else if (type->base == "bool")
    name = "bool";
  else if (type->base == "void")
    name = "void";
  else {
    throw_error(std::format("cannot get cpp scalar type from : {}", type->base),
                ERROR_CRITICAL, {});
  }

  if (type->extensions.has_no_extensions()) {
    return name;
  }

  return type->extensions.to_cpp_string(name);
}

void init_type_system() {
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
constexpr int get_type_unresolved() { return Type::invalid_id; }

// TODO: use some kind of SCALAR_TYPE flags in the ScalarType info to tell if somethings an integer, float, etc,
// TODO: and add conversion rules to it so we can safely up cast but explicitly down cast only.
// Right now, if we had user defined types, you could trick the type system into casting a number to and from your type if it ended with a multiple of 8 -> 64
constexpr bool type_is_numerical(const Type *t) {
  return t->base.ends_with("64") || t->base.ends_with("32") ||
          t->base.ends_with("16") || t->base.ends_with("8") ||
          t->base == "int" || t->base == "float";
};

ConversionRule type_conversion_rule(const Type *from, const Type *to) {
  if (!from || !to) {
    throw_error("type was null when checking type conversion rules",
                ERROR_CRITICAL, {});
  }
  
  if (from->is_kind(TYPE_SCALAR) && from->extensions.has_no_extensions() &&
      to->is_kind(TYPE_SCALAR) && to->extensions.has_no_extensions() &&
      from->base.starts_with(to->base[0])) {
    if (type_is_numerical(from) && type_is_numerical(to)) {
      return CONVERT_IMPLICIT;
    }
  }

  if (from->extensions.is_pointer(1) && to->extensions.is_pointer(1)) {
    return CONVERT_IMPLICIT;
  }

  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;


  return CONVERT_PROHIBITED;
}
int create_type(TypeKind kind, const std::string &name, TypeInfo *info,
                const TypeExt &extensions) {
  Type *type = new (type_alloc<Type>()) Type(num_types, kind);
  type->info = info;
  type->extensions = extensions;

  type->base = name;

  if (type->id > MAX_NUM_TYPES) {
    throw_error("Max types exceeded", ERROR_CRITICAL, {});
  }
  if (type_table[type->id]) {
    throw_error("type system created a type with the same ID twice",
                ERROR_CRITICAL, {});
  }

  type_table[type->id] = type;
  num_types += 1;
  return type->id;
}
std::string Type::to_string() const {

  switch (kind) {
  case TYPE_SCALAR:
    return base + extensions.to_string();
  case TYPE_FUNCTION:
    if (info.is_not_null())
      return info.get()->to_string();
    else
      return "invalid function type";
  case TYPE_STRUCT:
    return "struct NYI";
    break;
  }
}
bool Type::operator==(const Type &type) const {
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];

    if (tinfo->equals(base, extensions) && type.info.is_not_null() &&
        type_info_equals(type.info.get(), type.kind))
      return true;
  }
  return false;
}
bool Type::type_info_equals(const TypeInfo *info, TypeKind kind) const {
  if (!this->info && info) {
    return false;
  }
  if (this->kind != kind)
    return false;
  if (kind == TypeKind::TYPE_FUNCTION) {
    auto finfo = static_cast<const FunctionTypeInfo *>(info);
    auto sinfo = static_cast<const FunctionTypeInfo *>(this->info.get());
    bool params_eq = finfo->params_len == sinfo->params_len;

    if (!params_eq)
      return false;

    for (int i = 0; i < finfo->params_len; ++i)
      if (finfo->parameter_types[i] != sinfo->parameter_types[i]) {
        params_eq = false;
        break;
      }
    return finfo->return_type == sinfo->return_type && params_eq;
  }
  return false;
}
bool Type::equals(const std::string &name,
                  const TypeExt &type_extensions) const {
  if (name != this->base)
    return false;
  return type_extensions == this->extensions;
}
bool TypeExt::equals(const TypeExt &other) const {
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
std::string Type::to_cpp_string() const {
  switch (kind) {
  case TYPE_SCALAR:
    return extensions.to_cpp_string(this->base);
  case TYPE_FUNCTION:
    if (info.is_not_null())
      return info.get()->to_string();
    else
      return "invalid function type";
  case TYPE_STRUCT:
    return "struct NYI";
    break;
  }
}

int remove_one_pointer_ext(int operand_ty,
                           const std::vector<Token> &source_tokens) {
  auto ty = get_type(operand_ty);
  int ptr_depth = 0;
  for (const auto &ext : ty->extensions.extensions) {
    if (ext == TYPE_EXT_POINTER)
      ptr_depth++;
  }

  if (ptr_depth == 0) {
    throw_error("cannot dereference a non-pointer type.", ERROR_FAILURE,
                source_tokens);
  }

  bool pointer_removed = false;
  jstl::Vector<TypeExtEnum> extensions{};
  for (const auto &ext : ty->extensions.extensions) {
    if (!pointer_removed && ext == TYPE_EXT_POINTER) {
      pointer_removed = true;
    } else {
      extensions.push(ext);
    }
  }
  return find_type_id(
      ty->base, TypeExt{.extensions = extensions,
                                  .array_sizes = ty->extensions.array_sizes});
}