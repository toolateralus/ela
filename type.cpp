#include "type.hpp"
#include "error.hpp"
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
  ss << ')';
  return ss.str();
}
int find_type_id(const std::string &name, const FunctionTypeInfo &info,
                 const TypeExtensionInfo &ext) {
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
                 const TypeExtensionInfo &type_extensions) {

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
    printf("creating type: %s\n", t->base.c_str());
    return create_type((TypeKind)t->kind, name, nullptr, type_extensions);
  }

  return -1;
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
int get_type_unresolved() { return Type::invalid_id; }

ConversionRule type_conversion_rule(const Type *from, const Type *to) {
  if (!from || !to) {
    throw_error("type was null when checking type conversion rules",
                 ERROR_CRITICAL, {});
  }

  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  // TODO: have more thorough type conversion rules defined in the typeinfo or
  // something.
  // if (from->is_kind(TYPE_SCALAR) && to->is_kind(TYPE_SCALAR))
  //   return CONVERT_EXPLICIT;

  return CONVERT_PROHIBITED;
}
int create_type(TypeKind kind, const std::string &name, TypeInfo *info,
                const TypeExtensionInfo &extensions) {
  Type *type = new (type_alloc<Type>()) Type(num_types, kind);
  type->info = info;
  type->extensions = extensions;

  type->base = name;

  if (type->id > MAX_NUM_TYPES) {
    throw_error(
        "Max types exceeded",
        ERROR_CRITICAL,
        {}
    );
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
                  const TypeExtensionInfo &type_extensions) const {
  if (name != this->base)
    return false;
  return type_extensions == this->extensions;
}
bool TypeExtensionInfo::equals(const TypeExtensionInfo &other) const {
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
