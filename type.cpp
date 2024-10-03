#include "type.hpp"
#include "ast.hpp"
#include "error.hpp"
#include <sstream>

std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << global_get_type(return_type)->base;
  ss << "(";
  for (int i = 0; i < params_len; ++i) {
    auto t = global_get_type(parameter_types[i]);
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
int global_find_type_id(const std::string &name, const FunctionTypeInfo &info,
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
  return global_create_type(TYPE_FUNCTION, name, info_ptr, ext);
}
int global_find_type_id(const std::string &name,
                        const TypeExt &type_extensions) {

  // linear search with extensions failed: for aliased types,
  // we may need to create a type with a non 'name' name.
  // such as #alias intptr :: int*, where int* hasn't been used yet.
  for (int i = 0; i < num_types; ++i) {
    auto type = type_table[i];
    if (type->has_alias(name) && type->extensions == type_extensions) {
      return type->id;
    } else if (type->has_alias(name) && type->extensions != type_extensions) {
      return global_create_type(type->kind, type->base, type->info,
                                type_extensions);
    }
  }

  for (int i = 0; i < num_types; ++i) {
    auto type = type_table[i];
    auto base = type->base;

    if (type_extensions.has_no_extensions()) {
      if (type->names_match_or_alias(name) &&
          type->extensions.has_no_extensions()) {
        return type->id;
      }
    }
    if (type->equals(name, type_extensions))
      return type->id;
  }

  // NOTE:below is just for creating types with new extensions. new function
  // types, struct types, and enum types must be created manually this just
  // creates pointer and array types of base 'name'
  int base_id = -1;
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->names_match_or_alias(name) &&
        tinfo->extensions.has_no_extensions()) {
      base_id = tinfo->id;
      break;
    }
  }
  if (base_id != -1) {
    auto t = global_get_type(base_id);
    if (!t->has_alias(name))
      return global_create_type((TypeKind)t->kind, name, t->info,
                                type_extensions);
  }

  return -1;
}
std::string get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";
  if (type->base == "s64")
    name = "int64_t";
  else if (type->base == "s32")
    name = "int32_t";
  else if (type->base == "s16")
    name = "int16_t";
  else if (type->base == "s8")
    name = "int8_t";
  else if (type->base == "u64")
    name = "size_t";
  else if (type->base == "u32")
    name = "uint32_t";
  else if (type->base == "u16")
    name = "uint16_t";
  else if (type->base == "char" && type->extensions.is_pointer(1))
    name = "const char";
  else if (type->base == "u8" && type->extensions.is_pointer(1))
    name = "char";
  else if (type->base == "u8")
    name = "uint8_t";
  else if (type->base == "float32")
    name = "float";
  else if (type->base == "float64")
    name = "double";
  else if (type->base == "float")
    name = "float";
  else if (type->base == "int")
    name = "int";
  else if (type->base == "char")
    name = "char";
  else if (type->base == "bool")
    name = "bool";
  else if (type->base == "void")
    name = "void";
  else {
    return type->to_cpp_string();
  }

  if (type->extensions.has_no_extensions()) {
    return name;
  }

  return type->extensions.to_cpp_string(name);
}
ScalarTypeInfo *get_scalar_type_info(ScalarType type, size_t size,
                                     bool is_integral = false) {
  auto info = ast_alloc<ScalarTypeInfo>();
  info->scalar_type = type;
  info->size = size;
  info->is_integral = is_integral;
  return info;
}
void init_type_system() {
  // Signed integers
  {
    global_create_type(TYPE_SCALAR, "s64",
                       get_scalar_type_info(TYPE_S64, 8, true));
    global_create_type(TYPE_SCALAR, "s32",
                       get_scalar_type_info(TYPE_S32, 4, true));
    global_create_type(TYPE_SCALAR, "s16",
                       get_scalar_type_info(TYPE_S16, 2, true));
    global_create_type(TYPE_SCALAR, "s8",
                       get_scalar_type_info(TYPE_S16, 1, true));
  }

  // Unsigned integers
  {
    global_create_type(TYPE_SCALAR, "u64",
                       get_scalar_type_info(TYPE_U64, 8, true));
    global_create_type(TYPE_SCALAR, "u32",
                       get_scalar_type_info(TYPE_U32, 4, true));
    global_create_type(TYPE_SCALAR, "u16",
                       get_scalar_type_info(TYPE_U16, 2, true));
    global_create_type(TYPE_SCALAR, "u8",
                       get_scalar_type_info(TYPE_U16, 1, true));
  }

  // Floats
  {
    global_create_type(TYPE_SCALAR, "float32",
                       get_scalar_type_info(TYPE_FLOAT, 4));
    global_create_type(TYPE_SCALAR, "float64",
                       get_scalar_type_info(TYPE_DOUBLE, 8));
  }

  // Other
  {
    // Other
    // todo: alias these, don't generate new types.
    global_create_type(TYPE_SCALAR, "float",
                       get_scalar_type_info(TYPE_FLOAT, 4));
    global_create_type(TYPE_SCALAR, "int",
                       get_scalar_type_info(TYPE_S32, 4, true));

    global_create_type(TYPE_SCALAR, "char",
                       get_scalar_type_info(TYPE_U8, 1, true));
    global_create_type(TYPE_SCALAR, "bool",
                       get_scalar_type_info(TYPE_BOOL, 1, true));
    global_create_type(TYPE_SCALAR, "void", get_scalar_type_info(TYPE_VOID, 0));
  }
}
constexpr int get_type_unresolved() { return Type::invalid_id; }

constexpr bool type_is_numerical(const Type *t) {
  auto info = dynamic_cast<ScalarTypeInfo *>(t->info);
  if (!info)
    return false;

  auto scalar = info->scalar_type;
  return scalar == TYPE_S8 || scalar == TYPE_S16 || scalar == TYPE_S32 ||
         scalar == TYPE_S64 || scalar == TYPE_U8 || scalar == TYPE_U16 ||
         scalar == TYPE_U32 || scalar == TYPE_U64 || scalar == TYPE_FLOAT ||
         scalar == TYPE_DOUBLE;
}
constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR)
    return false;

  auto from_info = static_cast<ScalarTypeInfo *>(from->info);
  auto to_info = static_cast<ScalarTypeInfo *>(to->info);
  // do not allow casting of float to integer implicitly
  if (!from_info->is_integral && to_info->is_integral) {
    return false;
  }
  return from_info->size <= to_info->size;
}

// CLEANUP(Josh) 10/3/2024, 9:25:36 AM
// This could be significantly improved for readability
// PERFORMANCE(Josh) 10/3/2024, 9:25:51 AM
// Doing linear searches over the types a ton of times might get slower for large programs.
// However per 500 lines of unit tsting which creates a wide variety of types, we only really get
// like 50-100 types total, so a hash map is not really going to pay off that much probably.
ConversionRule type_conversion_rule(const Type *from, const Type *to) {
  if (!from || !to) {
    throw_error("type was null when checking type conversion rules",
                ERROR_CRITICAL, {});
  }

  // same exact type. no cast needed.
  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  // implicitly upcast integer and float types.
  // u8 -> u16 -> u32 etc legal.
  // u16 -> u8 == implicit required.
  if (from->is_kind(TYPE_SCALAR) && from->extensions.has_no_extensions() &&
      to->is_kind(TYPE_SCALAR) && to->extensions.has_no_extensions()) {
    if (type_is_numerical(from) && type_is_numerical(to)) {
      if (numerical_type_safe_to_upcast(from, to))
        return CONVERT_IMPLICIT;
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic
  if (from->is_kind(TYPE_SCALAR) && from->extensions.is_pointer(-1) &&
      to->is_kind(TYPE_SCALAR) && type_is_numerical(to) &&
      to->extensions.has_no_extensions()) {
    return CONVERT_IMPLICIT;
  }

  // cast all numerical types and pointers too booleans implicitly for if
  // statements
  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow
  // this in conditional statements.
  if ((type_is_numerical(from) || from->extensions.is_pointer()) &&
      to->id == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  // TODO: probably want to fix this. right now we have C style pointer casting:
  // any two pointers of the same depth can cast implicitly
  if (from->extensions.is_pointer(1) && to->extensions.is_pointer(1)) {
    return CONVERT_IMPLICIT;
  }

  // search structs for their cast tables.
  // Not super useful
  if (to->kind == TYPE_STRUCT) {
    auto info = static_cast<StructTypeInfo *>(to->info);
    for (const auto &cast : info->implicit_cast_table) {
      if (cast == from->id) {
        return CONVERT_IMPLICIT;
      }
    }
    for (const auto &cast : info->explicit_cast_table) {
      if (cast == from->id) {
        return CONVERT_EXPLICIT;
      }
    }
  }

  // if the type extensions are equal, return the conversion rule for the bases.
  // this allows int[] to cast to s8[] etc;
  if (!from->extensions.has_no_extensions() &&
      !to->extensions.has_no_extensions() &&
      from->extensions.equals(to->extensions)) {
    auto from_base = global_get_type(global_find_type_id(from->base, {}));
    auto to_base = global_get_type(global_find_type_id(to->base, {}));
    return type_conversion_rule(from_base, to_base);
  }

  return CONVERT_PROHIBITED;
}

std::string Type::to_string() const {
  switch (kind) {
  case TYPE_STRUCT:
  case TYPE_SCALAR:
    return base + extensions.to_string();
  case TYPE_FUNCTION:
    return info->to_string() + extensions.to_string();
    break;
  case TYPE_ENUM: {
    return base;
  }
  case TYPE_UNION:
    return base;
  }
}
bool Type::operator==(const Type &type) const {
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];

    if (tinfo->equals(base, extensions) && type.info &&
        type_info_equals(type.info, type.kind))
      return true;
  }
  return false;
}
bool Type::type_info_equals(const TypeInfo *info, TypeKind kind) const {
  if (this->kind != kind)
    return false;
  if (kind == TypeKind::TYPE_FUNCTION) {
    auto finfo = static_cast<const FunctionTypeInfo *>(info);
    auto sinfo = static_cast<const FunctionTypeInfo *>(this->info);

    if (finfo->is_varargs != sinfo->is_varargs) {
      return false;
    }

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
  if (!names_match_or_alias(name))
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
  case TYPE_STRUCT:
    return extensions.to_cpp_string(this->base);
  case TYPE_FUNCTION: {
    // TODO: make it so we don't just presume every func type is a func ptr.
    // I have no idea how we'll do that
    // This is a HOT mess.
    auto info = static_cast<FunctionTypeInfo *>(this->info);
    auto ret = global_get_type(info->return_type)->to_cpp_string();
    std::string params = "(" + extensions.to_string() + ")(";
    for (int i = 0; i < info->params_len; ++i) {
      params += global_get_type(info->parameter_types[i])->to_cpp_string();
      if (i != info->params_len - 1) {
        params += ", ";
      }
    }
    params += ")";
    return ret + params;
  }
  case TYPE_ENUM:
    return base;
  case TYPE_UNION:
    return extensions.to_cpp_string(this->base);
    ;
  }
}
int remove_one_pointer_ext(int operand_ty, const SourceRange &source_range) {
  auto ty = global_get_type(operand_ty);
  int ptr_depth = 0;
  for (const auto &ext : ty->extensions.extensions) {
    if (ext == TYPE_EXT_POINTER)
      ptr_depth++;
  }

  if (ptr_depth == 0) {
    throw_error("cannot dereference a non-pointer type.", ERROR_FAILURE,
                source_range);
  }

  bool pointer_removed = false;
  std::vector<TypeExtEnum> extensions{};
  for (const auto &ext : ty->extensions.extensions) {
    if (!pointer_removed && ext == TYPE_EXT_POINTER) {
      pointer_removed = true;
    } else {
      extensions.push_back(ext);
    }
  }
  return global_find_type_id(
      ty->base, TypeExt{.extensions = extensions,
                        .array_sizes = ty->extensions.array_sizes});
}
int global_create_struct_type(const std::string &name, Scope *scope) {
  auto type = new (type_alloc<Type>()) Type(num_types, TYPE_STRUCT);
  type_table[num_types] = type;
  type->base = name;
  auto info = type_alloc<StructTypeInfo>();
  info->scope = scope;
  type->info = info;
  num_types++;
  return type->id;
}
int global_create_union_type(const std::string &name, Scope *scope,
                             UnionKind kind) {
  auto type = new (type_alloc<Type>()) Type(num_types, TYPE_UNION);
  type_table[num_types] = type;
  type->base = name;
  auto info = type_alloc<UnionTypeInfo>();
  info->kind = kind;
  info->scope = scope;
  type->info = info;
  num_types++;
  return type->id;
}
int global_create_enum_type(const std::string &name,
                            const std::vector<std::string> &keys,
                            bool is_flags) {
  auto id = num_types;
  auto type = new (type_alloc<Type>()) Type(id, TYPE_ENUM);
  type_table[num_types] = type;
  type->base = name;

  auto info = new (type_alloc<EnumTypeInfo>()) EnumTypeInfo();
  info->is_flags = is_flags;
  info->keys = keys;
  type->info = info;

  num_types += 1;
  return type->id;
}
int global_create_type(TypeKind kind, const std::string &name, TypeInfo *info,
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
std::string global_get_function_typename(ASTFunctionDeclaration *decl) {
  std::stringstream ss;
  auto return_type = decl->return_type;
  ss << global_get_type(return_type->resolved_type)->to_string();
  ss << "(";
  for (const auto &param : decl->params->params) {
    ss << global_get_type(param->type->resolved_type)->to_string();
    if (param != decl->params->params.back()) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

int Type::get_element_type() const {
  if (!extensions.is_array()) {
    return -1;
  }
  auto extensions = this->extensions;
  extensions.extensions.pop_back();
  extensions.array_sizes.pop_back();
  return global_find_type_id(base, extensions);
}

// TODO(Josh) 10/3/2024, 9:26:51 AM
// Move this somewhere more appropriate, we're generating C++ code in the type system.
std::string Type::to_type_struct(Context &context) {
  static bool *type_cache = [] {
    auto arr = new bool[MAX_NUM_TYPES];
    memset(arr, false, MAX_NUM_TYPES);
    return arr;
  }();

  if (type_cache[this->id]) {
    return std::format("_type_info[{}]", this->id);
  }

  std::stringstream fields_ss;
  if (kind == TYPE_STRUCT) {
    auto info = static_cast<StructTypeInfo *>(this->info);
    
    if (info->scope->symbols.empty()) {
      fields_ss << "_type_info[" << id << "] = new Type {"
                << ".name = \"" << to_string() << "\","
                << ".id = " << id << "}";
      context.type_info_strings.push_back(fields_ss.str());
      return std::string("_type_info[") + std::to_string(id) + "]";
    }
    fields_ss << "{";

    int count = info->scope->symbols.size();
    int it = 0;
    for (const auto &tuple : info->scope->symbols) {
      auto &[name, sym] = tuple;
      auto t = global_get_type(sym.type_id);
      if (!t) {
        throw_error("Internal Compiler Error: Type was null in reflection 'to_type_struct()'", ERROR_FAILURE, {});
      }
      
      fields_ss << "new Field { " << std::format(".name = \"{}\"", name) << ", " << std::format(".type = {}", t->to_type_struct(context)) << " }";
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else {
    
    // new Type { .id = id,
    // .name = "name"
    // }
    
    fields_ss << "_type_info[" << id << "] = new Type {"
              << ".id = " << id << ",\n"
              << ".name = \"" << to_string() << "\"}";
              
    context.type_info_strings.push_back(fields_ss.str());
    return std::string("_type_info[") + std::to_string(id) + "]";
  }

  type_cache[this->id] = true;

  context.type_info_strings.push_back(
      std::format("_type_info[{}] = new Type {{ .id = {}, .name = \"{}\", .fields = {} }}", id, 
                  id, to_string(), fields_ss.str()));

  return std::format("_type_info[{}]", this->id);
}
Token get_anonymous_struct_name() {
  static int num = 0;
  auto tok = Token({}, "__anon_D" + std::to_string(num), TType::Identifier,
                   TFamily::Identifier);
  num++;
  return tok;
}

int voidptr_type() {
  static int type = global_find_type_id(
      "void", TypeExt{.extensions = {TYPE_EXT_POINTER}, .array_sizes = {}});
  return type;
}
int bool_type() {
  static int type = global_find_type_id("bool", {});
  return type;
}
int void_type() {
  static int type = global_find_type_id("void", {});
  return type;
}
int u8_type() {
  static int type = global_find_type_id("u8", {});
  return type;
}
int u16_type() {
  static int type = global_find_type_id("u16", {});
  return type;
}
int u32_type() {
  static int type = global_find_type_id("u32", {});
  return type;
}
int u64_type() {
  static int type = global_find_type_id("u64", {});
  return type;
}
int s8_type() {
  static int type = global_find_type_id("s8", {});
  return type;
}
int s16_type() {
  static int type = global_find_type_id("s16", {});
  return type;
}
int s32_type() {
  static int type = global_find_type_id("s32", {});
  return type;
}
int s64_type() {
  static int type = global_find_type_id("s64", {});
  return type;
}
int int_type() {
  static int type = global_find_type_id("int", {});
  return type;
}
int charptr_type() {
  static int type =
      global_find_type_id("char", {.extensions = {TYPE_EXT_POINTER}});
  return type;
}
int float32_type() {
  static int type = global_find_type_id("float32", {});
  return type;
}
int float64_type() {
  static int type = global_find_type_id("float64", {});
  return type;
}
int float_type() {
  static int type = global_find_type_id("float", {});
  return type;
}