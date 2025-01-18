#include "type.hpp"

#include <cstddef>
#include <ostream>
#include <sstream>
#include <vector>

#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"

std::string FunctionTypeInfo::to_string(const TypeExtensions &ext) const {
  std::stringstream ss;
  ss << "fn ";
  ss << ext.to_string() << ' ';
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

  ss << " -> " << global_get_type(return_type)->get_base().get_str();

  return ss.str();
}
std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << "fn ";
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

  ss << " -> " << global_get_type(return_type)->get_base().get_str();

  return ss.str();
}

Type *global_get_type(const int id) {
  [[unlikely]] if (id < 0)
    return nullptr;
  return &type_table[id];
}

int global_find_function_type_id(const FunctionTypeInfo &info, const TypeExtensions &type_extensions) {
  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i].kind != TYPE_FUNCTION)
      continue;
    const Type *type = &type_table[i];
    if (type->type_info_equals(&info, TYPE_FUNCTION) && type->get_ext() == type_extensions) {
      return type->id;
    }
  }
  auto base = -1;
  auto type_name = info.to_string();
  auto info_ptr = new (type_info_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);
  if (type_extensions.has_extensions()) {
    base = global_create_type(TYPE_FUNCTION, type_name, info_ptr, {});
  }
  return global_create_type(TYPE_FUNCTION, type_name, info_ptr, type_extensions, base);
}

int global_find_type_id(const int base, const TypeExtensions &type_extensions) {
  if (base < 0) {
    return -1;
  }

  if (!type_extensions.has_extensions()) {
    return base;
  }
  auto base_t = global_get_type(base);
  auto ext = type_extensions;
  while (base_t && base_t->base_id != Type::invalid_id) {
    ext = base_t->get_ext().append(ext);
    base_t = global_get_type(base_t->base_id);
  }

  if (!base_t) {
    throw_error("INTERNAL_COMPILER_ERROR: global_find_type_id() reduced a type to nullptr when removing extensions",
                {});
  }

  for (int i = 0; i < type_table.size(); ++i) {
    auto type = global_get_type(i);
    if (type->equals(base_t->id, ext))
      return type->id;
  }
  return global_create_type(base_t->kind, base_t->get_base(), base_t->get_info(), ext, base_t->id);
}

int global_find_type_id(std::vector<int> &tuple_types, const TypeExtensions &type_extensions) {
  for (int i = 0; i < type_table.size(); ++i) {
    auto type = &type_table[i];

    if (!type->is_kind(TYPE_TUPLE))
      continue;

    auto info = (type->get_info()->as<TupleTypeInfo>());

    if (info->types.size() != tuple_types.size())
      continue;

    for (int i = 0; i < info->types.size(); ++i)
      if (info->types[i] != tuple_types[i])
        goto end_of_loop;

    if (type->get_ext() == type_extensions) {
      // Found a matching type with the same extensions. Return it.
      return type->id;
    }
  end_of_loop:
    do {
    } while (false); // This line just prevents a MSVC syntax error. Silly.
  }

  // We didn't find the tuple type. Return a new one.
  return global_create_tuple_type(tuple_types, type_extensions);
}

ConversionRule type_conversion_rule(const Type *from, const Type *to, const SourceRange &source_range) {
  // just to make it more lax at call sites, we check here.
  if (!from || !to) {
    throw_error("Internal Compiler Error: type was null when checking type "
                "conversion rules",
                source_range);
  }

  // * Same exact type. no cast needed.
  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  // ! We can cast tuples as long as their interior types are castable?
  // ! this seems like nonsense. Why?
  if (from->is_kind(TYPE_TUPLE) && to->is_kind(TYPE_TUPLE)) {
    auto from_info = (from->get_info()->as<TupleTypeInfo>());
    auto to_info = (to->get_info()->as<TupleTypeInfo>());
    if (from_info->types.size() != to_info->types.size()) {
      return CONVERT_PROHIBITED;
    }
    ConversionRule rule;
    for (int i = 0; i < from_info->types.size(); ++i) {
      auto from_t = from_info->types[i];
      auto to_t = to_info->types[i];
      rule = type_conversion_rule(global_get_type(from_t), global_get_type(to_t), source_range);
      if (rule == CONVERT_PROHIBITED || rule == CONVERT_EXPLICIT) {
        return rule;
      }
    }
    return rule;
  }

  // implicitly upcast integer and float types.
  // u8 -> u16 -> u32 etc legal.
  // u16 -> u8 == implicit required.
  if (from->is_kind(TYPE_SCALAR) && from->get_ext().has_no_extensions() && 
      to->is_kind(TYPE_SCALAR) && to->get_ext().has_no_extensions()) {
    if (type_is_numerical(from) && type_is_numerical(to)) {
      if (numerical_type_safe_to_upcast(from, to)) {
        return CONVERT_IMPLICIT;
      }
      return CONVERT_EXPLICIT;
    } else if ((from->id == bool_type() && type_is_numerical(to)) || to->id == bool_type() && type_is_numerical(from)) { // Convert booleans to number types explicitly
      // TODO(Josh) 1/13/2025, 3:07:06 PM :: Why did I have to add this? I could've sworn we had this working othrwise.
      // TODO: It's possible we just never noticed.
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic, from scalar type pointers, to numerical types.
  const auto from_is_scalar_ptr = from->is_kind(TYPE_SCALAR) && from->get_ext().is_pointer();
  const auto to_is_non_ptr_number = type_is_numerical(to) && to->get_ext().has_no_extensions();
  if (from_is_scalar_ptr && to_is_non_ptr_number) {
    return CONVERT_IMPLICIT;
  }

  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow in if (...)
  // cast all numerical types and pointers to booleans implicitly.
  if ((type_is_numerical(from) || from->get_ext().is_pointer()) && to->id == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  if (type_is_numerical(from) && to->id == bool_type()) {
    return CONVERT_EXPLICIT;
  }

  // ! This needs to be re-evaluated. We should not be able to cast any pointer, to any other pointer.
  const auto implicit_ptr_cast = from->get_ext().is_pointer() && to->get_ext().is_pointer();

  // If we have a fixed array such as
  // char[5] and the argument takes void*
  // we check if char* can cast to void*, and if it can, we allow the cast.
  // this obviously works for char* too.
  const auto implicit_fixed_array_to_ptr_cast = [&]() {
    // not array, return.
    if (!from->get_ext().is_fixed_sized_array())
      return false;

    if (!to->get_ext().is_pointer())
      return false;

    auto element_ty_ptr = global_get_type(global_get_type(from->get_element_type())->take_pointer_to());
    auto rule = type_conversion_rule(element_ty_ptr, to, source_range);

    return rule == CONVERT_IMPLICIT || rule == CONVERT_NONE_NEEDED;
  }();

  // TODO: we should probably only allow implicit casting of pointers to void*, and u8*, for ptr arithmetic and C
  // interop. This is far too C-like and highly unsafe.
  if (implicit_ptr_cast || implicit_fixed_array_to_ptr_cast) {
    return CONVERT_IMPLICIT;
  }

  // TODO: this allows two way casting of enums to their underlying type. Is this what we want?
  // It kinda ruins the safety aspect of having strongly typed enums.
  {
    if (from->is_kind(TYPE_ENUM) && from->get_ext().has_no_extensions()) {
      auto enum_info = (from->get_info()->as<EnumTypeInfo>());
      return type_conversion_rule(global_get_type(enum_info->element_type), to, source_range);
    }

    // TODO: do a runtime bounds check on explicit casting of an integer to an enum type?
    // You can get segfaults from that easily.
    if (to->is_kind(TYPE_ENUM) && to->get_ext().has_no_extensions()) {
      auto enum_info = (to->get_info()->as<EnumTypeInfo>());
      return type_conversion_rule(from, global_get_type(enum_info->element_type), source_range);
    }
  }

  // * if the type extensions are equal, return the conversion rule for the bases.
  {
    // this allows int[] to cast to s8[] etc;
    // this kind of behaviour perhaps will cause problems later down the line, the more C like we become,
    // we can't simple reinterpret a u64[] dynamic array as a s8[]
    if (from->get_ext().has_extensions() && to->get_ext().has_extensions() &&
        from->get_ext().extensions.back() == to->get_ext().extensions.back()) {
      auto from_base = global_get_type(global_find_type_id(from->base_id, from->get_ext().without_back()));
      auto to_base = global_get_type(global_find_type_id(to->base_id, to->get_ext().without_back()));
      return type_conversion_rule(from_base, to_base, source_range);
    }
  }

  return CONVERT_PROHIBITED;
}

bool Type::type_info_equals(const TypeInfo *info, TypeKind kind) const {
  if (this->kind != kind)
    return false;
  if (kind == TypeKind::TYPE_FUNCTION) {
    auto finfo = static_cast<const FunctionTypeInfo *>(info);
    auto sinfo = static_cast<const FunctionTypeInfo *>(this->get_info());

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

bool Type::equals(const int base, const TypeExtensions &type_extensions) const {
  auto isBaseIdEqual = base_id == base;
  auto isTypeExtensionEqual = type_extensions == get_ext();
  return isBaseIdEqual && isTypeExtensionEqual;
}

bool TypeExtensions::equals(const TypeExtensions &other) const {
  if (extensions != other.extensions)
    return false;
  return true;
}

std::string Type::to_string() const {
  switch (kind) {
    case TYPE_FUNCTION:
      return (get_info()->as<FunctionTypeInfo>())->to_string(extensions) + extensions.to_string();
    case TYPE_STRUCT:
    case TYPE_TUPLE:
    case TYPE_SCALAR:
      return base.get_str() + extensions.to_string();
    case TYPE_ENUM:
    case TYPE_TAGGED_UNION:
    case TYPE_INTERFACE:
      return base.get_str();
      break;
  }
}


int global_create_interface_type(const InternedString &name, Scope *scope,
                                 std::vector<int> generic_args) {
  type_table.emplace_back(type_table.size(), TYPE_INTERFACE);
  Type *type = &type_table.back();
  type->set_base(name);
  type->generic_args = generic_args;
  InterfaceTypeInfo *info = type_info_alloc<InterfaceTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  return type->id;
}

int global_create_struct_type(const InternedString &name, Scope *scope, std::vector<int> generic_args) {
  type_table.emplace_back(type_table.size(), TYPE_STRUCT);
  Type *type = &type_table.back();
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  StructTypeInfo *info = type_info_alloc<StructTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  return type->id;
}

int global_create_tagged_union_type(const InternedString &name, Scope *scope) {
  type_table.emplace_back(type_table.size(), TYPE_TAGGED_UNION);
  Type *type = &type_table.back();
  type->set_base(name);
  TaggedUnionTypeInfo *info = type_info_alloc<TaggedUnionTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  return type->id;
}

int global_create_enum_type(const InternedString &name, Scope *scope, bool is_flags, size_t element_type) {
  type_table.emplace_back(type_table.size(), TYPE_ENUM);
  Type *type = &type_table.back();
  type->set_base(name);
  EnumTypeInfo *info = type_info_alloc<EnumTypeInfo>();
  info->is_flags = is_flags;
  info->scope = scope;
  type->set_info(info);
  return type->id;
}
int global_create_type(TypeKind kind, const InternedString &name, TypeInfo *info, const TypeExtensions &extensions,
                       const int base_id) {
  auto &type = type_table.emplace_back(type_table.size(), kind);
  type.base_id = base_id;
  type.set_ext(extensions);
  type.set_base(name);
  type.set_info(info);
  if (!info->scope) {
    info->scope = create_child(root_scope);
  }
  return type.id;
}
InternedString get_function_typename(ASTFunctionDeclaration *decl) {
  std::stringstream ss;
  auto return_type = decl->return_type;
  ss << "fn ";
  ss << "(";
  for (const auto &param : decl->params->params) {
    ss << global_get_type(param->resolved_type)->to_string();
    if (param != decl->params->params.back()) {
      ss << ", ";
    }
  }
  ss << ")";
  ss << " -> " << global_get_type(return_type->resolved_type)->to_string();
  return ss.str();
}

int Type::get_element_type() const {
  if (!extensions.is_pointer() && !extensions.is_array() && !extensions.is_fixed_sized_array() &&
      !extensions.is_map()) {
    throw_error(std::format("Internal compiler error: called get_element_type() on a non pointer/array/map type\ngot type: \"{}\"",
                            to_string()),
                {});
  }
  auto extensions = this->get_ext().without_back();
  if (is_kind(TYPE_TUPLE)) {
    auto info = (get_info()->as<TupleTypeInfo>());
    return global_find_type_id(info->types, extensions);
  } else
    return global_find_type_id(base_id, extensions);
}

// used for anonymous structs etc.
Token get_unique_identifier() {
  static int num = 0;
  auto tok = Token({}, "__anon_D" + std::to_string(num), TType::Identifier, TFamily::Identifier);
  num++;
  return tok;
}

ScalarTypeInfo *create_scalar_type_info(ScalarType type, size_t size, bool is_integral = false) {
  auto info = type_info_alloc<ScalarTypeInfo>();
  info->scalar_type = type;
  info->size = size;
  info->is_integral = is_integral;
  return info;
}

int char_type() {
  static int type = global_create_type(TYPE_SCALAR, "char", create_scalar_type_info(TYPE_CHAR, 1, true));
  return type;
}
int bool_type() {
  static int type = global_create_type(TYPE_SCALAR, "bool", create_scalar_type_info(TYPE_BOOL, 1, true));
  return type;
}
int void_type() {
  static int type = global_create_type(TYPE_SCALAR, "void", create_scalar_type_info(TYPE_VOID, 0));
  return type;
}
int u64_type() {
  static int type = global_create_type(TYPE_SCALAR, "u64", create_scalar_type_info(TYPE_U64, 8, true));
  return type;
}
int u32_type() {
  static int type = global_create_type(TYPE_SCALAR, "u32", create_scalar_type_info(TYPE_U32, 4, true));
  return type;
}
int u16_type() {
  static int type = global_create_type(TYPE_SCALAR, "u16", create_scalar_type_info(TYPE_U16, 2, true));
  return type;
}
int u8_type() {
  static int type = global_create_type(TYPE_SCALAR, "u8", create_scalar_type_info(TYPE_U8, 1, true));
  return type;
}
int s64_type() {
  static int type = global_create_type(TYPE_SCALAR, "s64", create_scalar_type_info(TYPE_S64, 8, true));
  return type;
}
int s32_type() {
  static int type = global_create_type(TYPE_SCALAR, "s32", create_scalar_type_info(TYPE_S32, 4, true));
  return type;
}
int s16_type() {
  static int type = global_create_type(TYPE_SCALAR, "s16", create_scalar_type_info(TYPE_S16, 2, true));
  return type;
}
int s8_type() {
  static int type = global_create_type(TYPE_SCALAR, "s8", create_scalar_type_info(TYPE_S8, 1, true));
  return type;
}
int float32_type() {
  static int type = global_create_type(TYPE_SCALAR, "float32", create_scalar_type_info(TYPE_FLOAT, 4));
  return type;
}
int float64_type() {
  static int type = global_create_type(TYPE_SCALAR, "float64", create_scalar_type_info(TYPE_DOUBLE, 8));
  return type;
}
int int_type() {
  static int type = global_create_type(TYPE_SCALAR, "int", create_scalar_type_info(TYPE_S32, 4, true));
  return type;
}
int float_type() {
  static int type = global_create_type(TYPE_SCALAR, "float", create_scalar_type_info(TYPE_FLOAT, 4));
  return type;
}
int voidptr_type() {
  static int type = global_find_type_id(void_type(), {.extensions = {{TYPE_EXT_POINTER}}});
  return type;
}
int charptr_type() {
  static int type = global_find_type_id(char_type(), {.extensions = {{TYPE_EXT_POINTER}}});
  return type;
}

int &range_type() {
  static int type;
  return type;
}

int &string_type() {
  static int type;
  return type;
}

int &c_string_type() {
  static int type;
  return type;
}

bool get_function_type_parameter_signature(Type *type, std::vector<int> &out) {
  out.clear();
  if (!type->is_kind(TYPE_FUNCTION)) {
    return false;
  }
  auto info = (type->get_info()->as<FunctionTypeInfo>());
  for (int i = 0; i < info->params_len; ++i) {
    out.push_back(info->parameter_types[i]);
  }
  return true;
}

// TODO(Josh) 10/5/2024, 10:04:29 AM
// This should be a lot more strict. We can't define assignment operators
// because in C++ it requires a reference. a lot of these operators should be
// banned too, we don't need () for example, it just creates a bunch of
// complexity.
void emit_warnings_or_errors_for_operator_overloads(const TType type, SourceRange &range) {
  switch (type) {
    case TType::Range:
    case TType::Comma:
    case TType::Semi:
    case TType::Varargs:
    case TType::Directive:
    case TType::ColonEquals:
    case TType::Dollar:
    case TType::RParen:
    case TType::RBrace:
      throw_error("Operator overload not allowed", range);
    case TType::Arrow:
      throw_warning(WarningUseDotNotArrowOperatorOverload, "Operator overload: Use '.' instead of '->'", range);
      return;

    // Valid
    case TType::Assign:
    case TType::Add:
    case TType::Sub:
    case TType::Mul:
    case TType::Div:
    case TType::Modulo:
    case TType::LogicalNot:
    case TType::Not:
    case TType::Or:
    case TType::And:
    case TType::SHL:
    case TType::SHR:
    case TType::Xor:
    case TType::LogicalOr:
    case TType::LogicalAnd:
    case TType::LT:
    case TType::GT:
    case TType::EQ:
    case TType::NEQ:
    case TType::LE:
    case TType::GE:
    case TType::LParen:
    case TType::LBrace:
    case TType::Dot:
    case TType::Increment:
    case TType::Decrement:
    case TType::CompAdd:
    case TType::CompSub:
    case TType::CompMul:
    case TType::CompDiv:
    case TType::CompMod:
    case TType::CompAnd:
    case TType::CompOr:
    case TType::CompXor:
    case TType::CompSHL:
    case TType::CompSHR:
      break;
    default:
      throw_error(std::format("Invalid operator overload {}", TTypeToString(type)), range);
  }
}

void init_type_system() {
  // Signed integers

  {
    s64_type();
    s32_type();
    s16_type();
    s8_type();
  }

  // Unsigned integers
  {
    u64_type();
    u32_type();
    u16_type();
    u8_type();
  }

  // Floats
  {
    float64_type();
    float32_type();
  }

  // Other
  {
    char_type();
    bool_type();
    void_type();

    // Other
    // CLEANUP: alias these, don't generate new types.
    int_type();
    float_type();

    // TODO: declare type alias here.
    // auto id = charptr_type();
    // global_create_type_alias(id, "c_string");
  }
}
bool type_is_numerical(const Type *t) {
  if (!t->is_kind(TYPE_SCALAR))
    return false;
  return t->id == char_type() || t->id == float_type() || t->id == int_type() || t->id == s8_type() ||
         t->id == s16_type() || t->id == s32_type() || t->id == s64_type() || t->id == u8_type() ||
         t->id == u16_type() || t->id == u32_type() || t->id == u64_type() || t->id == float32_type() ||
         t->id == float64_type();
}

constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR)
    return false;

  auto from_info = (from->get_info()->as<ScalarTypeInfo>());
  auto to_info = (to->get_info()->as<ScalarTypeInfo>());

  // do not allow casting of float to integer implicitly
  if (!from_info->is_integral && to_info->is_integral) {
    return false;
  }

  return from_info->size <= to_info->size;
}

std::string TypeExtensions::to_string() const {
  std::stringstream ss;
  for (const auto ext : extensions) {
    switch (ext.type) {
      case TYPE_EXT_POINTER:
        ss << "*";
        break;
      case TYPE_EXT_ARRAY: {
        ss << "[]";
      } break;
      case TYPE_EXT_MAP: {
        ss << "[" << global_get_type(ext.key_type)->to_string() << "]";
      } break;
      case TYPE_EXT_FIXED_ARRAY:
        ss << "[" << ext.array_size << "]";
        break;
      case TYPE_EXT_INVALID:
        throw_error("Internal compiler error: extension type invalid", {});
        break;
    }
  }
  return ss.str();
}

int get_map_value_type(Type *map_type) {
  auto id = global_find_type_id(map_type->base_id, map_type->get_ext().without_back());
  return id;
}

int global_create_tuple_type(const std::vector<int> &types, const TypeExtensions &ext) {
  type_table.emplace_back(type_table.size(), TYPE_TUPLE);
  Type *type = &type_table.back();

  //! BUG: we should allow nested tuples;
  //! see some of the repros ("repro/14.ela" i think?)

  type->set_base(get_tuple_type_name(types));

  auto info = type_info_alloc<TupleTypeInfo>();
  info->types = types;

  type->set_info(info);
  type->set_ext(ext);
  info->scope = create_child(root_scope);
  return type->id;
}

InternedString get_tuple_type_name(const std::vector<int> &types) {
  std::stringstream ss;
  ss << "(";
  for (auto it = types.begin(); it != types.end(); ++it) {
    auto type = global_get_type(*it);

    ss << type->to_string();

    if (it != types.end() - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}
int Type::take_pointer_to() const {
  auto ext = this->extensions;
  ext.extensions.push_back({TYPE_EXT_POINTER});
  return global_find_type_id(id, ext);
}

int find_operator_overload(TType op, Type *type) {
  std::string op_str = TTypeToString(op);
  std::transform(op_str.begin(), op_str.end(), op_str.begin(), ::tolower);
  auto scope = type->get_info()->scope;
  if (!scope) return -1;
  // TODO: make a system for type checking against this.
  if (auto symbol = scope->local_lookup(op_str)) {
    if (symbol->is_function() && symbol->type_id > 0) {
      return symbol->type_id;
    }
  }
  return -1;
}
std::string mangled_type_args(const std::vector<int> &args) {
  std::string s;
  int i = 0;
  for (const auto &arg : args) {
    if (i > 0) {
      s += "_" + std::to_string(arg);
    } else {
      s += "$" + std::to_string(arg);
    }
    i++;
  }
  return s;
}
