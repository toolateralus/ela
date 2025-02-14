#include "type.hpp"

#include <cstddef>
#include <ostream>
#include <ranges>
#include <sstream>
#include <vector>

#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"

std::vector<int> expand_function_types(std::vector<int> type_ids) {
  std::vector<int> output;
  for (auto type_id : type_ids) {
    auto type = global_get_type(type_id);
    if (type->is_kind(TYPE_FUNCTION)) {
      std::vector<int> fun_tys;
      auto &info = type->info.function;
      fun_tys.insert(fun_tys.end(), info.parameter_types.begin(), info.parameter_types.end());
      fun_tys.push_back(info.return_type);
      fun_tys = expand_function_types(fun_tys);
      output.insert(output.end(), fun_tys.begin(), fun_tys.end());
    } else {
      auto type = global_get_type(type_id);
      if (type->base_id != Type::INVALID_TYPE_ID) {
        type = global_get_type(type->base_id);
      }
      output.push_back(type_id);
    }
  }
  return output;
}

Type *global_get_type(const int id) {
  if (id < 0 || id > type_table.size())
    return nullptr;
  return type_table[id];
}

int global_find_function_type_id(const Function_Info &info, const Type_Metadata &meta) {
  auto type_info = Type_Info(info);
  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->kind != TYPE_FUNCTION)
      continue;
    const Type *type = type_table[i];
    if (type->type_info_equals(&type_info, TYPE_FUNCTION) && type->meta == meta) {
      return type->id;
    }
  }
  auto base = Type::INVALID_TYPE_ID;
  auto type_name = info.to_string();
  if (meta.has_extensions()) {
    base = global_create_type(TYPE_FUNCTION, type_name, std::move(type_info), {}, -1);
  }
  return global_create_type(TYPE_FUNCTION, type_name, std::move(type_info), meta, base);
}

int global_find_type_id(const int base, const Type_Metadata &meta) {
  if (base < 0)
    return Type::INVALID_TYPE_ID;

  if (!meta.has_extensions())
    return base;

  auto base_t = global_get_type(base);
  auto new_meta = meta;

  if (base_t && base_t->base_id != Type::INVALID_TYPE_ID) {
    new_meta = base_t->meta.append(meta);
    base_t = global_get_type(base_t->base_id);
  }

  if (!base_t) {
    throw_error("INTERNAL_COMPILER_ERROR: global_find_type_id() reduced a type to nullptr when removing extensions",
                {});
  }

  for (int i = 0; i < type_table.size(); ++i) {
    auto type = global_get_type(i);
    if (type->equals(base_t->id, meta))
      return type->id;
  }

  // Base types have a seperate scope from the extended types now.
  auto new_info = base_t->info;
  new_info.scope = {};
  return global_create_type(base_t->kind, base_t->base, std::move(new_info), meta, base_t->id);
}

int global_find_type_id(std::vector<int> &tuple_types, const Type_Metadata &type_extensions) {
  for (int i = 0; i < type_table.size(); ++i) {
    auto type = type_table[i];

    if (!type->is_kind(TYPE_TUPLE))
      continue;

    auto &info = type->info.tuple;

    if (info.types != tuple_types) {
      continue;
    }

    if (type->meta == type_extensions) {
      // Found a matching type with the same extensions. Return it.
      return type->id;
    } else {
      if (type->base_id != Type::INVALID_TYPE_ID) {
        return global_find_type_id(type->base_id, type_extensions);
      } else {
        return global_find_type_id(type->id, type_extensions);
      }
    }
  }

  // We didn't find the tuple type. Return a new one.
  auto base_id = global_create_tuple_type(tuple_types);
  return global_find_type_id(base_id, type_extensions);
}

ConversionRule type_conversion_rule(const Type *from, const Type *to, const Source_Range &source_range) {
  // just to make it more lax at call sites, we check here.
  if (!from || !to) {
    throw_error("internal compiler error: type was null when checking type "
                "conversion rules",
                source_range);
  }

  // * Same exact type. no cast needed.
  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  // implicitly upcast integer and float types.
  // u8 -> u16 -> u32 etc legal.
  // u16 -> u8 == implicit required.
  if (from->is_kind(TYPE_SCALAR) && from->meta.has_no_extensions() && to->is_kind(TYPE_SCALAR) &&
      to->meta.has_no_extensions()) {
    if (type_is_numerical(from) && type_is_numerical(to)) {
      if (numerical_type_safe_to_upcast(from, to)) {
        return CONVERT_IMPLICIT;
      }
      return CONVERT_EXPLICIT;
    } else if ((from->id == bool_type() && type_is_numerical(to)) ||
               to->id == bool_type() && type_is_numerical(from)) { // Convert booleans to number types explicitly
      // TODO(Josh) 1/13/2025, 3:07:06 PM :: Why did I have to add this? I could've sworn we had this working othrwise.
      // TODO: It's possible we just never noticed.
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic, from scalar type pointers, to numerical types.
  const auto from_is_scalar_ptr = from->meta.is_pointer();
  const auto to_is_non_ptr_number = type_is_numerical(to) && to->meta.has_no_extensions();

  if (from_is_scalar_ptr && to_is_non_ptr_number) {
    return CONVERT_IMPLICIT;
  }

  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow in if (...)
  // cast all numerical types and pointers to booleans implicitly.
  if ((type_is_numerical(from) || from->meta.is_pointer()) && to->id == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  if (type_is_numerical(from) && to->id == bool_type()) {
    return CONVERT_EXPLICIT;
  }

  // ! This needs to be re-evaluated. We should not be able to cast any pointer, to any other pointer.
  const auto implicit_ptr_cast = from->meta.is_pointer() && to->meta.is_pointer();

  // If we have a fixed array such as
  // char[5] and the argument takes void*
  // we check if char* can cast to void*, and if it can, we allow the cast.
  // this obviously works for char* too.
  const auto implicit_fixed_array_to_ptr_cast = [&]() {
    // not array, return.
    if (!from->meta.is_array())
      return false;

    if (!to->meta.is_pointer())
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
    if (from->is_kind(TYPE_ENUM) && from->meta.has_no_extensions()) {
      auto enum_info = from->info.$enum;
      return type_conversion_rule(global_get_type(enum_info.element_type), to, source_range);
    }

    // TODO: do a runtime bounds check on explicit casting of an integer to an enum type?
    // You can get segfaults from that easily.
    if (to->is_kind(TYPE_ENUM) && to->meta.has_no_extensions()) {
      auto enum_info = to->info.$enum;
      return type_conversion_rule(from, global_get_type(enum_info.element_type), source_range);
    }
  }

  // * if the type extensions are equal, return the conversion rule for the bases.
  {
    // this allows int[] to cast to s8[] etc;
    // this kind of behaviour perhaps will cause problems later down the line, the more C like we become,
    // we can't simple reinterpret a u64[] dynamic array as a s8[]
    if (from->meta.has_extensions() && to->meta.has_extensions() &&
        from->meta.extensions.back() == to->meta.extensions.back()) {
      auto from_base = global_get_type(global_find_type_id(from->base_id, from->meta.without_back()));
      auto to_base = global_get_type(global_find_type_id(to->base_id, to->meta.without_back()));
      return type_conversion_rule(from_base, to_base, source_range);
    }
  }

  return CONVERT_PROHIBITED;
}

bool Type::type_info_equals(const Type_Info *info, Type_Kind kind) const {
  if (this->kind != kind)
    return false;
  if (kind == Type_Kind::TYPE_FUNCTION) {
    auto finfo = info->function;
    auto sinfo = this->info.function;

    if (finfo.is_varargs != sinfo.is_varargs) {
      return false;
    }

    bool params_eq = finfo.parameter_types == sinfo.parameter_types;

    if (!params_eq)
      return false;

    return finfo.return_type == sinfo.return_type;
  }
  return false;
}

bool Type::equals(const int base, const Type_Metadata &type_extensions) const {
  auto isBaseIdEqual = base_id == base;
  auto isTypeExtensionEqual = type_extensions == meta;
  return isBaseIdEqual && isTypeExtensionEqual;
}

bool Type_Metadata::equals(const Type_Metadata &other) const {
  if (extensions != other.extensions)
    return false;
  return true;
}

std::string Type::to_string() const {
  switch (kind) {
    case TYPE_FUNCTION:
      return info.function.to_string(meta) + meta.to_string();
    case TYPE_STRUCT:
    case TYPE_TUPLE:
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_INTERFACE:
      return get_unmangled_name(this);
      break;
  }
}

int global_create_interface_type(const Interned_String &name, Scope scope,  AST *node, std::vector<int> generic_args) {
  Type_Info type_info{Interface_Info{}};
  type_info.scope = scope;
  Type *type = type_table.emplace_back(new Type(name, TYPE_INTERFACE, std::move(type_info), generic_args));
  type->declaring_node = node;
  return type->id;
}

int global_create_struct_type(const Interned_String &name, Scope scope, AST * node, std::vector<int> generic_args) {
  Type_Info info{Struct_Info{}};
  info.scope = scope;
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  Type *type = type_table.emplace_back(new Type(base, TYPE_STRUCT, std::move(info), generic_args));
  type->declaring_node = node;
  return type->id;
}

int global_create_enum_type(const Interned_String &name, Scope scope, AST *node, bool is_flags, size_t element_type) {
  Type_Info info(Enum_Info{});
  info.$enum.is_flags = is_flags;
  info.scope = scope;
  Type *type = type_table.emplace_back(new Type(name, TYPE_ENUM, std::move(info)));
  type->declaring_node = node;
  return type->id;
}
int global_create_type(Type_Kind kind, const Interned_String &name, Type_Info &&info, const Type_Metadata &meta,
                       const int base_id) {
  Type *type = type_table.emplace_back(new Type(name, kind, std::move(info)));
  type->base_id = base_id;
  type->meta = meta;
  return type->id;
}
Interned_String get_function_typename(AST *decl) {
  std::stringstream ss;
  auto return_type = decl->function.return_type;
  ss << "fn ";
  ss << "(";
  size_t i = 0;
  for (const auto &param : decl->function.parameters) {
    ss << global_get_type(param.resolved_type)->to_string();
    if (i != decl->function.parameters.size() - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  ss << " -> " << global_get_type(return_type->resolved_type)->to_string();
  return ss.str();
}

int Type::get_element_type() const {
  if (!meta.is_pointer() && !meta.is_array()) {
    throw_error(
        std::format("internal compiler error: called get_element_type() on a non pointer/array type\ngot type: \"{}\"",
                    to_string()),
        {});
  }
  auto extensions = this->meta.without_back();
  if (is_kind(TYPE_TUPLE)) {
    auto types = info.tuple.types; // silly copy for const method.
    return global_find_type_id(types, extensions);
  } else
    return global_find_type_id(base_id, extensions);
}

// used for anonymous structs etc.
Token get_unique_identifier() {
  static int num = 0;
  auto tok = Token({}, "__anon_D" + std::to_string(num), Token_Type::Identifier, TFamily::Identifier);
  num++;
  return tok;
}

Scalar_Info create_scalar_type_info(ScalarType type, size_t size, bool is_integral = false) {
  Scalar_Info info;
  info.scalar_type = type;
  info.size = size;
  info.is_integral = is_integral;
  return info;
}

int bool_type() {
  static int type = global_create_type(TYPE_SCALAR, "bool", create_scalar_type_info(TYPE_BOOL, 1, true), {}, -1);
  return type;
}
int void_type() {
  static int type = global_create_type(TYPE_SCALAR, "void", create_scalar_type_info(TYPE_VOID, 0), {}, -1);
  return type;
}
int u64_type() {
  static int type = global_create_type(TYPE_SCALAR, "u64", create_scalar_type_info(TYPE_U64, 8, true), {}, -1);
  return type;
}
int u32_type() {
  static int type = global_create_type(TYPE_SCALAR, "u32", create_scalar_type_info(TYPE_U32, 4, true), {}, -1);
  return type;
}
int u16_type() {
  static int type = global_create_type(TYPE_SCALAR, "u16", create_scalar_type_info(TYPE_U16, 2, true), {}, -1);
  return type;
}
int u8_type() {
  static int type = global_create_type(TYPE_SCALAR, "u8", create_scalar_type_info(TYPE_U8, 1, true), {}, -1);
  return type;
}
int s64_type() {
  static int type = global_create_type(TYPE_SCALAR, "s64", create_scalar_type_info(TYPE_S64, 8, true), {}, -1);
  return type;
}
int s32_type() {
  static int type = global_create_type(TYPE_SCALAR, "s32", create_scalar_type_info(TYPE_S32, 4, true), {}, -1);
  return type;
}
int s16_type() {
  static int type = global_create_type(TYPE_SCALAR, "s16", create_scalar_type_info(TYPE_S16, 2, true), {}, -1);
  return type;
}
int s8_type() {
  static int type = global_create_type(TYPE_SCALAR, "s8", create_scalar_type_info(TYPE_S8, 1, true), {}, -1);
  return type;
}
int f32_type() {
  static int type = global_create_type(TYPE_SCALAR, "f32", create_scalar_type_info(TYPE_FLOAT, 4), {}, -1);
  return type;
}
int f64_type() {
  static int type = global_create_type(TYPE_SCALAR, "f64", create_scalar_type_info(TYPE_DOUBLE, 8), {}, -1);
  return type;
}

int voidptr_type() {
  static int type = global_find_type_id(void_type(), {.extensions = {{TYPE_EXT_POINTER}}});
  return type;
}

bool get_function_type_parameter_signature(Type *type, std::vector<int> &out) {
  out.clear();
  if (!type->is_kind(TYPE_FUNCTION)) {
    return false;
  }
  auto info = type->info.function;
  for (const auto param: info.parameter_types) {
    out.push_back(param);
  }
  return true;
}

// TODO(Josh) 10/5/2024, 10:04:29 AM
// This should be a lot more strict. We can't define assignment operators
// because in C++ it requires a reference. a lot of these operators should be
// banned too, we don't need () for example, it just creates a bunch of
// complexity.
void emit_warnings_or_errors_for_operator_overloads(const Token_Type type, Source_Range &range) {
  switch (type) {
    case Token_Type::Range:
    case Token_Type::Comma:
    case Token_Type::Semi:
    case Token_Type::Varargs:
    case Token_Type::Directive:
    case Token_Type::ColonEquals:
    case Token_Type::RParen:
    case Token_Type::RBrace:
      throw_error("Operator overload not allowed", range);
    case Token_Type::Arrow:
      throw_warning(WarningUseDotNotArrowOperatorOverload, "Operator overload: Use '.' instead of '->'", range);
      return;

    // Valid
    case Token_Type::Assign:
    case Token_Type::Add:
    case Token_Type::Sub:
    case Token_Type::Mul:
    case Token_Type::Div:
    case Token_Type::Modulo:
    case Token_Type::LogicalNot:
    case Token_Type::Not:
    case Token_Type::Or:
    case Token_Type::And:
    case Token_Type::SHL:
    case Token_Type::SHR:
    case Token_Type::Xor:
    case Token_Type::LogicalOr:
    case Token_Type::LogicalAnd:
    case Token_Type::LT:
    case Token_Type::GT:
    case Token_Type::EQ:
    case Token_Type::NEQ:
    case Token_Type::LE:
    case Token_Type::GE:
    case Token_Type::LParen:
    case Token_Type::LBrace:
    case Token_Type::Dot:
    case Token_Type::Increment:
    case Token_Type::Decrement:
    case Token_Type::CompAdd:
    case Token_Type::CompSub:
    case Token_Type::CompMul:
    case Token_Type::CompDiv:
    case Token_Type::CompMod:
    case Token_Type::CompAnd:
    case Token_Type::CompOr:
    case Token_Type::CompXor:
    case Token_Type::CompSHL:
    case Token_Type::CompSHR:
      break;
    default:
      throw_error(std::format("Invalid operator overload {}", Token_Type_To_String(type)), range);
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
    f64_type();
    f32_type();
  }

  // Other
  {
    bool_type();
    void_type();
  }
}
bool type_is_numerical(const Type *t) {
  if (!t->is_kind(TYPE_SCALAR))
    return false;
  return t->id == s32_type() || t->id == s8_type() || t->id == s16_type() || t->id == s32_type() ||
         t->id == s64_type() || t->id == u8_type() || t->id == u16_type() || t->id == u32_type() ||
         t->id == u64_type() || t->id == f32_type() || t->id == f64_type();
}

constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR)
    return false;

  auto from_info = from->info.scalar;
  auto to_info = to->info.scalar;

  // do not allow casting of float to integer implicitly
  if (!from_info.is_integral && to_info.is_integral) {
    return false;
  }

  return from_info.size <= to_info.size;
}

std::string Type_Metadata::to_string() const {
  std::stringstream ss;
  for (const auto meta : extensions) {
    switch (meta.type) {
      case TYPE_EXT_POINTER:
        ss << "*";
        break;
      case TYPE_EXT_ARRAY:
        ss << "[]";
        break;
      case TYPE_EXT_INVALID:
        throw_error("internal compiler error: extension type invalid", {});
        break;
    }
  }
  return ss.str();
}

int global_create_tuple_type(const std::vector<int> &types) {
  Type_Info info{Tuple_Info{}};
  // We do this for dot expressions that do tuple.1 etc.
  // Only in the base type.
  for (const auto [i, type] : types | std::ranges::views::enumerate) {
    info.scope.insert(Symbol(std::to_string(i), type, nullptr, SYMBOL_IS_VARIABLE));
  }
  info.tuple.types = types;
  type_table.push_back(new Type(get_tuple_type_name(types), TYPE_TUPLE, std::move(info)));
  Type *type = type_table.back();
  return type->id;
}

Interned_String get_tuple_type_name(const std::vector<int> &types) {
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
  auto meta = this->meta;
  meta.extensions.push_back({TYPE_EXT_POINTER});
  return global_find_type_id(base_id == -1 ? id : base_id, meta);
}

std::string get_operator_overload_name(Token_Type op, OperationKind kind) {
  std::string output = "";
  switch (op) {
    case Token_Type::LBrace:
      return "subscript";

    // Do we want this? might be useful for stuff like Array implementations etc.
    // However, it feels like bringing in the complexity of C++'s
    // copy constructor, copy assign, copy assign ref, move constructor , etc.
    case Token_Type::Assign:

    // via interface Arithmetic
    case Token_Type::Add:
    case Token_Type::Sub: {
      if (kind == OPERATION_UNARY) {
        return "neg";
      }
    }
    case Token_Type::Mul: {
      if (kind == OPERATION_UNARY) {
        return "deref"; // ?? do we want this?
      }
    }
    case Token_Type::Div:
    case Token_Type::Modulo:

    // via interface Logical
    case Token_Type::LogicalNot:
    case Token_Type::LogicalOr:
    case Token_Type::LogicalAnd:

    // via interface Bitwise
    case Token_Type::Not:
    case Token_Type::Or:
    case Token_Type::And:
    case Token_Type::SHL:
    case Token_Type::SHR:
    case Token_Type::Xor:

    // via interface Compare.
    case Token_Type::LT:
    case Token_Type::GT:
    case Token_Type::EQ:
    case Token_Type::NEQ:
    case Token_Type::LE:
    case Token_Type::GE:

    // via interface Inc/Dec
    case Token_Type::Increment:
    case Token_Type::Decrement:

    // via interfaces CompArith/CompBitwise/CompLogical etc.
    case Token_Type::CompAdd:
    case Token_Type::CompSub:
    case Token_Type::CompMul:
    case Token_Type::CompDiv:
    case Token_Type::CompMod:
    case Token_Type::CompAnd:
    case Token_Type::CompOr:
    case Token_Type::CompXor:
    case Token_Type::CompSHL:
    case Token_Type::CompSHR:
      output = Token_Type_To_String(op);
    default:
      break;
  }
  std::transform(output.begin(), output.end(), output.begin(), ::tolower);
  return output;
}

int find_operator_overload(Token_Type op, Type *type, OperationKind kind) {
  if (!type) {
    return -1;
  }
  std::string op_str = get_operator_overload_name(op, kind);
  if (op_str.empty()) {
    return -1;
  }
  std::transform(op_str.begin(), op_str.end(), op_str.begin(), ::tolower);
  auto &scope = type->info.scope;

  if (auto symbol = scope.lookup(op_str)) {
    if (symbol->is_function() && symbol->type_id > 0) {
      return symbol->type_id;
    }
  }
  return Type::INVALID_TYPE_ID;
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
std::string Function_Info::to_string(const Type_Metadata &meta) const {
  std::stringstream ss;
  ss << "fn ";
  ss << meta.to_string() << ' ';
  ss << "(";
  size_t i = 0;
  for (const auto &param : parameter_types) {
    auto t = global_get_type(param);
    ss << get_unmangled_name(t);
    if (i < parameter_types.size() - 1) {
      ss << ", ";
    }
  }

  if (is_varargs)
    ss << ", ...)";
  else
    ss << ')';

  ss << " -> " << get_unmangled_name(global_get_type(return_type));

  return ss.str();
}
std::string Function_Info::to_string() const {
  std::stringstream ss;
  ss << "fn ";
  ss << "(";
  size_t i = 0;
  for (const auto &param : parameter_types) {
    auto t = global_get_type(param);
    ss << t->to_string();
    if (i < parameter_types.size() - 1) {
      ss << ", ";
    }
  }
  if (is_varargs)
    ss << ", ...)";
  else
    ss << ')';

  ss << " -> " << global_get_type(return_type)->base.get_str();

  return ss.str();
}
bool Type::implements(const Interned_String &interface) {
  for (auto id : interfaces) {
    auto iface = global_get_type(id);
    std::string iface_base_str = iface->base.get_str();
    std::string interface_str = interface.get_str();
    auto pos = iface_base_str.find('$');
    if (pos != std::string::npos) {
      iface_base_str = iface_base_str.substr(0, pos);
    }
    if (iface_base_str == interface_str) {
      return true;
    }
  }
  return false;
}
