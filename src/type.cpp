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
      auto info = type->get_info()->as<FunctionTypeInfo>();
      fun_tys.insert(fun_tys.end(), info->parameter_types, info->parameter_types + info->params_len);
      fun_tys.push_back(info->return_type);
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

std::string FunctionTypeInfo::to_string(const TypeExtensions &ext) const {
  std::stringstream ss;
  ss << "fn ";
  ss << ext.to_string() << ' ';
  ss << "(";
  for (int i = 0; i < params_len; ++i) {
    auto t = global_get_type(parameter_types[i]);
    ss << get_unmangled_name(t);
    if (i < params_len - 1) {
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

int global_find_function_type_id(const FunctionTypeInfo &info, const TypeExtensions &type_extensions) {
  const auto cmp_info_ptr = &info;

  for (const Type *type : type_table) {
    if (type->kind == TYPE_FUNCTION && type->get_ext() == type_extensions &&
        type->type_info_equals(cmp_info_ptr, TYPE_FUNCTION)) {
      return type->id;
    }
  }

  auto base = Type::INVALID_TYPE_ID;
  auto type_name = info.to_string();

  auto info_ptr = new (type_info_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);

  if (type_extensions.has_extensions()) {
    base = global_create_type(TYPE_FUNCTION, type_name, info_ptr, {});
  }

  return global_create_type(TYPE_FUNCTION, type_name, info_ptr, type_extensions, base);
}

int global_find_type_id(const int base, const TypeExtensions &type_extensions) {
  if (base < 0)
    return Type::INVALID_TYPE_ID;

  if (!type_extensions.has_extensions())
    return base;

  auto base_t = global_get_type(base);
  auto ext = type_extensions;

  [[likely]]
  if (base_t && base_t->base_id != Type::INVALID_TYPE_ID) {
    ext = base_t->get_ext().append(ext);
    base_t = global_get_type(base_t->base_id);
  }

  [[unlikely]]
  if (!base_t) {
    throw_error("INTERNAL_COMPILER_ERROR: global_find_type_id() reduced a type to nullptr when removing extensions",
                {});
  }

  for (int i = 0; i < type_table.size(); ++i) {
    auto type = global_get_type(i);
    if (type->equals(base_t->id, ext))
      return type->id;
  }

  // Base types have a seperate scope from the extended types now.

  TypeInfo *info = nullptr;
  switch (base_t->kind) {
    case TYPE_SCALAR: {
      info = new (type_info_alloc<ScalarTypeInfo>()) ScalarTypeInfo(*base_t->get_info()->as<ScalarTypeInfo>());
    } break;
    case TYPE_FUNCTION: {
      info = new (type_info_alloc<FunctionTypeInfo>()) FunctionTypeInfo(*base_t->get_info()->as<FunctionTypeInfo>());
    } break;
    case TYPE_STRUCT: {
      info = new (type_info_alloc<StructTypeInfo>()) StructTypeInfo(*base_t->get_info()->as<StructTypeInfo>());
    } break;
    case TYPE_ENUM: {
      info = new (type_info_alloc<EnumTypeInfo>()) EnumTypeInfo(*base_t->get_info()->as<EnumTypeInfo>());
    } break;
    case TYPE_TUPLE: {
      info = new (type_info_alloc<TupleTypeInfo>()) TupleTypeInfo(*base_t->get_info()->as<TupleTypeInfo>());
    } break;
    case TYPE_CHOICE: {
      info = new (type_info_alloc<ChoiceTypeInfo>()) ChoiceTypeInfo(*base_t->get_info()->as<ChoiceTypeInfo>());
    } break;
    case TYPE_INTERFACE: {
      info = new (type_info_alloc<InterfaceTypeInfo>()) InterfaceTypeInfo(*base_t->get_info()->as<InterfaceTypeInfo>());
    } break;
    case TYPE_DYN: {
      info = new (type_info_alloc<DynTypeInfo>()) DynTypeInfo(*base_t->get_info()->as<DynTypeInfo>());
    } break;
  }

  assert(info && "Copying type info for extended type failed");

  info->scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  info->scope->parent = base_t->get_info()->scope->parent;

  [[likely]] return global_create_type(base_t->kind, base_t->get_base(), info, ext, base_t->id);
}

int global_find_type_id(std::vector<int> &tuple_types, const TypeExtensions &type_extensions) {
  for (int i = 0; i < type_table.size(); ++i) {
    auto type = type_table[i];

    if (!type->is_kind(TYPE_TUPLE))
      continue;

    auto info = (type->get_info()->as<TupleTypeInfo>());

    if (info->types != tuple_types) {
      continue;
    }

    if (type->get_ext() == type_extensions) {
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

ConversionRule type_conversion_rule(const Type *from, const Type *to, const SourceRange &source_range) {
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
  // u16 -> u8 == explicit required.
  if (from->is_kind(TYPE_SCALAR) && from->get_ext().has_no_extensions() && to->is_kind(TYPE_SCALAR) &&
      to->get_ext().has_no_extensions()) {
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
  const auto from_is_scalar_ptr = from->get_ext().is_mut_pointer();
  const auto to_is_non_ptr_number = type_is_numerical(to) && to->get_ext().has_no_extensions();

  if (from_is_scalar_ptr && to_is_non_ptr_number) {
    return CONVERT_IMPLICIT;
  }

  // allow casting from number types to pointers explicitly
  if (type_is_numerical(from) && to->get_ext().is_pointer()) {
    return CONVERT_EXPLICIT;
  }

  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow in if (...)
  // cast all numerical types and pointers to booleans implicitly.
  if ((type_is_numerical(from) || from->get_ext().is_pointer()) && to->id == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  if (type_is_numerical(from) && to->id == bool_type()) {
    return CONVERT_EXPLICIT;
  }

  const auto operands_are_pointers = (from->get_ext().is_const_pointer() && to->get_ext().is_const_pointer()) ||
                                     (from->get_ext().is_mut_pointer() && to->get_ext().is_mut_pointer()) ||
                                     (from->get_ext().is_mut_pointer() && to->get_ext().is_const_pointer());

  bool elements_cast = false;
  if (operands_are_pointers) {
    auto from_elem = global_get_type(from->get_element_type());
    auto to_elem = global_get_type(to->get_element_type());
    auto conversion = type_conversion_rule(from_elem, to_elem);
    if (conversion == CONVERT_NONE_NEEDED ||
        conversion == CONVERT_IMPLICIT && (from->get_ext().pointer_depth() == to->get_ext().pointer_depth())) {
      elements_cast = true;
    }
  }

  const auto dest_is_u8_or_void_ptr =
      to->get_ext().is_pointer() && (to->base_id == u8_type() || to->base_id == void_type());

  const auto implicit_cast_void_pointer_to_any_ptr = to->get_ext().is_pointer() && (from->base_id == void_type());

  // Handling casting function pointers:
  /*
    Any function pointer type may implicitly cast to any other function pointer,
    so as long as all of it's parameters are equal or can implicitly convert to the target parameter,
    and the same for the return type.
  */
  if (operands_are_pointers && to->is_kind(TYPE_FUNCTION) && from->is_kind(TYPE_FUNCTION)) {
    // Wrong number of pointer extensions.
    if (to->get_ext().pointer_depth() != from->get_ext().pointer_depth()) {
      return CONVERT_PROHIBITED;
    }

    auto from_fn_info = from->get_info()->as<FunctionTypeInfo>();
    auto to_fn_info = to->get_info()->as<FunctionTypeInfo>();

    if (from_fn_info->params_len != to_fn_info->params_len) {
      return CONVERT_PROHIBITED;
    }

    auto &from_params = from_fn_info->parameter_types;
    auto &to_params = to_fn_info->parameter_types;

    for (int i = 0; i < from_fn_info->params_len; ++i) {
      auto from = global_get_type(from_params[i]);
      auto to = global_get_type(to_params[i]);
      auto rule = type_conversion_rule(from, to);
      if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
        return CONVERT_PROHIBITED;
      }
    }

    auto from_return = global_get_type(from_fn_info->return_type);
    auto to_return = global_get_type(to_fn_info->return_type);

    auto rule = type_conversion_rule(from_return, to_return);

    if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
      return CONVERT_PROHIBITED;
    }

    return CONVERT_IMPLICIT;
  }

  if (operands_are_pointers && (elements_cast || dest_is_u8_or_void_ptr || implicit_cast_void_pointer_to_any_ptr)) {
    return CONVERT_IMPLICIT;
  }

  // If we have a fixed array such as
  // char[5] and the argument takes void*
  // we check if char* can cast to void*, and if it can, we allow the cast.
  // this obviously works for char* too.
  {
    const auto implicit_fixed_array_to_ptr_cast = [&]() {
      // not array, return.
      if (!from->get_ext().is_fixed_sized_array())
        return false;

      if (!to->get_ext().is_pointer())
        return false;

      auto element_ty_ptr = global_get_type(global_get_type(from->get_element_type())->take_pointer_to(MUT));
      auto rule = type_conversion_rule(element_ty_ptr, to, source_range);

      return rule == CONVERT_IMPLICIT || rule == CONVERT_NONE_NEEDED;
    }();

    if (implicit_fixed_array_to_ptr_cast) {
      return CONVERT_IMPLICIT;
    }
  }

  // We basically have to allow *const to *mut to allow for pointer arithmetic,
  // you can't traverse a const array without this.
  if (from->get_ext().is_const_pointer() && to->get_ext().is_mut_pointer()) {
    return CONVERT_EXPLICIT;
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
  [[likely]]
  if (this->kind != kind)
    return false;

  [[unlikely]]
  if (kind == TypeKind::TYPE_FUNCTION) {
    auto finfo = static_cast<const FunctionTypeInfo *>(info);
    auto sinfo = static_cast<const FunctionTypeInfo *>(this->get_info());

    if (finfo->is_varargs != sinfo->is_varargs) {
      return false;
    }

    if (finfo->params_len != sinfo->params_len)
      return false;

    for (int i = 0; i < finfo->params_len; ++i)
      if (finfo->parameter_types[i] != sinfo->parameter_types[i]) {
        return false;
      }

    return finfo->return_type == sinfo->return_type;
  }

  [[likely]] return false;
}

std::string Type::to_string() const {
  switch (kind) {
    case TYPE_FUNCTION:
      return (get_info()->as<FunctionTypeInfo>())->to_string(extensions) + extensions.to_string();
    case TYPE_DYN:
      return "dyn " + get_unmangled_name(global_get_type(info->as<DynTypeInfo>()->interface_type));
    case TYPE_STRUCT:
    case TYPE_TUPLE:
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_CHOICE:
    case TYPE_INTERFACE:
      return get_unmangled_name(this);
      break;
  }
}

int global_create_interface_type(const InternedString &name, Scope *scope, std::vector<int> generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_INTERFACE));
  Type *type = type_table.back();
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  InterfaceTypeInfo *info = type_info_alloc<InterfaceTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  info->scope->name = name;
  return type->id;
}

int global_create_struct_type(const InternedString &name, Scope *scope, std::vector<int> generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_STRUCT));
  Type *type = type_table.back();
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  StructTypeInfo *info = type_info_alloc<StructTypeInfo>();
  info->scope = scope;
  info->scope->name = base;
  type->set_info(info);
  return type->id;
}

int global_create_tagged_union_type(const InternedString &name, Scope *scope, const std::vector<int> &generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_CHOICE));
  Type *type = type_table.back();
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  ChoiceTypeInfo *info = type_info_alloc<ChoiceTypeInfo>();
  info->scope = scope;
  info->scope->name = base;
  type->set_info(info);
  return type->id;
}

int global_create_enum_type(const InternedString &name, Scope *scope, bool is_flags, size_t element_type) {
  type_table.push_back(new Type(type_table.size(), TYPE_ENUM));
  Type *type = type_table.back();
  type->set_base(name);
  EnumTypeInfo *info = type_info_alloc<EnumTypeInfo>();
  info->is_flags = is_flags;
  info->scope = scope;
  info->scope->name = name;
  type->set_info(info);
  return type->id;
}

int global_create_type(TypeKind kind, const InternedString &name, TypeInfo *info, const TypeExtensions &extensions,
                       const int base_id) {
  type_table.push_back(new Type(type_table.size(), kind));
  auto type = type_table.back();
  type->base_id = base_id;
  type->set_ext(extensions);
  type->set_base(name);
  type->set_info(info);

  if (extensions.is_pointer() &&
      std::ranges::find(type->interfaces, is_pointer_interface()) == type->interfaces.end()) {
    type->interfaces.push_back(is_pointer_interface());
    if (extensions.is_const_pointer()) {
      type->interfaces.push_back(is_const_pointer_interface());
    } else {
      type->interfaces.push_back(is_mut_pointer_interface());
    }
  }

  if (!info->scope) {
    info->scope = create_child(nullptr);
  }
  info->scope->name = name;
  return type->id;
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
  if (!extensions.is_pointer() && !extensions.is_fixed_sized_array()) {
    throw_error(
        std::format("internal compiler error: called get_element_type() on a non pointer/array type\ngot type: \"{}\"",
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
int f32_type() {
  static int type = global_create_type(TYPE_SCALAR, "f32", create_scalar_type_info(TYPE_FLOAT, 4));
  return type;
}
int f64_type() {
  static int type = global_create_type(TYPE_SCALAR, "f64", create_scalar_type_info(TYPE_DOUBLE, 8));
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
    f64_type();
    f32_type();
  }

  // Other
  {
    bool_type();
    void_type();
  }

  is_const_pointer_interface();
  is_mut_pointer_interface();
  is_pointer_interface();

  is_tuple_interface();
  is_array_interface();
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
      case TYPE_EXT_POINTER_MUT:
        ss << "*mut";
        break;
      case TYPE_EXT_POINTER_CONST:
        ss << "*const";
        break;
      case TYPE_EXT_ARRAY:
        ss << "[]";
        break;
    }
  }
  return ss.str();
}

int global_create_tuple_type(const std::vector<int> &types) {
  type_table.push_back(new Type(type_table.size(), TYPE_TUPLE));
  Type *type = type_table.back();
  type->set_base(get_tuple_type_name(types));

  auto info = type_info_alloc<TupleTypeInfo>();
  info->types = types;

  type->set_info(info);
  info->scope = create_child(nullptr);

  // We do this for dot expressions that do tuple.1 etc.
  // Only in the base type.
  for (const auto [i, type] : types | std::ranges::views::enumerate) {
    info->scope->insert_variable(std::to_string(i), type, nullptr, MUT);
  }

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
int Type::take_pointer_to(bool is_mutable) const {
  auto ext = this->extensions;
  ext.extensions.push_back({is_mutable ? TYPE_EXT_POINTER_MUT : TYPE_EXT_POINTER_CONST});
  return global_find_type_id(base_id == -1 ? id : base_id, ext);
}

std::string get_operator_overload_name(TType op, OperationKind kind) {
  std::string output = "";
  switch (op) {
    case TType::LBrace:
      return "subscript";

    // Do we want this? might be useful for stuff like Array implementations etc.
    // However, it feels like bringing in the complexity of C++'s
    // copy constructor, copy assign, copy assign ref, move constructor , etc.
    case TType::Assign:

    // via interface Arithmetic
    case TType::Add:
    case TType::Sub: {
      if (kind == OPERATION_UNARY) {
        return "neg";
      }
    }
    case TType::Mul: {
      if (kind == OPERATION_UNARY) {
        return "deref";
      }
    }
    case TType::Div:
    case TType::Modulo:

    // via interface Logical
    case TType::LogicalNot:
    case TType::LogicalOr:
    case TType::LogicalAnd:

    // via interface Bitwise
    case TType::Not:
    case TType::Or:
    case TType::And:
    case TType::SHL:
    case TType::SHR:
    case TType::Xor:

    // via interface Compare.
    case TType::LT:
    case TType::GT:
    case TType::EQ:
    case TType::NEQ:
    case TType::LE:
    case TType::GE:

    // via interface Inc/Dec
    case TType::Increment:
    case TType::Decrement:

    // via interfaces CompArith/CompBitwise/CompLogical etc.
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
      output = TTypeToString(op);
    default:
      break;
  }
  std::transform(output.begin(), output.end(), output.begin(), ::tolower);
  return output;
}

int find_operator_overload(int mutability, Type *type, TType op, OperationKind kind) {
  if (!type) {
    return -1;
  }
  std::string op_str = get_operator_overload_name(op, kind);
  if (op_str.empty()) {
    return -1;
  }

  std::transform(op_str.begin(), op_str.end(), op_str.begin(), ::tolower);

  if (op_str == "subscript" && (type->get_ext().is_mut_pointer() || mutability == MUT)) {
    op_str = "subscript_mut";
  }

  auto scope = type->get_info()->scope;

  if (!scope)
    return Type::INVALID_TYPE_ID;

  if (auto symbol = scope->local_lookup(op_str)) {
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

int is_tuple_interface() {
  static int id = global_create_interface_type("IsTuple", create_child(nullptr), {});
  return id;
}

int is_array_interface() {
  static int id = global_create_interface_type("IsArray", create_child(nullptr), {});
  return id;
}

int is_pointer_interface() {
  static int id = global_create_interface_type("IsPointer", create_child(nullptr), {});
  return id;
}

int is_mut_pointer_interface() {
  static int id = global_create_interface_type("IsMutPointer", create_child(nullptr), {});
  return id;
}

int is_const_pointer_interface() {
  static int id = global_create_interface_type("IsConstPointer", create_child(nullptr), {});
  return id;
}

Type *ChoiceTypeInfo::get_variant_type(const InternedString &variant_name) const {
  int variant_index = get_variant_index(variant_name);
  if (variant_index == -1) {
    return nullptr;
  }
  return global_get_type(variants[variant_index].type);
}

int ChoiceTypeInfo::get_variant_index(const InternedString &variant_name) const {
  for (size_t i = 0; i < variants.size(); ++i) {
    if (variants[i].name == variant_name) {
      return i;
    }
  }
  return -1;
}

int StructTypeInfo::get_field_index(const InternedString &name) const {
  int index = 0;
  for (const auto &[sym_name, sym] : scope->symbols) {
    if (!sym.is_variable()) continue;
    if (sym_name == name) {
      return index;
    }
    ++index;
  }
  return -1;
}
