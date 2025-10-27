#include "type.hpp"

#include <cstddef>
#include <sstream>
#include <vector>

#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"

Type *Type::UNRESOLVED_GENERIC = reinterpret_cast<Type *>(1);

/*
  There's two copies of this, one for function pointers, and one for regular function types.
  We probably don't need this anymore, also this fails anyway

  we get this for:
    function :: fn(s32) {}
    fn := &function;
    typeof(fn) == "fn (s32) -> void*";
    the void* is wrong obviously, the * gets appended twice.
*/
std::string FunctionTypeInfo::to_string(const TypeExtensions &ext) const {
  std::stringstream ss;

  ss << "fn(";
  for (size_t i = 0; i < params_len; ++i) {
    auto t = parameter_types[i];
    ss << get_unmangled_name(t);
    if (i < params_len - 1) ss << ", ";
  }

  if (is_varargs)
    ss << ", ...)";
  else
    ss << ")";

  ss << " -> " << get_unmangled_name(return_type);

  // Wrap with pointer/array extensions (outermost → innermost)
  return extensions_to_string(ext, ss.str());
}

std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << "fn ";
  ss << "(";

  for (size_t i = 0; i < params_len; ++i) {
    auto t = parameter_types[i];
    ss << t->to_string();
    if (i < params_len - 1) {
      ss << ", ";
    }
  }

  if (is_varargs)
    ss << ", ...)";
  else
    ss << ')';

  ss << " -> " << return_type->basename.str();

  return ss.str();
}

Type *global_find_function_type_id(const FunctionTypeInfo &info, const TypeExtensions &type_extensions) {
  const auto cmp_info_ptr = &info;

  for (Type *type : function_type_table) {
    if (type->kind == TYPE_FUNCTION && type->extensions_equals(type_extensions) &&
        type->type_info_equals(cmp_info_ptr, TYPE_FUNCTION)) {
      return type;
    }
  }

  auto base = Type::INVALID_TYPE;
  auto type_name = info.to_string();

  auto info_ptr = new (type_info_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);

  if (type_extensions.size()) {
    base = global_create_type(TYPE_FUNCTION, type_name, info_ptr, {});
  }

  auto type = global_create_type(TYPE_FUNCTION, type_name, info_ptr, type_extensions, base);

  function_type_table.push_back(type);

  return type;
}

/// TODO:
// Interned type extensions; intern all new type extensions to a hash map,
// then just compare pointers. much cheaper, and also storing type extensions as a pointer will be cheaper
// than how we constantly copy them around currently.

// TODO:
// use a hashmap for the global type table. we can have a TypeKey{ ptr, extensions } and a custom
// hash function that will drastically improve type lookup times.

Type *global_find_type_id(Type *base_t, const TypeExtensions &type_extensions) {
  if (!type_is_valid(base_t)) return Type::INVALID_TYPE;

  if (!type_extensions.size()) return base_t;

  auto extensions_copy = type_extensions;  // copy vector

  if (base_t && type_is_valid(base_t->base_type)) {
    extensions_copy = base_t->append_extension(extensions_copy);  // copies base_t's std::vector<TypeExtension>
    base_t = base_t->base_type;
  }

  // expensive loop
  // expensive loop
  for (const auto &type : type_table) {
    if (type->base_type == base_t && extensions_copy == type->extensions) {
      return type;
    }
  }

  // Base types have a seperate scope from the extended types now.

  TypeInfo *info = nullptr;
  switch (base_t->kind) {
    case TYPE_SCALAR: {
      info = new (type_info_alloc<ScalarTypeInfo>()) ScalarTypeInfo(*base_t->info->as<ScalarTypeInfo>());
    } break;
    case TYPE_FUNCTION: {
      info = new (type_info_alloc<FunctionTypeInfo>()) FunctionTypeInfo(*base_t->info->as<FunctionTypeInfo>());
    } break;
    case TYPE_STRUCT: {
      info = new (type_info_alloc<StructTypeInfo>()) StructTypeInfo(*base_t->info->as<StructTypeInfo>());
    } break;
    case TYPE_ENUM: {
      info = new (type_info_alloc<EnumTypeInfo>()) EnumTypeInfo(*base_t->info->as<EnumTypeInfo>());
    } break;
    case TYPE_TUPLE: {
      info = new (type_info_alloc<TupleTypeInfo>()) TupleTypeInfo(*base_t->info->as<TupleTypeInfo>());
    } break;
    case TYPE_CHOICE: {
      info = new (type_info_alloc<ChoiceTypeInfo>()) ChoiceTypeInfo(*base_t->info->as<ChoiceTypeInfo>());
    } break;
    case TYPE_TRAIT: {
      info = new (type_info_alloc<TraitTypeInfo>()) TraitTypeInfo(*base_t->info->as<TraitTypeInfo>());
    } break;
    case TYPE_DYN: {
      info = new (type_info_alloc<DynTypeInfo>()) DynTypeInfo(*base_t->info->as<DynTypeInfo>());
    } break;
  }

  // not bad- arena allocator.
  info->scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
  info->scope->parent = base_t->info->scope->parent;

  // rare (comparatively) case-- have to create a new extended type.
  return global_create_type(base_t->kind, base_t->basename, info, extensions_copy, base_t);
}

Type *global_find_type_id(std::vector<Type *> &tuple_types, const TypeExtensions &type_extensions) {
  for (auto &type : type_table) {
    if (!type->is_kind(TYPE_TUPLE)) continue;

    auto info = (type->info->as<TupleTypeInfo>());

    if (info->types != tuple_types) {
      continue;
    }

    // Found a matching type with the same extensions. Return it.
    if (type->extensions_equals(type_extensions)) {
      return type;
    } else {
      if (type_is_valid(type->base_type)) {
        return global_find_type_id(type->base_type, type_extensions);
      } else {
        return global_find_type_id(type, type_extensions);
      }
    }
  }

  // We didn't find the tuple type. Return a new one.
  auto base_type = global_create_tuple_type(tuple_types);
  return global_find_type_id(base_type, type_extensions);
}

struct StructuralTypingRule {
  ConversionRule conversion_rule = CONVERT_PROHIBITED;
  bool is_applicable = false;
  static StructuralTypingRule Inapplicable() { return {}; }
  static StructuralTypingRule Applicable(ConversionRule r) { return {r, true}; }
};

StructuralTypingRule get_structural_typing_rule(const Type *from, const Type *to) {
  const auto from_info = from->info->as<StructTypeInfo>();
  const auto to_info = to->info->as<StructTypeInfo>();

  const bool from_is_structural = from_info->is_structural;
  const bool to_is_structural = to_info->is_structural;

  // both are nominal. no structural typing can occur, it's too unsafe.
  if (!to_is_structural && !from_is_structural) {
    return StructuralTypingRule::Inapplicable();
  }

  const auto &to_members = to_info->members;
  const auto &from_members = from_info->members;

  if (to_members.size() != from_members.size()) {
    return StructuralTypingRule::Inapplicable();
  }

  if (!to->extensions_equals(from->extensions)) {
    return StructuralTypingRule::Inapplicable();
  }

  for (size_t i = 0; i < to_members.size(); ++i) {
    const auto &to_member = to_members[i];
    const auto &from_member = from_members[i];
    if (to_member.type != from_member.type) {
      return StructuralTypingRule::Inapplicable();
    }
  }

  return StructuralTypingRule::Applicable(to_is_structural ? CONVERT_IMPLICIT : CONVERT_EXPLICIT);
}

ConversionRule type_conversion_rule(const Type *from, const Type *to, const Span &range) {
  // just to make it more lax at call sites, we check here.
  if (!from || !to) {
    throw_error("internal compiler error: type was null when checking type conversion rules", range);
  }

  // * Same exact type. no cast needed.
  if (from == to) return CONVERT_NONE_NEEDED;

  // implicitly upcast integer and float types.
  // u8 -> u16 -> u32 etc legal.
  // u16 -> u8 == implicit required.
  if (from->is_kind(TYPE_SCALAR) && from->has_no_extensions() && to->is_kind(TYPE_SCALAR) && to->has_no_extensions()) {
    if (type_is_numeric(from) && type_is_numeric(to)) {
      if (numerical_type_safe_to_upcast(from, to)) {
        return CONVERT_IMPLICIT;
      }
      return CONVERT_EXPLICIT;

      // !!!! I ADDED THE PARENTHESIS ON THE SECOND EXPRESSION HERE :: !!!!!
      // ! This may cause bugs!
    } else if ((from == bool_type() && type_is_numeric(to)) ||
               (to == bool_type() && type_is_numeric(from))) {  // Convert booleans to number types explicitly
      // TODO(Josh) 1/13/2025, 3:07:06 PM :: Why did I have to add this? I could've sworn we had this working othrwise.
      // TODO: It's possible we just never noticed.
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic, from scalar type pointers, to numerical types.
  const auto from_is_scalar_ptr = from->is_mut_pointer();
  const auto to_is_non_ptr_number = type_is_numeric(to) && to->has_no_extensions();

  if (from_is_scalar_ptr && to_is_non_ptr_number) {
    return CONVERT_IMPLICIT;
  }

  // allow casting from number types to pointers explicitly
  if (type_is_numeric(from) && to->is_pointer()) {
    return CONVERT_EXPLICIT;
  }

  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow in if (...)
  // cast all numerical types and pointers to booleans implicitly.
  if ((type_is_numeric(from) || from->is_pointer()) && to == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  if (type_is_numeric(from) && to == bool_type()) {
    return CONVERT_EXPLICIT;
  }

  const auto operands_are_pointers = (from->is_const_pointer() && to->is_const_pointer()) ||
                                     (from->is_mut_pointer() && to->is_mut_pointer()) ||
                                     (from->is_mut_pointer() && to->is_const_pointer());

  bool elements_cast = false;
  if (operands_are_pointers) {
    auto from_elem = from->get_element_type();
    auto to_elem = to->get_element_type();
    auto conversion = type_conversion_rule(from_elem, to_elem);
    if (conversion == CONVERT_NONE_NEEDED || (conversion == CONVERT_IMPLICIT && (from->pointer_depth() == to->pointer_depth()))) {
      elements_cast = true;
    }
  }

  const auto dest_is_u8_or_void_ptr = to->is_pointer() && (to->base_type == u8_type() || to->base_type == void_type());
  const auto implicit_cast_void_pointer_to_any_ptr = to->is_pointer() && (from->base_type == void_type());

  // Handling casting function pointers:
  /*
    Any function pointer type may implicitly cast to any other function pointer,
    so as long as all of it's parameters are equal or can implicitly convert to the target parameter,
    and the same for the return type.

    !this needs to be fixed for correct sizing of parameters.
  */
  if (operands_are_pointers && to->is_kind(TYPE_FUNCTION) && from->is_kind(TYPE_FUNCTION)) {
    // Wrong number of pointer extensions.
    if (to->pointer_depth() != from->pointer_depth()) {
      return CONVERT_PROHIBITED;
    }

    auto from_fn_info = from->info->as<FunctionTypeInfo>();
    auto to_fn_info = to->info->as<FunctionTypeInfo>();

    if (from_fn_info->params_len != to_fn_info->params_len) {
      return CONVERT_PROHIBITED;
    }

    auto &from_params = from_fn_info->parameter_types;
    auto &to_params = to_fn_info->parameter_types;

    for (size_t i = 0; i < from_fn_info->params_len; ++i) {
      auto from = from_params[i];
      auto to = to_params[i];
      auto rule = type_conversion_rule(from, to);
      if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
        return CONVERT_PROHIBITED;
      }
    }

    auto from_return = from_fn_info->return_type;
    auto to_return = to_fn_info->return_type;

    auto rule = type_conversion_rule(from_return, to_return);

    if (rule != CONVERT_NONE_NEEDED && rule != CONVERT_IMPLICIT) {
      return CONVERT_PROHIBITED;
    }

    return CONVERT_IMPLICIT;
  }

  if (operands_are_pointers && (elements_cast || dest_is_u8_or_void_ptr || implicit_cast_void_pointer_to_any_ptr)) {
    return CONVERT_IMPLICIT;
  } else if (operands_are_pointers && !elements_cast) {
    return CONVERT_EXPLICIT;
  }

  // If we have a fixed array such as
  // char[5] and the argument takes void*
  // we check if char* can cast to void*, and if it can, we allow the cast.
  // this obviously works for char* too.
  {
    const auto implicit_fixed_array_to_ptr_cast = [&]() {
      // not array, return.
      if (!from->is_fixed_sized_array()) return false;

      if (!to->is_pointer()) return false;

      auto element_ty_ptr = from->get_element_type()->take_pointer_to(MUT);
      auto rule = type_conversion_rule(element_ty_ptr, to, range);

      return rule == CONVERT_IMPLICIT || rule == CONVERT_NONE_NEEDED;
    }();

    if (implicit_fixed_array_to_ptr_cast) {
      return CONVERT_IMPLICIT;
    }
  }

  // We basically have to allow *const to *mut to allow for pointer arithmetic,
  // you can't traverse a const array without this.
  if (from->is_const_pointer() && to->is_mut_pointer()) {
    return CONVERT_EXPLICIT;
  }

  // TODO: this allows two way casting of enums to their underlying type. Is this what we want?
  // It kinda ruins the safety aspect of having strongly typed enums.
  {
    if (from->is_kind(TYPE_ENUM) && from->has_no_extensions()) {
      auto enum_info = (from->info->as<EnumTypeInfo>());
      return type_conversion_rule(enum_info->underlying_type, to, range);
    }

    // TODO: do a runtime bounds check on explicit casting of an integer to an enum type?
    // You can get segfaults from that easily.
    if (to->is_kind(TYPE_ENUM) && to->has_no_extensions()) {
      auto enum_info = (to->info->as<EnumTypeInfo>());
      return type_conversion_rule(from, enum_info->underlying_type, range);
    }
  }

  // if both are structs, then we might use structural typing.
  if (from->is_kind(TYPE_STRUCT) && to->is_kind(TYPE_STRUCT)) {
    auto rule = get_structural_typing_rule(from, to);
    if (rule.is_applicable) {
      return rule.conversion_rule;
    }
  }

  // * if the type extensions are equal, return the conversion rule for the bases.
  {
    // this allows int[] to cast to s8[] etc;
    // this kind of behaviour perhaps will cause problems later down the line, the more C like we become,
    // we can't simple reinterpret a u64[] dynamic array as a s8[]
    if (from->has_extensions() && to->has_extensions() && from->extensions.back() == to->extensions.back()) {
      auto from_base = global_find_type_id(from->base_type, from->extensions_without_back());
      auto to_base = global_find_type_id(to->base_type, to->extensions_without_back());
      return type_conversion_rule(from_base, to_base, range);
    }
  }

  return CONVERT_PROHIBITED;
}

bool Type::type_info_equals(const TypeInfo *info, TypeKind kind) const {
  if (this->kind != kind || kind != TYPE_FUNCTION) return false;

  auto finder_info = static_cast<const FunctionTypeInfo *>(info);
  auto self_info = static_cast<const FunctionTypeInfo *>(this->info);

  if (finder_info->return_type != self_info->return_type || finder_info->is_varargs != self_info->is_varargs ||
      finder_info->params_len != self_info->params_len) {
    return false;
  }

  return std::memcmp(finder_info->parameter_types, self_info->parameter_types, finder_info->params_len * sizeof(Type *)) == 0;
}

/*
  Get a display name of this type, in a readable manner to users.
  Unmangles.
*/
std::string Type::to_string() const {
  if (basename.str_ptr->starts_with("__anon_D")) {
    return extensions_to_string(extensions, "(anonymous type)");
  }

  switch (kind) {
    case TYPE_FUNCTION:
      return (info->as<FunctionTypeInfo>())->to_string(extensions);
    case TYPE_DYN:
      return "dyn " + get_unmangled_name(info->as<DynTypeInfo>()->trait_type);
    case TYPE_STRUCT:
    case TYPE_TUPLE:
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_CHOICE:
    case TYPE_TRAIT:
      return get_unmangled_name(this);
      break;
  }
}

Type *global_create_trait_type(const InternedString &name, Scope *scope, std::vector<Type *> generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_TRAIT));
  Type *type = type_table.back();
  std::string base = name.str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  TraitTypeInfo *info = type_info_alloc<TraitTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  info->scope->name = name;
  return type;
}

Type *global_create_struct_type(const InternedString &name, Scope *scope, std::vector<Type *> generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_STRUCT));
  Type *type = type_table.back();
  std::string base = name.str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }

  if (name == "Slice") {
    type->traits.push_back(is_slice_trait());
  } else if (name == "SliceMut") {
    type->traits.push_back(is_slice_mut_trait());
  }

  if (name == "RangeBase") {
    type->traits.push_back(is_range_trait());
  }

  type->set_base(base);
  type->generic_args = generic_args;
  StructTypeInfo *info = type_info_alloc<StructTypeInfo>();

  // ! THIS SHOULD NOT INHERIT THE NODES SCOPE,
  // ! IT SHOULD HAVE IT'S OWN SCOPE THAT'S DETACHED FROM THE ROOT SCOPE
  // ! ALL TYPES SUFFER THIS ISSUE, IT NEEDS TO BE FIXED.
  info->scope = scope;

  info->scope->name = base;
  type->set_info(info);

  if (info->is_union) {
    type->traits.push_back(is_union_trait());
  } else {
    type->traits.push_back(is_struct_trait());
  }

  for (const auto &[name, symbol] : scope->symbols) {
    if (symbol.is_variable) {
      info->members.push_back(TypeMember{
          .name = name,
          .type = symbol.resolved_type,
      });
    }
  }

  return type;
}

Type *global_create_choice_type(const InternedString &name, Scope *scope, const std::vector<Type *> &generic_args) {
  type_table.push_back(new Type(type_table.size(), TYPE_CHOICE));
  Type *type = type_table.back();
  std::string base = name.str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
  }
  type->set_base(base);
  type->generic_args = generic_args;
  ChoiceTypeInfo *info = type_info_alloc<ChoiceTypeInfo>();
  info->scope = scope;
  info->scope->name = base;
  type->set_info(info);
  type->traits.push_back(is_choice_trait());
  return type;
}

Type *global_create_enum_type(const InternedString &name, Scope *scope, bool is_flags, Type *underlying_type) {
  type_table.push_back(new Type(type_table.size(), TYPE_ENUM));
  Type *type = type_table.back();
  type->set_base(name);
  EnumTypeInfo *info = type_info_alloc<EnumTypeInfo>();
  info->underlying_type = underlying_type;
  info->is_flags = is_flags;
  info->scope = scope;
  info->scope->name = name;
  type->set_info(info);
  type->traits.push_back(is_enum_trait());
  return type;
}

Type *global_create_type(TypeKind kind, const InternedString &name, TypeInfo *info, const TypeExtensions &extensions,
                         Type *base_type) {
  type_table.push_back(new Type(type_table.size(), kind));
  auto type = type_table.back();
  type->base_type = base_type;
  type->set_ext(extensions);
  type->set_base(name);

  if (!info) {
    // ! What? why is this defaulting to struct type info?
    info = type_info_alloc<StructTypeInfo>();
  }

  type->set_info(info);

  if (type_extensions_is_back_pointer(extensions) && std::ranges::find(type->traits, is_pointer_trait()) == type->traits.end()) {
    type->traits.push_back(is_pointer_trait());

    if (type_extensions_is_back_const_pointer(extensions)) {
      type->traits.push_back(is_const_pointer_trait());
    } else {
      type->traits.push_back(is_mut_pointer_trait());
    }
    if (base_type->is_kind(TYPE_FUNCTION)) {
      type->traits.push_back(is_fn_ptr_trait());
    }
  }

  if (!info->scope) {
    info->scope = create_child(nullptr);
  }
  info->scope->name = name;
  return type;
}

Type *Type::get_element_type() const {
  if (!is_pointer() && !is_fixed_sized_array()) {
    throw_error(std::format("internal compiler error: called get_element_type() on a non pointer/array type\ngot type: \"{}\"",
                            to_string()),
                {});
  }
  auto extensions = extensions_without_back();
  if (is_kind(TYPE_TUPLE)) {
    auto info = (this->info->as<TupleTypeInfo>());
    return global_find_type_id(info->types, extensions);
  } else
    return global_find_type_id(base_type, extensions);
}

// used for anonymous structs etc.
Token get_unique_identifier() {
  static int num = 0;
  auto tok = Token({}, ANONYMOUS_TYPE_PREFIX + std::to_string(num), TType::Identifier, TFamily::Identifier);
  num++;
  return tok;
}

ScalarTypeInfo *create_scalar_type_info(ScalarType type, size_t size_in_bits, bool is_integral) {
  auto info = type_info_alloc<ScalarTypeInfo>();
  info->scalar_type = type;
  info->size_in_bits = size_in_bits;
  info->is_integral = is_integral;
  return info;
}

// an empty tuple type. this is a ZST just like void.
Type *unit_type() {
  static auto create = []() {
    type_table.push_back(new Type(type_table.size(), TYPE_TUPLE));
    Type *type = type_table.back();
    type->set_base(get_tuple_type_name({}));
    auto info = type_info_alloc<TupleTypeInfo>();
    info->types = {};
    type->set_info(info);
    info->scope = create_child(nullptr);
    return type;
  };
  static auto type = create();
  return type;
}

Type *bool_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "bool", create_scalar_type_info(TYPE_BOOL, 1, true));
  return type;
}
Type *void_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "void", create_scalar_type_info(TYPE_VOID, 0));
  return type;
}
Type *u64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u64", create_scalar_type_info(TYPE_UNSIGNED, 64, true));
  return type;
}
Type *u32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u32", create_scalar_type_info(TYPE_UNSIGNED, 32, true));
  return type;
}
Type *u16_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u16", create_scalar_type_info(TYPE_UNSIGNED, 16, true));
  return type;
}

Type *u8_ptr_type() {
  static Type *type = global_find_type_id(u8_type(), {{TYPE_EXT_POINTER_CONST}});
  return type;
}

Type *u8_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u8", create_scalar_type_info(TYPE_UNSIGNED, 8, true));
  return type;
}

Type *s64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s64", create_scalar_type_info(TYPE_SIGNED, 64, true));
  return type;
}
Type *s32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s32", create_scalar_type_info(TYPE_SIGNED, 32, true));
  return type;
}
Type *s16_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s16", create_scalar_type_info(TYPE_SIGNED, 16, true));
  return type;
}
Type *s8_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s8", create_scalar_type_info(TYPE_SIGNED, 8, true));
  return type;
}
Type *f32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f32", create_scalar_type_info(TYPE_FLOATING, 32));
  return type;
}

Type *f16_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f16", create_scalar_type_info(TYPE_FLOATING, 16));
  return type;
}

Type *f128_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f128", create_scalar_type_info(TYPE_FLOATING, 128));
  return type;
}

Type *f64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f64", create_scalar_type_info(TYPE_FLOATING, 64));
  return type;
}

// TODO(Josh) 10/5/2024, 10:04:29 AM
// TODO(Josh) 4/14/2024 9:59 AM
// idk just crazy how long it's been since I wrote that, this compiler is like 8 months old at this point.
// I removed the code that this original todo was for, but it's krazy how long it's been.
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
    f16_type();
    f32_type();
    f64_type();
    f128_type();
  }

  // Other
  {
    bool_type();
    void_type();
  }

  {  // initialize trait types.
    is_fn_ptr_trait();
    is_fn_trait();
    is_tuple_trait();
    is_struct_trait();
    is_enum_trait();
    is_choice_trait();
    is_dyn_trait();
    is_union_trait();
    is_array_trait();
    is_pointer_trait();
    is_mut_pointer_trait();
    is_const_pointer_trait();
    is_slice_trait();
    is_slice_mut_trait();
    blittable_trait();
  }
}

bool type_is_numeric(const Type *t) {
  if (!t->is_kind(TYPE_SCALAR) || t->has_extensions()) {
    return false;
  }
  ScalarTypeInfo *info = t->info->as<ScalarTypeInfo>();
  return info->scalar_type == TYPE_SIGNED || info->scalar_type == TYPE_UNSIGNED || info->scalar_type == TYPE_FLOATING;
}

constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR) return false;

  auto from_info = (from->info->as<ScalarTypeInfo>());
  auto to_info = (to->info->as<ScalarTypeInfo>());

  // do not allow casting of float to integer implicitly
  if (!from_info->is_integral && to_info->is_integral) {
    return false;
  }

  return from_info->size_in_bits <= to_info->size_in_bits;
}

Type *global_create_tuple_type(const std::vector<Type *> &types) {
  if (types.empty()) {
    return unit_type();
  }

  type_table.push_back(new Type(type_table.size(), TYPE_TUPLE));
  Type *type = type_table.back();
  type->set_base(get_tuple_type_name(types));

  auto info = type_info_alloc<TupleTypeInfo>();
  info->types = types;

  type->set_info(info);
  info->scope = create_child(nullptr);

  for (size_t i = 0; i < types.size(); ++i) {
    info->scope->insert_variable(std::to_string(i), types[i], nullptr, MUT);
    info->members.push_back(TypeMember{
        .name = "$" + std::to_string(i),
        .type = types[i],
    });
  }

  return type;
}

InternedString get_tuple_type_name(const std::vector<Type *> &types) {
  std::stringstream ss;
  ss << "(";
  for (auto it = types.begin(); it != types.end(); ++it) {
    auto type = *it;

    ss << type->to_string();

    if (it != types.end() - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

Type *Type::take_pointer_to(bool is_mutable) const {
  auto ext = this->extensions;
  ext.push_back({.type = is_mutable ? TYPE_EXT_POINTER_MUT : TYPE_EXT_POINTER_CONST, .array_size = 0});
  return global_find_type_id(base_type == Type::INVALID_TYPE ? (Type *)this : base_type, ext);
}

std::string get_operator_overload_name(TType op, OperationKind kind) {
  std::string output = "";

  if (kind == OPERATION_SLICE_INDEX) {
    return "slice_index";
  }

  switch (op) {
    case TType::LBrace:
      // TODO: This needs to be 'index'/'index_mut'
      return "index";

    // Do we want this? might be useful for stuff like Array implementations etc.
    // However, it feels like bringing in the complexity of C++'s
    // copy constructor, copy assign, copy assign ref, move constructor , etc.
    case TType::Assign:

    // via trait Arithmetic
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

    // via trait Logical
    case TType::LogicalNot:
    case TType::LogicalOr:
    case TType::LogicalAnd:

    // via trait Bitwise
    case TType::Not:
    case TType::Or:
    case TType::And:
    case TType::SHL:
    case TType::SHR:
    case TType::Xor:

    // via trait Compare.
    case TType::LT:
    case TType::GT:
    case TType::EQ:
    case TType::NEQ:
    case TType::LE:
    case TType::GE:

    // via trait Inc/Dec
    case TType::Increment:
    case TType::Decrement:
      output = ttype_to_string(op);
      break;
    // via traits CompArith/CompBitwise/CompLogical etc.
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
      output = ttype_to_string(op);
      output.replace(0, 4, "comp_");
    default:
      break;
  }
  std::transform(output.begin(), output.end(), output.begin(), ::tolower);
  return output;
}

Symbol *find_operator_overload(int mutability, Type *type, TType op, OperationKind kind) {
  if (!type) {
    return nullptr;
  }
  std::string op_str = get_operator_overload_name(op, kind);
  if (op_str.empty()) {
    return nullptr;
  }

  std::transform(op_str.begin(), op_str.end(), op_str.begin(), ::tolower);

  if (op_str == "index" && (type->is_mut_pointer() || mutability == MUT)) {
    op_str = "index_mut";
  }

  if (op_str == "slice_index" && (type->is_mut_pointer() || mutability == MUT)) {
    op_str = "slice_index_mut";
  }

  auto scope = type->info->scope;

  if (!scope) return nullptr;

  if (auto symbol = scope->local_lookup(op_str)) {
    if (symbol->is_function && type_is_valid(symbol->resolved_type)) {
      return symbol;
    }
  }

  return nullptr;
}

/*
  TODO: we should inline this, or make it static or something.
*/
std::string mangled_type_args(const std::vector<Type *> &args) {
  std::string s;
  int i = 0;
  for (const auto &arg : args) {
    /*
      ! BUG
      Why did I have to add this when refactoring the type system??
      This seems very wrong.


      Although, it kind of makes sense,
      because we call this when creating generic types that don't have type args,
      and since theyre not yet concrete, this probably just eneded up happening on the
      -1's in the args array anyway.
    */
    if (!type_is_valid(arg)) {
      s += "$-1";
      continue;
    }

    if (i > 0) {
      s += "_" + std::to_string(arg->uid);
    } else {
      s += "$" + std::to_string(arg->uid);
    }
    i++;
  }
  return s;
}

Type *is_range_trait() {
  static Type *id = global_create_trait_type("IsRange", create_child(nullptr), {});
  return id;
}

Type *is_fn_ptr_trait() {
  static Type *id = global_create_trait_type("IsFnPtr", create_child(nullptr), {});
  return id;
}

Type *is_fn_trait() {
  static Type *id = global_create_trait_type("IsFn", create_child(nullptr), {});
  return id;
}

Type *is_tuple_trait() {
  static Type *id = global_create_trait_type("IsTuple", create_child(nullptr), {});
  return id;
}

Type *is_struct_trait() {
  static Type *id = global_create_trait_type("IsStruct", create_child(nullptr), {});
  return id;
}
Type *is_enum_trait() {
  static Type *id = global_create_trait_type("IsEnum", create_child(nullptr), {});
  return id;
}
Type *is_choice_trait() {
  static Type *id = global_create_trait_type("IsChoice", create_child(nullptr), {});
  return id;
}
Type *is_dyn_trait() {
  static Type *id = global_create_trait_type("IsDyn", create_child(nullptr), {});
  return id;
}
Type *is_union_trait() {
  static Type *id = global_create_trait_type("IsUnion", create_child(nullptr), {});
  return id;
}

Type *is_array_trait() {
  static Type *id = global_create_trait_type("IsArray", create_child(nullptr), {});
  return id;
}

Type *is_pointer_trait() {
  static Type *id = global_create_trait_type("IsPointer", create_child(nullptr), {});
  return id;
}

Type *is_mut_pointer_trait() {
  static Type *id = global_create_trait_type("IsMutPointer", create_child(nullptr), {});
  return id;
}

Type *is_const_pointer_trait() {
  static Type *id = global_create_trait_type("IsConstPointer", create_child(nullptr), {});
  return id;
}

Type *is_slice_trait() {
  static Type *id = global_create_trait_type("IsSlice", create_child(nullptr), {});
  return id;
}

Type *is_slice_mut_trait() {
  static Type *id = global_create_trait_type("IsSliceMut", create_child(nullptr), {});
  return id;
}

Type *blittable_trait() {
  static Type *id = global_create_trait_type("Blittable", create_child(nullptr), {});
  return id;
}

bool Type::implements(const Type *trait) {
  auto found = std::ranges::find(traits, trait);
  if (found != traits.end()) {
    return true;
  }
  for (auto &the_trait : traits) {
    if (the_trait->generic_base_type == trait) {
      return true;
    }
  }
  return false;
}

Type *ChoiceTypeInfo::get_variant_type(const InternedString &variant_name) const {
  for (size_t i = 0; i < members.size(); ++i) {
    if (members[i].name == variant_name) {
      return members[i].type;
    }
  }
  return nullptr;
}

int ChoiceTypeInfo::get_variant_discriminant(const InternedString &variant_name) const {
  for (size_t i = 0; i < members.size(); ++i) {
    if (members[i].name == variant_name) {
      // We return +1 so that default constructed choice types never have a value that can be pattern matched.
      return i + 1;
    }
  }
  return -1;
}

void assess_and_try_add_blittable_trait(Type *type) {
  if (!type->is_kind(TYPE_STRUCT)) {
    return;
  }

  if (type->implements(blittable_trait())) {
    return;
  }

  StructTypeInfo *info = type->info->as<StructTypeInfo>();

  for (const auto &member : info->members) {
    // TODO: do we want to consider arrays plain old data?
    if (!member.type->is_kind(TYPE_SCALAR)) {
      if (!(member.type->has_extensions() && member.type->is_pointer())) {
        return;
      }
      if (!member.type->implements(blittable_trait())) {
        return;
      }
    }
  }

  type->traits.push_back(blittable_trait());
}

size_t Type::alignment_in_bits() const {
  const size_t max_align_bits = alignof(std::max_align_t) * CHAR_BIT;
  auto at_least_one = [](size_t v) { return v == 0 ? 1 : v; };

  if (kind == TYPE_SCALAR) {
    size_t bits = info->as<ScalarTypeInfo>()->size_in_bits;
    bits = at_least_one(bits);
    return std::min(bits, max_align_bits);
  }

  if (is_pointer()) {
    return alignof(void *) * CHAR_BIT;
  }

  if (is_fixed_sized_array()) {
    return base_type->alignment_in_bits();
  }

  if (kind == TYPE_STRUCT) {
    auto struct_info = info->as<StructTypeInfo>();
    size_t max_align = 1;
    // For both structs and unions we want the max member alignment
    for (auto &member : struct_info->members) {
      max_align = std::max(max_align, member.type->alignment_in_bits());
    }
    return at_least_one(max_align);
  }

  if (kind == TYPE_TUPLE) {
    auto tuple_info = info->as<TupleTypeInfo>();
    size_t max_align = 1;
    for (auto &member : tuple_info->members) {
      max_align = std::max(max_align, member.type->alignment_in_bits());
    }
    return at_least_one(max_align);
  }

  if (kind == TYPE_CHOICE) {
    const auto *choice_info = info->as<ChoiceTypeInfo>();
    size_t tag_align = alignof(int32_t) * CHAR_BIT;
    size_t max_align = tag_align;
    for (const auto &member : choice_info->members) {
      max_align = std::max(max_align, member.type->alignment_in_bits());
    }
    return at_least_one(max_align);
  }

  if (kind == TYPE_ENUM) {
    return info->as<EnumTypeInfo>()->underlying_type->alignment_in_bits();
  }

  if (kind == TYPE_DYN) {
    return alignof(void *) * CHAR_BIT;
  }

  return alignof(void *) * CHAR_BIT;
}

inline size_t align_to_bits(size_t value, size_t alignment_bits) {
  return (value + alignment_bits - 1) / alignment_bits * alignment_bits;
}
bool Type::try_get_offset_in_bits(size_t target_index, size_t &bit_offset) const {
  bit_offset = 0;
  size_t index = 0;

  switch (kind) {
    case TYPE_STRUCT: {
      const auto *sti = info->as<StructTypeInfo>();
      if (sti->is_union) {
        for (const auto &m : sti->members) {
          if (m.name.str().starts_with(ANONYMOUS_TYPE_PREFIX)) {
            for (size_t i = 0; i < info->members.size(); ++i) {
              if (index++ == target_index) {
                bit_offset = 0;
                return true;
              }
            }
          } else if (index++ == target_index) {
            bit_offset = 0;
            return true;
          }
        }
        return false;
      }
    }
    // This handles tuples, structs and dyn's falls through past struct since structs may be unions.
    case TYPE_TUPLE:
    case TYPE_DYN: {
      size_t offset = 0;
      for (const auto &m : info->members) {
        const Type *mt = m.type;
        offset = align_to_bits(offset, mt->alignment_in_bits());
        if (m.name.str().starts_with(ANONYMOUS_TYPE_PREFIX)) {
          // flatten one level
          size_t inner_off = offset;
          for (const auto &im : mt->info->members) {
            size_t aligned_off = align_to_bits(inner_off, im.type->alignment_in_bits());
            if (index++ == target_index) {
              bit_offset = aligned_off;
              return true;
            }
            inner_off += im.type->size_in_bits();
          }
        } else if (index++ == target_index) {
          bit_offset = offset;
          return true;
        }

        offset += mt->size_in_bits();
      }
      return false;
    }
    case TYPE_CHOICE: {
      const auto *choice = info->as<ChoiceTypeInfo>();
      const size_t tag_bits = sizeof(int32_t) * CHAR_BIT;
      for (const auto &m : choice->members) {
        const Type *mt = m.type;
        size_t payload_off = align_to_bits(tag_bits, mt->alignment_in_bits());
        if (index++ == target_index) {
          bit_offset = payload_off;
          return true;
        }
      }
      return false;
    }
    default:
      return false;
  }
}

bool Type::try_get_index_of_member(const InternedString &name, size_t &index) const {
  const TypeInfo *info = this->info;
  if (!info) {
    return false;
  }

  // 1. Check direct members
  for (size_t i = 0; i < info->members.size(); ++i) {
    const auto &m = info->members[i];
    if (m.name == name) {
      index = i;
      return true;
    }
  }

  // 2. Recurse into anonymous subtypes
  for (size_t i = 0; i < info->members.size(); ++i) {
    const auto &m = info->members[i];
    const Type *member_type = m.type;

    if (!m.name.str().starts_with(ANONYMOUS_TYPE_PREFIX)) {
      continue;
    }

    size_t subindex = 0;
    if (member_type->try_get_index_of_member(name, subindex)) {
      index = i;  // return the index of the containing anonymous member
      return true;
    }
  }

  // 3. Climb up extensions if any
  if (has_extensions() && base_type) {
    size_t base_index = 0;
    if (base_type->try_get_index_of_member(name, base_index)) {
      index = base_index;
      return true;
    }
  }

  return false;
}

size_t Type::size_in_bits() const {
  if (is_pointer()) {
    return sizeof(void *) * CHAR_BIT;
  }

  if (is_fixed_sized_array()) {
    size_t elem_bits = base_type->size_in_bits();
    size_t len = extensions.back().array_size;
    return elem_bits * len;
  }

  if (is_kind(TYPE_SCALAR)) {
    return info->as<ScalarTypeInfo>()->size_in_bits;
  }

  if (is_kind(TYPE_STRUCT)) {
    auto struct_info = info->as<StructTypeInfo>();
    size_t offset = 0;
    size_t max_align_bits = 1;

    if (struct_info->is_union) {
      size_t max_bits = 0;
      for (auto &member : struct_info->members) {
        size_t member_bits = member.type->size_in_bits();
        size_t align_bits = member.type->alignment_in_bits();
        max_bits = std::max(max_bits, member_bits);
        max_align_bits = std::max(max_align_bits, align_bits);
      }
      return align_to_bits(max_bits, max_align_bits);
    } else {
      for (auto &member : struct_info->members) {
        size_t align_bits = member.type->alignment_in_bits();
        size_t member_bits = member.type->size_in_bits();
        max_align_bits = std::max(max_align_bits, align_bits);
        offset = align_to_bits(offset, align_bits);
        offset += member_bits;
      }
      return align_to_bits(offset, max_align_bits);
    }
  }

  if (is_kind(TYPE_ENUM)) {
    return info->as<EnumTypeInfo>()->underlying_type->size_in_bits();
  }

  if (is_kind(TYPE_TUPLE)) {
    auto tuple_info = info->as<TupleTypeInfo>();
    size_t offset = 0;
    size_t max_align_bits = 1;
    for (auto &member : tuple_info->members) {
      size_t align_bits = member.type->alignment_in_bits();
      size_t member_bits = member.type->size_in_bits();
      max_align_bits = std::max(max_align_bits, align_bits);
      offset = align_to_bits(offset, align_bits);
      offset += member_bits;
    }
    return align_to_bits(offset, max_align_bits);
  }

  if (is_kind(TYPE_CHOICE)) {
    const auto *choice_info = info->as<ChoiceTypeInfo>();
    size_t tag_bits = sizeof(int32_t) * CHAR_BIT;
    size_t max_variant_bits = 0;
    size_t max_align_bits = alignof(int32_t) * CHAR_BIT;

    for (const auto &member : choice_info->members) {
      size_t variant_bits = member.type->size_in_bits();
      size_t align_bits = member.type->alignment_in_bits();
      max_variant_bits = std::max(max_variant_bits, variant_bits);
      max_align_bits = std::max(max_align_bits, align_bits);
    }

    size_t total_bits = tag_bits + align_to_bits(max_variant_bits, max_align_bits);
    return align_to_bits(total_bits, max_align_bits);
  }

  if (is_kind(TYPE_DYN)) {
    const DynTypeInfo *dyn = info->as<DynTypeInfo>();
    return (dyn->methods.size() + 1) * sizeof(void *) * CHAR_BIT;
  }

  // zero-sized types (e.g. fn, trait)
  return 0;
}

Type *find_or_create_arbitrary_integer_type(bool is_signed, size_t bits) {
  const ScalarType scalar_kind = is_signed ? TYPE_SIGNED : TYPE_UNSIGNED;
  auto info = create_scalar_type_info(scalar_kind, bits, true);
  std::string name = (is_signed ? "s" : "u") + std::to_string(bits);

  // @Performance: get rid of this stupid linear search.
  // It's not the only place we do it -- it's really the only option
  // with how the data is structured.
  //
  // Fixing this will be huge performance gains, restructuring types to be stored by hashed keys of { name, extensions }
  for (const auto &type : type_table) {
    if (type->is_kind(TYPE_SCALAR) && type->basename == name && type->has_no_extensions()) {
      return type;
    }
  }

  return global_create_type(TYPE_SCALAR, name, info, {});
}
