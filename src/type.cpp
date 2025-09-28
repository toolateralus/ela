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
  ss << "fn ";
  ss << extensions_to_string(ext) << ' ';
  ss << "(";

  for (size_t i = 0; i < params_len; ++i) {
    auto t = parameter_types[i];
    ss << get_unmangled_name(t);
    if (i < params_len - 1) {
      ss << ", ";
    }
  }

  if (is_varargs)
    ss << ", ...)";
  else
    ss << ')';

  ss << " -> " << get_unmangled_name(return_type);

  return ss.str();
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

  ss << " -> " << return_type->basename.get_str();

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

ConversionRule type_conversion_rule(const Type *from, const Type *to, const SourceRange &range) {
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
    if (type_is_numerical(from) && type_is_numerical(to)) {
      if (numerical_type_safe_to_upcast(from, to)) {
        return CONVERT_IMPLICIT;
      }
      return CONVERT_EXPLICIT;

      // !!!! I ADDED THE PARENTHESIS ON THE SECOND EXPRESSION HERE :: !!!!!
      // ! This may cause bugs!
    } else if ((from == bool_type() && type_is_numerical(to)) ||
               (to == bool_type() && type_is_numerical(from))) {  // Convert booleans to number types explicitly
      // TODO(Josh) 1/13/2025, 3:07:06 PM :: Why did I have to add this? I could've sworn we had this working othrwise.
      // TODO: It's possible we just never noticed.
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic, from scalar type pointers, to numerical types.
  const auto from_is_scalar_ptr = from->is_mut_pointer();
  const auto to_is_non_ptr_number = type_is_numerical(to) && to->has_no_extensions();

  if (from_is_scalar_ptr && to_is_non_ptr_number) {
    return CONVERT_IMPLICIT;
  }

  // allow casting from number types to pointers explicitly
  if (type_is_numerical(from) && to->is_pointer()) {
    return CONVERT_EXPLICIT;
  }

  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow in if (...)
  // cast all numerical types and pointers to booleans implicitly.
  if ((type_is_numerical(from) || from->is_pointer()) && to == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  if (type_is_numerical(from) && to == bool_type()) {
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
    if (conversion == CONVERT_NONE_NEEDED ||
        (conversion == CONVERT_IMPLICIT && (from->pointer_depth() == to->pointer_depth()))) {
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

  return std::memcmp(finder_info->parameter_types, self_info->parameter_types,
                     finder_info->params_len * sizeof(Type *)) == 0;
}

/*
  Get a display name of this type, in a readable manner to users.
  Unmangles.
*/
std::string Type::to_string() const {
  if (basename.str_ptr->starts_with("__anon_D")) {
    return extensions_to_string(extensions) + "(anonymous type)";
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
  std::string base = name.get_str();
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
  std::string base = name.get_str();
  if (!generic_args.empty()) {
    base += mangled_type_args(generic_args);
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

  if (type_extensions_is_back_pointer(extensions) &&
      std::ranges::find(type->traits, is_pointer_trait()) == type->traits.end()) {
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

  if (name == "Slice") {
    type->traits.push_back(is_slice_trait());
  } else if (name == "SliceMut") {
    type->traits.push_back(is_slice_mut_trait());
  }

  if (!info->scope) {
    info->scope = create_child(nullptr);
  }
  info->scope->name = name;
  return type;
}

Type *Type::get_element_type() const {
  if (!is_pointer() && !is_fixed_sized_array()) {
    throw_error(
        std::format("internal compiler error: called get_element_type() on a non pointer/array type\ngot type: \"{}\"",
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

ScalarTypeInfo *create_scalar_type_info(ScalarType type, size_t size, bool is_integral = false) {
  auto info = type_info_alloc<ScalarTypeInfo>();
  info->scalar_type = type;
  info->size = size;
  info->is_integral = is_integral;
  return info;
}

Type *bool_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "bool", create_scalar_type_info(TYPE_BOOL, 1, true));
  return type;
}
Type *void_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "void", create_scalar_type_info(TYPE_VOID, 0));
  return type;
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

Type *u64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u64", create_scalar_type_info(TYPE_U64, 8, true));
  return type;
}
Type *u32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u32", create_scalar_type_info(TYPE_U32, 4, true));
  return type;
}
Type *u16_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u16", create_scalar_type_info(TYPE_U16, 2, true));
  return type;
}

Type *u8_ptr_type() {
  static Type *type = global_find_type_id(u8_type(), {{TYPE_EXT_POINTER_CONST}});
  return type;
}

Type *char_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "char", create_scalar_type_info(TYPE_CHAR, 1, true));
  return type;
}

Type *char_ptr_type() {
  static Type *type = global_find_type_id(char_type(), {{TYPE_EXT_POINTER_CONST}});
  return type;
}

Type *u8_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "u8", create_scalar_type_info(TYPE_U8, 1, true));
  return type;
}

Type *s64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s64", create_scalar_type_info(TYPE_S64, 8, true));
  return type;
}
Type *s32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s32", create_scalar_type_info(TYPE_S32, 4, true));
  return type;
}
Type *s16_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s16", create_scalar_type_info(TYPE_S16, 2, true));
  return type;
}
Type *s8_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "s8", create_scalar_type_info(TYPE_S8, 1, true));
  return type;
}
Type *f32_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f32", create_scalar_type_info(TYPE_FLOAT, 4));
  return type;
}
Type *f64_type() {
  static Type *type = global_create_type(TYPE_SCALAR, "f64", create_scalar_type_info(TYPE_DOUBLE, 8));
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
    f64_type();
    f32_type();
  }

  // Other
  {
    bool_type();
    void_type();
    char_type();
    char_ptr_type();
  }

  { // initialize trait types.
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

bool type_is_numerical(const Type *t) {
  if (!t->is_kind(TYPE_SCALAR)) return false;
  return t == s32_type() || t == s8_type() || t == s16_type() || t == s32_type() || t == s64_type() || t == u8_type() ||
         t == u16_type() || t == u32_type() || t == u64_type() || t == f32_type() || t == f64_type();
}

constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR) return false;

  auto from_info = (from->info->as<ScalarTypeInfo>());
  auto to_info = (to->info->as<ScalarTypeInfo>());

  // do not allow casting of float to integer implicitly
  if (!from_info->is_integral && to_info->is_integral) {
    return false;
  }

  return from_info->size <= to_info->size;
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
      output = TTypeToString(op);
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
      output = TTypeToString(op);
      output.replace(0, 4, "comp_");
    default:
      break;
  }
  std::transform(output.begin(), output.end(), output.begin(), ::tolower);
  return output;
}

Type *find_operator_overload(int mutability, Type *type, TType op, OperationKind kind) {
  if (!type) {
    return Type::INVALID_TYPE;
  }
  std::string op_str = get_operator_overload_name(op, kind);
  if (op_str.empty()) {
    return Type::INVALID_TYPE;
  }

  std::transform(op_str.begin(), op_str.end(), op_str.begin(), ::tolower);

  if (op_str == "index" && (type->is_mut_pointer() || mutability == MUT)) {
    op_str = "index_mut";
  }

  auto scope = type->info->scope;

  if (!scope) return Type::INVALID_TYPE;

  if (auto symbol = scope->local_lookup(op_str)) {
    if (symbol->is_function && type_is_valid(symbol->resolved_type)) {
      return symbol->resolved_type;
    }
  }

  return Type::INVALID_TYPE;
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

size_t Type::size_in_bytes() const {
  if (!info) {
    return sizeof(void *);
  }

  if (is_pointer()) {
    return sizeof(void *);
  }

  if (is_fixed_sized_array()) {
    size_t elem_size = base_type->size_in_bytes();
    size_t len = extensions.back().array_size;
    return elem_size * len;
  }

  if (kind == TYPE_STRUCT && info->as<StructTypeInfo>()->is_union) {
    auto struct_info = info->as<StructTypeInfo>();
    size_t max_size = 0;
    size_t max_align = 1;
    for (auto &member : struct_info->members) {
      max_size = std::max(max_size, member.type->size_in_bytes());
      max_align = std::max(max_align, member.type->alignment_in_bytes());
    }
    // round up to max alignment
    max_size = (max_size + max_align - 1) & ~(max_align - 1);
    return max_size;
  }

  if (kind == TYPE_STRUCT) {
    auto struct_info = info->as<StructTypeInfo>();
    size_t offset = 0;
    size_t max_align = 1;

    for (auto &member : struct_info->members) {
      size_t member_size = member.type->size_in_bytes();
      size_t align = member.type->alignment_in_bytes();
      max_align = std::max(max_align, align);

      // align the offset
      offset = (offset + align - 1) & ~(align - 1);
      offset += member_size;
    }

    // Round up to max alignment
    offset = (offset + max_align - 1) & ~(max_align - 1);
    return offset;
  }

  if (kind == TYPE_SCALAR) {
    switch (info->as<ScalarTypeInfo>()->scalar_type) {
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

  return sizeof(void *);
}

size_t Type::alignment_in_bytes() const {
  if (!info) return alignof(void *);

  if (kind == TYPE_SCALAR) {
    switch (info->as<ScalarTypeInfo>()->scalar_type) {
      case TYPE_S8:
      case TYPE_U8:
      case TYPE_CHAR:
      case TYPE_BOOL:
        return 1;
      case TYPE_S16:
      case TYPE_U16:
        return 2;
      case TYPE_S32:
      case TYPE_U32:
        return 4;
      case TYPE_S64:
      case TYPE_U64:
        return 8;
      case TYPE_FLOAT:
        return 4;
      case TYPE_DOUBLE:
        return 8;
      default:
        return alignof(void *);
    }
  }

  if (is_pointer()) return alignof(void *);
  if (is_fixed_sized_array()) return base_type->alignment_in_bytes();

  if (kind == TYPE_STRUCT) {
    auto struct_info = info->as<StructTypeInfo>();
    size_t max_align = 1;
    for (auto &member : struct_info->members) max_align = std::max(max_align, member.type->alignment_in_bytes());
    return max_align;
  }

  return alignof(void *);
}
