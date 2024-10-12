#include "type.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include <sstream>
#include <unordered_set>

std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << global_get_type(return_type)->get_base();
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

Type *global_get_type(const int id) {
  [[unlikely]] if (id < 0)
    return nullptr;
  return type_table[id];
}

int global_create_type_alias(int aliased_type, const std::string &name) {

  // this type alias already exists so just return the type.
  if (type_alias_map.contains(name)) {
    return type_alias_map[name];
  }

  auto aliased = global_get_type(aliased_type);
  auto type = new (type_alloc<Type>()) Type(num_types, aliased->kind);
  type_table[num_types] = type;
  type->set_base(name);
  type->set_info(aliased->get_info());
  type->is_alias = true;
  type->alias_id = aliased_type;
  num_types++;
  type_alias_map[name] = type->id;
  aliased->has_aliases = true;
  aliased->aliases.push_back(type->id);
  return type->id;
}
int global_find_function_type_id(const std::string &name,
                                 const FunctionTypeInfo &info,
                                 const TypeExt &ext) {

  if (type_alias_map.contains(name)) {
    auto alias = type_alias_map[name];
    if (ext.has_no_extensions())
      return alias;
    else {
      auto base_type = global_get_type(alias);
      return global_create_type((TypeKind)base_type->kind,
                                base_type->get_base(), base_type->get_info(),
                                ext);
    }
  }

  for (int i = 0; i < num_types; ++i) {
    if (type_table[i]->kind != TYPE_FUNCTION)
      continue;
    const Type *type = type_table[i];
    if (name == type->get_base() &&
        type->type_info_equals(&info, TYPE_FUNCTION) &&
        ext.equals(type->get_ext())) {
      return type->id;
    }
  }
  auto info_ptr = new (type_alloc<FunctionTypeInfo>()) FunctionTypeInfo(info);
  return global_create_type(TYPE_FUNCTION, name, info_ptr, ext);
}

// PERFORMANCE(Josh) 10/5/2024, 9:55:59 AM
// We might want to upgrade to a hash map at a certain number of types or
// something. I think the linear search is fine but this is certainly one of the
// slowest functions in the compiler.
int global_find_type_id(const std::string &name,
                        const TypeExt &type_extensions) {
  if (type_alias_map.contains(name)) {
    auto alias = type_alias_map[name];
    if (type_extensions.has_no_extensions())
      return alias;
    else {
      auto base_type = global_get_type(alias);
      return global_create_type(
          (TypeKind)base_type->kind, base_type->get_base(),
          base_type->get_info(),
          base_type->get_ext_no_compound().append(type_extensions));
    }
  }
  for (int i = 0; i < num_types; ++i) {
    auto type = type_table[i];
    auto visited = std::unordered_set<const Type *>();
    if (type->equals(name, type_extensions, visited))
      return type->id;
  }

  // NOTE:below is just for creating types with new extensions. new function
  // types, struct types, and enum types must be created manually this just
  // creates pointer and array types of base 'name'
  int base_id = -1;
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];
    if (tinfo->get_base() == name && tinfo->get_ext().has_no_extensions()) {
      base_id = tinfo->id;
      break;
    }
  }
  if (base_id != -1) {
    auto t = global_get_type(base_id);
    return global_create_type((TypeKind)t->kind, name, t->get_info(),
                              type_extensions);
  }

  return -1;
}

// CLEANUP(Josh) 10/3/2024, 9:25:36 AM
// This could be significantly improved for readability
// PERFORMANCE(Josh) 10/3/2024, 9:25:51 AM
// Doing linear searches over the types a ton of times might get slower for
// large programs. However per 500 lines of unit tsting which creates a wide
// variety of types, we only really get like 50-100 types total, so a hash map
// is not really going to pay off that much probably.
ConversionRule type_conversion_rule(const Type *from, const Type *to) {
  if (!from || !to) {
    throw_error("Internal Compiler Error: type was null when checking type conversion rules", {});
  }

  // same exact type. no cast needed.
  if (from->id == to->id)
    return CONVERT_NONE_NEEDED;

  if (from->is_alias) {
    auto aliasType = global_get_type(type_alias_map[from->get_base()]);
    auto pointed_to = global_get_type(aliasType->alias_id);
    auto fullType = global_get_type(
        global_find_type_id(pointed_to->get_base(), from->get_ext()));
    return type_conversion_rule(fullType, to);
  }

  if (to->is_alias) {
    auto aliasType = global_get_type(type_alias_map[to->get_base()]);
    auto pointed_to = global_get_type(aliasType->alias_id);
    auto fullType = global_get_type(
        global_find_type_id(pointed_to->get_base(), to->get_ext()));
    return type_conversion_rule(from, fullType);
  }

  // implicitly upcast integer and float types.
  // u8 -> u16 -> u32 etc legal.
  // u16 -> u8 == implicit required.
  if (from->is_kind(TYPE_SCALAR) && from->get_ext().has_no_extensions() &&
      to->is_kind(TYPE_SCALAR) && to->get_ext().has_no_extensions()) {
    if (type_is_numerical(from) && type_is_numerical(to)) {
      if (numerical_type_safe_to_upcast(from, to))
        return CONVERT_IMPLICIT;
      return CONVERT_EXPLICIT;
    }
  }

  // allow pointer arithmetic
  if (from->is_kind(TYPE_SCALAR) && from->get_ext().is_pointer(-1) &&
      to->is_kind(TYPE_SCALAR) && type_is_numerical(to) &&
      to->get_ext().has_no_extensions()) {
    return CONVERT_IMPLICIT;
  }

  // cast all numerical types and pointers too booleans implicitly for if
  // statements
  // TODO(Josh) 10/1/2024, 8:58:13 PM Probably make this stricter and only allow
  // this in conditional statements.
  if ((type_is_numerical(from) || from->get_ext().is_pointer()) &&
      to->id == bool_type()) {
    return CONVERT_IMPLICIT;
  }

  // TODO: probably want to fix this. right now we have C style pointer casting:
  // any two pointers of the same depth can cast implicitly
  if (from->get_ext().is_pointer(1) && to->get_ext().is_pointer(1)) {
    return CONVERT_IMPLICIT;
  }

  // search structs for their cast tables.
  // Not super useful
  if (to->kind == TYPE_STRUCT) {
    auto info = static_cast<StructTypeInfo *>(to->get_info());
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
  if (from->get_ext().has_extensions() && to->get_ext().has_extensions() &&
      from->get_ext().extensions.back() == to->get_ext().extensions.back()) {
    auto from_base = global_get_type(
        global_find_type_id(from->get_base(), from->get_ext().without_back()));
    auto to_base = global_get_type(
        global_find_type_id(to->get_base(), to->get_ext().without_back()));
    return type_conversion_rule(from_base, to_base);
  }

  return CONVERT_PROHIBITED;
}

bool Type::operator==(const Type &type) const {
  for (int i = 0; i < num_types; ++i) {
    auto tinfo = type_table[i];

    auto visited = std::unordered_set<const Type *>();
    if (tinfo->equals(base, extensions, visited) && type.info &&
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

bool Type::equals(const std::string &name, const TypeExt &type_extensions,
                  std::unordered_set<const Type *> &visited) const {
  auto type = global_get_type(id);
  if (type->get_base() != name)
    return false;

  return type_extensions == type->get_ext();
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
  
  if (key_type != other.key_type) {
    return false;
  }
  
  return true;
}

std::string Type::to_string() const {
  auto base_no_ext = global_find_type_id(get_base(), {});
  auto base_type = global_get_type(base_no_ext);

  Type *type = (Type *)this;
  if (type->is_alias || base_type->is_alias) {
    base_type = global_get_type(base_type->alias_id);
    auto old_ext = base_type->get_ext().append(get_ext_no_compound());
    auto new_id = global_find_type_id(base_type->get_base(), old_ext);
    type = global_get_type(new_id);
  }

  switch (kind) {
  case TYPE_STRUCT:
  case TYPE_SCALAR:
    return type->base + type->extensions.to_string();
  case TYPE_FUNCTION:
    return type->info->to_string() + type->extensions.to_string();
    break;
  case TYPE_ENUM: {
    return type->base;
  }
  case TYPE_UNION:
    return type->base;
  }
}

// CLEANUP(Josh) 10/5/2024, 9:57:02 AM
// This should be in the emit visitor not here.
std::string to_type_struct(Type *type, Context &context) {
  auto id = type->get_true_type();
  auto new_type = global_get_type(id);
  
  if (new_type != type) {
    type = new_type;
  }

  static bool *type_cache = [] {
    auto arr = new bool[MAX_NUM_TYPES];
    memset(arr, false, MAX_NUM_TYPES);
    return arr;
  }();

  if (type_cache[id]) {
    return std::format("_type_info[{}]", id);
  }
  
  type_cache[id] = true;
  
  std::stringstream fields_ss;
  if (type->kind == TYPE_UNION) {
    auto info = static_cast<UnionTypeInfo *>(type->get_info());
    if (info->scope->symbols.empty()) {
      fields_ss << "_type_info[" << id << "] = new Type {"
                << ".name = \"" << type->to_string() << "\","
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
      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << "new Field { " << std::format(".name = \"{}\"", name) << ", "
                << std::format(".type = {}", to_type_struct(t, context))
                << " }";
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else if (type->kind == TYPE_ENUM) {
    auto info = static_cast<EnumTypeInfo *>(type->get_info());
    if (info->keys.empty()) {
      fields_ss << "_type_info[" << id << "] = new Type {"
                << ".name = \"" << type->to_string() << "\","
                << ".id = " << id << "}";
      context.type_info_strings.push_back(fields_ss.str());
      return std::string("_type_info[") + std::to_string(id) + "]";
    }

    fields_ss << "{";

    int count = info->keys.size();
    int it = 0;
    for (const auto &name : info->keys) {
      auto t = global_get_type(s32_type());
      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << "new Field { " << std::format(".name = \"{}\"", name) << ", "
                << std::format(".type = {}", to_type_struct(t, context))
                << " }";
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else if (type->kind == TYPE_STRUCT) {
    auto info = static_cast<StructTypeInfo *>(type->get_info());
    if (info->scope->symbols.empty()) {
      fields_ss << "_type_info[" << id << "] = new Type {"
                << ".name = \"" << type->to_string() << "\","
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
      if (!t)
        throw_error("Internal Compiler Error: Type was null in reflection "
                    "'to_type_struct()'",
                    {});
      fields_ss << "new Field { " << std::format(".name = \"{}\"", name) << ", "
                << std::format(".type = {}", to_type_struct(t, context))
                << " }";
      ++it;
      if (it < count) {
        fields_ss << ", ";
      }
    }
    fields_ss << "}";
  } else {
    fields_ss << "_type_info[" << id << "] = new Type {"
              << ".id = " << id << ",\n"
              << ".name = \"" << type->to_string() << "\"}";
    context.type_info_strings.push_back(fields_ss.str());
    return std::string("_type_info[") + std::to_string(id) + "]";
  }


  context.type_info_strings.push_back(std::format(
      "_type_info[{}] = new Type {{ .id = {}, .name = \"{}\", .fields = {} }};",
      id, id, type->to_string(), fields_ss.str()));

  return std::format("_type_info[{}]", id);
}

int remove_one_pointer_ext(int operand_ty, const SourceRange &source_range) {
  auto ty = global_get_type(operand_ty);
  int ptr_depth = 0;
  for (const auto &ext : ty->get_ext().extensions) {
    if (ext == TYPE_EXT_POINTER)
      ptr_depth++;
  }

  if (ptr_depth == 0) {
    throw_error("cannot dereference a non-pointer type.", source_range);
  }

  bool pointer_removed = false;
  std::vector<TypeExtEnum> extensions{};
  for (const auto &ext : ty->get_ext().extensions) {
    if (!pointer_removed && ext == TYPE_EXT_POINTER) {
      pointer_removed = true;
    } else {
      extensions.push_back(ext);
    }
  }
  return global_find_type_id(ty->get_base(),
                             TypeExt{.extensions = extensions,
                                     .array_sizes = ty->get_ext().array_sizes});
}
int global_create_struct_type(const std::string &name, Scope *scope) {
  auto type = new (type_alloc<Type>()) Type(num_types, TYPE_STRUCT);
  type_table[num_types] = type;
  type->set_base(name);
  auto info = type_alloc<StructTypeInfo>();
  info->scope = scope;
  type->set_info(info);
  num_types++;
  return type->id;
}
int global_create_union_type(const std::string &name, Scope *scope,
                             UnionFlags kind) {
  auto type = new (type_alloc<Type>()) Type(num_types, TYPE_UNION);
  type_table[num_types] = type;
  type->set_base(name);
  auto info = type_alloc<UnionTypeInfo>();
  info->flags = kind;
  info->scope = scope;
  type->set_info(info);
  num_types++;
  return type->id;
}
int global_create_enum_type(const std::string &name,
                            const std::vector<std::string> &keys,
                            bool is_flags) {
  auto id = num_types;
  auto type = new (type_alloc<Type>()) Type(id, TYPE_ENUM);
  type_table[num_types] = type;
  type->set_base(name);

  auto info = new (type_alloc<EnumTypeInfo>()) EnumTypeInfo();
  info->is_flags = is_flags;
  info->keys = keys;
  type->set_info(info);

  num_types += 1;
  return type->id;
}
int global_create_type(TypeKind kind, const std::string &name, TypeInfo *info,
                       const TypeExt &extensions) {
  Type *type = new (type_alloc<Type>()) Type(num_types, kind);
  type->set_info(info);
  type->set_ext(extensions);
  type->set_base(name);

  if (type->id > MAX_NUM_TYPES) {
    throw_error("Max types exceeded", {});
  }
  if (type_table[type->id]) {
    throw_error("type system created a type with the same ID twice", {});
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

  auto extensions = this->get_ext();
  for (auto it = extensions.extensions.rbegin();
       it != extensions.extensions.rend();) {
    if (*it == TYPE_EXT_ARRAY) {
      it = std::vector<TypeExtEnum>::reverse_iterator(
          extensions.extensions.erase((it + 1).base()));
      extensions.array_sizes.pop_back();
      return global_find_type_id(base, extensions);
    } else {
      ++it;
    }
  }
  
  return id;
}

// used for anonymous structs etc.
Token get_unique_identifier() {
  static int num = 0;
  auto tok = Token({}, "__anon_D" + std::to_string(num), TType::Identifier,
                   TFamily::Identifier);
  num++;
  return tok;
}

int char_type() {
  static int type = global_find_type_id("char", {});
  return type;
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

bool get_function_type_parameter_signature(Type *type, std::vector<int> &out) {
  out.clear();
  if (!type->is_kind(TYPE_FUNCTION)) {
    return false;
  }
  auto info = static_cast<FunctionTypeInfo *>(type->get_info());
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
void emit_warnings_or_errors_for_operator_overloads(const TType type,
                                                    SourceRange &range) {
  switch (type) {
  case TType::Assign:
  case TType::Range:
  case TType::Comma:
  case TType::Semi:
  case TType::Varargs:
  case TType::Directive:
  case TType::ColonEquals:
  case TType::New:
  case TType::Delete:
  case TType::Dollar:
  case TType::RParen:
  case TType::RBrace:
    throw_error("Operator overload not allowed", range);
  case TType::Arrow:
    throw_warning("Operator overload: Use '.' instead of '->'", range);
    return;

  // Valid
  case TType::Add:
  case TType::Sub:
  case TType::Mul:
  case TType::Div:
  case TType::Modulo:
  case TType::Not:
  case TType::BitwiseNot:
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
    throw_error(
        std::format("Invalid operator overload {}", TTypeToString(type)),
        range);
  }
}

int get_pointer_to_type(int base) {
  auto type = global_get_type(base);
  auto extensions = type->get_ext();
  extensions.extensions.push_back({TYPE_EXT_POINTER});
  return global_find_type_id(type->get_base(), extensions);
}

ScalarTypeInfo *create_scalar_type_info(ScalarType type, size_t size,
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
                       create_scalar_type_info(TYPE_S64, 8, true));
    global_create_type(TYPE_SCALAR, "s32",
                       create_scalar_type_info(TYPE_S32, 4, true));
    global_create_type(TYPE_SCALAR, "s16",
                       create_scalar_type_info(TYPE_S16, 2, true));
    global_create_type(TYPE_SCALAR, "s8",
                       create_scalar_type_info(TYPE_S16, 1, true));
  }

  // Unsigned integers
  {
    global_create_type(TYPE_SCALAR, "u64",
                       create_scalar_type_info(TYPE_U64, 8, true));
    global_create_type(TYPE_SCALAR, "u32",
                       create_scalar_type_info(TYPE_U32, 4, true));
    global_create_type(TYPE_SCALAR, "u16",
                       create_scalar_type_info(TYPE_U16, 2, true));
    global_create_type(TYPE_SCALAR, "u8",
                       create_scalar_type_info(TYPE_U16, 1, true));
  }

  // Floats
  {
    global_create_type(TYPE_SCALAR, "float32",
                       create_scalar_type_info(TYPE_FLOAT, 4));
    global_create_type(TYPE_SCALAR, "float64",
                       create_scalar_type_info(TYPE_DOUBLE, 8));
  }

  // Other
  {
    // Other
    // CLEANUP: alias these, don't generate new types.
    global_create_type(TYPE_SCALAR, "float",
                       create_scalar_type_info(TYPE_FLOAT, 4));
    global_create_type(TYPE_SCALAR, "int",
                       create_scalar_type_info(TYPE_S32, 4, true));

    global_create_type(TYPE_SCALAR, "char",
                       create_scalar_type_info(TYPE_U8, 1, true));
    global_create_type(TYPE_SCALAR, "bool",
                       create_scalar_type_info(TYPE_BOOL, 1, true));
    global_create_type(TYPE_SCALAR, "void",
                       create_scalar_type_info(TYPE_VOID, 0));
  }
}
bool type_is_numerical(const Type *t) {
  if (!t->is_kind(TYPE_SCALAR))
    return false;
  return t->id == char_type() || t->id == float_type() || t->id == int_type() ||
         t->id == s8_type() || t->id == s16_type() || t->id == s32_type() ||
         t->id == s64_type() || t->id == u8_type() || t->id == u16_type() ||
         t->id == u32_type() || t->id == u64_type() ||
         t->id == float32_type() || t->id == float64_type();
}

constexpr bool numerical_type_safe_to_upcast(const Type *from, const Type *to) {
  if (from->kind != TYPE_SCALAR || to->kind != TYPE_SCALAR)
    return false;

  auto from_info = static_cast<ScalarTypeInfo *>(from->get_info());
  auto to_info = static_cast<ScalarTypeInfo *>(to->get_info());

  // do not allow casting of float to integer implicitly
  if (!from_info->is_integral && to_info->is_integral) {
    return false;
  }

  return from_info->size <= to_info->size;
}

bool TypeExt::is_fixed_sized_array() const {
  for (const auto &ext : array_sizes) {
    if (ext.is_not_null()) {
      return true;
    }
  }
  return false;
}

std::string TypeExt::to_string() const {
  std::stringstream ss;
  auto array_sizes = this->array_sizes;
  
  for (const auto ext : extensions) {
    switch (ext) {
    case TYPE_EXT_POINTER:
      ss << "*";
      break;
    case TYPE_EXT_ARRAY: {
      auto size = array_sizes.back();
      array_sizes.pop_back();
      if (size.is_null())
        ss << "[]";
      else {
        ss << "[" << size << "]";
      }
    } break;
    case TYPE_EXT_MAP: {
      ss << "[" << global_get_type(key_type)->to_string() << "]";
    } break;
    } 
  }
  return ss.str();
}

int get_map_value_type(Type *map_type) { 
  // We assume that maps are only ever one deep right now.
  // Later we may come back and fix this up, just trying to get it working.
  auto id  = global_find_type_id(map_type->get_base(), map_type->get_ext().without_back());
  assert(!global_get_type(id)->get_ext().is_map());
  return id;
}
