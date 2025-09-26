#include "value.hpp"
#include <cstring>
#include "ast.hpp"
#include "type.hpp"

ValueType ArrayValue::get_value_type() const { return ValueType::ARRAY; }
bool ArrayValue::is_truthy() const { return !values.empty(); }
bool FunctionValue::is_truthy() const { return true; }
ValueType FunctionValue::get_value_type() const { return ValueType::FUNCTION; }
bool ObjectValue::is_truthy() const { return true; }
ValueType ObjectValue::get_value_type() const { return ValueType::OBJECT; }
ValueType Value::get_value_type() const { return value_type; }
ValueType IntValue::get_value_type() const { return ValueType::INTEGER; }
ValueType FloatValue::get_value_type() const { return ValueType::FLOATING; }
ValueType BoolValue::get_value_type() const { return ValueType::BOOLEAN; }
ValueType StringValue::get_value_type() const { return ValueType::STRING; }
ValueType CharValue::get_value_type() const { return ValueType::CHARACTER; }
ValueType NullValue::get_value_type() const { return ValueType::NULLPTR; }

bool NullValue::is_truthy() const { return false; }
bool CharValue::is_truthy() const { return value != '\0'; }
bool IntValue::is_truthy() const { return value != 0; }
bool FloatValue::is_truthy() const { return value != 0.0; }
bool BoolValue::is_truthy() const { return value; }
bool StringValue::is_truthy() const { return !value.empty(); }

PointerValue* new_pointer(Value** value) { return value_arena_alloc<PointerValue>((*value)->value_type, value); }
IntValue* new_int(const InternedString& str) { return value_arena_alloc<IntValue>(std::stoll(str.get_str())); }
FloatValue* new_float(const InternedString& str) { return value_arena_alloc<FloatValue>(std::stod(str.get_str())); }
BoolValue* new_bool(const InternedString& str) { return value_arena_alloc<BoolValue>(str.get_str() == "true"); }
IntValue* new_int(size_t val) { return value_arena_alloc<IntValue>(val); }
FloatValue* new_float(double val) { return value_arena_alloc<FloatValue>(val); }
BoolValue* new_bool(bool val) { return value_arena_alloc<BoolValue>(val); }
StringValue* new_string(const InternedString& str) { return value_arena_alloc<StringValue>(str.get_str()); }
StringValue* new_string(const std::string& str) { return value_arena_alloc<StringValue>(str); }
CharValue* new_char(char val) { return value_arena_alloc<CharValue>(val); }
NullValue* null_value() { return (NullValue*)SHARED_NULL_VALUE; }
ObjectValue* new_object(Type* type) { return value_arena_alloc<ObjectValue>(type); }
FunctionValue* new_function() { return value_arena_alloc<FunctionValue>(); }
ArrayValue* new_array(Type* type, const std::vector<Value*>& arr) { return value_arena_alloc<ArrayValue>(type, arr); }
ArrayValue* new_array(Type* type) { return value_arena_alloc<ArrayValue>(type); }

ReturnValue* return_value(Value* value) { return value_arena_alloc<ReturnValue>(value); }
ReturnValue* return_value() { return (ReturnValue*)SHARED_RETURN_VOID_VALUE; }

#include "constexpr.hpp"

Value* FunctionValue::call(CTInterpreter* interpreter, std::vector<Value*> arguments) {
  auto it = arguments.begin();
  auto temp_scope = create_child(block->scope->parent);

  for (const auto param : this->parameters->params) {
    if (param->tag == ASTParamDecl::Normal) {
      temp_scope->insert_local_variable(param->normal.name, param->resolved_type, nullptr, MUT);
      auto symbol = temp_scope->local_lookup(param->normal.name);
      symbol->value = *it;
      ++it;
    } else {
      temp_scope->insert_local_variable("self", param->resolved_type, nullptr, param->mutability);
      auto symbol = temp_scope->local_lookup("self");
      symbol->value = *it;
    }
  }

  auto old_scope = block->scope;
  block->scope = temp_scope;
  auto value = interpreter->visit_block(block);
  block->scope = old_scope;

  return value;
}
ASTNode* IntValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Integer;
  return literal;
}
ASTNode* FloatValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Float;
  return literal;
}
ASTNode* BoolValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = value ? "true" : "false";
  literal->tag = ASTLiteral::Bool;
  return literal;
}

ASTNode* StringValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = value;
  literal->tag = ASTLiteral::String;
  return literal;
}

ASTNode* CharValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Char;
  return literal;
}

ASTNode* NullValue::to_ast() const {
  auto literal = ast_alloc<ASTLiteral>();
  literal->value = "null";
  literal->tag = ASTLiteral::Null;
  return literal;
}

static inline std::string unescape_string_lit(const std::string& s) {
  std::string res;
  std::istringstream iss(s);
  char c;
  while (iss.get(c)) {
    if (c == '\\') {
      char next = iss.peek();
      switch (next) {
        case 'e':
          res += '\x1B';
          iss.get();
          break;
        case 'n':
          res += '\n';
          iss.get();
          break;
        case 't':
          res += '\t';
          iss.get();
          break;
        case 'r':
          res += '\r';
          iss.get();
          break;
        case 'f':
          res += '\f';
          iss.get();
          break;
        case 'v':
          res += '\v';
          iss.get();
          break;
        case 'a':
          res += '\a';
          iss.get();
          break;
        case 'b':
          res += '\b';
          iss.get();
          break;
        case '\\':
          res += '\\';
          iss.get();
          break;
        case '\'':
          res += '\'';
          iss.get();
          break;
        case '\"':
          res += '\"';
          iss.get();
          break;
        case '\?':
          res += '\?';
          iss.get();
          break;
        case '0':
          res += '\0';
          iss.get();
          break;
        default:
          // If the character following the backslash is not a recognized escape
          // sequence, append the backslash followed by the character itself.
          res += '\\';
          res += next;
          iss.get();
          break;
      }
    } else {
      res += c;
    }
  }
  return res;
}

std::string Value::to_string() const {
  switch (this->get_value_type()) {
    case ValueType::INTEGER:
      return std::to_string(((const IntValue*)this)->value);
    case ValueType::FLOATING:
      return std::to_string(((const FloatValue*)this)->value);
    case ValueType::BOOLEAN:
      return (((const BoolValue*)this)->value ? "true" : "false");
    case ValueType::CHARACTER: {
      char buf[2] = {((const CharValue*)this)->value, 0};
      return std::string(buf);
    }
    case ValueType::STRING: {
      return unescape_string_lit(((const StringValue*)this)->value);
    }
    case ValueType::NULLPTR:
      return "null";
    case ValueType::OBJECT:
    case ValueType::FUNCTION:
    case ValueType::ARRAY:
    default:
      return "(unsupported)";
  }
}

Value* default_value_of_scalar_t(ScalarType type) {
  switch (type) {
    case TYPE_VOID:
      return null_value();
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
      return new_int(0);
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
      return new_float(0);
    case TYPE_CHAR:
      return new_char(0);
    case TYPE_BOOL:
      return new_bool(false);
      break;
  }
  return null_value();
}

Value* default_value_of_fixed_array_of_t(Type* base_type, size_t size, CTInterpreter* interpreter) {
  auto array = new_array({});
  for (size_t i = 0; i < size; ++i) {
    array->values.push_back(default_value_of_t(base_type, interpreter));
  }
  return array;
}

Value* default_value_of_struct_t(Type* type, StructTypeInfo* info, CTInterpreter* interpreter) {
  auto object = new_object(type);
  for (const auto& member : info->members) {
    if (member.default_value.is_not_null()) {
      object->values[member.name] = interpreter->visit(member.default_value.get());
    } else {
      object->values[member.name] = default_value_of_t(member.type, interpreter);
    }
  }
  return object;
}

Value* default_value_of_tuple_t(Type* type, TupleTypeInfo* info, CTInterpreter* interpreter) {
  auto object = new_object(type);
  for (size_t i = 0; i < info->types.size(); ++i) {
    Type* type = info->types[i];
    object->values[std::format("${}", i)] = default_value_of_t(type, interpreter);
  }
  return object;
}

Value* default_value_of_choice_t(Type* type, ChoiceTypeInfo*) {
  auto object = new_object(type);
  // 0 is always the invalid out of bounds discriminant for choice types.
  // for interpreted choice types, we will just ignore initializing variants, only one can exist,
  // so only one shall exist ever.
  object->values["index"] = 0;
  return object;
}

Value* default_value_of_dyn_t(Type* type, DynTypeInfo* info) {
  auto object = new_object(type);
  object->values["instance"] = null_value();
  for (const auto& [name, _] : info->methods) {
    object->values[name] = null_value();
  }
  return object;
}

Value* default_value_of_t(Type* t, CTInterpreter* interpreter) {
  if (t->is_pointer()) {
    return null_value();
  }

  if (t->is_fixed_sized_array()) {
    return default_value_of_fixed_array_of_t(t->base_type, t->extensions.back().array_size, interpreter);
  }

  switch (t->kind) {
    case TYPE_SCALAR: {
      auto info = t->info->as<ScalarTypeInfo>();
      return default_value_of_scalar_t(info->scalar_type);
    } break;
    case TYPE_STRUCT:
      return default_value_of_struct_t(t, t->info->as<StructTypeInfo>(), interpreter);
    case TYPE_TUPLE:
      return default_value_of_tuple_t(t, t->info->as<TupleTypeInfo>(), interpreter);
    case TYPE_ENUM:
      return default_value_of_scalar_t(TYPE_S64);
    case TYPE_CHOICE:
      return default_value_of_choice_t(t, t->info->as<ChoiceTypeInfo>());
    case TYPE_DYN:
      return default_value_of_dyn_t(t, t->info->as<DynTypeInfo>());
    case TYPE_FUNCTION:
    case TYPE_TRAIT:
      return null_value();
  }
}

ASTNode* ObjectValue::to_ast() const {
  ASTInitializerList* init = ast_alloc<ASTInitializerList>();
  init->resolved_type = type;
  // TODO: figure out if we need this type allocated.
  init->target_type = ast_alloc<ASTType>();
  init->target_type.get()->resolved_type = type;
  init->tag = ASTInitializerList::INIT_LIST_NAMED;
  for (const auto& [key, value] : this->values) {
    init->key_values.push_back({key, (ASTExpr*)value->to_ast()});
  }
  return init;
}

ASTNode* ArrayValue::to_ast() const {
  ASTInitializerList* init = ast_alloc<ASTInitializerList>();
  init->resolved_type = type;
  // TODO: figure out if we need this type allocated.
  init->target_type = ast_alloc<ASTType>();
  init->target_type.get()->resolved_type = type->base_type;  // pass the base type to array initializers
  init->tag = ASTInitializerList::INIT_LIST_COLLECTION;
  for (const auto& value : this->values) {
    init->values.push_back((ASTExpr*)value->to_ast());
  }
  return init;
}

bool RawPointerValue::is_truthy() const { return ptr != nullptr; }

ValueType RawPointerValue::get_value_type() const { return value_type; }

ASTNode* RawPointerValue::to_ast() const {
  // Obviously strings can be built at compile time.
  if (type->is_kind(TYPE_SCALAR)) {
    auto info = type->info->as<ScalarTypeInfo>();
    if (info->scalar_type == TYPE_CHAR) {
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::String;
      literal->is_c_string = true;
      literal->value = std::string(ptr);
      return literal;
    }
  }
  // TODO: maybe convert pointers to static arrays at compile time? idk.
  throw_error(
      "You cannot pass pointers out of the compile time code into runtime code-- pointers cant exist in a binary. "
      "Just strings can.",
      {});
  return nullptr;
}

ContinueValue* continue_value() { return value_arena_alloc<ContinueValue>(); }
BreakValue* break_value() { return value_arena_alloc<BreakValue>(); }

Value* RawPointerValue::dereference() const {
  if (!ptr) {
    return null_value();
  }

  auto inner = type->base_type;

  switch (inner->kind) {
    case TYPE_SCALAR: {
      const auto sinfo = inner->info->as<ScalarTypeInfo>();
      switch (sinfo->scalar_type) {
        case TYPE_VOID:
          return (Value*)this;
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_S64:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
          return new_int(*(size_t*)ptr);
        case TYPE_FLOAT:
        case TYPE_DOUBLE:
          return new_float(*(double*)ptr);
        case TYPE_CHAR:
          return new_float(*(char*)ptr);
        case TYPE_BOOL:
          return new_float(*(bool*)ptr);
          break;
      }
    } break;
    // TODO: handle all cases here.
    default:
      break;
  }

  return null_value();
}

LValue* new_lvalue(Value** managed) {
  auto lvalue = value_arena_alloc<LValue>();
  lvalue->kind = LValue::MANAGED;
  lvalue->managed = managed;
  return lvalue;
}

LValue* new_lvalue(RawPointerValue* raw) {
  auto lvalue = value_arena_alloc<LValue>();
  lvalue->kind = LValue::RAW;
  lvalue->raw = raw;
  return lvalue;
}

void RawPointerValue::assign_from(Value* v) {
  switch (v->get_value_type()) {
    case ValueType::NULLPTR:
      ptr = nullptr;
      break;
    case ValueType::FLOATING:
      memcpy(ptr, &v->as<FloatValue>()->value, sizeof(double));
      break;
    case ValueType::BOOLEAN:
      memcpy(ptr, &v->as<BoolValue>()->value, sizeof(int));
      break;
    case ValueType::STRING:
      strcpy(ptr, v->as<StringValue>()->value.c_str());
      break;
    case ValueType::CHARACTER:
      *ptr = v->as<CharValue>()->value;
      break;
    case ValueType::INTEGER:
      memcpy(ptr, &v->as<IntValue>()->value, sizeof(size_t));
      break;

    // Not sure how we're going to handle these cases.
    case ValueType::ARRAY:
    case ValueType::POINTER:
    case ValueType::RAW_POINTER:
      break;
    default:
      break;
  }
}
