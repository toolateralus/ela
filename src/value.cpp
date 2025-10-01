#include "value.hpp"
#include <cstring>
#include "ast.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "thir.hpp"

ValueType ArrayValue::get_value_type() const { return ValueType::ARRAY; }

ValueType FunctionValue::get_value_type() const { return ValueType::FUNCTION; }

ValueType ObjectValue::get_value_type() const { return ValueType::OBJECT; }
ValueType Value::get_value_type() const { return value_type; }
ValueType IntValue::get_value_type() const { return ValueType::INTEGER; }
ValueType FloatValue::get_value_type() const { return ValueType::FLOATING; }
ValueType BoolValue::get_value_type() const { return ValueType::BOOLEAN; }
ValueType StringValue::get_value_type() const { return ValueType::STRING; }
ValueType CharValue::get_value_type() const { return ValueType::CHARACTER; }
ValueType NullValue::get_value_type() const { return ValueType::NULLPTR; }

bool FunctionValue::is_truthy() const { return true; }
bool ArrayValue::is_truthy() const { return true; }
bool ObjectValue::is_truthy() const { return true; }
bool NullValue::is_truthy() const { return false; }
bool CharValue::is_truthy() const { return value != '\0'; }
bool IntValue::is_truthy() const { return value != 0; }
bool FloatValue::is_truthy() const { return value != 0.0; }
bool BoolValue::is_truthy() const { return value; }
bool StringValue::is_truthy() const { return !value.empty(); }

PointerValue* new_pointer(Value** value) { return value_arena_alloc<PointerValue>((*value)->value_type, value); }
CharValue* new_char(char val) { return value_arena_alloc<CharValue>(val); }
IntValue* new_int(const InternedString& str) { return value_arena_alloc<IntValue>(std::stoll(str.get_str())); }
IntValue* new_int(size_t val) { return value_arena_alloc<IntValue>(val); }
FloatValue* new_float(const InternedString& str) { return value_arena_alloc<FloatValue>(std::stod(str.get_str())); }
FloatValue* new_float(double val) { return value_arena_alloc<FloatValue>(val); }
BoolValue* new_bool(const InternedString& str) { return value_arena_alloc<BoolValue>(str.get_str() == "true"); }
BoolValue* new_bool(bool val) { return value_arena_alloc<BoolValue>(val); }
StringValue* new_string(const InternedString& str) { return value_arena_alloc<StringValue>(str.get_str()); }
StringValue* new_string(const std::string& str) { return value_arena_alloc<StringValue>(str); }
ArrayValue* new_array(Type* type, const std::vector<Value*>& arr) { return value_arena_alloc<ArrayValue>(type, arr); }
ArrayValue* new_array(Type* type) { return value_arena_alloc<ArrayValue>(type); }
NullValue* null_value() { return (NullValue*)SHARED_NULL_VALUE; }
LValue* null_lvalue() {
  static Value* null_val = null_value();
  static LValue* null_lval = new_lvalue(&null_val);
  return null_lval;
}
ObjectValue* new_object(Type* type) { return value_arena_alloc<ObjectValue>(type); }
FunctionValue* new_function() { return value_arena_alloc<FunctionValue>(); }
ReturnValue* return_value(Value* value) { return value_arena_alloc<ReturnValue>(value); }
ReturnValue* return_value() { return (ReturnValue*)SHARED_RETURN_VOID_VALUE; }

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

#include "thir_interpreter.hpp"

Value* FunctionValue::call(Interpreter* interpreter, std::vector<Value*> arguments) {
  if (!block) {
    return null_value();
  }

  auto it = arguments.begin();

  // TODO: attach types to parameters in THIR.
  for (const auto& param : parameters) {
    interpreter->write_to_lvalue(param.associated_variable, *it);
    ++it;
  }

  auto value = interpreter->visit_block(block);

  if (value->is(ValueType::RETURN)) {
    auto return_value = value->as<ReturnValue>();
    if (return_value->value) {
      return return_value->value.get();
    }
  }
  return null_value();
}

THIR* IntValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Integer;
  literal->type = s64_type();
  return literal;
}

THIR* FloatValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Float;
  literal->type = f64_type();
  return literal;
}

THIR* BoolValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = value ? "true" : "false";
  literal->tag = ASTLiteral::Bool;
  literal->type = bool_type();
  return literal;
}

THIR* StringValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = value;
  literal->tag = ASTLiteral::String;
  literal->type = u8_ptr_type();  // TODO: support str and String better
  return literal;
}

THIR* CharValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = std::to_string(value);
  literal->tag = ASTLiteral::Char;
  literal->type = char_type();
  return literal;
}

THIR* NullValue::to_thir() const {
  auto literal = thir_alloc<THIRLiteral>();
  literal->value = "null";
  literal->tag = ASTLiteral::Null;
  literal->type = void_type()->take_pointer_to(true);
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

Value* default_value_of_fixed_array_of_t(Type* base_type, size_t size, Interpreter* interpreter) {
  auto array = new_array({});
  for (size_t i = 0; i < size; ++i) {
    array->values.push_back(default_value_of_t(base_type, interpreter));
  }
  return array;
}

Value* default_value_of_struct_t(Type* type, StructTypeInfo* info, Interpreter* interpreter) {
  auto object = new_object(type);
  for (const auto& member : info->members) {
    if (member.thir_value.is_not_null()) {
      object->values[member.name] = interpreter->visit_node(member.thir_value.get());
    } else {
      object->values[member.name] = default_value_of_t(member.type, interpreter);
    }
  }
  return object;
}

Value* default_value_of_tuple_t(Type* type, TupleTypeInfo* info, Interpreter* interpreter) {
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
  object->values[DISCRIMINANT_KEY] = 0;
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

Value* default_value_of_t(Type* t, Interpreter* interpreter) {
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

THIR* ObjectValue::to_thir() const {
  THIRAggregateInitializer* init = thir_alloc<THIRAggregateInitializer>();
  init->type = type;
  for (const auto& [key, value] : this->values) {
    if (type->is_kind(TYPE_DYN) && key == "instance") {
      continue;
    }
    init->key_values.push_back({key, value->to_thir()});
  }
  return init;
}

THIR* ArrayValue::to_thir() const {
  THIRCollectionInitializer* init = thir_alloc<THIRCollectionInitializer>();
  init->type = type;
  for (const auto& value : this->values) {
    init->values.push_back(value->to_thir());
  }
  return init;
}

bool RawPointerValue::is_truthy() const { return ptr != nullptr; }

ValueType RawPointerValue::get_value_type() const { return value_type; }

THIR* RawPointerValue::to_thir() const {
  if (type->is_kind(TYPE_SCALAR)) {
    auto info = type->info->as<ScalarTypeInfo>();
    if (info->scalar_type == TYPE_CHAR) {
      auto literal = thir_alloc<THIRLiteral>();
      literal->tag = ASTLiteral::Char;
      literal->value = std::string(1, *(char*)ptr);
      literal->type = char_type();
      return literal;
    }
    if (info->scalar_type == TYPE_BOOL) {
      auto literal = thir_alloc<THIRLiteral>();
      literal->tag = ASTLiteral::Bool;
      literal->value = (*(bool*)ptr) ? "true" : "false";
      literal->type = bool_type();
      return literal;
    }
    if (info->scalar_type == TYPE_S64 || info->scalar_type == TYPE_U64) {
      auto literal = thir_alloc<THIRLiteral>();
      literal->tag = ASTLiteral::Integer;
      literal->value = std::to_string(*(size_t*)ptr);
      literal->type = s64_type();
      return literal;
    }
    if (info->scalar_type == TYPE_FLOAT || info->scalar_type == TYPE_DOUBLE) {
      auto literal = thir_alloc<THIRLiteral>();
      literal->tag = ASTLiteral::Float;
      literal->value = std::to_string(*(double*)ptr);
      literal->type = f64_type();
      return literal;
    }
  }
  throw_error(
      "You cannot pass pointers out of the compile time code into runtime code-- pointers can't exist in a binary. "
      "Just strings, chars, and scalars can.",
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

  if (!inner) {
    // This is a non-pointer raw pointer.
    // this shouldn't happen but right now we have to do this,
    // for returning lvalue's to elements during pointer subscript, etc.
    inner = type;
  }

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

void RawPointerValue::assign_from(Value* v) {
  switch (v->get_value_type()) {
    case ValueType::NULLPTR:
      ptr = nullptr;
      break;
    case ValueType::INTEGER:
      memcpy(ptr, &v->as<IntValue>()->value, sizeof(size_t));
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

    // Not sure how we're going to handle these cases.
    case ValueType::ARRAY:
    case ValueType::POINTER:
    case ValueType::RAW_POINTER:
      break;
    default:
      break;
  }
}

FunctionValue::FunctionValue() : Value(ValueType::FUNCTION) {}

Value* FunctionValue::dyn_dispatch(const InternedString& method_name, Interpreter* interpreter,
                                   std::vector<Value*> arguments) {
  Value *arg0 = arguments[0];

  if (arg0->is(ValueType::POINTER)) {
    arg0 = *arg0->as<PointerValue>()->ptr;
  }

  if (!arg0->is(ValueType::OBJECT)) {
    throw_error(std::format("'dyn' object was ({}), so a dynamic dispatch failed at compile time for method: {}", arg0->to_string(), method_name), {});
  }

  ObjectValue *self = arg0->as<ObjectValue>();
  PointerValue *function_pointer = self->values[method_name]->as<PointerValue>();

  if (function_pointer->pointee_value_type != ValueType::FUNCTION) {
    throw_error("cannot call a non-pointer function with dyn dispatch at compile time. this is likely a bug", {});
  }

  FunctionValue *function = (*function_pointer->ptr)->as<FunctionValue>();

  return function->call(interpreter, arguments);
}

THIR* PointerValue::to_thir() const {
  if (pointee_value_type == ValueType::FUNCTION) {
    auto function = (*ptr)->as<FunctionValue>();
    auto variable = thir_alloc<THIRVariable>();
    variable->name = function->name;
    variable->type = function->type;
    variable->use_compile_time_value_at_emit_time = false;
    return variable;
  }
  return null_value()->to_thir();
}
