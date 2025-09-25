#include "value.hpp"

Value* Value::Int(const InternedString& str) { return arena_alloc<IntValue>(std::stoll(str.get_str())); }
Value* Value::Float(const InternedString& str) { return arena_alloc<FloatValue>(std::stod(str.get_str())); }
Value* Value::Bool(const InternedString& str) { return arena_alloc<BoolValue>(str.get_str() == "true"); }
Value* Value::Int(size_t val) { return arena_alloc<IntValue>(val); }
Value* Value::Float(double val) { return arena_alloc<FloatValue>(val); }
Value* Value::Bool(bool val) { return arena_alloc<BoolValue>(val); }
Value* Value::String(const InternedString& str) { return arena_alloc<StringValue>(str.get_str()); }
Value* Value::String(const std::string& str) { return arena_alloc<StringValue>(str); }
Value* Value::Char(char val) { return arena_alloc<CharValue>(val); }
Value* Value::Null() { return (Value*)SHARED_NULL_VALUE; }
Value* Value::Object(Type* type) { return arena_alloc<ObjectValue>(type); }
Value* Value::Function() { return arena_alloc<FunctionValue>(); }
Value* Value::Array(const std::vector<Value*>& arr) { return arena_alloc<ArrayValue>(arr); }

ValueType ArrayValue::get_value_type() const { return ValueType::ARRAY; }
bool ArrayValue::is_truthy() const { return !value.empty(); }
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
