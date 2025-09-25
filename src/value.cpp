#include "value.hpp"
#include "ast.hpp"

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

IntValue* IntV(const InternedString& str) { return arena_alloc<IntValue>(std::stoll(str.get_str())); }
FloatValue* FloatV(const InternedString& str) { return arena_alloc<FloatValue>(std::stod(str.get_str())); }
BoolValue* BoolV(const InternedString& str) { return arena_alloc<BoolValue>(str.get_str() == "true"); }
IntValue* IntV(size_t val) { return arena_alloc<IntValue>(val); }
FloatValue* FloatV(double val) { return arena_alloc<FloatValue>(val); }
BoolValue* BoolV(bool val) { return arena_alloc<BoolValue>(val); }
StringValue* StringV(const InternedString& str) { return arena_alloc<StringValue>(str.get_str()); }
StringValue* StringV(const std::string& str) { return arena_alloc<StringValue>(str); }
CharValue* CharV(char val) { return arena_alloc<CharValue>(val); }
NullValue* NullV() { return (NullValue*)SHARED_NULL_VALUE; }
ObjectValue* ObjectV(Type* type) { return arena_alloc<ObjectValue>(type); }
FunctionValue* FunctionV() { return arena_alloc<FunctionValue>(); }
ArrayValue* ArrayV(const std::vector<Value*>& arr) { return arena_alloc<ArrayValue>(arr); }

ReturnValue* ReturnV(Value* value) { return arena_alloc<ReturnValue>(value); }
ReturnValue* ReturnV() { return (ReturnValue*)SHARED_RETURN_VOID_VALUE; }

#include "constexpr.hpp"

Value* FunctionValue::Call(CTInterpreter* interpreter, std::vector<Value*> arguments) {
  auto it = arguments.begin();
  for (const auto param : this->parameters->params) {
    if (param->tag == ASTParamDecl::Normal) {
      interpreter->set_value(param->normal.name, *it);
    }
  }
  auto return_value = interpreter->visit_block(block)->as<ReturnValue>();

  if (return_value->value) {
    return return_value->value.get();
  }

  return NullV();
}
