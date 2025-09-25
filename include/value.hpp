#pragma once

#include <map>
#include <string>
#include <vector>
#include <utility>
#include "arena.hpp"
#include "interned_string.hpp"
#include "type.hpp"

extern jstl::Arena value_arena;

enum class ValueType {
  NULLPTR = 0,
  FLOATING = 1,
  BOOLEAN = 2,
  STRING = 3,
  CHARACTER = 4,
  INTEGER = 5,
  FUNCTION = 6,
  OBJECT = 7,
  ARRAY = 8,

  // Internal to interpreter.
  EXTERN_FUNCTION = 9,
  RETURN = 10,
};

template <typename T, typename... Args>
inline T* value_arena_alloc(Args&&... args) {
  void* mem = value_arena.allocate(sizeof(T));
  return new (mem) T(std::forward<Args>(args)...);
}

struct ASTNode;
struct Value {
  const ValueType value_type = ValueType::NULLPTR;
  Value(ValueType vt = ValueType::NULLPTR) : value_type(vt) {}
  Value() = delete;
  virtual ~Value() {}

  template <class T>
  inline T* as() const {
    return (T*)this;
  }

  virtual bool is_truthy() const = 0;
  virtual ValueType get_value_type() const;
  virtual ASTNode* to_ast() const { return nullptr; }

  virtual std::string to_string() const;
};

struct IntValue : Value {
  size_t value;
  IntValue(size_t val = 0) : Value(ValueType::INTEGER), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct FloatValue : Value {
  double value;
  FloatValue(double val = 0.0) : Value(ValueType::FLOATING), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct BoolValue : Value {
  bool value;
  BoolValue(bool val = false) : Value(ValueType::BOOLEAN), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct StringValue : Value {
  std::string value;
  StringValue(const std::string& str = "") : Value(ValueType::STRING), value(str) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct CharValue : Value {
  char value;
  CharValue(char c = '\0') : Value(ValueType::CHARACTER), value(c) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct NullValue : Value {
  NullValue() : Value(ValueType::NULLPTR) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

// a void or non-void return value.
struct ReturnValue : Value {
  ReturnValue() : Value(ValueType::RETURN) {}
  ReturnValue(Value* value) : Value(ValueType::RETURN) { this->value = value; }
  bool is_truthy() const override { return false; }
  ValueType get_value_type() const override { return value_type; }
  Nullable<Value> value = nullptr;
};

const static NullValue* SHARED_NULL_VALUE = new NullValue();
const static ReturnValue* SHARED_RETURN_VOID_VALUE = new ReturnValue();

struct ObjectValue : Value {
  Type* type = nullptr;
  std::map<InternedString, Value*> values {};
  ObjectValue(Type* t = nullptr) : Value(ValueType::OBJECT), type(t) {}

  bool is_truthy() const override;

  ValueType get_value_type() const override;

  ASTNode* to_ast() const override;
};

struct ArrayValue : Value {
  Type* type = nullptr;
  std::vector<Value*> values {};
  ArrayValue(Type* type, const std::vector<Value*>& arr) : Value(ValueType::ARRAY), type(type), values(arr) {}
  ArrayValue(Type* type) : Value(ValueType::ARRAY), type(type), values({}) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  ASTNode* to_ast() const override;
};

struct ASTBlock;
struct ASTParamsDecl;

struct CTInterpreter;
struct FunctionValue : Value {
  ASTBlock* block;
  ASTParamsDecl* parameters;
  FunctionValue() : Value(ValueType::FUNCTION) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
  Value* call(CTInterpreter* interpreter, std::vector<Value*> arguments);
};

struct ExternFunctionValue : Value {
  InternedString name;
  FunctionTypeInfo* info;

  ExternFunctionValue(InternedString name, FunctionTypeInfo* info)
      : Value(ValueType::EXTERN_FUNCTION), name(name), info(info) {}

  bool is_truthy() const override { return false; }
  ValueType get_value_type() const override { return value_type; }
};

IntValue* new_int(const InternedString& str);
IntValue* new_int(size_t val);
FloatValue* new_float(const InternedString& str);
FloatValue* new_float(double val);
BoolValue* new_bool(const InternedString& str);
BoolValue* new_bool(bool val);
StringValue* new_string(const InternedString& str);
StringValue* new_string(const std::string& str);
CharValue* new_char(char val);
NullValue* null_value();
ObjectValue* new_object(Type* type);
FunctionValue* new_function();
ArrayValue* new_array(Type* type, const std::vector<Value*>& arr);
ArrayValue* new_array(Type* type);
ReturnValue* return_value(Value* value);
ReturnValue* return_value();

struct CTInterpreter;
Value* default_value_of_t(Type* t, CTInterpreter*);