#pragma once

#include <string>
#include <vector>
#include <utility>
#include "arena.hpp"
#include "interned_string.hpp"
#include "type.hpp"

extern jstl::Arena value_arena;

enum class ValueType {
  INTEGER,
  FLOATING,
  BOOLEAN,
  STRING,
  CHARACTER,
  NULLPTR,
  FUNCTION,
  OBJECT,
  ARRAY,
};

template <typename T, typename... Args>
inline T* arena_alloc(Args&&... args) {
  void* mem = value_arena.allocate(sizeof(T));
  return new (mem) T(std::forward<Args>(args)...);
}

struct Value {
  ValueType value_type = ValueType::NULLPTR;
  Value(ValueType vt = ValueType::NULLPTR) : value_type(vt) {}
  virtual ~Value() {}

  template<class T>
  inline T *as() const {
    return (T*)this;
  }

  virtual bool is_truthy() const = 0;
  virtual ValueType get_value_type() const;

  static Value* Int(const InternedString& str);
  static Value* Float(const InternedString& str);
  static Value* Bool(const InternedString& str);
  static Value* Int(size_t val);
  static Value* Float(double val);
  static Value* Bool(bool val);
  static Value* String(const InternedString& str);
  static Value* String(const std::string& str);
  static Value* Char(char val);
  static Value* Null();
  static Value* Object(Type* type);
  static Value* Function();
  static Value* Array(const std::vector<Value*>& arr);
};

struct IntValue : Value {
  size_t value;
  IntValue(size_t val = 0) : Value(ValueType::INTEGER), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct FloatValue : Value {
  double value;
  FloatValue(double val = 0.0) : Value(ValueType::FLOATING), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct BoolValue : Value {
  bool value;
  BoolValue(bool val = false) : Value(ValueType::BOOLEAN), value(val) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct StringValue : Value {
  std::string value;
  StringValue(const std::string& str = "") : Value(ValueType::STRING), value(str) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct CharValue : Value {
  char value;
  CharValue(char c = '\0') : Value(ValueType::CHARACTER), value(c) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct NullValue : Value {
  NullValue() : Value(ValueType::NULLPTR) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

const static NullValue *SHARED_NULL_VALUE = new NullValue();

struct ObjectValue : Value {
  Type* type;
  ObjectValue(Type* t = nullptr) : Value(ValueType::OBJECT), type(t) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct FunctionValue : Value {
  FunctionValue() : Value(ValueType::FUNCTION) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

struct ArrayValue : Value {
  std::vector<Value*> value;
  ArrayValue(const std::vector<Value*>& arr = {}) : Value(ValueType::ARRAY), value(arr) {}
  bool is_truthy() const override;
  ValueType get_value_type() const override;
};

