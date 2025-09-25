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

IntValue* IntV(const InternedString& str) { return value_arena_alloc<IntValue>(std::stoll(str.get_str())); }
FloatValue* FloatV(const InternedString& str) { return value_arena_alloc<FloatValue>(std::stod(str.get_str())); }
BoolValue* BoolV(const InternedString& str) { return value_arena_alloc<BoolValue>(str.get_str() == "true"); }
IntValue* IntV(size_t val) { return value_arena_alloc<IntValue>(val); }
FloatValue* FloatV(double val) { return value_arena_alloc<FloatValue>(val); }
BoolValue* BoolV(bool val) { return value_arena_alloc<BoolValue>(val); }
StringValue* StringV(const InternedString& str) { return value_arena_alloc<StringValue>(str.get_str()); }
StringValue* StringV(const std::string& str) { return value_arena_alloc<StringValue>(str); }
CharValue* CharV(char val) { return value_arena_alloc<CharValue>(val); }
NullValue* NullV() { return (NullValue*)SHARED_NULL_VALUE; }
ObjectValue* ObjectV(Type* type) { return value_arena_alloc<ObjectValue>(type); }
FunctionValue* FunctionV() { return value_arena_alloc<FunctionValue>(); }
ArrayValue* ArrayV(const std::vector<Value*>& arr) { return value_arena_alloc<ArrayValue>(arr); }

ReturnValue* ReturnV(Value* value) { return value_arena_alloc<ReturnValue>(value); }
ReturnValue* ReturnV() { return (ReturnValue*)SHARED_RETURN_VOID_VALUE; }

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