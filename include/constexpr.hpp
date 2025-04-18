#pragma once
#include "error.hpp"
#include "interned_string.hpp"
#include "type.hpp"

struct Context;
struct ASTExpr;

struct Value {
  enum {
    INTEGER,
    FLOATING,
    BOOLEAN,
  } tag;

  struct {
    int integer;
    float floating;
    bool boolean;

    // ! We should add this for manipulating compile time structs.s
    // ! We'll just replace every usage of it with an initializer list, or something.
    // struct { 
    //   int type = Type::INVALID_TYPE;
    //   std::unordered_map<InternedString, Value> values;
    // } $struct;
  };

  bool is_truthy() {
    switch (tag) {
      case INTEGER:
        return integer;
      case FLOATING:
        return floating;
      case BOOLEAN:
        return boolean;
        break;
    }
  }

  static Value Int(const InternedString &str) {
    Value val;
    val.tag = INTEGER;
    val.integer = std::stoll(str.get_str());
    return val;
  }

  static Value Float(const InternedString &str) {
    Value val;
    val.tag = FLOATING;
    val.floating = std::stod(str.get_str());
    return val;
  }

  static Value Bool(const InternedString &str) {
    Value val;
    val.tag = BOOLEAN;
    val.boolean = (str.get_str() == "true");
    return val;
  }
  Value operator-() const {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = -integer};
    } else if (tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = -floating};
    }
    throw_error("Invalid type for unary minus", {});
    return {};
  }

  Value operator!() const {
    if (tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = !boolean};
    }
    throw_error("Invalid type for logical not", {});
    return {};
  }

  Value operator~() const {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = ~integer};
    }
    throw_error("Invalid type for bitwise not", {});
    return {};
  }

  Value operator+(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer + other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) +
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for addition", {});
    return {};
  }

  Value operator-(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer - other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) -
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for subtraction", {});
    return {};
  }

  Value operator*(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer * other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) *
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for multiplication", {});
    return {};
  }

  Value operator/(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer / other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = FLOATING,
                   .floating = (tag == FLOATING ? floating : integer) /
                               (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for division", {});
    return {};
  }

  Value operator%(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer % other.integer};
    }
    throw_error("Invalid types for modulo", {});
    return {};
  }

  Value operator|(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer | other.integer};
    }
    throw_error("Invalid types for bitwise or", {});
    return {};
  }

  Value operator&(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer & other.integer};
    }
    throw_error("Invalid types for bitwise and", {});
    return {};
  }

  Value operator<<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer << other.integer};
    }
    throw_error("Invalid types for shift left", {});
    return {};
  }

  Value operator>>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer >> other.integer};
    }
    throw_error("Invalid types for shift right", {});
    return {};
  }

  Value operator^(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer ^ other.integer};
    }
    throw_error("Invalid types for bitwise xor", {});
    return {};
  }

  Value operator||(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean || other.boolean};
    }
    throw_error("Invalid types for logical or", {});
    return {};
  }

  Value operator&&(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean && other.boolean};
    }
    throw_error("Invalid types for logical and", {});
    return {};
  }

  Value operator<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer < other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) <
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for less than comparison", {});
    return {};
  }

  Value operator>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer > other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) >
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for greater than comparison", {});
    return {};
  }

  Value operator==(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer == other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) ==
                              (other.tag == FLOATING ? other.floating : other.integer)};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean == other.boolean};
    }
    throw_error("Invalid types for equality comparison", {});
    return {};
  }

  Value operator!=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer != other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) !=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean != other.boolean};
    }
    throw_error("Invalid types for inequality comparison", {});
    return {};
  }

  Value operator<=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer <= other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) <=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for less than or equal comparison", {});
    return {};
  }

  Value operator>=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer >= other.integer};
    } else if (tag == FLOATING || other.tag == FLOATING) {
      return Value{.tag = BOOLEAN,
                   .boolean = (tag == FLOATING ? floating : integer) >=
                              (other.tag == FLOATING ? other.floating : other.integer)};
    }
    throw_error("Invalid types for greater than or equal comparison", {});
    return {};
  }
};

Value evaluate_constexpr(ASTExpr *node, Context &ctx);