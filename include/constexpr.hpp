#pragma once
#include "error.hpp"
#include "interned_string.hpp"

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
  };

  bool is_truthy() {
    switch (tag){
    case INTEGER:
      return integer;
    case FLOATING:
      return floating;
    case BOOLEAN:
      return boolean;
      break;
    }
  }

  static Value Int(const InternedString& str) {
    Value val;
    val.tag = INTEGER;
    val.integer = std::stoll(str.get_str());
    return val;
  }

  static Value Float(const InternedString& str) {
    Value val;
    val.tag = FLOATING;
    val.floating = std::stod(str.get_str());
    return val;
  }

  static Value Bool(const InternedString& str) {
    Value val;
    val.tag = BOOLEAN;
    val.boolean = (str.get_str() == "true");
    return val;
  }


  Value operator-() {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = -integer};
    } else if (tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = -floating};
    }
    throw_error("Invalid type for unary minus", {});
  }

  Value operator!() {
    if (tag == INTEGER) {
      integer = !integer;
      return *this;
    } else if (tag == FLOATING) {
      floating = !floating;
      return *this;
    } else if (tag == BOOLEAN) {
      boolean = !boolean;
    }
    throw_error("Invalid type for pre-increment", {});
  }

  Value operator~() const {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = ~integer};
    }
    throw_error("Invalid type for bitwise not", {});
  }

  Value operator+(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer + other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = floating + other.floating};
    }
    throw_error("Invalid types for addition", {});
  }

  Value operator-(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer - other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = floating - other.floating};
    }
    throw_error("Invalid types for subtraction", {});
  }

  Value operator*(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer * other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = floating * other.floating};
    }
    throw_error("Invalid types for multiplication", {});
  }

  Value operator/(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer / other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = FLOATING, .floating = floating / other.floating};
    }
    throw_error("Invalid types for division", {});
  }

  Value operator%(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer % other.integer};
    }
    throw_error("Invalid types for modulo", {});
  }

  Value operator!() const {
    if (tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = !boolean};
    }
    throw_error("Invalid type for logical not", {});
  }

  Value operator~() {
    if (tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = ~integer};
    }
    throw_error("Invalid type for bitwise not", {});
  }

  Value operator|(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer | other.integer};
    }
    throw_error("Invalid types for bitwise or", {});
  }

  Value operator&(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer & other.integer};
    }
    throw_error("Invalid types for bitwise and", {});
  }

  Value operator<<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer << other.integer};
    }
    throw_error("Invalid types for shift left", {});
  }

  Value operator>>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer >> other.integer};
    }
    throw_error("Invalid types for shift right", {});
  }

  Value operator^(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = INTEGER, .integer = integer ^ other.integer};
    }
    throw_error("Invalid types for bitwise xor", {});
  }

  Value operator||(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean || other.boolean};
    }
    throw_error("Invalid types for logical or", {});
  }

  Value operator&&(const Value &other) const {
    if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean && other.boolean};
    }
    throw_error("Invalid types for logical and", {});
  }

  Value operator<(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer < other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating < other.floating};
    }
    throw_error("Invalid types for less than comparison", {});
  }

  Value operator>(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer > other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating > other.floating};
    }
    throw_error("Invalid types for greater than comparison", {});
  }

  Value operator==(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer == other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating == other.floating};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean == other.boolean};
    }
    throw_error("Invalid types for equality comparison", {});
  }

  Value operator!=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer != other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating != other.floating};
    } else if (tag == BOOLEAN && other.tag == BOOLEAN) {
      return Value{.tag = BOOLEAN, .boolean = boolean != other.boolean};
    }
    throw_error("Invalid types for inequality comparison", {});
  }

  Value operator<=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer <= other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating <= other.floating};
    }
    throw_error("Invalid types for less than or equal comparison", {});
  }

  Value operator>=(const Value &other) const {
    if (tag == INTEGER && other.tag == INTEGER) {
      return Value{.tag = BOOLEAN, .boolean = integer >= other.integer};
    } else if (tag == FLOATING && other.tag == FLOATING) {
      return Value{.tag = BOOLEAN, .boolean = floating >= other.floating};
    }
    throw_error("Invalid types for greater than or equal comparison", {});
  }

};

Value evaluate_constexpr(ASTExpr *node, Context &ctx);