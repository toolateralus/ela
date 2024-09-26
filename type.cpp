#include "type.hpp"
#include <sstream>

std::string FunctionTypeInfo::to_string() const {
  std::stringstream ss;
  ss << get_type(return_type)->name;
  ss << "(";
  for (int i = 0; i < params_len; ++i) {
    auto t = get_type(parameter_types[i]);
    ss << t->to_string();
    if (i < params_len - 1) {
      ss << ", ";
    }
  }
  ss << ')';
  return ss.str();
}