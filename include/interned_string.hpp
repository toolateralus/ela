#pragma once
#include <cassert>
#include <cstddef>
#include <cstring>
#include <format>
#include <functional>
#include <string>
#include <unordered_map>
#include <memory>

struct InternedString {
  std::string *str_ptr;
  InternedString() = default;

  using InternedStringTable = std::unordered_map<size_t, std::unique_ptr<std::string>>;
  static InternedStringTable &table() {
    static InternedStringTable table;
    return table;
  }

  std::string str() const { return *str_ptr; }
  
  const char *c_str() const { return str_ptr->c_str(); }

  inline void insert_or_set(const std::string &value) {
    auto hash = std::hash<std::string>()(value);
    auto &tbl = table();
    auto it = tbl.find(hash);
    if (it == tbl.end()) {
      auto [new_it, inserted] = tbl.emplace(hash, std::make_unique<std::string>(value));
      str_ptr = new_it->second.get();
    } else {
      str_ptr = it->second.get();
    }
  }

  inline InternedString(const std::string &value) { insert_or_set(value); }
  inline InternedString(const char *str) { insert_or_set(std::string{str}); }
  inline InternedString(std::nullptr_t) {}

  inline bool operator==(const InternedString &other) const { return str_ptr == other.str_ptr; }
  inline bool operator!=(const InternedString &other) const { return str_ptr != other.str_ptr; }
  inline bool operator<(const InternedString &other) const { return str_ptr < other.str_ptr; }
};

namespace std {
template <>
struct hash<InternedString> {
  inline size_t operator()(const InternedString &string) const { return (size_t)string.str_ptr; }
};

template <>
struct formatter<InternedString, char> {
  template <class ParseContext>
  constexpr typename ParseContext::iterator parse(ParseContext &ctx) {
    auto it = ctx.begin();
    auto end = ctx.end();
    while (it != end && *it != '}') {
      ++it;
    }
    return it;
  }

  template <class FormatContext>
  typename FormatContext::iterator format(const InternedString &s, FormatContext &ctx) const {
    return std::format_to(ctx.out(), "{}", s.str());
  }
};
}  // namespace std