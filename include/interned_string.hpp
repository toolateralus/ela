#pragma once
#include <cassert>
#include <cstddef>
#include <cstring>
#include <format>
#include <functional>
#include <string>
#include <unordered_map>

struct InternedString {
  size_t hash;
  using InternedStringTable = std::unordered_map<size_t, std::string>;
  InternedString() = default;
  static InternedStringTable &table() {
    static InternedStringTable table;
    return table;
  }
  std::string get_str() const { return {table()[hash]}; }
  inline void insert_or_set(const std::string &value) {
    auto hash = std::hash<std::string>()(value);
    const auto [it, inserted] = table().emplace(hash, value);
    this->hash = it->first;
  }
  inline InternedString(const std::string &value) { insert_or_set(value); }
  inline InternedString(const char *str) { insert_or_set(std::string{str}); }
  inline bool operator==(const InternedString &other) const { return hash == other.hash; }
  inline bool operator!=(const InternedString &other) const { return hash != other.hash; }
  inline bool operator<(const InternedString &other) const { return hash < other.hash; }
};

namespace std {
template <>
struct hash<InternedString> {
  inline size_t operator()(const InternedString &string) const { return string.hash; }
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
    return std::format_to(ctx.out(), "{}", s.get_str());
  }
};
}  // namespace std
