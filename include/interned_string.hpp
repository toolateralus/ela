#pragma once
#include <cassert>
#include <cstddef>
#include <cstring>
#include <format>
#include <functional>
#include <string>
#include <unordered_map>
#include <memory>

struct Interned_String {
  size_t hash;

#ifdef DEBUG
  std::string *str_ptr; // TODO: Remove this once we're done debugging the compiler
#endif
  using InternedStringTable = std::unordered_map<size_t, std::unique_ptr<std::string>>;

  Interned_String() = default;

  static InternedStringTable &table() {
    static InternedStringTable table;
    return table;
  }

  std::string get_str() const { 
  #ifdef DEBUG
    return *str_ptr;
  #else 
    return *table()[hash];
  #endif
  }

  inline void insert_or_set(const std::string &value) {
    hash = std::hash<std::string>()(value);
    auto &tbl = table();
    auto it = tbl.find(hash);
    if (it == tbl.end()) {
      auto [new_it, inserted] = tbl.emplace(hash, std::make_unique<std::string>(value));
      #ifdef DEBUG
      str_ptr = new_it->second.get();
      #endif
    } else {
      #ifdef DEBUG
      str_ptr = it->second.get();
      #endif
    }
  }

  inline Interned_String(const std::string &value) { insert_or_set(value); }
  inline Interned_String(const char *str) { insert_or_set(std::string{str}); }

  inline bool operator==(const Interned_String &other) const { return hash == other.hash; }
  inline bool operator!=(const Interned_String &other) const { return hash != other.hash; }
  inline bool operator<(const Interned_String &other) const { return hash < other.hash; }
};

namespace std {
template <> struct hash<Interned_String> {
  inline size_t operator()(const Interned_String &string) const {
    return string.hash;
  }
};

template <> struct formatter<Interned_String, char> {
  template <class ParseContext> constexpr typename ParseContext::iterator parse(ParseContext &ctx) {
    auto it = ctx.begin();
    auto end = ctx.end();
    while (it != end && *it != '}') {
      ++it;
    }
    return it;
  }

  template <class FormatContext>
  typename FormatContext::iterator format(const Interned_String &s, FormatContext &ctx) const {
    return std::format_to(ctx.out(), "{}", s.get_str());
  }
};
} // namespace std