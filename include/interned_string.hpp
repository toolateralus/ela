#pragma once

#include <cstring>
#include <limits>
#include <string>
#include <unordered_map>

static inline size_t hash_string(const char *string) {
  size_t hash = 0;
  while (*string) {
    hash = hash * 31 + static_cast<size_t>(*string++);
  }
  return hash;
}

struct InternedString {
  int id;
  int m_length;
  InternedString() {
    id = std::numeric_limits<int>::min();
  }
  inline static std::unordered_map<int, const char*> &id_to_str() {
    static std::unordered_map<int, const char*> table;
    return table;
  };
  inline InternedString(const InternedString& other) {
    id = other.id;
    m_length = other.m_length;
  }
  inline InternedString(const std::string &string) {
    m_length = string.length();
    id = hash_string(string.data());
    if (id_to_str().contains(id)) {
      return;
    }
    char *data = new char[m_length]; // TODO: fix memory leak.
    id_to_str()[id] = data;
  }
  inline InternedString(const char *str) {
    m_length = strlen(str); // todo: could probably save a ton by not doing this.
    id = hash_string(str);
    if (id_to_str().contains(id)) {
      return;
    }
    id_to_str()[id] = str;
  }
  inline bool operator==(const char *string) const {
    return hash_string(string) == id;
  }
  inline bool operator==(const InternedString &string) const {
    return id == string.id;
  }
  inline bool operator==(const std::string &string) const {
    return hash_string(string.data()) == id;
  }
  inline size_t size() const {
    return m_length;
  }
  inline size_t length() const {
    return m_length;
  }
  inline bool contains(const InternedString& other) const {
    if (*this == other) return true;
    auto data = id_to_str()[id];
    auto other_data = id_to_str()[other.id];
    return strstr(data, other_data) != nullptr;
  }
  
  inline InternedString operator+(const InternedString &other) const {
    std::string combined_str = std::string(id_to_str()[id]) + std::string(id_to_str()[other.id]);
    return InternedString(combined_str);
  }
  inline InternedString operator+(const char *other) const {
    std::string combined_str = std::string(id_to_str()[id]) + std::string(other);
    return InternedString(combined_str);
  }

  inline InternedString operator+(const std::string &other) const {
    std::string combined_str = std::string(id_to_str()[id]) + other;
    return InternedString(combined_str);
  }
  
  inline std::string str() const { return {id_to_str()[id]}; }
};

#include <format>

template <>
struct std::formatter<InternedString> : std::formatter<std::string> {
  auto format(const InternedString& internedStr, std::format_context& ctx) {
    return std::formatter<std::string>::format(internedStr.str(), ctx);
  }
};

namespace std {
  template <>
  struct hash<InternedString> {
    size_t operator()(const InternedString &internedStr) const {
      return static_cast<size_t>(internedStr.id);
    }
  };
}