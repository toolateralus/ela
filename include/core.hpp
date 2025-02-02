#pragma once

#include <chrono>
#include <filesystem>
#include <format>
#include <functional>
#include <iostream>
#include <unordered_map>

#include "lex.hpp"

#define MB(n) (n * 1024 * 1024)
#define GB(n) (n * 1024 * 1024 * 1024)

// simple pointer wrapper that expresses intent about whether a pointer is optional or not
// without a nasty allocating wrapper like std::optional (which is also annoying to type)
template <class T>
struct Nullable {
  ~Nullable() = default;
  Nullable() {}
  Nullable(T *ptr) : ptr(ptr) {}
  T *ptr{};
  T *get() const { return ptr; }
  void set(T *ptr) { this->ptr = ptr; }
  operator bool() const { return ptr; }
  bool is_null() const { return ptr == nullptr; }
  bool is_not_null() const { return ptr != nullptr; }
};

struct ASTProgram;
struct Context;

struct CompilationMetric {
  std::string id;
  std::chrono::time_point<std::chrono::high_resolution_clock> start_time;
  std::chrono::time_point<std::chrono::high_resolution_clock> end_time;
  std::chrono::duration<double> duration;
  void begin() { start_time = std::chrono::high_resolution_clock::now(); }
  void end(const std::string &note) {
    id = note;
    end_time = std::chrono::high_resolution_clock::now();
    duration = end_time - start_time;
  }

  std::string get_time() {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
    if (ms >= 1000) {
      auto sec = std::chrono::duration_cast<std::chrono::seconds>(duration).count();
      return std::to_string(sec) + " s";
    } else if (ms >= 1) {
      return std::to_string(ms) + " ms";
    } else {
      auto us = std::chrono::duration_cast<std::chrono::microseconds>(duration).count();
      return std::to_string(us) + " µs";
    }
  }
};

struct CompileCommand {
  void print_metrics() {
    if (has_flag("metrics")) {
      std::cout << "\033[1;36m" << parse.id << "\033[0m " << "\033[1;32m" << parse.get_time() << "\033[0m\n";
      std::cout << "\033[1;36m" << lower.id << "\033[0m " << "\033[1;32m" << lower.get_time() << "\033[0m\n";
      std::cout << "\033[1;36m" << cpp.id << "\033[0m " << "\033[1;32m" << cpp.get_time() << "\033[0m\n";
    }
  }

  CompilationMetric parse;
  CompilationMetric lower;
  CompilationMetric cpp;

  CompileCommand() = default;
  std::filesystem::path input_path;
  std::filesystem::path output_path;
  std::filesystem::path binary_path;
  std::filesystem::path original_path;  // where the compiler was invoked from
  std::unordered_map<std::string, bool> flags;
  std::string compilation_flags;

  void print() const;
  void add_compilation_flag(const std::string &flags);
  CompileCommand(int argc, char *argv[]);
  std::string read_input_file();
  bool has_flag(const std::string &flag) const;
  int compile();
  void setup_ignored_warnings();
};

extern CompileCommand compile_command;

struct SourceRange {
  SourceLocation begin_location;
};

static std::string get_source_filename(const SourceRange &range) {
  return SourceLocation::files()[range.begin_location.file];
}

struct Defer {
  const std::function<void()> func;
  Defer(const std::function<void()> &&func) : func(func) {}
  ~Defer() { func(); }
};

template <class T>
struct std::formatter<std::vector<T>> {
  constexpr auto parse(std::format_parse_context &context) { return context.begin(); }
  auto format(const std::vector<T> &sVal, std::format_context &context) const {
    auto out = context.out();
    out = std::format_to(out, "[");
    for (auto it = sVal.begin(); it != sVal.end(); ++it) {
      if (it != sVal.begin()) {
        out = std::format_to(out, ", ");
      }
      out = std::format_to(out, "{}", *it);
    }
    out = std::format_to(out, "]");
    return out;
  }
};


template <class T>
requires(std::is_enum_v<T>)
struct Flags {
  using underlying = std::underlying_type_t<T>;
  Flags() = default;
  Flags(T value) : value(static_cast<underlying>(value)) {}
  Flags(underlying value) : value(value) {}

  inline Flags &operator|=(T other) {
    value |= static_cast<underlying>(other);
    return *this;
  }
  inline Flags &operator&=(T other) {
    value &= static_cast<underlying>(other);
    return *this;
  }
  inline Flags operator|(T other) const {
    return Flags(value | static_cast<underlying>(other));
  }
  inline Flags operator&(T other) const {
    return Flags(value & static_cast<underlying>(other));
  }

  inline Flags &operator|=(underlying other) {
    value |= other;
    return *this;
  }
  inline Flags &operator&=(underlying other) {
    value &= other;
    return *this;
  }
  inline Flags operator|(underlying other) const {
    return Flags(value | other);
  }
  inline Flags operator&(underlying other) const {
    return Flags(value & other);
  }
  inline explicit operator bool() const {
    return value != 0;
  }
  underlying value{};
};
