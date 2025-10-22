#pragma once

#include <llvm/Passes/OptimizationLevel.h>
#include <chrono>
#include <filesystem>
#include <functional>
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
  void end() {
    end_time = std::chrono::high_resolution_clock::now();
    duration = end_time - start_time;
  }

  template <class T>
  T run(const std::string &note, std::function<T()> fn) {
    id = std::move(note);
    begin();
    auto result = fn();
    end();
    return result;
  }

  template <>
  void run(const std::string &note, std::function<void()> fn) {
    id = std::move(note);
    begin();
    fn();
    end();
  }

  inline std::string get_time() {
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

  inline std::string to_string() { return "\033[1;36m" + id + "\033[0m " + "\033[1;32m" + get_time() + "\033[0m\n"; }
};

struct Span;
struct CompileCommand {
  CompilationMetric parse_metric;
  CompilationMetric typing_metric;
  CompilationMetric thir_gen_metric;
  CompilationMetric mir_gen_metric;
  CompilationMetric llvm_gen_metric;
  CompilationMetric llvm_opt_metric;
  CompilationMetric clang_invocation_metric;

  CompileCommand() = default;
  std::filesystem::path input_path;
  std::filesystem::path output_path;
  std::filesystem::path binary_path;
  std::filesystem::path original_path;  // where the compiler was invoked from
  std::unordered_map<std::string, bool> flags;
  llvm::OptimizationLevel opt_level = llvm::OptimizationLevel::O0;
  std::string opt_level_arg;
  std::string c_flags;

  int compile();
  void print_command() const;
  void add_c_flag(const std::string &flags);
  CompileCommand(const std::vector<std::string> &args, std::vector<std::string> &runtime_args, bool &run_on_finished,
                 bool &run_tests, bool &lldb);
  bool has_flag(const std::string &flag) const;

  void print_metrics() {
    const static CompilationMetric metrics[] = {
        parse_metric, typing_metric, thir_gen_metric, mir_gen_metric, llvm_gen_metric, llvm_opt_metric, clang_invocation_metric,
    };
    for (auto metric : metrics) {
      const std::string string = metric.to_string();
      fprintf(stdout, "%s\n", string.c_str());
    }
  }

  void request_compile_time_code_execution(const Span &range);
};

extern CompileCommand compile_command;

static inline std::string get_source_filename(const Span &range) { return Span::files()[range.file]; }

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
  inline Flags operator|(T other) const { return Flags(value | static_cast<underlying>(other)); }
  inline Flags operator&(T other) const { return Flags(value & static_cast<underlying>(other)); }

  inline Flags &operator|=(underlying other) {
    value |= other;
    return *this;
  }
  inline Flags &operator&=(underlying other) {
    value &= other;
    return *this;
  }
  inline Flags operator|(underlying other) const { return Flags(value | other); }
  inline Flags operator&(underlying other) const { return Flags(value & other); }
  inline explicit operator bool() const { return value != 0; }
  underlying value{};
};

#define HAS_FLAG(var, flag) ((var & flag) != 0)
#define DOESNT_HAVE_FLAG(var, flag) ((var & flag) == 0)

static inline size_t calculate_strings_actual_length(const std::string_view &str_view) {
  size_t length = 0;
  for (size_t i = 0; i < str_view.size(); ++i) {
    if (str_view[i] == '\\' && i + 1 < str_view.size()) {
      switch (str_view[i + 1]) {
        case 'n':
        case 't':
        case 'r':
        case '\\':
        case '"':
          ++i;  // Skip the escape character
          break;
        case 'x':  // Hexadecimal escape sequence
          i += 2;  // Skip \x and the next two hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'u':  // Unicode escape sequence
          i += 4;  // Skip \u and the next four hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        case 'U':  // Unicode escape sequence
          i += 8;  // Skip \U and the next eight hex digits
          while (i + 1 < str_view.size() && std::isxdigit(str_view[i + 1])) {
            ++i;
          }
          break;
        default:
          if (str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {  // Octal escape sequence
            ++i;                                                   // Skip the first digit
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i;  // Skip the second digit
            }
            if (i + 1 < str_view.size() && str_view[i + 1] >= '0' && str_view[i + 1] <= '7') {
              ++i;  // Skip the third digit
            }
          }
          break;
      }
    }
    ++length;
  }
  return length;
}

#define ENTER_SCOPE($new_scope)       \
  const auto $old_scope_ = ctx.scope; \
  ctx.scope = $new_scope;             \
  const Defer $scope_defer([&] { ctx.scope = $old_scope_; });

#define SOURCE_LOCATION() (std::format("{}:{}", __FILE__, __LINE__))
