#pragma once

#include <chrono>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <sstream>
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
  inline void print() const {
    std::cout << "\033[1;32mInput Path:\033[0m " << input_path << std::endl;
    std::cout << "\033[1;32mOutput Path:\033[0m " << output_path << std::endl;
    std::cout << "\033[1;32mBinary Path:\033[0m " << binary_path << std::endl;
    std::cout << "\033[1;32mFlags:\033[0m" << std::endl;
    for (const auto &flag : flags) {
      std::cout << "  \033[1;34m--" << flag.first << "\033[0m: " << (flag.second ? "true" : "false") << std::endl;
    }
  }
  inline void add_compilation_flag(const std::string &flags) {
    this->compilation_flags += flags;
    if (!this->compilation_flags.ends_with(' ')) {
      this->compilation_flags += ' ';
    }
  }
  inline CompileCommand(int argc, char *argv[]) {
    if (argc < 2) {
      printf(
          "\033[31mUsage: <input.ela> (optional)::[-o "
          "<output.cpp>] [--flag]\033[0m\n");
    }

    for (int i = 1; i < argc; ++i) {
      std::string arg = argv[i];
      if (arg == "-o" && i + 1 < argc) {
        output_path = argv[++i];
      } else if (arg.rfind("--", 0) == 0) {
        flags[arg.substr(2)] = true;
      } else {
        input_path = arg;
      }
    }

    if (input_path.empty()) {
      printf("\033[31mError: No input file specified.\033[0m\n");
      exit(1);
    }

    std::filesystem::path input_fs_path(input_path);

    if (!std::filesystem::exists(input_fs_path)) {
      printf("%s\n", (std::format("\033[31mError: File '{}' does not exist.\033[0m", input_path.string())).c_str());
      exit(1);
    }

    std::filesystem::path parent_path = input_fs_path.parent_path();
    if (!parent_path.empty() && !std::filesystem::exists(parent_path)) {
      printf("%s\n",
             std::format("\033[31mError: Parent directory '{}' does not exist.\033[0m", parent_path.string()).c_str());
      exit(1);
    }

    binary_path = input_fs_path.stem().string();

    if (output_path.empty()) {
      std::string filename = input_fs_path.filename().string();
      size_t pos = filename.rfind(".ela");
      if (pos != std::string::npos) {
        filename.replace(pos, 4, ".cpp");
      } else {
        filename += ".cpp";
      }
      output_path = filename;
    }
  }
  inline std::string read_input_file() {
    std::ifstream stream(std::filesystem::canonical(input_path));
    std::stringstream ss;
    ss << stream.rdbuf();
    if (ss.str().empty()) {
      printf("%s\n", std::format("\033[31mError: {} is empty.\033[0m", input_path.string()).c_str());
      exit(1);
    }
    return ss.str();
  }
  bool has_flag(const std::string &flag) const;
  int compile();
};

extern CompileCommand compile_command;
extern std::vector<Token> all_tokens;

struct SourceRange {
  inline bool empty() const {
    return begin < 0 || end < 0 || begin > end || begin == end || begin > all_tokens.size() || end > all_tokens.size();
  }
  int64_t begin, end;
  int64_t begin_loc;
  std::vector<Token> get_tokens() const {
    int64_t valid_begin = std::clamp(begin, int64_t(0), int64_t(all_tokens.size()));
    int64_t valid_end = std::clamp(end, valid_begin, int64_t(all_tokens.size()));
    if (valid_end - valid_begin == 0) {
      valid_begin = std::max(int64_t(0), valid_begin - 2);
      valid_end = std::min(int64_t(all_tokens.size()), valid_end + 3);
    }
    return std::vector<Token>(all_tokens.begin() + valid_begin, all_tokens.begin() + valid_end);
  }
};

static std::string get_source_filename(const SourceRange &range) {
  auto tokens = range.get_tokens();
  if (tokens.empty()) {
    return "";
  }
  auto token = tokens[0];
  auto location = token.location;
  return location.files()[location.file];
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