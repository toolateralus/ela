#pragma once

#include "lex.hpp"
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <sstream>
#include <filesystem>
#include <unordered_map>

#define MB(n) (n * 1024 * 1024)
#define GB(n) (n * 1024 * 1024 * 1024)

struct ASTProgram;
struct Context;

struct CompileCommand {
  CompileCommand() = default;
  std::filesystem::path input_path;
  std::filesystem::path output_path;
  std::filesystem::path binary_path;
  std::filesystem::path original_path; // where the compiler was invoked from
  std::unordered_map<std::string, bool> flags;
  std::string compilation_flags;
  inline void print() const {
    std::cout << "\e[1;32mInput Path:\e[0m " << input_path << std::endl;
    std::cout << "\e[1;32mOutput Path:\e[0m " << output_path << std::endl;
    std::cout << "\e[1;32mBinary Path:\e[0m " << binary_path << std::endl;
    std::cout << "\e[1;32mFlags:\e[0m" << std::endl;
    for (const auto &flag : flags) {
      std::cout << "  \e[1;34m--" << flag.first
                << "\e[0m: " << (flag.second ? "true" : "false") << std::endl;
    }
  }
  inline void add_compilation_flags(const std::string &flags)  {
    this->compilation_flags += flags;
    if (!this->compilation_flags.ends_with(' ')) {
      this->compilation_flags += ' ';
    }
  }
  inline CompileCommand(int argc, char *argv[]) {
    if (argc < 2) {
      throw std::invalid_argument("\e[31mUsage: <input.ela> (optional)::[-o "
                                  "<output.cpp>] [--flag]\e[0m");
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
      throw std::invalid_argument(
          "\e[31mError: No input file specified.\e[0m");
    }

    std::filesystem::path input_fs_path(input_path);
    if (!std::filesystem::exists(input_fs_path)) {
      throw std::runtime_error(std::format(
          "\e[31mError: File '{}' does not exist.\e[0m", input_path.string()));
    }

    std::filesystem::path parent_path = input_fs_path.parent_path();
    if (!parent_path.empty() && !std::filesystem::exists(parent_path)) {
      throw std::runtime_error(std::format(
          "\e[31mError: Parent directory '{}' does not exist.\e[0m",
          parent_path.string()));
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
      throw std::runtime_error(std::format("\e[31mError: {} is empty.\e[0m", input_path.string()));
    }
    return ss.str();
  }
  ASTProgram *process_ast(Context &context);
  void emit_code(ASTProgram *root, Context &context);
  bool has_flag(const std::string &flag) const;
  void compile();
};

extern CompileCommand compile_command;
bool get_compilation_flag(const std::string &flag);
extern std::vector<Token> all_tokens;

struct SourceRange {
  
  
  inline bool empty() const {
    return begin < 0 || end < 0 ||
           begin > end ||
           begin == end ||
           begin > all_tokens.size() ||
           end > all_tokens.size(); 
  }
  int64_t begin, end;
  int64_t begin_loc, end_loc;
  std::span<Token> get_tokens() const {
      int64_t valid_begin = std::clamp(begin, int64_t(0), int64_t(all_tokens.size()));
      int64_t valid_end = std::clamp(end, valid_begin, int64_t(all_tokens.size()));
      return std::span<Token>(all_tokens).subspan(valid_begin, valid_end - valid_begin);
  }
};

struct Defer {
  const std::function<void()> func;
  Defer(const std::function<void()> &&func) : func(func){
    
  } 
  ~Defer() {
    func();
  }
};