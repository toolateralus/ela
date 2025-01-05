#pragma once

#include <sstream>

#include "core.hpp"
#include "lex.hpp"

enum ErrorSeverity {
  ERROR_INFO,
  ERROR_WARNING,
  ERROR_FAILURE,
};

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
static bool supports_color() {
  if (!_isatty(_fileno(stdout))) {
    return false;
  }
  HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hOut == INVALID_HANDLE_VALUE) {
    return false;
  }
  DWORD dwMode = 0;
  if (!GetConsoleMode(hOut, &dwMode)) {
    return false;
  }
  dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if (!SetConsoleMode(hOut, dwMode)) {
    return false;
  }
  return true;
}

#else
#include <unistd.h>
static bool supports_color() {
  if (!isatty(STDOUT_FILENO)) {
    return false;
  }
  const char *term = getenv("TERM");
  if (term == NULL) return false;
  return strstr(term, "color") != NULL || strstr(term, "xterm") != NULL || strstr(term, "screen") != NULL ||
         strstr(term, "tmux") != NULL;
}
#endif

static bool terminal_supports_color = supports_color();

static std::string format_message(const std::string &message) {
  std::stringstream formatted;
  int width = 0;
  int size = 0;
  for (char ch : message) {
    formatted << ch;
    if (ch == '\n') {
      formatted << '\t';
    }
    if (width > 50 && message.length() > size + 1 && std::isspace(message[size + 1])) {
      formatted << "\n\t";
      width = 0;
    }
    size++;
    width++;
  }
  return formatted.str();
}
static std::string format_source_location(const SourceRange &source_range, ErrorSeverity severity) {
  const char *color = "";
  const char *code_color = "";

  auto span = source_range.get_tokens();

  if (terminal_supports_color) {
    switch (severity) {
      case ERROR_INFO:
        color = "\033[36m";         // Cyan
        code_color = "\033[1;36m";  // Bold Cyan
        break;
      case ERROR_WARNING:
        color = "\033[33m";         // Yellow
        code_color = "\033[1;33m";  // Bold Yellow
        break;
      case ERROR_FAILURE:
        color = "\033[31m";         // Red
        code_color = "\033[1;31m";  // Bold Red
        break;
    }
  }

  // nocheckin
  if (span.empty()) {
    return "Error: No tokens in source range\n";
  }

  std::stringstream ss;
  if (terminal_supports_color) ss << "\033[90m";
  ss << std::string(80, '-');
  if (terminal_supports_color) ss << "\033[0m\n";
  ss << '\n';
  ss << color << span.front().location.ToString() << (terminal_supports_color ? "\033[0m\n" : "\n");
  if (terminal_supports_color) ss << "\033[90m";
  ss << std::string(80, '-');
  if (terminal_supports_color) ss << "\033[0m";
  ss << '\n';

  // Read the source file
  auto first = span.front();
  auto last = span.back();

  std::ifstream src_file(SourceLocation::files()[first.location.file]);
  if (!src_file.is_open()) {
    return "Error: Unable to open source file\n";
  }

  std::string file_content((std::istreambuf_iterator<char>(src_file)), std::istreambuf_iterator<char>());
  src_file.close();

  std::stringstream source_code;
  std::string caret_indicator;

  size_t line_start = 0;
  size_t line_end = 0;
  size_t current_line = 1;

  for (size_t i = 0; i < file_content.size(); ++i) {
    if (file_content[i] == '\n') {
      current_line++;
      if (current_line == first.location.line) {
        line_start = i + 1;
      } else if (current_line == first.location.line + 1) {
        line_end = i;
        break;
      }
    }
  }

  if (line_end == 0) {
    line_end = file_content.size();
  }

  std::string line = file_content.substr(line_start, line_end - line_start);
  source_code << line << '\n';

  caret_indicator = std::string(first.location.column - 1, ' ');
  caret_indicator += std::string(first.value.get_str().length(), '^');

  if (terminal_supports_color) ss << "\033[34m";

  ss << source_code.str();

  if (terminal_supports_color) ss << "\033[32m";

  ss << caret_indicator << '\n';

  if (terminal_supports_color) ss << "\033[0m";

  return ss.str();
}
enum WarningFlags {
  WarningNone = 0,
  WarningUseDotNotArrowOperatorOverload = 1 << 0,  // --Wno-arrow-operator
  WarningInaccessibleDeclaration = 1 << 1,         // --Wno-inaccessible-decl
  WarningEmptyStringInterpolation = 1 << 2,        // --Wno-empty-string-interp
  WarningNonNullDeletedPointer = 1 << 3,           // --Wno-non-null-deleted
  WarningAmbigousVariants = 1 << 4,                // --Wno-amiguous-variant
  WarningSwitchBreak = 1 << 5,                     // --Wno-switch-break
  WarningDownCastFixedArrayParam = 1 << 6,         // --Wno-array-param
  WarningIgnoreAll = 1 << 7,                       // --Wignore-all
};

extern int ignored_warnings;

static void throw_warning(const WarningFlags id, const std::string message, const SourceRange &source_range) {
  if ((ignored_warnings & id) != 0 || (ignored_warnings & WarningIgnoreAll) != 0) {
    return;
  }
  std::stringstream ss;
  if (terminal_supports_color) ss << "\033[36m";
  ss << "Warning:\n\t" << format_message(message);
  if (terminal_supports_color) ss << "\033[0m\n";
  ss << format_source_location(source_range, ERROR_WARNING);
  const auto token_str = ss.str();
  std::cerr << token_str << std::endl;
}

[[noreturn]] static void throw_error(const std::string &message, const SourceRange &source_range) {
  std::stringstream ss;
  if (terminal_supports_color) ss << "\033[31m";
  ss << "Error:\n\t" << message;
  if (terminal_supports_color) ss << "\033[0m\n";
  ss << format_source_location(source_range, ERROR_FAILURE);
  const auto token_str = ss.str();
  printf("%s\n", token_str.c_str());
  exit(1);
}
