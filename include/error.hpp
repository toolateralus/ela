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
  if (term == NULL)
    return false;

  return strstr(term, "dumb") == nullptr || (strstr(term, "color") != NULL || strstr(term, "xterm") != NULL ||
                                             strstr(term, "screen") != NULL || strstr(term, "tmux") != NULL);
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
    if (width > 80 && message.length() > size + 1 && std::isspace(message[size + 1])) {
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

  if (terminal_supports_color) {
    switch (severity) {
      case ERROR_INFO:
        color = "\033[36m";        // Cyan
        code_color = "\033[1;36m"; // Bold Cyan
        break;
      case ERROR_WARNING:
        color = "\033[33m";        // Yellow
        code_color = "\033[1;33m"; // Bold Yellow
        break;
      case ERROR_FAILURE:
        color = "\033[31m";        // Red
        code_color = "\033[1;31m"; // Bold Red
        break;
    }
  }

  std::stringstream ss;
  ss << color << source_range.begin_location.ToString() << (terminal_supports_color ? "\033[0m\n" : "\n");
  ss << '\n';

  // Read the source file
  auto first = source_range.begin_location;
  auto last = source_range.end_location;
  std::ifstream src_file(SourceLocation::files()[first.file]);

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

  // Find the start and end positions for the lines around the error
  size_t error_line_start = 0;
  size_t error_line_end = 0;
  size_t context_start = 0;
  size_t context_end = 0;

  for (size_t i = 0; i < file_content.size(); ++i) {
    if (file_content[i] == '\n') {
      current_line++;
      if (current_line == first.line - 2) {
        context_start = i + 1;
      }
      if (current_line == first.line) {
        error_line_start = i + 1;
      }
      if (current_line == first.line + 1) {
        error_line_end = i;
      }
      if (current_line == first.line + 3) {
        context_end = i;
        break;
      }
    }
  }

  if (context_end == 0) {
    context_end = file_content.size();
  }

  std::string context = file_content.substr(context_start, context_end - context_start);
  source_code << context << '\n';

  // Generate the caret indicator
  size_t caret_line_start = error_line_start - context_start;
  std::string caret_line = file_content.substr(error_line_start, error_line_end - error_line_start);

  if (first.column <= 1) {
    caret_indicator = "";
  } else {
    caret_indicator = std::string(std::max(1ul, first.column - 1), ' ');
  }
  caret_indicator += "^^^";

  if (terminal_supports_color)
    ss << "\033[90;3m";

  // Insert the context lines
  std::istringstream context_stream(context);
  std::string line;
  size_t line_count = 0;
  while (std::getline(context_stream, line)) {
    ss << line << '\n';
    line_count++;
    if (line_count == 3) { // Insert caret indicator after the error line
      ss << caret_indicator << '\n';
    }
  }

  if (terminal_supports_color)
    ss << "\033[0m";

  return ss.str();
}
enum WarningFlags {
  WarningNone = 0,
  WarningUseDotNotArrowOperatorOverload = 1 << 0, // --Wno-arrow-operator
  WarningInaccessibleDeclaration = 1 << 1,        // --Wno-inaccessible-decl
  WarningEmptyStringInterpolation = 1 << 2,       // --Wno-empty-string-interp
  WarningNonNullDeletedPointer = 1 << 3,          // --Wno-non-null-deleted
  WarningAmbigousVariants = 1 << 4,               // --Wno-amiguous-variant
  WarningSwitchBreak = 1 << 5,                    // --Wno-switch-break
  WarningDownCastFixedArrayParam = 1 << 6,        // --Wno-array-param
  WarningIgnoreAll = 1 << 7,                      // --Wignore-all
};

extern int ignored_warnings;

using PanicHandler = void(*)(const std::string &, const SourceRange &);

extern PanicHandler panic_handler;

static PanicHandler get_default_panic_handler() {
  return [] [[noreturn]] (auto message, auto source_range) {
    std::stringstream ss;

    std::string lower_message = message;
    std::transform(lower_message.begin(), lower_message.end(), lower_message.begin(), ::tolower);

    if (lower_message.contains("undeclared") ||
        lower_message.contains("use of") && compile_command.has_flag("freestanding")) {
      lower_message += "\n You are in a freestanding environment. Many types that are normally built in, are not "
                       "included in this mode";
    }

    ss << "\033[1;31m" << format_message(message) << "\033[0m";
    ss << "\n\tat: " << format_source_location(source_range, ERROR_FAILURE);
    const auto err = ss.str();
    printf("%s\n", err.c_str());
    exit(1);
  };
}

static void throw_warning(const WarningFlags id, const std::string message, const SourceRange &source_range) {
  if ((ignored_warnings & id) != 0 || (ignored_warnings & WarningIgnoreAll) != 0) {
    return;
  }
  std::stringstream ss;
  if (terminal_supports_color)
    ss << "\033[36m";
  ss << "Warning:\n\t" << format_message(message);
  if (terminal_supports_color)
    ss << "\033[0m\n";
  ss << format_source_location(source_range, ERROR_WARNING);
  const auto token_str = ss.str();
  std::cerr << token_str << std::endl;
}

static void throw_error(const std::string &message, const SourceRange &source_range) {
  if (!panic_handler) {
    get_default_panic_handler()(message, source_range);
  }
  panic_handler(message, source_range);
}
