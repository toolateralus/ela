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

static inline std::string get_text_representation_of_source_range(const SourceRange &source_range,
                                                                  size_t num_lines_of_source_to_show) {
  std::stringstream ss;
  if (num_lines_of_source_to_show > 0) {
    ss << '\n';
    auto first = source_range;
    std::ifstream src_file(SourceRange::files()[first.file]);

    if (!src_file.is_open()) {
      return "Error: Unable to open source file\n";
    }

    std::string line;
    size_t line_index = 0;
    size_t start_line = (first.line > num_lines_of_source_to_show) ? first.line - num_lines_of_source_to_show : 0;
    size_t end_line = first.line + num_lines_of_source_to_show;

    while (std::getline(src_file, line)) {
      if (line_index >= start_line && line_index <= end_line) {
        ss << line << '\n';
        if (line_index == first.line - 1) {
          std::string caret_indicator = std::string(std::max(1ul, first.column), ' ') + "^^^";
          if (terminal_supports_color) {
            ss << "\033[90;3m";
          }
          ss << caret_indicator << '\n';
          if (terminal_supports_color) {
            ss << "\033[0m";
          }
        }
      }
      if (line_index > end_line) {
        break;
      }
      line_index++;
    }
    src_file.close();
  }
  return ss.str();
}

static std::string format_source_location(const SourceRange &source_range, ErrorSeverity severity,
                                          int num_lines_of_source_to_show = 5) {
  const char *color = "";

  if (terminal_supports_color) {
    switch (severity) {
      case ERROR_INFO:
        color = "\033[36m"; // Cyan
        break;
      case ERROR_WARNING:
        color = "\033[33m"; // Yellow
        break;
      case ERROR_FAILURE:
        color = "\033[31m"; // Red
        break;
    }
  }

  std::stringstream ss;
  ss << color << source_range.ToString() << (terminal_supports_color ? "\033[0m" : "");
  ss << get_text_representation_of_source_range(source_range, num_lines_of_source_to_show);

  if (terminal_supports_color)
    ss << "\033[0m";

  return ss.str();
}
enum WarningFlags {
  WarningNone = 0,
  WarningUseDotNotArrowOperatorOverload = 1 << 0, // --Wno-arrow-operator
  WarningInaccessibleDeclaration = 1 << 1,        // --Wno-inaccessible-decl
  WarningSwitchBreak = 1 << 2,                    // --Wno-switch-break
  WarningIgnoreAll = 1 << 3,                      // --Wignore-all
};

extern int ignored_warnings;

using PanicHandler = void (*)(const std::string &, const SourceRange &, void *);

extern PanicHandler panic_handler;

static PanicHandler get_default_panic_handler() {
  return [] [[noreturn]] (auto message, auto source_range, [[gnu::unused]] auto unused) {
    std::stringstream ss;

    std::string lower_message = message;
    std::transform(lower_message.begin(), lower_message.end(), lower_message.begin(), ::tolower);

    if (lower_message.contains("undeclared") ||
        (lower_message.contains("use of") && compile_command.has_flag("freestanding"))) {
      lower_message += "\n You are in a freestanding environment. Many types that are normally built in, are not "
                       "included in this mode";
    }

    ss << message;
    ss << "\n\tat: " << format_source_location(source_range, ERROR_FAILURE);
    const auto err = ss.str();
    printf("%s\n", err.c_str());
    exit(1);
  };
}

static inline void set_panic_handler(PanicHandler handler) { panic_handler = handler; }

static inline void reset_panic_handler() { panic_handler = get_default_panic_handler(); }

static inline void throw_warning(const WarningFlags id, const std::string message, const SourceRange &source_range) {
  if ((ignored_warnings & id) != 0 || (ignored_warnings & WarningIgnoreAll) != 0) {
    return;
  }
  std::stringstream ss;
  if (terminal_supports_color)
    ss << "\033[36m";
  ss << "Warning:\n\t" << message;
  if (terminal_supports_color)
    ss << "\033[0m\n";
  ss << format_source_location(source_range, ERROR_WARNING);
  const auto token_str = ss.str();
  std::cerr << token_str << std::endl;
}

extern void *error_user_data;

static inline void set_error_user_data(void *data) { error_user_data = data; }

template <class T> T *get_error_user_data_as() {
  if (!error_user_data)
    return nullptr;
  return (T *)error_user_data;
}

inline void throw_error(const std::string &message, const SourceRange &source_range) {
  if (!panic_handler) {
    get_default_panic_handler()(message, source_range, error_user_data);
  }
  panic_handler(message, source_range, error_user_data);
}
