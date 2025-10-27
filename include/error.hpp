#pragma once

#include <format>
#include <iostream>
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
  if (term == NULL) return false;

  return strstr(term, "dumb") == nullptr || (strstr(term, "color") != NULL || strstr(term, "xterm") != NULL ||
                                             strstr(term, "screen") != NULL || strstr(term, "tmux") != NULL);
}
#endif

static bool terminal_supports_color = supports_color();

static inline std::string get_source_line_from_span(const Span &span) {
  std::ifstream src_file(Span::files()[span.file]);

  if (!src_file.is_open()) {
    return "<no src>";
  }

  std::string line;
  size_t line_index = 0;

  while (std::getline(src_file, line)) {
    if (line_index == span.line - 1) {
      src_file.close();
      return line;
    }
    line_index++;
  }

  src_file.close();
  return "<no src>";
}

static inline std::string colorize(const std::string &text, const char *ansi_code) {
  if (terminal_supports_color) {
    return std::string(ansi_code) + text + "\033[0m";
  }
  return text;
}
static inline std::string get_text_representation_of_source_range(const Span &span, size_t num_lines_of_source_to_show) {
  std::stringstream ss;
  if (num_lines_of_source_to_show == 0) return "";

  std::ifstream src_file(Span::files()[span.file]);
  if (!src_file.is_open()) return "Error: Unable to open source file\n";

  ss << '\n';

  std::string line;
  size_t current_line = 1;

  while (std::getline(src_file, line)) {
    if (current_line >= (span.line > num_lines_of_source_to_show ? span.line - num_lines_of_source_to_show : 1) &&
        current_line <= span.line + num_lines_of_source_to_show) {
      ss << line << '\n';

      if (current_line == span.line) {
        size_t caret_len = std::max<size_t>(1, span.length);
        std::string caret_line(span.column - 1, ' ');
        caret_line += std::string(caret_len, '^');
        ss << colorize(caret_line, "\033[90;3m") << '\n';
      }
    }

    if (current_line > span.line + num_lines_of_source_to_show) break;
    current_line++;
  }

  return ss.str();
}

static std::string format_source_location(const Span &span, ErrorSeverity severity, int num_lines_of_source_to_show = 1) {
  const char *color = "";

  if (terminal_supports_color) {
    switch (severity) {
      case ERROR_INFO:
        color = "\033[1;36m";  // Cyan
        break;
      case ERROR_WARNING:
        color = "\033[1;33m";  // Yellow
        break;
      case ERROR_FAILURE:
        color = "\033[1;31m";  // Red
        break;
    }
  }

  std::stringstream ss;
  ss << color << span.to_string() << (terminal_supports_color ? "\033[0m\n" : "\n");

  // if (num_lines_of_source_to_show) {
  // ss << get_source_line_from_span(span);
  // } else {
  ss << get_text_representation_of_source_range(span, num_lines_of_source_to_show);
  // }

  if (terminal_supports_color) ss << "\033[0m";

  return ss.str();
}
enum WarningFlags {
  WARNING_USE_DOT_NOT_ARROW_OP_OVERLOAD = 1 << 0,  // --Wno-arrow-operator
  WARNING_INACCESSIBLE_DECLARATION = 1 << 1,       // --Wno-inaccessible-decl
  WARNING_SWITCH_BREAK = 1 << 2,                   // --Wno-switch-break
  WARNING_IGNORE_ALL = 1 << 3,                     // --Wignore-all
  WARNING_ARRAY_ASSIGNMENT_MEMCPY = 1 << 4,        // --Warray-assignment-memcpy
  WARNING_RETURNING_ARRAY = 1 << 5,                // --Wno-returning-array
  WARNING_DEPRECATED = 1 << 6,                     // --Wno-deprecated
  // Used only in development so for new warnings, you don't have to extend this immediately.
  WARNING_GENERAL = 1 << 7,
  WARNING_COUNT = WARNING_GENERAL + 1,
};

// All of these would be prefixed with -- in the command line.
// this is just for detecting the flags in the command struct.

// TODO: put this in a hashmap, there is no reason we need to allocate
// this much memory to store ~8 flags.
const std::string WARNING_FLAG_STRINGS[WARNING_COUNT]{
    [WARNING_USE_DOT_NOT_ARROW_OP_OVERLOAD] = "Wno-arrow-operator",
    [WARNING_INACCESSIBLE_DECLARATION] = "Wno-inaccessible-decl",
    [WARNING_SWITCH_BREAK] = "Wno-switch-break",
    [WARNING_IGNORE_ALL] = "Wignore-all",
    [WARNING_ARRAY_ASSIGNMENT_MEMCPY] = "Wno-array-assignment-memcpy",
    [WARNING_RETURNING_ARRAY] = "Wno-returning-array",
    [WARNING_DEPRECATED] = "Wno-deprecated",
    [WARNING_GENERAL] = "Wno-general",
};

extern int num_ignored_warnings;

using PanicHandler = void (*)(const std::string &, const Span &, void *);

extern PanicHandler panic_handler;

static PanicHandler get_default_panic_handler() {
  return [] [[noreturn]] (auto message, auto span, [[gnu::unused]] auto unused) {
    std::stringstream ss;
    ss << "Error at: " << format_source_location(span, ERROR_FAILURE) << '\n';
    ss << message;
    const auto err = ss.str();
    fprintf(stderr, "%s\n", err.c_str());
    exit(1);
  };
}

static inline void set_panic_handler(PanicHandler handler) { panic_handler = handler; }

static inline void reset_panic_handler() { panic_handler = get_default_panic_handler(); }

static inline void throw_warning(const WarningFlags id, const std::string message, const Span &span) {
  if ((num_ignored_warnings & id) != 0 || (num_ignored_warnings & WARNING_IGNORE_ALL) != 0) {
    return;
  }
  std::stringstream ss;
  if (terminal_supports_color) ss << "\033[36m";
  ss << "(ignore with --" << WARNING_FLAG_STRINGS[id] << ")\n";
  ss << "Warning:\n\t" << message;
  if (terminal_supports_color) ss << "\033[0m\n";
  ss << format_source_location(span, ERROR_WARNING);
  const auto token_str = ss.str();
  std::cerr << token_str << std::endl;
}

extern void *error_user_data;

static inline void set_error_user_data(void *data) { error_user_data = data; }

template <class T>
T *get_error_user_data_as() {
  if (!error_user_data) return nullptr;
  return (T *)error_user_data;
}

inline void throw_error(const std::string &message, const Span &span) {
  if (!panic_handler) {
    get_default_panic_handler()(message, span, error_user_data);
  }
  panic_handler(message, span, error_user_data);
}

template <typename... Args>
void throw_errorf(std::format_string<Args...> format, Args &&...args) {
  std::string error = std::format(format, std::forward<Args>(args)...);
  throw_error(error, {});
}

template <typename... Args>
inline static void throw_error(const std::format_string<Args...> format, const Span &span, const Args &&...args) {
  std::string error = std::format(format, std::forward(args)...);
  throw_error(error, span);
}
