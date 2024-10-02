#pragma once

#include "core.hpp"
#include <jstl/memory/arena.hpp>
#include <span>
#include <sstream>
#include <stdexcept>

struct Error;

#ifndef MAX_ERRORS
#define MAX_ERRORS 100
#endif

// TODO(Josh) Use warnings more and add a way to report the non-critical errors at end of compilation. 9/30/2024, 10:22:07 AM
static Error *error_table[MAX_ERRORS];
static int num_errors = 0;

enum ErrorSeverity {
  ERROR_INFO,
  ERROR_WARNING,
  ERROR_FAILURE,
  ERROR_CRITICAL,
};

struct Error {
  Error(const std::string &message, const ErrorSeverity severity,
        const SourceRange source_range)
      : message(message), severity(severity), source_range(source_range) {}

  const std::string message = "";
  const ErrorSeverity severity = ERROR_INFO;
  const SourceRange source_range{};
};

static jstl::Arena error_arena = {sizeof(Error) * MAX_ERRORS};

static std::string format_message(const std::string &message) {
  std::stringstream formatted;
  int width = 0;
  int size = 0;
  for (char ch : message) {
    formatted << ch;
    if (ch == '\n') {
      formatted << '\t';
    }
    if (width > 50 && message.length() > size+1 && std::isspace(message[size+1])) {
      formatted << "\n\t";
      width = 0;
    }
    size++;
    width++;
  }
  return formatted.str();
}

static std::string format_source_location(const SourceRange &source_range, ErrorSeverity severity) {
    const char *color;
    const char *code_color;
    switch (severity) {
    case ERROR_INFO:
        color = "\e[36m"; // Cyan
        code_color = "\e[1;36m"; // Bold Cyan
        break;
    case ERROR_WARNING:
        color = "\e[33m"; // Yellow
        code_color = "\e[1;33m"; // Bold Cyan
        break;
    case ERROR_FAILURE:
    case ERROR_CRITICAL:
        color = "\e[31m"; // Red
        code_color = "\e[1;31m"; // Bold red
        break;
    }

    std::stringstream ss;
    auto span = source_range.get_tokens();
    ss << "\e[90m" << std::string(80, '-') << "\e[0m\n"; // Top border
    ss << color << span.front().location.ToString() << "\e[0m\n";
    ss << "\e[90m" << std::string(80, '-') << "\e[0m\n"; // Separator
    ss << "\e[36;1;30m>> ";
    for (const auto &tok : span) {
      ss << code_color << tok.value << " \e[0m";
    }
    ss << "\e[36;1;30m <<";
    ss << "\n\e[90m" << std::string(80, '^') << "\e[0m"; // Bottom border
    
    return ss.str();
}
static void throw_warning(const std::string message, const SourceRange &source_range) {
  std::stringstream ss;
  auto span = source_range.get_tokens();
  ss << "\e[36m" << "Warning:\n\t" << format_message(message) << "\e[0m\n";
  ss << format_source_location(source_range, ERROR_WARNING);
  const auto token_str = ss.str();

  std::cerr << token_str << std::endl;
}

[[noreturn]] static void throw_error(const std::string &message,
                                     const ErrorSeverity severity,
                                     const SourceRange &source_range) {
  if (num_errors >= MAX_ERRORS) {
    throw std::runtime_error("Maximum number of errors exceeded");
  }

  auto errid = num_errors++;
  auto memory = (Error *)error_arena.allocate(sizeof(Error));
  error_table[errid] = new (memory) Error(message, severity, source_range);

  std::stringstream ss;
  auto span = source_range.get_tokens();

  ss << "\e[31m" << "Error:\n\t" << message << "\e[0m\n";
  ss << format_source_location(source_range, severity);
  const auto token_str = ss.str();

  switch (severity) {
  case ERROR_INFO:
  case ERROR_WARNING:
    // this should never happen
    throw std::runtime_error("Do not use throw_error for warning and infos!");
  case ERROR_FAILURE:
  case ERROR_CRITICAL:
    throw std::runtime_error(token_str);
    break;
  }
}
