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

static Error *error_table[MAX_ERRORS];
static int num_errors = 0;

enum ErrorSeverity {
  ERROR_INFO,
  ERROR_WARNING,
  ERROR_FAILURE,
};

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

static bool supports_color() {
  if (!isatty(STDOUT_FILENO)) {
    return false;
  }
  const char* term = getenv("TERM");
  if (term == NULL) return false;
  return strstr(term, "color") != NULL ||
          strstr(term, "xterm") != NULL ||
          strstr(term, "screen") != NULL ||
          strstr(term, "tmux") != NULL;
}

static bool terminal_supports_color =  false;//supports_color();

struct Error {
  Error(const std::string &message,
        const SourceRange source_range)
      : message(message), source_range(source_range) {}

  const std::string message = "";
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
    const char *color = "";
    const char *code_color = "";
    if (terminal_supports_color) {
        switch (severity) {
        case ERROR_INFO:
            color = "\e[36m"; // Cyan
            code_color = "\e[1;36m"; // Bold Cyan
            break;
        case ERROR_WARNING:
            color = "\e[33m"; // Yellow
            code_color = "\e[1;33m"; // Bold Yellow
            break;
        case ERROR_FAILURE:
            color = "\e[31m"; // Red
            code_color = "\e[1;31m"; // Bold Red
            break;
        }
    }

    std::stringstream ss;
    auto span = source_range.get_tokens();
    if (terminal_supports_color) ss << "\e[90m";
    ss << std::string(80, '-');
    if (terminal_supports_color) ss << "\e[0m\n";
    ss << '\n';
    ss << color << span.front().location.ToString() << (terminal_supports_color ? "\e[0m\n" : "\n");
    if (terminal_supports_color) ss << "\e[90m";
    ss << std::string(80, '-');
    if(terminal_supports_color) ss << "\e[0m";
    ss << '\n';
    if (terminal_supports_color) ss << "\e[36;1;30m>> ";
    for (const auto &tok : span) 
        ss << code_color << tok.value << (terminal_supports_color ? " \e[0m" : " ");
    if (terminal_supports_color) ss << "\e[36;1;30m <<";
    if (terminal_supports_color) ss << "\e[90m";
    ss << "\n" << std::string(80, '^');
    if (terminal_supports_color) ss << "\e[0m"; 
    
    return ss.str();
}

static void throw_warning(const std::string message, const SourceRange &source_range) {
    std::stringstream ss;
    auto span = source_range.get_tokens();
    if (terminal_supports_color) ss << "\e[36m";
    ss << "Warning:\n\t" << format_message(message);
    if (terminal_supports_color) ss << "\e[0m\n";
    ss << format_source_location(source_range, ERROR_WARNING);
    const auto token_str = ss.str();

    std::cerr << token_str << std::endl;
}

[[noreturn]] static void throw_error(const std::string &message, const SourceRange &source_range) {
    auto errid = num_errors++;
    auto memory = (Error *)error_arena.allocate(sizeof(Error));
    error_table[errid] = new (memory) Error(message, source_range);
    std::stringstream ss;
    auto span = source_range.get_tokens();
    if (terminal_supports_color) ss << "\e[31m";
    ss << "Error:\n\t" << message;
    if (terminal_supports_color) ss << "\e[0m\n";
    ss << format_source_location(source_range, ERROR_FAILURE);
    const auto token_str = ss.str();
    printf("%s\n", token_str.c_str());
    exit(1);
}