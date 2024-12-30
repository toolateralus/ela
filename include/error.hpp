#pragma once

#include "core.hpp"
#include <span>
#include <sstream>

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
  const char* term = getenv("TERM");
  if (term == NULL) return false;
  return strstr(term, "color") != NULL ||
          strstr(term, "xterm") != NULL ||
          strstr(term, "screen") != NULL ||
          strstr(term, "tmux") != NULL;
}
#endif

static bool terminal_supports_color =  supports_color();

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

    auto span = source_range.get_tokens();

    if (terminal_supports_color) {
        switch (severity) {
        case ERROR_INFO:
            color = "\033[36m"; // Cyan
            code_color = "\033[1;36m"; // Bold Cyan
            break;
        case ERROR_WARNING:
            color = "\033[33m"; // Yellow
            code_color = "\033[1;33m"; // Bold Yellow
            break;
        case ERROR_FAILURE:
            color = "\033[31m"; // Red
            code_color = "\033[1;31m"; // Bold Red
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
    if(terminal_supports_color) ss << "\033[0m";
    ss << '\n';
    if (terminal_supports_color) ss << "\033[36;1;30m>> ";
    for (const auto &tok : span)
        ss << code_color << tok.value.get_str() << (terminal_supports_color ? " \033[0m" : " ");
    if (terminal_supports_color) ss << "\033[36;1;30m <<";
    if (terminal_supports_color) ss << "\033[90m";
    ss << "\n" << std::string(80, '^');
    if (terminal_supports_color) ss << "\033[0m";

    return ss.str();
}

static void throw_warning(const std::string message, const SourceRange &source_range) {
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
