#pragma once

#include "core.hpp"
#include <jstl/memory/arena.hpp>
#include <sstream>
#include <stdexcept>

struct Error;

#ifndef MAX_ERRORS
#define MAX_ERRORS 100
#endif

// @Josh
// TODO: determine if this is neccesary. Would we ever look up errors? or just
// TODO(cont.): store them to emit them all once a critical error has been
// encountered, or
// TODO(cont.): once compilation is complete?
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
  // TODO: emit a error message based on the global table of tokens.
  if (!source_range.empty()) {
    auto span = source_range.get_tokens();
    ss << "\e[31m" << span.front().location.ToString() << "\e[0m\n";
    for (const auto &tok : span) {
      ss << "\e[41;1;37m" << tok.value << " \e[0m";
    }
  }

  const auto token_str = ss.str();

  // TODO: do something more sophisticated here. for now in early dev, just throw on all errors.
  switch (severity) {
  case ERROR_INFO:
  case ERROR_WARNING:
  case ERROR_FAILURE:
  case ERROR_CRITICAL:
    throw std::runtime_error(message + '\n' + token_str);
    break;
  }
}
