#pragma once

#include <jstl/memory/arena.hpp>
#include <stdexcept>

struct Error;

#ifndef MAX_ERRORS
#define MAX_ERRORS 100
#endif

// @Josh
// TODO: determine if this is neccesary. Would we ever look up errors? or just
// TODO(cont.): store them to emit them all once a critical error has been encountered, or
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
  const char *message;
  const ErrorSeverity severity;
};

static jstl::Arena error_arena = {sizeof(Error) * MAX_ERRORS};

static void throw_error(Error &&err) {
  if (num_errors >= MAX_ERRORS) {
    throw std::runtime_error("Maximum number of errors exceeded");
  }
  
  auto errid = num_errors++;
  auto memory = (Error *)error_arena.allocate(sizeof(Error));
  error_table[errid] = new (memory) Error(err);

  // TODO: do something more sophisticated here. for now in early dev, just
  // throw on all errors.
  switch (err.severity) {
  case ERROR_INFO:
  case ERROR_WARNING:
  case ERROR_FAILURE:
  case ERROR_CRITICAL:
    throw std::runtime_error(err.message);
    break;
  }
}
