#pragma once

/**
 * @class Arena
 * @brief Represents an arena for allocating memory in a fixed-size buffer.
 *
 * The Arena class provides a simple trait for allocating memory in a
 * fixed-size buffer. It prevents dynamic memory allocation by using a
 * pre-allocated buffer. The buffer size is specified during construction and
 * cannot be changed afterwards. Memory is allocated sequentially from the
 * buffer, and deallocation is not supported. If the buffer is full and there is
 * not enough space to allocate the requested memory, an exception of type
 * std::runtime_error is thrown.
 */

#include <cstring>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <cstddef>

namespace jstl {
struct Arena final {
  Arena() = delete;

  Arena(const Arena &) = delete;
  Arena(Arena &&) = delete;
  Arena &operator=(const Arena &) = delete;
  Arena &operator=(Arena &&) = delete;


  inline Arena(size_t capacity) : capacity(capacity), data(new char[capacity]), ptr(0) {}

  inline ~Arena() { delete[] data; delete next; }

  inline char *allocate(size_t size_in_bytes) {
    constexpr size_t MAX_ALIGN = alignof(std::max_align_t);
    size_t aligned_ptr = (ptr + MAX_ALIGN - 1) & ~(MAX_ALIGN - 1);

    if (aligned_ptr + size_in_bytes > capacity) {
      if (!next) {
        next = new Arena(capacity);
      }
      return next->allocate(size_in_bytes);
    }

    char *mem = (char *)data + aligned_ptr;
    ptr = aligned_ptr + size_in_bytes;
    return mem;
  }

 private:
  Arena *next = nullptr;
  const size_t capacity = 0;
  char *data = nullptr;
  size_t ptr = 0;

};
}  // namespace jstl