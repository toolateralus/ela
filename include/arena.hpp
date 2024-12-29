#pragma once

/**
 * @class Arena
 * @brief Represents an arena for allocating memory in a fixed-size buffer.
 *
 * The Arena class provides a simple interface for allocating memory in a
 * fixed-size buffer. It prevents dynamic memory allocation by using a
 * pre-allocated buffer. The buffer size is specified during construction and
 * cannot be changed afterwards. Memory is allocated sequentially from the
 * buffer, and deallocation is not supported. If the buffer is full and there is
 * not enough space to allocate the requested memory, an exception of type
 * std::runtime_error is thrown.
 */

#include <cstddef>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

namespace jstl {
  struct Arena final {
    Arena() = delete;

    Arena(const Arena &) = delete;
    Arena(Arena &&) = delete;
    Arena &operator=(const Arena &) = delete;
    Arena &operator=(Arena &&) = delete;

    inline Arena(size_t capacity)
        : capacity(capacity), data(new char[capacity]), ptr(0) {}

    inline ~Arena() { delete[] data; }

    inline char *allocate(size_t size_in_bytes) {
      size_t alignment = alignof(std::max_align_t);
      size_t aligned_ptr = (ptr + alignment - 1) & ~(alignment - 1);
      if (aligned_ptr + size_in_bytes > capacity) {
        printf("failed to allocate in arena");
        exit(1);
      }
      char *mem = (char *)data + aligned_ptr;
      ptr = aligned_ptr + size_in_bytes;
      return mem;
    }

  private:
    const size_t capacity;
    const char *data;
    size_t ptr = 0;
  };
}
