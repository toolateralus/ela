#pragma once

/**
 * @class Arena
 * @brief Simple memory arena with linked blocks for fast allocation.
 *
 * Arena is a basic memory allocator that hands out memory from a fixed-size buffer.
 * When the buffer fills up, it automatically links to a new Arena block of the same size.
 * Allocations are fast and sequential; individual deallocation isn't supported.
 * Use this for quick, temporary allocations where you free everything at once by destroying the arena.
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