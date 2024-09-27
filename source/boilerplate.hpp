#include <jstl/containers/vector.hpp>
#include <stdint.h>

using f64 = double;
using u64 = uint64_t;
using s64 = int64_t;

using s32 = int32_t;
using u32 = uint32_t;
using f32 = float;

using s16 = int16_t;
using u16 = uint16_t;

using s8 = int8_t;
using u8 = uint8_t;

using string = const char *;

template <class T> using _array = jstl::Vector<T>;



#ifdef TESTING
#include <stdexcept>

#define assert(condition)                                                      \
  if (condition == false)                                                      \
    throw std::runtime_error("assertion: " #condition " failed");
    
struct _test {
  const char *name;
  void (*function)();
  void run() {
    printf("\033[1;34mrunning test: \033[1;33m%s\033[0m...\n", name);
    try {
      function();
      printf("\033[1;32mpassed\033[0m\n");
    } catch (const std::runtime_error &e) {
      printf("\033[1;31mfailed: %s\033[0m\n", e.what());
    }
  }
};

#define __TEST_RUNNER_MAIN                                                     \
  int main() {                                                                 \
    for (const auto &test : tests) {                                           \
      test.run();                                                              \
    }                                                                          \
  }                                                                            \

#else 
#include <assert.h>
#endif
