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

#define assert(message, condition)                                                      \
  if (condition == false)                                                      \
    throw std::runtime_error("\e[33massertion: " #condition " failed :: \e[31m" + std::string(message));

#include <iostream>
#include <iomanip>

struct _test {
  _test() {}
  _test(const char *name, void (*function)()): name(name), function(function) { }
  const char *name;
  void (*function)();
  void run() const {
    std::cout << "\033[1;33mtesting \033[1;37m...\033[1;36m" << std::setw(20) << std::left << name;
    try {
        function();
        std::cout << "\033[1;32m[passed]\033[0m\n";
    } catch (const std::runtime_error &e) {
        std::cout << "\033[1;31m[failed]...\n" << e.what() << "\033[0m\n";
    }
}
};

#define __TEST_RUNNER_MAIN                                                                                               \
  int main() {                                                                                                                       \
    for (const auto &test : tests) {                                                                                                 \
      test.run();                                                                                                                    \
    }                                                                                                                                \
  }

#else
#include <assert.h>
#endif

