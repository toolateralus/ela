//  * Manually call destructor on one or many objects. useful for unions that
//  own non-trivial objects.
#include <utility>
template <class... T> void destruct(T &...t) { (t.~T(), ...); }
template <class T, class... Args> T *construct(void *memory, Args &&...args) {
  return new (memory) T(std::forward<Args>(args)...);
}
#if USE_STD_LIB
#include <tuple>
struct string;
#endif

struct Range {
  using value_type = signed long long;
  value_type first = 0, last = 0, span = 0, increment = 1;
  bool is_reverse = false;
  struct iterator {
    value_type current, end_value, increment;
    bool is_reverse;
    iterator(value_type start, value_type end_value, bool is_reverse, value_type increment)
        : current(start), end_value(end_value), is_reverse(is_reverse), increment(increment) {}
    value_type operator*() const { return current; }

    iterator &operator++() {
      if (is_reverse) {
        current -= increment;
      } else {
        current += increment;
      }
      return *this;
    }

    bool operator!=(const iterator &other) const {
      if (is_reverse) {
        return current > other.end_value;
      } else {
        return current < other.end_value;
      }
    }
  };
  iterator begin() const { return iterator(first, last, is_reverse, increment); }
  iterator end() const { return iterator(first, last, is_reverse, increment); }
  iterator begin() { return iterator(first, last, is_reverse, increment); }
  iterator end() { return iterator(first, last, is_reverse, increment); }
  bool operator==(const value_type number) const { return number >= first && number <= last; }
  bool contains(const value_type number) { return number >= first && number <= last; }
#if USE_STD_LIB
  string to_string() const;
#endif
};

#if USE_STD_LIB

#include <initializer_list>
#include <stdint.h>
#include <unordered_map>
#include <errno.h>

// TODO: replace these with our own types.
// They may not be faster, but they might be. What will be faster is compilation
// times.
template <class Key, class Value> using _map = std::unordered_map<Key, Value>;

template <class T> void move(T *to, const T from) { *to = std::move(from); }

using float64 = double;
using u64 = uint64_t;
using s64 = int64_t;

using s32 = int32_t;
using u32 = uint32_t;
using float32 = float;

using s16 = int16_t;
using u16 = uint16_t;

using s8 = int8_t;
using u8 = uint8_t;

extern "C" int printf(const char *format, ...);
extern "C" int snprintf(char *str, size_t size, const char *format, ...);
extern "C" int sprintf(char *str, const char *format, ...);
extern "C" void *memcpy(void *, void *, size_t);
extern "C" void *malloc(size_t);
extern "C" void free(void*);
extern "C" void *memset(void *, int, size_t);
extern "C" void *realloc(void*, size_t);
extern "C" int strlen(const char *);

#undef RAND_MAX
#undef assert

template <class T> struct _array {
  T *data = nullptr;
  u32 length = 0;
  u32 capacity = 0;
  bool is_view = false;

  _array() : data(nullptr), length(0), capacity(0), is_view(false) {}

  _array(int length) : length(length), capacity(length), is_view(false) {
    data = static_cast<T*>(malloc(sizeof(T) * length));
    if (data) {
      memset(data, 0, sizeof(T) * length);
    }
  }

  _array(const _array &other)
      : length(other.length), capacity(other.capacity), is_view(other.is_view) {
    data = static_cast<T*>(malloc(sizeof(T) * other.length));
    if (data) {
      std::copy(other.data, other.data + other.length, data);
    }
  }

  _array(_array &&other) noexcept
      : data(other.data), length(other.length), capacity(other.capacity), is_view(other.is_view) {
    other.data = nullptr;
    other.length = 0;
    other.capacity = 0;
    other.is_view = false;
  }

  _array &operator=(const _array &other) {
    if (this != &other) {
      T *new_data = static_cast<T*>(malloc(sizeof(T) * other.length));
      if (new_data) {
        std::copy(other.data, other.data + other.length, new_data);
        if (length != 0 && data && !is_view) {
          free(data);
        }
        data = new_data;
        length = other.length;
        capacity = other.capacity;
        is_view = other.is_view;
      }
    }
    return *this;
  }

  _array &operator=(_array &&other) noexcept {
    if (this != &other) {
      if (data && length != 0 && !is_view) {
        free(data);
      }
      data = other.data;
      length = other.length;
      capacity = other.capacity;
      is_view = other.is_view;
      other.data = nullptr;
      other.length = 0;
      other.capacity = 0;
      other.is_view = false;
    }
    return *this;
  }

  _array(std::initializer_list<T> list) : length(list.size()), capacity(list.size()), is_view(false) {
    if (list.size() != 0) {
      data = static_cast<T*>(malloc(list.size() * sizeof(T)));
      if (data) {
        std::copy(list.begin(), list.end(), data);
      }
    }
  }

  ~_array() {
    if (length != 0 && !is_view && data) {
      free(data);
      data = nullptr;
    }
  }

  void resize(int new_capacity) {
    if (new_capacity > capacity) {
      T *new_data = static_cast<T*>(realloc(data, new_capacity * sizeof(T)));
      if (new_data) {
        data = new_data;
        capacity = new_capacity;
      }
    }
  }

  void push(const T &value) {
    if (length >= capacity) {
      resize(capacity == 0 ? 1 : capacity * 2);
    }
    data[length++] = value;
  }

  T pop() {
    if (length > 0) {
      return data[--length];
    }
    return T(); // Return default-constructed T if array is empty
  }

  void erase(const T &value) {
    size_t write_index = 0;
    for (size_t read_index = 0; read_index < length; ++read_index) {
      if (data[read_index] != value) {
        data[write_index] = data[read_index];
        ++write_index;
      }
    }
    length = write_index;
  }

  T &operator[](int n) { return data[n]; }

  const T &operator[](int n) const { return data[n]; }

  _array<T> operator[](const Range &range) {
    _array<T> result;
    result.data = data + range.first;
    result.length = range.last - range.first;
    result.capacity = result.length;
    result.is_view = true;
    return result;
  }

  bool operator==(const _array &other) const {
    return length == other.length && std::equal(data, data + length, other.data);
  }

  bool operator!=(const _array &other) const { return !(*this == other); }

  auto begin() { return data; }
  auto end() { return data + length; }
};

// For now, we'll just use a simple null terminated string.
struct string {
  char *data = nullptr;
  u32 length = 0;
  bool is_view = false;
  string() {}
  string(char *str) {
    if (str == nullptr) {
      return;
    }
    length = 0;
    while (str[length] != '\0') {
      ++length;
    }
    data = (char*)malloc(length + 1);
    data[length] = '\0';
    std::copy(str, str + length, data);
  }
  string(char *begin, char *end, bool is_view = false) {
    this->is_view = is_view;
    if (is_view) {
      data = begin;
      length = std::distance(begin, end);
    } else {
      length = std::distance(begin, end);
      data = new char[length + 1];
      std::copy(begin, end, data);
      data[length] = '\0';
    }
  }
  
  string(const string &other) {
    if (other.data) {
      length = other.length;
      data = new char[length + 1];
      std::copy(other.data, other.data + length, data);
      data[length] = '\0';
    }
  }
  string &operator=(const string &other) {
    if (this != &other) {
      if (data) delete[] data;
      if (other.data) {
        length = other.length;
        data = new char[length + 1];
        std::copy(other.data, other.data + length, data);
        data[length] = '\0';
      } else {
        data = nullptr;
        length = 0;
      }
    }
    return *this;
  }
 
  ~string() {
    if (data && !is_view && length != 0)
      delete[] data;
  }
  
  char &operator[](int n) { return data[n]; }

  string operator[](const Range &range) const {
    string result(data + range.first, data + range.last, true);
    return result;
  }

  auto begin() { return data; }
  auto end() { return data + length; }

  bool operator==(const string &other) const {
    if (length != other.length)
      return false;
    for (int i = 0; i < length; ++i) {
      if (data[i] != other.data[i])
        return false;
    }
    return true;
  }

  bool operator!=(const string &other) const { return !(*this == other); }

  auto begin() const { return data; }
  auto end() const { return data + length; }
};

inline string Range::to_string() const {
  char buffer[50];
  snprintf(buffer, sizeof(buffer), "%lld..%lld", first, last);
  return string(buffer);
}

namespace std {
template <> struct hash<string> {
  std::size_t operator()(const string &s) const noexcept {
    std::size_t h = 0;
    for (char c : s) {
      h = h * 31 + c;
    }
    return h;
  }
};
} // namespace std

struct Type;
struct Field {
  char *name;
  Type *type;
  size_t size;
  size_t offset;

  template <class T, class T1> inline void set(T *target, T1 data) const {
    memcpy(reinterpret_cast<char *>(target) + offset, (char *)&data, sizeof(T1));
  }

  template <class T> inline s8 *get(T *source) const {
    return reinterpret_cast<s8 *>(reinterpret_cast<char *>(source) + offset);
  }
};

struct Element {
  char *data;
  Type *type;
};

struct Type {
  int id;                              // the integer ID used to identify this type.
  char *name;                          // the type's name.
  size_t size;                         // sizeof(T)
  u64 flags;                           // defined in reflection.ela and emit.cpp, the values of the flags.
  _array<Field *> fields;              // get a list of struct fields, enum variants.
  _array<Element> (*elements)(char *); // get a list of the Elements, which can be used to reflect on arrays.
  Type *element_type;                  // the type this type has a pointer to, is an array of, is a map of, etc.
};

static _array<string> &Env_args() {
  static _array<string> args;
  return args;
}

#ifdef TESTING

#define assert(message, condition)                                                                                     \
  if (!(condition))                                                                                                    \
    throw __test_exception("\033[31mAssertion failed: \n\t\033[1;31mcondition ::\033[0m(\033[1;34m%s\033[0m), "        \
                           "\n\t\033[1;31mmessage   ::\033[0m(\033[1;34m%s\033[0m])\033[0m\n",                         \
                           #condition, message);

extern "C" int snprintf(char *buf, size_t size, const char *fmt, ...);
extern "C" int strcpy(const char *, char *);
extern "C" int strlen(const char *);
struct __test_exception {
  const char *m_what;

  template <typename... Args> __test_exception(const char *fmt, Args &&...args) {
    char buf[1024];
    snprintf(buf, sizeof(buf), fmt, args...);
    m_what = new char[strlen(buf) + 1];
    strcpy(const_cast<char *>(m_what), buf);
  }

  ~__test_exception() { delete[] m_what; }

  const char *what() const { return m_what; }
};
struct __COMPILER_GENERATED_TEST {
  __COMPILER_GENERATED_TEST() {}
  __COMPILER_GENERATED_TEST(const char *name, void (*function)()) : name(name), function(function) {}
  const char *name;
  void (*function)();
  bool run() const {
#ifdef TEST_VERBOSE
    printf("\033[1;33mtesting \033[1;37m...\033[1;36m%-40s", name);
    try {
      function();
      printf("\033[1;32m[passed]\033[0m\n");
      return true;
    } catch (__test_exception &e) {
      printf("\033[1;31m[failed]\033[0m\n");
      printf("%s", e.what());
      return false;
    }
#else
    try {
      function();
      return true;
    } catch (__test_exception &e) {
      printf("%s", e.what());
      return false;
    }
#endif
  }
};

#define __TEST_RUNNER_MAIN                                                                                             \
  int main() {                                                                                                         \
    int failed = 0;                                                                                                    \
    int passed = 0;                                                                                                    \
    const char *failed_tests[sizeof(tests) / sizeof(tests[0])];                                                        \
    int failed_index = 0;                                                                                              \
    for (const auto &test : tests) {                                                                                   \
      if (test.run()) {                                                                                                \
        passed++;                                                                                                      \
      } else {                                                                                                         \
        failed_tests[failed_index++] = test.name;                                                                      \
        failed++;                                                                                                      \
      }                                                                                                                \
    }                                                                                                                  \
    printf("\033[1;31mfailed: %d, \033[1;32mpassed: "                                                                  \
           "%d\033[0m\n",                                                                                              \
           failed, passed);                                                                                            \
    if (failed > 0) {                                                                                                  \
      for (int i = 0; i < failed_index; ++i) {                                                                         \
        printf("\033[1;31mfailed \033[0m::(\033[1;35m%s\033[0m)\n", failed_tests[i]);                                  \
      }                                                                                                                \
    }                                                                                                                  \
  }

#else
#define assert(message, condition)                                                                                     \
  if (!(condition)) {                                                                                                  \
    printf("assertion failed: %s\n " #condition "\n", message);                                                        \
    exit(1);                                                                                                           \
  }

#endif

#else
using float64 = double;
using u64 = unsigned long long int;
using s64 = signed long long int;

using s32 = signed int;
using u32 = unsigned int;
using float32 = float;

using s16 = short int;
using u16 = unsigned short int;

using s8 = signed char;
using u8 = unsigned char;
#endif
