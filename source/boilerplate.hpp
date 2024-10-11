#include <cstdint>

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

#undef RAND_MAX
#undef assert

#include <algorithm>
#include <initializer_list>

struct Range {
  int m_begin, m_end;
  Range(int m_begin, int m_end) : m_begin(m_begin), m_end(m_end) {}
  struct iterator {
    int current;
    iterator(int start) : current(start) {}
    int operator*() const { return current; }
    iterator &operator++() {
      ++current;
      return *this;
    }
    bool operator!=(const iterator &other) const {
      return current != other.current;
    }
  };
  iterator begin() const { return iterator(m_begin); }
  iterator end() const { return iterator(m_end); }
  iterator begin() { return iterator(m_begin); }
  iterator end() { return iterator(m_end); }
};

// TODO: implement this. not sure how we want to approach it.
// template <class T> struct Any {
//   T value;
//   template <class U> Any(U&& v) : value(std::forward<U>(v)) {}
//   template <class U> operator U() { return static_cast<U>(value); }
// };

// TODO: implement this actually, where this will represent all dynamic arrays
// in the language, such as int[]. int[32] is still a C fixed buffer.
template <class T> struct _array {
  T *data = nullptr;
  int length = 0;
  int capacity = 1;
  _array() { data = new T[capacity]; }
  T &operator[](int n) { return data[n]; }
  _array(const _array &other) {
    length = other.length;
    capacity = other.capacity;
    data = new T[capacity];
    std::copy(other.data, other.data + length, data);
  }
  _array(int length) {
    capacity = length * 1.5f;
    this->length = length;
    data = new T[length];
  }
  _array(T *array, int len) {
    length = len;
    capacity = len;
    data = new T[capacity];
    std::copy(array, array + length, data);
  }

  void erase(const T &value) {
    auto it = std::find(begin(), end(), value);
    if (it != end()) {
      std::move(it + 1, end(), it);
      --length;
    }
  }

  T &operator[](int n) const { return data[n]; }
  explicit operator void *() { return data; }
  explicit operator T *() { return data; }
  template <class From>
    requires std::is_convertible_v<From, T>
  operator _array<From>() {
    _array<From> result;
    result.length = length;
    result.data = new From[length];
    std::copy(data, data + length, result.data);
    return result;
  }
  _array(std::initializer_list<T> list) {
    length = list.size();
    capacity = length * 1.5f;
    data = new T[capacity];
    std::copy(list.begin(), list.end(), data);
  }
  ~_array() {
    if (data)
      delete[] data;
  }
  T *begin() const { return data; }
  T *end() const { return data + length; }
  void push(const T &value) {
    if (length >= capacity) {
      reserve(capacity * 2);
    }
    data[length++] = value;
  }
  T pop() {
    if (length > 0) {
      auto value = data[length - 1];
      --length;
      return value;
    }
    return T();
  }
  void resize(int new_size) {
    if (new_size > capacity) {
      reserve(new_size);
    }
    length = new_size;
  }
  void reserve(int new_capacity) {
    if (new_capacity > capacity) {
      T *new_data = new T[new_capacity];
      std::copy(data, data + length, new_data);
      delete[] data;
      data = new_data;
      capacity = new_capacity;
    }
  }
};

// For now, we'll just use a simple null terminated string.
struct string {
  char *data = nullptr;
  int length = 0;
  string() {}
  string(char *str) {
    if (str == nullptr) {
      return;
    }
    length = 0;
    while (str[length] != '\0') {
      ++length;
    }
    data = new char[length + 1];
    data[length] = '\0';
    std::copy(str, str + length, data);
  }

  string(char *begin, char *end) {
    length = std::distance(begin, end);
    data = new char[length + 1];
    std::copy(begin, end, data);
    data[length] = '\0';
  }

  ~string() {
    if (data)
      delete[] data;
  }
  char &operator[](int n) { return data[n]; }
  explicit operator char *() { return data; }
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
      delete[] data;
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
  auto begin() { return data; }
  auto end() { return data + length; }
};

struct Type;
struct Field {
  char *name;
  Type *type;
};

struct Type {
  int id;
  char *name;
  _array<Field *> fields;
};

#ifdef TESTING

#define assert(message, condition)                                             \
  if (!(condition))                                                            \
    throw __test_exception("\e[31mAssertion failed: %s, message: %s\e[0m\n",   \
                           #condition, #message);

extern "C" int snprintf(char *buf, size_t size, const char *fmt, ...);
extern "C" int strcpy(const char *, char *);
extern "C" int strlen(const char *);
struct __test_exception {
  const char *m_what;

  template <typename... Args>
  __test_exception(const char *fmt, Args &&...args) {
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
  __COMPILER_GENERATED_TEST(const char *name, void (*function)())
      : name(name), function(function) {}
  const char *name;
  void (*function)();
  bool run() const {

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
  }
};

#define __TEST_RUNNER_MAIN                                                     \
  int main() {                                                                 \
    int failed = 0;                                                            \
    int passed = 0;                                                            \
    for (const auto &test : tests) {                                           \
      if (test.run())                                                          \
        passed++;                                                              \
      else                                                                     \
        failed++;                                                              \
    }                                                                          \
    printf("Test run complete!\n\033[1;31mfailed: %d, \033[1;32mpassed: "      \
           "%d\033[0m\n",                                                      \
           failed, passed);                                                    \
  }
#else
#endif
