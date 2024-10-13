#include <cstdint>
#include <unordered_map>
#include <vector>

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
  std::vector<T> vector;

  s64 length;
  s64 capacity;
  void *data;

  void update() {
    length = vector.size();
    capacity = vector.capacity();
    data = vector.data();
  }
  _array() {
    update();
  }
  _array(const _array &other) {
    vector = other.vector;
    update();
  }
  _array(int length) {
    vector.resize(length);
    update();
  }
  _array(T *array, int len) {
    vector = std::vector(array, array + len);
    update();
  }
  bool operator==(const _array &other) const { return vector == other.vector; }
  bool operator!=(const _array &other) const { return vector != other.vector; }
  void erase(const T &value) {
    auto it = std::remove(vector.begin(), vector.end(), value);
    if (it != vector.end()) {
      vector.erase(it, vector.end());
    }
    update();
  }
  
  auto &operator[](int n) const { return vector[n]; }
  auto &operator[](int n) { return vector[n]; }
  
  explicit operator void *() { return (void *)vector.data(); }
  explicit operator T *() { return (T *)vector.data(); }
  _array(std::initializer_list<T> list) {
    vector = std::vector<T>(list);
    update();
  }

  auto begin() const { return vector.data(); }
  auto end() const { return vector.data() + vector.size(); }
  auto begin() { return vector.data(); }
  auto end() { return vector.data() + vector.size(); }
  void push(const T &value) {
    vector.push_back(value);
    update();
  }
  auto pop() {
    auto value = vector.back();
    vector.pop_back();
    update();
    return value;
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

// Specialize std::hash for your string class
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

template <class Key, class Value> using _map = std::unordered_map<Key, Value>;

extern "C" void *memcpy(void *, void *, size_t);

struct Type;
struct Field {
  char *name;
  Type *type;
  size_t size;
  size_t offset;
  
  template<class T, class T1>
  void set(T *target, T1 data) const {
    memcpy(reinterpret_cast<char*>(target) + offset, (char*)&data, sizeof(T1));
  }
  
  template<class T>
  s8* get(T source) const {
    return reinterpret_cast<s8*>(reinterpret_cast<char*>(source) + offset);
  }
};

struct Type {
  int id;
  char *name;
  _array<Field *> fields;
  size_t size;
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
