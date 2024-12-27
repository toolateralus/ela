//  * Manually call destructor on one or many objects. useful for unions that
//  own non-trivial objects.
template <class... T> void destruct(T &...t) { (t.~T(), ...); }

#if USE_STD_LIB
struct string;
#endif

struct Range {
  using value_type = signed long long;
  Range() {}
  Range(value_type first, value_type last, value_type increment = 1)
      : first(first), last(last), span(last - first), is_reverse(first > last),
        increment(increment) {
    if (is_reverse) {
      first--;
      span = first - last;
    }
  }
  Range(const Range &other) {
    first = other.first;
    last = other.last;
    span = other.span;
    is_reverse = other.is_reverse;
    increment = other.increment;
  }
  value_type first = 0, last = 0, span = 0, increment = 1;
  bool is_reverse = false;

  struct iterator {
    value_type current, end_value, increment;
    bool is_reverse;
    iterator(value_type start, value_type end_value, bool is_reverse,
             value_type increment)
        : current(start), end_value(end_value), is_reverse(is_reverse),
          increment(increment) {}
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

  iterator begin() const {
    return iterator(first, last, is_reverse, increment);
  }
  iterator end() const { return iterator(first, last, is_reverse, increment); }
  iterator begin() { return iterator(first, last, is_reverse, increment); }
  iterator end() { return iterator(first, last, is_reverse, increment); }

  bool operator==(const value_type number) const {
    return number >= first && number <= last;
  }

  bool contains(const value_type number) {
    return number >= first && number <= last;
  }

#if USE_STD_LIB
  string to_string() const;
#endif
};

#if USE_STD_LIB

#include <initializer_list>
#include <stdint.h>
#include <unordered_map>

// TODO: replace these with our own types.
// They may not be faster, but they might be. What will be faster is compilation
// times.
template <class Key, class Value> using _map = std::unordered_map<Key, Value>;

template <typename...> struct _tuple {
  _tuple() = default;
  ~_tuple() = default;
};

template <typename Head, typename... Tail> struct _tuple<Head, Tail...> {
  Head head;
  _tuple<Tail...> tail;
  _tuple() = default;
  _tuple(Head h, Tail... t) : head(h), tail(t...) {}
};

template <typename... Args> _tuple(Args...) -> _tuple<Args...>;

// Helper to get the size of the tuple
template <typename T> struct tuple_size;

template <typename... Args>
struct tuple_size<_tuple<Args...>>
    : std::integral_constant<std::size_t, sizeof...(Args)> {};

// Helper to get the type of the element at a specific index
template <std::size_t I, typename T> struct tuple_element;

template <typename Head, typename... Tail>
struct tuple_element<0, _tuple<Head, Tail...>> {
  using type = Head;
};

template <std::size_t I, typename Head, typename... Tail>
struct tuple_element<I, _tuple<Head, Tail...>> {
  using type = typename tuple_element<I - 1, _tuple<Tail...>>::type;
};

// Original get function for lvalue references
template <std::size_t I, typename Head, typename... Tail>
decltype(auto) get(_tuple<Head, Tail...> &t) {
  if constexpr (I == 0) {
    return t.head;
  } else {
    return get<I - 1>(t.tail);
  }
}

// Overload for const lvalue references
template <std::size_t I, typename Head, typename... Tail>
decltype(auto) get(const _tuple<Head, Tail...> &t) {
  if constexpr (I == 0) {
    return t.head;
  } else {
    return get<I - 1>(t.tail);
  }
}

// Overload for rvalue references
template <std::size_t I, typename Head, typename... Tail>
decltype(auto) get(_tuple<Head, Tail...> &&t) {
  if constexpr (I == 0) {
    return std::move(t.head);
  } else {
    return get<I - 1>(std::move(t.tail));
  }
}

namespace std {
template <typename... Args>
struct tuple_size<_tuple<Args...>>
    : std::integral_constant<std::size_t, sizeof...(Args)> {};
template <std::size_t I, typename... Args>
struct tuple_element<I, _tuple<Args...>> {
  using type = typename ::tuple_element<I, _tuple<Args...>>::type;
};
} // namespace std

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

#undef RAND_MAX
#undef assert

// TODO: implement `Any` type... not sure how we want to approach it.
// template <class T> struct Any {
//   T value;
//   template <class U> Any(U&& v) : value(std::forward<U>(v)) {}
//   template <class U> operator U() { return static_cast<U>(value); }
// };

template <class T> struct _array {
  T *data = nullptr;
  s32 length = 0;
  s32 capacity = 0;
  bool is_view = false;

  _array() : data(nullptr), length(0), capacity(0), is_view(false) {}

  _array(int length)
      : data(new T[length]), length(length), capacity(length), is_view(false) {}

  _array(T *array, int len)
      : data(new T[len]), length(len), capacity(len), is_view(false) {
    std::copy(array, array + len, data);
  }

  _array(T *begin, T *end) {
    length = std::distance(end - begin);
    capacity = length;
    data = new T[length];
    std::copy(begin, end, data);
  }

  _array(const _array &other)
      : data(new T[other.length]), length(other.length),
        capacity(other.capacity), is_view(other.is_view) {
    std::copy(other.data, other.data + other.length, data);
  }

  _array(_array &&other) noexcept
      : data(other.data), length(other.length), capacity(other.capacity),
        is_view(other.is_view) {
    other.data = nullptr;
    other.length = 0;
    other.capacity = 0;
    other.is_view = false;
  }

  _array &operator=(const _array &other) {
    if (this != &other) {
      T *new_data = new T[other.length];
      std::copy(other.data, other.data + other.length, new_data);
      if (data && !is_view) {
        delete[] data;
      }
      data = new_data;
      length = other.length;
      capacity = other.capacity;
      is_view = other.is_view;
    }
    return *this;
  }

  _array &operator=(_array &&other) noexcept {
    if (this != &other) {
      if (data && !is_view)
        delete[] data;
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

  _array(std::initializer_list<T> list)
      : length(list.size()), capacity(list.size()), is_view(false) {
    if (list.size() != 0) {
      data = new T[list.size()];
      std::copy(list.begin(), list.end(), data);
    }
    length = list.size();
  }

  ~_array() {
    if (!is_view && data) {
      delete[] data;
      data = nullptr;
    }
  }

  void resize(int new_capacity) {
    if (new_capacity > capacity) {
      T *new_data = new T[new_capacity];
      std::move(data, data + length, new_data);
      if (data && !is_view) {
        delete[] data;
      }
      data = new_data;
      capacity = new_capacity;
    }
  }

  void push(const T &value) {
    if (length == capacity) {
      resize(capacity == 0 ? 1 : capacity * 2);
    }
    data[length++] = value;
  }

  T pop() { return data[--length]; }

  void erase(const T &value) {
    std::size_t writeIndex = 0;
    for (std::size_t readIndex = 0; readIndex < length; ++readIndex) {
      if (data[readIndex] != value) {
        data[writeIndex] = data[readIndex];
        ++writeIndex;
      }
    }
    length = writeIndex;
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
    return length == other.length &&
           std::equal(data, data + length, other.data);
  }

  bool operator!=(const _array &other) const { return !(*this == other); }

  explicit operator void *() { return (void *)data; }
  explicit operator T *() { return (T *)data; }

  auto begin() const { return data; }
  auto end() const { return data + length; }
  auto begin() { return data; }
  auto end() { return data + length; }
};

// For now, we'll just use a simple null terminated string.
struct string {
  char *data = nullptr;
  s32 length = 0;
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
    data = new char[length + 1];
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
  ~string() {
    if (data && !is_view)
      delete[] data;
  }
  char &operator[](int n) { return data[n]; }
  explicit operator char *() { return data; }

  string operator[](const Range &range) const {
    string result(data + range.first, data + range.last, true);
    return result;
  }

  string substr(int start, int end) const {
    if (start < 0 || end > length || start > end) {
      return string();
    }
    return string(data + start, data + end, false);
  }

  string substr(const Range &r) const { return substr(r.first, r.last); }

  void push(const char &value) {
    char *new_data = new char[length + 2];
    std::copy(data, data + length, new_data);
    new_data[length] = value;
    new_data[length + 1] = '\0';
    delete[] data;
    data = new_data;
    ++length;
  }

  void erase_at(int index) {
    if (index < 0 || index >= length || length <= 0) {
      return;
    }
    char *new_data = new char[length];
    std::copy(data, data + index, new_data);
    std::copy(data + index + 1, data + length, new_data + index);
    delete[] data;
    data = new_data;
    --length;
    data[length] = '\0';
  }

  void insert_at(int index, const char &value) {
    if (data == nullptr) {
      data = new char[2];
      data[0] = value;
      data[1] = '\0';
      length = 1;
      return;
    }
    char *new_data = new char[length + 2];
    std::copy(data, data + index, new_data);
    new_data[index] = value;
    std::copy(data + index, data + length, new_data + index + 1);
    new_data[length + 1] = '\0';
    delete[] data;
    data = new_data;
    ++length;
  }

  void insert_substr_at(int index, const string &substr) {
    if (data == nullptr) {
      data = new char[substr.length + 1];
      std::copy(substr.data, substr.data + substr.length, data);
      data[substr.length] = '\0';
      length = substr.length;
      return;
    }
    int substr_length = substr.length;
    char *new_data = new char[length + substr_length + 1];
    std::copy(data, data + index, new_data);
    std::copy(substr.data, substr.data + substr_length, new_data + index);
    std::copy(data + index, data + length, new_data + index + substr_length);
    new_data[length + substr_length] = '\0';
    delete[] data;
    data = new_data;
    length += substr_length;
  }

  char pop() {
    if (length <= 0) {
      return 0;
    }
    char value = data[length - 1];
    char *new_data = new char[length];
    std::copy(data, data + length - 1, new_data);
    new_data[length - 1] = '\0';
    delete[] data;
    data = new_data;
    --length;
    return value;
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
    memcpy(reinterpret_cast<char *>(target) + offset, (char *)&data,
           sizeof(T1));
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
  int id;
  char *name;
  size_t size;
  u64 flags; // defined in reflection.ela and emit.cpp, the values of the flags.
  _array<Field *> fields;
  _array<Element> (*elements)(char *);
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
#define assert(message, condition)                                             \
  if (!(condition)) {                                                          \
    printf("assertion failed: %s\n "#condition"\n", message);                                 \
    exit(1);                                                                   \
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
