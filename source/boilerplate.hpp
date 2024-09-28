#include <cstdint>
#include <cstddef>

using f64 = double;
using u64 = size_t;
using s64 = int64_t;

using s32 = int32_t;
using u32 = uint32_t;
using f32 = float;

using s16 = int16_t;
using u16 = uint16_t;

using s8 = int8_t;
using u8 = uint8_t;

using string = const char *;

extern "C" int printf(const char *format, ...);

// UNUSED
// TODO: implement a bare minimum array type.
template <class T> struct _array {
  T *m_data = nullptr;
  int m_length = 0;
  _array() {}
  T *begin() const { return m_data; }
  T *end() const { return m_data + m_length; }
};

#define TESTING
#ifdef TESTING
#define assert(message, condition)                                             \
  if (!(condition))                                                            \
    throw __test_exception("\e[31mAssertion failed: %s, message: %s\e[0m\n", #condition,       \
           #message);                                                           \
  
  extern "C" int snprintf(char *buf, size_t size, const char *fmt, ...);
  extern "C" int strcpy(const char *, char *);
  extern "C" int strlen(const char *);
  struct __test_exception {
    const char *m_what;

    template<typename ...Args>
    __test_exception(const char *fmt, Args&&... args) {
        char buf[1024];
        snprintf(buf, sizeof(buf), fmt, args...);
        m_what = new char[strlen(buf) + 1];
        strcpy(const_cast<char*>(m_what), buf);
    }

    ~__test_exception() {
        delete[] m_what;
    }

    const char* what() const {
        return m_what;
    }
};                                               
  struct __COMPILER_GENERATED_TEST {                                           
    __COMPILER_GENERATED_TEST() {}                                             
    __COMPILER_GENERATED_TEST(const char *name, void (*function)())            
        : name(name), function(function) {}                                    
    const char *name;                                                          
    void (*function)();                                                        
    void run() const {                                                         
      printf("\033[1;33mtesting \033[1;37m...\033[1;36m%-40s", name);          
      try {                                                                    
        function();                                                            
        printf("\033[1;32m[passed]\033[0m\n");                                 
      } catch (__test_exception &e) {                                                          
        printf("\033[1;31m[failed]\033[0m\n");
        printf("%s", e.what());
      }                                                                        
    }                                                                          
  };


#define __TEST_RUNNER_MAIN                                                     \
  int main() {                                                                 \
    for (const auto &test : tests) {                                           \
      test.run();                                                              \
    }                                                                          \
  }
#else
#endif
