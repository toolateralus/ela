#define TESTING
#define USE_STD_LIB 1
#include "/usr/local/lib/ela/boilerplate.hpp"
extern Type **_type_info;
#define TESTING
extern "C" int system(const char*);;
extern "C" void free(void*);;
extern "C" void* malloc(u64);;
extern "C" void* calloc(u64, u64);;
extern "C" void* realloc(void*, u64);;
extern "C" void* memcpy(void*, void*, u64);;
extern "C" void* memset(void*, int, u64);;
extern "C" int memmove(void*, void*, s64);;
extern "C" int printf(const char*, ...);;
extern "C" void exit(int);;
;
static static void println(string str) {
  printf((char*)"%s\n", [&] -> string { char* buf = new char[1024];
sprintf(buf, "%s",str.data);
 auto str = string(); str.data = buf; str.length = strlen(buf); return str; }().data);
};
static static void panic(string msg) {
  println(msg);
  exit((s32)1);
};
extern "C" int scanf(const char*, ...);;
extern "C" int getchar();;
extern "C" void sleep(int);;
extern "C" void usleep(int);;
extern "C" const char* strdup(const char*);;
extern "C" const char* strndup(const char*, u64);;
extern "C" const char* strerror(int);;
extern "C" s64 strtol(const char*, char***, int);;
extern "C" u64 strtoul(const char*, char***, int);;
extern "C" float64 strtod(const char*, char***);;
extern "C" const char* strtok(const char*, const char*);;
extern "C" const char* strchr(const char*, int);;
extern "C" const char* strrchr(const char*, int);;
extern "C" const char* strstr(const char*, const char*);;
extern "C" int strlen(const char*);;
extern "C" int strcmp(const char*, const char*);;
extern "C" const char* strcat(const char*, const char*);;
extern "C" int snprintf(char*, u64, const char*, ...);;
extern "C" int sprintf(char*, const char*, ...);;
extern "C" int strncmp(const char*, const char*, int);;
extern "C" int isalnum(int);;
extern "C" int isalpha(int);;
extern "C" int isspace(int);;
extern "C" int isdigit(int);;
extern "C" int islower(int);;
extern "C" int isupper(int);;
extern "C" int isprint(int);;
extern "C" int ispunct(int);;
extern "C" int atoi(const char*);;
extern "C" float64 atof(const char*);;
extern "C" int rand();;
extern "C" void srand(u32);;
struct Random{
  static constexpr int Max  = (s32)2147483647;
};
;
float Random_next(float32 max) {
  return (((float)rand() / (float)Random::Max) * max);
;
};
struct Strings;
;
string string_empty() {
  return {};
;
}string string_from(char* data) {
  int len  = strlen(data);
  string str {};
  (str.data =(char*) strdup(data));
  (str.length = len);
  return str;
;
}string string_from_ptr(char* begin, char* end) {
  string str {};
  (str.length = (end - begin));
  (str.data =(char*) malloc((str.length + (s32)1)));
  memcpy(str.data, begin, str.length);
  (str.data[str.length] = (s32)0);
  return str;
;
}void string_insert(string* self, s64 pos, string substr) {
  if (((pos < (s32)0) || (pos > self->length))) {
    return;
;
  };
  char* new_data  = (char*)malloc(((self->length + substr.length) + (s32)1));
  memcpy(new_data, self->data, pos);
  memcpy((new_data + pos), substr.data, substr.length);
  memcpy(((new_data + pos) + substr.length), (self->data + pos), (self->length - pos));
  (new_data[(self->length + substr.length)] = (s32)0);
  (self->data =(char*) new_data);
  (self->length += substr.length);
}bool string_starts_with(string* self, string substr) {
  if (((self->data == (std::nullptr_t)nullptr) || (substr.data == (std::nullptr_t)nullptr))) {
    return (bool)false;
;
  };
  int str_len  = self->length;
  int substr_len  = substr.length;
  if ((str_len < substr_len)) {
    return (bool)false;
;
  };
  if (((str_len == substr_len) && (strcmp(self->data, substr.data) == (s32)0))) {
    return (bool)true;
;
  };
  return (strncmp(self->data, substr.data, substr_len) == (s32)0);
;
}bool string_ends_with(string* self, string pattern) {
  if (((!self->data) || (!pattern.data))) {
    return (bool)false;
;
  };
  int len  = self->length;
  int patlen  = pattern.length;
  if ((len < patlen)) {
    return (bool)false;
;
  };
  return (strncmp((self->data + (len - patlen)), pattern.data, patlen) == (s32)0);
;
}string string_replace(string* self, string pattern, string replacement) {
  if ((((!self->data) || (!pattern.data)) || (!replacement.data))) {
    return (*self);
;
  };
  string result {};
  int str_len  = self->length;
  int pattern_len  = pattern.length;
  int replacement_len  = replacement.length;
  int i {};
  while ((i < str_len)) {
    if ((strncmp((self->data + i), pattern.data, pattern_len) == (s32)0)) {
      string_insert(&result, result.length, replacement);
      (i += pattern_len);
    } else  {
      string_insert(&result, result.length, string_from_ptr((self->data + i), ((self->data + i) + (s32)1)));
      (i += (s32)1);
    };
  };
  return result;
;
}string string_substr(string* self, int start, int end) {
  if ((self->length == (s32)0)) {
    return {};
;
  };
  int len  = self->length;
  if ((start > end)) {
    return {};
;
  };
  if ((len < (end - start))) {
    return {};
;
  };
  char* dest  = (char*)malloc(((sizeof(char) * (end - start)) + (s32)1));
  memcpy(dest, (self->data + start), (end - start));
  (dest[(end - start)] = (s32)0);
  return string_from_ptr(dest, (dest + (end - start)));
;
}_array<string> string_split(string* self, char delimiter) {
  if ((!self->data)) {
    return {};
;
  };
  _array<string> result {};
  int start  = (s32)0;
  int str_len  = self->length;
  for (auto i : Range((s32)0, str_len)) {
    if ((self->data[i] == delimiter)) {
      if ((i > start)) {
        result.push(string_from_ptr((self->data + start), (self->data + i)));
;
      };
      (start = (i + (s32)1));
    };
  };
  result.push(string_from_ptr((self->data + start), (self->data + str_len)));
;
  return result;
;
}bool string_contains(string* self, string pattern) {
  if (((self->data == (std::nullptr_t)nullptr) || (pattern.data == (std::nullptr_t)nullptr))) {
    return (bool)false;
;
  };
  int str_len  = self->length;
  int pattern_len  = pattern.length;
  if ((str_len < pattern_len)) {
    return (bool)false;
;
  };
  for (auto i : Range((s32)0, ((str_len - pattern_len) + (s32)1))) {
    if ((strncmp((self->data + i), pattern.data, pattern_len) == (s32)0)) {
      return (bool)true;
;
    };
  };
  return (bool)false;
;
};
string to_string(s64 num) {
   if (num == (s32)0)  {
    return string_from((char*)"0");
;
  } else  if (num == (s32)1)  {
    return string_from((char*)"1");
;
  } else  if (num == (s32)2)  {
    return string_from((char*)"2");
;
  } else  if (num == (s32)3)  {
    return string_from((char*)"3");
;
  } else  if (num == (s32)4)  {
    return string_from((char*)"4");
;
  } else  if (num == (s32)5)  {
    return string_from((char*)"5");
;
  } else  if (num == (s32)6)  {
    return string_from((char*)"6");
;
  } else  if (num == (s32)7)  {
    return string_from((char*)"7");
;
  } else  if (num == (s32)8)  {
    return string_from((char*)"8");
;
  } else  if (num == (s32)9)  {
    return string_from((char*)"9");
;
  };
  bool negative  = (bool)false;
  if ((num < (s32)0)) {
    (negative = (bool)true);
    (num = (s64)(-num));
  };
  char buffer[20]{};
  int index  = (s32)0;
  while ((num > (s32)0)) {
    (buffer[index] = ((char)'0' + (num % (s32)10)));
    (num /= (s32)10);
    index++;
  };
  if (negative) {
    (buffer[index] = (char)'-');
    index++;
  };
  for (auto i : Range((s32)0, (index / (s32)2))) {
    int temp  = buffer[i];
    (buffer[i] = buffer[((index - i) - (s32)1)]);
    (buffer[((index - i) - (s32)1)] = temp);
  };
  return string_from_ptr((char*)buffer, ((char*)buffer + index));
;
};
extern "C" float fmod(float, float);;
extern "C" float64 fabs(float64);;
extern "C" bool isinf(float64);;
extern "C" bool isnan(float64);;
extern "C" float64 pow(float64, float64);;
extern "C" float64 sqrt(float64);;
extern "C" float64 cos(float64);;
extern "C" float64 sin(float64);;
;
;
;
float64 clamp(float64 v, float64 min, float64 max) {
  if ((v < min)) {
    return min;
;
  };
  if ((v > max)) {
    return max;
;
  };
  return v;
;
};
float lerp(float a, float b, float t) {
  return ((a * ((float32)1.0f - t)) + (b * t));
;
};
extern "C" int time(void*);;
struct timespec_t{
  s64 tv_sec {};
  s64 tv_nsec {};
};
;
extern "C" int clock_gettime(int, timespec_t*);;
struct tm_t{
  int tm_sec {};
  int tm_min {};
  int tm_hour {};
  int tm_mday {};
  int tm_mon {};
  int tm_year {};
  int tm_wday {};
  int tm_yday {};
  int tm_isdst {};
};
;
extern "C" tm_t* localtime_r(s64*, tm_t*);;
struct Timing;
;
timespec_t Timing_get_timespec_since_epoch() {
  timespec_t ts {};
  clock_gettime((s32)0, (&ts));
  return ts;
;
}float64 Timing_seconds() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return ((float64)ts.tv_sec + ((float64)ts.tv_nsec / (float64)(float32)1000000000.0f));
;
}float64 Timing_milliseconds() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return (((float64)ts.tv_sec * (float32)1000.0f) + ((float64)ts.tv_nsec / (float64)(float32)1000000.0f));
;
}float64 Timing_microseconds() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return (((float64)ts.tv_sec * (float32)1000000.0f) + ((float64)ts.tv_nsec / (float32)1000.0f));
;
}float64 Timing_minutes() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return (((float64)ts.tv_sec / (float32)60.0f) + ((float64)ts.tv_nsec / ((float32)60.0f * (float32)1000000000.0f)));
;
}float64 Timing_hours() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return (((float64)ts.tv_sec / (float32)3600.0f) + ((float64)ts.tv_nsec / (float64)((float32)3600.0f * (float32)1000000000.0f)));
;
}float64 Timing_days() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  return (((float64)ts.tv_sec / (float32)86400.0f) + ((float64)ts.tv_nsec / (float64)((float32)86400.0f * (float32)1000000000.0f)));
;
}string Timing_date_time() {
  timespec_t ts  = Timing_get_timespec_since_epoch();
  s64 time_t  = ts.tv_sec;
  tm_t tm {};
  localtime_r((&time_t), (&tm));
  char buffer[64]{};
  snprintf(buffer, (s32)64, (char*)"%04d-%02d-%02d %02d:%02d:%02d", (tm.tm_year + (s32)1900), (tm.tm_mon + (s32)1), tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
  return string_from_ptr((char*)buffer, ((char*)buffer + strlen(buffer)));
;
};
void* get_null_after_malloc() {
  void* n  = (void*)malloc((s32)16);
;
 {
    free(n);
    (n =(void*) (std::nullptr_t)nullptr);
  };
  return n;
;
};
void test_defer() {
  assert((char*)"defer failed", (get_null_after_malloc() == (std::nullptr_t)nullptr));
};
__COMPILER_GENERATED_TEST tests[1] = { __COMPILER_GENERATED_TEST("test_defer", test_defer) };
__TEST_RUNNER_MAIN;