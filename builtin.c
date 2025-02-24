#define TESTING
#define USE_STD_LIB 1

  
  typedef unsigned long long int u64;
  typedef signed long long int s64;
  
  typedef signed int s32;
  typedef unsigned int u32;
  
  typedef double f64;
  typedef float f32;
  
  typedef short int s16;
  typedef unsigned short int u16;
  
  typedef signed char s8;
  typedef unsigned char u8;
  #include <stddef.h>
  
  #if USE_STD_LIB
    #include <stdint.h>
    #include <errno.h>
    #undef RAND_MAX
  #endif
  
  #ifdef TESTING
    int printf(u8 *, ...);
    void exit(s32);
    
    typedef struct {
      const char *name;
      void (*function)();
    } $ela_test;
    static void $ela_test_run($ela_test *test) {
      #if TEST_VERBOSE 
        printf("running %s\n", test->name);
      #endif
      test->function();
    }
  #endif
  
typedef struct Type Type;
typedef struct List$30 List$30;
extern List$30 _type_info;
#define TESTING
typedef struct Type Type;
typedef struct  Range_Enumerator$1{
  s32 begin;
  s32 end;
  s32 idx;
} Range_Enumerator$1;
typedef struct  Range_Base$1{
  s32 begin;
  s32 end;
} Range_Base$1;
typedef struct Range_Base$1 Range_Base$1;
Range_Enumerator$1 $45_enumerator( Range_Base$1* self) {
  return (Range_Enumerator$1) {.begin = (s32)self->begin,
.end = (s32)self->end,
.idx = (s32)0};
;
}bool $77_done( Range_Enumerator$1 self) {
  return (self.idx >= self.end);
;
}s32 $77_current( Range_Enumerator$1 self) {
  return self.idx;
;
}typedef struct Range_Enumerator$1 Range_Enumerator$1;
void $77_next( Range_Enumerator$1* self) {
  self->idx++;
}typedef struct  Std_Allocator{
} Std_Allocator;
typedef struct  str{
  u8* data;
  u64 length;
} str;
typedef struct  String{
  u8* data;
  u64 length;
  u64 capacity;
} String;
extern u8* strdup(u8*);typedef struct str str;
u32 strlen(u8* string) {
  if ((!string)) {
    return 0;
;
  };
  if (((u32)string[0] == '\0')) {
    return 0;
;
  };
  s32 i  = 0;
;
  while ((*string))  {
    i++;
    string++;
  };
  return i;
;
}typedef struct  Slice$7{
  u8* data;
  u64 length;
} Slice$7;
typedef struct {s32 $0;
s32 $1;
} $tuple1$1;
typedef struct  Iter$7{
  u8* ptr;
  u8* end;
} Iter$7;
u8 $13_subscript( str* self, s64 idx) {
  return self->data[idx];
;
}bool $13_eq( str self, str other) {
  if ((self.length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 idx = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if (($13_subscript(&self, idx) != $13_subscript(&other, idx))) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}Iter$7 $13_iter( str* self) {
  return (Iter$7) {.ptr = (u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
}bool $170_done( Iter$7 self) {
  return ((self.ptr >= self.end) || (self.ptr == self.end));
;
}u8* $170_current( Iter$7 self) {
  return self.ptr;
;
}typedef struct Iter$7 Iter$7;
void $170_next( Iter$7* self) {
  self->ptr++;
}typedef struct  Slice$5{
  u32* data;
  u64 length;
} Slice$5;
typedef struct String String;
s32 strncmp(u8* s, u8* str2, s32 len) {
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = len};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((s[i] != str2[i])) {
          return (s[i] - str2[i]);
;
        };
        if ((s[i] == '\0')) {
          return 0;
;
        };
      }    }
  }
;
  return 0;
;
}extern void* realloc(void*, u64);void* memcpy(void* dest, void* src, u64 n) {
  u8* d  = dest;
;
  u8* s  = src;
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = n};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        (d[i] = s[i]);
      }    }
  }
;
  return dest;
;
}extern void* malloc(u64);void $14_resize( String* self, u64 new_size) {
  if ((new_size < self->length)) {
    (self->length = new_size);
  };
  (self->capacity = new_size);
  (self->data =(u8*) realloc(self->data, (new_size * sizeof(u8))));
}extern s32 memmove(void*, void*, s64);extern void free(void*);u8 $14_subscript( String* self, s64 idx) {
  return self->data[idx];
;
}typedef struct Field Field;
typedef struct  List$25{
  Field* data;
  u64 length;
  u64 capacity;
} List$25;
typedef struct List$24 List$24;
typedef struct  List$30{
  Type** data;
  u64 length;
  u64 capacity;
} List$30;
typedef struct  Type{
  s32 id;
  u8* name;
  u64 size;
  u64 flags;
  List$25 fields;
  List$24(*elements)(u8*);
  Type* element_type;
} Type;
typedef enum {
TypeFlags_INTEGER = 1,
TypeFlags_FLOAT = 2,
TypeFlags_BOOL = 4,
TypeFlags_STRING = 8,
TypeFlags_STRUCT = 16,
TypeFlags_TAGGED_UNION = 32,
TypeFlags_ENUM = 64,
TypeFlags_TUPLE = 128,
TypeFlags_ARRAY = 256,
TypeFlags_FUNCTION = 512,
TypeFlags_POINTER = 1024,
TypeFlags_SIGNED = 2048,
TypeFlags_UNSIGNED = 4096} TypeFlags;
bool $17_has_no_extension( Type* self) {
  return (((self->flags & TypeFlags_POINTER) == 0) && ((self->flags & TypeFlags_ARRAY) == 0));
;
}typedef struct  Iter$25{
  Field* ptr;
  Field* end;
} Iter$25;
typedef struct  Field{
  u8* name;
  Type* type;
  u64 size;
  u64 offset;
  s64 enum_value;
} Field;
typedef struct List$25 List$25;
Iter$25 $213_iter( List$25* self) {
  return (Iter$25) {.ptr = (Field*)self->data,
.end = (Field*)(Field*)(self->data + self->length)};
;
}bool $227_done( Iter$25 self) {
  return ((self.ptr >= self.end) || (self.ptr == self.end));
;
}Field* $227_current( Iter$25 self) {
  return self.ptr;
;
}typedef struct Iter$25 Iter$25;
void $227_next( Iter$25* self) {
  self->ptr++;
}s32 strcmp(u8* str1, u8* str2) {
  while (((*str1) && ((*str1) == (*str2))))  {
    str1++;
    str2++;
  };
  return ((*str1) - (*str2));
;
}extern s32 printf(u8*, ...);extern void exit(s32);void(*panic_handler)(str)= (void(*)(str)) {0};
void $lambda$0 (str msg) {
  printf("panic(): %s\n", msg.data);
  exit(1);
}
typedef struct  List$13{
  str* data;
  u64 length;
  u64 capacity;
} List$13;
typedef struct Env Env;
typedef struct  Env{
  List$13 m_args;
} Env;
void panic(str msg) {
  if ((!panic_handler)) {
    (panic_handler =(void(*)(str)) $lambda$0);;
  };
  panic_handler(msg);
}List$13 $310_clone( List$13 self) {
  void* new_data  = malloc((sizeof(str) * self.capacity));
;
  if ((!new_data)) {
    panic((str) { .data = "Failed to allocate in List clone", .length = 32 });
  };
  memcpy(new_data, self.data, (sizeof(str) * self.length));
  return (List$13) {.data = (str*)new_data,
.length = (u64)self.length,
.capacity = (u64)self.capacity};
;
}static Env* $26_current() {
  static Env self = (Env) {};
;
  return (&self);
;
}typedef struct List$13 List$13;
void $310_resize( List$13* self, u64 new_capacity) {
  if (((new_capacity < self->capacity) && (new_capacity < self->length))) {
    (self->length = new_capacity);
  };
  (self->capacity = new_capacity);
  (self->data =(str*) realloc(self->data, (sizeof(str) * self->capacity)));
  if ((!self->data)) {
    panic((str) { .data = "Failed to allocate in List![{#type(T).name}]", .length = 44 });
  };
}void $310_push( List$13* self, str v) {
  if (((self->length + 1) >= self->capacity)) {
    if ((self->capacity == 0)) {
      (self->capacity = 4);
    };
    $310_resize(self, (self->capacity * 2));
  };
  (self->data[self->length] = v);
  self->length++;
}void assert(str message, bool condition) {
  if ((!condition)) {
    printf("assertion failed: %s\n", message.data);
    exit(1);
  };
}typedef struct  List$1{
  s32* data;
  u64 length;
  u64 capacity;
} List$1;
typedef struct List$1 List$1;
void $346_resize( List$1* self, u64 new_capacity) {
  if (((new_capacity < self->capacity) && (new_capacity < self->length))) {
    (self->length = new_capacity);
  };
  (self->capacity = new_capacity);
  (self->data =(s32*) realloc(self->data, (sizeof(s32) * self->capacity)));
  if ((!self->data)) {
    panic((str) { .data = "Failed to allocate in List![{#type(T).name}]", .length = 44 });
  };
}void $346_push( List$1* self, s32 v) {
  if (((self->length + 1) >= self->capacity)) {
    if ((self->capacity == 0)) {
      (self->capacity = 4);
    };
    $346_resize(self, (self->capacity * 2));
  };
  (self->data[self->length] = v);
  self->length++;
}s32 $346_pop( List$1* self) {
  if ((self->length == 0)) {
    panic((str) { .data = "Attempted to pop from an empty List![{#type(T).name}]", .length = 53 });
  };
  s32 value  = self->data[(self->length - 1)];
;
  (self->length -= 1);
  return value;
;
}typedef struct  Init_List$1{
  s32* data;
  u64 length;
} Init_List$1;
static List$1 $346_init(Init_List$1 init) {
  List$1 self = (List$1) {};
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = init.length};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 idx = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        $346_push(&self, init.data[idx]);
      }    }
  }
;
  return self;
;
}List$1 $346_subarray( List$1 self, Range_Base$1 range) {
  if ((((range.begin < 0) || (range.end > self.length)) || (range.begin > range.end))) {
    panic((str) { .data = "Invalid range for subarray", .length = 26 });
  };
  s32 len  = (range.end - range.begin);
;
  void* new_data  = malloc((sizeof(s32) * len));
;
  if ((!new_data)) {
    panic((str) { .data = "Failed to allocate in subarray", .length = 30 });
  };
  memcpy(new_data, (self.data + range.begin), (sizeof(s32) * len));
  return (List$1) {.data = (s32*)new_data,
.length = (u64)len,
.capacity = (u64)len};
;
}s32 $346_subscript( List$1* self, s64 idx) {
  return self->data[idx];
;
}#define USE_STD_LIB 1

  
  typedef unsigned long long int u64;
  typedef signed long long int s64;
  
  typedef signed int s32;
  typedef unsigned int u32;
  
  typedef double f64;
  typedef float f32;
  
  typedef short int s16;
  typedef unsigned short int u16;
  
  typedef signed char s8;
  typedef unsigned char u8;
  #include <stddef.h>
  
  #if USE_STD_LIB
    #include <stdint.h>
    #include <errno.h>
    #undef RAND_MAX
  #endif
  
  #ifdef TESTING
    int printf(u8 *, ...);
    void exit(s32);
    
    typedef struct {
      const char *name;
      void (*function)();
    } $ela_test;
    static void $ela_test_run($ela_test *test) {
      #if TEST_VERBOSE 
        printf("running %s\n", test->name);
      #endif
      test->function();
    }
  #endif
  
typedef struct Type Type;
typedef struct List$30 List$30;
extern List$30 _type_info;
#define TESTING
;
;
;
;
;
typedef struct  Type Type;
;
typedef struct  any{
  void* ptr;
  Type* type;
} any;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
extern s32 system(u8*);;;;extern void* calloc(u64, u64);;;;;extern s32 scanf(u8*, ...);;extern s32 getchar();;extern void sleep(s32);;extern void usleep(s32);;extern s32 snprintf(u8*, u64, u8*, ...);;extern s32 sprintf(u8*, u8*, ...);;;extern u8* strndup(u8*, u64);;extern u8* strerror(s32);;extern u8* strtok(u8*, u8*);;extern u8* strcat(u8*, u8*);;extern u8* strncat(u8*, u8*, u64);;extern s64 strtol(u8*, u8***, s32);;extern u64 strtoul(u8*, u8***, s32);;extern f64 strtod(u8*, u8***);;extern s32 atoi(u8*);;extern f64 atof(u8*);;;;
;
void* memset(void* dest, s32 c, u64 n) {
  u8* d  = (u8*)dest;
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = n};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        (d[i] = (u8)c);
      }    }
  }
;
  return dest;
;
};
;
;
;
s32 isalnum(s32 c) {
  return (s32)((((c >= '0') && (c <= '9')) || ((c >= 'A') && (c <= 'Z'))) || ((c >= 'a') && (c <= 'z')));
;
};
s32 isalpha(s32 c) {
  return (s32)(((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')));
;
};
s32 isspace(s32 c) {
  return (s32)((((((c == ' ') || (c == '\t')) || (c == '\n')) || (c == '\v')) || (c == '\f')) || (c == '\r'));
;
};
s32 isdigit(s32 c) {
  return (s32)((c >= '0') && (c <= '9'));
;
};
s32 islower(s32 c) {
  return (s32)((c >= 'a') && (c <= 'z'));
;
};
s32 isupper(s32 c) {
  return (s32)((c >= 'A') && (c <= 'Z'));
;
};
s32 isprint(s32 c) {
  return (s32)((c >= 32) && (c < 127));
;
};
s32 ispunct(s32 c) {
  return (s32)(((((c >= '!') && (c <= '/')) || ((c >= ':') && (c <= '@'))) || ((c >= '[') && (c <= '`'))) || ((c >= '{') && (c <= '~')));
;
};
;;;static Std_Allocator $23_get() {
  static Std_Allocator self = (Std_Allocator) {};
;
  return self;
;
};;
void panic (str msg);
;
;;;;
;
;
;
;
;
;
String $13_as_string( str self) {
  return (String) {.data = (u8*)strdup(self.data),
.length = (u64)self.length};
;
};;
static str $13_empty() {
  return (str) {0};
;
}bool $13_starts_with( str* self, u8* prefix) {
  u32 prefix_len  = strlen(prefix);
;
  if ((self->length < prefix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((self->data[i] != prefix[i])) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}bool $13_ends_with( str* self, u8* suffix) {
  u32 suffix_len  = strlen(suffix);
;
  if ((self->length < suffix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = suffix_len};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((self->data[((self->length - suffix_len) + i)] != suffix[i])) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}Slice$7 $13_slice( str* self, Range_Base$1 range) {
auto $temp_tuple$0 = ($tuple1$1) {.$0 = range.begin, .$1 = range.end};
auto start = $temp_tuple$0.$0;
auto end = $temp_tuple$0.$1;
;
  s32 length  = (end - start);
;
  if ((((start < 0) || (length < 0)) || ((start + length) > self->length))) {
    return (Slice$7) {0};
;
  };
  return (Slice$7) {.data = (u8*)(self->data + start),
.length = (u64)length};
;
};
;
bool $13_neq( str self, str other) {
  return (!$13_eq(self, other));
;
};
u64 $13_hash( str self) {
  u64 hash  = 0xCBF29CE484222325;
;
  {
    str $_range_id0 = self;
    Iter$7 $_loop_id0 = $13_iter(&$_range_id0);
    while (!$170_done($_loop_id0)) {
      u8 byte = *$170_current($_loop_id0);
      $170_next(&$_loop_id0);
 {
        (hash ^= byte);
        (hash *= 0x100000001B3);
      }    }
  }
;
  return hash;
;
};
Slice$5 $13_as_char_slice( str self) {
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)self.length};
;
};
Slice$7 $13_as_byte_slice( str self) {
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};
str $14_replace( String* self, u8* old, u8* replacement) {
  u32 old_len  = strlen(old);
;
  u32 new_len  = strlen(replacement);
;
  str result  = (str) {0};
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self->length};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((strncmp((self->data + i), old, old_len) == 0)) {
          (result.data =(u8*) realloc(result.data, ((result.length + new_len) + 1)));
          memcpy((result.data + result.length), replacement, new_len);
          (result.length += new_len);
          (i += (old_len - 1));
        } else  {
          (result.data =(u8*) realloc(result.data, ((result.length + 1) + 1)));
          (result.data[result.length] = self->data[i]);
          result.length++;
        };
      }    }
  }
;
  (result.data[result.length] = 0);
  return result;
;
}static String $14_from(u8* data) {
  if ((!data)) {
    return (String) {0};
;
  };
  u32 len  = strlen(data);
;
  return (String) {.data = (u8*)strdup(data),
.capacity = (u64)len,
.length = (u64)len};
;
}static String $14_from_ptr(u8* begin, u8* end) {
  String self = (String) {};
;
  (self.length = (end - begin));
  (self.capacity = self.length);
  (self.data =(u8*) malloc(((sizeof(u8) * self.length) + 1)));
  memcpy(self.data, begin, self.length);
  (self.data[self.length] = 0);
  return self;
;
};void $14_push( String* self, u8 ch) {
  if ((self->capacity == 0)) {
    (self->capacity = (256 / 3));
  };
  if (((self->length + 1) >= self->capacity)) {
    $14_resize(self, (self->capacity * 3));
  };
  (self->data[self->length] = ch);
  self->length++;
}u8 $14_pop( String* self) {
  if ((self->length == 0)) {
    return (u8)'\0';
;
  };
  u8 ch  = self->data[self->length--];
;
  return ch;
;
}u8 $14_pop_front( String* self) {
  if ((self->length == 0)) {
    return (u8)'\0';
;
  };
  u8 ch  = self->data[0];
;
  memmove(self->data, (self->data + 1), (self->length - 1));
  self->length--;
  return ch;
;
}void $14_push_front( String* self, u8 ch) {
  if ((self->capacity == 0)) {
    (self->capacity = (256 / 3));
  };
  if (((self->length + 1) >= self->capacity)) {
    $14_resize(self, (self->capacity * 3));
  };
  memmove((self->data + 1), self->data, self->length);
  (self->data[0] = ch);
  self->length++;
};void $14_deinit( String* self) {
  free(self->data);
  (self->length = 0);
};;
str $14_as_str( String self) {
  return (str) {.data = (u8*)self.data,
.length = (u64)self.length};
;
}static String $14_empty() {
  return (String) {0};
;
}bool $14_eq( String* self, String other) {
  if ((self->length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self->length};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 idx = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if (($14_subscript(&(*self), idx) != $14_subscript(&other, idx))) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}bool $14_starts_with( String* self, u8* prefix) {
  u32 prefix_len  = strlen(prefix);
;
  if ((self->length < prefix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((self->data[i] != prefix[i])) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}bool $14_ends_with( String* self, u8* suffix) {
  u32 suffix_len  = strlen(suffix);
;
  if ((self->length < suffix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = suffix_len};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        if ((self->data[((self->length - suffix_len) + i)] != suffix[i])) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}u8 $14_front( String* self) {
  if ((self->length == 0)) {
    return (u8)'\0';
;
  };
  return self->data[0];
;
}u8 $14_back( String* self) {
  if ((self->length == 0)) {
    return (u8)'\0';
;
  };
  return self->data[(self->length - 1)];
;
};
Iter$7 $14_iter( String* self) {
  return (Iter$7) {.ptr = (u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
};
Slice$5 $14_as_char_slice( String self) {
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)self.length};
;
};
Slice$7 $14_as_byte_slice( String self) {
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};
typedef struct  Element Element;
;
typedef struct  Type Type;
;
;;;typedef struct  Element{
  u8* data;
  Type* type;
} Element;
;List$30 _type_info = (List$30) {};
;;
;
bool $17_is_signed( Type* self) {
  return ((self->flags & TypeFlags_SIGNED) != 0);
;
}bool $17_is_unsigned( Type* self) {
  return ((self->flags & TypeFlags_UNSIGNED) != 0);
;
}bool $17_is_array( Type* self) {
  return ((self->flags & TypeFlags_ARRAY) != 0);
;
}bool $17_is_string( Type* self) {
  return ((self->flags & TypeFlags_STRING) != 0);
;
}bool $17_is_scalar( Type* self) {
  return ($17_has_no_extension(self) && (((((self->flags & TypeFlags_INTEGER) != 0) || ((self->flags & TypeFlags_FLOAT) != 0)) || ((self->flags & TypeFlags_BOOL) != 0)) || ((self->flags & TypeFlags_STRING) != 0)));
;
}bool $17_is_struct( Type* self) {
  return ((self->flags & TypeFlags_STRUCT) != 0);
;
}bool $17_is_enum( Type* self) {
  return ((self->flags & TypeFlags_ENUM) != 0);
;
}bool $17_is_integral( Type* self) {
  return ((self->flags & TypeFlags_INTEGER) != 0);
;
}bool $17_is_float( Type* self) {
  return ((self->flags & TypeFlags_FLOAT) != 0);
;
}bool $17_is_bool( Type* self) {
  return ((self->flags & TypeFlags_BOOL) != 0);
;
}bool $17_is_tuple( Type* self) {
  return ((self->flags & TypeFlags_TUPLE) != 0);
;
}bool $17_is_function( Type* self) {
  return ((self->flags & TypeFlags_FUNCTION) != 0);
;
}bool $17_is_pointer( Type* self) {
  return ((self->flags & TypeFlags_POINTER) != 0);
;
}Field* $17_get_field( Type* self, u8* name) {
  {
    List$25 $_range_id0 = self->fields;
    Iter$25 $_loop_id0 = $213_iter(&$_range_id0);
    while (!$227_done($_loop_id0)) {
      Field* field = $227_current($_loop_id0);
      $227_next(&$_loop_id0);
 {
        if ((strcmp(field->name, name) == 0)) {
          return field;
;
        };
      }    }
  }
;
  return NULL;
;
};
;
;
;
;
;
;
void set_panic_handler(void(*handler)(str)) {
  (panic_handler =(void(*)(str)) handler);
};
;
;static List$13 $26_args() {
  return $310_clone($26_current()->m_args);
;
}static void $26_initialize(s32 argc, str* argv) {
  Env* self  = $26_current();
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = argc};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        $310_push(&self->m_args, argv[i]);
      }    }
  }
;
};;
Slice$5 $5_as_char_slice( u32 self) {
  static u32 chars[2] =  {'\0', '\0'};
  (chars[0] = self);
  return (Slice$5) {.data = (u32*)chars,
.length = (u64)1};
;
};
void test_string() {
  str my_string  = (str) { .data = "Xaryu Baryu", .length = 11 };
;
  assert((str) { .data = "str length failed to equal expected value", .length = 41 }, (my_string.length == 11));
};
void test_array() {
  s32 array[11] =  {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = 11};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        assert((str) { .data = "array element failed to init", .length = 28 }, (i == array[i]));
      }    }
  }
;
};
typedef struct  Xaryu{
  s32 value;
} Xaryu;
;
void test_array_concat_operator() {
  List$1 array = (List$1) {};
;
  $346_push(&array, 10);
  assert((str) { .data = "oncat operator failed", .length = 21 }, (array.length == 1));
};
void test_array_pop_operator() {
  List$1 array = (List$1) {};
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = 100};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        $346_push(&array, 10);
      }    }
  }
;
  s32 n = (s32) {0};
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = 100};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        (n = $346_pop(&array));
      }    }
  }
;
  assert((str) { .data = "pop operator failed", .length = 19 }, ((array.length == 0) && (n == 10)));
};
void test_range_var() {
  Range_Base$1 r  = (Range_Base$1) {.begin = 0, .end = 10};
;
  assert((str) { .data = "range variable failed", .length = 21 }, ((r.begin == 0) && (r.end == 10)));
  s32 i = (s32) {0};
;
  {
    Range_Base$1 $_range_id0 = r;
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 z = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        assert((str) { .data = "range iteration failed", .length = 22 }, (i == z));
        i++;
      }    }
  }
;
};
void test_slicing_array() {
  List$1 arr  = $346_init((Init_List$1) { .data = (s32[]) {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, .length = 11});
;
  assert((str) { .data = "Length", .length = 6 }, (arr.length == 11));
  List$1 subarr  = $346_subarray(arr, (Range_Base$1) {.begin = 0, .end = 10});
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = 10};
    Range_Enumerator$1 $_loop_id0 = $45_enumerator(&$_range_id0);
    while (!$77_done($_loop_id0)) {
      s32 i = $77_current($_loop_id0);
      $77_next(&$_loop_id0);
 {
        assert((str) { .data = "Slice failed", .length = 12 }, ($346_subscript(&subarr, i) == $346_subscript(&arr, i)));
      }    }
  }
;
  assert((str) { .data = "Slice failed", .length = 12 }, (subarr.length == 10));
};

  #ifdef TESTING
  #define __TEST_RUNNER_MAIN                                                                                             \
    int main() {                                                                                                         \
      for (int i = 0; i < sizeof(tests) / sizeof($ela_test); i++) {                                      \
        $ela_test_run(&tests[i]);                                                                        \
      }                                                                                                                  \
    }                                                                                                                     
  #endif
  
$ela_test tests[6] = { ($ela_test){.name = "test_string", .function = &test_string},($ela_test){.name = "test_array", .function = &test_array},($ela_test){.name = "test_array_concat_operator", .function = &test_array_concat_operator},($ela_test){.name = "test_array_pop_operator", .function = &test_array_pop_operator},($ela_test){.name = "test_range_var", .function = &test_range_var},($ela_test){.name = "test_slicing_array", .function = &test_slicing_array} };
__TEST_RUNNER_MAIN;