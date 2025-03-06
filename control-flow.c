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
    #if TEST_VERBOSE
      int printf(u8 *, ...);
    #endif
    
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
typedef struct  List$31{
  Type** data;
  u64 length;
  u64 capacity;
} List$31;
typedef struct Type Type;
extern List$31 _type_info;
typedef struct  str{
  u8* data;
  u64 length;
} str;
typedef struct  Slice$7{
  u8* data;
  u64 length;
} Slice$7;
typedef struct  String{
  u8* data;
  u64 length;
  u64 capacity;
} String;
extern u8*  strdup(u8*); typedef struct str str;
extern u32  strlen(u8*); typedef struct  Range_Iter$1{
  s32 begin;
  s32 end;
  s32 idx;
} Range_Iter$1;
typedef struct  Range_Base$1{
  s32 begin;
  s32 end;
} Range_Base$1;
typedef struct Range_Base$1 Range_Base$1;
Range_Iter$1 $53_iter( Range_Base$1* self) {
  return (Range_Iter$1) {.begin = (s32)self->begin,
.end = (s32)self->end,
.idx = (s32)0};
;
}typedef struct  Option$1{
  union {
    s32 s;
};
;
  bool has_value;
} Option$1;
typedef struct Range_Iter$1 Range_Iter$1;
static Option$1 $96_Some(s32 v) {
  return (Option$1) {.s = (s32)v,
.has_value = (bool)true};
;
}static Option$1 $96_None() {
  return (Option$1) {.has_value = (bool)false};
;
}Option$1 $94_next( Range_Iter$1* self) {
  Option$1 value  = $96_Some(self->idx);
;
  if ((self->idx >= self->end)) {
    return $96_None();
;
  };
  self->idx++;
  return value;
;
}typedef struct $tuple1$1 {s32 $0;
s32 $1;
} $tuple1$1;
typedef struct  Basic_Iter$7{
  u8* ptr;
  u8* end;
} Basic_Iter$7;
u8* $13_subscript( str* self, s64 idx) {
  return (&self->data[idx]);
;
}bool $13_eq( str self, str other) {
  if ((self.length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 idx = $next0.s;      $94_next(&$_loop_id0);
 {
        if ((*$13_subscript(&self, idx) != *$13_subscript(&other, idx))) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}Basic_Iter$7 $13_iter( str* self) {
  return (Basic_Iter$7) {.ptr = (u8*)(u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
}typedef struct  Option$35{
  union {
    u8* s;
};
;
  bool has_value;
} Option$35;
typedef struct Basic_Iter$7 Basic_Iter$7;
static Option$35 $257_Some(u8* v) {
  return (Option$35) {.s = (u8*)v,
.has_value = (bool)true};
;
}static Option$35 $257_None() {
  return (Option$35) {.has_value = (bool)false};
;
}Option$35 $255_next( Basic_Iter$7* self) {
  Option$35 value  = $257_Some(self->ptr);
;
  if ((self->ptr >= self->end)) {
    return $257_None();
;
  };
  self->ptr++;
  return value;
;
}typedef struct  Slice$5{
  u32* data;
  u64 length;
} Slice$5;
typedef struct String String;
extern s32  strncmp(u8*, u8*, s32); extern void*  realloc(void*, u64); extern void*  memcpy(void*, void*, u64); void $14_resize( String* self, u64 new_size) {
  if ((new_size < self->length)) {
    (self->length = new_size);
  };
  (self->capacity = new_size);
  (self->data =(u8*) realloc(self->data, ((new_size * sizeof(u8)) + 1)));
  (self->data[self->length] = 0);
}extern s32  memmove(void*, void*, s64); extern void  free(void*); u8* $14_subscript( String* self, s64 idx) {
  return (&self->data[idx]);
;
}bool $14_eq( String self, String other) {
  if ((self.length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 idx = $next0.s;      $94_next(&$_loop_id0);
 {
        if ((*$14_subscript(&self, idx) != *$14_subscript(&other, idx))) {
          return false;
;
        };
      }    }
  }
;
  return true;
;
}typedef struct Field Field;
typedef struct  List$27{
  Field* data;
  u64 length;
  u64 capacity;
} List$27;
typedef struct List$28 List$28;
typedef struct  Basic_Iter$27{
  Field* ptr;
  Field* end;
} Basic_Iter$27;
typedef struct  Field{
  u8* name;
  Type* type;
  u64 size;
  u64 offset;
  s64 enum_value;
} Field;
typedef struct  Type{
  s32 id;
  u8* name;
  u64 size;
  u64 flags;
  List$27 fields;
  List$28(*elements)(u8*);
  Type* element_type;
} Type;
typedef struct List$27 List$27;
Basic_Iter$27 $316_iter( List$27* self) {
  return (Basic_Iter$27) {.ptr = (Field*)self->data,
.end = (Field*)(Field*)(self->data + self->length)};
;
}typedef struct  Option$317{
  union {
    Field* s;
};
;
  bool has_value;
} Option$317;
typedef struct Basic_Iter$27 Basic_Iter$27;
static Option$317 $335_Some(Field* v) {
  return (Option$317) {.s = (Field*)v,
.has_value = (bool)true};
;
}static Option$317 $335_None() {
  return (Option$317) {.has_value = (bool)false};
;
}Option$317 $333_next( Basic_Iter$27* self) {
  Option$317 value  = $335_Some(self->ptr);
;
  if ((self->ptr >= self->end)) {
    return $335_None();
;
  };
  self->ptr++;
  return value;
;
}extern s32  strcmp(u8*, u8*); typedef enum {
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
bool $12_has_no_extension( Type* self) {
  return (((self->flags & TypeFlags_POINTER) == 0) && ((self->flags & TypeFlags_ARRAY) == 0));
;
}typedef struct any any;
typedef struct  any{
  void* ptr;
  Type* type;
} any;
extern s32  printf(u8*, ...);; extern void  exit(s32); void(*panic_handler)(str)= {0};
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
static Env* $29_current() {
  static Env self = {};
;
  return (&self);
;
}extern void*  malloc(u64); List$13 $418_clone( List$13 self) {
  void* new_data  = malloc((sizeof(str) * self.capacity));
;
  memcpy(new_data, self.data, (sizeof(str) * self.length));
  return (List$13) {.data = (str*)new_data,
.length = (u64)self.length,
.capacity = (u64)self.capacity};
;
}typedef struct List$13 List$13;
void $418_resize( List$13* self, u64 new_capacity) {
  if (((new_capacity < self->capacity) && (new_capacity < self->length))) {
    (self->length = new_capacity);
  };
  (self->capacity = new_capacity);
  (self->data =(str*) realloc(self->data, (sizeof(str) * self->capacity)));
}void $418_push( List$13* self, str v) {
  if (((self->length + 1) >= self->capacity)) {
    if ((self->capacity == 0)) {
      (self->capacity = 4);
    };
    $418_resize(self, (self->capacity * 2));
  };
  (self->data[self->length] = v);
  self->length++;
};;typedef struct  Type Type;
;;;;;;;;;u8 RESULT_IS_ERR  = 0;
;u8 RESULT_IS_OK  = 1;
;;;;
;;;;;;;;;;;;;;;;;;
void panic (str msg);
;;;
;;
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
extern s32  system(u8*); ;;;extern void*  calloc(u64, u64); ;;;;extern s32  scanf(u8*, ...);; ;extern s32  getchar(); ;extern void  sleep(s32); ;extern void  usleep(s32); ;extern s32  snprintf(u8*, u64, u8*, ...);; ;extern s32  sprintf(u8*, u8*, ...);; ;;extern u8*  strndup(u8*, u64); ;extern u8*  strerror(s32); ;extern u8*  strtok(u8*, u8*); ;extern u8*  strcat(u8*, u8*); ;extern u8*  strncat(u8*, u8*, u64); ;extern s64  strtol(u8*, u8***, s32); ;extern u64  strtoul(u8*, u8***, s32); ;extern f64  strtod(u8*, u8***); ;extern s32  atoi(u8*); ;extern f64  atof(u8*); ;;;extern void*  memset(void*, s32, u64); ;;;;;extern s32  isalnum(s32); ;extern s32  isalpha(s32); ;extern s32  isspace(s32); ;extern s32  isdigit(s32); ;extern s32  islower(s32); ;extern s32  isupper(s32); ;extern s32  isprint(s32); ;extern s32  ispunct(s32); ;;
void panic (str msg);
;;;;;;;;;;;;;
;;;;Slice$7 $249_as_byte_slice( Slice$7 self) {
  return self;
;
};;
String $13_as_string( str self) {
  return (String) {.data = (u8*)strdup(self.data),
.length = (u64)self.length};
;
};;bool $13_starts_with( str* self, u8* prefix) {
  u32 prefix_len  = strlen(prefix);
;
  if ((self->length < prefix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
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
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
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
};;bool $13_is_empty( str self) {
  return (self.length == 0);
;
}static str $13_empty() {
  return (str) {0};
;
}Slice$7 $13_slice( str* self, Range_Base$1 range) {
$tuple1$1 $deconstruction$0 = ($tuple1$1) {.$0 = range.begin, .$1 = range.end};auto start = $deconstruction$0.$0;
auto end = $deconstruction$0.$1;
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
};;bool $13_neq( str self, str other) {
  return (!$13_eq(self, other));
;
};u64 $13_hash( str self) {
  u64 hash  = 0xCBF29CE484222325;
;
  {
    str $_range_id0 = self;
    Basic_Iter$7 $_loop_id0 = $13_iter(&$_range_id0);
    while (1) {
auto $next0 = $255_next(&$_loop_id0);
if (!$next0.has_value) break;
      u8 byte = *$next0.s;      $255_next(&$_loop_id0);
 {
        (hash ^= byte);
        (hash *= 0x100000001B3);
      }    }
  }
;
  return hash;
;
};Slice$5 $13_as_char_slice( str self) {
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)self.length};
;
};Slice$7 $13_as_byte_slice( str self) {
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};;
str $14_replace( String* self, u8* old, u8* replacement) {
  u32 old_len  = strlen(old);
;
  u32 new_len  = strlen(replacement);
;
  str result  = (str) {0};
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self->length};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
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
}static String $14_from_ptr(u8* data) {
  u32 length  = strlen(data);
;
  return (String) {.data = (u8*)strdup(data),
.capacity = (u64)length,
.length = (u64)length};
;
};void $14_push( String* self, u8 ch) {
  if ((self->capacity == 0)) {
    (self->capacity = (256 / 3));
  };
  if (((self->length + 1) >= self->capacity)) {
    $14_resize(self, ((self->capacity * 3) + 1));
  };
  (self->data[self->length] = ch);
  self->length++;
  (self->data[self->length] = 0x0);
}u8 $14_pop( String* self) {
  if ((self->length == 0)) {
    return (u8)0x0;
;
  };
  u8 ch  = self->data[self->length--];
;
  (self->data[self->length] = 0x0);
  return ch;
;
}u8 $14_pop_front( String* self) {
  if ((self->length == 0)) {
    return (u8)0x0;
;
  };
  u8 ch  = self->data[0];
;
  memmove(self->data, (self->data + 1), (self->length - 1));
  self->length--;
  (self->data[self->length] = 0x0);
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
  (self->data[self->length] = 0x0);
};void $14_deinit( String* self) {
  free(self->data);
  (self->length = 0);
};bool $14_starts_with( String* self, u8* prefix) {
  u32 prefix_len  = strlen(prefix);
;
  if ((self->length < prefix_len)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
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
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
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
};;str $14_as_str( String self) {
  return (str) {.data = (u8*)self.data,
.length = (u64)self.length};
;
}bool $14_is_empty( String self) {
  return (self.length == 0);
;
}static String $14_empty() {
  return (String) {0};
;
}u8 $14_front( String* self) {
  if ((self->length == 0)) {
    return (u8)0x0;
;
  };
  return self->data[0];
;
}u8 $14_back( String* self) {
  if ((self->length == 0)) {
    return (u8)0x0;
;
  };
  return self->data[(self->length - 1)];
;
};bool $14_neq( String self, String other) {
  return (!$14_eq(self, other));
;
};Basic_Iter$7 $14_iter( String* self) {
  return (Basic_Iter$7) {.ptr = (u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
};Slice$5 $14_as_char_slice( String self) {
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)(self.length / 4)};
;
};Slice$7 $14_as_byte_slice( String self) {
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};;
;;typedef struct  Element{
  u8* data;
  Type* type;
} Element;
;;List$31 _type_info = {};
;Field* $12_get_field( Type* self, u8* name) {
  {
    List$27 $_range_id0 = self->fields;
    Basic_Iter$27 $_loop_id0 = $316_iter(&$_range_id0);
    while (1) {
auto $next0 = $333_next(&$_loop_id0);
if (!$next0.has_value) break;
      Field* field = $next0.s;      $333_next(&$_loop_id0);
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
}u64 $12_offset( Type* self, str member) {
  {
    List$27 $_range_id0 = self->fields;
    Basic_Iter$27 $_loop_id0 = $316_iter(&$_range_id0);
    while (1) {
auto $next0 = $333_next(&$_loop_id0);
if (!$next0.has_value) break;
      Field field = *$next0.s;      $333_next(&$_loop_id0);
 {
        if ((strncmp(field.name, member.data, (s32)member.length) == 0)) {
          return field.offset;
;
        };
      }    }
  }
;
  return (u64)(-1);
;
};;;bool $12_is_signed( Type* self) {
  return ((self->flags & TypeFlags_SIGNED) != 0);
;
}bool $12_is_unsigned( Type* self) {
  return ((self->flags & TypeFlags_UNSIGNED) != 0);
;
}bool $12_is_array( Type* self) {
  return ((self->flags & TypeFlags_ARRAY) != 0);
;
}bool $12_is_string( Type* self) {
  return ((self->flags & TypeFlags_STRING) != 0);
;
}bool $12_is_scalar( Type* self) {
  return ($12_has_no_extension(self) && (((((self->flags & TypeFlags_INTEGER) != 0) || ((self->flags & TypeFlags_FLOAT) != 0)) || ((self->flags & TypeFlags_BOOL) != 0)) || ((self->flags & TypeFlags_STRING) != 0)));
;
}bool $12_is_struct( Type* self) {
  return ((self->flags & TypeFlags_STRUCT) != 0);
;
}bool $12_is_enum( Type* self) {
  return ((self->flags & TypeFlags_ENUM) != 0);
;
}bool $12_is_integral( Type* self) {
  return ((self->flags & TypeFlags_INTEGER) != 0);
;
}bool $12_is_float( Type* self) {
  return ((self->flags & TypeFlags_FLOAT) != 0);
;
}bool $12_is_bool( Type* self) {
  return ((self->flags & TypeFlags_BOOL) != 0);
;
}bool $12_is_tuple( Type* self) {
  return ((self->flags & TypeFlags_TUPLE) != 0);
;
}bool $12_is_function( Type* self) {
  return ((self->flags & TypeFlags_FUNCTION) != 0);
;
}bool $12_is_pointer( Type* self) {
  return ((self->flags & TypeFlags_POINTER) != 0);
;
};;;void $17_deinit( any* self) {
  free(self->ptr);
};;;
void assert(str message, bool condition) {
  if ((!condition)) {
    printf("assertion failed: %s\n", message.data);
    exit(1);
  };
};;;;
;
;
void set_panic_handler(void(*handler)(str)) {
  (panic_handler =(void(*)(str)) handler);
};
void panic(str msg) {
  if ((!panic_handler)) {
    (panic_handler =(void(*)(str)) $lambda$0);;
  };
  panic_handler(msg);
};
;static List$13 $29_args() {
  return $418_clone($29_current()->m_args);
;
}static void $29_initialize(s32 argc, u8** argv) {
  Env* self  = $29_current();
;
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = argc};
    Range_Iter$1 $_loop_id0 = $53_iter(&$_range_id0);
    while (1) {
auto $next0 = $94_next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;      $94_next(&$_loop_id0);
 {
        $418_push(&self->m_args, (str) {.data = (u8*)argv[i],
.length = (u64)strlen(argv[i])});
      }    }
  }
;
};;
Slice$5 $5_as_char_slice( u32 self) {
  static u32 chars[2] =  {0x0, 0x0};
  (chars[0] = self);
  return (Slice$5) {.data = (u32*)chars,
.length = (u64)1};
;
};
Slice$7 $5_as_byte_slice( u32 self) {
  static u8 chars[2] =  {0x0, 0x0};
  (chars[0] = self);
  return (Slice$7) {.data = (u8*)chars,
.length = (u64)1};
;
};
;;
;
;
;
;
;
;
;
;
;
typedef struct  __Struct{
  s32 x;
  s32 y;
} __Struct;
;
;
typedef enum {
SwitchCaseEnum_SwitchVariant0 = 0,
SwitchCaseEnum_SwitchVariant1 = 1} SwitchCaseEnum;
;
s32 switch_helper() {
  s32 number  = 5;
;
  auto $switch_target$0 = number;
 if ($switch_target$0 == 0)  {
    return 0;
;
  } else  if ($switch_target$0 == 5)  {
    return 5;
;
  };
  return 1;
;
};
;
;
;
;
