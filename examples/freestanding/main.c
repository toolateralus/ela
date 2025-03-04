
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
typedef struct  str{
  u8* data;
  u64 length;
} str;
typedef struct  Slice$7{
  u8* data;
  u64 length;
} Slice$7;
typedef struct str str;
typedef struct  Range_Base$1{
  s32 begin;
  s32 end;
} Range_Base$1;
typedef struct $tuple1$1 {s32 $0;
s32 $1;
} $tuple1$1;
typedef struct  Iter$7{
  u8* ptr;
  u8* end;
} Iter$7;
typedef struct  Range_Enumerator$1{
  s32 begin;
  s32 end;
  s32 idx;
} Range_Enumerator$1;
typedef struct Range_Base$1 Range_Base$1;
Range_Enumerator$1 $51_enumerator( Range_Base$1* self) {
  return (Range_Enumerator$1) {.begin = (s32)self->begin,
.end = (s32)self->end,
.idx = (s32)0};
;
}bool $83_done( Range_Enumerator$1 self) {
  return (self.idx >= self.end);
;
}s32 $83_current( Range_Enumerator$1 self) {
  return self.idx;
;
}typedef struct Range_Enumerator$1 Range_Enumerator$1;
void $83_next( Range_Enumerator$1* self) {
  self->idx++;
}u8* $13_subscript( str* self, s64 idx) {
  return (&self->data[idx]);
;
}bool $13_eq( str self, str other) {
  if ((self.length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    Range_Enumerator$1 $_loop_id0 = $51_enumerator(&$_range_id0);
    while (!$83_done($_loop_id0)) {
      s32 idx = $83_current($_loop_id0);
      $83_next(&$_loop_id0);
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
}Iter$7 $13_iter( str* self) {
  return (Iter$7) {.ptr = (u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
}bool $153_done( Iter$7 self) {
  return ((self.ptr >= self.end) || (self.ptr == self.end));
;
}u8* $153_current( Iter$7 self) {
  return self.ptr;
;
}typedef struct Iter$7 Iter$7;
void $153_next( Iter$7* self) {
  self->ptr++;
}typedef struct  Slice$5{
  u32* data;
  u64 length;
} Slice$5;
typedef struct  String{
  u8* data;
  u64 length;
  u64 capacity;
} String;
typedef struct String String;
u8* $14_subscript( String* self, s64 idx) {
  return (&self->data[idx]);
;
}bool $14_eq( String self, String other) {
  if ((self.length != other.length)) {
    return false;
;
  };
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    Range_Enumerator$1 $_loop_id0 = $51_enumerator(&$_range_id0);
    while (!$83_done($_loop_id0)) {
      s32 idx = $83_current($_loop_id0);
      $83_next(&$_loop_id0);
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
typedef struct  List$28{
  Field* data;
  u64 length;
  u64 capacity;
} List$28;
typedef struct List$29 List$29;
typedef struct  Type{
  s32 id;
  u8* name;
  u64 size;
  u64 flags;
  List$28 fields;
  List$29(*elements)(u8*);
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
}void(*panic_handler)(str)= (void(*)(str)) {0};
void $lambda$0 (str msg) {
  s32* n  = NULL;
;
  ((*n) = 10);
}
typedef struct Env Env;
typedef struct  Env{
  str* m_args;
  u64 length;
} Env;
typedef struct $tuple162$4 {str* $0;
u64 $1;
} $tuple162$4;
static Env* $30_current() {
  static Env self = (Env) {};
;
  return (&self);
;
}typedef struct  LinearAlloc{
  u8 mem[24576];
  u64 ptr;
} LinearAlloc;
static constexpr s32 LINEAR_ALLOCATOR_SIZE  = (1024 * 24);
void memset(u8* p, u64 value, u64 len) {
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = len};
    Range_Enumerator$1 $_loop_id0 = $51_enumerator(&$_range_id0);
    while (!$83_done($_loop_id0)) {
      s32 i = $83_current($_loop_id0);
      $83_next(&$_loop_id0);
 {
        (p[i] = value);
      }    }
  }
;
}typedef struct LinearAlloc LinearAlloc;
extern void  print(u8*); extern void  exit(s32); static LinearAlloc $32_create() {
  LinearAlloc self = (LinearAlloc) {};
;
  memset((u8*)self.mem, 0, LINEAR_ALLOCATOR_SIZE);
  return self;
;
}typedef struct Vector2 Vector2;
typedef struct  Vector2{
  f32 x;
  f32 y;
} Vector2;
u8* $32_alloc( LinearAlloc* self, u64 size) {
  u8* data  = (&self->mem[self->ptr]);
;
  (self->ptr += size);
  if ((self->ptr > LINEAR_ALLOCATOR_SIZE)) {
    print("\e[031mLinear allocator ran out of memory \e[033m(expected for demo)\n");
    exit(1);
  };
  return data;
;
}void memcpy(u8** dest, u8* src, u64 len) {
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = len};
    Range_Enumerator$1 $_loop_id0 = $51_enumerator(&$_range_id0);
    while (!$83_done($_loop_id0)) {
      s32 i = $83_current($_loop_id0);
      $83_next(&$_loop_id0);
 {
        ((*dest)[i] = src[i]);
      }    }
  }
;
};;;;;typedef struct  Type Type;
;typedef struct  any{
  void* ptr;
  Type* type;
} any;
;;typedef struct  None{
} None;
;u8 RESULT_IS_ERR  = 0;
;u8 RESULT_IS_OK  = 1;
;;;;
;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;extern s32  isalnum(s32); ;extern s32  isalpha(s32); ;extern s32  isspace(s32); ;extern s32  isdigit(s32); ;extern s32  islower(s32); ;extern s32  isupper(s32); ;extern s32  isprint(s32); ;extern s32  ispunct(s32); ;;
void panic (str msg);
;;;;;;
;;;Slice$7 $147_as_byte_slice( Slice$7 self) {
  return self;
;
};;
;;bool $13_is_empty( str self) {
  return (self.length == 0);
;
}static str $13_empty() {
  return (str) {0};
;
}Slice$7 $13_slice( str* self, Range_Base$1 range) {
auto $temp_tuple$1 = ($tuple1$1) {.$0 = range.begin, .$1 = range.end};
auto $temp_tuple$0 = &$temp_tuple$1;
auto start = $temp_tuple$0->$0;
auto end = $temp_tuple$0->$1;
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
    Iter$7 $_loop_id0 = $13_iter(&$_range_id0);
    while (!$153_done($_loop_id0)) {
      u8 byte = *$153_current($_loop_id0);
      $153_next(&$_loop_id0);
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
;str $14_as_str( String self) {
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
};Iter$7 $14_iter( String* self) {
  return (Iter$7) {.ptr = (u8*)self->data,
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
typedef struct  Field{
  u8* name;
  Type* type;
  u64 size;
  u64 offset;
  s64 enum_value;
} Field;
;;typedef struct  Element{
  u8* data;
  Type* type;
} Element;
;;;bool $17_is_signed( Type* self) {
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
};;;;
;;
;;
;
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
;$tuple162$4 $30_args( Env* self) {
  return ($tuple162$4) {.$0 = self->m_args, .$1 = self->length};
;
}static void $30_initialize(s32 argc, str* argv) {
  Env* self  = $30_current();
;
  (self->m_args =(str*) argv);
  (self->length = argc);
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
;
;
;
;
;
;
;
;
;
extern  s32 strlen(u8* s) {
  s32 i = (s32) {0};
;
  while ((s[i] != 0x0))  {
    i++;
  };
  return i;
;
};
;
;
;
;
extern  void main() {
  LinearAlloc allocator  = $32_create();
;
  print("\e[1;4;32mHello (free-standing) World!\e[0m\n");
  while (true)  {
    Vector2* v  = $32_alloc(&allocator, sizeof(Vector2));
;
    Vector2* v1  = $32_alloc(&allocator, sizeof(Vector2));
;
    memcpy((u8**)(&v1), (u8*)v, sizeof(Vector2));
  };
};
