#define USE_STD_LIB 1
#line 0 "boilerplate.hpp"
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

#line 36 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  List$58{
  Type** data;
  u64 length;
  u64 capacity;
} List$58;
typedef struct Type Type;
extern List$58 _type_info;

#line 12 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  str{
  u8* data;
  u64 length;
} str;

#line 54 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Slice$7{
  u8* data;
  u64 length;
} Slice$7;

#line 20 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  String{
  u8* data;
  u64 length;
  u64 capacity;
} String;

#line 25 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strdup(u8*); typedef struct str str;

#line 44 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u32  strlen(u8*); 
#line 27 "/usr/local/lib/ela/bootstrap/range.ela"
typedef struct  RangeIter$1{
  s32 begin;
  s32 end;
  s32 idx;
} RangeIter$1;

#line 4 "/usr/local/lib/ela/bootstrap/range.ela"
typedef struct  Range_Base$1{
  s32 begin;
  s32 end;
} Range_Base$1;
typedef struct Range_Base$1 Range_Base$1;

#line 63 "/usr/local/lib/ela/bootstrap/range.ela"
RangeIter$1 Range_Base$1$iter( Range_Base$1* self)
#line 63 "/usr/local/lib/ela/bootstrap/range.ela"
 {

#line 64 "/usr/local/lib/ela/bootstrap/range.ela"

#line 64 "/usr/local/lib/ela/bootstrap/range.ela"
  return (RangeIter$1) {.begin = (s32)self->begin,
.end = (s32)self->end,
.idx = (s32)0};
;
}
#line 76 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Option$1{
  
#line 78 "/usr/local/lib/ela/bootstrap/typedef.ela"
union {
    s32 s;
};
;
  bool has_value;
} Option$1;
typedef struct RangeIter$1 RangeIter$1;

#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
Option$1 Option$1$Some(s32 v)
#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$1) {.s = (s32)v,
.has_value = (bool)true};
;
}
#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
Option$1 Option$1$None()
#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$1) {.has_value = (bool)false};
;
}
#line 40 "/usr/local/lib/ela/bootstrap/range.ela"
Option$1 RangeIter$1$next( RangeIter$1* self)
#line 40 "/usr/local/lib/ela/bootstrap/range.ela"
 {

#line 41 "/usr/local/lib/ela/bootstrap/range.ela"
  
#line 41 "/usr/local/lib/ela/bootstrap/range.ela"
Option$1 value  = Option$1$Some(self->idx);
;

#line 42 "/usr/local/lib/ela/bootstrap/range.ela"

#line 42 "/usr/local/lib/ela/bootstrap/range.ela"
  if ((self->idx >= self->end))
#line 42 "/usr/local/lib/ela/bootstrap/range.ela"
 {

#line 43 "/usr/local/lib/ela/bootstrap/range.ela"

#line 43 "/usr/local/lib/ela/bootstrap/range.ela"
    return Option$1$None();
;
  };

#line 45 "/usr/local/lib/ela/bootstrap/range.ela"

#line 45 "/usr/local/lib/ela/bootstrap/range.ela"
  self->idx++;

#line 46 "/usr/local/lib/ela/bootstrap/range.ela"

#line 46 "/usr/local/lib/ela/bootstrap/range.ela"
  return value;
;
}typedef struct $tuple1$1 {s32 $0;
s32 $1;
} $tuple1$1;

#line 16 "/usr/local/lib/ela/bootstrap/iter.ela"
typedef struct  Iter$7{
  u8* ptr;
  u8* end;
} Iter$7;

#line 29 "/usr/local/lib/ela/bootstrap/iter.ela"
typedef struct  IterMut$62{
  u8** ptr;
  u8** end;
} IterMut$62;

#line 36 "/usr/local/lib/ela/bootstrap/str.ela"
u8* str$subscript( str* self, u64 idx)
#line 36 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 37 "/usr/local/lib/ela/bootstrap/str.ela"

#line 37 "/usr/local/lib/ela/bootstrap/str.ela"
  return (&self->data[idx]);
;
}
#line 83 "/usr/local/lib/ela/bootstrap/str.ela"
bool str$eq( str self, str other)
#line 83 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 84 "/usr/local/lib/ela/bootstrap/str.ela"

#line 84 "/usr/local/lib/ela/bootstrap/str.ela"
  if ((self.length != other.length))
#line 84 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 84 "/usr/local/lib/ela/bootstrap/str.ela"

#line 84 "/usr/local/lib/ela/bootstrap/str.ela"
    return false;
;
  };

#line 85 "/usr/local/lib/ela/bootstrap/str.ela"

#line 85 "/usr/local/lib/ela/bootstrap/str.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 idx = $next0.s;
#line 85 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 86 "/usr/local/lib/ela/bootstrap/str.ela"

#line 86 "/usr/local/lib/ela/bootstrap/str.ela"
        if (((*str$subscript(&self, idx)) != (*str$subscript(&other, idx))))
#line 86 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 86 "/usr/local/lib/ela/bootstrap/str.ela"

#line 86 "/usr/local/lib/ela/bootstrap/str.ela"
          return false;
;
        };
      }    }
  }
;

#line 88 "/usr/local/lib/ela/bootstrap/str.ela"

#line 88 "/usr/local/lib/ela/bootstrap/str.ela"
  return true;
;
}
#line 68 "/usr/local/lib/ela/bootstrap/str.ela"
Iter$7 str$iter( str* self)
#line 68 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 69 "/usr/local/lib/ela/bootstrap/str.ela"

#line 69 "/usr/local/lib/ela/bootstrap/str.ela"
  return (Iter$7) {.ptr = (u8*)(u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
}
#line 76 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Option$61{
  
#line 78 "/usr/local/lib/ela/bootstrap/typedef.ela"
union {
    u8* s;
};
;
  bool has_value;
} Option$61;
typedef struct Iter$7 Iter$7;

#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
Option$61 Option$61$None()
#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$61) {.has_value = (bool)false};
;
}
#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
Option$61 Option$61$Some(u8* v)
#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$61) {.s = (u8*)v,
.has_value = (bool)true};
;
}
#line 40 "/usr/local/lib/ela/bootstrap/iter.ela"
Option$61 Iter$7$next( Iter$7* self)
#line 40 "/usr/local/lib/ela/bootstrap/iter.ela"
 {

#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"
  if ((self->ptr >= self->end))
#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"
 {

#line 42 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 42 "/usr/local/lib/ela/bootstrap/iter.ela"
    return Option$61$None();
;
  };

#line 44 "/usr/local/lib/ela/bootstrap/iter.ela"
  
#line 44 "/usr/local/lib/ela/bootstrap/iter.ela"
Option$61 value  = Option$61$Some(self->ptr);
;

#line 45 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 45 "/usr/local/lib/ela/bootstrap/iter.ela"
  self->ptr++;

#line 46 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 46 "/usr/local/lib/ela/bootstrap/iter.ela"
  return value;
;
}
#line 54 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Slice$5{
  u32* data;
  u64 length;
} Slice$5;
typedef struct String String;

#line 42 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  strncmp(u8*, u8*, s32); 
#line 11 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void*  realloc(void*, u64); 
#line 40 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void*  memcpy(void*, void*, u64); 
#line 51 "/usr/local/lib/ela/bootstrap/String.ela"
void String$resize( String* self, u64 new_size)
#line 51 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 52 "/usr/local/lib/ela/bootstrap/String.ela"

#line 52 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((new_size < self->length))
#line 52 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 52 "/usr/local/lib/ela/bootstrap/String.ela"

#line 52 "/usr/local/lib/ela/bootstrap/String.ela"
    (self->length = new_size);
  };

#line 53 "/usr/local/lib/ela/bootstrap/String.ela"

#line 53 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->capacity = new_size);

#line 54 "/usr/local/lib/ela/bootstrap/String.ela"

#line 54 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data =(u8*) realloc(self->data, ((new_size * sizeof(u8)) + 1)));

#line 55 "/usr/local/lib/ela/bootstrap/String.ela"

#line 55 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = 0);
}
#line 39 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  memmove(void*, void*, s64); 
#line 8 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void  free(void*); 
#line 125 "/usr/local/lib/ela/bootstrap/String.ela"
u8* String$subscript( String* self, u64 idx)
#line 125 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 126 "/usr/local/lib/ela/bootstrap/String.ela"

#line 126 "/usr/local/lib/ela/bootstrap/String.ela"
  return (&self->data[idx]);
;
}
#line 159 "/usr/local/lib/ela/bootstrap/String.ela"
bool String$eq( String self, String other)
#line 159 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 160 "/usr/local/lib/ela/bootstrap/String.ela"

#line 160 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self.length != other.length))
#line 160 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 160 "/usr/local/lib/ela/bootstrap/String.ela"

#line 160 "/usr/local/lib/ela/bootstrap/String.ela"
    return false;
;
  };

#line 161 "/usr/local/lib/ela/bootstrap/String.ela"

#line 161 "/usr/local/lib/ela/bootstrap/String.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self.length};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 idx = $next0.s;
#line 161 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 162 "/usr/local/lib/ela/bootstrap/String.ela"

#line 162 "/usr/local/lib/ela/bootstrap/String.ela"
        if (((*String$subscript(&self, idx)) != (*String$subscript(&other, idx))))
#line 162 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 162 "/usr/local/lib/ela/bootstrap/String.ela"

#line 162 "/usr/local/lib/ela/bootstrap/String.ela"
          return false;
;
        };
      }    }
  }
;

#line 164 "/usr/local/lib/ela/bootstrap/String.ela"

#line 164 "/usr/local/lib/ela/bootstrap/String.ela"
  return true;
;
}
#line 16 "/usr/local/lib/ela/bootstrap/iter.ela"
typedef struct  Iter$62{
  u8** ptr;
  u8** end;
} Iter$62;
typedef struct Field Field;

#line 36 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  List$34{
  Field* data;
  u64 length;
  u64 capacity;
} List$34;
typedef struct List$35 List$35;

#line 16 "/usr/local/lib/ela/bootstrap/iter.ela"
typedef struct  Iter$34{
  Field* ptr;
  Field* end;
} Iter$34;

#line 1 "/usr/local/lib/ela/bootstrap/reflection.ela"
typedef struct  Field{
  u8* name;
  Type* type;
  u64 size;
  u64 offset;
  s64 enum_value;
} Field;

#line 9 "/usr/local/lib/ela/bootstrap/reflection.ela"
typedef struct  Type{
  s32 id;
  u8* name;
  u64 size;
  u64 flags;
  List$34 fields;
  List$35(*elements)(u8*);
  Type* element_type;
} Type;
typedef struct List$34 List$34;

#line 191 "/usr/local/lib/ela/bootstrap/list.ela"
Iter$34 List$34$iter( List$34* self)
#line 191 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 192 "/usr/local/lib/ela/bootstrap/list.ela"

#line 192 "/usr/local/lib/ela/bootstrap/list.ela"
  return (Iter$34) {.ptr = (Field*)self->data,
.end = (Field*)(Field*)(self->data + self->length)};
;
}
#line 76 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Option$528{
  
#line 78 "/usr/local/lib/ela/bootstrap/typedef.ela"
union {
    Field* s;
};
;
  bool has_value;
} Option$528;
typedef struct Iter$34 Iter$34;

#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
Option$528 Option$528$None()
#line 28 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"

#line 29 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$528) {.has_value = (bool)false};
;
}
#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
Option$528 Option$528$Some(Field* v)
#line 22 "/usr/local/lib/ela/bootstrap/option.ela"
 {

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"

#line 23 "/usr/local/lib/ela/bootstrap/option.ela"
  return (Option$528) {.s = (Field*)v,
.has_value = (bool)true};
;
}
#line 40 "/usr/local/lib/ela/bootstrap/iter.ela"
Option$528 Iter$34$next( Iter$34* self)
#line 40 "/usr/local/lib/ela/bootstrap/iter.ela"
 {

#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"
  if ((self->ptr >= self->end))
#line 41 "/usr/local/lib/ela/bootstrap/iter.ela"
 {

#line 42 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 42 "/usr/local/lib/ela/bootstrap/iter.ela"
    return Option$528$None();
;
  };

#line 44 "/usr/local/lib/ela/bootstrap/iter.ela"
  
#line 44 "/usr/local/lib/ela/bootstrap/iter.ela"
Option$528 value  = Option$528$Some(self->ptr);
;

#line 45 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 45 "/usr/local/lib/ela/bootstrap/iter.ela"
  self->ptr++;

#line 46 "/usr/local/lib/ela/bootstrap/iter.ela"

#line 46 "/usr/local/lib/ela/bootstrap/iter.ela"
  return value;
;
}
#line 43 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  strcmp(u8*, u8*); 
#line 57 "/usr/local/lib/ela/bootstrap/reflection.ela"
typedef enum {
TypeFlags$INTEGER = 1,
TypeFlags$FLOAT = 2,
TypeFlags$BOOL = 4,
TypeFlags$STRING = 8,
TypeFlags$STRUCT = 16,
TypeFlags$TAGGED_UNION = 32,
TypeFlags$ENUM = 64,
TypeFlags$TUPLE = 128,
TypeFlags$ARRAY = 256,
TypeFlags$FUNCTION = 512,
TypeFlags$POINTER = 1024,
TypeFlags$SIGNED = 2048,
TypeFlags$UNSIGNED = 4096} TypeFlags;

#line 82 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$has_no_extension( Type* self)
#line 82 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 83 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 83 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return (((self->flags & TypeFlags$POINTER) == 0) && ((self->flags & TypeFlags$ARRAY) == 0));
;
}typedef struct any any;

#line 46 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  any{
  void* ptr;
  Type* type;
} any;

#line 13 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  printf(u8*, ...);; 
#line 14 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void  exit(s32); 
#line 60 "/usr/local/lib/ela/bootstrap/lib.ela"
void(*panic_handler)(str)= {0};

#line 72 "/usr/local/lib/ela/bootstrap/lib.ela"
void $lambda$0 (str msg)
#line 69 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 70 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 70 "/usr/local/lib/ela/bootstrap/lib.ela"
  printf("panic(): %s\n", msg.data);

#line 71 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 71 "/usr/local/lib/ela/bootstrap/lib.ela"
  exit(1);
}

#line 36 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  List$17{
  str* data;
  u64 length;
  u64 capacity;
} List$17;
typedef struct Env Env;

#line 107 "/usr/local/lib/ela/bootstrap/lib.ela"
typedef struct  Env{
  List$17 m_args;
} Env;

#line 111 "/usr/local/lib/ela/bootstrap/lib.ela"
Env* Env$current()
#line 111 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 112 "/usr/local/lib/ela/bootstrap/lib.ela"
  
#line 112 "/usr/local/lib/ela/bootstrap/lib.ela"
static Env self = {};
;

#line 113 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 113 "/usr/local/lib/ela/bootstrap/lib.ela"
  return (&self);
;
}
#line 9 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void*  malloc(u64); 
#line 115 "/usr/local/lib/ela/bootstrap/list.ela"
List$17 List$17$clone( List$17 self)
#line 115 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 116 "/usr/local/lib/ela/bootstrap/list.ela"
  
#line 116 "/usr/local/lib/ela/bootstrap/list.ela"
void* new_data  = malloc((sizeof(str) * self.capacity));
;

#line 117 "/usr/local/lib/ela/bootstrap/list.ela"

#line 117 "/usr/local/lib/ela/bootstrap/list.ela"
  memcpy(new_data, self.data, (sizeof(str) * self.length));

#line 118 "/usr/local/lib/ela/bootstrap/list.ela"

#line 118 "/usr/local/lib/ela/bootstrap/list.ela"
  return (List$17) {.data = (str*)new_data,
.length = (u64)self.length,
.capacity = (u64)self.capacity};
;
}typedef struct List$17 List$17;

#line 31 "/usr/local/lib/ela/bootstrap/list.ela"
void List$17$resize( List$17* self, u64 new_capacity)
#line 31 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 33 "/usr/local/lib/ela/bootstrap/list.ela"

#line 33 "/usr/local/lib/ela/bootstrap/list.ela"
  if (((new_capacity < self->capacity) && (new_capacity < self->length)))
#line 33 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 34 "/usr/local/lib/ela/bootstrap/list.ela"

#line 34 "/usr/local/lib/ela/bootstrap/list.ela"
    (self->length = new_capacity);
  };

#line 36 "/usr/local/lib/ela/bootstrap/list.ela"

#line 36 "/usr/local/lib/ela/bootstrap/list.ela"
  (self->capacity = new_capacity);

#line 37 "/usr/local/lib/ela/bootstrap/list.ela"

#line 37 "/usr/local/lib/ela/bootstrap/list.ela"
  (self->data =(str*) realloc(self->data, (sizeof(str) * self->capacity)));
}
#line 55 "/usr/local/lib/ela/bootstrap/list.ela"
void List$17$push( List$17* self, str v)
#line 55 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 56 "/usr/local/lib/ela/bootstrap/list.ela"

#line 56 "/usr/local/lib/ela/bootstrap/list.ela"
  if (((self->length + 1) >= self->capacity))
#line 56 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 57 "/usr/local/lib/ela/bootstrap/list.ela"

#line 57 "/usr/local/lib/ela/bootstrap/list.ela"
    if ((self->capacity == 0))
#line 57 "/usr/local/lib/ela/bootstrap/list.ela"
 {

#line 57 "/usr/local/lib/ela/bootstrap/list.ela"

#line 57 "/usr/local/lib/ela/bootstrap/list.ela"
      (self->capacity = 4);
    };

#line 58 "/usr/local/lib/ela/bootstrap/list.ela"

#line 58 "/usr/local/lib/ela/bootstrap/list.ela"
    List$17$resize(self, (self->capacity * 2));
  };

#line 60 "/usr/local/lib/ela/bootstrap/list.ela"

#line 60 "/usr/local/lib/ela/bootstrap/list.ela"
  (self->data[self->length] = v);

#line 61 "/usr/local/lib/ela/bootstrap/list.ela"

#line 61 "/usr/local/lib/ela/bootstrap/list.ela"
  self->length++;
}
#line 7 "/home/josh_arch/source/c++/ela/tests/all-libs.ela"

#line 2 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 4 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 6 "/usr/local/lib/ela/bootstrap/typedef.ela"

#line 6 "/usr/local/lib/ela/bootstrap/typedef.ela"
typedef struct  Type Type;
;
#line 12 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 20 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 30 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 36 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 46 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 54 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 59 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 65 "/usr/local/lib/ela/bootstrap/typedef.ela"

#line 65 "/usr/local/lib/ela/bootstrap/typedef.ela"
u8 RESULT_IS_ERR  = 0;
;
#line 66 "/usr/local/lib/ela/bootstrap/typedef.ela"

#line 66 "/usr/local/lib/ela/bootstrap/typedef.ela"
u8 RESULT_IS_OK  = 1;
;
#line 68 "/usr/local/lib/ela/bootstrap/typedef.ela"
;
#line 76 "/usr/local/lib/ela/bootstrap/typedef.ela"
;;

#line 6 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 11 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 18 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 24 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 29 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 34 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 43 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 52 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 57 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 65 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 70 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 78 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 85 "/usr/local/lib/ela/bootstrap/interfaces.ela"

#line 85 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 89 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 93 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 96 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;
#line 99 "/usr/local/lib/ela/bootstrap/interfaces.ela"
;;

#line 2 "/usr/local/lib/ela/bootstrap/option.ela"

#line 2 "/usr/local/lib/ela/bootstrap/option.ela"
void panic (str msg);
;
#line 3 "/usr/local/lib/ela/bootstrap/option.ela"
;
#line 38 "/usr/local/lib/ela/bootstrap/option.ela"

#line 38 "/usr/local/lib/ela/bootstrap/option.ela"
;
#line 45 "/usr/local/lib/ela/bootstrap/option.ela"

#line 45 "/usr/local/lib/ela/bootstrap/option.ela"
;;

#line 2 "/usr/local/lib/ela/bootstrap/result.ela"
;;

#line 7 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 16 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 29 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 37 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 50 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 67 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 75 "/usr/local/lib/ela/bootstrap/iter.ela"
;
#line 80 "/usr/local/lib/ela/bootstrap/iter.ela"
;;

#line 5 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 7 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 8 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 9 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 10 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 12 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 13 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 14 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 15 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 17 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 18 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 20 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 22 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 23 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 25 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 27 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 28 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 29 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 30 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 32 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 33 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 34 "/usr/local/lib/ela/bootstrap/numeric.ela"
;
#line 35 "/usr/local/lib/ela/bootstrap/numeric.ela"
;;

#line 4 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 9 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 10 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 11 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 12 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 14 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 15 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 16 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 17 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 19 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 21 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 27 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 33 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 38 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 50 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 62 "/usr/local/lib/ela/bootstrap/range.ela"
;
#line 79 "/usr/local/lib/ela/bootstrap/range.ela"
;;

#line 5 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 6 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 6 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  system(u8*); ;
#line 8 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 9 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 10 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 10 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void*  calloc(u64, u64); ;
#line 11 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 13 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 14 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 16 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 16 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  scanf(u8*, ...);; ;
#line 17 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 17 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  getchar(); ;
#line 19 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 19 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void  sleep(s32); ;
#line 20 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 20 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void  usleep(s32); ;
#line 22 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 22 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  snprintf(u8*, u64, u8*, ...);; ;
#line 23 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 23 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  sprintf(u8*, u8*, ...);; ;
#line 25 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 26 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 26 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strndup(u8*, u64); ;
#line 27 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 27 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strerror(s32); ;
#line 29 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 29 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strtok(u8*, u8*); ;
#line 30 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 30 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strcat(u8*, u8*); ;
#line 31 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 31 "/usr/local/lib/ela/bootstrap/libc.ela"
extern u8*  strncat(u8*, u8*, u64); ;
#line 33 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 33 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  atoi(u8*); ;
#line 34 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 34 "/usr/local/lib/ela/bootstrap/libc.ela"
extern f64  atof(u8*); ;
#line 39 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 40 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 41 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 41 "/usr/local/lib/ela/bootstrap/libc.ela"
extern void*  memset(void*, s32, u64); ;
#line 42 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 43 "/usr/local/lib/ela/bootstrap/libc.ela"
;
#line 44 "/usr/local/lib/ela/bootstrap/libc.ela"
;;
#line 46 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 46 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isalnum(s32); ;
#line 47 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 47 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isalpha(s32); ;
#line 48 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 48 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isspace(s32); ;
#line 49 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 49 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isdigit(s32); ;
#line 50 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 50 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  islower(s32); ;
#line 51 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 51 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isupper(s32); ;
#line 52 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 52 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  isprint(s32); ;
#line 53 "/usr/local/lib/ela/bootstrap/libc.ela"

#line 53 "/usr/local/lib/ela/bootstrap/libc.ela"
extern s32  ispunct(s32); ;;

#line 1 "/usr/local/lib/ela/bootstrap/list.ela"

#line 1 "/usr/local/lib/ela/bootstrap/list.ela"
void panic (str msg);
;
#line 3 "/usr/local/lib/ela/bootstrap/list.ela"

#line 4 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 100 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 111 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 126 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 144 "/usr/local/lib/ela/bootstrap/list.ela"
;;
#line 177 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 209 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 218 "/usr/local/lib/ela/bootstrap/list.ela"
;
#line 230 "/usr/local/lib/ela/bootstrap/list.ela"

#line 231 "/usr/local/lib/ela/bootstrap/list.ela"
;;;

#line 2 "/usr/local/lib/ela/bootstrap/slice.ela"

#line 3 "/usr/local/lib/ela/bootstrap/slice.ela"
;;
#line 15 "/usr/local/lib/ela/bootstrap/slice.ela"
;
#line 24 "/usr/local/lib/ela/bootstrap/slice.ela"
;
#line 51 "/usr/local/lib/ela/bootstrap/slice.ela"
;
#line 67 "/usr/local/lib/ela/bootstrap/slice.ela"

#line 68 "/usr/local/lib/ela/bootstrap/slice.ela"
Slice$7 Slice$7$as_byte_slice( Slice$7 self)
#line 68 "/usr/local/lib/ela/bootstrap/slice.ela"
 {

#line 69 "/usr/local/lib/ela/bootstrap/slice.ela"

#line 69 "/usr/local/lib/ela/bootstrap/slice.ela"
  return self;
;
};;

#line 2 "/usr/local/lib/ela/bootstrap/str.ela"

#line 3 "/usr/local/lib/ela/bootstrap/str.ela"

#line 4 "/usr/local/lib/ela/bootstrap/str.ela"
String str$as_string( str self)
#line 4 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 5 "/usr/local/lib/ela/bootstrap/str.ela"

#line 5 "/usr/local/lib/ela/bootstrap/str.ela"
  return (String) {.data = (u8*)strdup(self.data),
.length = (u64)self.length};
;
};;
#line 13 "/usr/local/lib/ela/bootstrap/str.ela"

#line 14 "/usr/local/lib/ela/bootstrap/str.ela"

#line 15 "/usr/local/lib/ela/bootstrap/str.ela"
bool str$starts_with( str* self, u8* prefix)
#line 15 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 16 "/usr/local/lib/ela/bootstrap/str.ela"
  
#line 16 "/usr/local/lib/ela/bootstrap/str.ela"
u32 prefix_len  = strlen(prefix);
;

#line 17 "/usr/local/lib/ela/bootstrap/str.ela"

#line 17 "/usr/local/lib/ela/bootstrap/str.ela"
  if ((self->length < prefix_len))
#line 17 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 17 "/usr/local/lib/ela/bootstrap/str.ela"

#line 17 "/usr/local/lib/ela/bootstrap/str.ela"
    return false;
;
  };

#line 18 "/usr/local/lib/ela/bootstrap/str.ela"

#line 18 "/usr/local/lib/ela/bootstrap/str.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 18 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 19 "/usr/local/lib/ela/bootstrap/str.ela"

#line 19 "/usr/local/lib/ela/bootstrap/str.ela"
        if ((self->data[i] != prefix[i]))
#line 19 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 19 "/usr/local/lib/ela/bootstrap/str.ela"

#line 19 "/usr/local/lib/ela/bootstrap/str.ela"
          return false;
;
        };
      }    }
  }
;

#line 21 "/usr/local/lib/ela/bootstrap/str.ela"

#line 21 "/usr/local/lib/ela/bootstrap/str.ela"
  return true;
;
}
#line 24 "/usr/local/lib/ela/bootstrap/str.ela"
bool str$ends_with( str* self, u8* suffix)
#line 24 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 25 "/usr/local/lib/ela/bootstrap/str.ela"
  
#line 25 "/usr/local/lib/ela/bootstrap/str.ela"
u32 suffix_len  = strlen(suffix);
;

#line 26 "/usr/local/lib/ela/bootstrap/str.ela"

#line 26 "/usr/local/lib/ela/bootstrap/str.ela"
  if ((self->length < suffix_len))
#line 26 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 26 "/usr/local/lib/ela/bootstrap/str.ela"

#line 26 "/usr/local/lib/ela/bootstrap/str.ela"
    return false;
;
  };

#line 27 "/usr/local/lib/ela/bootstrap/str.ela"

#line 27 "/usr/local/lib/ela/bootstrap/str.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = suffix_len};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 27 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 28 "/usr/local/lib/ela/bootstrap/str.ela"

#line 28 "/usr/local/lib/ela/bootstrap/str.ela"
        if ((self->data[((self->length - suffix_len) + i)] != suffix[i]))
#line 28 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 28 "/usr/local/lib/ela/bootstrap/str.ela"

#line 28 "/usr/local/lib/ela/bootstrap/str.ela"
          return false;
;
        };
      }    }
  }
;

#line 30 "/usr/local/lib/ela/bootstrap/str.ela"

#line 30 "/usr/local/lib/ela/bootstrap/str.ela"
  return true;
;
};;
#line 35 "/usr/local/lib/ela/bootstrap/str.ela"

#line 39 "/usr/local/lib/ela/bootstrap/str.ela"
u8* str$subscript_mut( str* self, u64 idx)
#line 39 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 40 "/usr/local/lib/ela/bootstrap/str.ela"

#line 40 "/usr/local/lib/ela/bootstrap/str.ela"
  return (&self->data[idx]);
;
};
#line 44 "/usr/local/lib/ela/bootstrap/str.ela"

#line 45 "/usr/local/lib/ela/bootstrap/str.ela"
bool str$is_empty( str self)
#line 45 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 45 "/usr/local/lib/ela/bootstrap/str.ela"

#line 45 "/usr/local/lib/ela/bootstrap/str.ela"
  return (self.length == 0);
;
}
#line 47 "/usr/local/lib/ela/bootstrap/str.ela"
str str$empty()
#line 47 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 48 "/usr/local/lib/ela/bootstrap/str.ela"

#line 48 "/usr/local/lib/ela/bootstrap/str.ela"
  return (str) {0};
;
}
#line 51 "/usr/local/lib/ela/bootstrap/str.ela"
Slice$7 str$slice( str* self, Range_Base$1 range)
#line 51 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 52 "/usr/local/lib/ela/bootstrap/str.ela"

#line 52 "/usr/local/lib/ela/bootstrap/str.ela"
$tuple1$1 $deconstruction$0 = ($tuple1$1) {.$0 = range.begin, .$1 = range.end};
#line 52 "/usr/local/lib/ela/bootstrap/str.ela"
auto start = $deconstruction$0.$0;
auto end = $deconstruction$0.$1;

#line 52 "/usr/local/lib/ela/bootstrap/str.ela"
;

#line 53 "/usr/local/lib/ela/bootstrap/str.ela"
  
#line 53 "/usr/local/lib/ela/bootstrap/str.ela"
s32 length  = (end - start);
;

#line 55 "/usr/local/lib/ela/bootstrap/str.ela"

#line 55 "/usr/local/lib/ela/bootstrap/str.ela"
  if ((((start < 0) || (length < 0)) || ((start + length) > self->length)))
#line 55 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 56 "/usr/local/lib/ela/bootstrap/str.ela"

#line 56 "/usr/local/lib/ela/bootstrap/str.ela"
    return (Slice$7) {0};
;
  };

#line 59 "/usr/local/lib/ela/bootstrap/str.ela"

#line 59 "/usr/local/lib/ela/bootstrap/str.ela"
  return (Slice$7) {.data = (u8*)(self->data + start),
.length = (u64)length};
;
};
#line 67 "/usr/local/lib/ela/bootstrap/str.ela"

#line 74 "/usr/local/lib/ela/bootstrap/str.ela"
IterMut$62 str$iter_mut( str* self)
#line 74 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 75 "/usr/local/lib/ela/bootstrap/str.ela"

#line 75 "/usr/local/lib/ela/bootstrap/str.ela"
  return (IterMut$62) {.ptr = (u8**)(u8*)self->data,
.end = (u8**)(self->data + self->length)};
;
};
#line 82 "/usr/local/lib/ela/bootstrap/str.ela"

#line 91 "/usr/local/lib/ela/bootstrap/str.ela"
bool str$neq( str self, str other)
#line 91 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 92 "/usr/local/lib/ela/bootstrap/str.ela"

#line 92 "/usr/local/lib/ela/bootstrap/str.ela"
  return (!str$eq(self, other));
;
};
#line 96 "/usr/local/lib/ela/bootstrap/str.ela"

#line 97 "/usr/local/lib/ela/bootstrap/str.ela"
u64 str$hash( str self)
#line 97 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 98 "/usr/local/lib/ela/bootstrap/str.ela"
  
#line 98 "/usr/local/lib/ela/bootstrap/str.ela"
u64 hash  = 0xCBF29CE484222325;
;

#line 99 "/usr/local/lib/ela/bootstrap/str.ela"

#line 99 "/usr/local/lib/ela/bootstrap/str.ela"
  {
    str $_range_id0 = self;
    Iter$7 $_loop_id0 = str$iter(&$_range_id0);
    while (1) {
auto $next0 = Iter$7$next(&$_loop_id0);
if (!$next0.has_value) break;
      u8 byte = *$next0.s;
#line 99 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 100 "/usr/local/lib/ela/bootstrap/str.ela"

#line 100 "/usr/local/lib/ela/bootstrap/str.ela"
        (hash ^= byte);

#line 101 "/usr/local/lib/ela/bootstrap/str.ela"

#line 101 "/usr/local/lib/ela/bootstrap/str.ela"
        (hash *= 0x100000001B3);
      }    }
  }
;

#line 103 "/usr/local/lib/ela/bootstrap/str.ela"

#line 103 "/usr/local/lib/ela/bootstrap/str.ela"
  return hash;
;
};
#line 107 "/usr/local/lib/ela/bootstrap/str.ela"

#line 108 "/usr/local/lib/ela/bootstrap/str.ela"
Slice$5 str$as_char_slice( str self)
#line 108 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 109 "/usr/local/lib/ela/bootstrap/str.ela"

#line 109 "/usr/local/lib/ela/bootstrap/str.ela"
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)self.length};
;
};
#line 116 "/usr/local/lib/ela/bootstrap/str.ela"

#line 117 "/usr/local/lib/ela/bootstrap/str.ela"
Slice$7 str$as_byte_slice( str self)
#line 117 "/usr/local/lib/ela/bootstrap/str.ela"
 {

#line 118 "/usr/local/lib/ela/bootstrap/str.ela"

#line 118 "/usr/local/lib/ela/bootstrap/str.ela"
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};;

#line 2 "/usr/local/lib/ela/bootstrap/String.ela"

#line 4 "/usr/local/lib/ela/bootstrap/String.ela"

#line 5 "/usr/local/lib/ela/bootstrap/String.ela"
String String$replace( String* self, u8* old, u8* replacement)
#line 5 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 6 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 6 "/usr/local/lib/ela/bootstrap/String.ela"
u32 old_len  = strlen(old);
;

#line 7 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 7 "/usr/local/lib/ela/bootstrap/String.ela"
u32 new_len  = strlen(replacement);
;

#line 8 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 8 "/usr/local/lib/ela/bootstrap/String.ela"
String result  = (String) {0};
;

#line 9 "/usr/local/lib/ela/bootstrap/String.ela"

#line 9 "/usr/local/lib/ela/bootstrap/String.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = self->length};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 9 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 10 "/usr/local/lib/ela/bootstrap/String.ela"

#line 10 "/usr/local/lib/ela/bootstrap/String.ela"
        if ((strncmp((self->data + i), old, old_len) == 0))
#line 10 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 11 "/usr/local/lib/ela/bootstrap/String.ela"

#line 11 "/usr/local/lib/ela/bootstrap/String.ela"
          (result.data =(u8*) realloc(result.data, ((result.length + new_len) + 1)));

#line 12 "/usr/local/lib/ela/bootstrap/String.ela"

#line 12 "/usr/local/lib/ela/bootstrap/String.ela"
          memcpy((result.data + result.length), replacement, new_len);

#line 13 "/usr/local/lib/ela/bootstrap/String.ela"

#line 13 "/usr/local/lib/ela/bootstrap/String.ela"
          (result.length += new_len);

#line 14 "/usr/local/lib/ela/bootstrap/String.ela"

#line 14 "/usr/local/lib/ela/bootstrap/String.ela"
          (i += (old_len - 1));
        }
#line 15 "/usr/local/lib/ela/bootstrap/String.ela"
 else 
#line 15 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 16 "/usr/local/lib/ela/bootstrap/String.ela"

#line 16 "/usr/local/lib/ela/bootstrap/String.ela"
          (result.data =(u8*) realloc(result.data, ((result.length + 1) + 1)));

#line 17 "/usr/local/lib/ela/bootstrap/String.ela"

#line 17 "/usr/local/lib/ela/bootstrap/String.ela"
          (result.data[result.length] = self->data[i]);

#line 18 "/usr/local/lib/ela/bootstrap/String.ela"

#line 18 "/usr/local/lib/ela/bootstrap/String.ela"
          result.length++;
        };
      }    }
  }
;

#line 21 "/usr/local/lib/ela/bootstrap/String.ela"

#line 21 "/usr/local/lib/ela/bootstrap/String.ela"
  (result.data[result.length] = 0);

#line 22 "/usr/local/lib/ela/bootstrap/String.ela"

#line 22 "/usr/local/lib/ela/bootstrap/String.ela"
  return result;
;
}
#line 25 "/usr/local/lib/ela/bootstrap/String.ela"

#line 35 "/usr/local/lib/ela/bootstrap/String.ela"
String String$from_ptr(u8* data)
#line 35 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 36 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 36 "/usr/local/lib/ela/bootstrap/String.ela"
u32 length  = strlen(data);
;

#line 37 "/usr/local/lib/ela/bootstrap/String.ela"

#line 37 "/usr/local/lib/ela/bootstrap/String.ela"
  return (String) {.data = (u8*)strdup(data),
.capacity = (u64)length,
.length = (u64)length};
;
};
#line 50 "/usr/local/lib/ela/bootstrap/String.ela"

#line 58 "/usr/local/lib/ela/bootstrap/String.ela"
void String$push( String* self, u8 ch)
#line 58 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 59 "/usr/local/lib/ela/bootstrap/String.ela"

#line 59 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->capacity == 0))
#line 59 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 59 "/usr/local/lib/ela/bootstrap/String.ela"

#line 59 "/usr/local/lib/ela/bootstrap/String.ela"
    (self->capacity = (256 / 3));
  };

#line 60 "/usr/local/lib/ela/bootstrap/String.ela"

#line 60 "/usr/local/lib/ela/bootstrap/String.ela"
  if (((self->length + 1) >= self->capacity))
#line 60 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 61 "/usr/local/lib/ela/bootstrap/String.ela"

#line 61 "/usr/local/lib/ela/bootstrap/String.ela"
    String$resize(self, ((self->capacity * 3) + 1));
  };

#line 63 "/usr/local/lib/ela/bootstrap/String.ela"

#line 63 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = ch);

#line 64 "/usr/local/lib/ela/bootstrap/String.ela"

#line 64 "/usr/local/lib/ela/bootstrap/String.ela"
  self->length++;

#line 65 "/usr/local/lib/ela/bootstrap/String.ela"

#line 65 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = 0x0);
}
#line 68 "/usr/local/lib/ela/bootstrap/String.ela"
u8 String$pop( String* self)
#line 68 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 69 "/usr/local/lib/ela/bootstrap/String.ela"

#line 69 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length == 0))
#line 69 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 69 "/usr/local/lib/ela/bootstrap/String.ela"

#line 69 "/usr/local/lib/ela/bootstrap/String.ela"
    return (u8)0x0;
;
  };

#line 70 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 70 "/usr/local/lib/ela/bootstrap/String.ela"
u8 ch  = self->data[self->length--];
;

#line 71 "/usr/local/lib/ela/bootstrap/String.ela"

#line 71 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = 0x0);

#line 72 "/usr/local/lib/ela/bootstrap/String.ela"

#line 72 "/usr/local/lib/ela/bootstrap/String.ela"
  return ch;
;
}
#line 75 "/usr/local/lib/ela/bootstrap/String.ela"
u8 String$pop_front( String* self)
#line 75 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 76 "/usr/local/lib/ela/bootstrap/String.ela"

#line 76 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length == 0))
#line 76 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 76 "/usr/local/lib/ela/bootstrap/String.ela"

#line 76 "/usr/local/lib/ela/bootstrap/String.ela"
    return (u8)0x0;
;
  };

#line 77 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 77 "/usr/local/lib/ela/bootstrap/String.ela"
u8 ch  = self->data[0];
;

#line 78 "/usr/local/lib/ela/bootstrap/String.ela"

#line 78 "/usr/local/lib/ela/bootstrap/String.ela"
  memmove(self->data, (self->data + 1), (self->length - 1));

#line 79 "/usr/local/lib/ela/bootstrap/String.ela"

#line 79 "/usr/local/lib/ela/bootstrap/String.ela"
  self->length--;

#line 80 "/usr/local/lib/ela/bootstrap/String.ela"

#line 80 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = 0x0);

#line 81 "/usr/local/lib/ela/bootstrap/String.ela"

#line 81 "/usr/local/lib/ela/bootstrap/String.ela"
  return ch;
;
}
#line 84 "/usr/local/lib/ela/bootstrap/String.ela"
void String$push_front( String* self, u8 ch)
#line 84 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 85 "/usr/local/lib/ela/bootstrap/String.ela"

#line 85 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->capacity == 0))
#line 85 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 85 "/usr/local/lib/ela/bootstrap/String.ela"

#line 85 "/usr/local/lib/ela/bootstrap/String.ela"
    (self->capacity = (256 / 3));
  };

#line 86 "/usr/local/lib/ela/bootstrap/String.ela"

#line 86 "/usr/local/lib/ela/bootstrap/String.ela"
  if (((self->length + 1) >= self->capacity))
#line 86 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 87 "/usr/local/lib/ela/bootstrap/String.ela"

#line 87 "/usr/local/lib/ela/bootstrap/String.ela"
    String$resize(self, (self->capacity * 3));
  };

#line 89 "/usr/local/lib/ela/bootstrap/String.ela"

#line 89 "/usr/local/lib/ela/bootstrap/String.ela"
  memmove((self->data + 1), self->data, self->length);

#line 90 "/usr/local/lib/ela/bootstrap/String.ela"

#line 90 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[0] = ch);

#line 91 "/usr/local/lib/ela/bootstrap/String.ela"

#line 91 "/usr/local/lib/ela/bootstrap/String.ela"
  self->length++;

#line 92 "/usr/local/lib/ela/bootstrap/String.ela"

#line 92 "/usr/local/lib/ela/bootstrap/String.ela"
  (self->data[self->length] = 0x0);
};
#line 96 "/usr/local/lib/ela/bootstrap/String.ela"

#line 97 "/usr/local/lib/ela/bootstrap/String.ela"
void String$deinit( String* self)
#line 97 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 98 "/usr/local/lib/ela/bootstrap/String.ela"

#line 98 "/usr/local/lib/ela/bootstrap/String.ela"
  free(self->data);
};
#line 102 "/usr/local/lib/ela/bootstrap/String.ela"

#line 103 "/usr/local/lib/ela/bootstrap/String.ela"
bool String$starts_with( String* self, u8* prefix)
#line 103 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 104 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 104 "/usr/local/lib/ela/bootstrap/String.ela"
u32 prefix_len  = strlen(prefix);
;

#line 105 "/usr/local/lib/ela/bootstrap/String.ela"

#line 105 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length < prefix_len))
#line 105 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 105 "/usr/local/lib/ela/bootstrap/String.ela"

#line 105 "/usr/local/lib/ela/bootstrap/String.ela"
    return false;
;
  };

#line 106 "/usr/local/lib/ela/bootstrap/String.ela"

#line 106 "/usr/local/lib/ela/bootstrap/String.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = prefix_len};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 106 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 107 "/usr/local/lib/ela/bootstrap/String.ela"

#line 107 "/usr/local/lib/ela/bootstrap/String.ela"
        if ((self->data[i] != prefix[i]))
#line 107 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 107 "/usr/local/lib/ela/bootstrap/String.ela"

#line 107 "/usr/local/lib/ela/bootstrap/String.ela"
          return false;
;
        };
      }    }
  }
;

#line 109 "/usr/local/lib/ela/bootstrap/String.ela"

#line 109 "/usr/local/lib/ela/bootstrap/String.ela"
  return true;
;
}
#line 112 "/usr/local/lib/ela/bootstrap/String.ela"
bool String$ends_with( String* self, u8* suffix)
#line 112 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 113 "/usr/local/lib/ela/bootstrap/String.ela"
  
#line 113 "/usr/local/lib/ela/bootstrap/String.ela"
u32 suffix_len  = strlen(suffix);
;

#line 114 "/usr/local/lib/ela/bootstrap/String.ela"

#line 114 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length < suffix_len))
#line 114 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 114 "/usr/local/lib/ela/bootstrap/String.ela"

#line 114 "/usr/local/lib/ela/bootstrap/String.ela"
    return false;
;
  };

#line 115 "/usr/local/lib/ela/bootstrap/String.ela"

#line 115 "/usr/local/lib/ela/bootstrap/String.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = suffix_len};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 115 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 116 "/usr/local/lib/ela/bootstrap/String.ela"

#line 116 "/usr/local/lib/ela/bootstrap/String.ela"
        if ((self->data[((self->length - suffix_len) + i)] != suffix[i]))
#line 116 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 116 "/usr/local/lib/ela/bootstrap/String.ela"

#line 116 "/usr/local/lib/ela/bootstrap/String.ela"
          return false;
;
        };
      }    }
  }
;

#line 118 "/usr/local/lib/ela/bootstrap/String.ela"

#line 118 "/usr/local/lib/ela/bootstrap/String.ela"
  return true;
;
};;
#line 124 "/usr/local/lib/ela/bootstrap/String.ela"

#line 128 "/usr/local/lib/ela/bootstrap/String.ela"
u8* String$subscript_mut( String* self, u64 idx)
#line 128 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 129 "/usr/local/lib/ela/bootstrap/String.ela"

#line 129 "/usr/local/lib/ela/bootstrap/String.ela"
  return (&self->data[idx]);
;
};
#line 132 "/usr/local/lib/ela/bootstrap/String.ela"

#line 134 "/usr/local/lib/ela/bootstrap/String.ela"
str String$as_str( String self)
#line 134 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 135 "/usr/local/lib/ela/bootstrap/String.ela"

#line 135 "/usr/local/lib/ela/bootstrap/String.ela"
  return (str) {.data = (u8*)self.data,
.length = (u64)self.length};
;
}
#line 141 "/usr/local/lib/ela/bootstrap/String.ela"
bool String$is_empty( String self)
#line 141 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 141 "/usr/local/lib/ela/bootstrap/String.ela"

#line 141 "/usr/local/lib/ela/bootstrap/String.ela"
  return (self.length == 0);
;
}
#line 143 "/usr/local/lib/ela/bootstrap/String.ela"
String String$empty()
#line 143 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 144 "/usr/local/lib/ela/bootstrap/String.ela"

#line 144 "/usr/local/lib/ela/bootstrap/String.ela"
  return (String) {0};
;
}
#line 147 "/usr/local/lib/ela/bootstrap/String.ela"
u8 String$front( String* self)
#line 147 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 148 "/usr/local/lib/ela/bootstrap/String.ela"

#line 148 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length == 0))
#line 148 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 148 "/usr/local/lib/ela/bootstrap/String.ela"

#line 148 "/usr/local/lib/ela/bootstrap/String.ela"
    return (u8)0x0;
;
  };

#line 149 "/usr/local/lib/ela/bootstrap/String.ela"

#line 149 "/usr/local/lib/ela/bootstrap/String.ela"
  return self->data[0];
;
}
#line 152 "/usr/local/lib/ela/bootstrap/String.ela"
u8 String$back( String* self)
#line 152 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 153 "/usr/local/lib/ela/bootstrap/String.ela"

#line 153 "/usr/local/lib/ela/bootstrap/String.ela"
  if ((self->length == 0))
#line 153 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 153 "/usr/local/lib/ela/bootstrap/String.ela"

#line 153 "/usr/local/lib/ela/bootstrap/String.ela"
    return (u8)0x0;
;
  };

#line 154 "/usr/local/lib/ela/bootstrap/String.ela"

#line 154 "/usr/local/lib/ela/bootstrap/String.ela"
  return self->data[(self->length - 1)];
;
};
#line 158 "/usr/local/lib/ela/bootstrap/String.ela"

#line 166 "/usr/local/lib/ela/bootstrap/String.ela"
bool String$neq( String self, String other)
#line 166 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 167 "/usr/local/lib/ela/bootstrap/String.ela"

#line 167 "/usr/local/lib/ela/bootstrap/String.ela"
  return (!String$eq(self, other));
;
};
#line 171 "/usr/local/lib/ela/bootstrap/String.ela"

#line 172 "/usr/local/lib/ela/bootstrap/String.ela"
Iter$7 String$iter( String* self)
#line 172 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 173 "/usr/local/lib/ela/bootstrap/String.ela"

#line 173 "/usr/local/lib/ela/bootstrap/String.ela"
  return (Iter$7) {.ptr = (u8*)self->data,
.end = (u8*)(self->data + self->length)};
;
}
#line 178 "/usr/local/lib/ela/bootstrap/String.ela"
Iter$62 String$iter_mut( String* self)
#line 178 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 179 "/usr/local/lib/ela/bootstrap/String.ela"

#line 179 "/usr/local/lib/ela/bootstrap/String.ela"
  return (Iter$62) {.ptr = (u8**)self->data,
.end = (u8**)(self->data + self->length)};
;
};
#line 186 "/usr/local/lib/ela/bootstrap/String.ela"

#line 187 "/usr/local/lib/ela/bootstrap/String.ela"
Slice$5 String$as_char_slice( String self)
#line 187 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 188 "/usr/local/lib/ela/bootstrap/String.ela"

#line 188 "/usr/local/lib/ela/bootstrap/String.ela"
  return (Slice$5) {.data = (u32*)self.data,
.length = (u64)(self.length / 4)};
;
};
#line 195 "/usr/local/lib/ela/bootstrap/String.ela"

#line 196 "/usr/local/lib/ela/bootstrap/String.ela"
Slice$7 String$as_byte_slice( String self)
#line 196 "/usr/local/lib/ela/bootstrap/String.ela"
 {

#line 197 "/usr/local/lib/ela/bootstrap/String.ela"

#line 197 "/usr/local/lib/ela/bootstrap/String.ela"
  return (Slice$7) {.data = (u8*)self.data,
.length = (u64)self.length};
;
};;

#line 1 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 9 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 19 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 19 "/usr/local/lib/ela/bootstrap/reflection.ela"
typedef struct  Element{
  u8* data;
  Type* type;
} Element;
;
#line 24 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 25 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 26 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 29 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 34 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 34 "/usr/local/lib/ela/bootstrap/reflection.ela"
List$58 _type_info = {};
;
#line 36 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 37 "/usr/local/lib/ela/bootstrap/reflection.ela"
Field* Type$get_field( Type* self, u8* name)
#line 37 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 38 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 38 "/usr/local/lib/ela/bootstrap/reflection.ela"
  {
    List$34 $_range_id0 = self->fields;
    Iter$34 $_loop_id0 = List$34$iter(&$_range_id0);
    while (1) {
auto $next0 = Iter$34$next(&$_loop_id0);
if (!$next0.has_value) break;
      Field* field = $next0.s;
#line 38 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 39 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 39 "/usr/local/lib/ela/bootstrap/reflection.ela"
        if ((strcmp(field->name, name) == 0))
#line 39 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 40 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 40 "/usr/local/lib/ela/bootstrap/reflection.ela"
          return field;
;
        };
      }    }
  }
;

#line 43 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 43 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return NULL;
;
}
#line 46 "/usr/local/lib/ela/bootstrap/reflection.ela"
u64 Type$offset( Type* self, str member)
#line 46 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 47 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 47 "/usr/local/lib/ela/bootstrap/reflection.ela"
  {
    List$34 $_range_id0 = self->fields;
    Iter$34 $_loop_id0 = List$34$iter(&$_range_id0);
    while (1) {
auto $next0 = Iter$34$next(&$_loop_id0);
if (!$next0.has_value) break;
      Field field = *$next0.s;
#line 47 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 48 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 48 "/usr/local/lib/ela/bootstrap/reflection.ela"
        if ((strncmp(field.name, member.data, (s32)member.length) == 0))
#line 48 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 49 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 49 "/usr/local/lib/ela/bootstrap/reflection.ela"
          return field.offset;
;
        };
      }    }
  }
;

#line 52 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 52 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return (u64)(-1);
;
};;
#line 57 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 73 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 74 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_signed( Type* self)
#line 74 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 75 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 75 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$SIGNED) != 0);
;
}
#line 78 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_unsigned( Type* self)
#line 78 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 79 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 79 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$UNSIGNED) != 0);
;
}
#line 87 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_array( Type* self)
#line 87 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 88 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 88 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$ARRAY) != 0);
;
}
#line 91 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_string( Type* self)
#line 91 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 92 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 92 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$STRING) != 0);
;
}
#line 95 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_scalar( Type* self)
#line 95 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 96 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 96 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return (Type$has_no_extension(self) && (((((self->flags & TypeFlags$INTEGER) != 0) || ((self->flags & TypeFlags$FLOAT) != 0)) || ((self->flags & TypeFlags$BOOL) != 0)) || ((self->flags & TypeFlags$STRING) != 0)));
;
}
#line 103 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_struct( Type* self)
#line 103 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 104 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 104 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$STRUCT) != 0);
;
}
#line 107 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_enum( Type* self)
#line 107 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 108 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 108 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$ENUM) != 0);
;
}
#line 111 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_integral( Type* self)
#line 111 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 112 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 112 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$INTEGER) != 0);
;
}
#line 115 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_float( Type* self)
#line 115 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 116 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 116 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$FLOAT) != 0);
;
}
#line 119 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_bool( Type* self)
#line 119 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 120 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 120 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$BOOL) != 0);
;
}
#line 123 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_tuple( Type* self)
#line 123 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 124 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 124 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$TUPLE) != 0);
;
}
#line 127 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_function( Type* self)
#line 127 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 128 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 128 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$FUNCTION) != 0);
;
}
#line 131 "/usr/local/lib/ela/bootstrap/reflection.ela"
bool Type$is_pointer( Type* self)
#line 131 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 132 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 132 "/usr/local/lib/ela/bootstrap/reflection.ela"
  return ((self->flags & TypeFlags$POINTER) != 0);
;
};
#line 136 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 137 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 146 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 155 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 160 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 161 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 162 "/usr/local/lib/ela/bootstrap/reflection.ela"
;
#line 171 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 174 "/usr/local/lib/ela/bootstrap/reflection.ela"
void any$deinit( any* self)
#line 174 "/usr/local/lib/ela/bootstrap/reflection.ela"
 {

#line 175 "/usr/local/lib/ela/bootstrap/reflection.ela"

#line 175 "/usr/local/lib/ela/bootstrap/reflection.ela"
  free(self->ptr);
};;;

#line 30 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 30 "/usr/local/lib/ela/bootstrap/lib.ela"
void assert(str message, bool condition)
#line 30 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 31 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 31 "/usr/local/lib/ela/bootstrap/lib.ela"
  if ((!condition))
#line 31 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 32 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 32 "/usr/local/lib/ela/bootstrap/lib.ela"
    printf("\033[1;31massertion failed\033[0m: \033[1;34m\"%s\"\033[0m\n", message.data);

#line 33 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 33 "/usr/local/lib/ela/bootstrap/lib.ela"
    exit(1);
  };
};
#line 37 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 37 "/usr/local/lib/ela/bootstrap/lib.ela"
;
#line 47 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 47 "/usr/local/lib/ela/bootstrap/lib.ela"
;;
;
;

#line 62 "/usr/local/lib/ela/bootstrap/lib.ela"
void set_panic_handler(void(*handler)(str))
#line 62 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 63 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 63 "/usr/local/lib/ela/bootstrap/lib.ela"
  (panic_handler =(void(*)(str)) handler);
};

#line 66 "/usr/local/lib/ela/bootstrap/lib.ela"
void panic(str msg)
#line 66 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 67 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 67 "/usr/local/lib/ela/bootstrap/lib.ela"
  if ((!panic_handler))
#line 67 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 68 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 69 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 69 "/usr/local/lib/ela/bootstrap/lib.ela"
    (panic_handler =(void(*)(str)) $lambda$0);;
  };

#line 81 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 81 "/usr/local/lib/ela/bootstrap/lib.ela"
  panic_handler(msg);
};

#line 107 "/usr/local/lib/ela/bootstrap/lib.ela"
;
#line 110 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 115 "/usr/local/lib/ela/bootstrap/lib.ela"
List$17 Env$args()
#line 115 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 116 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 116 "/usr/local/lib/ela/bootstrap/lib.ela"
  return List$17$clone(Env$current()->m_args);
;
}
#line 118 "/usr/local/lib/ela/bootstrap/lib.ela"
void Env$initialize(s32 argc, u8** argv)
#line 118 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 119 "/usr/local/lib/ela/bootstrap/lib.ela"
  
#line 119 "/usr/local/lib/ela/bootstrap/lib.ela"
Env* self  = Env$current();
;

#line 120 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 120 "/usr/local/lib/ela/bootstrap/lib.ela"
  {
    Range_Base$1 $_range_id0 = (Range_Base$1) {.begin = 0, .end = argc};
    RangeIter$1 $_loop_id0 = Range_Base$1$iter(&$_range_id0);
    while (1) {
auto $next0 = RangeIter$1$next(&$_loop_id0);
if (!$next0.has_value) break;
      s32 i = $next0.s;
#line 120 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 121 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 121 "/usr/local/lib/ela/bootstrap/lib.ela"
        List$17$push(&self->m_args, (str) {.data = (u8*)argv[i],
.length = (u64)strlen(argv[i])});
      }    }
  }
;
};;

#line 131 "/usr/local/lib/ela/bootstrap/lib.ela"
Slice$5 u32$as_char_slice( u32 self)
#line 131 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 132 "/usr/local/lib/ela/bootstrap/lib.ela"
  
#line 132 "/usr/local/lib/ela/bootstrap/lib.ela"
static u32 chars[2] =  {0x0, 0x0};;

#line 135 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 135 "/usr/local/lib/ela/bootstrap/lib.ela"
  (chars[0] = self);

#line 136 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 136 "/usr/local/lib/ela/bootstrap/lib.ela"
  return (Slice$5) {.data = (u32*)chars,
.length = (u64)1};
;
};

#line 144 "/usr/local/lib/ela/bootstrap/lib.ela"
Slice$7 u32$as_byte_slice( u32 self)
#line 144 "/usr/local/lib/ela/bootstrap/lib.ela"
 {

#line 145 "/usr/local/lib/ela/bootstrap/lib.ela"
  
#line 145 "/usr/local/lib/ela/bootstrap/lib.ela"
static u8 chars[2] =  {0x0, 0x0};;

#line 148 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 148 "/usr/local/lib/ela/bootstrap/lib.ela"
  (chars[0] = self);

#line 149 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 149 "/usr/local/lib/ela/bootstrap/lib.ela"
  return (Slice$7) {.data = (u8*)chars,
.length = (u64)1};
;
};

#line 177 "/usr/local/lib/ela/bootstrap/lib.ela"

#line 177 "/usr/local/lib/ela/bootstrap/lib.ela"
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
