
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
  #include <stdarg.h>

  // I don't think we need any of these includes anymore.
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
  
typedef struct str {
  u8* data;
  u64 length;
} str;
typedef struct $tuple24$29 {
  str $0;
  void* $1;
} $tuple24$29;
typedef struct Env Env;
typedef struct str str;
typedef struct Slice$24 {
  str* data;
  u64 length;
} Slice$24;
typedef struct Env {
  Slice$24 args_slice;
} Env;
Env* Env$current() {
  static Env self  = {};
  return (&self);
}
typedef struct RangeIter$1 {
  s32 begin;
  s32 end;
  s32 idx;
} RangeIter$1;
typedef struct RangeBase$1 {
  s32 begin;
  s32 end;
} RangeBase$1;
typedef struct RangeBase$1 RangeBase$1;
RangeIter$1 RangeBase$1$iter(RangeBase$1* self) {
  return (RangeIter$1){
    .begin = (s32)self->begin,
    .idx = (s32)self->begin,
    .end = (s32)self->end};
}
typedef struct $tuple1 {
  s32 $0;
} $tuple1;
typedef struct Option$1 Option$1;
typedef $tuple1 Option$1$Some;
typedef struct Option$1 {
  int index;
  union {
    Option$1$Some Some;
  };
} Option$1;
typedef struct RangeIter$1 RangeIter$1;
static inline Option$1 Some$1(s32 t) {
  return (Option$1) {
.index = 1,
.Some = ($tuple1) {.$0 = t}};
}
Option$1 RangeIter$1$next(RangeIter$1* self) {
  if ((self->idx == self->end)) {
    return  (Option$1) { .index = 0};
  }
  Option$1 value  = Some$1(self->idx);
  if ((self->begin > self->end)) {
    self->idx--;
  }
  else{
    self->idx++;
  }
  return value;
}
u32 Env$env_strlen(u8* s) {
  s32 i  = 0;
  while ((*s)) {
    i++;
  }
  return i;
}
void Env$initialize(s32 argc, u8** argv) {
  Env* self  = Env$current();
static str data[48] = {0};
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = argc};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 i = $next.Some.$0;
      {
        (data[i] = (str){
          .data = (u8*)argv[i],
          .length = (u64)Env$env_strlen(argv[i])});
      }
    }
  }
  (self->args_slice = (Slice$24){
    .data = (str*)data,
    .length = (u64)argc});
}
typedef struct dyn$Allocator{
void *instance;
void(*deinit)(void*);
void*(*copy)(void*, void*, u64, u64);
void*(*resize)(void*, void*, u64, u64);
void*(*allocate_array)(void*, u64, u64);
void(*free)(void*, void*);
void*(*allocate)(void*, u64);
} dyn$Allocator;
typedef struct Type Type;
void panic(str msg);
u8 u8$min_value() {
  return 0;
}
u8 u8$max_value() {
  return 255;
}
u16 u16$min_value() {
  return 0;
}
u16 u16$max_value() {
  return 65535;
}
u32 u32$min_value() {
  return 0;
}
u32 u32$max_value() {
  return 4294967295;
}
u64 u64$min_value() {
  return 0;
}
u64 u64$max_value() {
  return 18446744073709551615;
}
s8 s8$min_value() {
  return (s8)(-128);
}
s8 s8$max_value() {
  return 127;
}
s16 s16$min_value() {
  return (s16)(-32768);
}
s16 s16$max_value() {
  return 32767;
}
s32 s32$min_value() {
  return (s32)(-2147483648);
}
s32 s32$max_value() {
  return 2147483647;
}
s64 s64$min_value() {
  return (s64)(-9223372036854775808);
}
s64 s64$max_value() {
  return 9223372036854775807;
}
f32 f32$min_value() {
  return (f32)(-340282350000000000000000000000000000000.0f);
}
f32 f32$max_value() {
  return 340282350000000000000000000000000000000.0f;
}
f64 f64$min_value() {
  return (f64)(-179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0);
}
f64 f64$max_value() {
  return 179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0;
}
bool dyn$Allocator$has_instance(dyn$Allocator self) {
  return (self.instance != NULL);
}
dyn$Allocator global_allocator = {0};
void set_global_allocator(dyn$Allocator allocator) {
  (global_allocator = allocator);
}
dyn$Allocator get_global_allocator() {
  return global_allocator;
}
typedef struct Slice$7 {
  u8* data;
  u64 length;
} Slice$7;
Slice$7 Slice$7$as_byte_slice(Slice$7 self) {
  return self;
}
u8* str$subscript(str* self, u64 idx) {
  return (&self->data[idx]);
}
u8* str$subscript_mut(str* self, u64 idx) {
  return (&self->data[idx]);
}
typedef struct $tuple1$1 {
  s32 $0;
  s32 $1;
} $tuple1$1;
Slice$7 str$slice(str* self, RangeBase$1 range) {
$tuple1$1 $deconstruction$0 = ($tuple1$1) {.$0 = range.begin, .$1 = range.end};
auto start = $deconstruction$0.$0;
auto end = $deconstruction$0.$1;
  s32 length  = (end - start);
  if ((((start < 0) || (length < 0)) || ((start + length) > self->length))) {
    return (Slice$7){};
  }
  return (Slice$7){
    .data = (u8*)(self->data + start),
    .length = (u64)length};
}
typedef struct Iter$7 {
  u8* ptr;
  u8* end;
} Iter$7;
Iter$7 str$iter(str* self) {
  return (Iter$7){
    .ptr = (u8*)(u8*)self->data,
    .end = (u8*)(self->data + self->length)};
}
typedef struct IterMut$7 {
  u8* ptr;
  u8* end;
} IterMut$7;
IterMut$7 str$iter_mut(str* self) {
  return (IterMut$7){
    .ptr = (u8*)(u8*)self->data,
    .end = (u8*)(self->data + self->length)};
}
bool str$eq(str self, str other) {
  if ((self.length != other.length)) {
    return false;
  }
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = self.length};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 idx = $next.Some.$0;
      {
        if (((*(str$subscript(&self, idx))) != (*(str$subscript(&other, idx))))) {
          return false;
        }
      }
    }
  }
  return true;
}
bool str$neq(str self, str other) {
  return (!str$eq(self, other));
}
typedef struct $tuple7 {
  u8 $0;
} $tuple7;
typedef struct Option$7 Option$7;
typedef $tuple7 Option$7$Some;
typedef struct Option$7 {
  int index;
  union {
    Option$7$Some Some;
  };
} Option$7;
typedef struct Iter$7 Iter$7;
static inline Option$7 Iter$7$next(Iter$7* self) {
  if ((self->ptr >= self->end)) {
    return  (Option$7) { .index = 0};
  }
  Option$7 value  = (Option$7) {
.index = 1,
.Some = ($tuple7) {.$0 = (*self->ptr)}};
  self->ptr++;
  return value;
}
u64 str$hash(str self) {
  u64 hash  = 0xCBF29CE484222325;
  {
    str $iterable = self;
    Iter$7 $iterator = str$iter(&$iterable);
    while (1) {
      auto $next = Iter$7$next(&$iterator);
      if ($next.index == 0) break;
      u8 byte = $next.Some.$0;
      {
        (hash ^= byte);
        (hash *= 0x100000001B3);
      }
    }
  }
  return hash;
}
typedef struct Slice$5 {
  u32* data;
  u64 length;
} Slice$5;
Slice$5 str$as_char_slice(str self) {
  return (Slice$5){
    .data = (u32*)self.data,
    .length = (u64)self.length};
}
Slice$7 str$as_byte_slice(str self) {
  return (Slice$7){
    .data = (u8*)self.data,
    .length = (u64)self.length};
}
typedef struct String String;
typedef struct String {
  u8* data;
  u64 length;
  u64 capacity;
  dyn$Allocator allocator;
} String;
u8* String$subscript(String* self, u64 idx) {
  return (&self->data[idx]);
}
u8* String$subscript_mut(String* self, u64 idx) {
  return (&self->data[idx]);
}
str String$as_str(String self) {
  return (str){
    .data = (u8*)self.data,
    .length = (u64)self.length};
}
Slice$7 String$slice(String self, RangeBase$1 range) {
  return (Slice$7){
    .data = (u8*)(self.data + range.begin),
    .length = (u64)(range.end - range.begin)};
}
typedef struct SliceMut$7 {
  u8* data;
  u64 length;
} SliceMut$7;
SliceMut$7 String$slice_mut(String self, RangeBase$1 range) {
  return (SliceMut$7){
    .data = (u8*)(u8*)(self.data + range.begin),
    .length = (u64)(range.end - range.begin)};
}
bool String$eq(String self, String other) {
  if ((self.length != other.length)) {
    return false;
  }
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = self.length};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 idx = $next.Some.$0;
      {
        if (((*(String$subscript(&self, idx))) != (*(String$subscript(&other, idx))))) {
          return false;
        }
      }
    }
  }
  return true;
}
bool String$neq(String self, String other) {
  return (!String$eq(self, other));
}
Iter$7 String$iter(String* self) {
  return (Iter$7){
    .ptr = (u8*)self->data,
    .end = (u8*)(self->data + self->length)};
}
Iter$7 String$iter_mut(String* self) {
  return (Iter$7){
    .ptr = (u8*)self->data,
    .end = (u8*)(self->data + self->length)};
}
Slice$5 String$as_char_slice(String self) {
  return (Slice$5){
    .data = (u32*)self.data,
    .length = (u64)(self.length / 4)};
}
Slice$7 String$as_byte_slice(String self) {
  return (Slice$7){
    .data = (u8*)self.data,
    .length = (u64)self.length};
}
u64 String$hash(String self) {
  return str$hash(String$as_str(self));
}
typedef struct Field Field;
typedef struct List$199 {
  Field* data;
  u64 length;
  u64 capacity;
  dyn$Allocator allocator;
} List$199;
typedef struct List$41 {
  Type** data;
  u64 length;
  u64 capacity;
  dyn$Allocator allocator;
} List$41;
typedef struct $tuple24$29 $tuple24$29;
typedef struct List$336 {
  $tuple24$29* data;
  u64 length;
  u64 capacity;
  dyn$Allocator allocator;
} List$336;
typedef struct Iter$199 {
  Field* ptr;
  Field* end;
} Iter$199;
typedef struct Field {
  str name;
  Type* type;
  u64 size;
  u64 offset;
  s64 enum_value;
} Field;
typedef struct Type {
  s32 id;
  str name;
  u64 size;
  u64 flags;
  List$199 fields;
  Type* element_type;
  List$41 generic_args;
  List$41 traits;
  List$336 methods;
} Type;
typedef struct List$199 List$199;
static inline Iter$199 List$199$iter(List$199* self) {
  return (Iter$199){
    .ptr = (Field*)self->data,
    .end = (Field*)(Field*)(self->data + self->length)};
}
typedef struct $tuple199 {
  Field $0;
} $tuple199;
typedef struct Option$199 Option$199;
typedef $tuple199 Option$199$Some;
typedef struct Option$199 {
  int index;
  union {
    Option$199$Some Some;
  };
} Option$199;
typedef struct Iter$199 Iter$199;
static inline Option$199 Iter$199$next(Iter$199* self) {
  if ((self->ptr >= self->end)) {
    return  (Option$199) { .index = 0};
  }
  Option$199 value  = (Option$199) {
.index = 1,
.Some = ($tuple199) {.$0 = (*self->ptr)}};
  self->ptr++;
  return value;
}
u64 Type$offset(Type* self, str member) {
  {
    List$199 $iterable = self->fields;
    Iter$199 $iterator = List$199$iter(&$iterable);
    while (1) {
      auto $next = Iter$199$next(&$iterator);
      if ($next.index == 0) break;
      Field field = $next.Some.$0;
      {
        if ((str$eq(field.name, member))) {
          return field.offset;
        }
      }
    }
  }
  return (u64)(-1);
}
typedef enum {
  TypeFlags$INTEGER = 1,
  TypeFlags$FLOAT = 2,
  TypeFlags$BOOL = 4,
  TypeFlags$STRING = 8,
  TypeFlags$STRUCT = 16,
  TypeFlags$CHOICE = 32,
  TypeFlags$ENUM = 64,
  TypeFlags$TUPLE = 128,
  TypeFlags$ARRAY = 256,
  TypeFlags$FUNCTION = 512,
  TypeFlags$POINTER = 1024,
  TypeFlags$SIGNED = 2048,
  TypeFlags$UNSIGNED = 4096,
  TypeFlags$TRAIT = 8192,
  TypeFlags$UNION = 16384
} TypeFlags;
bool Type$is_trait(Type* self) {
  return ((self->flags & TypeFlags$TRAIT) != 0);
}
bool Type$is_signed(Type* self) {
  return ((self->flags & TypeFlags$SIGNED) != 0);
}
bool Type$is_unsigned(Type* self) {
  return ((self->flags & TypeFlags$UNSIGNED) != 0);
}
bool Type$has_no_extension(Type* self) {
  return (((self->flags & TypeFlags$POINTER) == 0) && ((self->flags & TypeFlags$ARRAY) == 0));
}
bool Type$is_array(Type* self) {
  return ((self->flags & TypeFlags$ARRAY) != 0);
}
bool Type$is_string(Type* self) {
  return ((self->flags & TypeFlags$STRING) != 0);
}
bool Type$is_scalar(Type* self) {
  return (Type$has_no_extension(self) && (((((self->flags & TypeFlags$INTEGER) != 0) || ((self->flags & TypeFlags$FLOAT) != 0)) || ((self->flags & TypeFlags$BOOL) != 0)) || ((self->flags & TypeFlags$STRING) != 0)));
}
bool Type$is_struct(Type* self) {
  return ((self->flags & TypeFlags$STRUCT) != 0);
}
bool Type$is_enum(Type* self) {
  return ((self->flags & TypeFlags$ENUM) != 0);
}
bool Type$is_integral(Type* self) {
  return ((self->flags & TypeFlags$INTEGER) != 0);
}
bool Type$is_float(Type* self) {
  return ((self->flags & TypeFlags$FLOAT) != 0);
}
bool Type$is_bool(Type* self) {
  return ((self->flags & TypeFlags$BOOL) != 0);
}
bool Type$is_tuple(Type* self) {
  return ((self->flags & TypeFlags$TUPLE) != 0);
}
bool Type$is_function(Type* self) {
  return ((self->flags & TypeFlags$FUNCTION) != 0);
}
bool Type$is_pointer(Type* self) {
  return ((self->flags & TypeFlags$POINTER) != 0);
}
bool Type$is_choice(Type* self) {
  return ((self->flags & TypeFlags$CHOICE) != 0);
}
bool Type$is_union(Type* self) {
  return ((self->flags & TypeFlags$UNION) != 0);
}
typedef struct any {
  void* ptr;
  Type* type;
} any;
typedef struct $tuple40 {
  any $0;
} $tuple40;
typedef struct Option$40 Option$40;
typedef $tuple40 Option$40$Some;
typedef struct Option$40 {
  int index;
  union {
    Option$40$Some Some;
  };
} Option$40;
typedef struct ElementIter ElementIter;
typedef struct ElementIter {
  u8* ptr;
  u8* end;
  Type* element_type;
} ElementIter;
static inline Option$40 None$410() {
  return  (Option$40) { .index = 0};
}
static inline Option$40 Some$40(any t) {
  return (Option$40) {
.index = 1,
.Some = ($tuple40) {.$0 = t}};
}
Option$40 ElementIter$next(ElementIter* self) {
  if ((self->ptr >= self->end)) {
    return None$410();
  }
  Option$40 value  = Some$40((any){
    .ptr = (void*)self->ptr,
    .type = (Type*)self->element_type});
  (self->ptr += self->element_type->size);
  return value;
}
bool str$starts_with$24(str self, str prefix) {
Slice$7 $deconstruction$1 = str$as_byte_slice(prefix);
auto data = $deconstruction$1.data;
auto length = $deconstruction$1.length;
  if ((self.length < length)) {
    return false;
  }
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = length};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 i = $next.Some.$0;
      {
        if ((self.data[i] != data[i])) {
          return false;
        }
      }
    }
  }
  return true;
}
bool $lambda$1 (Field* field)
{
  return str$eq(field->name, (str) { .data = "length", .length = 6 });
}

Field* List$199$find_first_of(List$199 self, bool(*finder)(Field*)) {
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = self.length};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 idx = $next.Some.$0;
      {
        if (finder((&self.data[idx]))) {
          return (&self.data[idx]);
        }
      }
    }
  }
  return NULL;
}
bool $lambda$3 (Field* field)
{
  return str$eq(field->name, (str) { .data = "data", .length = 4 });
}

u8* Field$get$7(Field* self, u8* source) {
  return ((u8*)source + self->offset);
}
u8* Field$get$7(Field* self, u8* source);
typedef struct List$41 List$41;
Type** List$41$subscript(List$41* self, u64 idx) {
  return (&self->data[idx]);
}
ElementIter Type$elements(Type self, u8* ptr) {
  if (str$starts_with$24(self.name, (str) { .data = "List!<", .length = 6 })) {
    Field* length_field  = List$199$find_first_of(self.fields, $lambda$1);
    Field* data_field  = List$199$find_first_of(self.fields, $lambda$3);
    u64 length  = (*(u64*)Field$get$7(length_field, ptr));
    u8* data  = (*(u8**)Field$get$7(data_field, ptr));
    Type* element_type  = (*(List$41$subscript(&self.generic_args, 0)));
    u8* end  = (data + (length * element_type->size));
    return (ElementIter){
      .ptr = (u8*)(u8*)data,
      .end = (u8*)end,
      .element_type = (Type*)element_type};
  }
  return (ElementIter){
    .ptr = (u8*)(u8*)ptr,
    .end = (u8*)(ptr + self.size),
    .element_type = (Type*)self.element_type};
}
typedef struct TupleElementIter TupleElementIter;
typedef struct TupleElementIter {
  u8* ptr;
  u64 element_index;
  Type* tuple_type;
} TupleElementIter;
Option$40 None$410();
Field* List$199$subscript(List$199* self, u64 idx) {
  return (&self->data[idx]);
}
Option$40 Some$40(any t);
Option$40 TupleElementIter$next(TupleElementIter* self) {
  if ((self->element_index >= self->tuple_type->fields.length)) {
    return None$410();
  }
  Field field  = (*(List$199$subscript(&self->tuple_type->fields, self->element_index)));
  Type* element_type  = field.type;
  u64 offset  = Type$offset(self->tuple_type, field.name);
  any value  = (any){
    .ptr = (void*)(self->ptr + offset),
    .type = (Type*)element_type};
  self->element_index++;
  return Some$40(value);
}
TupleElementIter Type$tuple_elements(Type* self, u8* instance) {
  return (TupleElementIter){
    .ptr = (u8*)instance,
    .element_index = (u64)0,
    .tuple_type = (Type*)self};
}
typedef struct Iter$41 {
  Type** ptr;
  Type** end;
} Iter$41;
static inline Iter$41 List$41$iter(List$41* self) {
  return (Iter$41){
    .ptr = (Type**)self->data,
    .end = (Type**)(Type**)(self->data + self->length)};
}
typedef struct $tuple41 {
  Type* $0;
} $tuple41;
typedef struct Option$41 Option$41;
typedef $tuple41 Option$41$Some;
typedef struct Option$41 {
  int index;
  union {
    Option$41$Some Some;
  };
} Option$41;
typedef struct Iter$41 Iter$41;
static inline Option$41 Iter$41$next(Iter$41* self) {
  if ((self->ptr >= self->end)) {
    return  (Option$41) { .index = 0};
  }
  Option$41 value  = (Option$41) {
.index = 1,
.Some = ($tuple41) {.$0 = (*self->ptr)}};
  self->ptr++;
  return value;
}
bool Type$implements(Type* self, str trait_name) {
  {
    List$41 $iterable = self->traits;
    Iter$41 $iterator = List$41$iter(&$iterable);
    while (1) {
      auto $next = Iter$41$next(&$iterator);
      if ($next.index == 0) break;
      Type* _trait = $next.Some.$0;
      {
        if ((str$eq(_trait->name, trait_name))) {
          return true;
        }
      }
    }
  }
  return false;
}
u64 HASH_INITIAL_VALUE  = 0xCBF29CE484222325;
u64 HASH_FACTOR  = 0x100000001B3;
u64 s8$hash(s8 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 s16$hash(s16 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 s32$hash(s32 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 s64$hash(s64 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 u8$hash(u8 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 u16$hash(u16 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 u32$hash(u32 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 u64$hash(u64 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 f32$hash(f32 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= (u32)self);
  (hash *= HASH_FACTOR);
  return hash;
}
u64 f64$hash(f64 self) {
  u64 hash  = HASH_INITIAL_VALUE;
  (hash ^= (u32)self);
  (hash *= HASH_FACTOR);
  return hash;
}
void(*panic_handler)(str)= {0};
void set_panic_handler(void(*handler)(str)) {
  (panic_handler = handler);
}
void $lambda$4 (str msg)
{
  s32* n  = NULL;
  ((*n) = 10);
}

void panic(str msg) {
  if ((!panic_handler)) {
    (panic_handler = $lambda$4);
  }
  panic_handler(msg);
}
Slice$24 Env$args() {
  return Env$current()->args_slice;
}
Slice$5 u32$as_char_slice(u32 self) {
  static u32 chars[2] = {0x0, 0x0};
  (chars[0] = self);
  return (Slice$5){
    .data = (u32*)chars,
    .length = (u64)1};
}
Slice$7 u32$as_byte_slice(u32 self) {
  static u8 chars[2] = {0x0, 0x0};
  (chars[0] = self);
  return (Slice$7){
    .data = (u8*)chars,
    .length = (u64)1};
}
extern void  print(u8*);
extern void  exit(s32);
void memset(u8* p, u64 value, u64 len) {
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = len};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 i = $next.Some.$0;
      {
        (p[i] = value);
      }
    }
  }
}
void memcpy(u8** dest, u8* src, u64 len) {
  {
    RangeBase$1 $iterable = (RangeBase$1) {.begin = 0, .end = len};
    RangeIter$1 $iterator = RangeBase$1$iter(&$iterable);
    while (1) {
      auto $next = RangeIter$1$next(&$iterator);
      if ($next.index == 0) break;
      s32 i = $next.Some.$0;
      {
        ((*dest)[i] = src[i]);
      }
    }
  }
}
s32 strlen(u8* s) {
  s32 i = {0};
  while ((s[i] != 0x0)) {
    i++;
  }
  return i;
}
typedef struct LinearAlloc {
  u8 mem[24576];
  u64 ptr;
} LinearAlloc;
static constexpr s32 LINEAR_ALLOCATOR_SIZE  = (1024 * 24);
LinearAlloc LinearAlloc$new() {
  LinearAlloc self  = {};
  memset((u8*)self.mem,0,LINEAR_ALLOCATOR_SIZE);
  return self;
}
typedef struct LinearAlloc LinearAlloc;
void* LinearAlloc$alloc(LinearAlloc* self, u64 size) {
  u8* data  = (&self->mem[self->ptr]);
  (self->ptr += size);
  if ((self->ptr > LINEAR_ALLOCATOR_SIZE)) {
    print("\e[031mLinear allocator ran out of memory \e[033m(expected for demo)\n");
    exit(1);
  }
  return data;
}
typedef struct Vector2 Vector2;
typedef struct Vector2 {
  f32 x;
  f32 y;
} Vector2;
extern  void main() {
  LinearAlloc allocator  = LinearAlloc$new();
  print("\e[1;4;32mHello (free-standing) World!\e[0m\n");
  while (true) {
    Vector2* v  = (Vector2*)LinearAlloc$alloc(&allocator, sizeof(Vector2));
    Vector2* v1  = (Vector2*)LinearAlloc$alloc(&allocator, sizeof(Vector2));
    memcpy((u8**)(&v1),(u8*)v,sizeof(Vector2));
  }
}
List$41 _type_info  = {};

#ifdef TESTING
  #define __TEST_RUNNER_MAIN\
    for (int i = 0; i < sizeof(tests) / sizeof($ela_test); i++) { $ela_test_run(&tests[i]); }
#else 
  #define __TEST_RUNNER_MAIN\
    __ela_main_();
#endif
  