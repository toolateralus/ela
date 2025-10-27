#pragma once
#include <llvm/Support/CommandLine.h>
#include <cstdint>
#include "error.hpp"
/*
64-bit C ABI summary

x86_64 System V (Linux/macOS/BSD)
  struct return: ≤16B direct, >16B sret
  struct args: ≤16B direct, >16B byval / scalarize / packed scalarization

x86_64 Microsoft (Windows)
  struct return: ≤8B direct, >8B sret
  struct args: ≤8B direct, >8B byval

direct_limit: SysV=16B, Win64=8B
use sret/byval for large or mixed aggregates

We do not currently plan to support ARM for now,
until these abi's are well defined and handled.

Also, this system will be extensible, so you can submit a pull request with
a new calling convention, or possibly in the future, attach a calling conv as a compiler
plugin written in Ela.
*/

// I define this as an enum so cross compilation is possible, without recompiling the compiler,
// as long as both targets are 64-bit, or 32-bit.
static enum {
  PLATFORM_WINDOWS,
  // linux, macos, freebsd, pretty much everything besides windows.
  PLATFORM_UNIX,
} platform =
#if defined(__linux)
    PLATFORM_UNIX;
#elif defined(_WIN32)
    PLATFORM_WINDOWS;
#endif

#include "type.hpp"

/* *
Destructure an aggregate to 8 byte register-values,
example:
  struct Struct {
    a: s64,
    b: s64
  };

  function(Struct) -> function(s64, s64)


  If we have more than a few members, or we could pack members into larger integers,
  take some amount of sub-8 byte integers, and pack them to fit in a register.
  this is a form of scalarization but we classify it differently so we
  can more easily lower it.

  example:
  struct Struct {
    a: u8,
    b: u8,
    c: u8
    d: u8,
    e: u8,
    f: u8,
    g: u8,
    h: u8,
  }

  function(Struct) -> function(u64);

  struct Struct {
    a: s16,
    b: s16,
    d: s16,
    e: s16
  }

  function(Struct) -> function(s64);

  struct Struct {
    a: s16,
    b: s16,
    c: u32
  }

  // Note: the signedness doesn't matter here, because
  // we reinterpret this packed int in the function to whatever
  // correct sign we were expecting, and signedness has no impact on
  // the data layout, rather it impacts operations done on a value.
  function(Struct) -> function(s64);

  struct {
    a: s32,
    u: u32
  }

  function(Struct) -> function(s64);

  Floats do not get packed in the SysV 64 C ABI!
  However, if we had:

  struct {
    f: f32,
    a: u8,
    b: u16,
    c: u8
  }

  because we still scalarize non-homogenous aggregates which only contain scalars, we would get:

  function(Struct) -> function(f32, u32);
*/

struct Scalarization_Source_Element {
  // The type we had, and decided needed to be packed into the group.
  // This is only applicable when packing integers.
  const Type *original_type;
  // The offset from the beginning of the group in bits.
  size_t bit_offset;
  size_t member_index;
};

// This is equivalent to a single register, or parameter in our MIR function,
// that was the target of either scalarizing an aggregate of integers that were packed into one
// "group" or register, or just some float or f64 that was extracted from an aggregate or whatever.
struct Scalarization_Group {
  // The index of this pack group within the descriptor. TODO: determine if this is neccesary.
  size_t index;
  // There is no guarantee that the result of an integer pack, is 64 bit!
  // we take up as little space as possible, with no padding.
  const Type *type;
  // The types & offsets of the aggregates subtypes that were combined to create this group.
  std::vector<Scalarization_Source_Element> source_elements;
};

// Sometimes, packing or scalarizing an aggregate can result in several packed 64 bit integers.
// or just multiple scalarized values.
// So, we actually need a vector to describe the resulting packed or scalarized list of types and
// their offset to understand how the ABI needs us to pass this aggregate as a deconstructed
// set of integers or floats.
struct Scalarization_Descriptor {
  constexpr static const size_t NUM_GROUPS = 2;
  std::array<Scalarization_Group, NUM_GROUPS> groups;
  size_t length = 0;
  constexpr inline void insert(const size_t &index, const Scalarization_Group &group) {
    groups[index] = group;
    length = index + 1;
  }
  constexpr inline void remove(const size_t &index) { length = index + 1; }
  constexpr size_t size() const { return NUM_GROUPS; }
};

struct Argument_Convention {
  bool pass_via_memory = false;  // pass pointer-to, i.e byval
  // if scalarization_descriptor.length >= 0, do scalarize.
  Scalarization_Descriptor scalarization_descriptor;
};

struct Return_Convention {
  bool indirect = false;  // sret
  // if scalarization_descriptor.length >= 0, do scalarize.
  Scalarization_Descriptor scalarization_descriptor;
};

struct Call_Conv {
  virtual ~Call_Conv() {}

  static inline bool is_aggregate_of_scalarizable_types(const Type *t) {
    size_t scalarizable_members{};
    for (const auto &[_name, type, _value, _thir_value] : t->info->members) {
      if (type->has_extensions() || type->kind != TYPE_SCALAR) {
        continue;
      }
      const ScalarTypeInfo *info = type->info->as<ScalarTypeInfo>();
      scalarizable_members += info->is_float() || info->is_integer;
    }
    return scalarizable_members == t->info->members.size();
  }

  // This works the same on SysV and ARM (in a practical sense) so it's a default implemented base method,
  // but you'd probably want to override this if you end up using it for anything but SysV
  virtual bool type_needs_scalarization(const Type *t, uint8_t limit_in_bits) const {
    if (t->has_extensions() || (t->kind == TYPE_SCALAR && t->size_in_bits() < 64)) {
      return false;
    }

    if (t->size_in_bits() > limit_in_bits) {
      return false;
    }

    if (is_aggregate_of_scalarizable_types(t)) {
      // if this contains several members, or is > 64 bit (u128, etc)
      // we need to pack it into a larger integer type,
      // so we can use as few registers as possible.
      // Also, floats get this optimization applied.
      if (t->info->members.size() > 1 || t->size_in_bits() > 64) {
        // SysV 64 | ARM 64 specific? Do not use this function for windows CC.
        return true;
      }
    }

    return false;
  }

  virtual Argument_Convention get_argument(const Type *t) const = 0;
  virtual Return_Convention get_return(const Type *t) const = 0;

  virtual uint8_t arg_size_limit_bits() const = 0;
  virtual uint8_t return_size_limit_bits() const = 0;
};

struct SysV64_C : Call_Conv {
  void scalarize_aggregate(const Type *t, Scalarization_Descriptor &descriptor) const;

  Argument_Convention get_argument(const Type *t) const override;
  Return_Convention get_return(const Type *t) const override;

  inline uint8_t arg_size_limit_bits() const override { return 128; }
  inline uint8_t return_size_limit_bits() const override { return 128; }
};

struct Win64_C : Call_Conv {
  Argument_Convention get_argument(const Type *t) const override;
  Return_Convention get_return(const Type *t) const override;

  inline uint8_t arg_size_limit_bits() const override { return 64; }
  inline uint8_t return_size_limit_bits() const override { return 64; }
};
