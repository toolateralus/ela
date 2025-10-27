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

enum Scalarization_Class {
  DO_NOT_SCALARIZE,
  SCALARIZE_FLOATS,
  PACK_INTEGERS,
};

struct Pack_Source_Element {
  // The type we had, and decided needed to be packed into the group.
  const Type *original_type;
  // The offset from the beginning of the group in bits.
  size_t bit_offset;
  size_t member_index;
};

struct Pack_Group {
  // The index of this pack group within the descriptor. TODO: determine if this is neccesary.
  size_t index;
  // There is no guarantee that the resulting pack, is 64 bit!
  // we take up as much space as neccesary, with no padding.
  const Type *type;
  // The types & offsets of the aggregates subtypes that were combined to create this group.
  std::vector<Pack_Source_Element> source_elements;
};

// Sometimes, packing an aggregate can result in several packed 64 bit integers.
// So, we actually need a vector to describe the resulting packed list of types and
// their offset to understand how the ABI needs us to pass this aggregate as a deconstructed
// set of integers.
struct Integer_Packing_Descriptor {
  constexpr static const size_t NUM_GROUPS = 2;
  std::array<Pack_Group, NUM_GROUPS> groups;
  size_t length = 0;
  constexpr inline void insert(const size_t &index, const Pack_Group &group) {
    groups[index] = group;
    length = index + 1;
  }
  constexpr inline void remove(const size_t &index) { length = index + 1; }
  constexpr size_t size() const { return NUM_GROUPS; }
};

struct Argument_Convention {
  /* *
    For Scalarization_Class::SCALARIZE_FLOATS/SCALARIZE_INTEGERS/::
  Destructure an aggregate to 8 byte register-values,
  example:
    struct Struct {
      a: s64,
      b: s64
    };

    function(Struct) -> function(s64, s64)
    * For Scalarization_Class::PACKED_INTEGER::
    Floats do not get packed in the SysV 64 C ABI!

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
  */
  Scalarization_Class scalarization;
  bool pass_via_memory = false;  // pass pointer-to
  std::vector<Type *> float_scalarization_targets;
  Integer_Packing_Descriptor integer_packing_descriptor;
};

struct Return_Convention {
  bool scalarize = false;
  bool indirect = false;  // sret
  std::vector<Type *> float_scalarization_targets;
  Integer_Packing_Descriptor integer_packing_descriptor;
};

struct Calling_Convention {
  virtual ~Calling_Convention() {}

  static inline bool type_is_homogenous_aggregate_of_floating_point_types(const Type *t) {
    size_t num_floating_point_members{};
    for (const auto &[_name, type, _value, _thir_value] : t->info->members) {
      if (type->has_extensions() || type->kind != TYPE_SCALAR) {
        continue;
      }
      const ScalarTypeInfo *info = type->info->as<ScalarTypeInfo>();
      num_floating_point_members += info->is_float();
    }
    return num_floating_point_members == t->info->members.size();
  }

  static inline bool type_is_homogenous_aggregate_of_integer_types(const Type *t) {
    size_t num_integer_types{};
    for (const auto &[_name, type, _value, _thir_value] : t->info->members) {
      if (type->has_extensions() || type->kind != TYPE_SCALAR) {
        continue;
      }
      const ScalarTypeInfo *info = type->info->as<ScalarTypeInfo>();
      num_integer_types += info->is_integral;
    }
    return num_integer_types == t->info->members.size();
  }

  // This works the same on SysV and ARM (in a practical sense) so it's a default implemented base method,
  // but you'd probably want to override this if you end up using it for anything but SysV
  virtual Scalarization_Class type_is_scalarizable(const Type *t, uint8_t limit) const {
    if (t->size_in_bits() / 8 > limit) {
      return DO_NOT_SCALARIZE;
    }

    if (type_is_homogenous_aggregate_of_floating_point_types(t)) {
      return SCALARIZE_FLOATS;
    }

    if (type_is_homogenous_aggregate_of_integer_types(t)) {
      // if this contains several members, or is > 64 bit (u128, etc)
      // we need to pack it into a larger integer type,
      // so we can use as few registers as possible.
      if (t->info->members.size() > 1 || t->size_in_bits() > 64) {
        // SysV 64 | ARM 64 specific? Do not use this function for windows CC.
        return PACK_INTEGERS;
      }
      return DO_NOT_SCALARIZE;
    }

    return DO_NOT_SCALARIZE;
  }

  virtual Argument_Convention get_argument_convention(const Type *t) const = 0;
  virtual Return_Convention get_return_convention(const Type *t) const = 0;

  virtual uint8_t direct_return_size_in_bytes_limit() const = 0;
  virtual uint8_t direct_arg_size_in_bytes_limit() const = 0;
};

struct SysV64_C_Calling_Convention : Calling_Convention {
  Argument_Convention get_argument_convention(const Type *t) const override {
    Argument_Convention cc;

    // Large structs → pass via memory (byval)
    if (t->size_in_bits() / 8 > direct_arg_size_in_bytes_limit()) {
      cc.pass_via_memory = true;
      return cc;
    }

    // Small homogeneous structs of floats → scalarize into several FPU registers.
    cc.scalarization = type_is_scalarizable(t, direct_arg_size_in_bytes_limit());
    if (cc.scalarization == SCALARIZE_FLOATS) {
      // Split the struct into scalar types for registers
      for (const auto &[_name, member_type, _value, _thir_value] : t->info->members) {
        if (!member_type->has_extensions() && member_type->kind == TYPE_SCALAR) {
          cc.float_scalarization_targets.push_back(member_type);
        } else {
          std::string error = std::format(
              "[CALL_CONV]: mistakenly considered type '{}' as scalarizable, yet we found a type with "
              "extensions, or "
              "something that wasn't a scalar within it.",
              member_type->to_string());
          throw_error(error, {});
        }
      }
      return cc;
    }
    if (cc.scalarization == PACK_INTEGERS) {
      Integer_Packing_Descriptor &descriptor = cc.integer_packing_descriptor;
      size_t group_index = 0;

      Pack_Group current_group{};
      current_group.index = group_index;
      size_t group_bits = 0;

      for (size_t i = 0; i < t->info->members.size(); ++i) {
        const Type *member_type = t->info->members[i].type;
        size_t member_bits = member_type->size_in_bits();
        size_t member_offset = 0;

        while (member_bits > 0) {
          size_t space_in_group = 64 - group_bits;  // max bits per group
          size_t bits_to_take = std::min(member_bits, space_in_group);

          current_group.source_elements.push_back({member_type, member_offset});
          group_bits += bits_to_take;
          member_offset += bits_to_take;
          member_bits -= bits_to_take;

          if (group_bits == 64) {
            current_group.type = find_or_create_arbitrary_integer_type(false, group_bits);
            descriptor.insert(group_index, current_group);
            group_index++;

            if (group_index > 2) {
              throw_errorf("[CALL_CONV]: integer aggregate of {} bits requires more than 2 groups, which SysV64 forbids.",
                           t->size_in_bits());
            }

            current_group = Pack_Group{};
            current_group.index = group_index;
            group_bits = 0;
          }
        }
      }

      // finalize any remaining bits in the last group
      if (group_bits > 0) {
        current_group.type = find_or_create_arbitrary_integer_type(false, group_bits);
        descriptor.insert(group_index, current_group);
        group_index++;
      }

      // clear remaining unused descriptor slots
      for (; group_index < descriptor.size(); ++group_index) {
        descriptor.remove(group_index);
      }
    }

    // Small heterogeneous structs → pass as a single register
    return cc;
  }

  Return_Convention get_return_convention(const Type *t) const override {
    Return_Convention cc;

    // Large or non-scalarizable structs → sret
    if (t->size_in_bits() / 8 > direct_return_size_in_bytes_limit() ||
        !type_is_scalarizable(t, direct_return_size_in_bytes_limit())) {
      cc.indirect = true;
      return cc;
    }

    // Small homogeneous structs → scalarize
    if (type_is_scalarizable(t, direct_return_size_in_bytes_limit())) {
      cc.scalarize = true;

      for (const auto &[_name, member_type, _value, _thir_value] : t->info->members) {
        if (!member_type->has_extensions() && member_type->kind == TYPE_SCALAR) {
          cc.float_scalarization_targets.push_back(member_type);
        }
      }
    }

    // Small heterogeneous structs → return directly in a single register
    return cc;
  }

  uint8_t direct_return_size_in_bytes_limit() const override { return 16; }
  uint8_t direct_arg_size_in_bytes_limit() const override { return 16; }
};

struct Win64_C_Calling_Convention : Calling_Convention {
  Argument_Convention get_argument_convention(const Type *t) const override {
    Argument_Convention ac;

    if (t->size_in_bits() / 8 > direct_arg_size_in_bytes_limit()) {
      // Large structs -> pass via memory (byval)
      ac.pass_via_memory = true;
    }
    // No scalarization for Win64 structs; always either byval or single argument
    return ac;
  }

  Return_Convention get_return_convention(const Type *t) const override {
    Return_Convention rc;

    if (t->size_in_bits() / 8 > direct_return_size_in_bytes_limit()) {
      // Large return -> use sret
      rc.indirect = true;
    }
    // No scalarization for Win64; small returns just go in register
    return rc;
  }

  uint8_t direct_return_size_in_bytes_limit() const override { return 8; }
  uint8_t direct_arg_size_in_bytes_limit() const override { return 8; }
};
