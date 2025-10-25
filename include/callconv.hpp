#pragma once
#include <cstdint>

/*
64-bit C ABI summary

x86_64 System V (Linux/macOS/BSD)
  struct return: ≤16B direct, >16B sret
  struct args: ≤16B direct, >16B byval

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

struct Argument_Convention {
  bool scalarize = false;
  bool pass_via_memory = false;  // pass pointer-to
  std::vector<Type *> scalarization_targets;
};

struct Return_Convention {
  bool scalarize = false;
  bool indirect = false;  // sret
  std::vector<Type *> scalarization_targets;
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

  // This works the same on SysV and ARM (in a practical sense) so it's a base virtual,
  // but you'd probably want to override this if you end up using it for anything but SysV
  virtual bool type_is_scalarizable(const Type *t, uint8_t limit) const {
    if (t->size_in_bits() / 8 > limit) {
      return false;
    }

    if (type_is_homogenous_aggregate_of_floating_point_types(t)) {
      return true;
    }

    if (type_is_homogenous_aggregate_of_integer_types(t)) {
      return true;
    }

    return false;
  }

  virtual Argument_Convention get_argument_convention(const Type *t) const = 0;
  virtual Return_Convention get_return_convention(const Type *t) const = 0;

  virtual uint8_t direct_return_size_in_bytes_limit() const = 0;
  virtual uint8_t direct_arg_size_in_bytes_limit() const = 0;

  // Return convention is returned so the MIR generator can handle scalarizing return values when neccesary.
  virtual Type *get_abi_compliant_function_signature(const FunctionTypeInfo *info, Return_Convention &rc) const = 0;
};

struct SysV64_C_Calling_Convention : Calling_Convention {
  Argument_Convention get_argument_convention(const Type *t) const override {
    Argument_Convention cc;

    // Large structs → pass via memory (byval)
    if (t->size_in_bits() / 8 > direct_arg_size_in_bytes_limit()) {
      cc.pass_via_memory = true;
      return cc;
    }

    // Small homogeneous structs → scalarize
    if (type_is_scalarizable(t, direct_arg_size_in_bytes_limit())) {
      cc.scalarize = true;

      // Split the struct into scalar types for registers
      for (const auto &[_name, member_type, _value, _thir_value] : t->info->members) {
        if (!member_type->has_extensions() && member_type->kind == TYPE_SCALAR) {
          cc.scalarization_targets.push_back(member_type);
        }
      }

      return cc;
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
          cc.scalarization_targets.push_back(member_type);
        }
      }
    }

    // Small heterogeneous structs → return directly in a single register
    return cc;
  }

  Type *get_abi_compliant_function_signature(const FunctionTypeInfo *info, Return_Convention &rc) const override {
    Type *ret_ty = info->return_type;
    std::vector<Type *> parameter_types(info->parameter_types, info->parameter_types + info->params_len);

    std::vector<Type *> new_params;
    for (auto *param : parameter_types) {
      Argument_Convention ac = get_argument_convention(param);

      if (ac.pass_via_memory) {
        new_params.push_back(param->take_pointer_to());
      } else if (ac.scalarize) {
        new_params.insert(new_params.end(), ac.scalarization_targets.begin(), ac.scalarization_targets.end());
      } else {
        new_params.push_back(param);
      }
    }

    rc = get_return_convention(ret_ty);
    if (rc.indirect) {
      ret_ty = void_type();
      new_params.insert(new_params.begin(), info->return_type->take_pointer_to());
    }

    FunctionTypeInfo new_type_info;
    new_type_info.return_type = ret_ty;
    std::memcpy(new_type_info.parameter_types, new_params.data(), new_params.size() * sizeof(Type *));
    return global_find_function_type_id(new_type_info, {});
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
