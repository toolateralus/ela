#include "callconv.hpp"
#include "type.hpp"
void SysV64_C::scalarize_aggregate(const Type *t, Scalarization_Descriptor &descriptor) const {
  size_t group_index = 0;
  Scalarization_Group current_group{};
  current_group.index = group_index;
  size_t group_bits = 0;

  printf("[scalarize_aggregate] Starting scalarization of type: %s\n", t->to_string().c_str());

  for (size_t i = 0; i < t->info->members.size(); ++i) {
    const Type *member_type = t->info->members[i].type;
    size_t member_bits = member_type->size_in_bits();
    size_t member_offset = 0;

    bool is_float = member_type->info->as<ScalarTypeInfo>()->is_float();
    printf("  Member %zu: %s, %zu bits (%s)\n", i, t->info->members[i].name.c_str(), member_bits, is_float ? "float" : "integer");

    if (is_float) {
      // finalize any existing integer group
      if (!current_group.source_elements.empty()) {
        current_group.type = find_or_create_arbitrary_integer_type(false, group_bits);
        descriptor.insert(group_index, current_group);
        printf("    Finalizing integer group %zu: %zu bits\n", group_index, group_bits);
        group_index++;
        current_group = {};
        current_group.index = group_index;
        group_bits = 0;
      }

      // floats always get their own group
      Scalarization_Group float_group{};
      float_group.index = group_index;
      float_group.type = member_type;
      float_group.source_elements.push_back({member_type, 0, i});
      descriptor.insert(group_index, float_group);
      printf("    Added float group %zu for member %s\n", group_index, t->info->members[i].name.c_str());
      group_index++;
      continue;
    }

    // integer member handling
    while (member_bits > 0) {
      size_t space_in_group = 64 - group_bits;
      size_t bits_to_take = std::min(member_bits, space_in_group);

      if (space_in_group == 0) {
        // finalize current full group
        current_group.type = find_or_create_arbitrary_integer_type(false, group_bits);
        descriptor.insert(group_index, current_group);
        printf("    Finalized full integer group %zu: %zu bits\n", group_index, group_bits);
        group_index++;
        if (group_index > 2) {
          throw_errorf("[CALL_CONV]: integer aggregate of %zu bits requires >2 groups (SysV64 limit)", t->size_in_bits());
        }
        current_group = {};
        current_group.index = group_index;
        group_bits = 0;
        space_in_group = 64;
        bits_to_take = std::min(member_bits, space_in_group);
      }

      // split member into current group
      current_group.source_elements.push_back({member_type, member_offset, i});
      printf("      Packing %zu bits of member %s into group %zu at offset %zu\n", bits_to_take, t->info->members[i].name.c_str(),
             group_index, member_offset);

      member_offset += bits_to_take;
      member_bits -= bits_to_take;
      group_bits += bits_to_take;

      // finalize group if exactly full
      if (group_bits == 64) {
        current_group.type = u64_type();
        descriptor.insert(group_index, current_group);
        printf("    Finalized full 64-bit integer group %zu\n", group_index);
        group_index++;
        if (group_index > 2) {
          throw_errorf("[CALL_CONV]: integer aggregate of %zu bits requires >2 groups (SysV64 limit)", t->size_in_bits());
        }
        current_group = {};
        current_group.index = group_index;
        group_bits = 0;
      }
    }
  }

  // finalize leftover integer group
  if (!current_group.source_elements.empty()) {
    current_group.type = find_or_create_arbitrary_integer_type(false, group_bits);
    descriptor.insert(group_index, current_group);
    printf("  Finalized leftover group %zu: %zu bits\n", group_index, group_bits);
    group_index++;
  }

  // clear remaining unused descriptor slots
  for (; group_index < descriptor.size(); ++group_index) {
    descriptor.remove(group_index);
    printf("  Cleared unused descriptor slot %zu\n", group_index);
  }

  printf("[scalarize_aggregate] Completed scalarization for type: %s\n", t->to_string().c_str());
}

Argument_Convention SysV64_C::get_argument(const Type *t) const {
  Argument_Convention cc{.pass_via_memory = false, .scalarization_descriptor = {}};

  if (t->is_pointer()) {
    return cc;
  }

  // Large structs → pass via memory (byval)
  if (t->size_in_bits() > arg_size_limit_bits()) {
    cc.pass_via_memory = true;
    return cc;
  }

  const bool is_scalarizable = type_needs_scalarization(t, arg_size_limit_bits());

  if (!is_scalarizable) {
    return cc;
  }

  Scalarization_Descriptor &descriptor = cc.scalarization_descriptor;
  scalarize_aggregate(t, descriptor);

  return cc;
}

Return_Convention SysV64_C::get_return(const Type *t) const {
  Return_Convention cc;

  if (t == void_type()) {
    return cc;
  }

  // TODO: need to scalarize packed return aggregates
  // Large or non-scalarizable structs → sret
  if (t->size_in_bits() > return_size_limit_bits()) {
    cc.indirect = true;
    return cc;
  }

  // Small heterogeneous structs → return directly in a single register
  return cc;
}
