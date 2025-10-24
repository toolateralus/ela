#pragma once

#include <cassert>
#include <cstdint>
#include <span>
#include <vector>
#include "mir.hpp"

#undef alloca

namespace mir::VM {

struct Stack_Frame {
  // All values stored as a blob of words, always 64 bit.
  // This simplifies how we reason about pointers, especially those
  // that come from external sources like an FFI malloc/realloc call.

  // It does add some cost to actually reading and writing values.
  mutable std::vector<uint64_t> stack{};
  std::vector<Basic_Block *> basic_blocks;

  // basic_blocks[bb_p].code[ip] == current instruction
  uint64_t ip, bb_p;
  uint64_t ret_addr;

  // these have to allocate because it could be a temp or an immediate value,
  // so we have no choice but to do some RAII backing temporary memory.
  // Maybe we can find a way to bypass this in a safe way
  // which would definitely be an optimization
  using Arg = std::vector<uint64_t>;

  std::vector<Arg> argument_stack;

  std::unordered_map<const Basic_Block *, uint32_t> basic_block_table{};
  std::unordered_map<uint32_t, uint64_t> temp_to_stack_address_table;

  // each function encodes how many bytes it needs to allocate in the MIR,
  // so we pass this in here each time we create a function frame.s
  inline explicit Stack_Frame(mir::Function *f) : basic_blocks(f->basic_blocks) {
    assert(f->stack_size_needed_in_bytes % 8 == 0 && "stack size must be 8-byte aligned");
    stack = std::vector<uint64_t>(f->stack_size_needed_in_bytes / 8);

    // Build a hashmap so we dont have to do linear searches for jumps.
    uint32_t idx{};
    for (const auto &bb : basic_blocks) {
      basic_block_table[bb] = idx;
    }

    uint64_t word_offset{};
    for (const auto &temp : f->temps) {
      temp_to_stack_address_table[temp.index] = word_offset;
      const size_t size = temp.type->size_in_bytes();
      if (size < 8 || size % 8 != 0) {
        word_offset += 8;
      } else {
        word_offset += size;
      }
    }
  }

  inline std::span<uint64_t> read(size_t address, size_t size_in_words) const { return {stack.data() + address, size_in_words}; }

  inline uint64_t read(size_t address) const { return stack[address]; }

  inline void write(size_t address, std::span<const uint64_t> data) {
    assert(address + data.size() <= stack.size());
    std::memcpy(stack.data() + address, data.data(), data.size() * sizeof(uint64_t));
  }

  inline void write(size_t address, uint64_t w) { stack[address] = w; }

  // basically a memcpy from one stack frame to another, we use this to hoist return values up to
  // calling functions when we return some alloca'd memory etc.
  inline void hoist(size_t base, size_t size_in_words, size_t dest_base, Stack_Frame &dest) const {
    std::memcpy(dest.stack.data() + dest_base, stack.data() + base, size_in_words * sizeof(uint64_t));
  }

  template <typename T>
  T reinterpret(size_t address) const {
    static size_t size_in_words = sizeof(T) / 8;
    if constexpr (sizeof(T) < 8 || sizeof(T) % 8 != 0) {
      size_in_words = 8;
    }

    std::span<const uint64_t> span = read(address, size_in_words);

    T value;
    std::memcpy(&value, span.data(), sizeof(T));  // avoids aliasing UB
    return value;
  }

  inline const Instruction &fetch() const {
    assert(bb_p < basic_blocks.size());
    const Basic_Block *bb = basic_blocks[bb_p];
    assert(ip < bb->code.size());
    return bb->code[ip];
  }

  inline void jump(const Basic_Block *to) {
    bb_p = basic_block_table[to];
    ip = 0;  // always jump to the zeroth instruction in a basic block.
  }

  inline size_t get_address_of_temp(uint32_t temp) const { return temp_to_stack_address_table.at(temp); }

  inline size_t size_in_words(const Type *type) const { return bytes_to_words(type->size_in_bytes()); }

  inline size_t bytes_to_words(size_t size) const {
    if (size < 8) {
      size = 8;
    }
    if (size % 8 != 0) {
      size = 8;
    }
    return std::max<size_t>(size / 8, 1);
  }
};

struct Context {
  std::vector<Stack_Frame> call_stack;
};

void interpret(Context &c, mir::Module &m, uint32_t entry_point = 0);

}  // namespace mir::VM