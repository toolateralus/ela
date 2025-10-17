#pragma once
#include <cstdint>
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "value.hpp"

enum Op_Code : uint8_t {
  OP_NOOP, // no operands or destination.

  /* Arithmetic */
  // dest=result, left=left, right=right :D. unary only stores left operand.
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_AND,
  OP_OR,
  OP_XOR,
  OP_SHL,
  OP_SHR,
  OP_NOT,
  OP_LOGICAL_AND,
  OP_LOGICAL_OR,
  OP_LOGICAL_NOT,
  OP_EQ,
  OP_NE,
  OP_LT,
  OP_LE,
  OP_GT,
  OP_GE,
  OP_NEG,

  /* Memory */
  OP_LOAD,          // dest=result, left=addr (tag determines const/imm/register)
  OP_STORE,         // left=addr,   right=value
  OP_ALLOCA,        // dest=result, left=size_in_bytes

  // see Byte_Code for info on bb.
  OP_JMP,            // left=bb
  OP_JMP_TRUE,       // left=bb, right=condition
  OP_JMP_FALSE,      // left=bb, right=condition

  OP_PUSH_ARG,       // push argument to stack.   left=value
  OP_CALL,           // dest=result, left=fn_idx, right=n_args

  OP_RET,            // left=value
  OP_RET_VOID,       // no operands.

  /* Type casting */
  OP_CAST,           // dest=result, left=value, right=type_index
  OP_BITCAST,        // dest=result, left=value, right=type_index
  // get element pointer, calculate address of struct field or element in array
  OP_GEP,            // dest=ptr,    left=base,  right=index
};

struct Basic_Block;
// This is more so an TMIR, (typed mid level intermediate representation)
// since we still carry type information for all operands and such for
// the ability to serialize effectively
struct Byte_Code {
  Op_Code opcode = OP_NOOP;
  uint32_t dest = 0, left = 0, right = 0;
  Type *dest_t, left_t, right_t;
  enum : uint8_t {
    // instructions use bb_dest for jumps, otherwise this means the unions is irrelevant.
    BC_INSTRUCTION,
    BC_CONSTANT_VALUE,
    BC_IMMEDIATE_VALUE,
  } tag : 2 = BC_INSTRUCTION;

  union {
    Value *constant;
    Value *immediate;
    Basic_Block *bb_dest;
  };

#ifdef DEBUG
  Span span;
#endif
};

struct Basic_Block {
  InternedString label;
  std::vector<Byte_Code> code;
  Basic_Block *next;
};

struct Global_Variable {
  Span span;
  InternedString name;
  Type *type;
  Value *value;
};

struct Function {
  InternedString name;
  Span span;
  std::vector<Basic_Block *> basic_blocks;

  uint32_t temporary_count = 0;
  uint32_t parameter_count = 0;

  FunctionTypeInfo *type_info;

  enum Flags : uint8_t {
    FUNCTION_FLAGS_NONE = 0,
    FUNCTION_FLAGS_IS_INLINE = 1 << 0,
    FUNCTION_FLAGS_IS_VAR_ARGS = 1 << 1,
    FUNCTION_FLAGS_IS_EXTERN = 1 << 2,
    FUNCTION_FLAGS_IS_ENTRY_POINT = 1 << 3,
    FUNCTION_FLAGS_IS_EXPORTED = 1 << 4,
    FUNCTION_FLAGS_IS_TEST = 1 << 5,
  };

  // store the flags as an int so C++ doesn't do that type casting nonsense
  // where FLAG | FLAG == int and you have to cast everything.
  // It's so stupid.
  uint8_t flags = FUNCTION_FLAGS_NONE;

  inline uint32_t create_temporary() { return temporary_count++; }
  inline Basic_Block *entry_block() const { return basic_blocks[0]; }
};

struct Module {
  std::vector<Function> functions;
  std::vector<Global_Variable> globals;
  std::unordered_map<InternedString, Function *> function_table;

  Function *get_function(InternedString name);
  Function *create_function(InternedString name, Type *ret_type);
};