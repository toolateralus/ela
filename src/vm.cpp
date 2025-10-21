#include "vm.hpp"
#include "mir.hpp"

namespace Mir::VM {

static inline uint64_t decode_operand(const Operand &op, const Stack_Frame &sf) {
  if (op.tag == Operand::OPERAND_TEMP) {
    size_t addr = sf.get_address_of_temp(op.temp);
    return sf.read(addr);
  } else if (op.tag == Operand::OPERAND_IMMEDIATE_VALUE) {
    const Constant &c = op.imm;
    switch (c.tag) {
      case Constant::CONST_INT:
        return c.int_lit;
      case Constant::CONST_FLOAT:
        return c.float_lit;
      case Constant::CONST_BOOL:
        return c.bool_lit;
      case Constant::CONST_CHAR:
        return c.char_lit;
      case Constant::CONST_STRING:
        // this will just be a pointer to the string's storage. it's interned in a static map and
        // guaranteed to be stable forever.
        return (uint64_t)c.string_lit.c_str();
      case Constant::CONST_INVALID:
        goto error;
    }
  } else {
  error:
    assert(false && "Unsupported operand or immediate value type for value read");
  }
}

// for LOAD and PUSH_ARG where we may need the entire sized value read.
static inline std::vector<uint64_t> decode_operand_span(const Operand &op, const Stack_Frame &sf) {
  const size_t size = sf.size_in_words(op.type);
  if (op.tag == Operand::OPERAND_TEMP) {
    const size_t addr = sf.get_address_of_temp(op.temp);
    return std::vector<uint64_t>(sf.stack.begin() + addr, sf.stack.begin() + addr + size);
  } else if (op.tag == Operand::OPERAND_IMMEDIATE_VALUE) {
    const Constant &c = op.imm;
    std::vector<uint64_t> result(size, 0);
    switch (c.tag) {
      case Constant::CONST_INT:
        result[0] = c.int_lit;
        break;
      case Constant::CONST_FLOAT:
        std::memcpy(&result[0], &c.float_lit, sizeof(c.float_lit));
        break;
      case Constant::CONST_BOOL:
        result[0] = c.bool_lit;
        break;
      case Constant::CONST_CHAR:
        result[0] = c.char_lit;
        break;
      case Constant::CONST_STRING:
        result[0] = reinterpret_cast<uint64_t>(c.string_lit.c_str());
        break;
      case Constant::CONST_INVALID:
        assert(false && "Invalid immediate value");
    }
    return result;
  } else {
    assert(false && "Unsupported operand type");
  }
}

void interpret(Context &c, Mir::Module &m, uint32_t entry_point) {
  Mir::Function *entry_point_function = m.functions[entry_point];
  Stack_Frame &sf = c.call_stack.emplace_back(entry_point_function);

  while (true) {
    const Instruction &instr = sf.fetch();
    sf.ip++;

    if (instr.opcode >= OP_ADD && instr.opcode <= OP_LOGICAL_OR) {
      const uint64_t left = decode_operand(instr.left, sf), right = decode_operand(instr.right, sf);
      const uint64_t address = sf.get_address_of_temp(instr.dest.temp);
      uint64_t result = 0;
      switch (instr.opcode) {
        case OP_ADD:
          result = left + right;
          break;
        case OP_SUB:
          result = left - right;
          break;
        case OP_MUL:
          result = left * right;
          break;
        case OP_DIV:
          result = left / right;
          break;
        case OP_MOD:
          result = left % right;
          break;
        case OP_AND:
          result = left & right;
          break;
        case OP_OR:
          result = left | right;
          break;
        case OP_XOR:
          result = left ^ right;
          break;
        case OP_SHL:
          result = left << right;
          break;
        case OP_SHR:
          result = left >> right;
          break;
        case OP_EQ:
          result = left == right;
          break;
        case OP_NE:
          result = left != right;
          break;
        case OP_LT:
          result = left < right;
          break;
        case OP_LE:
          result = left <= right;
          break;
        case OP_GT:
          result = left > right;
          break;
        case OP_GE:
          result = left >= right;
          break;
        case OP_LOGICAL_AND:
          result = left && right;
          break;
        case OP_LOGICAL_OR:
          result = left || right;
          break;
        default:
          throw_error("[VM]: invalid binary opcode, did you change the order of the Op_Code enum without fixing this condition?",
                      instr.span);
          exit(1);
      }
      sf.write(address, result);
      continue;
    }

    switch (instr.opcode) {
      case OP_LOGICAL_NOT: {
        const uint64_t operand = decode_operand(instr.left, sf);
        const uint64_t address = sf.get_address_of_temp(instr.dest.temp);
        sf.write(address, !operand);
        continue;
      }
      case OP_NEG: {
        const uint64_t operand = decode_operand(instr.left, sf);
        const uint64_t address = sf.get_address_of_temp(instr.dest.temp);
        sf.write(address, -operand);
        continue;
      }
      case OP_JMP: {
        sf.jump(instr.left.bb);
        continue;
      }
      case OP_JMP_TRUE: {
        const uint64_t condition = decode_operand(instr.right, sf);
        const Operand &destinations = instr.left;
        if (condition) {
          sf.jump(destinations.bb_pair.target);
        } else {
          sf.jump(destinations.bb_pair.fallthrough);
        }
        continue;
      }
      case OP_PUSH_ARG: {
        sf.argument_stack.push_back(decode_operand_span(instr.left, sf));
        continue;
      }
      case OP_LOAD: {
        uint64_t address = sf.get_address_of_temp(instr.dest.temp);
        sf.write(address, decode_operand_span(instr.left, sf));
        continue;
      }
      case OP_STORE: {
        uint64_t address = sf.get_address_of_temp(instr.left.temp);
        sf.write(address, decode_operand_span(instr.right, sf));
        continue;
      }
      case OP_CALL: {
        Mir::Function *fn = m.functions[instr.left.temp];
        // TODO: dispatch externs, we need to rewrite the FFI dispatcher to operate on
        // spans of longs, which should actually be easier than the previous marshalling
        // strategy

        Stack_Frame &calling_sf = sf;
        sf.ret_addr = instr.dest.temp;

        sf = c.call_stack.emplace_back(fn);
        auto arg_iter = calling_sf.argument_stack.begin();
        for (const auto &param : fn->parameter_temps) {
          uint64_t address = sf.get_address_of_temp(param.index);
          sf.write(address, *arg_iter);
          arg_iter += 1;
        }
        continue;
      }
      case OP_RET: {
        Stack_Frame &caller_sf = c.call_stack[c.call_stack.size() - 2];
        if (instr.left.tag == Operand::OPERAND_TEMP) {
          uint64_t address = sf.get_address_of_temp(instr.left.temp);
          sf.hoist(address, sf.size_in_words(instr.left.type), caller_sf.ret_addr, caller_sf);
        } else {
          caller_sf.write(caller_sf.ret_addr, decode_operand_span(instr.left, sf));
        }
        c.call_stack.pop_back();
        sf = c.call_stack.back();
        continue;
      }
      case OP_RET_VOID: {
        c.call_stack.pop_back();
        sf = c.call_stack.back();
        continue;
      }
      case OP_GEP: {
        uint64_t ptr = decode_operand(instr.left, sf);
        const Type *type = instr.left.type->get_element_type();
        // don't use the literal, may be an index expression;
        const uint64_t index_of_member = decode_operand(instr.right, sf);
        const size_t offset_in_words = sf.bytes_to_words(type->offset_in_bytes(index_of_member));
        sf.write(instr.dest.temp, ptr + offset_in_words);
        continue;
      }

      /*
        these opcodes don't do anything in the interpreter.
        types are interpeted as the correct memory on read,
        the stack is already all pre-allocated and always zeroed on
        function entry.
        TODO: implement function pointers later.
      */
      case OP_CALL_PTR:
      case OP_LOAD_FN_PTR:
        continue;
      case OP_CAST:
      case OP_BITCAST:
      case OP_ZERO_INIT:
      case OP_ALLOCA:
        continue;

      default:  // binary opcodes are handled with an if to help condense code.
        continue;
    }
  }
}
}  // namespace Mir::VM
