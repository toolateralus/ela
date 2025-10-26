#pragma once
#include <cstdint>
#include <optional>
#include <stack>
#include <unordered_map>
#include <vector>
#include "arena.hpp"
#include "callconv.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "thir.hpp"
#include "type.hpp"
#include "value.hpp"

/*
  We should've been namespacing things this entire time but oh well.
  it's not too late to start, especially things like this that are kind of end-of-pipeline
  and self contained.
*/
namespace mir {
enum Op_Code : uint8_t {
  OP_UNREACHABLE = 0,
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
  OP_EQ,
  OP_NE,
  OP_LT,
  OP_LE,
  OP_GT,
  OP_GE,
  OP_LOGICAL_AND,
  OP_LOGICAL_OR,

  OP_NEG,

  /* Unary */
  OP_LOGICAL_NOT,
  OP_NOT,
  /*
    Memory
    temporaries are assumed to always we a pointer to stack memory, just like LLVM.
    load/store always take a pointer operand, and alloca always returns a pointer operand.
  */
  OP_LOAD,    // dest=result, left=ptr (tag determines const/imm/register)
  OP_STORE,   // left=ptr,   right=value
  OP_ALLOCA,  // dest=result, left=type_index (type->uid) (stack allocate),
  OP_ZERO_INIT,
  OP_LOAD_FN_PTR,  // dest=ptr, right=fn_idx,

  // see Instruction for info on bb.
  OP_JMP,       // left=bb
  OP_JMP_TRUE,  // left=bb, right=condition

  OP_PUSH_ARG,  // push argument to stack.   left=value
  OP_CALL,      // dest=result, left=fn_idx, right=n_args
  OP_CALL_PTR,  // dest=result, left=fn_ptr, right=n_args

  OP_RET,       // left=value
  OP_RET_VOID,  // no operands.

  /* Type casting */
  OP_CAST,     // dest=result, left=value, right=type_index (in global table, type->uid)
  OP_BITCAST,  // dest=result, left=value, right=type_index (in global table, type->uid)

  // get element pointer, calculate address of struct field or element in array
  OP_GEP,  // dest=ptr,    left=base,  right=index
};

static constexpr inline const char *opcode_to_string(enum Op_Code op) {
  switch (op) {
#define X(NAME) \
  case NAME:    \
    return #NAME;
    X(OP_UNREACHABLE)
    X(OP_ADD)
    X(OP_SUB)
    X(OP_MUL)
    X(OP_DIV)
    X(OP_MOD)
    X(OP_AND)
    X(OP_OR)
    X(OP_XOR)
    X(OP_SHL)
    X(OP_SHR)
    X(OP_EQ)
    X(OP_NE)
    X(OP_LT)
    X(OP_LE)
    X(OP_GT)
    X(OP_GE)
    X(OP_LOGICAL_AND)
    X(OP_LOGICAL_OR)
    X(OP_NEG)
    X(OP_LOGICAL_NOT)
    X(OP_NOT)
    X(OP_LOAD)
    X(OP_STORE)
    X(OP_ALLOCA)
    X(OP_ZERO_INIT)
    X(OP_LOAD_FN_PTR)
    X(OP_JMP)
    X(OP_JMP_TRUE)
    X(OP_PUSH_ARG)
    X(OP_CALL)
    X(OP_CALL_PTR)
    X(OP_RET)
    X(OP_RET_VOID)
    X(OP_CAST)
    X(OP_BITCAST)
    X(OP_GEP)

#undef X
    default:
      return "UNKNOWN";
  }
}

struct Basic_Block;

struct Constant {
  Type *type;  // TODO: remove this to optimize the size of these giant structs.
  union {
    InternedString string_lit;
    int64_t int_lit;
    double float_lit;
    bool bool_lit;
    uint16_t char_lit;
  };
  enum : uint8_t {
    CONST_INVALID,
    CONST_INT,
    CONST_STRING,
    CONST_FLOAT,
    CONST_BOOL,
    CONST_CHAR,
    CONST_NULLPTR,
  } tag = CONST_INVALID;

  static Constant Nullptr(Type *t = void_type()->take_pointer_to()) {
    Constant c;
    c.tag = CONST_NULLPTR;
    c.type = t;
    return c;
  }

  static Constant Int(int64_t value, Type *t = s32_type()) {
    Constant c;
    c.int_lit = value;
    c.tag = CONST_INT;
    c.type = t;
    return c;
  }

  static Constant Char(uint16_t value, Type *t = u32_type()) {
    Constant c;
    c.int_lit = value;
    c.tag = CONST_CHAR;
    c.type = t;
    return c;
  }

  static Constant String(InternedString value, Type *t = u8_ptr_type()) {
    Constant c;
    c.string_lit = value;
    c.tag = CONST_STRING;
    c.type = t;
    return c;
  }

  static Constant Float(double value, Type *t = f32_type()) {
    Constant c;
    c.float_lit = value;
    c.tag = CONST_FLOAT;
    c.type = t;
    return c;
  }

  static Constant Bool(bool value) {
    Constant c;
    c.bool_lit = value;
    c.tag = CONST_BOOL;
    c.type = bool_type();
    return c;
  }
  void print(FILE *) const;
};

struct Global_Variable;
struct Operand {
  Type *type = nullptr;

  enum : uint8_t {
    OPERAND_NULL,             // placeholder.       || shouldn't ever be used, but should be checked for (removed after initdevel)
    OPERAND_TEMP,             // union.temp         || local variable, temporary, "register", etc. parent->local[temp] = value.
    OPERAND_IMMEDIATE_VALUE,  // union.immediate    || immediate value.
    OPERAND_GLOBAL_VARIABLE_REFERENCE,  // union.gv           || ref to a global var
    OPERAND_BASIC_BLOCK,                // union.bb           || basic block target for jumps.
    OPERAND_BASIC_BLOCK_PAIR,           // union.bb           || basic block target/fallthrough for conditional jump
    OPERAND_TYPE,                       // this->type         || a type reference, mainly for casting and alloca.
  } tag = OPERAND_NULL;

  union {
    Global_Variable *gv;
    Constant imm;
    Basic_Block *bb;

    struct {
      Basic_Block *target;
      Basic_Block *fallthrough;
    } bb_pair;

    struct {
      uint32_t temp = 0;
    };
  };

  static Operand MakeNull() { return {.tag = OPERAND_NULL}; }

  static Operand Make_Type_Ref(Type *t) {
    Operand o;
    o.type = t;
    o.tag = OPERAND_TYPE;
    return o;
  }

  static Operand Make_Temp(uint32_t i, Type *t) {
    Operand o;
    o.temp = i;
    o.type = t;
    o.tag = OPERAND_TEMP;
    return o;
  }
  static Operand Make_Global_Ref(Global_Variable *gv);

  static Operand Make_Imm(Constant i, Type *t) {
    Operand o;
    o.imm = i;
    o.type = t;
    o.tag = OPERAND_IMMEDIATE_VALUE;
    return o;
  }

  static Operand Make_BB(Basic_Block *b) {
    Operand o;
    o.bb = b;
    o.tag = OPERAND_BASIC_BLOCK;
    return o;
  }

  static Operand Make_BB_Pair(Basic_Block *target, Basic_Block *fallthrough) {
    Operand o;
    o.bb_pair = {
        .target = target,
        .fallthrough = fallthrough,
    };
    o.tag = OPERAND_BASIC_BLOCK_PAIR;
    return o;
  }

  void print(FILE *) const;
};

// This is more so an TMIR, (typed mid level intermediate representation)
// since we still carry type information for all operands and such for
// the ability to serialize effectively

// this is a giant struct, we should really have ANOTHER ir that's much lower level,
// and optimized.
struct Module;
struct Instruction {
  Op_Code opcode;
  Operand dest;
  Operand left;
  Operand right;
  Span span;
  uint32_t index;  // for debugging
  void print(FILE *, Module &) const;
};

struct Function;
struct Basic_Block {
  InternedString label;
  std::vector<Instruction> code;

  Basic_Block() = default;
  explicit Basic_Block(InternedString l) : label(l) {}

  inline void push(Instruction &&bc) {
    bc.index = code.size();
    code.push_back(std::move(bc));
  }
  inline const Instruction &back() const {
    assert(code.size() && "no instructions");
    return code.back();
  }
  inline Op_Code back_opcode() const { return back().opcode; }
  void print(FILE *, Module &) const;

  inline bool ends_with_terminator() const {
    if (code.empty()) {
      return false;
    }
    Instruction const &back = code.back();
    return back.opcode == OP_JMP || back.opcode == OP_JMP_TRUE || back.opcode == OP_RET || back.opcode == OP_RET_VOID ||
           back.opcode == OP_UNREACHABLE;
  }

  inline bool ends_with_non_divergent_terminator() const {
    if (code.empty()) {
      return false;
    }
    Instruction const &back = code.back();
    return back.opcode == OP_RET || back.opcode == OP_RET_VOID || back.opcode == OP_UNREACHABLE;
  }

  void finalize(Function *f) const;
};

extern jstl::Arena mir_arena;

static inline InternedString generate_temp_identifier(size_t index, std::string prefix = "t") {
  return std::format("{}{}", prefix, index);
}

// local variable handle.
struct Temporary {
  InternedString name;
  Type *type;
  uint32_t index;
};

struct Function {
  uint32_t index;  // how to refer to this function.
  InternedString name;
  Span span;

  std::vector<Basic_Block *> basic_blocks;
  Basic_Block *insert_block;

  // parameters are stored in here as the first locals, to simplify indexing
  // in the MIR
  std::vector<Temporary> temps;

  // These are used just for printing.
  std::vector<Temporary> parameter_temps;

  uint64_t stack_size_needed_in_bytes = 0;

  FunctionTypeInfo *type_info;
  Type *type;

  enum Flags : uint8_t {
    FUNCTION_FLAGS_NONE = 0,
    FUNCTION_FLAGS_IS_INLINE = 1 << 0,
    FUNCTION_FLAGS_IS_VAR_ARGS = 1 << 1,
    FUNCTION_FLAGS_IS_EXTERN = 1 << 2,
    FUNCTION_FLAGS_IS_EXPORTED = 1 << 3,
    FUNCTION_FLAGS_IS_NO_RETURN = 1 << 4,
    FUNCTION_FLAGS_IS_CONSTRUCTOR_0 = 1 << 5,
    FUNCTION_FLAGS_IS_CONSTRUCTOR_1 = 1 << 6,
  };

  uint8_t flags = FUNCTION_FLAGS_NONE;

  inline Basic_Block *entry_block() const {
    assert(basic_blocks.size());
    return basic_blocks[0];
  }

  static inline InternedString generate_default_bb_label(size_t index) { return std::format("bb{}", index); }

  inline Basic_Block *create_and_enter_basic_block(std::optional<InternedString> label = std::nullopt) {
    if (label == std::nullopt) {
      label = generate_default_bb_label(this->basic_blocks.size());
    }

    bool clash = false;
    size_t index = 0;
    InternedString original_label = *label;
    do {
      clash = false;
      for (auto *bb : basic_blocks) {
        if (bb->label == *label) {
          label = std::format("{}{}", original_label, index++);
          clash = true;
          break;
        }
      }
    } while (clash);

    Basic_Block *block = mir_arena.construct<Basic_Block>(*label);
    basic_blocks.push_back(block);
    insert_block = block;
    return block;
  }

  // TODO: find basic blocks that are empty and have no jump references and purge or merge when possible
  // control flow ends up leaving a bunch of junk in the IR
  inline void finalize() {
    stack_size_needed_in_bytes = 0;
    for (const auto &temp : temps) {
      if (temp.type && (temp.type->size_in_bits() / 8) > 0) {
        size_t align = temp.type->alignment_in_bits() * 8;
        if (align && stack_size_needed_in_bytes % align != 0) {
          stack_size_needed_in_bytes += align - (stack_size_needed_in_bytes % align);
        }
        stack_size_needed_in_bytes += temp.type->size_in_bits() / 8;
      }
    }
    if (stack_size_needed_in_bytes % 16 != 0) {
      stack_size_needed_in_bytes += 16 - (stack_size_needed_in_bytes % 16);
    }

    for (auto *bb : basic_blocks) {
      bb->finalize(this);
    }
  }

  inline Basic_Block *get_insert_block() { return insert_block; }

  inline void reset_insert_block() { insert_block = basic_blocks.back(); }

  inline void set_insert_block(Basic_Block *bb) { insert_block = bb; }

  void print(FILE *, Module &) const;
};

struct Global_Variable {
  InternedString name;
  Type *type;
  bool has_external_linkage = true;
};

struct Module {
  std::vector<Function *> functions;
  std::unordered_map<InternedString, Function *> function_table;
  std::vector<Global_Variable *> global_variables;
  std::unordered_map<THIRVariable const *, Global_Variable *> global_variable_table;
  std::unordered_set<Type *> used_types;

  std::function<void()> on_break_or_continue;

  std::unordered_map<THIRVariable const *, Operand> variables;  // used for lowering, referencing.
  std::stack<Function *> function_stack;                        // used for lowering only.
  Function *current_function;

  Calling_Convention *cc;

  Module(Calling_Convention *calling_conv) : cc(calling_conv) {}

  inline Operand create_temporary(Type *type, std::optional<InternedString> label = std::nullopt) {
    Function *f = current_function;
    assert(type && f);
    uint32_t idx = f->temps.size();

    if (label == std::nullopt) {
      label = generate_temp_identifier(idx);
    }

    f->temps.push_back(Temporary{
        .name = *label,
        .type = type,
        .index = idx,
    });

    used_types.insert(type);

    return Operand::Make_Temp(idx, type);
  }

  inline Function *get_function(InternedString name) {
    const auto it = function_table.find(name);
    assert(it != function_table.end() && "unable to find function");
    return it->second;
  }

  inline void enter_function(Function *f) {
    if (current_function) {
      function_stack.push(current_function);
    }
    current_function = f;
  }

  inline void leave_function() {
    if (function_stack.empty()) {
      throw_error(
          std::format("function stack was empty but we tried to leave_function(). current_function: {}", current_function->name),
          current_function->span);
    }
    current_function = function_stack.top();
    function_stack.pop();
  }

  // small helper to insert and enter a basic block in the current function
  inline Basic_Block *create_and_enter_basic_block(std::optional<InternedString> label = std::nullopt) {
    assert(current_function);
    return current_function->create_and_enter_basic_block(label);
  }

  inline void finalize() {  // compute stack sizes after all code has compiled.
    for (const auto &f : functions) {
      f->finalize();
    }
  }

  void print(FILE *) const;
};

Operand generate_variable(const THIRVariable *node, Module &m);
Operand generate_function(const THIRFunction *node, Module &m);
Operand generate_expr_block(const THIRExprBlock *node, Module &m);

Operand generate_bin_expr(const THIRBinExpr *node, Module &m);
Operand generate_unary_expr(const THIRUnaryExpr *node, Module &m);

Operand generate_ptr_bin_expr(const THIRPtrBinExpr *node, Module &m);
Operand generate_ptr_unary_expr(const THIRPtrUnaryExpr *node, Module &m);

Operand generate_literal(const THIRLiteral *node, Module &m);
Operand generate_call(const THIRCall *node, Module &m);
Operand generate_member_access(const THIRMemberAccess *node, Module &m);
Operand generate_cast(const THIRCast *node, Module &m);
Operand generate_index(const THIRIndex *node, Module &m);

Operand generate_aggregate_initializer(const THIRAggregateInitializer *node, Module &m,
                                       Nullable<Operand> existing_alloca = nullptr);

Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m,
                                        Nullable<Operand> existing_alloca = nullptr);

Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m, Nullable<Operand> existing_alloca = nullptr);

Operand load_variable(const THIRVariable *node, Module &m);

void generate_return(const THIRReturn *node, Module &m);
void generate_break(const THIRBreak *node, Module &m);
void generate_continue(const THIRContinue *node, Module &m);

void generate_block(const THIRBlock *node, Module &m);
void generate_for(const THIRFor *node, Module &m);
void generate_if(const THIRIf *node, Module &m);
void generate_while(const THIRWhile *node, Module &m);

Operand generate_lvalue_addr(const THIR *node, Module &m);
Operand generate_index_addr(const THIRIndex *node, Module &m);
Operand generate_member_access_addr(const THIRMemberAccess *node, Module &m);

void compile(const THIR *entry_point, Module &m, const std::vector<THIRFunction *> &constructors,
             const THIRFunction *global_initializer, const std::vector<THIRVariable *> reflection_variables);

static inline void generate(const THIR *node, Module &m) {
  switch (node->get_node_type()) {
    case THIRNodeType::Block:
      generate_block((THIRBlock *)node, m);
      break;
    case THIRNodeType::Variable:
      generate_variable((THIRVariable *)node, m);
      break;
    case THIRNodeType::Function:
      generate_function((THIRFunction *)node, m);
      break;
    case THIRNodeType::ExpressionBlock:
      generate_expr_block((THIRExprBlock *)node, m);
      break;
    case THIRNodeType::BinExpr:
      generate_bin_expr((THIRBinExpr *)node, m);
      break;
    case THIRNodeType::UnaryExpr:
      generate_unary_expr((THIRUnaryExpr *)node, m);
      break;
    case THIRNodeType::PtrBinExpr:
      generate_ptr_bin_expr((THIRPtrBinExpr *)node, m);
      break;
    case THIRNodeType::PtrUnaryExpr:
      generate_ptr_unary_expr((THIRPtrUnaryExpr *)node, m);
      break;
    case THIRNodeType::Literal:
      generate_literal((THIRLiteral *)node, m);
      break;
    case THIRNodeType::Call:
      generate_call((THIRCall *)node, m);
      break;
    case THIRNodeType::MemberAccess:
      generate_member_access((THIRMemberAccess *)node, m);
      break;
    case THIRNodeType::Cast:
      generate_cast((THIRCast *)node, m);
      break;
    case THIRNodeType::Index:
      generate_index((THIRIndex *)node, m);
      break;
    case THIRNodeType::AggregateInitializer:
      generate_aggregate_initializer((THIRAggregateInitializer *)node, m);
      break;
    case THIRNodeType::CollectionInitializer:
      generate_collection_initializer((THIRCollectionInitializer *)node, m);
      break;
    case THIRNodeType::EmptyInitializer:
      generate_empty_initializer((THIREmptyInitializer *)node, m);
      break;
    case THIRNodeType::Return:
      generate_return((THIRReturn *)node, m);
      break;
    case THIRNodeType::Break:
      generate_break((THIRBreak *)node, m);
      break;
    case THIRNodeType::Continue:
      generate_continue((THIRContinue *)node, m);
      break;
    case THIRNodeType::For:
      generate_for((THIRFor *)node, m);
      break;
    case THIRNodeType::If:
      generate_if((THIRIf *)node, m);
      break;
    case THIRNodeType::While:
      generate_while((THIRWhile *)node, m);
      break;
    // TODO: verify we want to ignore these.
    case THIRNodeType::Type:
    case THIRNodeType::Noop:
    case THIRNodeType::Program:
      break;
      break;
  }
}

static inline Operand generate_expr(const THIR *node, Module &m, Nullable<Operand> existing_alloca = nullptr) {
  switch (node->get_node_type()) {
    case THIRNodeType::PtrBinExpr:
      return generate_ptr_bin_expr((THIRPtrBinExpr *)node, m);
    case THIRNodeType::PtrUnaryExpr:
      return generate_ptr_unary_expr((THIRPtrUnaryExpr *)node, m);
    case THIRNodeType::Variable:
      return load_variable((THIRVariable *)node, m);
    case THIRNodeType::Function:
      return generate_function((THIRFunction *)node, m);
    case THIRNodeType::ExpressionBlock:
      return generate_expr_block((THIRExprBlock *)node, m);
    case THIRNodeType::BinExpr:
      return generate_bin_expr((THIRBinExpr *)node, m);
    case THIRNodeType::UnaryExpr:
      return generate_unary_expr((THIRUnaryExpr *)node, m);
    case THIRNodeType::Literal:
      return generate_literal((THIRLiteral *)node, m);
    case THIRNodeType::Call:
      return generate_call((THIRCall *)node, m);
    case THIRNodeType::MemberAccess:
      return generate_member_access((THIRMemberAccess *)node, m);
    case THIRNodeType::Cast:
      return generate_cast((THIRCast *)node, m);
    case THIRNodeType::Index:
      return generate_index((THIRIndex *)node, m);
    case THIRNodeType::AggregateInitializer:
      return generate_aggregate_initializer((THIRAggregateInitializer *)node, m, existing_alloca);
    case THIRNodeType::CollectionInitializer:
      return generate_collection_initializer((THIRCollectionInitializer *)node, m, existing_alloca);
    case THIRNodeType::EmptyInitializer:
      return generate_empty_initializer((THIREmptyInitializer *)node, m, existing_alloca);
    default:
      return Operand{};
  }
}

#define EMIT_OP(OP) m.current_function->get_insert_block()->push(Instruction{OP, .span = node->span})
#define EMIT_NULLARY(OP, DEST) m.current_function->get_insert_block()->push(Instruction{OP, DEST, .span = node->span})
#define EMIT_UNARY(OP, DEST, LEFT) m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT, .span = node->span})
#define EMIT_BINARY(OP, DEST, LEFT, RIGHT) \
  m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT, RIGHT, .span = node->span})
#define EMIT_BINOP(OP, DEST, LEFT, RIGHT) \
  m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT, RIGHT, .span = node->span})

// OP_CALL: dest=result, left=fn_idx, right=n_args
#define EMIT_CALL(DEST, FN_IDX, N_ARGS) \
  m.current_function->get_insert_block()->push(Instruction{OP_CALL, DEST, FN_IDX, N_ARGS, .span = node->span})
#define EMIT_CALL_PTR(DEST, FN_IDX, N_ARGS) \
  m.current_function->get_insert_block()->push(Instruction{OP_CALL_PTR, DEST, FN_IDX, N_ARGS, .span = node->span})

// OP_RET: left=value
#define EMIT_RET(VAL) m.current_function->get_insert_block()->push(Instruction{OP_RET, Operand(), VAL, .span = node->span})
#define EMIT_RET_VOID() m.current_function->get_insert_block()->push(Instruction{OP_RET_VOID, .span = node->span})

// OP_LOAD: dest=result, left=ptr
#define EMIT_LOAD(DEST, PTR)                                                                           \
  do {                                                                                                 \
    auto p = (PTR);                                                                                    \
    if (p.tag != Operand::OPERAND_GLOBAL_VARIABLE_REFERENCE) {                                         \
      assert((p).type->is_pointer() && "Got a non pointer type in a load");                            \
    }                                                                                                  \
    m.current_function->get_insert_block()->push(Instruction{OP_LOAD, DEST, (p), .span = node->span}); \
  } while (false);

// OP_STORE: left=ptr, right=value
#define EMIT_STORE(PTR, VAL) \
  m.current_function->get_insert_block()->push(Instruction{OP_STORE, Operand(), PTR, VAL, .span = node->span})

// OP_ALLOCA: dest=result, left=type_index
#define EMIT_ALLOCA(DEST, TYPE_INDEX_UID) \
  m.current_function->get_insert_block()->push(Instruction{OP_ALLOCA, DEST, TYPE_INDEX_UID, .span = node->span})

// OP_PUSH_ARG: left=value
#define EMIT_PUSH_ARG(ARG) \
  m.current_function->get_insert_block()->push(Instruction{OP_PUSH_ARG, Operand(), ARG, .span = node->span})

// OP_CAST / OP_BITCAST: dest=result, left=value, right=type_index
#define EMIT_CAST(DEST, VAL, TYPE_IDX) \
  m.current_function->get_insert_block()->push(Instruction{OP_CAST, DEST, VAL, TYPE_IDX, .span = node->span})

#define EMIT_BITCAST(DEST, VAL, TYPE_IDX) \
  m.current_function->get_insert_block()->push(Instruction{OP_BITCAST, DEST, VAL, TYPE_IDX, .span = node->span})

// OP_GEP: dest=ptr, left=base, right=index
#define EMIT_GEP(DEST, BASE, INDEX) \
  m.current_function->get_insert_block()->push(Instruction{OP_GEP, DEST, BASE, INDEX, .span = node->span})

// OP_LOAD_FN_PTR: dest=ptr, right=fn_idx (left unused)
#define EMIT_LOAD_FN_PTR(DEST, FN_IDX) \
  m.current_function->get_insert_block()->push(Instruction{OP_LOAD_FN_PTR, DEST, Operand(), FN_IDX, .span = node->span})

// Jump macros (OP_JMP, OP_JMP_TRUE, OP_JMP_FALSE)
// OP_JMP: left=bb
#define EMIT_JUMP(TARGET_BB) \
  m.current_function->get_insert_block()->push(Instruction{OP_JMP, Operand(), Operand::Make_BB(TARGET_BB), .span = node->span})

// OP_JMP_TRUE: left=bb, right=condition
#define EMIT_JUMP_TRUE(TARGET_BB, FALL_THROUGH_BB, COND) \
  m.current_function->get_insert_block()->push(          \
      Instruction{OP_JMP_TRUE, Operand(), Operand::Make_BB_Pair(TARGET_BB, FALL_THROUGH_BB), COND, .span = node->span})

#define EMIT_ZERO_INIT(PTR, TY) \
  m.current_function->get_insert_block()->push(Instruction{OP_ZERO_INIT, Operand(), PTR, TY, .span = node->span})

#define EMIT_UNREACHABLE() m.current_function->get_insert_block()->push(Instruction{OP_UNREACHABLE, .span = node->span})

}  // namespace mir
