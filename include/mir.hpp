#pragma once
#include <cstdint>
#include <optional>
#include <stack>
#include <unordered_map>
#include <vector>
#include "arena.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "thir.hpp"
#include "type.hpp"

/*
  We should've been namespacing things this entire time but oh well.
  it's not too late to start, especially things like this that are kind of end-of-pipeline
  and self contained.
*/
namespace Mir {
enum Op_Code : uint8_t {
  OP_NOOP,  // no operands or destination.

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

  /*
    Memory
    temporaries are assumed to always we a pointer to stack memory, just like LLVM.
    load/store always take a pointer operand, and alloca always returns a pointer operand.
  */
  OP_LOAD,          // dest=result, left=ptr (tag determines const/imm/register)
  OP_STORE,         // left=ptr,   right=value
  OP_ALLOCA,        // dest=result, left=type_index (type->uid) (stack allocate),
  OP_LOAD_GLOBAL,   // dest=value, right=g_idx
  OP_STORE_GLOBAL,  // dest=g_idx, right=value

  // see Instruction for info on bb.
  OP_JMP,        // left=bb
  OP_JMP_TRUE,   // left=bb, right=condition
  OP_JMP_FALSE,  // left=bb, right=condition

  OP_PUSH_ARG,  // push argument to stack.   left=value
  OP_CALL,      // dest=result, left=fn_idx, right=n_args

  OP_RET,       // left=value
  OP_RET_VOID,  // no operands.

  /* Type casting */
  OP_CAST,     // dest=result, left=value, right=type_index (in global table, type->uid)
  OP_BITCAST,  // dest=result, left=value, right=type_index (in global table, type->uid)
  // get element pointer, calculate address of struct field or element in array
  OP_GEP,  // dest=ptr,    left=base,  right=index
};

struct Basic_Block;

struct Constant {
  union {
    InternedString string_lit;
    uint64_t int_lit;
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
  } tag = CONST_INVALID;

  static Constant Int(uint64_t value) {
    Constant c;
    c.int_lit = value;
    c.tag = CONST_INT;
    return c;
  }

  static Constant Char(uint16_t value) {
    Constant c;
    c.int_lit = value;
    c.tag = CONST_CHAR;
    return c;
  }

  static Constant String(InternedString value) {
    Constant c;
    c.string_lit = value;
    c.tag = CONST_STRING;
    return c;
  }

  static Constant Float(double value) {
    Constant c;
    c.float_lit = value;
    c.tag = CONST_FLOAT;
    return c;
  }

  static Constant Bool(bool value) {
    Constant c;
    c.bool_lit = value;
    c.tag = CONST_BOOL;
    return c;
  }

  void print(FILE *f) const;
};

struct Operand {
  Type *type = nullptr;

  enum : uint8_t {
    OPERAND_NULL,             // placeholder.       || shouldn't ever be used, but should be checked for (removed after initdevel)
    OPERAND_TEMP,             // union.temp         || local variable, temporary, "register", etc. parent->local[temp] = value.
    OPERAND_CONSTANT,         // union.constant     || referencing a constant, data section
    OPERAND_IMMEDIATE_VALUE,  // union.immediate    || immediate value.
    OPERAND_BASIC_BLOCK,      // union.bb           || basic block target for jumps.
    OPERAND_TYPE,             // this->type         || a type reference, mainly for casting and alloca.
  } tag = OPERAND_NULL;

  union {
    Constant constant;
    Constant immediate;
    Basic_Block *bb;
    uint32_t temp = 0;
  };

  static Operand Null() { return {.tag = OPERAND_NULL}; }

  static Operand Ty(Type *t) {
    Operand o;
    o.type = t;
    o.tag = OPERAND_TYPE;
    return o;
  }

  static Operand Temp(uint32_t i, Type *t) {
    Operand o;
    o.temp = i;
    o.type = t;
    o.tag = OPERAND_TEMP;
    return o;
  }

  static Operand Const(Constant c, Type *t) {
    Operand o;
    o.constant = c;
    o.type = t;
    o.tag = OPERAND_CONSTANT;
    return o;
  }

  static Operand Imm(Constant i, Type *t) {
    Operand o;
    o.immediate = i;
    o.type = t;
    o.tag = OPERAND_IMMEDIATE_VALUE;
    return o;
  }

  static Operand BB(Basic_Block *b) {
    Operand o;
    o.bb = b;
    o.tag = OPERAND_BASIC_BLOCK;
    return o;
  }
};

// This is more so an TMIR, (typed mid level intermediate representation)
// since we still carry type information for all operands and such for
// the ability to serialize effectively
struct Instruction {
  Op_Code opcode = OP_NOOP;
  Operand dest;
  Operand left;
  Operand right;
#ifdef DEBUG
  Span span;
#endif
  void print(FILE *f) const;
};

struct Basic_Block {
  InternedString label;
  std::vector<Instruction> code;

  Basic_Block() = default;
  explicit Basic_Block(InternedString l) : label(l) {}

  inline void push(Instruction &&bc) { code.push_back(std::move(bc)); }
  inline const Instruction &back() const {
    assert(code.size() && "no instructions");
    return code.back();
  }
  inline Op_Code back_opcode() const { return back().opcode; }
  void print(FILE *f) const;
};

extern jstl::Arena mir_arena;

static inline InternedString generate_temp_identifier(size_t index) { return std::format("t{}", index); }

// local variable handle.
struct Temporary {
  InternedString name;
  Type *type;
};

struct Function {
  uint32_t index;  // how to refer to this function.

  InternedString name;
  Span span;

  std::vector<Basic_Block *> basic_blocks;

  // parameters are stored in here as the first locals, to simplifying indexing
  // in the MIR
  std::vector<Temporary> temps;

  uint64_t stack_size_needed_in_bytes = 0;

  FunctionTypeInfo *type_info;
  Type *type;

  enum Flags : uint8_t {
    FUNCTION_FLAGS_NONE = 0,
    FUNCTION_FLAGS_IS_INLINE = 1 << 0,
    FUNCTION_FLAGS_IS_VAR_ARGS = 1 << 1,
    FUNCTION_FLAGS_IS_EXTERN = 1 << 2,
    FUNCTION_FLAGS_IS_ENTRY_POINT = 1 << 3,
    FUNCTION_FLAGS_IS_EXPORTED = 1 << 4,
    FUNCTION_FLAGS_IS_TEST = 1 << 5,
  };

  uint8_t flags = FUNCTION_FLAGS_NONE;

  inline Basic_Block *entry_block() const {
    assert(basic_blocks.size());
    return basic_blocks[0];
  }

  static inline InternedString generate_default_bb_label() {
    static size_t index = 0;
    return std::format("bb{}", index++);
  }

  inline Basic_Block *enter_bb(InternedString label = generate_default_bb_label()) {
    Basic_Block *block = mir_arena.construct<Basic_Block>(label);
    basic_blocks.push_back(block);
    return block;
  }

  inline Basic_Block *get_insert_block() {
    assert(basic_blocks.size() && "no basic blocks");
    return basic_blocks.back();
  }
  void print(FILE *f) const;
};

struct Module {
  std::vector<Function *> functions;
  std::unordered_map<InternedString, Function *> function_table;

  std::unordered_set<Type *> used_types;

  std::unordered_map<THIRVariable const *, Operand> variables;  // used for lowering, referencing.
  std::stack<Function *> function_stack;                        // used for lowering only.
  Function *current_function;

  inline Operand create_temporary(Type *type, std::optional<InternedString> label = std::nullopt) {
    Function *f = current_function;
    assert(type && f);
    size_t idx = f->temps.size();

    if (label == std::nullopt) {
      label = generate_temp_identifier(idx);
    }

    f->temps.push_back(Temporary{
        .name = *label,
        .type = type,
    });

    f->stack_size_needed_in_bytes += type->size_in_bytes();

    used_types.insert(type);

    return Operand::Temp(idx, type);
  }

  inline Function *get_function(InternedString name) {
    const auto it = function_table.find(name);
    assert(it != function_table.end() && "unable to find function");
    return it->second;
  }

  inline Function *create_function(const THIRFunction *node, uint32_t &index) {
    Function *f = mir_arena.construct<Function>();
    index = (uint32_t)functions.size();
    f->name = node->name;
    f->type_info = node->type->info->as<FunctionTypeInfo>();
    f->type = node->type;

    size_t param_idx = 0;
    for (const auto &param : node->parameters) {
      f->temps.push_back({
          .name = generate_temp_identifier(param_idx),
          .type = param.associated_variable->type,
      });
      variables[param.associated_variable] = Operand::Temp(param_idx, param.associated_variable->type);
      param_idx++;
    }

    functions.push_back(f);
    function_table[node->name] = f;
    return f;
  }

  inline void enter_function(Function *f) {
    if (current_function) {
      function_stack.push(current_function);
    }
    current_function = f;
  }

  inline void leave_function() {
    assert(function_stack.size() && "cannot leave entry point function");
    current_function = function_stack.top();
    function_stack.pop();
  }

  // small helper to insert and enter a basic block in the current function
  inline Basic_Block *create_basic_block(InternedString label = Function::generate_default_bb_label()) {
    assert(current_function);
    return current_function->enter_bb(label);
  }

  inline Basic_Block *get_insert_block() {
    assert(current_function && "no current function");
    return current_function->basic_blocks.back();
  }

  void print(FILE *f) const;
};

void generate_block(const THIRBlock *node, Module &m);
void generate_variable(const THIRVariable *node, Module &m);
Operand generate_function(const THIRFunction *node, Module &m);
Operand generate_expr_block(const THIRExprBlock *node, Module &m);
Operand generate_bin_expr(const THIRBinExpr *node, Module &m);
Operand generate_unary_expr(const THIRUnaryExpr *node, Module &m);
Operand generate_literal(const THIRLiteral *node, Module &m);
Operand generate_call(const THIRCall *node, Module &m);
Operand generate_member_access(const THIRMemberAccess *node, Module &m);
Operand generate_cast(const THIRCast *node, Module &m);
Operand generate_index(const THIRIndex *node, Module &m);
Operand generate_aggregate_initializer(const THIRAggregateInitializer *node, Module &m);
Operand generate_collection_initializer(const THIRCollectionInitializer *node, Module &m);
Operand generate_empty_initializer(const THIREmptyInitializer *node, Module &m);
Operand load_variable(const THIRVariable *node, Module &m);

void generate_return(const THIRReturn *node, Module &m);
void generate_break(const THIRBreak *node, Module &m);
void generate_continue(const THIRContinue *node, Module &m);
void generate_for(const THIRFor *node, Module &m);
void generate_if(const THIRIf *node, Module &m);
void generate_while(const THIRWhile *node, Module &m);

Operand generate_lvalue_addr(const THIR *node, Module &m);
Operand generate_index_addr(const THIRIndex *node, Module &m);
Operand generate_member_access_addr(const THIRMemberAccess *node, Module &m);

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
  }
}

static inline Operand generate_expr(const THIR *node, Module &m) {
  switch (node->get_node_type()) {
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
      return generate_aggregate_initializer((THIRAggregateInitializer *)node, m);
    case THIRNodeType::CollectionInitializer:
      return generate_collection_initializer((THIRCollectionInitializer *)node, m);
    case THIRNodeType::EmptyInitializer:
      return generate_empty_initializer((THIREmptyInitializer *)node, m);
    default:
      return Operand{};
  }
}

#define EMIT_OP(OP) m.current_function->get_insert_block()->push(Instruction{OP})
#define EMIT_NULLARY(OP, DEST) m.current_function->get_insert_block()->push(Instruction{OP, DEST})
#define EMIT_UNARY(OP, DEST, LEFT) m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT})
#define EMIT_BINARY(OP, DEST, LEFT, RIGHT) m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT, RIGHT})
#define EMIT_BINOP(OP, DEST, LEFT, RIGHT) m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT, RIGHT})
#define EMIT_UNOP(OP, DEST, LEFT) m.current_function->get_insert_block()->push(Instruction{OP, DEST, LEFT})
#define EMIT_CALL(DEST, FN_IDX, N_ARGS) m.current_function->get_insert_block()->push(Instruction{OP_CALL, DEST, FN_IDX, N_ARGS})
#define EMIT_RET(VAL) m.current_function->get_insert_block()->push(Instruction{OP_RET, Operand(), VAL})
#define EMIT_RET_VOID() m.current_function->get_insert_block()->push(Instruction{OP_RET_VOID})
#define EMIT_LOAD(DEST, PTR) m.current_function->get_insert_block()->push(Instruction{OP_LOAD, DEST, PTR})
#define EMIT_STORE(PTR, VAL) m.current_function->get_insert_block()->push(Instruction{OP_STORE, Operand(), PTR, VAL})
#define EMIT_ALLOCA(DEST, TYPE_INDEX_UID) \
  m.current_function->get_insert_block()->push(Instruction{OP_ALLOCA, DEST, TYPE_INDEX_UID})
#define EMIT_PUSH_ARG(ARG) m.current_function->get_insert_block()->push(Instruction{OP_PUSH_ARG, Operand(), ARG})
#define EMIT_CAST(DEST, VAL, TYPE_IDX) m.current_function->get_insert_block()->push(Instruction{OP_CAST, DEST, VAL, TYPE_IDX})
#define EMIT_BITCAST(DEST, VAL, TYPE_IDX) \
  m.current_function->get_insert_block()->push(Instruction{OP_BITCAST, DEST, VAL, TYPE_IDX})
#define EMIT_GEP(DEST, BASE, INDEX) m.current_function->get_insert_block()->push(Instruction{OP_GEP, DEST, BASE, INDEX})
#define EMIT_LOAD_GLOBAL(DEST, G_IDX) \
  m.current_function->get_insert_block()->push(Instruction{OP_LOAD_GLOBAL, DEST, Operand(), G_IDX})
#define EMIT_STORE_GLOBAL(G_IDX, VAL) \
  m.current_function->get_insert_block()->push(Instruction{OP_STORE_GLOBAL, Operand(), G_IDX, VAL})
#define EMIT_JUMP_TRUE(TARGET_BB, COND) \
  m.current_function->get_insert_block()->push(Instruction{OP_JMP_TRUE, Operand(), Operand::BB(TARGET_BB), COND})
#define EMIT_JUMP_FALSE(TARGET_BB, COND) \
  m.current_function->get_insert_block()->push(Instruction{OP_JMP_FALSE, Operand(), Operand::BB(TARGET_BB), COND})
#define EMIT_JUMP(TARGET_BB) m.current_function->get_insert_block()->push(Instruction{OP_JMP, Operand(), Operand::BB(TARGET_BB)})

}  // namespace Mir
