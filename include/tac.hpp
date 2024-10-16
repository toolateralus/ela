#pragma once
#include "ast.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unordered_map>

[[noreturn]] static void tac_error(const char *err) {
  printf("error: %s\n", err);
  exit(1);
}

enum Opcode : uint16_t {
  OPCODE_HALT,

  /* BINARY OPERATORS */
  OPCODE_ASSIGN,
  OPCODE_ADD,
  OPCODE_SUB,
  OPCODE_MUL,
  OPCODE_DIV,
  OPCODE_MOD,

  OPCODE_OR,
  OPCODE_XOR,

  OPCODE_AND,
  OPCODE_LOGICAL_AND,
  OPCODE_LOGICAL_OR,

  OPCODE_CMP, // <=> return -1 if lt, 0 if eq, 1 if gt

  OPCODE_SHL,
  OPCODE_SHR,

  /* UNARY OPERATORS */
  OPCODE_NOT,
  OPCODE_LOGICAL_NOT,
  OPCODE_NEGATE,
  OPCODE_INC,
  OPCODE_DEC,

  /* CONTROL FLOW  */
  OPCODE_BR_EQ,
  OPCODE_BR_NEQ,
  OPCODE_BR_LT,
  OPCODE_BR_GT,
  OPCODE_BR_LE,
  OPCODE_BR_GE,
  OPCODE_JUMP,

  /* FUNCTIONS */
  OPCODE_PUSH_ARG,
  OPCODE_POP_ARG,
  OPCODE_CALL,
  OPCODE_RETURN,
  OPCODE_RETURN_VOID,

  /* MEMORY RELATED */
  OPCODE_LOAD,
  OPCODE_STORE,
  OPCODE_PUSH,
  OPCODE_POP,
  OPCODE_ENTER,
  OPCODE_LEAVE,
  OPCODE_SUBSCRIPT,
  OPCODE_FIELD,
  OPCODE_ALLOC,
  OPCODE_FREE,
  OPCODE_LABEL,

};

static inline Opcode op_from_ttype(const TType &ttype) {
  switch (ttype) {
  case TType::Assign:
    return OPCODE_ASSIGN;
  case TType::Add:
    return OPCODE_ADD;
  case TType::Sub:
    return OPCODE_SUB;
  case TType::Mul:
    return OPCODE_MUL;
  case TType::Div:
    return OPCODE_DIV;
  case TType::Modulo:
    return OPCODE_MOD;
  case TType::Or:
    return OPCODE_OR;
  case TType::Xor:
    return OPCODE_XOR;
  case TType::And:
    return OPCODE_AND;
  case TType::LogicalAnd:
    return OPCODE_LOGICAL_AND;
  case TType::LogicalOr:
    return OPCODE_LOGICAL_OR;
  case TType::SHL:
    return OPCODE_SHL;
  case TType::SHR:
    return OPCODE_SHR;
  case TType::Not:
    return OPCODE_NOT;
  case TType::BitwiseNot:
    return OPCODE_LOGICAL_NOT;
  case TType::LT:
    return OPCODE_BR_LT;
  case TType::GT:
    return OPCODE_BR_GT;
  case TType::EQ:
    return OPCODE_BR_EQ;
  case TType::NEQ:
    return OPCODE_BR_NEQ;
  default:
    tac_error(
        std::format("Unexpected operator {}", TTypeToString(ttype)).c_str());
  }
}

// * Helper macros for avoiding excess branching in interpreter or compiler.
#define IS_BINARY(n) ((n >= OPCODE_ASSIGN) && (n <= OPCODE_SHR))
#define IS_UNARY(n) ((n >= OPCODE_NOT) && (n <= OPCODE_NEGATE))
#define IS_CONTROL_FLOW(n) ((n >= OPCODE_BR_EQ) && (n <= OPCODE_JUMP))
#define IS_MEMORY(n) ((n >= OPCODE_LOAD) && (n <= OPCODE_FREE))
#define IS_FUNCTION(n) ((n >= OPCODE_PUSH_ARG) && (n <= OPCODE_RETURN_VOID))

enum Tag {
  VALUE_TAG_NONE,
  VALUE_TAG_REFERENCE,
  VALUE_TAG_NULL,
  VALUE_TAG_INTEGER,
  VALUE_TAG_FLOAT,
  VALUE_TAG_STRING,
  VALUE_TAG_ARRAY,
  VALUE_TAG_STRUCT,
};

struct Value {
  Type *type;
  Tag tag;
  union {
    int reference;
    int _integer;
    float _float;
    char *_string;
    int array_temp;
    int struct_temp;
  };

  std::string to_string() const {
    switch (tag) {
    case VALUE_TAG_REFERENCE:
      return std::format("reference: {}", reference);
    case VALUE_TAG_NULL:
      return "null";
    case VALUE_TAG_INTEGER:
      return std::format("integer: {}", _integer);
    case VALUE_TAG_FLOAT:
      return std::format("float: {}", _float);
    case VALUE_TAG_STRING:
      return std::format("string: {}", _string);
    case VALUE_TAG_ARRAY:
      return std::format("array_temp: {}", array_temp);
    case VALUE_TAG_STRUCT:
      return std::format("struct_temp: {}", struct_temp);
    default:
      return "unknown";
    }
  }
};

struct TAC {
  Opcode opcode;
  Value left;
  Value right;
  size_t destination;
  
  std::string to_string() const {
    return std::format("left: {}, right: {}, opcode: {}, destination: {}", left.to_string(), right.to_string(), (int)opcode, destination);
  }
};

static int last_temp{};
inline int get_temp() { return last_temp++; }

static int last_label{};
inline int get_label() { return last_label++; }

struct TACSymbol {
  int index;
  Type *type;
};

struct TACVisitor : VisitorBase {
  std::unordered_map<std::string, TACSymbol> symbols;

  std::vector<TAC> codes;
  inline void emit(const uint64_t &destination, const Opcode &opcode,
                   const Value &left = {}, const Value &right = {}) {
    codes.push_back({
        .opcode = opcode,
        .left = left,
        .right = right,
        .destination = destination,
    });
  }
  inline explicit TACVisitor(Context &ctx) : ctx(ctx), type_visitor(ctx) {
    codes.reserve(5000);
  }
  inline Value value_of(ASTExpr *node) {
    return std::any_cast<Value>(node->accept(this));
  }
  inline Type *type_of(ASTNode *node) {
    return global_get_type(std::any_cast<int>(node->accept(&type_visitor)));
  }
  inline Type *type_of(const ASTType *type) {
    return global_get_type(type->resolved_type);
  }
  inline Type *type_of(const int &type) { return global_get_type(type); }

  TypeVisitor type_visitor;
  Context &ctx;

  std ::any visit(ASTProgram *node) override {
    for (const auto &statement : node->statements) {
      statement->accept(this);
    }
    return {};
  }

  std ::any visit(ASTBlock *node) override {
    auto previous = ctx.scope;
    ctx.set_scope(node->scope);
    for (const auto &statement : node->statements) {
      statement->accept(this);
    }
    ctx.set_scope(node->scope);
    return {};
  }
  std::any visit(ASTFunctionDeclaration *node) override {
    auto label = get_label();
    symbols[node->name.value] = {
        label, type_of(ctx.scope->lookup(node->name.value)->type_id)};
    emit(label, OPCODE_LABEL);
    emit(0, OPCODE_ENTER);
    node->params->accept(this);
    if (node->block) {
      node->block.get()->accept(this);
    }
    emit(0, OPCODE_LEAVE);
    return {};
  }
  std::any visit(ASTParamsDecl *node) override {
    for (const auto &param : node->params) {
      param->accept(this);
    }
    return {};
  }
  std::any visit(ASTParamDecl *node) override {
    emit(get_temp(), OPCODE_POP_ARG);
    return {};
  }
  std::any visit(ASTDeclaration *node) override {
    if (node->value)
      emit(get_temp(), OPCODE_ASSIGN, value_of(node->value.get()));
    else
      emit(get_temp(), OPCODE_ASSIGN);
    return {};
  }
  std ::any visit(ASTExprStatement *node) override {
    node->expression->accept(this);
    return {};
  }
  std::any visit(ASTBinExpr *node) override {
    auto left = std::any_cast<Value>(node->left->accept(this));
    auto right = std::any_cast<Value>(node->right->accept(this));
    int temp = get_temp();
    emit(temp, op_from_ttype(node->op.type), left, right);
    return Value{.type = type_of(node->resolved_type),
                 .tag = VALUE_TAG_INTEGER,
                 ._integer = temp};
  }
  std::any visit(ASTUnaryExpr *node) override {
    auto operand = std::any_cast<Value>(node->operand->accept(this));
    int temp = get_temp();
    emit(temp, op_from_ttype(node->op.type), operand);
    return Value{.type = type_of(node->operand),
                 .tag = VALUE_TAG_INTEGER,
                 ._integer = temp};
  }
  std::any visit(ASTIdentifier *node) override {
    auto symbol = symbols[node->value.value];
    return Value{.type = symbol.type,
                 .tag = VALUE_TAG_REFERENCE,
                 .reference = symbol.index};
  }
  std::any visit(ASTLiteral *node) override {
    Value value;
    value.type = type_of(node);
    switch (node->tag) {
    case ASTLiteral::Null:
      value.tag = VALUE_TAG_NULL;
      break;
    case ASTLiteral::Bool:
    case ASTLiteral::Integer:
      value.tag = VALUE_TAG_INTEGER;
      value._integer = stoll(node->value);
      break;
    case ASTLiteral::Float:
      value.tag = VALUE_TAG_FLOAT;
      value._float = stof(node->value);
      break;
    case ASTLiteral::Char:
    case ASTLiteral::String:
      value.tag = VALUE_TAG_STRING;
      value._string = strdup(node->value.c_str());
      break;
    default:
      tac_error("Unknown literal type");
    }
    return value;
  }
  std::any visit(ASTCall *node) override {
    for (const auto &arg : node->arguments->arguments) {
      emit(0, OPCODE_PUSH_ARG, value_of(arg));
    }
    emit(get_temp(), OPCODE_CALL,
         Value{.type = type_of(node->type),
               .tag = VALUE_TAG_INTEGER,
               .reference = symbols[node->name.value].index});
    return {};
  }
  std::any visit(ASTArguments *node) override {
    for (const auto &arg : node->arguments) {
      arg->accept(this);
    }
    return {};
  }
  std::any visit(ASTReturn *node) override {
    if (node->expression) {
      emit(0, OPCODE_RETURN, value_of(node->expression.get()));
    } else {
      emit(0, OPCODE_RETURN_VOID);
    }
    return {};
  }
  std::any visit(ASTType *node) override {
    // Handle type node
    return {};
  }
  std::any visit(ASTContinue *node) override {
    emit(0, OPCODE_JUMP); // Assuming a jump to the start of the loop
    return {};
  }
  std::any visit(ASTBreak *node) override {
    emit(0, OPCODE_JUMP); // Assuming a jump to the end of the loop
    return {};
  }
  std::any visit(ASTFor *node) override {return{};}
  
  std::any visit(ASTIf *node) override {
    auto cond_value = value_of(node->condition);
    auto else_label = get_label();
    emit(else_label, OPCODE_BR_EQ, cond_value,
         Value{.tag = VALUE_TAG_INTEGER, ._integer = 0});
    node->block->accept(this);
    if (node->_else) {
      auto end_label = get_label();
      emit(end_label, OPCODE_JUMP);
      emit(else_label, OPCODE_LABEL);
      node->_else.get()->accept(this);
      emit(end_label, OPCODE_LABEL);
    } else {
      emit(else_label, OPCODE_LABEL);
    }
    return {};
  }
  std::any visit(ASTElse *node) override { return {}; }

  std::any visit(ASTWhile *node) override {
    auto start_label = get_label();
    emit(start_label, OPCODE_LABEL);
    auto cond_value = value_of(node->condition.get());
    auto end_label = get_label();
    emit(end_label, OPCODE_BR_EQ, cond_value,
         Value{.tag = VALUE_TAG_INTEGER, ._integer = 0});
    node->block->accept(this);
    emit(start_label, OPCODE_JUMP);
    emit(end_label, OPCODE_LABEL);
    return {};
  }
  std::any visit(ASTStructDeclaration *node) override {
    // Handle struct declaration
    return {};
  }
  std::any visit(ASTDotExpr *node) override {
    return {};
  }
  std::any visit(ASTSubscript *node) override {
    return {};
  }
  std::any visit(ASTMake *node) override {
    // Handle make expression
    return {};
  }
  std::any visit(ASTInitializerList *node) override {
    // Handle initializer list
    return {};
  }
  std::any visit(ASTEnumDeclaration *node) override {
    // Handle enum declaration
    return {};
  }
  std::any visit(ASTUnionDeclaration *node) override {
    // Handle union declaration
    return {};
  }
  std::any visit(ASTAllocate *node) override {
   
  }
  std::any visit(ASTRange *node) override {
    // Handle range expression
    return {};
  }
  std::any visit(ASTSwitch *node) override {
    // Handle switch statement
    return {};
  }
  std::any visit(ASTTuple *node) override {
    // Handle tuple expression
    return {};
  }
  std::any visit(ASTTupleDeconstruction *node) override {
    // Handle tuple deconstruction
    return {};
  }
};
