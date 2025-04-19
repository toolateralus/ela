#pragma once

#include "type.hpp"

enum struct THIRNodeType : unsigned char {

  Path,
  Type,
  BinExpr,
  UnaryExpr,
  Literal,
  Tuple,
  Arguments,
  Call,
  DotExpr,
  MethodCall,
  Cast,
  Lambda,
  Range,
  Index,
  InitializerList,

  PatternMatch,

  Size_Of,
  Dyn_Of,
  Type_Of,

  Return,
  Break,
  Continue,
  For,
  If,
  Else,
  While,
  Switch,
  Defer,
  Variable,
  Block,

  TupleDeconstruction,
};

struct THIR {
  THIR() {}
  Type *type;
  SourceRange source_range;
  virtual ~THIR() {}
  virtual THIRNodeType get_node_type() const = 0;
};

struct THIRBlock : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Block; }
};

struct THIRPath : THIR {

  THIRNodeType get_node_type() const override { return THIRNodeType::Path; }
};

struct THIRType : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Type; }
};

struct THIRBinExpr : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::BinExpr; }
};

struct THIRUnaryExpr : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::UnaryExpr; }
};

struct THIRLiteral : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Literal; }
};

struct THIRTuple : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Tuple; }
};

struct THIRArguments : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Arguments; }
};

struct THIRCall : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Call; }
};

struct THIRDotExpr : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::DotExpr; }
};

struct THIRMethodCall : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::MethodCall; }
};

struct THIRCast : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Cast; }
};

struct THIRLambda : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Lambda; }
};

struct THIRRange : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Range; }
};

struct THIRIndex : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Index; }
};

struct THIRPatternMatch : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::PatternMatch; }
};

struct THIRSizeOf : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Size_Of; }
};

struct THIRDynOf : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Dyn_Of; }
};

struct THIRTypeOf : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Type_Of; }
};

struct THIRReturn : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Return; }
};

struct THIRBreak : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Break; }
};

struct THIRContinue : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Continue; }
};

struct THIRFor : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::For; }
};

struct THIRIf : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::If; }
};

struct THIRElse : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Else; }
};

struct THIRWhile : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::While; }
};

struct THIRSwitch : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Switch; }
};

struct THIRDefer : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Defer; }
};

struct THIRVariable : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::Variable; }
};

struct THIRTupleDeconstruction : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::TupleDeconstruction; }
};

struct THIRInitializerList : THIR {
  THIRNodeType get_node_type() const override { return THIRNodeType::InitializerList; }
};

struct THIRFunction {
  // includes parameters and return type of course.
  Type *type;
  // Where the meat of the executable portion of the program resides.
  THIRBlock block;

  size_t id;
};

struct THIRContext {
  std::vector<THIRFunction> functions;
  inline size_t insert_function(const THIRFunction &fn) {
    size_t id = functions.size();
    functions.push_back(std::move(fn));
    return id;
  }
};
