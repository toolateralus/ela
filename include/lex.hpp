#pragma once

#include <deque>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "interned_string.hpp"

enum struct TType {
  Eof = -1,
  Identifier,
  Integer,
  Float,
  String,
  MultiLineString,
  Char,

  Assign,
  Add,
  Sub,
  Mul,
  Div,
  Modulo,
  Range,
  Arrow,
  Comma,
  Semi,

  Not,
  LogicalNot,
  Or,
  And,
  SHL,
  SHR,
  Xor,
  LogicalOr,
  LogicalAnd,
  LT,
  GT,
  EQ,
  NEQ,
  LE,
  GE,
  LParen,
  RParen,
  LCurly,
  RCurly,
  LBrace,
  RBrace,
  DoubleColon,
  Dot,

  Increment,
  Decrement,

  Return,
  Break,
  Continue,
  For,
  While,
  If,
  Else,

  CompAdd,
  CompSub,
  CompMul,
  CompDiv,
  CompMod,
  CompAnd,
  CompOr,
  CompXor,
  CompSHL,
  CompSHR,

  True,
  False,
  Null,
  Varargs,
  Directive,    // #
  ColonEquals,  //  :=

  Struct,
  Enum,
  Union,

  Then,
  Colon,
  In,

  Switch,
  Fn,

  GenericBrace,  // '!<' for ![T, T1]

  PtrSubscript,  // '![', an explicit operator for pointer arithmetic. with how much implicit typing and index
                 // overloading we have, we need to distinguish the 2, to prevent subtle bugs.

  As,              // 'as' for casting
  ExpressionBody,  // => for expr body, implicit return expr where a block was otherwise expected.
  Defer,

  Impl,   // impl
  Trait,  // trait
  Where,

  Size_Of,
  Type_Of,

  Type,
  Import,
  Module,
  Attribute,
  Mut,
  Const,
  Dyn,
  Dyn_Of,
  Choice,
  Is,
  Extern,

  Self,
  Using,

  Uninitialized,
};

#define TTYPE_CASE(type) \
  case TType::type:      \
    return #type

static inline std::string TTypeToString(TType type) {
  switch (type) {
    TTYPE_CASE(MultiLineString);
    TTYPE_CASE(Using);
    TTYPE_CASE(Extern);
    TTYPE_CASE(Char);
    TTYPE_CASE(ExpressionBody);
    TTYPE_CASE(GenericBrace);
    TTYPE_CASE(Colon);
    TTYPE_CASE(ColonEquals);
    TTYPE_CASE(Varargs);
    TTYPE_CASE(True);
    TTYPE_CASE(False);
    TTYPE_CASE(Null);
    TTYPE_CASE(Identifier);
    TTYPE_CASE(Integer);
    TTYPE_CASE(Float);
    TTYPE_CASE(String);
    TTYPE_CASE(Assign);
    TTYPE_CASE(Add);
    TTYPE_CASE(Sub);
    TTYPE_CASE(Mul);
    TTYPE_CASE(Div);
    TTYPE_CASE(Modulo);
    TTYPE_CASE(Range);
    TTYPE_CASE(Arrow);
    TTYPE_CASE(Comma);
    TTYPE_CASE(Semi);
    TTYPE_CASE(Or);
    TTYPE_CASE(And);
    TTYPE_CASE(SHR);
    TTYPE_CASE(SHL);
    TTYPE_CASE(Xor);
    TTYPE_CASE(LogicalOr);
    TTYPE_CASE(LogicalAnd);
    TTYPE_CASE(LT);
    TTYPE_CASE(GT);
    TTYPE_CASE(EQ);
    TTYPE_CASE(NEQ);
    TTYPE_CASE(LE);
    TTYPE_CASE(GE);
    TTYPE_CASE(LParen);
    TTYPE_CASE(RParen);
    TTYPE_CASE(LCurly);
    TTYPE_CASE(RCurly);
    TTYPE_CASE(LBrace);
    TTYPE_CASE(RBrace);
    TTYPE_CASE(Eof);
    TTYPE_CASE(DoubleColon);
    TTYPE_CASE(Dot);
    TTYPE_CASE(LogicalNot);
    TTYPE_CASE(Not);

    TTYPE_CASE(Increment);
    TTYPE_CASE(Decrement);

    TTYPE_CASE(CompAdd);
    TTYPE_CASE(CompSub);
    TTYPE_CASE(CompMul);
    TTYPE_CASE(CompDiv);
    TTYPE_CASE(CompMod);
    TTYPE_CASE(CompAnd);
    TTYPE_CASE(CompOr);
    TTYPE_CASE(CompXor);
    TTYPE_CASE(CompSHL);
    TTYPE_CASE(CompSHR);

    TTYPE_CASE(As);
    TTYPE_CASE(Impl);
    TTYPE_CASE(Type);
    TTYPE_CASE(Import);
    TTYPE_CASE(Module);
    TTYPE_CASE(Return);
    TTYPE_CASE(Break);
    TTYPE_CASE(Continue);
    TTYPE_CASE(Struct);
    TTYPE_CASE(For);
    TTYPE_CASE(While);
    TTYPE_CASE(If);
    TTYPE_CASE(Else);

    TTYPE_CASE(Size_Of);
    TTYPE_CASE(Type_Of);

    TTYPE_CASE(Trait);
    TTYPE_CASE(Where);
    TTYPE_CASE(Defer);
    TTYPE_CASE(Fn);
    TTYPE_CASE(Switch);
    TTYPE_CASE(In);
    TTYPE_CASE(Then);
    TTYPE_CASE(Union);
    TTYPE_CASE(Directive);
    TTYPE_CASE(Enum);
    TTYPE_CASE(Attribute);

    TTYPE_CASE(Mut);
    TTYPE_CASE(Const);

    TTYPE_CASE(Dyn);
    TTYPE_CASE(Dyn_Of);
    TTYPE_CASE(Choice);
    TTYPE_CASE(Is);
    TTYPE_CASE(PtrSubscript);
    TTYPE_CASE(Self);
    TTYPE_CASE(Uninitialized);
  }
  return "Unknown";
}

enum struct TFamily {
  Literal,
  Operator,
  Keyword,
  Identifier,
};

struct Span {
  Span() {}
  Span(size_t line, size_t column, size_t file, size_t start)
      : line(line), column(column), start(start), file(file), valid(true) {}

  size_t line = 0, column = 0, length = 0, start = 0;
  size_t file = 0;
  bool valid : 1 = false;

  inline void finalize(size_t pos) { length = pos - start; }

  static std::vector<std::string> &files() {
    static std::vector<std::string> files;
    return files;
  }

  static std::string &user_entry_file() {
    static std::string path;
    return path;
  }

  inline std::string to_string() const {
    if (!valid) {
      return "<no src>";
    }
    return files()[file] + ":" + std::to_string(line) + ":" + std::to_string(column);
  }

  inline std::string filename() const { return files()[file]; }
};

static inline bool ttype_is_relational(TType type) {
  switch (type) {
    case TType::LT:
    case TType::GT:
    case TType::EQ:
    case TType::NEQ:
    case TType::LE:
    case TType::GE:
    case TType::LogicalOr:
    case TType::LogicalAnd:
      return true;
    default:
      return false;
  }
}

static inline bool ttype_is_comp_assign(TType type) {
  return type == TType::CompAdd || type == TType::CompSub || type == TType::CompMul || type == TType::CompDiv ||
         type == TType::CompMod || type == TType::CompAnd || type == TType::CompOr || type == TType::CompXor ||
         type == TType::CompSHL || type == TType::CompSHR;
}

struct Token {
  Token() {}

  Token(Span location, InternedString value, TType type, TFamily family)
      : value(std::move(value)), type(type), family(family), span(location) {}
  InternedString value;
  TType type;
  TFamily family;
  Span span;

  static Token &Eof() {
    static Token eof = Token(Span(0, 0, 0, 0), {""}, TType::Eof, TFamily::Operator);
    return eof;
  }

  bool is_eof() const { return type == TType::Eof; }
};

static std::unordered_map<std::string, TType> keywords{
    {"using", TType::Using},
    {"const", TType::Const},
    {"mut", TType::Mut},
    {"module", TType::Module},
    {"import", TType::Import},
    {"type", TType::Type},
    {"extern", TType::Extern},
    // control flow
    {"in", TType::In},
    {"where", TType::Where},
    {"sizeof", TType::Size_Of},
    {"typeof", TType::Type_Of},
    {"fn", TType::Fn},
    {"switch", TType::Switch},
    {"then", TType::Then},
    {"return", TType::Return},
    {"break", TType::Break},
    {"continue", TType::Continue},
    {"for", TType::For},
    {"while", TType::While},
    {"if", TType::If},
    {"else", TType::Else},
    // type declarations
    {"struct", TType::Struct},
    {"union", TType::Union},
    {"enum", TType::Enum},
    // literals
    {"true", TType::True},
    {"false", TType::False},
    {"null", TType::Null},
    // miscellaneous
    {"as", TType::As},
    {"impl", TType::Impl},
    {"defer", TType::Defer},
    {"trait", TType::Trait},
    {"dyn", TType::Dyn},
    {"dynof", TType::Dyn_Of},
    {"choice", TType::Choice},
    // for pattern matching
    {"is", TType::Is},
    {"Self", TType::Self},
};

static std::unordered_map<std::string, TType> operators{
    {"---", TType::Uninitialized},
    {"=>", TType::ExpressionBody},
    {":", TType::Colon},
    {"@", TType::Attribute},
    {":=", TType::ColonEquals},
    {"...", TType::Varargs},
    {"#", TType::Directive},
    {".", TType::Dot},
    {"!", TType::LogicalNot},
    {"~", TType::Not},
    {"::", TType::DoubleColon},
    {"->", TType::Arrow},
    {"..", TType::Range},
    {"+", TType::Add},
    {"-", TType::Sub},
    {"*", TType::Mul},
    {"/", TType::Div},
    {"%", TType::Modulo},
    {"=", TType::Assign},
    {",", TType::Comma},
    {";", TType::Semi},
    {"(", TType::LParen},
    {")", TType::RParen},
    {"{", TType::LCurly},
    {"}", TType::RCurly},
    {"|", TType::Or},
    {"&", TType::And},
    {"||", TType::LogicalOr},
    {"&&", TType::LogicalAnd},
    {"<<", TType::SHL},
    {"^", TType::Xor},
    {"<", TType::LT},
    {">", TType::GT},
    {"==", TType::EQ},
    {"!=", TType::NEQ},
    {"<=", TType::LE},
    {">=", TType::GE},
    {"[", TType::LBrace},
    {"]", TType::RBrace},
    {"++", TType::Increment},
    {"--", TType::Decrement},
    {"+=", TType::CompAdd},
    {"-=", TType::CompSub},
    {"*=", TType::CompMul},
    {"/=", TType::CompDiv},
    {"%=", TType::CompMod},
    {"&=", TType::CompAnd},
    {"|=", TType::CompOr},
    {"^=", TType::CompXor},
    {"<<=", TType::CompSHL},
    {">>=", TType::CompSHR},
    {"!<", TType::GenericBrace},
    {"![", TType::PtrSubscript},
};

void throw_error(const std::string &message, const Span &span);

static inline std::string ttype_get_operator_string(TType type, const Span &range) {
  for (const auto &op : operators) {
    if (op.second == type) {
      return op.first;
    }
  }

  if (type == TType::SHR) {
    return ">>";
  }
  if (type == TType::CompSHR) {
    return ">>=";
  }

  throw_error("invalid operator?", range);
  return {};
}

struct Lexer {
  struct State {
    bool operator==(const Lexer::State &other) const { return other.input == input; }

    State(const std::string &input, size_t file_idx, size_t input_len, const std::filesystem::path &path)
        : input(input), path(path), file_idx(file_idx), input_len(input_len) {}

    std::string input{};
    std::filesystem::path path;
    std::deque<Token> lookahead_buffer{};
    size_t pos = 0;
    size_t col = 1;
    size_t line = 1;
    size_t file_idx{};
    size_t input_len{};

    static State from_string(const std::string &input) { return State(input, 0, input.length(), ""); }

    static State from_file(const std::string &filename) {
      auto canonical = std::filesystem::canonical(filename);
      auto path = canonical.string();
      std::filesystem::current_path(canonical.parent_path());

      if (!std::filesystem::exists(canonical)) {
        printf("File %s does not exist. Quitting..", canonical.string().c_str());
        exit(1);
      }

      std::ifstream file(canonical);
      std::stringstream ss;
      ss << file.rdbuf();
      auto input = ss.str();

      bool found = false;
      size_t file_idx = 0;
      for (const auto &file : Span::files()) {
        if (file == path) {
          found = 1;
        } else {
          ++file_idx;
        }
      }
      if (!found) {
        Span::files().push_back(path);
      }
      return State(input, file_idx, input.length(), canonical);
    }
  };

  void get_token(State &state);
};
