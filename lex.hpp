#pragma once

#include <deque>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

enum struct TType {
  Eof = -1,
  Identifier,
  Integer,
  Float,
  String,

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
  BitwiseNot,
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
  Directive,
  Struct,
  ColonEquals,
  Enum,
  Union,
  New,
  Delete,
};

#define TTYPE_CASE(type)                                                       \
  case TType::type:                                                            \
    return #type

static inline std::string TTypeToString(TType type) {
  switch (type) {
    TTYPE_CASE(Union);
    TTYPE_CASE(Directive);
    TTYPE_CASE(Enum);
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
    TTYPE_CASE(SHL);
    TTYPE_CASE(SHR);
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
    TTYPE_CASE(Not);
    TTYPE_CASE(BitwiseNot);

    TTYPE_CASE(Increment);
    TTYPE_CASE(Decrement);

    TTYPE_CASE(Return);
    TTYPE_CASE(Break);
    TTYPE_CASE(Continue);
    TTYPE_CASE(Struct);
    TTYPE_CASE(For);
    TTYPE_CASE(While);
    TTYPE_CASE(If);
    TTYPE_CASE(Else);

    TTYPE_CASE(CompAdd);
    TTYPE_CASE(CompSub);
    TTYPE_CASE(CompMul);
    TTYPE_CASE(CompDiv);
    TTYPE_CASE(CompMod);
    TTYPE_CASE(New);
    TTYPE_CASE(Delete);
    TTYPE_CASE(CompAnd);
    TTYPE_CASE(CompOr);
    TTYPE_CASE(CompXor);
    TTYPE_CASE(CompSHL);
    TTYPE_CASE(CompSHR);
  }
  return "Unknown";
}

enum struct TFamily {
  Literal,
  Operator,
  Keyword,
  Identifier,
};

struct SourceLocation {
  SourceLocation() {}
  SourceLocation(size_t line, size_t column, std::size_t file)
      : line(line), column(column), file(file) {}
  size_t line = 0, column = 0;
  size_t file = 0;

  static std::vector<std::string> &files() {
    static std::vector<std::string> files;
    return files;
  }
  std::string ToString() const {
    return files()[file] + ":" + std::to_string(line) + ":" +
           std::to_string(column);
  }
};

struct Token {
  inline bool is_relational() const {
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
  inline bool is_comp_assign() const {
    return type == TType::CompAdd || type == TType::CompSub ||
           type == TType::CompMul || type == TType::CompDiv ||
           type == TType::CompMod || type == TType::CompAnd ||
           type == TType::CompOr || type == TType::CompXor ||
           type == TType::CompSHL || type == TType::CompSHR;
  }

  Token() {}
  Token(SourceLocation location, std::string value, TType type, TFamily family)
      : value(std::move(value)), type(type), family(family),
        location(location) {}
  std::string value;
  TType type;
  TFamily family;
  SourceLocation location;
  static Token &Eof() {
    static Token eof =
        Token(SourceLocation(0, 0, 0), "", TType::Eof, TFamily::Operator);
    return eof;
  }

  bool is_eof() const { return type == TType::Eof; }
};

struct Lexer {
  struct State {
    State(const std::string &input, size_t file_idx, size_t input_len)
        : input(input), file_idx(file_idx), input_len(input_len) {}

    std::string input{};
    std::deque<Token> lookahead_buffer{};
    size_t pos = 0;
    size_t col = 1;
    size_t line = 1;
    size_t file_idx{};
    size_t input_len{};

    static State from_file(const std::string &input,
                           const std::string &filename) {
      auto canonical = std::filesystem::canonical(filename);
      auto path = canonical.string();
      bool found = false;
      size_t file_idx = 0;
      for (const auto &file : SourceLocation::files()) {
        if (file == path) {
          found = 1;
        } else {
          ++file_idx;
        }
      }
      if (!found) {
        SourceLocation::files().push_back(path);
      }
      return State(input, file_idx, input.length());
    }
  };

  const std::unordered_map<std::string, TType> keywords{
      {"union", TType::Union},
      {"enum", TType::Enum},
      {"return", TType::Return},
      {"break", TType::Break},
      {"continue", TType::Continue},

      {"for", TType::For},
      {"while", TType::While},
      {"if", TType::If},
      {"else", TType::Else},
      {"struct", TType::Struct},
      {"true", TType::True},
      {"false", TType::False},
      {"null", TType::Null},
      {"new", TType::New},
      {"delete", TType::Delete},
      };
      

  const std::unordered_map<std::string, TType> operators{
      {":=", TType::ColonEquals },
      {"...", TType::Varargs},  {"#", TType::Directive},
      {".", TType::Dot},        {"!", TType::Not},
      {"~", TType::BitwiseNot}, {"::", TType::DoubleColon},
      {"->", TType::Arrow},     {"..", TType::Range},
      {"+", TType::Add},        {"-", TType::Sub},
      {"*", TType::Mul},        {"/", TType::Div},
      {"%", TType::Modulo},     {"=", TType::Assign},
      {",", TType::Comma},      {";", TType::Semi},
      {"(", TType::LParen},     {")", TType::RParen},
      {"{", TType::LCurly},     {"}", TType::RCurly},
      {"|", TType::Or},         {"&", TType::And},
      {"||", TType::LogicalOr}, {"&&", TType::LogicalAnd},
      {"<<", TType::SHL},       {">>", TType::SHR},
      {"^", TType::Xor},        {"<", TType::LT},
      {">", TType::GT},         {"==", TType::EQ},
      {"!=", TType::NEQ},       {"<=", TType::LE},
      {">=", TType::GE},        {"[", TType::LBrace},
      {"]", TType::RBrace},     {"++", TType::Increment},
      {"--", TType::Decrement},

      {"+=", TType::CompAdd},   {"-=", TType::CompSub},
      {"*=", TType::CompMul},   {"/=", TType::CompDiv},
      {"%=", TType::CompMod},   {"&=", TType::CompAnd},
      {"|=", TType::CompOr},    {"^=", TType::CompXor},
      {"<<=", TType::CompSHL},  {">>=", TType::CompSHR}};

  void get_token(State &state);
};
