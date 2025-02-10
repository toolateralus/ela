#pragma once

#include <deque>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "interned_string.hpp"

enum struct Token_Type: char {
  Eof = -1,
  Identifier,
  Integer,
  Float,
  String,
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
  Directive,   // #
  ColonEquals, //  :=

  Struct,
  Enum,
  Union,

  Then,
  Colon,
  In,

  Switch,
  Fn,

  GenericBrace,   // '!<' for ![T, T1]
  As,             // 'as' for casting
  ExpressionBody, // => for expr body, implicit return expr where a block was otherwise expected.
  Defer,

  Impl,      // impl
  Interface, // interface
  Where,
  Is,
  Size_Of,
};

#define Token_Type_Case(type)                                                                                               \
  case Token_Type::type:                                                                                                    \
    return #type

static inline std::string Token_Type_To_String(Token_Type type) {
  switch (type) {
    Token_Type_Case(Size_Of);
    Token_Type_Case(Interface);
    Token_Type_Case(Where);
    Token_Type_Case(Defer);
    Token_Type_Case(Is);
    Token_Type_Case(Char);
    Token_Type_Case(ExpressionBody);
    Token_Type_Case(GenericBrace);
    Token_Type_Case(Fn);
    Token_Type_Case(Colon);
    Token_Type_Case(Switch);
    Token_Type_Case(In);
    Token_Type_Case(Then);
    Token_Type_Case(Union);
    Token_Type_Case(Directive);
    Token_Type_Case(Enum);
    Token_Type_Case(ColonEquals);
    Token_Type_Case(Varargs);
    Token_Type_Case(True);
    Token_Type_Case(False);
    Token_Type_Case(Null);
    Token_Type_Case(Identifier);
    Token_Type_Case(Integer);
    Token_Type_Case(Float);
    Token_Type_Case(String);
    Token_Type_Case(Assign);
    Token_Type_Case(Add);
    Token_Type_Case(Sub);
    Token_Type_Case(Mul);
    Token_Type_Case(Div);
    Token_Type_Case(Modulo);
    Token_Type_Case(Range);
    Token_Type_Case(Arrow);
    Token_Type_Case(Comma);
    Token_Type_Case(Semi);
    Token_Type_Case(Or);
    Token_Type_Case(And);
    Token_Type_Case(SHL);
    Token_Type_Case(SHR);
    Token_Type_Case(Xor);
    Token_Type_Case(LogicalOr);
    Token_Type_Case(LogicalAnd);
    Token_Type_Case(LT);
    Token_Type_Case(GT);
    Token_Type_Case(EQ);
    Token_Type_Case(NEQ);
    Token_Type_Case(LE);
    Token_Type_Case(GE);
    Token_Type_Case(LParen);
    Token_Type_Case(RParen);
    Token_Type_Case(LCurly);
    Token_Type_Case(RCurly);
    Token_Type_Case(LBrace);
    Token_Type_Case(RBrace);
    Token_Type_Case(Eof);
    Token_Type_Case(DoubleColon);
    Token_Type_Case(Dot);
    Token_Type_Case(LogicalNot);
    Token_Type_Case(Not);

    Token_Type_Case(Increment);
    Token_Type_Case(Decrement);

    Token_Type_Case(Return);
    Token_Type_Case(Break);
    Token_Type_Case(Continue);
    Token_Type_Case(Struct);
    Token_Type_Case(For);
    Token_Type_Case(While);
    Token_Type_Case(If);
    Token_Type_Case(Else);

    Token_Type_Case(CompAdd);
    Token_Type_Case(CompSub);
    Token_Type_Case(CompMul);
    Token_Type_Case(CompDiv);
    Token_Type_Case(CompMod);
    Token_Type_Case(CompAnd);
    Token_Type_Case(CompOr);
    Token_Type_Case(CompXor);
    Token_Type_Case(CompSHL);
    Token_Type_Case(CompSHR);
    Token_Type_Case(As);
    Token_Type_Case(Impl);
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
  SourceLocation(size_t line, size_t column, std::size_t file) : line(line), column(column), file(file) {}
  size_t line = 0, column = 0;
  size_t file = 0;

  static std::vector<std::string> &files() {
    static std::vector<std::string> files;
    return files;
  }
  std::string ToString() const { return files()[file] + ":" + std::to_string(line) + ":" + std::to_string(column); }
};

struct Token {
  inline bool is_relational() const {
    switch (type) {
      case Token_Type::LT:
      case Token_Type::GT:
      case Token_Type::EQ:
      case Token_Type::NEQ:
      case Token_Type::LE:
      case Token_Type::GE:
      case Token_Type::LogicalOr:
      case Token_Type::LogicalAnd:
        return true;
      default:
        return false;
    }
  }
  inline bool is_comp_assign() const {
    return type == Token_Type::CompAdd || type == Token_Type::CompSub || type == Token_Type::CompMul || type == Token_Type::CompDiv ||
           type == Token_Type::CompMod || type == Token_Type::CompAnd || type == Token_Type::CompOr || type == Token_Type::CompXor ||
           type == Token_Type::CompSHL || type == Token_Type::CompSHR;
  }

  Token() {}

  Token(SourceLocation location, Interned_String value, Token_Type type, TFamily family)
      : value(std::move(value)), type(type), family(family), location(location) {}
  Interned_String value;
  Token_Type type;
  TFamily family;
  SourceLocation location;
  static Token &Eof() {
    static Token eof = Token(SourceLocation(0, 0, 0), {""}, Token_Type::Eof, TFamily::Operator);
    return eof;
  }

  bool is_eof() const { return type == Token_Type::Eof; }
};

static std::unordered_map<std::string, Token_Type> keywords{
    // control flow
    {"in", Token_Type::In},
    {"where", Token_Type::Where},
    {"is", Token_Type::Is},
    {"sizeof", Token_Type::Size_Of},
    {"fn", Token_Type::Fn},
    {"switch", Token_Type::Switch},
    {"then", Token_Type::Then},
    {"return", Token_Type::Return},
    {"break", Token_Type::Break},
    {"continue", Token_Type::Continue},
    {"for", Token_Type::For},
    {"while", Token_Type::While},
    {"if", Token_Type::If},
    {"else", Token_Type::Else},
    // type declarations
    {"struct", Token_Type::Struct},
    {"union", Token_Type::Union},
    {"enum", Token_Type::Enum},
    // literals
    {"true", Token_Type::True},
    {"false", Token_Type::False},
    {"null", Token_Type::Null},

    // miscellaneous
    {"as", Token_Type::As},
    {"impl", Token_Type::Impl},
    {"defer", Token_Type::Defer},
    {"interface", Token_Type::Interface},
};

static std::unordered_map<std::string, Token_Type> operators{{"=>", Token_Type::ExpressionBody},
                                                        {":", Token_Type::Colon},
                                                        {":=", Token_Type::ColonEquals},
                                                        {"...", Token_Type::Varargs},
                                                        {"#", Token_Type::Directive},
                                                        {".", Token_Type::Dot},
                                                        {"!", Token_Type::LogicalNot},
                                                        {"~", Token_Type::Not},
                                                        {"::", Token_Type::DoubleColon},
                                                        {"->", Token_Type::Arrow},
                                                        {"..", Token_Type::Range},
                                                        {"+", Token_Type::Add},
                                                        {"-", Token_Type::Sub},
                                                        {"*", Token_Type::Mul},
                                                        {"/", Token_Type::Div},
                                                        {"%", Token_Type::Modulo},
                                                        {"=", Token_Type::Assign},
                                                        {",", Token_Type::Comma},
                                                        {";", Token_Type::Semi},
                                                        {"(", Token_Type::LParen},
                                                        {")", Token_Type::RParen},
                                                        {"{", Token_Type::LCurly},
                                                        {"}", Token_Type::RCurly},
                                                        {"|", Token_Type::Or},
                                                        {"&", Token_Type::And},
                                                        {"||", Token_Type::LogicalOr},
                                                        {"&&", Token_Type::LogicalAnd},
                                                        {"<<", Token_Type::SHL},
                                                        {">>", Token_Type::SHR},
                                                        {"^", Token_Type::Xor},
                                                        {"<", Token_Type::LT},
                                                        {">", Token_Type::GT},
                                                        {"==", Token_Type::EQ},
                                                        {"!=", Token_Type::NEQ},
                                                        {"<=", Token_Type::LE},
                                                        {">=", Token_Type::GE},
                                                        {"[", Token_Type::LBrace},
                                                        {"]", Token_Type::RBrace},
                                                        {"++", Token_Type::Increment},
                                                        {"--", Token_Type::Decrement},
                                                        {"+=", Token_Type::CompAdd},
                                                        {"-=", Token_Type::CompSub},
                                                        {"*=", Token_Type::CompMul},
                                                        {"/=", Token_Type::CompDiv},
                                                        {"%=", Token_Type::CompMod},
                                                        {"&=", Token_Type::CompAnd},
                                                        {"|=", Token_Type::CompOr},
                                                        {"^=", Token_Type::CompXor},
                                                        {"<<=", Token_Type::CompSHL},
                                                        {">>=", Token_Type::CompSHR},
                                                        {"![", Token_Type::GenericBrace}};

struct Lexer {
  struct State {
    bool operator==(const Lexer::State &other) const { return other.input == input; }

    State(const std::string &input, size_t file_idx, size_t input_len, const std::filesystem::path &path)
        : input(input), file_idx(file_idx), path(path), input_len(input_len) {}

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
      return State(input, file_idx, input.length(), canonical);
    }
  };

  void get_token(State &state);
};
