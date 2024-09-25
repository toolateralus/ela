#include "lex.hpp"
#include <cctype>
#include <iostream>
#include <sstream>
#include <vector>

using std::string;
using std::stringstream;

Token Lexer::get_token(State &state) {
 if (!state.queue.empty()) {
    auto tok = state.queue.back();
    state.queue.pop_back();
    return tok;
  }
  size_t &pos = state.pos;
  const std::string &input = state.input;
  const size_t len = state.input_len;
  size_t &lines = state.line;
  std::stringstream token;
  
  while (pos < len) {
    token.clear();
    
    char c = input[pos];

    if (c == '\n') {
      pos++;
      lines++;
      continue;
    }

    if (c == ' ' || c == '\t') {
      pos++;
      continue;
    }

    if (c == '/' && pos + 1 < len && input[pos + 1] == '/') {
      pos += 2;
      size_t newlinePos = input.find('\n', pos);
      pos = (newlinePos != std::string::npos) ? newlinePos + 1 : len;
      lines += (newlinePos != std::string::npos);
      continue;
    }


    size_t col = lines == 0 ? pos : pos / lines;
    SourceLocation location {state.line, state.col, state.file_idx};
    
    if (c == '"') {
      pos++;
      c = input[pos];
      while (c != '"') {
        if (c == '\\' && pos + 1 < len) {
          switch (input[pos + 1]) {
            case 'n':
              token.put('\n');
              break;
            case 't':
              token.put('\t');
              break;
            case 'e':
              token.put('\e');
              break;
            case '\\':
              token.put('\\');
              break;
            case '"':
              token.put('\"');
              break;
            case 'r':
              token.put('\r');
              break;
            case 'b':
              token.put('\b');
              break;
            case 'f':
              token.put('\f');
              break;
            case 'v':
              token.put('\v');
              break;
            default:
              std::cout << "crisp: unable to lex escape char: " << input[pos + 1] << std::endl;
              exit(1);
              break;
          }
          pos += 2;
        } else {
          if (c == '\n') 
            lines++;
          token.put(c);
          pos++;
        }
        c = input[pos];
      }
      pos++;
      return Token(location, token.str(), TType::String, TFamily::Literal);
    }
    
    if (std::isalpha(c)) {
      while (pos < len && (std::isalnum(c) || c == '_')) {
        token.put(c);
        pos++;
        c = input[pos];
      }
      string value = token.str();
      if (keywords.contains(value)) {
        return Token(location, value, keywords.at(value), TFamily::Keyword);
      } else {
        return Token(location, value, TType::Identifier, TFamily::Identifier);
      }
    } else if (std::ispunct(c)) {
      std::string longest_match;
      std::string current_match;
      while (pos < len && std::ispunct(c)) {
        if (c == '\"') {
          break;
        }
        current_match += input[pos];
        if (operators.find(current_match) != operators.end()) {
          longest_match = current_match;
        } else {
          if (!longest_match.empty()) {
            return Token(location, longest_match, operators.at(longest_match), TFamily::Operator);
          }
        }
        pos++;
        c = input[pos];
      }
      
      if (!longest_match.empty()) {
        return Token(location, longest_match, operators.at(longest_match), TFamily::Operator);
      } else {
        std::cout << "crisp: unable to lex operator: " << current_match << std::endl;
        exit(1);
      }
    } else if (std::isdigit(c)) {
      bool is_float = false;
      int startPos = pos;
      while (pos < len && (std::isdigit(c) || c == '.')) {
        if (c == '.') {
          if (is_float) {
            auto str = token.str();
            str.pop_back();
            pos++;
            state.queue.emplace_back(location, "..", TType::Range, TFamily::Operator);
            return Token(location, str, TType::Integer, TFamily::Literal);
          }
          is_float = true;
        }
        token.put(c);
        pos++;
        c = input[pos];
      }
      auto value = token.str();

      if (!is_float)
        return Token(location, value, TType::Integer, TFamily::Literal);
      else
        return Token(location, value, TType::Float, TFamily::Literal);
    } else {
      std::cout << "crisp: unable to lex : " << c << std::endl;
      exit(1);
    }
  }
  return Token::Eof();
}
