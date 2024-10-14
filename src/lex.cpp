#include "lex.hpp"
#include <cctype>
#include <iostream>
#include <sstream>

using std::string;
using std::stringstream;

void Lexer::get_token(State &state) {

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
    
    if (c == '/' && pos + 1 < len && input[pos + 1] == '*') {
      pos += 2;
      while (pos + 1 < len && !(input[pos] == '*' && input[pos + 1] == '/')) {
        if (input[pos] == '\n') {
          lines++;
        }
        pos++;
      }
      pos += 2;
      continue;
    }

    size_t col = lines == 0 ? pos : pos / lines;
    SourceLocation location{state.line, state.col, state.file_idx};

    if (c == '"') {
      pos++;
      c = input[pos];
      while (pos < len) {
        c = input[pos];
        if (c == '"') break;
        else if (c == '\n') lines++;
        else if (c == '\\') {
          if (pos + 1 < len) {
            token.put(c);
            pos++;
            token.put(input[pos]);
            pos++;
          } else {
            std::cout << location.ToString();
            std::cout << "\nela: incomplete escape sequence at end of input\n";
            exit(1);
          }
        } else {
          token.put(c);
          pos++;
          c = input[pos];
        }
      }
      pos++;
      state.lookahead_buffer.push_back(
          Token(location, token.str(), TType::String, TFamily::Literal));
      return;
    }

    if (std::isalpha(c) || c == '_') {
      while (pos < len && (std::isalnum(c) || c == '_')) {
        token.put(c);
        pos++;
        c = input[pos];
      }
      string value = token.str();
      if (keywords.contains(value)) {
        state.lookahead_buffer.push_back(
            Token(location, value, keywords.at(value), TFamily::Keyword));
        return;
      } else {
        state.lookahead_buffer.push_back(
            Token(location, value, TType::Identifier, TFamily::Identifier));
        return;
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
            state.lookahead_buffer.push_back(Token(location, longest_match,
                                                   operators.at(longest_match),
                                                   TFamily::Operator));
            return;
          }
        }
        pos++;
        c = input[pos];
      }

      if (!longest_match.empty()) {
        state.lookahead_buffer.push_back(Token(location, longest_match,
                                               operators.at(longest_match),
                                               TFamily::Operator));
        return;
      } else {
        std::cout << location.ToString();
        std::cout << "\nela: unable to lex operator: " << current_match
                  << std::endl;
        exit(1);
      }
    } else if (std::isdigit(c)) {
      bool is_float = false;
      bool is_hex = false;
      bool is_bin = false;
      int startPos = pos;

      if (c == '0' && (input[pos + 1] == 'x' || input[pos + 1] == 'X')) {
        is_hex = true;
        pos += 2; // Skip '0x'
        c = input[pos];
      } else if (c == '0' && (input[pos + 1] == 'b' || input[pos + 1] == 'B')) {
        is_bin = true;
        pos += 2; // Skip '0b'
        c = input[pos];
      }

      while (pos < len && 
             (std::isdigit(c) || c == '.' || (is_hex && std::isxdigit(c)) ||
              (is_bin && (c == '0' || c == '1')) || c == '_')) {
        if (c == '_') {
          pos++;
          c = input[pos];
        }
        if (c == '.') {
          if (is_float) {
            auto str = token.str();
            str.pop_back();
            pos++;
            state.lookahead_buffer.push_back(
                Token(location, str, TType::Integer, TFamily::Literal));
                
            state.lookahead_buffer.emplace_back(location, "..", TType::Range,
                                                TFamily::Operator);
            return;
          }
          is_float = true;
        }
        token.put(c);
        pos++;
        c = input[pos];
        
        if (c == '_') {
          pos++;
          c = input[pos];
        }
        
      }
      auto value = token.str();
      
      if (is_hex) {
        state.lookahead_buffer.push_back(Token(location, "0x" + value, TType::Integer, TFamily::Literal));
      } else if (is_bin) {
        state.lookahead_buffer.push_back(
            Token(location, "0b" + value, TType::Integer, TFamily::Literal));
      } else if (!is_float) {
        state.lookahead_buffer.push_back(
            Token(location, value, TType::Integer, TFamily::Literal));
      } else {
        state.lookahead_buffer.push_back(
            Token(location, value, TType::Float, TFamily::Literal));
      }
      return;
    } else {
      std::cout << location.ToString();
      std::cout << "\nela: unable to lex : " << c << std::endl;
      exit(1);
    }
  }
  state.lookahead_buffer.push_back(Token::Eof());
}
