#include "lex.hpp"
#include "error.hpp"
#include <set>

using std::string;
using std::stringstream;

// TODO: if we encounterthese, just prefix them in tokenizer with $ so they become valid identifiers.
// TODO: we should not have reserved words from host language leak into this langauge.
static std::set<std::string> reserved = {"auto",   "break",  "case",    "const",    "continue", "default",  "do",
                                         "double", "else",   "enum",    "extern",   "float",    "for",      "goto",
                                         "if",     "int",    "long",    "register", "return",   "short",    "signed",
                                         "struct", "switch", "typedef", "union",    "unsigned", "volatile", "while"};

void Lexer::get_token(State &state) {
  size_t &pos = state.pos;
  const std::string &input = state.input;
  const size_t len = state.input_len;
  size_t &lines = state.line;
  size_t &col = state.col;
  std::stringstream token;

  while (pos < len) {
    token.clear();

    char c = input[pos];

    if (c == '\n') {
      pos++;
      lines++;
      col = 1; // Reset column position at the start of a new line
      continue;
    }

    if (c == ' ' || c == '\t') {
      pos++;
      col++;
      continue;
    }

    // single comment
    if (c == '/' && pos + 1 < len && input[pos + 1] == '/') {
      pos += 2;
      col += 2;
      size_t newlinePos = input.find('\n', pos);
      if (newlinePos != std::string::npos) {
        lines++;
        col = 1; // Reset column position at the start of a new line
        pos = newlinePos + 1;
      } else {
        pos = len;
      }
      continue;
    }

    // multi line comment
    if (c == '/' && pos + 1 < len && input[pos + 1] == '*') {
      pos += 2;
      col += 2;
      while (pos + 1 < len && !(input[pos] == '*' && input[pos + 1] == '/')) {
        if (input[pos] == '\n') {
          lines++;
          col = 1; // Reset column position at the start of a new line
        } else {
          col++;
        }
        pos++;
      }
      pos += 2;
      col += 2;
      continue;
    }

    SourceLocation location{state.line, state.col, state.file_idx};

    if (c == '\'') {
        auto start = pos;
        pos++; // move past '
        col++;
        c = input[pos];
        uint32_t codepoint = 0;
    
        if (c == '\\') {
            // Handle escape sequences
            pos++;
            col++;
            c = input[pos];
            pos++;
            col++;
            switch (c) {
                case 'n': codepoint = '\n'; break;
                case 'v': codepoint = '\v'; break;
                case 'b': codepoint = '\b'; break;
                case 't': codepoint = '\t'; break;
                case 'f': codepoint = '\f'; break;
                case 'r': codepoint = '\r'; break;
                case '\\': codepoint = '\\'; break;
                case '\'': codepoint = '\''; break;
                case '\"': codepoint = '\"'; break;
                case '0': codepoint = '\0'; break;
                case 'x':
                case 'u':
                case 'U': {
                    int num_digits = (c == 'x') ? 2 : (c == 'u') ? 4 : 8;
                    pos++;
                    col++;
                    codepoint = 0;
                    for (int i = 0; i < num_digits; ++i) {
                        if (pos >= input.size() || !std::isxdigit(input[pos])) {
                            throw_error("invalid hexadecimal escape sequence", {location});
                        }
                        codepoint = (codepoint << 4) | std::stoi(std::string(1, input[pos]), nullptr, 16);
                        pos++;
                        col++;
                    }
                    break;
                }
                default:
                    if (c >= '0' && c <= '7') { // Octal escape sequence
                        codepoint = 0;
                        for (int i = 0; i < 3 && c >= '0' && c <= '7'; ++i) {
                            codepoint = (codepoint << 3) | (c - '0');
                            pos++;
                            col++;
                            c = input[pos];
                        }
                    } else {
                        throw_error(std::format("invalid escape sequence {}", c), {location});
                    }
                    break;
            }
        } else if (DOESNT_HAVE_FLAG(c, 0x80)) {
            // ASCII character
            codepoint = c;
            pos++;
            col++;
        } else {
            // UTF-8 character
            int num_bytes = 0;
            if ((c & 0xE0) == 0xC0)
                num_bytes = 2;
            else if ((c & 0xF0) == 0xE0)
                num_bytes = 3;
            else if ((c & 0xF8) == 0xF0)
                num_bytes = 4;
            else
                throw_error("invalid UTF-8 start byte", {location});
    
            for (int i = 0; i < num_bytes; ++i) {
                if (pos >= input.size() || (i > 0 && (input[pos] & 0xC0) != 0x80)) {
                    throw_error("invalid UTF-8 continuation byte", {location});
                }
                codepoint = (codepoint << 6) | (input[pos] & 0x3F);
                pos++;
                col++;
            }
        }
    
        c = input[pos];
        if (c != '\'') {
            throw_error(std::format("invalid char literal: too many characters {}", codepoint), {location});
        }
        pos++; // move past '
        col++;
    
        std::stringstream ss;
        ss << "0x" << std::hex << codepoint;
        state.lookahead_buffer.push_back(Token(location, ss.str(), TType::Char, TFamily::Literal));
        return;
    }

    if (c == '"') {
      pos++;
      col++;
      c = input[pos];
      while (pos < len) {
        c = input[pos];
        if (c == '"') {
          break;
        } if (c == '\n') {
          throw_error("You can't directly embed a '\\n' in string by just letting it span multiple lines", {location});
        } else if (c == '\\') {
          if (pos + 1 < len) {
            token.put(c);
            pos++;
            col++;
            token.put(input[pos]);
            pos++;
            col++;
          } else {
            throw_error("incomplete escape sequence at end of input", {location});
          }
        } else {
          token.put(c);
          pos++;
          col++;
          c = input[pos];
        }
      }
      pos++;
      col++;
      state.lookahead_buffer.push_back(Token(location, token.str(), TType::String, TFamily::Literal));
      return;
    }

    if (std::isalpha(c) || c == '_' ||
        (c & 0x80) != 0) { // Check if the character is alphabetic, underscore, or a UTF-8 start byte
      while (pos < len) {
        if (std::isalnum(c) || c == '_') {
          token << c;
          pos++;
          col++;
          c = input[pos];
        } else if (HAS_FLAG(c, 0x80)) { // Check if the character is a UTF-8 start byte
          int num_bytes = 0;
          if ((c & 0xE0) == 0xC0)
            num_bytes = 2;
          else if ((c & 0xF0) == 0xE0)
            num_bytes = 3;
          else if ((c & 0xF8) == 0xF0)
            num_bytes = 4;
          else
            break; // Stop processing if it's not a valid UTF-8 start byte

          for (int i = 0; i < num_bytes; ++i) {
            if (pos >= len || (i > 0 && (input[pos] & 0xC0) != 0x80)) {
              break; // Stop processing if it's not a valid UTF-8 continuation byte
            }
            token << input[pos];
            pos++;
            col++;
          }

          c = input[pos];
        } else {
          break; // Stop processing if it's not a valid identifier character
        }
      }

      string value = token.str();
      if (keywords.contains(value)) {
        state.lookahead_buffer.push_back(Token(location, value, keywords.at(value), TFamily::Keyword));
      } else {
        if (reserved.contains(value)) {
          value = "$" + value;
        }
        state.lookahead_buffer.push_back(Token(location, value, TType::Identifier, TFamily::Identifier));
      }
      return;
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
            state.lookahead_buffer.push_back(
                Token(location, longest_match, operators.at(longest_match), TFamily::Operator));
            return;
          }
        }
        pos++;
        col++;
        c = input[pos];
      }

      if (!longest_match.empty()) {
        state.lookahead_buffer.push_back(
            Token(location, longest_match, operators.at(longest_match), TFamily::Operator));
        return;
      } else {
        throw_error("unable to lex operator :: '" + current_match + '\'', {location});
      }
    } else if (std::isdigit(c)) {
      bool is_float = false;
      bool is_hex = false;
      bool is_bin = false;
      if (c == '0' && (input[pos + 1] == 'x' || input[pos + 1] == 'X')) {
        is_hex = true;
        pos += 2; // Skip '0x'
        col += 2;
        c = input[pos];
      } else if (c == '0' && (input[pos + 1] == 'b' || input[pos + 1] == 'B')) {
        is_bin = true;
        pos += 2; // Skip '0b'
        col += 2;
        c = input[pos];
      }

      while (pos < len && (std::isdigit(c) || c == '.' || (is_hex && std::isxdigit(c)) ||
                           (is_bin && (c == '0' || c == '1')) || c == '_')) {
        if (c == '_') {
          pos++;
          col++;
          c = input[pos];
        }
        if (c == '.' && pos + 1 < len && input[pos + 1] == '.') {
          break;
        }
        if (c == '.') {
          if (is_float) {
            throw_error("got too many '.' periods in a float literal.", {location});
          }
          is_float = true;
        }
        token.put(c);
        pos++;
        col++;
        c = input[pos];
        if (c == '_') {
          pos++;
          col++;
          c = input[pos];
        }
      }
      auto value = token.str();

      if (is_hex) {
        state.lookahead_buffer.push_back(Token(location, "0x" + value, TType::Integer, TFamily::Literal));
      } else if (is_bin) {
        state.lookahead_buffer.push_back(Token(location, "0b" + value, TType::Integer, TFamily::Literal));
      } else if (!is_float) {
        state.lookahead_buffer.push_back(Token(location, value, TType::Integer, TFamily::Literal));
      } else {
        state.lookahead_buffer.push_back(Token(location, value, TType::Float, TFamily::Literal));
      }
      return;
    } else {
      throw_error("unable to lex character :: '" + std::string(1, c) + '\'', {location});
    }
  }
  state.lookahead_buffer.push_back(Token::Eof());
}