#pragma once

#include <cstdlib>
#include <cstring>
#include <iostream>


struct StringBuilder {
  const size_t block_length = 8192 * 12;
  struct Block {
    char *data;
    size_t length;
    Block *next;

    Block(size_t block_length)
        : data(static_cast<char *>(std::calloc(block_length, sizeof(char)))), length(0), next(nullptr) {
    }

    ~Block() {
      if (data) {
        std::free(data);
      }
      if (next) {
        delete next;
      }
    }
  };

  Block *root;
  Block *current;

  inline StringBuilder() : root(new Block(block_length)), current(root) {}

  inline StringBuilder(size_t size) : root(new Block(size)), current(root), block_length(size) {}

  inline ~StringBuilder() { delete root; }

  inline StringBuilder &operator<<(const std::string &str) {
    size_t str_len = str.length();
    size_t remaining_space = block_length - current->length;

    if (str_len <= remaining_space) {
      std::memcpy(current->data + current->length, str.c_str(), str_len);
      current->length += str_len;
    } else {
      const char *src = str.c_str();
      size_t offset = 0;

      while (offset < str_len) {
        if (remaining_space == 0) {
          current->next = new Block(block_length);
          current = current->next;
          remaining_space = block_length;
        }

        size_t copy_len = std::min(str_len - offset, remaining_space);
        std::memcpy(current->data + current->length, src + offset, copy_len);
        current->length += copy_len;
        offset += copy_len;
        remaining_space -= copy_len;

        if (remaining_space == 0 && offset < str_len) {
          current->next = new Block(block_length);
          current = current->next;
          remaining_space = block_length;
        }
      }
    }
    return *this;
  }

  inline friend std::ostream &operator<<(std::ostream &os, const StringBuilder &sb) {
    Block *block = sb.root;
    while (block) {
      os.write(block->data, block->length);
      block = block->next;
    }
    return os;
  }

  inline StringBuilder &operator<<(const char *str) { return *this << std::string(str); }

  inline StringBuilder &operator<<(char c) {
    if (current->length < block_length) {
      current->data[current->length++] = c;
    } else {
      current->next = new Block(block_length);
      current = current->next;
      current->data[current->length++] = c;
    }
    return *this;
  }

  inline StringBuilder &operator<<(int value) { return *this << std::to_string(value); }

  inline StringBuilder &operator<<(double value) { return *this << std::to_string(value); }

  inline size_t length() const {
    size_t total_length = 0;
    Block *block = root;
    while (block) {
      total_length += block->length;
      block = block->next;
    }
    return total_length;
  }

  inline std::string str() const {
    std::string result;
    result.reserve(length());
    Block *block = root;
    while (block) {
      result.append(block->data, block->length);
      block = block->next;
    }
    return result;
  }

  inline void clear() {
    delete root;
    root = new Block(block_length);
  }
};
