#include <cstddef>
#include <cstring>
#include <string>

struct StringBuilder {
  // Rather large block size because this is just for the emit visitor,
  // and typically it creates pretty large strings.
  static constexpr auto BLOCK_SIZE = 8192;
  struct Block {
    char data[BLOCK_SIZE];
    size_t length = 0;
    Block *next = nullptr;
    inline Block(const char *str = "", size_t len = 0) {
      next = nullptr;
      length = len < BLOCK_SIZE ? len : BLOCK_SIZE;
      std::memcpy(data, str, length);
    }
  };
  Block *root = nullptr;
  Block *current = nullptr;

  inline StringBuilder() : root(new Block), current(root) {}
  inline ~StringBuilder() { clear(); }

  inline StringBuilder &operator<<(const std::string &str) {
    if (!current) {
      current = new Block();
      root = current;
    }
    size_t remaining = str.size();
    const char *data = str.c_str();
    while (remaining > 0) {
      size_t to_copy = std::min(remaining, BLOCK_SIZE - current->length);
      std::memcpy(current->data + current->length, data, to_copy);
      current->length += to_copy;
      data += to_copy;
      remaining -= to_copy;

      if (remaining > 0) {
        current->next = new Block();
        current = current->next;
      }
    }
    return *this;
  }

  inline StringBuilder &operator<<(char c) {
    if (!current) {
      current = new Block();
      root = current;
    }
    if (current->length < BLOCK_SIZE) {
      current->data[current->length++] = c;
    } else {
      current->next = new Block(&c, 1);
      current = current->next;
    }
    return *this;
  }

  inline std::string str() const {
    std::string result;
    Block *block = root;
    while (block) {
      result.append(block->data, block->length);
      block = block->next;
    }
    return result;
  }

  inline void str(const std::string &new_str) {
    clear();
    root = new Block(new_str.c_str(), new_str.size());
    current = root;
  }

  inline void clear() {
    Block *block = root;
    while (block) {
      Block *next = block->next;
      delete block;
      block = next;
    }
    root = new Block();
    current = root;
  }
};