

#include <cstddef>
#include <cstring>
#include <string>

struct StringBuilder {
  // rather large block size cause this is just for the emit visitor,
  // and typically it creates pretty large strings.
  static constexpr auto BLOCK_SIZE = 8192;
  struct Block {
    char data[BLOCK_SIZE];
    size_t length = 0;
    Block *next = nullptr;
    inline Block(const char *str = "", size_t len = 0) {
      length = len < BLOCK_SIZE ? len : BLOCK_SIZE;
      std::memcpy(data, str, length);
    }
    inline ~Block() {
      if (next)
        delete next;
    }
  };
  Block *root;
  inline StringBuilder() : root(new Block) {}
  inline ~StringBuilder() {
    delete root;
  }
  inline StringBuilder &operator<<(const std::string &str) {
    Block *current = root;
    while (current->next) {
      current = current->next;
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
    Block *current = root;
    while (current->next) {
      current = current->next;
    }
    if (current->length < BLOCK_SIZE) {
      current->data[current->length++] = c;
    } else {
      current->next = new Block(&c, 1);
    }
    return *this;
  }
  inline std::string str() const {
    std::string result;
    Block *current = root;
    while (current) {
      result.append(current->data, current->length);
      current = current->next;
    }
    return result;
  }
  inline void str(const std::string &new_str) {
    delete root;
    root = new Block(new_str.c_str(), new_str.size());
  }
  inline void clear() {
    delete root;
    root = new Block();
  }
};
