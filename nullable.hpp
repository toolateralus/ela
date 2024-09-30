#pragma once




template<class T>
struct Nullable {
  Nullable() {}
  Nullable(T *ptr) : ptr(ptr) {}
  T *ptr {};
  T *get() const {
    return ptr;
  }
  void set(T *ptr) {
    this->ptr = ptr;
  }
  operator bool() const {
    return ptr;
  }
  bool is_null() const {
    return ptr == nullptr;
  }
  bool is_not_null() const {
    return ptr != nullptr;
  }
};