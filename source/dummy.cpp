#include "boilerplate.hpp"
s32 test_if_3 () {
  s32 v  = 1;
  if (!v) {
    return 0;
  } else   if (v) {
    return 1;
  } else  {
    return 0;
  };
};
s32 test_for () {
  for (s32 i {}; i < 10; ++i) {
    return 0;
  };
  _array<s32> arr {};
  for (auto v : arr) {
    return 0;
  };
  return 1;
};
s32 test_while () {
  s32* n {};
  s32 v {};
  bool b  = true;
  if (b) {
  };
  while (v) {
    return 1;
  };
  return 1;
};
s32 test_if_1 () {
  s32 v  = 1;
  if (v) {
    return 1;
  };
  return 0;
};
s32 test_if_2 () {
  s32 v  = 1;
  if (v) {
    return 1;
  } else  {
    return 0;
  };
  return 1;
};
void test_comp_assign () {
  s32 deref  = 1;
  deref += deref;
  deref -= deref;
  deref *= deref;
  deref /= deref;
  deref %= deref;
  deref &= deref;
  deref |= deref;
  deref ^= deref;
  deref <<= deref;
  deref >>= deref;
};
s32 test_operators () {
  s32 left {};
  s32 right {};
  s32 result {};
  result = left + right;
  result = left - right;
  result = left * right;
  result = left / right;
  result = left % right;
  result = left == right;
  result = left != right;
  result = left < right;
  result = left > right;
  result = left <= right;
  result = left >= right;
  result = left && right;
  result = left || right;
  result = left & right;
  result = left | right;
  result = left ^ right;
  result = left << right;
  result = left >> right;
  return result;
};
float sqrt (float f) {
  return f;
};
s32 abs () {
  return 0;
};
int main (s32 argc, char** argv) {
  s32 i  = 0;
  ++i;
  i = ++i;
  float deref {};
  test_while();
  sqrt(deref);
  test_if_1();
  test_if_2();
  test_if_3();
  return 0;
};
