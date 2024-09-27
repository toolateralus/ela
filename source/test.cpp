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
  for (s32 i {}; (i < 10); ++i) {
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
  bool result_bool {};
  (result_bool = (left == right));
  (result_bool = (left != right));
  (result_bool = (left < right));
  (result_bool = (left > right));
  (result_bool = (left <= right));
  (result_bool = (left >= right));
  (result_bool = (left && right));
  (result_bool = (left || right));
  (result = (left + right));
  (result = (left - right));
  (result = (left * right));
  (result = (left / right));
  (result = (left % right));
  (result = (left & right));
  (result = (left | right));
  (result = (left ^ right));
  (result = (left << right));
  (result = (left >> right));
  return result;
};
f32 sqrt (f32 f) {
  return f;
};
void test_bool () {
  bool test1  = ((1 == (1 + 2)) || (1 == 2));
  bool test2  = ((3 > 2) && (2 < 4));
  bool test3  = (!(5 <= 5) || (6 >= 6));
  bool test4  = ((7 != 8) && (9 == 9));
  assert((test1 == false));
  assert((test2 == true));
  assert((test3 == true));
  assert((test4 == true));
};
s32 abs () {
  return 0;
};
int main () {
  assert((test_for() == 1));
  assert((test_while() == 1));
  assert((test_if_1() == 1));
  assert((test_if_2() == 1));
  test_comp_assign();
  s32 result  = test_operators();
  assert((result == 0));
  assert((sqrt(4.0) == 4.0));
  test_bool();
  assert((abs() == 0));
};
