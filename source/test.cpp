#define TESTING
#include "test.hpp"
void test_if_3 () {
  s32 v  = 1;
  if (!v) {
    assert("v should be 0", (v == 0));
  } else   if (v) {
    assert("v should be 1", (v == 1));
  } else  {
    assert("Unexpected case", false);
  };
};
void test_for () {
  for (s32 i {}; (i < 10); ++i) {
    assert("i should be less than 10", (i < 10));
    return;
  };
  _array<s32> arr {};
  for (auto v : arr) {
    assert("Unexpected case in array loop", false);
    return;
  };
  assert("End of test_for reached", true);
};
void test_while () {
  s32* n {};
  s32 v {};
  bool b  = true;
  if (b) {
    assert("b should be true", (b == true));
  };
  while (v) {
    assert("v should not be 0", (v != 0));
    return;
  };
  assert("End of test_while reached", true);
};
void test_if_1 () {
  s32 v  = 1;
  if (v) {
    assert("v should be 1", (v == 1));
  } else  {
    assert("Unexpected case", false);
  };
};
void test_if_2 () {
  s32 v  = 1;
  if (v) {
    assert("v should be 1", (v == 1));
  } else  {
    assert("Unexpected case", false);
  };
  assert("End of test_if_2 reached", true);
};
void test_comp_assign () {
  s32 deref  = 1;
  deref += deref;
  assert("+=", (deref == 2));
  deref -= deref;
  assert("-=", (deref == 0));
  deref *= deref;
  assert("*=)", (deref == 0));
  (deref = 2);
  deref /= deref;
  assert("/=", (deref == 1));
  (deref = 2);
  deref %= deref;
  assert("%=", (deref == 0));
  deref &= deref;
  assert("&=", (deref == 0));
  deref |= deref;
  assert("|=", (deref == 0));
  deref ^= deref;
  assert("^=", (deref == 0));
  deref <<= deref;
  assert("<<=", (deref == 0));
  deref >>= deref;
  assert(">>=", (deref == 0));
};
void failure_case () {
  assert("This should fail", false);
};
void test_operators () {
  s32 left  = 1;
  s32 right  = 1;
  s32 result {};
  bool result_bool {};
  (result_bool = (left == right));
  assert("left should equal right", (result_bool == true));
  (result_bool = (left != right));
  assert("left should not equal right", (result_bool == false));
  (result_bool = (left < right));
  assert("left should not be less than right", (result_bool == false));
  (result_bool = (left > right));
  assert("left should not be greater than right", (result_bool == false));
  (result_bool = (left <= right));
  assert("left should be less than or equal to right", (result_bool == true));
  (result_bool = (left >= right));
  assert("left should be greater than or equal to right", (result_bool == true));
  (result_bool = (left && right));
  assert("left and right should be true", (result_bool == true));
  (result_bool = (left || right));
  assert("left or right should be true", (result_bool == true));
  (result = (left + right));
  assert("left + right should be 2", (result == 2));
  (result = (left - right));
  assert("left - right should be 0", (result == 0));
  (result = (left * right));
  assert("left * right should be 1", (result == 1));
  (result = (left / right));
  assert("left / right should be 1", (result == 1));
  (result = (left % right));
  assert("left % right should be 0", (result == 0));
  (result = (left & right));
  assert("left & right should be 1", (result == 1));
  (result = (left | right));
  assert("left | right should be 1", (result == 1));
  (result = (left ^ right));
  assert("left ^ right should be 0", (result == 0));
  (result = (left << right));
  assert("left << right should be 2", (result == 2));
  (result = (left >> right));
  assert("left >> right should be 0", (result == 0));
};
void test_relational_results () {
  bool test1  = ((1 == (1 + 2)) || (1 == 2));
  bool test2  = ((3 > 2) && (2 < 4));
  bool test3  = (!(5 <= 5) || (6 >= 6));
  bool test4  = ((7 != 8) && (9 == 9));
  assert("test1 should be false", (test1 == false));
  assert("test2 should be true", (test2 == true));
  assert("test3 should be true", (test3 == true));
  assert("test4 should be true", (test4 == true));
};
const jstl::Vector<__COMPILER_GENERATED_TEST> tests{__COMPILER_GENERATED_TEST("test_if_3", test_if_3),__COMPILER_GENERATED_TEST("test_for", test_for),__COMPILER_GENERATED_TEST("test_while", test_while),__COMPILER_GENERATED_TEST("test_if_1", test_if_1),__COMPILER_GENERATED_TEST("test_if_2", test_if_2),__COMPILER_GENERATED_TEST("test_comp_assign", test_comp_assign),__COMPILER_GENERATED_TEST("failure_case", failure_case),__COMPILER_GENERATED_TEST("test_operators", test_operators),__COMPILER_GENERATED_TEST("test_relational_results", test_relational_results)};
__TEST_RUNNER_MAIN;