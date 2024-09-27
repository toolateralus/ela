#define TESTING
#include "dummy.hpp"
void test_something () {
  assert(false);
};
;
__TEST_RUNNER_MAIN(auto tests = {_test("test_something", test_something)});