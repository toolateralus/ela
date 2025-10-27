#include "callconv.hpp"

Argument_Convention Win64_C::get_argument(const Type *t) const {
  Argument_Convention ac;

  if (t->size_in_bits() > arg_size_limit_bits()) {
    // Large structs -> pass via memory (byval)
    ac.pass_via_memory = true;
  }
  // No scalarization for Win64 structs; always either byval or single argument
  return ac;
}

Return_Convention Win64_C::get_return(const Type *t) const {
  Return_Convention rc;

  if (t->size_in_bits() > return_size_limit_bits()) {
    // Large return -> use sret
    rc.indirect = true;
  }
  // No scalarization for Win64; small returns just go in register
  return rc;
}
