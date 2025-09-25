#include <ffi.h>
#include <ffitarget.h>
#include "interned_string.hpp"

struct Value;
struct FunctionTypeInfo;
Value* compile_time_ffi_dispatch(InternedString& name, FunctionTypeInfo* fti,
                                 const std::vector<Value*>& args) noexcept;
