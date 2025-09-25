#include <ffi.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstring>
#include "interned_string.hpp"
#include "value.hpp"
#include "type.hpp"
#include <dlfcn.h>

extern std::unordered_map<std::string, void*> loaded_ffi_extern_functions;

ffi_type* to_ffi_type(Type* t) noexcept {
  if (!t) {
    return &ffi_type_pointer;  // fallback
  }

  if (t->is_pointer()) {
    return &ffi_type_pointer;
  }

  if (t->is_fixed_sized_array()) {
    return &ffi_type_pointer;
  }

  switch (t->info->as<ScalarTypeInfo>()->scalar_type) {
    case TYPE_FLOAT:
      return &ffi_type_float;
    case TYPE_DOUBLE:
      return &ffi_type_double;
    case TYPE_S8:
      return &ffi_type_sint8;
    case TYPE_S16:
      return &ffi_type_sint16;
    case TYPE_S32:
      return &ffi_type_sint32;
    case TYPE_S64:
      return &ffi_type_sint64;
    case TYPE_U8:
      return &ffi_type_uint8;
    case TYPE_U16:
      return &ffi_type_uint16;
    case TYPE_U32:
      return &ffi_type_uint32;
    case TYPE_U64:
      return &ffi_type_uint64;
    case TYPE_BOOL:
      return &ffi_type_uint8;
    case TYPE_CHAR:
      return &ffi_type_schar;
    case TYPE_STRING:
    case TYPE_VOID:
      return &ffi_type_pointer;
    default:
      throw_error(std::format("unable to get ffi type for {}", t->to_string()), {});
      return nullptr;
  }
}

void* try_load_libc_or_libm_symbol(const std::string& name) noexcept {
  void* sym = dlsym(RTLD_DEFAULT, name.c_str());
  if (sym) {
    loaded_ffi_extern_functions[name] = sym;
    return sym;
  }
  return nullptr;
}

template <typename T>
T ffi_coerce_numeric(Value* v) {
  switch (v->get_value_type()) {
    case ValueType::INTEGER:
      return static_cast<T>(v->as<IntValue>()->value);
    case ValueType::FLOATING:
      return static_cast<T>(v->as<FloatValue>()->value);
    case ValueType::BOOLEAN:
      return static_cast<T>(v->as<BoolValue>()->value ? 1 : 0);
    case ValueType::CHARACTER:
      return static_cast<T>(v->as<CharValue>()->value);
    default:
      return T{};  // fallback
  }
}

Value* compile_time_ffi_dispatch(InternedString& name, FunctionTypeInfo* fti,
                                 const std::vector<Value*>& args) noexcept {
  if (!fti) {
    return nullptr;
  }

  void* fn_ptr = loaded_ffi_extern_functions[name.get_str()];
  if (!fn_ptr) fn_ptr = try_load_libc_or_libm_symbol(name.get_str());
  if (!fn_ptr) return nullptr;

  size_t nargs = args.size();
  std::vector<ffi_type*> arg_types(nargs);
  std::vector<void*> arg_values(nargs);
  std::vector<std::vector<uint8_t>> storage(nargs);  // backing storage for all args

  for (size_t i = 0; i < nargs; ++i) {
    Type* expected = (i < fti->params_len) ? fti->parameter_types[i] : nullptr;  // only known for non-varargs
    Value* v = args[i];

    size_t sz = 0;

    if (expected) {
      sz = expected->size_in_bytes();
    } else {
      // For varargs, infer type from the argument itself
      switch (v->get_value_type()) {
        case ValueType::FLOATING:
          sz = sizeof(double);
          break;  // C varargs promotes float->double
        case ValueType::INTEGER:
          sz = sizeof(int);
          break;  // promotes char/short->int
        case ValueType::BOOLEAN:
          sz = sizeof(int);
          break;
        case ValueType::CHARACTER:
          sz = sizeof(int);
          break;  // promoted to int
        case ValueType::STRING:
          sz = sizeof(const char*);
          break;
        case ValueType::NULLPTR:
          sz = sizeof(void*);
          break;
        default:
          sz = sizeof(void*);
          break;
      }
    }

    storage[i].resize(sz);

    ffi_type* ffi_ty = expected ? to_ffi_type(expected) : nullptr;
    if (!ffi_ty) {
      switch (v->get_value_type()) {
        case ValueType::FLOATING:
          ffi_ty = &ffi_type_double;
          break;
        case ValueType::INTEGER:
          ffi_ty = &ffi_type_sint;
          break;
        case ValueType::BOOLEAN:
          ffi_ty = &ffi_type_sint;
          break;
        case ValueType::CHARACTER:
          ffi_ty = &ffi_type_sint;
          break;
        case ValueType::STRING:
          ffi_ty = &ffi_type_pointer;
          break;
        case ValueType::NULLPTR:
          ffi_ty = &ffi_type_pointer;
          break;
        default:
          ffi_ty = &ffi_type_pointer;
          break;
      }
    }

    arg_types[i] = ffi_ty;

    if (expected && !expected->has_extensions()) {
      auto scalar_info = expected->info->as<ScalarTypeInfo>();
      switch (scalar_info->scalar_type) {
        case TYPE_S8: {
          int8_t tmp = ffi_coerce_numeric<int8_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_U8: {
          uint8_t tmp = ffi_coerce_numeric<uint8_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_S16: {
          int16_t tmp = ffi_coerce_numeric<int16_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_U16: {
          uint16_t tmp = ffi_coerce_numeric<uint16_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_S32: {
          int32_t tmp = ffi_coerce_numeric<int32_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_U32: {
          uint32_t tmp = ffi_coerce_numeric<uint32_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_S64: {
          int64_t tmp = ffi_coerce_numeric<int64_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_U64: {
          uint64_t tmp = ffi_coerce_numeric<uint64_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_FLOAT: {
          float tmp = ffi_coerce_numeric<float>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_DOUBLE: {
          double tmp = ffi_coerce_numeric<double>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_BOOL: {
          uint8_t tmp = ffi_coerce_numeric<uint8_t>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        case TYPE_CHAR: {
          char tmp = ffi_coerce_numeric<char>(v);
          memcpy(storage[i].data(), &tmp, sz);
          break;
        }
        default:
          memcpy(storage[i].data(), &v, sz);
          break;
      }
    } else {
      // vararg / unknown / pointer
      switch (v->get_value_type()) {
        case ValueType::FLOATING: {
          double tmp = ffi_coerce_numeric<double>(v);  // promote float->double
          memcpy(storage[i].data(), &tmp, sizeof(double));
          arg_types[i] = &ffi_type_double;
          break;
        }
        case ValueType::INTEGER: {
          int tmp = ffi_coerce_numeric<int>(v);  // promote char/short->int
          memcpy(storage[i].data(), &tmp, sizeof(int));
          arg_types[i] = &ffi_type_sint;
          break;
        }
        case ValueType::BOOLEAN: {
          int tmp = v->as<BoolValue>()->value ? 1 : 0;
          memcpy(storage[i].data(), &tmp, sizeof(int));
          arg_types[i] = &ffi_type_sint;
          break;
        }
        case ValueType::CHARACTER: {
          int tmp = (int)v->as<CharValue>()->value;  // promoted to int
          memcpy(storage[i].data(), &tmp, sizeof(int));
          arg_types[i] = &ffi_type_sint;
          break;
        }
        case ValueType::STRING: {
          // call to string for escapement;
          auto string = v->as<StringValue>()->to_string();
          const char* s = strdup(string.data());
          memcpy(storage[i].data(), &s, sizeof(const char*));
          arg_types[i] = &ffi_type_pointer;
          break;
        }
        case ValueType::NULLPTR: {
          void* p = nullptr;
          memcpy(storage[i].data(), &p, sizeof(void*));
          arg_types[i] = &ffi_type_pointer;
          break;
        }
        default: {
          void* p = nullptr;
          memcpy(storage[i].data(), &p, sizeof(void*));
          arg_types[i] = &ffi_type_pointer;
          break;
        }
      }
    }

    arg_values[i] = storage[i].data();
  }

  ffi_cif cif{};
  ffi_type* ffi_ret = to_ffi_type(fti->return_type);
  int prep_ok = fti->is_varargs
                    ? ffi_prep_cif_var(&cif, FFI_DEFAULT_ABI, fti->params_len, nargs, ffi_ret, arg_types.data())
                    : ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nargs, ffi_ret, arg_types.data());
  if (prep_ok != FFI_OK) return nullptr;

  size_t ret_size = fti->return_type ? fti->return_type->size_in_bytes() : sizeof(void*);
  std::vector<uint8_t> ret_storage(ret_size);

  ffi_call(&cif, FFI_FN(fn_ptr), ret_storage.data(), arg_values.data());

  Type* ret_type = fti->return_type;
  if (!ret_type || ret_type->info->as<ScalarTypeInfo>()->scalar_type == TYPE_VOID) return null_value();

  auto scalar_type = ret_type->info->as<ScalarTypeInfo>()->scalar_type;
  switch (scalar_type) {
    case TYPE_FLOAT: {
      float tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(float));
      return new_float(tmp);
    }
    case TYPE_DOUBLE: {
      double tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(double));
      return new_float(tmp);
    }
    case TYPE_S8:
    case TYPE_U8: {
      uint8_t tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(uint8_t));
      return new_int(tmp);
    }
    case TYPE_S16:
    case TYPE_U16: {
      uint16_t tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(uint16_t));
      return new_int(tmp);
    }
    case TYPE_S32:
    case TYPE_U32: {
      uint32_t tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(uint32_t));
      return new_int(tmp);
    }
    case TYPE_S64:
    case TYPE_U64: {
      uint64_t tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(uint64_t));
      return new_int(tmp);
    }
    case TYPE_BOOL: {
      uint8_t tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(uint8_t));
      return new_bool(tmp != 0);
    }
    case TYPE_CHAR: {
      char tmp;
      memcpy(&tmp, ret_storage.data(), sizeof(char));
      return new_char(tmp);
    }
    case TYPE_STRING: {
      void* ptr;
      memcpy(&ptr, ret_storage.data(), sizeof(void*));
      return new_string(ptr ? std::string((const char*)ptr) : "");
    }
    default:
      return null_value();
  }
}
