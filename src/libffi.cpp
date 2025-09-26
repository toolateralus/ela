#include <ffi.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstring>
#include <cstdlib>
#include "error.hpp"
#include "interned_string.hpp"
#include "value.hpp"
#include "type.hpp"
#include <dlfcn.h>
#include <algorithm>

extern std::unordered_map<std::string, void*> loaded_ffi_extern_functions;

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
      return T{};
  }
}

ffi_type* to_ffi_type(Type* t) {
  if (!t) return &ffi_type_pointer;

  if (t->is_pointer() || t->is_fixed_sized_array()) return &ffi_type_pointer;

  if (t->is_kind(TYPE_SCALAR)) {
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
      case TYPE_VOID:
        return &ffi_type_void;
      default:
        throw_error(std::format("unable to get ffi type for {}", t->to_string()), {});
        return nullptr;
    }
  } else {
    throw_error("non scalar types not yet supported by the ffi library", {});
    return nullptr;
  }
}

void* try_load_libc_or_libm_symbol(const std::string& name) {
  void* sym = dlsym(RTLD_DEFAULT, name.c_str());
  if (sym) loaded_ffi_extern_functions[name] = sym;
  return sym;
}

struct FFIContext {
  std::vector<ffi_type*> arg_types;
  std::vector<std::vector<uint8_t>> storage;         // backing storage for each argument
  std::vector<void*> arg_values;                     // pointers into storage
  std::vector<char*> allocated_strings;              // strdup'd strings
  std::vector<std::vector<uint8_t>> nested_storage;  // storage for dereferenced PointerValues

  FFIContext(size_t nargs) {
    arg_types.resize(nargs);
    storage.resize(nargs);
    arg_values.resize(nargs);
  }

  ~FFIContext() {
    for (auto s : allocated_strings) free(s);
  }

  void marshal_value_into_storage(Value* v, std::vector<uint8_t>& out_storage, ffi_type*& out_type) {
    switch (v->get_value_type()) {
      case ValueType::FLOATING: {
        double tmp = ffi_coerce_numeric<double>(v);
        out_type = &ffi_type_double;
        out_storage.resize(sizeof(double));
        memcpy(out_storage.data(), &tmp, sizeof(double));
        break;
      }
      case ValueType::INTEGER: {
        long long tmp = ffi_coerce_numeric<long long>(v);
        out_type = &ffi_type_sint;
        out_storage.resize(sizeof(long long));
        memcpy(out_storage.data(), &tmp, sizeof(long long));
        break;
      }
      case ValueType::BOOLEAN: {
        int tmp = v->as<BoolValue>()->value ? 1 : 0;
        out_type = &ffi_type_sint;
        out_storage.resize(sizeof(int));
        memcpy(out_storage.data(), &tmp, sizeof(int));
        break;
      }
      case ValueType::CHARACTER: {
        int tmp = static_cast<int>(v->as<CharValue>()->value);
        out_type = &ffi_type_sint;
        out_storage.resize(sizeof(int));
        memcpy(out_storage.data(), &tmp, sizeof(int));
        break;
      }
      case ValueType::STRING: {
        std::string s = v->as<StringValue>()->to_string();
        char* cstr = strdup(s.c_str());
        allocated_strings.push_back(cstr);
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(char*));
        memcpy(out_storage.data(), &cstr, sizeof(char*));
        break;
      }
      case ValueType::NULLPTR: {
        void* p = nullptr;
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));
        memcpy(out_storage.data(), &p, sizeof(void*));
        break;
      }
      case ValueType::RAW_POINTER: {
        void* p = v->as<RawPointerValue>()->ptr;
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));
        memcpy(out_storage.data(), &p, sizeof(void*));
        break;
      }
      case ValueType::POINTER: {
        PointerValue* pv = v->as<PointerValue>();
        if (pv->ptr) {
          nested_storage.emplace_back();
          marshal_value_into_storage(*(pv->ptr), nested_storage.back(), out_type);
          void* p = nested_storage.back().data();
          out_type = &ffi_type_pointer;
          out_storage.resize(sizeof(void*));
          memcpy(out_storage.data(), &p, sizeof(void*));
        } else {
          void* p = nullptr;
          out_type = &ffi_type_pointer;
          out_storage.resize(sizeof(void*));
          memcpy(out_storage.data(), &p, sizeof(void*));
        }
        break;
      }
      case ValueType::ARRAY: {
        ArrayValue* arr = v->as<ArrayValue>();
        size_t elem_size = arr->type->base_type->size_in_bytes();
        size_t count = arr->values.size();
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));

        nested_storage.emplace_back(elem_size * (count == 0 ? 1 : count));
        auto& buf = nested_storage.back();
        uint8_t* arr_storage = buf.data();
        if (!buf.empty()) memset(arr_storage, 0, buf.size());

        std::function<void(Value*, uint8_t*, size_t)> marshal_value_to_buffer;

        marshal_value_to_buffer = [&](Value* sv, uint8_t* target, size_t stride) {
          switch (sv->get_value_type()) {
            case ValueType::INTEGER: {
              long long tmp = ffi_coerce_numeric<long long>(sv);
              size_t copy_n = std::min(stride, sizeof(tmp));
              memcpy(target, &tmp, copy_n);
              break;
            }
            case ValueType::FLOATING: {
              double tmp = ffi_coerce_numeric<double>(sv);
              size_t copy_n = std::min(stride, sizeof(tmp));
              memcpy(target, &tmp, copy_n);
              break;
            }
            case ValueType::BOOLEAN: {
              int tmp = sv->as<BoolValue>()->value ? 1 : 0;
              size_t copy_n = std::min(stride, sizeof(tmp));
              memcpy(target, &tmp, copy_n);
              break;
            }
            case ValueType::CHARACTER: {
              int tmp = static_cast<int>(sv->as<CharValue>()->value);
              size_t copy_n = std::min(stride, sizeof(tmp));
              memcpy(target, &tmp, copy_n);
              break;
            }
            case ValueType::STRING: {
              std::string s = sv->as<StringValue>()->to_string();
              char* cstr = strdup(s.c_str());
              allocated_strings.push_back(cstr);
              void* p = cstr;
              size_t copy_n = std::min(stride, sizeof(void*));
              memcpy(target, &p, copy_n);
              break;
            }
            case ValueType::RAW_POINTER: {
              void* p = sv->as<RawPointerValue>()->ptr;
              size_t copy_n = std::min(stride, sizeof(void*));
              memcpy(target, &p, copy_n);
              break;
            }
            case ValueType::POINTER: {
              PointerValue* pv = sv->as<PointerValue>();
              if (pv->ptr) {
                nested_storage.emplace_back();
                ffi_type* dummy = nullptr;
                marshal_value_into_storage(*(pv->ptr), nested_storage.back(), dummy);
                void* p = nested_storage.back().data();
                size_t copy_n = std::min(stride, sizeof(void*));
                memcpy(target, &p, copy_n);
              } else {
                void* p = nullptr;
                size_t copy_n = std::min(stride, sizeof(void*));
                memcpy(target, &p, copy_n);
              }
              break;
            }
            case ValueType::ARRAY: {
              ArrayValue* nested = sv->as<ArrayValue>();
              size_t n_elem_size = nested->type->base_type->size_in_bytes();
              size_t n_count = nested->values.size();
              nested_storage.emplace_back(n_elem_size * (n_count == 0 ? 1 : n_count));
              auto& nbuf = nested_storage.back();
              if (!nbuf.empty()) memset(nbuf.data(), 0, nbuf.size());
              for (size_t j = 0; j < n_count; ++j) {
                marshal_value_to_buffer(nested->values[j], nbuf.data() + j * n_elem_size, n_elem_size);
              }
              void* p = nbuf.data();
              size_t copy_n = std::min(stride, sizeof(void*));
              memcpy(target, &p, copy_n);
              break;
            }
            default: {
              void* p = nullptr;
              size_t copy_n = std::min(stride, sizeof(void*));
              memcpy(target, &p, copy_n);
              break;
            }
          }
        };

        for (size_t idx = 0; idx < count; ++idx) {
          uint8_t* target = arr_storage + idx * elem_size;
          marshal_value_to_buffer(arr->values[idx], target, elem_size);
        }

        void* p = arr_storage;
        memcpy(out_storage.data(), &p, sizeof(void*));
      } break;
      default: {
        void* p = nullptr;
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));
        memcpy(out_storage.data(), &p, sizeof(void*));
        break;
      }
    }
  }

  void marshal_arg(FunctionTypeInfo* fti, size_t i, Value* v) {
    Type* expected = (i < fti->params_len) ? fti->parameter_types[i] : nullptr;

    ffi_type* ffi_ty = expected ? to_ffi_type(expected) : nullptr;
    marshal_value_into_storage(v, storage[i], ffi_ty);

    arg_types[i] = ffi_ty;
    arg_values[i] = storage[i].data();
  }

  void unmarshal_value(Value* v, void* storage) {
    switch (v->get_value_type()) {
      case ValueType::INTEGER:
        v->as<IntValue>()->value = *reinterpret_cast<int64_t*>(storage);
        break;
      case ValueType::FLOATING:
        v->as<FloatValue>()->value = *reinterpret_cast<double*>(storage);
        break;
      case ValueType::BOOLEAN:
        v->as<BoolValue>()->value = *reinterpret_cast<int*>(storage) != 0;
        break;
      case ValueType::CHARACTER:
        v->as<CharValue>()->value = static_cast<char>(*reinterpret_cast<int*>(storage));
        break;
      case ValueType::POINTER: {
        PointerValue* pv = v->as<PointerValue>();
        if (pv->ptr && *pv->ptr) {
          void* inner_storage = *reinterpret_cast<void**>(storage);
          unmarshal_value(*pv->ptr, inner_storage);
        }
        break;
      }
      case ValueType::ARRAY: {
        ArrayValue* arr = v->as<ArrayValue>();
        uint8_t* ptr = reinterpret_cast<uint8_t*>(storage);
        size_t offset = 0;
        const size_t sz = arr->type->base_type->size_in_bytes();
        for (auto& elem : arr->values) {
          unmarshal_value(elem, ptr + offset);
          offset += sz;
        }
        break;
      }
      default:
        // for raw pointers or unknowns, we just copy pointer back
        *reinterpret_cast<void**>(v) = *reinterpret_cast<void**>(storage);
        break;
    }
  }

  void writeback_pointer_values(const std::vector<Value*>& args) {
    for (size_t i = 0; i < args.size(); ++i) {
      if (args[i]->get_value_type() == ValueType::POINTER) {
        PointerValue* pv = args[i]->as<PointerValue>();
        if (pv->ptr && *pv->ptr) {
          void* storage_ptr = nested_storage.front().data();
          nested_storage.erase(nested_storage.begin());
          unmarshal_value(*pv->ptr, storage_ptr);
        }
      }
    }
  }
};

Value* unmarshal_scalar_return(ScalarTypeInfo* info, const std::vector<uint8_t>& ret_storage) {
  switch (info->scalar_type) {
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
    default:
      return null_value();
  }
}


/* 
  TODO:
  
  we need to handle marshalling and umarshalling managed values better.
  right now it's very offshoot and chance.
*/
Value* compile_time_ffi_dispatch(InternedString& name, FunctionTypeInfo* fti, const std::vector<Value*>& args) {
  void* fn_ptr = loaded_ffi_extern_functions[name.get_str()];

  if (!fn_ptr) {
    fn_ptr = try_load_libc_or_libm_symbol(name.get_str());
  }

  if (!fn_ptr) {
    return nullptr;
  }

  if (!fti) {
    return nullptr;
  }

  Type* ret_type = fti->return_type;
  size_t ret_size = sizeof(void*);

  if (ret_type) {
    ret_size = ret_type->size_in_bytes();
  }

  if (!ret_type->is_kind(TYPE_SCALAR)) {
    throw_error("ffi doesn't support non-scalar return types", {});
  }

  auto ret_ty_scalar_info = ret_type->info->as<ScalarTypeInfo>();

  std::vector<uint8_t> ret_storage(ret_size);

  FFIContext ctx(args.size());
  for (size_t i = 0; i < args.size(); ++i) {
    ctx.marshal_arg(fti, i, args[i]);
  }

  ffi_cif cif{};
  ffi_type* ffi_ret = to_ffi_type(fti->return_type);

  int prep_ok;
  if (fti->is_varargs) {
    prep_ok = ffi_prep_cif_var(&cif, FFI_DEFAULT_ABI, fti->params_len, args.size(), ffi_ret, ctx.arg_types.data());
  } else {
    prep_ok = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args.size(), ffi_ret, ctx.arg_types.data());
  }

  if (prep_ok != FFI_OK) {
    throw_error(std::format("Failed ffi call, prep_ok={}", prep_ok), {});
    return nullptr;
  }

  ffi_call(&cif, FFI_FN(fn_ptr), ret_storage.data(), ctx.arg_values.data());

  ctx.writeback_pointer_values(args);

  if (!ret_type || ret_type == void_type()) {
    return null_value();
  }

  return unmarshal_scalar_return(ret_ty_scalar_info, ret_storage);
}
