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
  if (t->is_kind(TYPE_STRUCT)) {
    auto info = t->info->as<StructTypeInfo>();
    size_t n = info->members.size();
    ffi_type** elements = new ffi_type*[n + 1];
    for (size_t i = 0; i < n; ++i) {
      elements[i] = to_ffi_type(info->members[i].type);
    }
    elements[n] = nullptr;
    ffi_type* struct_type = new ffi_type;
    struct_type->size = 0;
    struct_type->alignment = 0;
    struct_type->type = FFI_TYPE_STRUCT;
    struct_type->elements = elements;
    return struct_type;
  }

  if (!t) {
    return &ffi_type_pointer;
  }

  if (t->is_pointer() || t->is_fixed_sized_array()) {
    return &ffi_type_pointer;
  }

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
      case TYPE_CHAR:  // We use 32 bit unsigned chars, UTF8+ support.
        return &ffi_type_sint32;
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

extern std::vector<std::string> DYNAMIC_LIBRARY_LOAD_PATH;

std::unordered_map<std::string, void*> loaded_dl_libraries;

void* try_load_symbol(const std::string& name) {
  if (loaded_ffi_extern_functions.contains(name)) {
    return loaded_ffi_extern_functions[name];
  }

  void* sym = dlsym(RTLD_DEFAULT, name.c_str());
  if (sym) {
    loaded_ffi_extern_functions[name] = sym;
  }

  for (const auto& lib : DYNAMIC_LIBRARY_LOAD_PATH) {
    if (loaded_dl_libraries.contains(lib)) {
      void* sym = dlsym(loaded_dl_libraries[lib], name.c_str());
      if (sym) {
        loaded_ffi_extern_functions[name] = sym;
        return sym;
      }
    }

    void* the_lib = dlopen(lib.c_str(), RTLD_NOW);

    if (!the_lib) {
      throw_warning(WARNING_GENERAL,
                    std::format("failed to load dynamic library at compile time. lib='{}', is your path correct?", lib),
                    {});
      continue;
    }

    void* sym = dlsym(the_lib, name.c_str());

    if (!sym) {
      throw_warning(WARNING_GENERAL, std::format("failed to get sym '{}' from lib '{}' at compile time.", name, lib),
                    {});
      dlclose(the_lib);
    } else {
      loaded_dl_libraries[lib] = the_lib;
      loaded_ffi_extern_functions[name] = sym;
    }
  }

  return sym;
}

struct NestedAlloc {
  size_t arg_index;
  size_t elem_size;
  size_t count;
  std::vector<uint8_t> buf;
};

struct FFIContext {
  std::vector<ffi_type*> arg_types;
  std::vector<std::vector<uint8_t>> storage;  // backing storage for each argument (holds scalar bytes or a pointer)
  std::vector<void*> arg_values;              // pointers into storage
  std::vector<char*> allocated_strings;       // strdup'd strings
  std::vector<NestedAlloc> nested_allocs;     // metadata'd nested buffers (per-arg)
  std::vector<std::vector<size_t>> nested_alloc_indices_by_arg;  // mapping arg -> nested_alloc indices

  FFIContext(size_t nargs) {
    arg_types.resize(nargs);
    storage.resize(nargs);
    arg_values.resize(nargs);
    nested_alloc_indices_by_arg.resize(nargs);
  }

  ~FFIContext() {
    for (auto s : allocated_strings) {
      free(s);
    }
  }

  // helper to make a nested allocation and record its ownership by arg_index
  size_t create_nested_alloc(size_t arg_index, size_t total_bytes, size_t elem_size, size_t count) {
    NestedAlloc na;
    na.arg_index = arg_index;
    na.elem_size = elem_size;
    na.count = count;
    na.buf.resize(total_bytes);
    if (!na.buf.empty()) {
      memset(na.buf.data(), 0, na.buf.size());
    }
    nested_allocs.push_back(std::move(na));
    size_t idx = nested_allocs.size() - 1;
    nested_alloc_indices_by_arg[arg_index].push_back(idx);
    return idx;
  }

  // write an integer of known width into target (up to elem_size)
  void copy_integer_to_target(long long val, uint8_t* target, size_t stride) {
    if (stride >= sizeof(int64_t)) {
      memcpy(target, &val, sizeof(int64_t));
    } else if (stride == 4) {
      int32_t v32 = static_cast<int32_t>(val);
      memcpy(target, &v32, sizeof(v32));
    } else if (stride == 2) {
      int16_t v16 = static_cast<int16_t>(val);
      memcpy(target, &v16, sizeof(v16));
    } else if (stride == 1) {
      int8_t v8 = static_cast<int8_t>(val);
      memcpy(target, &v8, sizeof(v8));
    } else {
      // as fallback, copy as much as fits
      size_t copy_n = std::min(stride, sizeof(int64_t));
      memcpy(target, &val, copy_n);
    }
  }

  // write a double to target (stride likely >= sizeof(double))
  void copy_double_to_target(double val, uint8_t* target, size_t stride) {
    size_t copy_n = std::min(stride, sizeof(double));
    memcpy(target, &val, copy_n);
  }

  // helper to marshal a single element value INTO a preallocated buffer target
  void marshal_value_to_buffer_element(Value* sv, uint8_t* target, size_t stride, size_t arg_index) {
    switch (sv->get_value_type()) {
      case ValueType::INTEGER: {
        long long tmp = ffi_coerce_numeric<long long>(sv);
        copy_integer_to_target(tmp, target, stride);
        break;
      }
      case ValueType::FLOATING: {
        double tmp = ffi_coerce_numeric<double>(sv);
        copy_double_to_target(tmp, target, stride);
        break;
      }
      case ValueType::BOOLEAN: {
        int tmp = sv->as<BoolValue>()->value ? 1 : 0;
        size_t copy_n = std::min(stride, sizeof(int));
        memcpy(target, &tmp, copy_n);
        break;
      }
      case ValueType::CHARACTER: {
        int tmp = static_cast<int>(sv->as<CharValue>()->value);
        size_t copy_n = std::min(stride, sizeof(int));
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
        if (pv->ptr && *pv->ptr) {
          // create nested buffer for the pointee and copy pointer
          // We use a mini-marshalling into a nested buffer and then copy the pointer value into target.
          // Create a temporary nested alloc for this nested pointer; it will be recorded under arg_index.
          // We marshal the pointee value INTO that nested alloc buffer using marshal_value_into_storage.
          std::vector<uint8_t> tmp_storage;
          ffi_type* dummy = nullptr;
          marshal_value_into_storage(*(pv->ptr), tmp_storage, dummy);

          size_t nested_idx = create_nested_alloc(arg_index, tmp_storage.size(), tmp_storage.size(), 1);
          NestedAlloc& na = nested_allocs[nested_idx];
          memcpy(na.buf.data(), tmp_storage.data(), tmp_storage.size());

          void* p = na.buf.data();
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
        // nested array inside an array: allocate nested buffer and copy pointer into target
        ArrayValue* nested = sv->as<ArrayValue>();
        size_t n_elem_size = nested->type->base_type->size_in_bytes();
        size_t n_count = nested->values.size();
        size_t total = n_elem_size * (n_count == 0 ? 1 : n_count);
        size_t nested_idx = create_nested_alloc(arg_index, total, n_elem_size, n_count);
        NestedAlloc& na = nested_allocs[nested_idx];
        for (size_t j = 0; j < n_count; ++j) {
          marshal_value_to_buffer_element(nested->values[j], na.buf.data() + j * n_elem_size, n_elem_size, arg_index);
        }
        void* p = na.buf.data();
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
  }

  // marshal_value_into_storage: marshals a Value into out_storage and selects an out_type
  // If the Value is pointer/array, we allocate nested buffers and put a pointer into out_storage.
  void marshal_value_into_storage(Value* v, std::vector<uint8_t>& out_storage, ffi_type*& out_type,
                                  size_t arg_index = 0) {
    // Support for objects: map<InternedString, Value*>
    if (v->get_value_type() == ValueType::OBJECT) {
      // Assume ObjectValue has: std::unordered_map<InternedString, Value*> fields;
      ObjectValue* obj = v->as<ObjectValue>();
      // Marshal as a struct: flatten fields in order of type's struct members
      if (obj->type->is_kind(TYPE_STRUCT)) {
        StructTypeInfo* stinfo = obj->type->info->as<StructTypeInfo>();
        size_t n = stinfo->members.size();
        std::vector<uint8_t> struct_buf;
        struct_buf.resize(obj->type->size_in_bytes(), 0);
        size_t offset = 0;
        for (size_t i = 0; i < n; ++i) {
          const auto& member = stinfo->members[i];
          auto it = obj->values.find(member.name);
          if (it != obj->values.end()) {
            std::vector<uint8_t> field_buf;
            ffi_type* dummy = nullptr;
            marshal_value_into_storage(it->second, field_buf, dummy, arg_index);
            memcpy(struct_buf.data() + offset, field_buf.data(),
                   std::min(field_buf.size(), member.type->size_in_bytes()));
          }
          offset += member.type->size_in_bytes();
        }
        out_type = to_ffi_type(obj->type);
        out_storage = std::move(struct_buf);
        return;
      }
    }

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
        // use 64-bit signedstorage for integers internally (consistent with prior behavior)
        out_type = &ffi_type_sint64;
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
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));
        if (pv->ptr && *pv->ptr) {
          // If the pointee is an ARRAY, we want a contiguous element buffer
          Value* pointee = *pv->ptr;
          if (pointee->get_value_type() == ValueType::ARRAY) {
            ArrayValue* aval = pointee->as<ArrayValue>();
            size_t elem_size = aval->type->base_type->size_in_bytes();
            size_t count = aval->values.size();
            size_t total = elem_size * (count == 0 ? 1 : count);
            size_t nested_idx = create_nested_alloc(arg_index, total, elem_size, count);
            NestedAlloc& na = nested_allocs[nested_idx];
            for (size_t j = 0; j < count; ++j) {
              marshal_value_to_buffer_element(aval->values[j], na.buf.data() + j * elem_size, elem_size, arg_index);
            }
            void* p = na.buf.data();
            memcpy(out_storage.data(), &p, sizeof(void*));
          } else {
            // pointee is scalar: create a nested alloc sized for that scalar
            std::vector<uint8_t> tmp_storage;
            ffi_type* dummy = nullptr;
            marshal_value_into_storage(*pv->ptr, tmp_storage, dummy, arg_index);
            size_t nested_idx = create_nested_alloc(arg_index, tmp_storage.size(), tmp_storage.size(), 1);
            NestedAlloc& na = nested_allocs[nested_idx];
            memcpy(na.buf.data(), tmp_storage.data(), tmp_storage.size());
            void* p = na.buf.data();
            memcpy(out_storage.data(), &p, sizeof(void*));
          }
        } else {
          void* p = nullptr;
          memcpy(out_storage.data(), &p, sizeof(void*));
        }
        break;
      }
      case ValueType::ARRAY: {
        ArrayValue* arr = v->as<ArrayValue>();
        size_t elem_size = arr->type->base_type->size_in_bytes();
        size_t count = arr->values.size();
        size_t total = elem_size * (count == 0 ? 1 : count);
        size_t nested_idx = create_nested_alloc(arg_index, total, elem_size, count);
        NestedAlloc& na = nested_allocs[nested_idx];
        for (size_t idx = 0; idx < count; ++idx) {
          marshal_value_to_buffer_element(arr->values[idx], na.buf.data() + idx * elem_size, elem_size, arg_index);
        }
        void* p = na.buf.data();
        out_type = &ffi_type_pointer;
        out_storage.resize(sizeof(void*));
        memcpy(out_storage.data(), &p, sizeof(void*));
        break;
      }
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
    Type* expected = nullptr;
    if (i < fti->params_len) {
      expected = fti->parameter_types[i];
    }

    ffi_type* ffi_ty = nullptr;
    if (expected) {
      ffi_ty = to_ffi_type(expected);
    }

    // pass arg index so nested allocs are recorded correctly per-arg
    marshal_value_into_storage(v, storage[i], ffi_ty, i);

    // fallback: if ffi_ty is null, default to pointer
    if (!ffi_ty) {
      ffi_ty = &ffi_type_pointer;
    }

    arg_types[i] = ffi_ty;
    arg_values[i] = storage[i].data();
  }

  // Unmarshal a single managed Value from a region of memory (storage_ptr)
  void unmarshal_value(Value* v, void* storage_ptr) {
    switch (v->get_value_type()) {
      case ValueType::INTEGER: {
        // read into 64-bit and store into IntValue
        uint64_t tmp64 = 0;
        memcpy(&tmp64, storage_ptr, std::min<size_t>(sizeof(tmp64), sizeof(uint64_t)));
        v->as<IntValue>()->value = static_cast<size_t>(tmp64);
        break;
      }
      case ValueType::FLOATING: {
        double tmp = 0.0;
        memcpy(&tmp, storage_ptr, std::min<size_t>(sizeof(tmp), sizeof(double)));
        v->as<FloatValue>()->value = tmp;
        break;
      }
      case ValueType::BOOLEAN: {
        uint8_t tmp = 0;
        memcpy(&tmp, storage_ptr, std::min<size_t>(sizeof(tmp), sizeof(uint8_t)));
        v->as<BoolValue>()->value = (tmp != 0);
        break;
      }
      case ValueType::CHARACTER: {
        char tmp = 0;
        memcpy(&tmp, storage_ptr, std::min<size_t>(sizeof(tmp), sizeof(char)));
        v->as<CharValue>()->value = tmp;
        break;
      }
      case ValueType::POINTER: {
        PointerValue* pv = v->as<PointerValue>();
        if (pv->ptr && *pv->ptr) {
          // storage_ptr is pointer-sized pointer to buffer; read it and unmarshal into pointee
          void* inner_storage = nullptr;
          memcpy(&inner_storage, storage_ptr, sizeof(void*));
          if (inner_storage) {
            unmarshal_value(*pv->ptr, inner_storage);
          }
        }
        break;
      }
      case ValueType::ARRAY: {
        ArrayValue* arr = v->as<ArrayValue>();
        size_t sz = arr->type->base_type->size_in_bytes();
        uint8_t* ptr = reinterpret_cast<uint8_t*>(storage_ptr);
        size_t offset = 0;
        for (auto& elem : arr->values) {
          unmarshal_value(elem, ptr + offset);
          offset += sz;
        }
        break;
      }
      case ValueType::RAW_POINTER: {
        // If caller expects a raw pointer, write the pointer value into the RawPointerValue->ptr field.
        RawPointerValue* rp = v->as<RawPointerValue>();
        void* p = nullptr;
        memcpy(&p, storage_ptr, sizeof(void*));
        rp->ptr = reinterpret_cast<char*>(p);
        break;
      }
      default: {
        // unknown: ignore
        break;
      }
    }
  }

  // After ffi_call, writeback nested buffers to their original managed Values.
  // Use the per-arg nested_alloc_indices_by_arg mapping.
  void writeback_pointer_values(const std::vector<Value*>& args) {
    for (size_t argi = 0; argi < args.size(); ++argi) {
      if (args[argi]->get_value_type() != ValueType::POINTER && args[argi]->get_value_type() != ValueType::ARRAY) {
        continue;
      }

      // For arrays and pointers we may have nested allocs recorded under this arg index.
      std::vector<size_t>& indices = nested_alloc_indices_by_arg[argi];
      for (size_t idx_i = 0; idx_i < indices.size(); ++idx_i) {
        size_t nested_idx = indices[idx_i];
        NestedAlloc& na = nested_allocs[nested_idx];
        // If the arg is a POINTER, then args[argi] is a PointerValue* whose pointee should be written back
        if (args[argi]->get_value_type() == ValueType::POINTER) {
          PointerValue* pv = args[argi]->as<PointerValue>();
          if (pv->ptr && *pv->ptr) {
            // na.buf.data() is the buffer the C function operated on
            unmarshal_value(*pv->ptr, na.buf.data());
          }
        } else if (args[argi]->get_value_type() == ValueType::ARRAY) {
          // args[argi] is the ArrayValue passed; copy back elementwise
          ArrayValue* arr = args[argi]->as<ArrayValue>();
          size_t elem_size = na.elem_size;
          uint8_t* base = na.buf.data();
          for (size_t e = 0; e < arr->values.size(); ++e) {
            unmarshal_value(arr->values[e], base + e * elem_size);
          }
        }
      }
    }
  }
};

Value* unmarshal_pointer_value(Type* rtype, const std::vector<uint8_t>& ret_storage) {
  char* ptr = nullptr;
  memcpy(&ptr, ret_storage.data(), sizeof(char*));
  return new_raw_pointer(rtype, ptr);
}

Value* unmarshal_scalar_return(Type* rtype, ScalarTypeInfo* info, const std::vector<uint8_t>& ret_storage) {
  if (ret_storage.empty()) {
    return null_value();
  }

  if (rtype->is_pointer()) {
    return unmarshal_pointer_value(rtype, ret_storage);
  }
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

Value* compile_time_ffi_dispatch(InternedString& name, FunctionTypeInfo* fti, const std::vector<Value*>& args) {
  void* fn_ptr = try_load_symbol(name.get_str());

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

  if (ret_type) {
    if (!ret_type->is_kind(TYPE_SCALAR)) {
      throw_error("ffi doesn't support non-scalar return types", {});
    }
  }

  auto ret_ty_scalar_info = ret_type ? ret_type->info->as<ScalarTypeInfo>() : nullptr;
  std::vector<uint8_t> ret_storage(ret_size);

  FFIContext ctx(args.size());
  for (size_t i = 0; i < args.size(); ++i) {
    ctx.marshal_arg(fti, i, args[i]);
  }

  ffi_cif cif{};
  ffi_type* ffi_ret = to_ffi_type(fti->return_type);

  int prep_ok = 0;
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

  return unmarshal_scalar_return(ret_type, ret_ty_scalar_info, ret_storage);
}
