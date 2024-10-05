#include "scope.hpp"
#include "type.hpp"

#include <dlfcn.h>

Context::Context() {
  root_scope = scope;
  // define some default functions that may or may not be macros.
  {
    // We still define assert and sizeof manually here, because
    // there's no way to do this in language currently, as they're macros
    FunctionTypeInfo assert_info{};
    assert_info.return_type = void_type();
    assert_info.parameter_types[0] = charptr_type();
    assert_info.parameter_types[1] = bool_type();
    assert_info.params_len = 2;
    scope->insert(
        "assert",
        global_find_function_type_id("void(char *, bool)", assert_info, {}),
        SYMBOL_IS_FUNCTION);

    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = global_find_type_id("s64", {});
    sizeof_info.is_varargs = true;
    // no other function will ever use this type. thats why we have a ?, because
    // we have no first class types yet.
    scope->insert("sizeof",
                  global_find_function_type_id("s64(?)", sizeof_info, {}),
                  SYMBOL_IS_FUNCTION);
  }

  // define types used for reflection, which are currently half implemented due
  // to ineffectiveness.
  {
    auto type_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto field_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    type_scope->parent = root_scope;
    field_scope->parent = root_scope;
    auto type_id = global_create_struct_type("Type", type_scope);
    auto field_id = global_create_struct_type("Field", field_scope);

    // Type*
    auto type_ptr =
        global_find_type_id("Type", {.extensions = {TYPE_EXT_POINTER}});
    // Field* []
    auto field_arr = global_find_type_id(
        "Field", {.extensions = {TYPE_EXT_POINTER, TYPE_EXT_ARRAY},
                  .array_sizes = {-1}});
    // Field*
    auto field_ptr =
        global_find_type_id("Field", {.extensions = {TYPE_EXT_POINTER}});

    type_scope->insert("id", s32_type());
    type_scope->insert("name", charptr_type());
    type_scope->insert("fields", field_arr);

    field_scope->insert("name", charptr_type());
    field_scope->insert("type", type_ptr);
  }

  // string struct, stores length info.
  {
    auto str_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    str_scope->parent = root_scope;

    auto type_id = global_create_struct_type("string", str_scope);
    auto type = global_get_type(type_id);

    static_cast<StructTypeInfo *>(type->get_info())->implicit_cast_table = {
        charptr_type(),
    };

    str_scope->insert("data", charptr_type());
    str_scope->insert("length", s32_type());
    str_scope->insert("[", s8_type());
  }

  // !BUG fix segfault when accessing this table from in-language.
  root_scope->insert(
      "_type_info",
      global_find_type_id("Type",
                          {.extensions = {TYPE_EXT_POINTER, TYPE_EXT_ARRAY},
                           .array_sizes = {-1}}));
}

void Scope::insert(const std::string &name, int type_id, int flags) {
  auto sym = Symbol{name, type_id, flags};
  symbols[name] = sym;
  ordered_symbols.push_back(name);
}

Symbol *Scope::lookup(const std::string &name) {
  if (symbols.find(name) != symbols.end()) {
    return &symbols[name];
  } else if (parent) {
    return parent->lookup(name);
  }
  return nullptr;
}

void Scope::erase(const std::string &name) {
  symbols.erase(name);
  ordered_symbols.erase(std::remove(ordered_symbols.begin(), ordered_symbols.end(), name), ordered_symbols.end());
}
