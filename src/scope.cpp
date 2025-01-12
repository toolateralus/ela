#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"

Context::Context() {
  root_scope = scope;

#if defined(__linux)
  root_scope->defines().insert("PLATFORM_LINUX");
#elif defined(_WIN32)
  root_scope->defines().insert("PLATFORM_WINDOWS");
#elif defined(__APPLE__)
  root_scope->defines().insert("PLATFORM_MACOS");
#elif defined(__ANDROID__)
  root_scope->defines().insert("PLATFORM_ANDROID");
#elif defined(__unix__)
  root_scope->defines().insert("PLATFORM_UNIX");
#elif defined(__FreeBSD__)
  root_scope->defines().insert("PLATFORM_FREEBSD");
#endif

  if (compile_command.has_flag("test")) {
    root_scope->add_def("TESTING");
  }

  {
    // ** DO NOT REMOVE ***
    scope->types["c_string"] = c_string_type() = charptr_type();
    // ** ------------- ***
  }
  // Range type
  {
    auto range_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type = global_create_struct_type("Range", range_scope);
    // ** DO NOT REMOVE ***
    range_type() = type;
    // ** ------------- ***
    range_scope->insert("first", s64_type());
    range_scope->insert("last", s64_type());
    range_scope->insert("span", s64_type());
    range_scope->insert("increment", s64_type());

    auto func = FunctionTypeInfo{};
    func.params_len = 1;
    func.parameter_types[0] = s64_type();
    func.return_type = bool_type();
    range_scope->insert("contains", global_find_function_type_id(func, {}));
    range_scope->parent = root_scope;
    root_scope->types.insert({"Range", type});
  }

  // define some default functions that may or may not be macros.
  {
    // We still define assert and sizeof manually here, because
    // there's no way to do this in language currently, as they're macros
    FunctionTypeInfo assert_info{};
    assert_info.return_type = void_type();
    assert_info.parameter_types[0] = charptr_type();
    assert_info.parameter_types[1] = bool_type();
    assert_info.params_len = 2;
    scope->insert("assert", global_find_function_type_id(assert_info, {}), SYMBOL_IS_FUNCTION);

    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = u32_type();
    sizeof_info.is_varargs = true;
    // no other function will ever use this type. thats why we have a ?, because
    // we have no first class types yet.
    scope->insert("sizeof", global_find_function_type_id(sizeof_info, {}), SYMBOL_IS_FUNCTION);
  }

  // define types used for reflection.
  {
    auto type_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto field_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto element_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();

    type_scope->parent = root_scope;
    field_scope->parent = root_scope;
    element_scope->parent = root_scope;

    auto type_id = global_create_struct_type("Type", type_scope);
    auto field_id = global_create_struct_type("Field", field_scope);
    auto element_id = global_create_struct_type("Element", element_scope);

    // Type*
    auto type_ptr = global_find_type_id(type_id, {.extensions = {{TYPE_EXT_POINTER}}});

    // Field*[]
    auto field_arr = global_find_type_id(field_id, {.extensions = {{TYPE_EXT_POINTER}, {TYPE_EXT_ARRAY}}});
    // Element[]
    auto element_arr = global_find_type_id(element_id, {.extensions = {{TYPE_EXT_ARRAY}}});
    // Field*
    auto field_ptr = global_find_type_id(field_id, {.extensions = {{TYPE_EXT_POINTER}}});

    type_scope->insert("id", s32_type());
    type_scope->insert("name", charptr_type());
    type_scope->insert("fields", field_arr);
    type_scope->insert("size", u64_type());
    type_scope->insert("flags", u64_type());
    type_scope->insert("element_type", type_ptr);

    field_scope->insert("name", charptr_type());
    field_scope->insert("type", type_ptr);
    field_scope->insert("size", u64_type());
    field_scope->insert("offset", u64_type());

    element_scope->insert("data", charptr_type());
    element_scope->insert("type", type_ptr);

    // field.get()
    auto get_info = FunctionTypeInfo{};
    get_info.is_varargs = true;
    get_info.return_type = charptr_type();
    auto _t = global_find_function_type_id(get_info, {});
    field_scope->insert("get", _t, SYMBOL_IS_FUNCTION);
    auto get_sym = field_scope->local_lookup("get");
    get_sym->type_id = _t;

    // field.set()
    auto set_info = FunctionTypeInfo{};
    set_info.is_varargs = true;
    set_info.return_type = void_type();
    _t = global_find_function_type_id(set_info, {});
    field_scope->insert("set", _t, SYMBOL_IS_FUNCTION);
    auto set_sym = field_scope->local_lookup("set");
    set_sym->type_id = _t;

    // type.elements()
    auto elements_info = FunctionTypeInfo{};
    elements_info.return_type = element_arr;
    elements_info.params_len = 1;
    elements_info.parameter_types[0] = charptr_type();
    _t = global_find_function_type_id(elements_info, {});
    type_scope->insert("elements", _t, SYMBOL_IS_FUNCTION);
    auto elements_sym = type_scope->local_lookup("elements");
    elements_sym->type_id = _t;
  }

  // string struct, stores length info.
  {
    auto str_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    str_scope->parent = root_scope;

    auto type_id = global_create_struct_type("string", str_scope);
    // ** DO NOT REMOVE **
    string_type() = type_id;
    // ** ------------- **

    auto type = global_get_type(type_id);
    static_cast<StructTypeInfo *>(type->get_info())->implicit_cast_table = {
        charptr_type(),
        c_string_type(),
    };

    str_scope->insert("data", charptr_type());
    str_scope->insert("length", s32_type());
    str_scope->insert("is_view", bool_type());
    auto func = FunctionTypeInfo{};
    func.parameter_types[0] = char_type();
    func.return_type = void_type();
    func.params_len = 1;
  }

  // Env type
  {
    auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type = global_create_struct_type("Env", scope);

    auto func = FunctionTypeInfo{};
    func.params_len = 0;
    auto str_array = global_find_type_id(string_type(), TypeExtensions{.extensions = {{TYPE_EXT_ARRAY}}});
    func.return_type = str_array;
    scope->insert("args", global_find_function_type_id(func, {}));
    scope->parent = root_scope;
    root_scope->types.insert({"Env", type});
  }

  auto info = FunctionTypeInfo{};
  info.is_varargs = true;
  info.return_type = void_type();
  root_scope->insert("destruct", global_find_function_type_id(info, {}), SYMBOL_IS_FUNCTION);

  info.is_varargs = true;
  info.return_type = void_type();
  root_scope->insert("move", global_find_function_type_id(info, {}), SYMBOL_IS_FUNCTION);

  // TODO: make a more succint way to interact with tuples. This is garbo trash, and it totally dodges our type system.
  root_scope->insert("get", global_find_function_type_id(info, {}), SYMBOL_IS_FUNCTION);

  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i].get_info()->scope) {
      type_table[i].get_info()->scope->parent = root_scope;
    }
    root_scope->types.insert({type_table[i].get_base(), i});
  }
}

void Scope::insert(const InternedString &name, int type_id, int flags) {
  auto sym = Symbol{name, type_id, flags};
  symbols[name] = sym;
  ordered_symbols.push_back(name);
}

Symbol *Scope::lookup(const InternedString &name) {
  if (symbols.find(name) != symbols.end()) {
    return &symbols[name];
  } else if (parent) {
    return parent->lookup(name);
  }
  return nullptr;
}

void Scope::erase(const InternedString &name) {
  symbols.erase(name);
  ordered_symbols.erase(std::remove(ordered_symbols.begin(), ordered_symbols.end(), name), ordered_symbols.end());
}

void Scope::declare_interface(const InternedString &name, ASTInterfaceDeclaration *node) {
  symbols[name] = {
    .name = name,
    .declaring_node = node,
  };
}
