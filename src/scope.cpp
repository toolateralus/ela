#include "scope.hpp"
#include "type.hpp"

Context::Context() {
  root_scope = scope;

  // Range type
  {
    auto range_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type = global_create_struct_type("Range", range_scope);
    range_scope->insert("first", s64_type());
    range_scope->insert("last", s64_type());
    range_scope->insert("span", s64_type());

    auto func = FunctionTypeInfo{};
    func.params_len=1;
    func.parameter_types[0] = s64_type();
    func.return_type = bool_type();
    range_scope->insert("contains", global_find_function_type_id("bool(s64)", func, {}));
    range_scope->parent = root_scope;
    root_scope->types.insert(type);
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
    scope->insert(
        "assert",
        global_find_function_type_id("void(char *, bool)", assert_info, {}),
        SYMBOL_IS_FUNCTION);

    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = u32_type();
    sizeof_info.is_varargs = true;
    // no other function will ever use this type. thats why we have a ?, because
    // we have no first class types yet.
    scope->insert("sizeof",
                  global_find_function_type_id("s64(?)", sizeof_info, {}),
                  SYMBOL_IS_FUNCTION);
  }

  // define types used for reflection.
  {
    auto type_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto field_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto element_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();

    type_scope->parent = root_scope;
    field_scope->parent = root_scope;
    element_scope->parent = root_scope;

    global_create_struct_type("Type", type_scope);
    global_create_struct_type("Field", field_scope);
    global_create_struct_type("Element", element_scope);

    // Type*
    auto type_ptr =
        global_find_type_id("Type", {.extensions = {TYPE_EXT_POINTER}});
        
    // Field*[]
    auto field_arr = global_find_type_id(
        "Field", {.extensions = {TYPE_EXT_POINTER, TYPE_EXT_ARRAY},
                  .array_sizes = {nullptr}});
    // Element[]
    auto element_arr = global_find_type_id(
        "Element", {.extensions = {TYPE_EXT_ARRAY},
                  .array_sizes = {nullptr}});
    // Field*
    global_find_type_id("Field", {.extensions = {TYPE_EXT_POINTER}});

    type_scope->insert("id", s32_type());
    type_scope->insert("name", charptr_type());
    type_scope->insert("fields", field_arr);
    type_scope->insert("size", u64_type());
    type_scope->insert("flags", u64_type());

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
    auto _t = global_find_function_type_id("s8*(...)", get_info, {});
    field_scope->insert("get", _t, SYMBOL_IS_FUNCTION);
    auto get_sym = field_scope->local_lookup("get");
    get_sym->function_overload_types.push_back(_t);

    // field.set()
    auto set_info = FunctionTypeInfo{};
    set_info.is_varargs = true;
    set_info.return_type = void_type();
    _t = global_find_function_type_id("void(...)", set_info, {});
    field_scope->insert("set", _t, SYMBOL_IS_FUNCTION);
    auto set_sym = field_scope->local_lookup("set");
    set_sym->function_overload_types.push_back(_t);

    // type.elements()
    auto elements_info = FunctionTypeInfo{};
    elements_info.return_type = element_arr;
    elements_info.params_len = 1;
    elements_info.parameter_types[0] = charptr_type();
    _t = global_find_function_type_id("Element[](char*)", elements_info, {});
    type_scope->insert("elements", _t, SYMBOL_IS_FUNCTION);
    auto elements_sym = type_scope->local_lookup("elements");
    elements_sym->function_overload_types.push_back(_t);
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
    str_scope->insert("is_view", bool_type()); // is this a borrowing copy?

    str_scope->insert("[", s8_type(), SYMBOL_IS_FUNCTION);

    auto func = FunctionTypeInfo{};
    func.parameter_types[0] = char_type();
    func.return_type = void_type();
    func.params_len=1;

    str_scope->insert("push", global_find_function_type_id("void(char)", func, {}), SYMBOL_IS_FUNCTION);

    func.parameter_types[0] = -1;
    func.params_len=0;
    func.return_type=char_type();
    str_scope->insert("pop", global_find_function_type_id("char()", func, {}), SYMBOL_IS_FUNCTION);


    func.parameter_types[0] = int_type();
    func.params_len=1;
    func.return_type=void_type();
    str_scope->insert("erase_at", global_find_function_type_id("void(int)", func, {}), SYMBOL_IS_FUNCTION);

    func.parameter_types[0] = int_type();
    func.parameter_types[1] = char_type();
    func.params_len = 2;
    func.return_type= void_type();
    str_scope->insert("insert_at", global_find_function_type_id("void(int, char)", func, {}), SYMBOL_IS_FUNCTION);

    func.parameter_types[1] = global_find_type_id("string", {});
    str_scope->insert("insert_substr_at", global_find_function_type_id("void(int, char)", func, {}), SYMBOL_IS_FUNCTION);

    func.params_len=1;
    func.return_type = global_find_type_id("string", {});
    func.parameter_types[0]= global_find_type_id("Range", {});
    str_scope->insert("substr", global_find_function_type_id("string(Range)", func, {}), SYMBOL_IS_FUNCTION);

    auto sym = str_scope->local_lookup("[");
    auto info = type_info_alloc<FunctionTypeInfo>();
    info->parameter_types[0] = int_type();
    info->return_type = s8_type();
    sym->function_overload_types.push_back(global_find_function_type_id("s8(int)", *info, {}));

  }


  // Env type
  {
    auto scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type = global_create_struct_type("Env", scope);

    auto func = FunctionTypeInfo{};
    func.params_len=0;
    auto str_array = global_find_type_id("string", TypeExt{.extensions = {TYPE_EXT_ARRAY}, .array_sizes = {nullptr}});
    func.return_type = str_array; 
    scope->insert("args", global_find_function_type_id("string[]()", func, {}));
    scope->parent = root_scope;
    root_scope->types.insert(type);
  }


  auto info = FunctionTypeInfo{};
  info.is_varargs = true;
  info.return_type = void_type();
  root_scope->insert("destruct", global_find_function_type_id("void(...)", info, {}), SYMBOL_IS_FUNCTION);

  info.is_varargs = true;
  info.return_type = void_type();
  root_scope->insert("move", global_find_function_type_id("void(...)", info, {}), SYMBOL_IS_FUNCTION);


  // TODO: make a more succint way to interact with tuples. This is garbo trash, and it totally dodges our type system.
  root_scope->insert("get", global_find_function_type_id("void(...)", info, {}), SYMBOL_IS_FUNCTION);


  for (int i = 0; i < type_table.size(); ++i) {
    root_scope->types.insert(i);
  }



}

void Scope::insert(const InternedString &name, int type_id, int flags) {
  auto sym = Symbol{name, type_id, flags};
  symbols[name] = sym;
  ordered_symbols.push_back(name);
}

/*
  !BUG !!! SUPER CRITICAL !!!
  ! Sometimes in methods we get a cyclic scope reference. I Don't want to right now but this most certainly needs to be resolved STAT
  ! There is a repro for this, with a possible and likely explanation for why this is happening
*/

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
