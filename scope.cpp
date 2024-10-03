#include "scope.hpp"
#include "ast.hpp"
#include "type.hpp"

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
    scope->insert("assert", global_find_function_type_id("void(char *, bool)", assert_info, {}), SYMBOL_IS_FUNCTION);

    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = global_find_type_id("s64", {});
    sizeof_info.is_varargs = true;
    // no other function will ever use this type. thats why we have a ?, because we have no first class types yet.
    scope->insert("sizeof", global_find_function_type_id("s64(?)", sizeof_info, {}), SYMBOL_IS_FUNCTION);
    
  }
  
  // define types used for reflection, which are currently half implemented due to ineffectiveness.
  {
    auto type_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto field_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    type_scope->parent= root_scope;
    field_scope->parent=root_scope;
    auto type_id = global_create_struct_type("Type", type_scope);
    auto field_id = global_create_struct_type("Field", field_scope);
    
    // Type*
    auto type_ptr = global_find_type_id("Type", {.extensions = {TYPE_EXT_POINTER}});
    // Field* []
    auto field_arr = global_find_type_id("Field", {.extensions = {TYPE_EXT_POINTER, TYPE_EXT_ARRAY}, .array_sizes = {-1}});
    // Field*
    auto field_ptr = global_find_type_id("Field", {.extensions = {TYPE_EXT_POINTER}});
    
    type_scope->insert("id", s32_type());
    type_scope->insert("name", charptr_type());
    type_scope->insert("fields", field_arr);

    field_scope->insert("name", charptr_type());
    field_scope->insert("type", type_ptr);
  }
  
  // string struct, stores length info.
  {
    auto str_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type_id = global_create_struct_type("string", str_scope);
    auto type = global_get_type(type_id);
    
    static_cast<StructTypeInfo*>(type->info)->implicit_cast_table = {
      charptr_type(),
    };
    
    str_scope->insert("data", charptr_type());
    str_scope->insert("length", s32_type());
    str_scope->insert("[", s8_type());
  }
  
  // add the base types for all of the primitives to the root scope.
  for (int i = 0; i < num_types; ++i) {
    auto type = global_get_type(i);
    root_scope->types.insert(i);
  }
  
  // TODO fix segfault when accessing this table from in-language.
  root_scope->insert("_type_info", global_find_type_id("Type", {.extensions = {TYPE_EXT_POINTER, TYPE_EXT_ARRAY}, .array_sizes = {-1}}));
}

std::string Scope::get_function_typename(ASTFunctionDeclaration *decl) {
  std::stringstream ss;
  auto return_type = decl->return_type;
  ss << get_type(return_type->resolved_type)->to_string();
  ss << "(";
  for (const auto &param : decl->params->params) {
    ss << get_type(param->type->resolved_type)->to_string();
    if (param != decl->params->params.back()) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

int Scope::find_function_type_id(const std::string &name, const FunctionTypeInfo &info,
                        const TypeExt &ext) {
  if (global_type_aliases.contains(name)) {
    return find_alias(name, ext);
  }
  auto id =  global_find_function_type_id(name, info, ext);
  root_scope->types.insert(id);
  return id;
}
int Scope::find_type_id(const std::string &name, const TypeExt &ext) {
  if (global_type_aliases.contains(name)) {
    return find_alias(name, ext);
  }
  auto id = global_find_type_id(name, ext);
  if (types.contains(id))
    return id;
  auto base_id = global_find_type_id(name, {});
  
  // this is likely for a newly created type that just extends an existing type that lives in the root scope.
  // such as a function or method that instantiates a float* where there have been no float* in the rest of the program yet,
  if (base_id != -1 && id == -1 && root_scope->types.contains(base_id)) {
    id = global_find_type_id(name, ext);
    root_scope->types.insert(id);
    return id;
  }
  
  // a type (possibly our local type) with new extensions got created, so we just add it to this
  // scope.
  if (!types.contains(id) && types.contains(base_id)) {
    types.insert(id);
    return id;
  }
  
  if (parent) {
    auto id = parent->find_type_id(name, ext);
    if (id != -1)
      return id;
  }
  
  return -1;
}
int Scope::find_alias(const std::string name, const TypeExt &ext) {
  for (const auto &[symname, sym] : symbols) {
    if (symname != name)
      continue;
    auto type = global_get_type(sym.type_id);
    if (type->extensions.equals({})) {
      return type->id;
    } else {
      auto new_exts = type->extensions;
      auto in_exts = ext;
      for (const auto &ext_ : in_exts.extensions) {
        if (ext_ == TYPE_EXT_ARRAY) {
          auto back = in_exts.array_sizes.back();
          in_exts.array_sizes.pop_back();
          new_exts.array_sizes.push_back(back);
          new_exts.extensions.push_back(TYPE_EXT_ARRAY);
        } else {
          new_exts.extensions.push_back(TYPE_EXT_POINTER);
        }
      }
      auto new_id = global_find_type_id(name, new_exts);
      types.insert(new_id);
      return new_id;
    }
  }
  if (parent) {
    return parent->find_alias(name, ext);
  }
  return -1;
}

Type *Scope::get_type(int id) const {
  if (types.contains(id)) return global_get_type(id);
  if (parent) {
    return parent->get_type(id);
  }
  
  // !BUG: This makes no sense, yet this fixes a segfault that's happening.
  // if (!parent) {
  //   return global_get_type(id);
  // }
  return nullptr;
}

int Scope::create_struct_type(const std::string &name, Scope *scope) {
  auto type = ::global_create_struct_type(name, scope);
  types.insert(type);
  return type;
}
int Scope::create_union_type(const std::string &name, Scope *scope,
                             UnionKind kind) {
  auto type = ::global_create_union_type(name, scope, kind);
  types.insert(type);
  return type;
}
int Scope::create_enum_type(const std::string &name,
                            const std::vector<std::string> &keys,
                            bool is_flags) {
  auto type = ::global_create_enum_type(name, keys, is_flags);
  types.insert(type);
  return type;
}
int Scope::create_type(TypeKind kind, const std::string &name, TypeInfo *info,
                       const TypeExt &extensions) {
  auto type = ::global_create_type(kind, name, info, extensions);
  types.insert(type);
  return type;
}
void Scope::insert(const std::string &name, int type_id, int flags) {
  auto sym = Symbol{name, type_id, flags};
  symbols[name] = sym; 
  if (sym.is_type_alias()) {
    global_type_aliases.insert({name, type_id});
  }
}
Symbol *Scope::lookup(const std::string &name) {
  if (symbols.find(name) != symbols.end()) {
    return &symbols[name];
  } else if (parent) {
    return parent->lookup(name);
  }
  return nullptr;
}
void Scope::erase(const std::string &name) { symbols.erase(name); }

void Scope::on_scope_enter() {
  for (const auto &[id, sym] : symbols) {
    if (sym.is_type_alias()) {
      global_type_aliases.insert({id, sym.type_id});
    }
  }
}
void Scope::on_scope_exit() {
  for (const auto &[id, sym] : symbols) {
    if (sym.is_type_alias()) {
      auto pointed_to = global_get_type(sym.type_id);
      auto it = global_type_aliases.find(sym.name);
      if (it != global_type_aliases.end())
        global_type_aliases.erase(it);
    }
  }
}
