#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"

void get_varargs_handlers(Context *c) {
  auto scope = c->root_scope;

  auto va_list_type = global_create_type(TYPE_STRUCT, "va_list", nullptr, {}, nullptr);
  va_list_type->info->as<StructTypeInfo>()->flags |= STRUCT_IS_FORWARD_DECLARED;
  scope->insert_type(va_list_type, "va_list", TYPE_STRUCT, nullptr);
  
  FunctionTypeInfo func_type_info;
  func_type_info.is_varargs = true;
  func_type_info.return_type = void_type();

  auto va_func_type = global_find_function_type_id(func_type_info, {});

  scope->insert_function("va_start", va_func_type, nullptr);
  scope->insert_function("va_end", va_func_type, nullptr);
}

Context::Context() {
  scope = create_child(nullptr);
  root_scope = scope;

#if defined(__linux)
  scope->defines().insert("PLATFORM_LINUX");
#elif defined(_WIN32)
  scope->defines().insert("PLATFORM_WINDOWS");
#elif defined(__APPLE__)
  scope->defines().insert("PLATFORM_MACOS");
#elif defined(__ANDROID__)
  scope->defines().insert("PLATFORM_ANDROID");
#elif defined(__unix__)
  scope->defines().insert("PLATFORM_UNIX");
#elif defined(__FreeBSD__)
  scope->defines().insert("PLATFORM_FREEBSD");
#endif

  get_varargs_handlers(this);

  if (compile_command.has_flag("freestanding"))
    Scope::defines().insert("FREESTANDING");

  if (compile_command.has_flag("test")) {
    Scope::add_def("TESTING");
  }

  for (int i = 0; i < type_table.size(); ++i) {
    if (type_table[i]->info->scope) {
      type_table[i]->info->scope->parent = scope;
    }
    scope->create_type_alias(type_table[i]->basename, type_table[i], type_table[i]->kind, nullptr);
  }
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

bool Symbol::is_generic_function() const {
  if (!is_function())
    return false;

  auto declaration = this->function.declaration;

  if (declaration->generic_parameters.size() != 0 || declaration->generic_arguments.size() != 0 ||
      declaration->generic_instantiations.size() != 0) {
    return true;
  }

  return false;
}