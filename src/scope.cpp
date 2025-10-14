#include "ast.hpp"
#include "scope.hpp"
#include "core.hpp"
#include "type.hpp"

void get_varargs_handlers(Context *c) {
  auto scope = c->root_scope;

  auto va_list_type = global_create_type(TYPE_STRUCT, "va_list", "va_list");
  va_list_type->info->as<StructTypeInfo>()->is_forward_declared = true;

  scope->insert_type("va_list", va_list_type);

  FunctionTypeInfo func_type_info;
  func_type_info.is_varargs = true;
  func_type_info.return_type = void_type();

  auto va_func_type = global_find_function_type_id(func_type_info, {});

  auto va_start = ast_alloc<ASTFunctionDeclaration>();
  auto va_end = ast_alloc<ASTFunctionDeclaration>();
  auto va_copy = ast_alloc<ASTFunctionDeclaration>();

  va_start->name = "va_start";
  va_start->parameters = ParameterList{};
  va_start->parameters.is_varargs = true;
  va_start->return_type = ast_alloc<ASTType>();
  va_start->return_type->normal.path = nullptr;
  va_start->return_type->resolved_type = void_type();
  va_start->resolved_type = va_func_type;
  scope->insert("va_start", va_func_type, CONST, va_start, {}, false);

  va_end->name = "va_end";
  va_end->parameters = ParameterList{};
  va_end->parameters.is_varargs = true;
  va_end->return_type = ast_alloc<ASTType>();
  va_end->return_type->normal.path = nullptr;
  va_end->return_type->resolved_type = void_type();
  va_end->resolved_type = va_func_type;
  scope->insert("va_end", va_func_type, CONST, va_end, {}, false);

  va_copy->name = "va_copy";
  va_copy->parameters = ParameterList{};
  va_copy->parameters.is_varargs = true;
  va_copy->return_type = ast_alloc<ASTType>();
  va_copy->return_type->normal.path = nullptr;
  va_copy->return_type->resolved_type = void_type();
  va_copy->resolved_type = va_func_type;
  scope->insert("va_copy", va_func_type, CONST, va_copy, {}, false);
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

  if (compile_command.has_flag("freestanding")) Scope::defines().insert("FREESTANDING");

  if (compile_command.has_flag("test")) {
    Scope::add_def("TESTING");
  }

  for (size_t i = 0; i < type_table.size(); ++i) {
    Type *type = type_table[i];
    scope->create_type_alias(type->basename, type, nullptr, type->generic_args, false);
  }
}