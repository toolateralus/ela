/*
  This should ONLY ever be included in main, or the LSP's main.

  this defines all the externs from the program all over the place
*/

#include <cstdio>
#include <cstdlib>
#include <unordered_map>

#include "arena.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

jstl::Arena type_info_arena{MB(10)};
jstl::Arena scope_arena{MB(10)};
jstl::Arena ast_arena{MB(10)};
jstl::Arena thir_arena(MB(10));
jstl::Arena value_arena(MB(10));
jstl::Arena binding_arena(MB(10));

std::vector<std::string> DYNAMIC_LIBRARY_LOAD_PATH{};

std::vector<Type *> type_table{};
std::vector<Type *> structural_type_table{};
std::vector<Type *> function_type_table{};
std::unordered_map<std::string, void *> loaded_ffi_extern_functions{};

size_t lambda_unique_id = 0;

void *error_user_data;

PanicHandler panic_handler = get_default_panic_handler();

CompileCommand compile_command;

std::unordered_map<InternedString, Scope *> import_scopes;
std::unordered_set<InternedString> include_set;

Type *g_refl_Field_type = nullptr;
Type *g_refl_Method_type = nullptr;
Type *g_refl_Type_type = nullptr;
Type *g_testing_Test_type = nullptr;
Type *g_Destroy_trait_type = nullptr;
Type *g_str_type = nullptr;
Type *g_String_type = nullptr;

ASTVariable *g_testing_tests_declaration = nullptr;
ASTFunctionDeclaration *g_testing_runner_declaration = nullptr;

ASTStructDeclaration *g_List_declaration = nullptr;
ASTStructDeclaration *g_InitList_declaration = nullptr;
ASTStructDeclaration *g_Slice_declaration = nullptr;
ASTStructDeclaration *g_SliceMut_declaration = nullptr;
ASTChoiceDeclaration *g_Option_type = nullptr;

Type *g_Init_trait_type = nullptr, *g_Iterable_trait_type = nullptr, *g_Iterator_trait_type = nullptr;

int ignored_warnings = 0;

bool run_on_finished = false;

std::string current_lexer_path, previous_lexer_path;

/* Global storage for commonly used types and traits.
  These are initialized in init_type_system(). */
Type *g_u64_type;
Type *g_u32_type;
Type *g_u16_type;
Type *g_u8_type;
Type *g_u8_ptr_type;
Type *g_s64_type;
Type *g_s32_type;
Type *g_s16_type;
Type *g_s8_type;
Type *g_f64_type;
Type *g_f32_type;
Type *g_char_type;
Type *g_char_ptr_type;
Type *g_bool_type;
Type *g_void_type;
Type *g_unit_type;

/* Traits */
Type *g_is_fn_ptr_trait;
Type *g_is_fn_trait;
Type *g_is_tuple_trait;
Type *g_is_struct_trait;
Type *g_is_enum_trait;
Type *g_is_choice_trait;
Type *g_is_dyn_trait;
Type *g_is_union_trait;
Type *g_is_array_trait;
Type *g_is_pointer_trait;
Type *g_is_mut_pointer_trait;
Type *g_is_const_pointer_trait;
Type *g_is_slice_trait;
Type *g_is_slice_mut_trait;
Type *g_blittable_trait;

void reset_all_global_state() {
  using arena = jstl::Arena;
  ast_arena.~arena();
  scope_arena.~arena();
  type_table.clear();
  structural_type_table.clear();
  function_type_table.clear();
  import_scopes.clear();
  include_set.clear();
  type_info_arena.~arena();
  thir_arena.~arena();
  value_arena.~arena();
  binding_arena.~arena();

  reset_global_types();

  new (&type_info_arena) jstl::Arena(MB(10));
  new (&scope_arena) jstl::Arena(MB(10));
  new (&ast_arena) jstl::Arena(MB(10));
  new (&thir_arena) jstl::Arena(MB(10));
  new (&value_arena) jstl::Arena(MB(10));
  new (&binding_arena) jstl::Arena(MB(10));

  // clear containers and tables
  DYNAMIC_LIBRARY_LOAD_PATH.clear();
  type_table.clear();
  structural_type_table.clear();
  function_type_table.clear();
  loaded_ffi_extern_functions.clear();

  lambda_unique_id = 0;

  // reset runtime / handlers
  error_user_data = nullptr;
  panic_handler = get_default_panic_handler();
  compile_command = CompileCommand{};

  // clear scope/include tracking
  import_scopes.clear();
  include_set.clear();

  // reset global type/AST pointers
  g_refl_Field_type = nullptr;
  g_refl_Method_type = nullptr;
  g_refl_Type_type = nullptr;
  g_testing_Test_type = nullptr;
  g_Destroy_trait_type = nullptr;
  g_str_type = nullptr;
  g_String_type = nullptr;

  g_testing_tests_declaration = nullptr;
  g_testing_runner_declaration = nullptr;

  g_List_declaration = nullptr;
  g_InitList_declaration = nullptr;
  g_Slice_declaration = nullptr;
  g_SliceMut_declaration = nullptr;
  g_Option_type = nullptr;

  g_Init_trait_type = nullptr;
  g_Iterable_trait_type = nullptr;
  g_Iterator_trait_type = nullptr;

  ignored_warnings = 0;
  run_on_finished = false;

  current_lexer_path.clear();
  previous_lexer_path.clear();
}