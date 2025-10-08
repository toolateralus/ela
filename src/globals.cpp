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