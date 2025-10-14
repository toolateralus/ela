#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <ostream>
#include <unordered_map>

#include "arena.hpp"
#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "scope.hpp"
#include "type.hpp"

/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/

using std::string;
using std::vector;

size_t global_num_symbols_declared; // used to key symbols.


// CLEANUP: cut these sizes down, i don't think its gaining us anything since
// we have a linked list style arena these days.
jstl::Arena symbol_arena{MB(10)};
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

#include <string>

int main(int argc, char *argv[]) {
  if (const char *env_p = std::getenv("ELA_LIB_PATH")) {
    if (terminal_supports_color) {
      std::cout << "\033[1;34mnote\033[0m: environment variable 'ELA_LIB_PATH' is set, loading import libraries "
                   "from='\033[1;32m"
                << env_p << "\033[0m'\n";
    } else {
      std::cout << "note: environment variable 'ELA_LIB_PATH' is set, loading import libraries from='" << env_p
                << "'\n";
    }
  }

  std::vector<std::string> runtime_args;
  bool run_on_finished = false, run_tests = false, lldb = false;
  compile_command =
      CompileCommand(std::vector<string>(argv, argc + argv), runtime_args, run_on_finished, run_tests, lldb);

  auto result = compile_command.compile();

  if (lldb) {
    std::string invocation = "lldb ./" + compile_command.binary_path.string();
    std::string args;
    for (const auto &arg : runtime_args) {
      args += arg + " ";
    }
    std::string command = invocation + " " + args;
    std::cout << "running lldb: " << command << std::endl;
    system(command.c_str());
    return 0;
  }

  if (run_on_finished && result == 0) {
    std::string invocation = "./" + compile_command.binary_path.string();
    std::string args;
    for (const auto &arg : runtime_args) {
      args += arg + " ";
    }
    std::string command = invocation + " " + args;
    std::cout << "\033[1;30mrunning:\033[0m " << "\033[1;34m" << command << "\033[0m" << std::endl;
    auto status = system(command.c_str());

    if (status == -1) {
      perror("system");
      exit(1);
    } else {
      if (WIFEXITED(status)) {
        int exit_status = WEXITSTATUS(status);
        if (exit_status != 0) {
          std::cerr << "\033[1;31mexited with code " << exit_status << "\033[0m\n";
        }
      } else if (WIFSIGNALED(status)) {
        int signal_number = WTERMSIG(status);
        std::cerr << "\033[1;31mterminated by signal\033[1;34m " << strsignal(signal_number) << "\033[0m\n";
      } else {
        std::cerr << "\033[1;31mfailed with unknown status\033[0m\n";
      }
    }
  }

  if (run_tests) {
    system("rm test");
  }

  return result != 0;
}
