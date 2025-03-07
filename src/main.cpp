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

// This is probably WAYY over allocated but we just want to be sure there's enough space.
jstl::Arena type_info_arena{MB(10)};
// the same for this
jstl::Arena scope_arena{MB(10)};
// the same for this
jstl::Arena ast_arena{MB(10)};

std::vector<Type *> type_table{};

size_t LAMBDA_UNIQUE_ID = 0;

// TODO: remove me, we want file scopes.
Scope *root_scope;

void *error_user_data;

PanicHandler panic_handler = get_default_panic_handler();

CompileCommand compile_command;

std::unordered_map<InternedString, Scope *> import_map;
std::unordered_set<InternedString> include_set;
/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/

int ignored_warnings = WarningNone;

static bool run_on_finished = false;

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
  bool run_on_finished = false, run_tests = false;
  compile_command = CompileCommand(std::vector<string>(argv, argc + argv), runtime_args, run_on_finished, run_tests);

  auto result = compile_command.compile();

  if (run_on_finished && result == 0) {
    std::string invocation = "./" + compile_command.binary_path.string();
    std::string args;
    for (const auto &arg : runtime_args) {
      args += arg + " ";
    }
    std::string command = invocation + " " + args;
    std::cout << "running: " << command << std::endl;
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