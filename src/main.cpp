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
#include "strings.hpp"

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

std::unordered_map<InternedString, Scope*> import_map;
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
  for (int i = 0; i < argc; ++i) {
    if (strcmp(argv[i], "--h") == 0 || strcmp(argv[i], "--help") == 0) {
      printf(HELP_STRING);
      return 0;
    }
  }

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

  vector<string> original_args(argv + (argc >= 2 ? 2 : 1), argv + argc);

  if (argc >= 2 && (strcmp(argv[1], "run") == 0 || strcmp(argv[1], "r") == 0)) {
    argv[1] = (char *)"main.ela";
    argc = 2;
    run_on_finished = true;
  }

  if (argc >= 2 && (strcmp(argv[1], "build") == 0 || strcmp(argv[1], "b") == 0)) {
    argv[1] = (char *)"main.ela";
    argc = 2;
  }

  bool run_tests = false;
  if (argc >= 2 && (strcmp(argv[1], "test") == 0 || strcmp(argv[1], "t") == 0)) {
    run_tests = true;
    argv[1] = (char *)"main.ela";
    argc = 2;
    run_on_finished = true;
  }

  if (argc >= 2 && (strcmp(argv[1], "init") == 0)) {
    std::ofstream file("main.ela");
    if (argc > 2 && (strcmp(argv[2], "raylib") == 0)) {
      file << RAYLIB_INIT_CODE;
    } else {
      file << MAIN_INIT_CODE;
    }
    return 0;
  }

  compile_command = CompileCommand(argc, argv);

  if (run_tests) {
    compile_command.flags["test"] = true;
  }

  if (compile_command.has_flag("freestanding")) {
    compile_command.compilation_flags += " -ffreestanding -nostdlib ";
  }

  if (compile_command.has_flag("x"))
    compile_command.print();

  compile_command.setup_ignored_warnings();

  init_type_system();
  auto result = compile_command.compile();

  if (run_on_finished) {
    if (result == 0) {
      string invocation = ("./" + compile_command.binary_path.string());
      string args = "";
      for (const auto &arg : original_args) {
        args += arg + " ";
      }
      auto command = invocation + " " + args;
      std::cout << "Running: " << command << std::endl;
      system(command.c_str());
    }
  }

  return result != 0;
}
