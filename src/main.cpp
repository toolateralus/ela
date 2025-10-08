#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <ostream>
#include "core.hpp"
#include "error.hpp"

using std::string;
using std::vector;


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
  compile_command = CompileCommand(std::vector<string>(argv, argc + argv), runtime_args, run_on_finished, run_tests, lldb);

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
