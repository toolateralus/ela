#include "core.hpp"
#include "error.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <cstdlib>
#include <filesystem>
#include "emit.hpp"
#include "resolver.hpp"

bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}

int CompileCommand::compile() {
  init_type_system();
  Context context{};
  original_path = std::filesystem::current_path();

  auto root = parse.run<ASTProgram *>("parser", [&]() -> ASTProgram * {
    Parser parser(input_path.string(), context);
    ASTProgram *root = parser.parse_program();
    return root;
  });

  lower.run<void>("typing & lowering to C", [&] {
    Typer typer{context};
    THIRGen thir_gen(context);
    Emitter emitter;
    Resolver resolver(emitter);
    typer.visit(root);
    
    auto thir = thir_gen.visit_program(root);
    resolver.visit_node(thir);



    std::filesystem::current_path(compile_command.original_path);
    std::ofstream output(compile_command.output_path);

    std::string program;
    if (compile_command.has_flag("test")) {
      output << "#define TESTING\n";
    }

    output << BOILERPLATE_C_CODE << '\n';
    output << emitter.code.str();

    emitter.code.clear();
    // Emit our main last always
    THIRFunction *ep = thir_gen.emit_runtime_entry_point();
    emitter.emit_function(ep);
    output << emitter.code.str();
  });

  if (has_flag("no-compile")) {
    return 0;
  }

  std::string extra_flags = c_flags;
  if (has_flag("release")) {
    extra_flags += " -O3 ";
  } else {
    extra_flags += " -g ";
  }

  const static std::string ignored_warnings = "-w";

  const std::string output_flag = (c_flags.find("-o") != std::string::npos) ? "" : "-o " + binary_path.string();

  const auto compilation_string =
      std::format("clang -std=c23 {} {} {} {}", ignored_warnings, output_path.string(), output_flag, extra_flags);

  if (compile_command.has_flag("x")) {
    printf("\033[1;36m%s\n\033[0m", compilation_string.c_str());
  }

  int result = cpp.run<int>("invoking 'clang' compiler on transpiled C code",
                            [&compilation_string] { return system(compilation_string.c_str()); });

  if (!has_flag("s")) {
    std::filesystem::remove(output_path);
  }

  std::filesystem::current_path(original_path);

  if (has_flag("metrics")) {
    print_metrics();
  }

  return result;
}

void CompileCommand::add_c_flag(const std::string &flags) {
  this->c_flags += flags;
  if (!this->c_flags.ends_with(' ')) {
    this->c_flags += ' ';
  }
}

void CompileCommand::print_command() const {
  std::cout << "\033[1;32mInput Path:\033[0m " << input_path << std::endl;
  std::cout << "\033[1;32mOutput Path:\033[0m " << output_path << std::endl;
  std::cout << "\033[1;32mBinary Path:\033[0m " << binary_path << std::endl;
  std::cout << "\033[1;32mFlags:\033[0m" << std::endl;
  for (const auto &flag : flags) {
    std::cout << "  \033[1;34m--" << flag.first << "\033[0m: " << (flag.second ? "true" : "false") << std::endl;
  }
}

CompileCommand::CompileCommand(const std::vector<std::string> &args, std::vector<std::string> &runtime_args,
                               bool &run_on_finished, bool &run_tests, bool &lldb) {
  auto default_input_path = "main.ela";
  std::string init_string = "";
  for (size_t i = 1; i < args.size(); ++i) {
    std::string arg = args[i];

    if (arg == "help" || arg == "h") {
      printf("\033[1;0;1m%s\033[0m\n", HELP_STRING);
      exit(0);
    } else if (arg == "run" || arg == "r") {
      run_on_finished = true;
    } else if (arg == "build" || arg == "b") {
      default_input_path = "main.ela";
    } else if (arg == "test" || arg == "t") {
      run_tests = true;
      default_input_path = "test.ela";
      run_on_finished = true;
    } else if (arg == "init") {
      if (i + 1 < args.size() && args[i + 1] == "raylib") {
        init_string = RAYLIB_INIT_CODE;
      } else {
        init_string = HELLO_WORLD_INIT_CODE;
      }
    } else if (arg == "lldb") {
      run_on_finished = false;
      lldb = true;
    } else if (arg == "-o" && i + 1 < args.size()) {
      output_path = args[++i];
    } else if (arg.ends_with(".ela") &&
               input_path.empty()) {  // Sometimes this is annoying if you're just passing args to a thing. like ela r
                                      // main.ela where main.ela is the arg not the file. we should use a rust like --
                                      // seperator to seperate runtime args from ela compiler args.
      input_path = arg;
    } else if (arg.starts_with("--")) {
      flags[arg.substr(2)] = true;
    } else {
      runtime_args.push_back(arg);
    }
  }

  if (input_path.empty()) {
    input_path = default_input_path;
  }

  if (!init_string.empty()) {
    std::ofstream file(input_path);
    file << init_string;
    file.flush();
    file.close();
    exit(0);
  }

  std::filesystem::path input_fs_path(input_path);
  if (!std::filesystem::exists(input_fs_path)) {
    printf("%s\n", (std::format("\033[31mError: File '{}' does not exist. If you don't provide a input path, it will "
                                "default to an appropriate default, often `./main.ela`\033[0m",
                                input_path.string()))
                       .c_str());
    exit(1);
  }

  std::filesystem::path parent_path = input_fs_path.parent_path();
  if (!parent_path.empty() && !std::filesystem::exists(parent_path)) {
    printf("%s\n",
           std::format("\033[31mError: Parent directory '{}' does not exist.\033[0m", parent_path.string()).c_str());
    exit(1);
  }

  binary_path = input_fs_path.stem().string();

  if (output_path.empty()) {
    std::string filename = input_fs_path.filename().string();
    size_t pos = filename.rfind(".ela");
    if (pos != std::string::npos) {
      filename.replace(pos, 4, ".c");
    } else {
      filename += ".c";
    }
    output_path = filename;
  }

  if (run_tests) {
    flags["test"] = true;
  }

  if (flags["freestanding"]) {
    c_flags += " -ffreestanding -nostdlib ";
  }

  if (has_flag("x")) {
    print_command();
  }

  for (int i = 0; i < WARNING_COUNT; ++i) {
    if (WARNING_FLAG_STRINGS[i].empty()) {
      continue;
    }
    if (has_flag(WARNING_FLAG_STRINGS[i])) {
      ignored_warnings |= i;
    }
  }
}