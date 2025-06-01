#include "core.hpp"
#include "error.hpp"
#include "strings.hpp"
#include "thir.hpp"
#include "thir_emitter.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <cstdlib>
#include <filesystem>

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
    Typer type_visitor{context};
    THIRVisitor thir_visitor(context);
    THIREmitter thir_emitter;
    type_visitor.visit(root);
    auto thir = thir_visitor.visit_program(root);
    thir_emitter.emit_program(thir);

    std::filesystem::current_path(compile_command.original_path);
    std::ofstream output(compile_command.output_path);
    output << thir_emitter.code.str();

    // Emitter emit(context, type_visitor);
    // Resolver dep_resolver(context, &emit);
    // emit.dep_emitter = &dep_resolver;

    // static const auto testing = compile_command.has_flag("test");
    // const bool is_freestanding =
    //     compile_command.c_flags.contains("-ffreestanding") || compile_command.c_flags.contains("-nostdlib");

    // if (!is_freestanding) {
    //   emit.code << "#define USE_STD_LIB 1\n";
    // } else {
    //   if (compile_command.has_flag("test")) {
    //     throw_error(
    //         "You cannot use unit tests in a freestanding or nostlib "
    //         "environment due to lack of exception handling",
    //         {});
    //   }
    // }

    // if (compile_command.has_flag("test-verbose")) {
    //   emit.code << "#define TEST_VERBOSE;\n";
    //   std ::cout << "adding TEST_VERBOSE\n";
    // }

    // if (testing) {
    //   emit.code << "#define TESTING\n";
    // }

    // root->accept(&dep_resolver);
    // root->accept(&emit);

    // std::filesystem::current_path(compile_command.original_path);
    // std::ofstream output(compile_command.output_path);

    // std::string program;
    // if (compile_command.has_flag("test")) {
    //   output << "#define TESTING\n";
    // }

    // output << BOILERPLATE_C_CODE << '\n';

    // const bool uses_reflection = emit.reflected_upon_types.size();

    // if (uses_reflection) {
    //   output << "typedef struct Type Type;\n";
    //   output << emit.reflection_externs.str();
    // }

    // output << emit.code.str();

    // if (uses_reflection) {
    //   output << emit.reflection_initialization.str();
    // }

    // output.flush();
    // output.close();
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
        init_string = MAIN_INIT_CODE;
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

  {
    if (has_flag("Wignore-all")) {
      ignored_warnings |= WarningIgnoreAll;
    }
    if (has_flag("Wno-arrow-operator")) {
      ignored_warnings |= WarningUseDotNotArrowOperatorOverload;
    }
    if (has_flag("Wno-inaccessible-decl")) {
      ignored_warnings |= WarningInaccessibleDeclaration;
    }
    if (has_flag("Wno-switch-break")) {
      ignored_warnings |= WarningSwitchBreak;
    }
  }
}