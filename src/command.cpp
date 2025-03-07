#include "core.hpp"
#include "strings.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <filesystem>

bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}

void emit(ASTNode *root, Context &context, Typer &type_visitor, int type_list) {
  Emitter emit(context, type_visitor);
  DependencyEmitter dependencyEmitter(context, &emit);

  static const auto testing = compile_command.has_flag("test");
  const bool is_freestanding = compile_command.c_flags.contains("-ffreestanding") ||
                               compile_command.c_flags.contains("-nostdlib");

  if (!is_freestanding) {
    emit.code << "#define USE_STD_LIB 1\n";
  } else {
    if (compile_command.has_flag("test")) {
      throw_error("You cannot use unit tests in a freestanding or nostlib "
                  "environment due to lack of exception handling",
                  {});
    }
  }

  if (compile_command.has_flag("test-verbose")) {
    emit.code << "#define TEST_VERBOSE;\n";
    std ::cout << "adding TEST_VERBOSE\n";
  }

  if (!compile_command.has_flag("release")) {
    emit.code << "#line 0 \"boilerplate.hpp\"";
  }

  emit.code << BOILERPLATE_C_CODE << '\n';

  if (!is_freestanding) {
    dependencyEmitter.declare_type(type_list);
    dependencyEmitter.define_type(type_list);
    emit.code << "typedef struct Type Type;\n";
    emit.code << std::format("extern {} _type_info;\n", emit.to_cpp_string(global_get_type(type_list)));
  }

  if (testing) {
    emit.code << "#define TESTING\n";
  }

  root->accept(&dependencyEmitter);
  root->accept(&emit);

  std::filesystem::current_path(compile_command.original_path);
  std::ofstream output(compile_command.output_path);

  std::string program;
  if (compile_command.has_flag("test")) {
    output << "#define TESTING\n";
  }

  output << emit.code.str();
  output.flush();
  output.close();
}

int CompileCommand::compile() {
  init_type_system();
  Lexer lexer{};
  Context context{};
  original_path = std::filesystem::current_path();
  parse.begin();
  Parser parser(input_path.string(), context);
  ASTProgram *root = parser.parse_program();
  parse.end("parsing done.");

  lower.begin();
  auto type_ptr_id = context.scope->find_type_id("Type", {{{TYPE_EXT_POINTER_CONST}}});
  Typer type_visitor{context};
  auto type_list = type_visitor.find_generic_type_of("List", {type_ptr_id}, {});
  type_visitor.visit(root);

  emit(root, context, type_visitor, type_list);

  lower.end("lowering to cpp complete");

  int result = 0;

  if (!has_flag("no-compile")) {
    std::string extra_flags = c_flags;

    if (has_flag("release"))
      extra_flags += " -O3 ";
    else
      extra_flags += " -g ";

    static std::string ignored_warnings = "-w";

    std::string output_flag = (c_flags.find("-o") != std::string::npos) ? "" : "-o " + binary_path.string();

    auto compilation_string =
        std::format("clang -std=c23 {} {} {} {}", ignored_warnings, output_path.string(), output_flag, extra_flags);

    if (compile_command.has_flag("x"))
      printf("\033[1;36m%s\n\033[0m", compilation_string.c_str());

    cpp.begin();
    result = system(compilation_string.c_str());
    cpp.end("invoking `clang` C compiler and `lld` linker");
    if (!has_flag("s")) {
      std::filesystem::remove(output_path);
    }
  }

  std::filesystem::current_path(original_path);
  print_metrics();
  return result;
}

void CompileCommand::setup_ignored_warnings() {
  if (has_flag("--Wignore-all")) {
    ignored_warnings |= WarningIgnoreAll;
  }
  if (has_flag("--Wno-arrow-operator")) {
    ignored_warnings |= WarningUseDotNotArrowOperatorOverload;
  }
  if (has_flag("--Wno-inaccessible-decl")) {
    ignored_warnings |= WarningInaccessibleDeclaration;
  }
  if (has_flag("--Wno-empty-string-interp")) {
    ignored_warnings |= WarningEmptyStringInterpolation;
  }
  if (has_flag("--Wno-non-null-deleted")) {
    ignored_warnings |= WarningNonNullDeletedPointer;
  }
  if (has_flag("--Wno-ambiguous-variant")) {
    ignored_warnings |= WarningAmbigousVariants;
  }
  if (has_flag("--Wno-switch-break")) {
    ignored_warnings |= WarningSwitchBreak;
  }
  if (has_flag("--Wno-array-param")) {
    ignored_warnings |= WarningDownCastFixedArrayParam;
  }
}

CompileCommand::CompileCommand(const std::vector<std::string> &args) {
  std::vector<std::string> runtime_args;
  bool run_on_finished;
  bool run_tests;

  for (size_t i = 0; i < args.size(); ++i) {
    std::string arg = args[i];
    if (arg == "run" || arg == "r") {
      input_path = "main.ela";
      run_on_finished = true;
    } else if (arg == "build" || arg == "b") {
      input_path = "main.ela";
    } else if (arg == "test" || arg == "t") {
      run_tests = true;
      input_path = "test.ela";
      run_on_finished = true;
    } else if (arg == "init") {
      std::ofstream file("main.ela");
      if (i + 1 < args.size() && args[i + 1] == "raylib") {
        file << RAYLIB_INIT_CODE;
      } else {
        file << MAIN_INIT_CODE;
      }
      exit(0);
    } else if (arg == "-o" && i + 1 < args.size()) {
      output_path = args[++i];
    } else if (arg.ends_with(".ela") && input_path.empty()) {
      input_path = arg;
    } else if (arg.starts_with("--")) {
      flags[arg.substr(2)] = true;
    } else {
      runtime_args.push_back(arg);
    }
  }

  if (input_path.empty()) {
    printf("\033[31mError: No input file specified.\033[0m\n");
    exit(1);
  }

  std::filesystem::path input_fs_path(input_path);
  if (!std::filesystem::exists(input_fs_path)) {
    printf("%s\n", (std::format("\033[31mError: File '{}' does not exist.\033[0m", input_path.string())).c_str());
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

  setup_ignored_warnings();
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
