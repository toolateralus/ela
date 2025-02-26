#include "core.hpp"
#include "strings.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <filesystem>

bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}

void emit(ASTNode *root, Context& context, Typer &type_visitor) {
  Emitter emit(context, type_visitor);
  DependencyEmitter dependencyEmitter(context, &emit);

  static const auto testing = compile_command.has_flag("test");
  const bool is_freestanding = compile_command.compilation_flags.contains("-ffreestanding") ||
                               compile_command.compilation_flags.contains("-nostdlib");

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
  
  emit.code << INESCAPABLE_BOILERPLATE_AAAGHHH << '\n';

  if (!is_freestanding) {
    emit.code << "typedef struct Type Type;\n";
    auto type_ptr_id = context.scope->find_type_id("Type", {{{TYPE_EXT_POINTER}}});
    emit.code << std::format("typedef struct List${} List${};\nextern List${} _type_info;\n", type_ptr_id, type_ptr_id,
                        type_ptr_id);
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
  Lexer lexer{};
  Context context{};
  original_path = std::filesystem::current_path();
  parse.begin();
  Parser parser(input_path.string(), context);
  ASTProgram *root = parser.parse();
  parse.end("parsing done.");

  lower.begin();
  Typer type_visitor{context};
  type_visitor.visit(root);

  emit(root, context, type_visitor);
  
  lower.end("lowering to cpp complete");

  int result = 0;

  if (!has_flag("no-compile")) {
    std::string extra_flags = compilation_flags;

    if (has_flag("release"))
      extra_flags += " -O3 ";
    else
      extra_flags += " -g ";

    static std::string ignored_warnings = "-w";

    std::string output_flag = (compilation_flags.find("-o") != std::string::npos) ? "" : "-o " + binary_path.string();

    auto compilation_string = std::format("clang -std=c23 {} {} {} {}", ignored_warnings,
                                          output_path.string(), output_flag, extra_flags);

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

CompileCommand::CompileCommand(int argc, char *argv[]) {
  if (argc < 2) {
    printf("\033[31mUsage: <input.ela> (optional)::[-o "
           "<output.cpp>] [--flag]\033[0m\n");
  }

  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "-o" && i + 1 < argc) {
      output_path = argv[++i];
    } else if (arg.rfind("--", 0) == 0) {
      flags[arg.substr(2)] = true;
    } else {
      input_path = arg;
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
}

std::string CompileCommand::read_input_file() {
  std::ifstream stream(std::filesystem::canonical(input_path));
  std::stringstream ss;
  ss << stream.rdbuf();
  if (ss.str().empty()) {
    printf("%s\n", std::format("\033[31mError: {} is empty.\033[0m", input_path.string()).c_str());
    exit(1);
  }
  return ss.str();
}

void CompileCommand::add_compilation_flag(const std::string &flags) {
  this->compilation_flags += flags;
  if (!this->compilation_flags.ends_with(' ')) {
    this->compilation_flags += ' ';
  }
}

void CompileCommand::print() const {
  std::cout << "\033[1;32mInput Path:\033[0m " << input_path << std::endl;
  std::cout << "\033[1;32mOutput Path:\033[0m " << output_path << std::endl;
  std::cout << "\033[1;32mBinary Path:\033[0m " << binary_path << std::endl;
  std::cout << "\033[1;32mFlags:\033[0m" << std::endl;
  for (const auto &flag : flags) {
    std::cout << "  \033[1;34m--" << flag.first << "\033[0m: " << (flag.second ? "true" : "false") << std::endl;
  }
}
