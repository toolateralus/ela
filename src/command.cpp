#include "core.hpp"
#include "llvm.hpp"
#include "strings.hpp"
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

  if (false) {
    Emitter emit(context, type_visitor);
    DependencyEmitter dependencyEmitter(context, &emit);
    emit.dep_emitter = &dependencyEmitter;

    static const auto testing = compile_command.has_flag("test");
    const bool is_freestanding =
        compile_command.c_flags.contains("-ffreestanding") || compile_command.c_flags.contains("-nostdlib");

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

    emit.code << BOILERPLATE_C_CODE << '\n';

    if (!is_freestanding && !has_flag("nostdlib")) {
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

  LLVMEmitter emitter(context);
  emitter.visit_program(root);

  lower.end("lowering to cpp complete");
  int result = 0;

  /*
    TODO: we need to use a 'target_machine' thing to setup the target triple etc.
  */
  {
    // Finalize the LLVM IR and compile it to an executable
    std::string output_filename =
        std::filesystem::path(get_source_filename(root->source_range)).filename().replace_extension("");
    std::error_code ec;
    llvm::raw_fd_ostream dest(output_filename + ".ll", ec, (llvm::sys::fs::OpenFlags)0);

    if (ec) {
      std::cerr << "Could not open file: " << ec.message() << std::endl;
      return 1;
    }

    emitter.di_builder->finalize();
    emitter.module->print(dest, nullptr);
    dest.flush();

    auto llc_command = std::format("clang -lc -lm {}.ll -o {}", output_filename, output_filename);
    system(llc_command.c_str());
  }
  std::filesystem::current_path(original_path);
  print_metrics();

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
    if (arg == "run" || arg == "r") {
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
               input_path.empty()) { // Sometimes this is annoying if you're just passing args to a thing. like ela r
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
}