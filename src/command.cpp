#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Verifier.h>
#include "callconv.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interpreter.hpp"
#include "lex.hpp"
#include "llvm_emit.hpp"
#include "strings.hpp"
#include "thir.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include "ast.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <system_error>
#include "mir.hpp"

bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}

#if 0
#include "vm.hpp"
#endif

int CompileCommand::compile() {
  init_type_system();
  Context context{};
  original_path = std::filesystem::current_path();

  if (compile_command.has_flag("release")) {
    compile_command.flags["nl"] = true;
  }

  ASTProgram *program = parse_metric.run<ASTProgram *>("Parsing", [&]() -> ASTProgram * {
    Parser parser(input_path.string(), context);
    ASTProgram *root = parser.parse_program();
    return root;
  });

  typing_metric.run<void>("Typing AST", [&] {
    Typer typer{context};
    typer.visit(program);
  });

  THIR *thir_program = nullptr;
  THIRGen thir_gen(context);
  THIR *entry_point = thir_gen_metric.run<THIR *>("Generating THIR", [&] {
    thir_program = thir_gen.visit_program(program);
    return thir_gen.emit_runtime_entry_point();
  });

#if defined(__linux)
  // TODO: once we handle windows, actually use this lol.
  SysV64_C_Calling_Convention calling_conv;
#elif defined(_WIN32)
  Win64_C_Calling_Convention calling_conv;
#endif
  mir::Module m{&calling_conv};

  mir_gen_metric.run<void>("Generating MIR", [&] {
    if (entry_point) {
      mir::compile(entry_point, m, thir_gen.constructors, thir_gen.global_initializer_function,
                   thir_gen.generate_all_tests_slice_variable(), thir_gen.get_all_reflection_variables());
    } else {
      mir::compile(thir_program, m, thir_gen.constructors, thir_gen.global_initializer_function,
                   thir_gen.generate_all_tests_slice_variable(), thir_gen.get_all_reflection_variables());
    }
    m.finalize();

#if 0
    mir::VM::Context c;
    mir::VM::interpret(c, m, 0);
#endif
    if (compile_command.has_flag("save-mir")) {
      auto path = compile_command.binary_path.string() + std::string{".emir"};
      FILE *f = fopen(path.c_str(), "w");
      m.print(f);
      fflush(f);
      fclose(f);
    }
  });

  LLVM_Emitter llvm_emitter{m};
  llvm_gen_metric.run<void>("Generating LLVM IR", [&] { llvm_emitter.emit_module(); });
  llvm_opt_metric.run<void>("Running LLVM Optimization Passes.", [&] {
    std::error_code ec;
    auto path = compile_command.binary_path.string() + ".ll";
    llvm::raw_fd_ostream llvm_output_stream(path, ec);
    if (ec) {
      throw_error(ec.message(), {});
    }

    std::string s;
    llvm::raw_string_ostream err_stream{s};
    // We still write the file out for inspection, but don't optimize
    if (llvm::verifyModule(*llvm_emitter.llvm_module, &err_stream)) {
      llvm::errs() << "Module verification failed:\n";
      llvm::errs() << err_stream.str() << '\n';
    } else if (has_flag("release")) {
      printf("optimizing the LLVM IR\n");
      using namespace llvm;
      // Create the analysis managers.
      // These must be declared in this order so that they are destroyed in the
      // correct order due to inter-analysis-manager references.
      LoopAnalysisManager LAM;
      FunctionAnalysisManager FAM;
      CGSCCAnalysisManager CGAM;
      ModuleAnalysisManager MAM;

      // Create the new pass manager builder.
      // Take a look at the PassBuilder constructor parameters for more
      // customization, e.g. specifying a TargetMachine or various debugging
      // options.
      PassBuilder PB;

      // Register all the basic analyses with the managers.
      PB.registerModuleAnalyses(MAM);
      PB.registerCGSCCAnalyses(CGAM);
      PB.registerFunctionAnalyses(FAM);
      PB.registerLoopAnalyses(LAM);
      PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

      // Create the pass manager.
      // This one corresponds to a typical -O2 optimization pipeline.
      ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(opt_level);

      // Optimize the IR!
      MPM.run(*llvm_emitter.llvm_module, MAM);
    }

    llvm_emitter.llvm_module->print(llvm_output_stream, nullptr);
    llvm_output_stream.flush();
  });

  int result = 0;
  if (!has_flag("no-compile")) {
    std::string extra_flags = c_flags;
    if (!has_flag("release")) {
      extra_flags += " -g ";
    }

    const std::string output_flag = (c_flags.find("-o") != std::string::npos) ? "" : "-o " + binary_path.string();
    const std::string obj_file = output_path.filename().string() + ".o";

    std::string llc_debug_flag = "";

    const auto llc_string = std::format("llc -filetype=obj -relocation-model=pic {} -o {}", output_path.string(), obj_file);
    const auto compilation_string = std::format("clang -fPIE {} {} {}", obj_file, output_flag, extra_flags);

    result = clang_invocation_metric.run<int>("Creating and Linking Object Files", [&] {
      int result = 0;
      if ((result = system(llc_string.c_str()) != 0)) {
        return result;
      }
      return system(compilation_string.c_str());
    });

    if (!has_flag("s")) {
      std::filesystem::remove(output_path);
    }

    if (compile_command.has_flag("x")) {
      printf("\033[1;36m%s\n\033[0m", llc_string.c_str());
      printf("\033[1;36m%s\n\033[0m", compilation_string.c_str());
    }
  } else {
    clang_invocation_metric.ignore("Creating and Linking Object Files");
  }

  std::error_code ec;  // ignored
  std::filesystem::remove(output_path.string() + ".o", ec);

  std::filesystem::current_path(original_path);

  if (has_flag("metrics")) {
    printf("\033[1;37mcompiled: %zu lines of code\033[0m \033[3;90m(excluding comments)\033[0m\n",
           num_lines_code_processed_by_lexer_excluding_comments);
    printf("\033[3;90mcomments occupied: %zu lines.\033[0m\n", num_lines_comments_processed_by_lexer);
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
    } else if (arg.starts_with("-O")) {
      opt_level_arg = arg;
      if (arg == "-O0") {
        opt_level = llvm::OptimizationLevel::O0;
      } else if (arg == "-O1") {
        opt_level = llvm::OptimizationLevel::O1;
      } else if (arg == "-O2") {
        opt_level = llvm::OptimizationLevel::O2;
      } else if (arg == "-O3") {
        opt_level = llvm::OptimizationLevel::O3;
      } else if (arg == "-Os") {
        opt_level = llvm::OptimizationLevel::Os;
      } else if (arg == "-Oz") {
        opt_level = llvm::OptimizationLevel::Oz;
      }

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
    printf("%s\n", std::format("\033[31mError: Parent directory '{}' does not exist.\033[0m", parent_path.string()).c_str());
    exit(1);
  }

  binary_path = input_fs_path.stem().string();

  if (output_path.empty()) {
    std::string filename = input_fs_path.filename().string();
    size_t pos = filename.rfind(".ela");
    if (pos != std::string::npos) {
      filename.replace(pos, 4, ".ll");
    } else {
      filename += ".ll";
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
      num_ignored_warnings |= i;
    }
  }
}
void CompileCommand::request_compile_time_code_execution(const Span &range) {
  if (has_flag("ctfe-validate")) {
    std::cout << "\033[1;33mrequesting ctfe at: " << range.to_string() << "\033[0m\nproceed? [Y(es)/N(o)/S(how source)]: ";

    char response;
    std::cin >> response;

    if (response == 'S' || response == 's') {
      std::cout << format_source_location(range, ERROR_INFO);
      return request_compile_time_code_execution(range);
    }

    if (response != 'y' && response != 'Y') {
      throw_error("compile time function execution denied.", range);
      std::exit(1);
    }
  }
}
