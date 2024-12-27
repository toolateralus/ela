#include "ast.hpp"
#include "core.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

#include "visitor.hpp"
#include <cstdio>
#include <cstdlib>
#include <filesystem>

#include <ostream>
#include <unordered_map>

/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/

using std::string;
using std::vector;

Type **type_table = new Type *[MAX_NUM_TYPES];
int num_types;

// this is just an approximation.
// It may be too little since this arena is used a lot.
jstl::Arena type_arena{(sizeof(Type) * MAX_NUM_TYPES)};

// the same for this
jstl::Arena scope_arena{MB(100)};

std::unordered_map<InternedString, int> type_alias_map;

// the same for this
jstl::Arena ast_arena{MB(100)};

// TODO: remove me, we want file scopes.
Scope * root_scope;

CompileCommand compile_command;

std::vector<Allocation> allocation_info;

std::vector<Token> all_tokens;

std::unordered_set<InternedString> import_set;
/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/

static bool run_on_finished = false;

int main(int argc, char *argv[]) {

  for (int i = 0; i < argc; ++i) {
      if (strcmp(argv[i], "--h") == 0 || strcmp(argv[i], "--help") == 0) {
        printf(R"_(
Ela compiler:
   compile a file: `ela <filename.ela>`
   compile & run a 'main.ela' in current directory: `ela run`
   initialize a 'main.ela' file in current directory: `ela init`

   Available flags:
   --release     Compile with -O3 flag and with no debug information from Ela. defaults to false, which is a debug build.
   --verbose     Write a file and dump the AST representation of your program to 'stdout'.
   --no-compile  Transpile to C++ but don't invoke the clang++ compiler automatically.
   --s           Don't delete the `.hpp` and `.cpp` files used to transpile.
   --metrics     Write performance metrics to stdout.
   --test        Only emit functions marked `#test` and create a test runner. You still have to run the binary to run the tests.
)_");
    return 0;
      }
  }

  vector<string> original_args(argv + (argc >= 2 ? 2 : 1), argv + argc);


  if (argc >= 2 && (strcmp(argv[1], "run") == 0 || strcmp(argv[1], "r") == 0)) {
    argv[1] = (char*)"main.ela";
    argc = 2;
    run_on_finished = true;
  }

  if (argc == 2 && (strcmp(argv[1], "init") == 0)) {
    std::ofstream file("main.ela");
    file << "main :: (argc: int, argv: c_string*) {\n\n}\n";
    file.flush();
    file.close();
    return 0;
  }

  compile_command = CompileCommand(argc, argv);
  if (compile_command.has_flag("x")) compile_command.print();

  init_type_system();
  auto result = compile_command.compile();


  if (run_on_finished) {
    if (result == 0) {
      string invocation = ("./" + compile_command.binary_path.string());
      string args = "";
      for (const auto &arg: original_args) {
        args += arg + " ";
      }
      auto command = invocation + " " + args;
      std::cout << "Running: " << command << std::endl;
      system(command.c_str());
    }
  }


  if (compile_command.has_flag("sanitize")) {
    if (report_unfreed_allocations()) {
      return 1;
    }
  }
  return result != 0;
}

bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}

int CompileCommand::compile() {
  Lexer lexer;
  Context context;
  auto input = read_input_file();
  original_path = std::filesystem::current_path();
  parse.begin();
  Parser parser(input, input_path, context);
  ASTProgram *root = parser.parse();
  parse.end(std::format("Parsed {} tokens", all_tokens.size()));

  lower.begin();
  Typer type_visitor{context};
  type_visitor.visit(root);

  if (has_flag("verbose")) {
    SerializeVisitor serializer(context);
    auto serialized_view = std::any_cast<std::string>(serializer.visit(root));
    printf("%s\n", serialized_view.c_str());
    std::ofstream ast(binary_path.string() + ".coffee");
    ast << serialized_view;
    ast.flush();
    ast.close();
  }

  EmitVisitor emit(context, type_visitor);
  emit.visit(root);

  lower.end("lowering to cpp complete");

  std::string program;
  if (compile_command.has_flag("test")) {
    program = "#define TESTING\n" + emit.code.str();
  } else {
    program = emit.code.str();
  }

  std::ofstream output(output_path);
  output << program;
  output.flush();
  output.close();
  int result = 0;

  if (!has_flag("no-compile")) {
    std::string extra_flags = compilation_flags;

    // Use '-ftime-trace' for debugging C++ compilation times;
    // You need google chrome to analyse the JSON results.
    
    if (has_flag("release"))  extra_flags += " -O3 ";
    else extra_flags += " -g " ;

    static std::string ignored_warnings = "-w";

    std::string output_flag = (compilation_flags.find("-o") != std::string::npos) ? "" : "-o " + binary_path.string();

    auto compilation_string = std::format("clang++ -std=c++23 {} -L/usr/local/lib {} {} {}", ignored_warnings, output_path.string(), output_flag, extra_flags);

    if (compile_command.has_flag("x"))
      printf("\e[1;36m%s\n\e[0m", compilation_string.c_str());

    cpp.begin();
    result = system(compilation_string.c_str());
    cpp.end("compiling and linking cpp");
    if (!has_flag("s")) {
      std::filesystem::remove(output_path);
    }
  }

  std::filesystem::current_path(original_path);
  print_metrics();
  return result;
}
