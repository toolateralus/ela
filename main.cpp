#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <fstream>
#include <iostream>
#include <jstl/containers/vector.hpp>
#include <sstream>

/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/
Type *type_table[MAX_NUM_TYPES];
int num_types;

// this is just an approximation.
// It may be too little since this arena is used a lot.
jstl::Arena type_arena{(sizeof(Type) * MAX_NUM_TYPES)};
// the same for this
jstl::Arena scope_arena{MB(10)};
// the same for this
jstl::Arena ast_arena{MB(10)};
/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/
#include <filesystem>

struct CompileCommand {
  std::filesystem::path input_path;
  std::filesystem::path output_path;
  std::filesystem::path binary_path;
  std::filesystem::path original_path; // where the compiler was invoked from
  std::unordered_map<std::string, bool> flags;

  void print() const {
    std::cout << "\e[1;32mInput Path:\e[0m " << input_path << std::endl;
    std::cout << "\e[1;32mOutput Path:\e[0m " << output_path << std::endl;
    std::cout << "\e[1;32mBinary Path:\e[0m " << binary_path << std::endl;
    std::cout << "\e[1;32mFlags:\e[0m" << std::endl;
    for (const auto &flag : flags) {
      std::cout << "  \e[1;34m--" << flag.first
                << "\e[0m: " << (flag.second ? "true" : "false") << std::endl;
    }
  }

  CompileCommand(int argc, char *argv[]) {
    if (argc < 2) {
      throw std::invalid_argument("\e[31mUsage: <input.ela> (optional)::[-o "
                                  "<output.cpp>] [--flag]\e[0m");
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
      throw std::invalid_argument(
          "\e[31mError: No input file specified.\e[0m");
    }

    std::filesystem::path input_fs_path(input_path);
    if (!std::filesystem::exists(input_fs_path)) {
      throw std::runtime_error(std::format(
          "\e[31mError: File '{}' does not exist.\e[0m", input_path.string()));
    }

    std::filesystem::path parent_path = input_fs_path.parent_path();
    if (!parent_path.empty() && !std::filesystem::exists(parent_path)) {
      throw std::runtime_error(std::format(
          "\e[31mError: Parent directory '{}' does not exist.\e[0m",
          parent_path.string()));
    }

    binary_path = input_fs_path.stem().string();

    if (output_path.empty()) {
      std::string filename = input_fs_path.filename().string();
      size_t pos = filename.rfind(".ela");
      if (pos != std::string::npos) {
        filename.replace(pos, 4, ".cpp");
      } else {
        filename += ".cpp";
      }
      output_path = filename;
    }
  }
  std::string read_input_file() {
    std::ifstream stream(input_path);
    std::stringstream ss;
    ss << stream.rdbuf();
    
    if (ss.str().empty()) {
      throw std::runtime_error(std::format("\e[31mError: {} is empty.\e[0m", input_path.string()));
    }
    
    return ss.str();
  }

  ASTProgram *process_ast(Context &context) {
    auto input = read_input_file();
    original_path = std::filesystem::current_path();
    std::filesystem::current_path(input_path.parent_path());
    Parser parser(input, input_path, context);
    ASTProgram *root = parser.parse();
    TypeVisitor typer{context};
    typer.visit(root);
    return root;
  }

  void emit_code(ASTProgram *root, Context &context) {
    SerializeVisitor serializer(context);
    auto serialized_view = std::any_cast<std::string>(serializer.visit(root));

    if (!has_flag("silent")) {
      printf("%s\n", serialized_view.c_str());
    }

    std::ofstream ast("ast.toml");
    ast << serialized_view;
    ast.flush();
    ast.close();

    EmitVisitor emit(context);
    emit.visit(root);

    auto header = emit.get_header();
    std::filesystem::path header_output_path = output_path;
    header_output_path.replace_extension(".hpp");
    std::ofstream header_file(header_output_path);
    header_file << header;
    header_file.flush();
    header_file.close();

    auto program = std::format("#include \"{}\"\n", header_output_path.filename().string()) + emit.get_code();
    std::ofstream output(output_path);
    output << program;
    output.flush();
    output.close();

    printf("\e[31m");
    system(std::format("clang++ -std=c++23 {} -o {}", output_path.string(),
                       binary_path.string())
               .c_str());
    printf("\e[0m");

    std::filesystem::current_path(original_path);
  }

  inline bool has_flag(const std::string &flag) const {
    auto it = flags.find(flag);
    return it != flags.end() && it->second;
  }

  inline void compile() {
    Lexer lexer;
    Context context;
    ASTProgram *root = process_ast(context);
    emit_code(root, context);
  }
};

int main(int argc, char *argv[]) {
  try {
    CompileCommand cmd(argc, argv);
    cmd.print();
    init_type_system();

    cmd.compile();

  } catch (const std::exception &e) {
    fprintf(stderr, "\e[31m%s\e[0m\n", e.what());
    return 1;
  }

  return 0;
}