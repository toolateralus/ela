#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

#include "visitor.hpp"
#include <jstl/containers/vector.hpp>

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

CompileCommand compile_command;

/*
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/


int main(int argc, char *argv[]) {
  try {
    compile_command = CompileCommand(argc, argv);
    compile_command.print();
    init_type_system();
    compile_command.compile();
  } catch (const std::exception &e) {
    fprintf(stderr, "\e[31m%s\e[0m\n", e.what());
    return 1;
  }
  return 0;
}

ASTProgram *CompileCommand::process_ast(Context &context) {
  auto input = read_input_file();
  original_path = std::filesystem::current_path();
  std::filesystem::current_path(input_path.parent_path());
  Parser parser(input, input_path, context);
  ASTProgram *root = parser.parse();
  return root;
}
void CompileCommand::emit_code(ASTProgram *root, Context &context) {
  TypeVisitor type_visitor{context};
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

  auto header = emit.get_header();
  std::filesystem::path header_output_path = output_path;
  header_output_path.replace_extension(".hpp");
  std::ofstream header_file(header_output_path);
  header_file << header;
  header_file.flush();
  header_file.close();

  auto program =
      std::format("#include \"{}\"\n", header_output_path.filename().string()) +
      emit.get_code();
      
  if (compile_command.has_flag("test")) {
    program = "#define TESTING\n" + program;
  }

  std::ofstream output(output_path);
  output << program;
  output.flush();
  output.close();


  std::string extra_flags = "-lc";

  printf("\e[31m");
  system(std::format("clang++ -std=c++23 {} {} -o {}", extra_flags, output_path.string(),
                     binary_path.string())
             .c_str());
  printf("\e[0m");


  if (!has_flag("s")) {
    std::filesystem::remove(output_path);
    std::filesystem::remove(header_output_path);
  }

  std::filesystem::current_path(original_path);
}
bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}
void CompileCommand::compile() {
  Lexer lexer;
  Context context;
  ASTProgram *root = process_ast(context);
  emit_code(root, context);
}

bool get_compilation_flag(const std::string &flag) {
  return compile_command.has_flag(flag);
}
