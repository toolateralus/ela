#include "ast.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"

#include "visitor.hpp"
#include <cstdlib>
#include <jstl/containers/vector.hpp>
#include <unordered_map>

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

std::unordered_map<std::string, int> global_type_alias_map;

// the same for this
jstl::Arena ast_arena{MB(10)};

// TODO: remove me, we want file scopes.
Scope * root_scope;

CompileCommand compile_command;

std::vector<Allocation> allocation_info;

std::vector<Token> all_tokens;

std::unordered_set<std::string> import_set;
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
    auto result = compile_command.compile();
    
    
    if (compile_command.has_flag("sanitize")) {
      if (report_unfreed_allocations()) {
        return 1;
      }
    }
    return result != 0;
  } catch (const std::exception &e) {
    fprintf(stderr, "\e[31m%s\e[0m\n", e.what());
    return 1;
  }
  return 0;
}

ASTProgram *CompileCommand::process_ast(Context &context) {
  auto input = read_input_file();
  original_path = std::filesystem::current_path();
  std::filesystem::current_path(
      std::filesystem::canonical(input_path).parent_path());
      
  parse.begin();
  Parser parser(input, input_path, context);
  ASTProgram *root = parser.parse();
  parse.end(std::format("Parsed {} tokens", all_tokens.size()));
  return root;
}
int CompileCommand::emit_code(ASTProgram *root, Context &context) {
  
  lower.begin();
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
  lower.end("lowering to cpp complete");
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

  std::string extra_flags = "" + compilation_flags;

  auto compilation_string = std::format("clang++ -std=c++23 -L/usr/local/lib -Wno-writable-strings -Wno-parentheses-equality -Wno-c99-designator {} -o {} {}", output_path.string(), binary_path.string(), extra_flags);
                     
  printf("\e[1;36m%s\n\e[0m", compilation_string.c_str());
  cpp.begin();
  auto result = system(compilation_string.c_str());
  printf("compiler returned %d\n", result);
  cpp.end("compiling and linking cpp");
  if (!has_flag("s")) {
    std::filesystem::remove(output_path);
    std::filesystem::remove(header_output_path);
  }
  std::filesystem::current_path(original_path);
  return result;
}
bool CompileCommand::has_flag(const std::string &flag) const {
  auto it = flags.find(flag);
  return it != flags.end() && it->second;
}
int CompileCommand::compile() {
  Lexer lexer;
  Context context;
  ASTProgram *root = process_ast(context);
  auto result = emit_code(root, context);
  print_metrics();
  return result;
}

bool get_compilation_flag(const std::string &flag) {
  return compile_command.has_flag(flag);
}
