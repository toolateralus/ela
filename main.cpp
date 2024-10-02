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

std::unordered_map<std::string, int> global_type_aliases;

// the same for this
jstl::Arena ast_arena{MB(10)};

CompileCommand compile_command;

std::vector<ASTAllocate*> allocation_info;

std::vector<Token> all_tokens;

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

  std::string extra_flags = "-lc " + compilation_flags;

  auto compilation_string = std::format("clang++ -std=c++23 -Wno-writable-strings -Wno-parentheses-equality -Wno-c99-designator {} {} -o {}", extra_flags,
                     output_path.string(), binary_path.string());
                     
  printf("\e[1;36m%s\n\e[0m", compilation_string.c_str());
  cpp.begin();
  auto result = system(compilation_string.c_str());
  printf("compiler return %d\n", result);
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

Context::Context() {
  // define some default functions that may or may not be macros.
  {
    // CLEANUP(Josh) Fix this? if there is a way. Perhaps we want C style macros, as unsafe and annoying to debug they are. 9/30/2024, 9:30:29 AM
    // We still define assert and sizeof manually here, because
    // there's no way to do this in language currently, as they're macros
    FunctionTypeInfo assert_info{};
    assert_info.return_type = void_type();
    assert_info.parameter_types[0] = charptr_type();
    assert_info.parameter_types[1] = bool_type();
    assert_info.params_len = 2;
    current_scope->insert("assert", global_find_type_id("void(char *, bool)", assert_info, {}));

    FunctionTypeInfo sizeof_info{};
    sizeof_info.return_type = global_find_type_id("s64", {});
    sizeof_info.is_varargs = true;
    // no other function will ever use this type. thats why we have a ?, because we have no first class types yet.
    current_scope->insert("sizeof", global_find_type_id("s64(?)", sizeof_info, {}));
    root_scope = current_scope;  
  }
  
  // define types used for reflection, which are currently half implemented due to ineffectiveness.
  {
    auto type_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto field_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type_id = global_create_struct_type("Type", type_scope);
    auto field_id = global_create_struct_type("Field", field_scope);
    type_scope->insert("id", s32_type());
    type_scope->insert("name", charptr_type());
    type_scope->insert("fields", global_find_type_id("Field", {.extensions = {TYPE_EXT_POINTER}}));
    field_scope->insert("name", charptr_type());
    field_scope->insert("type", global_find_type_id("Type", {.extensions = {TYPE_EXT_POINTER}}));
  }
  
  // string struct, stores length info.
  // same as array.
  {
    auto str_scope = new (scope_arena.allocate(sizeof(Scope))) Scope();
    auto type_id = global_create_struct_type("string", str_scope);
    auto type = global_get_type(type_id);
    
    static_cast<StructTypeInfo*>(type->info)->implicit_cast_table = {
      charptr_type(),
    };
    
    str_scope->insert("data", charptr_type());
    str_scope->insert("length", s32_type());
  }
  
  // add the base types for all of the primitives.  
  for (int i = 0; i < num_types; ++i) {
    root_scope->types.insert(i);
  }
}

std::string Scope::get_function_typename(ASTFunctionDeclaration *decl) {
  std::stringstream ss;
  auto return_type = decl->return_type;
  ss << get_type(return_type->resolved_type)->to_string();
  ss << "(";
  for (const auto &param : decl->params->params) {
    ss << get_type(param->type->resolved_type)->to_string();
    if (param != decl->params->params.back()) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}
