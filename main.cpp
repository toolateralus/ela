#include "ast.hpp"
#include "visitor.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include <fstream>
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


int main(int argc, char *argv[]) {
  // initialize the basic scalar types etc.
  init_type_system();
  Lexer lexer;

  std::ifstream stream(argc > 1 ? argv[1] : "dummy.ela");
  std::stringstream ss;
  ss << stream.rdbuf();
  auto str = ss.str();

  Context context;
  Parser parser(str, "dummy.ela", context);
  ASTProgram *root = parser.parse();

  printf("%p\n", root);
  
  TypeVisitor typer {context};
  typer.visit(root);
  
  SerializeVisitor serializer(context);
  auto serialized_view = std::any_cast<std::string>(serializer.visit(root));
  printf("%s\n", serialized_view.c_str());
  
  FILE *ast = fopen("ast.toml", "w");
  fprintf(ast, "%s", serialized_view.c_str());
  fflush(ast);
  fclose(ast);
  
  return 0;
}


