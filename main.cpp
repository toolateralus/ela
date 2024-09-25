#include "ast.hpp"
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
TypeInfo *type_info_table[MAX_NUM_TYPES];
int num_types;

jstl::Arena type_arena{(sizeof(Type) * MAX_NUM_TYPES) +
                       (sizeof(TypeInfo) * MAX_NUM_TYPES)};
jstl::Arena scope_arena = {GB(1)};
jstl::Arena ast_arena{GB(1)};
/* 
  #########################
  ### PROVIDING EXTERNS ###
  #########################
*/


int main(int argc, char *argv[]) {
  Lexer lexer;

  std::ifstream stream("dummy.ela");
  std::stringstream ss;
  ss << stream.rdbuf();
  auto str = ss.str();

  init_type_system();

  Context context;
  Parser parser(str, "dummy.ela", context);
  ASTProgram *root = parser.parse();

  printf("%p\n", root);

  return 0;
}
