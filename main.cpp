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
TypeInfo *type_info_table[MAX_NUM_TYPES];
int num_types;

// this is just an approximation. 
// It may be too little since this arena is used a lot.
jstl::Arena type_arena{(sizeof(Type) * MAX_NUM_TYPES) +
                       (sizeof(TypeInfo) * MAX_NUM_TYPES) * 2};
// the same for this
jstl::Arena scope_arena = {MB(10)};
// the same for this
jstl::Arena ast_arena{MB(10)};
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

  // initialize the basic scalar types etc.
  init_type_system();

  Context context;
  Parser parser(str, "dummy.ela", context);
  ASTProgram *root = parser.parse();

  printf("%p\n", root);
  
  SerializeVisitor visitor;
  
  auto serialized_view = std::any_cast<std::string>(visitor.visit(root));
  
  printf("%s\n", serialized_view.c_str());

  return 0;
}
