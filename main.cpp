#include "ast.hpp"
#include "lex.hpp"
#include <fstream>
#include <sstream>
#include "scope.hpp"
#include "type.hpp"

int main(int argc, char *argv[]) {
  Lexer lexer;
  
  std::ifstream stream("dummy.ela");
  std::stringstream ss;
  ss << stream.rdbuf();
  auto str = ss.str();
  
  init_type_system();
  
  Context context;
  Parser parser(str, "dummy.ela", context);
  auto root = parser.parse();
  
  
  
  // auto state = Lexer::State::from_file(str, "dummy.ela");
  // while (auto token = lexer.get_token(state)) 
  //   printf("token: value='%s'\n  type='%s'\n", token.value.c_str(), TTypeToString(token.type).c_str());
  
  return 0;
}
