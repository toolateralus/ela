#pragma once
#include "ast.hpp"
#include <vector>

struct Context; // forward declaration for scope access

struct SymbolPass final {
  Typer &typer;
  SymbolPass (Typer &typer): typer(typer) {}
  std::vector<ASTStructDeclaration*> struct_declarations;
  std::vector<ASTTraitDeclaration*> trait_declarations;
  std::vector<ASTChoiceDeclaration*> choice_declarations;
  std::vector<ASTEnumDeclaration*> enum_declarations;
  std::vector<ASTFunctionDeclaration*> function_declarations;
  std::vector<ASTAlias*> alias_declarations;
  std::vector<ASTImpl*> impl_declarations;
  void collect_declarations(ASTNode *node);
  void run(ASTProgram* ast);
  void process_type_declarations();
  void process_function_headers();
  void type_enum(ASTEnumDeclaration* node);
};
