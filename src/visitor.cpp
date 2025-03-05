#include "visitor.hpp"



#include "ast.hpp"
#include "core.hpp"
#include "type.hpp"

/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
// {

// clang-format off
void ASTWhere::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTModule::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTImport::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTType_Of::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTTaggedUnionDeclaration::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTSwitch::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTProgram::accept(VisitorBase *visitor) {visitor->visit(this); }
void ASTBlock::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTType::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTExprStatement::accept(VisitorBase *visitor) {visitor->visit(this); }
void ASTVariable::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTBinExpr::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTUnaryExpr::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTIdentifier::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTLiteral::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTParamDecl::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTParamsDecl::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTFunctionDeclaration::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTTuple::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTCall::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTArguments::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTReturn::accept(VisitorBase *visitor) { visitor->visit(this); };
void ASTBreak::accept(VisitorBase *visitor) { visitor->visit(this); };
void ASTContinue::accept(VisitorBase *visitor) { visitor->visit(this); };
void ASTFor::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTIf::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTLambda::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTNoop::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTElse::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTWhile::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTStructDeclaration::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTDotExpr::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTScopeResolution::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTSubscript::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTCast::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTEnumDeclaration::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTInitializerList::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTRange::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTTupleDeconstruction::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTAlias::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTImpl::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTDefer::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTStatementList::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTInterfaceDeclaration::accept(VisitorBase *visitor) { visitor->visit(this); }
void ASTSize_Of::accept(VisitorBase *visitor) { visitor->visit(this); }

// clang-format on
// }
/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
void Parser::parse_pointer_extensions(ASTType *type) {
  int pointer_depth = 0;

  while (peek().type == TType::Mul) {
    eat();
    pointer_depth++;
  }

  Mutability mutability;

  if (pointer_depth != 0) {
    if (peek().type == TType::Const) {
      eat();
      mutability = CONST;
    } else if (peek().type == TType::Mut) {
      eat();
      mutability = MUT;
    } else {
      throw_error(
          "'*const/*mut' are required for all pointer types now, as a prefix. such as '*const s32', '**mut s64'",
          {peek().location});
    }
  }

  for (int i = 0; i < pointer_depth; ++i) {
    type->extensions.push_back({mutability ? TYPE_EXT_POINTER_CONST : TYPE_EXT_POINTER_MUT});
  }
}
