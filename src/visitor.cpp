#include "visitor.hpp"

#include <any>

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
std::any ASTSelfType::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTTaggedUnionDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTSwitch::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTProgram::accept(VisitorBase *visitor) {return visitor->visit(this); }
std::any ASTBlock::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTType::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTExprStatement::accept(VisitorBase *visitor) {return visitor->visit(this); }
std::any ASTDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTBinExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTUnaryExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIdentifier::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTLiteral::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTParamDecl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTParamsDecl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTFunctionDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTTuple::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCall::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTArguments::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTReturn::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTBreak::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTContinue::accept(VisitorBase *visitor) { return visitor->visit(this); };
std::any ASTFor::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIf::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTUnionDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTNoop::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTElse::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTWhile::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTStructDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTDotExpr::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTScopeResolution::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTSubscript::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCast::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTEnumDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTInitializerList::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTRange::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTTupleDeconstruction::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTAlias::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTImpl::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTDefer::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTStatementList::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTInterfaceDeclaration::accept(VisitorBase *visitor) { return visitor->visit(this); }

// clang-format on
// }
/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/

