#include "visitor.hpp"

#include "ast.hpp"
#include "core.hpp"
#include "scope.hpp"
#include "type.hpp"

/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
// {

// We use this so we have a chance to dynamically erase AST and replace it at visitation time.
#define NODE_VISIT()                               \
  ASTNode *old_parent = visitor->parent_node;      \
  ASTNode *old_parent_prev = visitor->parent_prev; \
  visitor->parent_prev = old_parent;               \
  visitor->parent_node = this;                     \
  Defer _defer([&] {                               \
    visitor->parent_node = old_parent;             \
    visitor->parent_prev = old_parent_prev;        \
  });                                              \
  visitor->visit(this);

// clang-format off
void ASTUnpackElement::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTUnpack::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTDyn_Of::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTWhere::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTModule::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTImport::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTType_Of::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTChoiceDeclaration::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTSwitch::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTProgram::accept(VisitorBase *visitor) {NODE_VISIT() }
void ASTBlock::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTType::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTExprStatement::accept(VisitorBase *visitor) {NODE_VISIT() }
void ASTVariable::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTBinExpr::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTUnaryExpr::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTLiteral::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTParamDecl::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTMethodCall::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTForCStyle::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTParamsDecl::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTWhereStatement::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTFunctionDeclaration::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTTuple::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTCall::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTArguments::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTReturn::accept(VisitorBase *visitor) { NODE_VISIT() };
void ASTBreak::accept(VisitorBase *visitor) { NODE_VISIT() };
void ASTContinue::accept(VisitorBase *visitor) { NODE_VISIT() };
void ASTFor::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTIf::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTLambda::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTNoop::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTElse::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTWhile::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTStructDeclaration::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTDotExpr::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTIndex::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTCast::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTEnumDeclaration::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTInitializerList::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTRange::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTDestructure::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTAlias::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTImpl::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTDefer::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTStatementList::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTTraitDeclaration::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTIntrinsic::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTPatternMatch::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTPath::accept(VisitorBase *visitor) { NODE_VISIT() }
void ASTRun::accept(VisitorBase *visitor) { NODE_VISIT() }

// clang-format on
// }
/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/

size_t temporary_variable_index = 0;
std::string get_temporary_variable() { return "$" + std::to_string(temporary_variable_index++); }
