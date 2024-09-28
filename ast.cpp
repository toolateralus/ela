#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "type.hpp"
#include "visitor.hpp"
#include <cassert>
#include <filesystem>
#include <format>
#include <fstream>

void Parser::init_directive_routines() {
  // #include
  // Just like C's include, just paste a text file right above where the include
  // is used. Not a pre processor!
  {
    directive_routines.push_back(DirectiveRoutine{
        .identifier = "include",
        .kind = DIRECTIVE_KIND_STATEMENT,
        .run = [](Parser *parser) static {
          auto filename = parser->expect(TType::String).value;
          if (!std::filesystem::exists(filename)) {
            throw_error(
                std::format("Couldn't find included file: {}", filename),
                ERROR_CRITICAL, {});
          }
          std::stringstream ss;
          std::ifstream isftr(filename);
          ss << isftr.rdbuf();
          parser->states.push_back(Lexer::State::from_file(ss.str(), filename));
          return nullptr;
        }});
  }

  // #read
  // Read a file into a string at compile time. Nice for embedding resources
  // into your program.
  // TODO: add a binary mode, where its just an array of chars or something.
  {
    directive_routines.push_back(
        {.identifier = "read",
         .kind = DIRECTIVE_KIND_EXPRESSION,
         .run = [](Parser *parser) static {
           auto filename = parser->expect(TType::String).value;
           if (!std::filesystem::exists(filename)) {
             throw_error(std::format("Couldn't find 'read' file: {}", filename),
                         ERROR_CRITICAL, {});
           }
           std::stringstream ss;
           std::ifstream isftr(filename);
           ss << isftr.rdbuf();
           auto string = ast_alloc<ASTLiteral>();
           string->tag = ASTLiteral::RawString;
           string->value = ss.str();
           return string;
         }});
  }

  // #test
  // declare a test function. Only gets compiled into --test builds, and
  // produces a test main, a builtin test suite.
  // TODO: add categories and extra naming stuff to these. would be nice for
  // filtering etc.
  {
    directive_routines.push_back(
        {.identifier = "test",
         .kind = DIRECTIVE_KIND_STATEMENT,
         .run = [](Parser *parser) static -> Nullable<ASTNode> {
           auto name = parser->expect(TType::Identifier);
           parser->expect(TType::DoubleColon);
           auto func = parser->parse_function_declaration(name);
           func->flags |= FUNCTION_TEST;
           return func;
         }});
  }

  // #foreign
  // Declare a foreign function, like C's extern. Super janky and bad because
  // our boilerplate is crap and uses stdlib stuff.
  {
    directive_routines.push_back(
        {.identifier = "foreign",
         .kind = DIRECTIVE_KIND_STATEMENT,
         .run = [](Parser *parser) static {
           auto function = ast_alloc<ASTFuncDecl>();
           parser->begin_token_frame();

           auto name = parser->expect(TType::Identifier);

           if (parser->context.current_scope != parser->context.root_scope) {
             throw_error(
                 std::format(
                     "cannot declare a non-top level foreign function:: {}",
                     name.value),
                 ERROR_CRITICAL, parser->token_frames.back());
           }

           parser->expect(TType::DoubleColon);
           function->params = parser->parse_parameters();
           function->name = name;

           if (parser->peek().type != TType::Arrow) {
             function->return_type = ASTType::get_void();
           } else {
             parser->expect(TType::Arrow);
             function->return_type = parser->parse_type();
           }

           parser->end_token_frame(function);
           function->flags |= FUNCTION_FOREIGN;
           parser->expect(TType::Semi);
           return function;
         }});
  }

  // #import
  // Imports from usr/local/lib/ela by identifier and no file ext.
  {
    directive_routines.push_back(
        {.identifier = "import",
         .kind = DIRECTIVE_KIND_STATEMENT,
         .run = [](Parser *parser) static -> Nullable<ASTNode> {
           auto iden = parser->expect(TType::Identifier).value;
           static std::filesystem::path path = [] {
#ifdef _WIN32
             return std::filesystem::path("C:\\Program Files\\ela");
#else
              return std::filesystem::path("/usr/local/lib/ela");
#endif
           }();
           auto filename = path.string() + "/" + iden + ".ela";
           if (!std::filesystem::exists(filename)) {
             throw_error(
                 std::format("Couldn't find imported module: {}", filename),
                 ERROR_CRITICAL, {});
           }
           std::stringstream ss;
           std::ifstream isftr(filename);
           ss << isftr.rdbuf();
           parser->states.push_back(
               Lexer::State::from_file(ss.str(), filename));
           return nullptr;
         }});
  }
}

Nullable<ASTNode> Parser::process_directive(DirectiveKind kind,
                                            const std::string &identifier) {
  // compare aganist the kind of the routine with expected type, based on parser
  // location
  for (const auto &routine : directive_routines) {
    if (routine.kind == kind && routine.identifier == identifier) {
      return routine.run(this);
    }
  }

  throw_error(
      std::format("failed to call unknown directive routine: {}", identifier),
      ERROR_FAILURE, {});
}

ASTProgram *Parser::parse() {
  begin_token_frame();
  auto program = ast_alloc<ASTProgram>();

  while (tok.type != TType::Eof) {
    if (tok.type == TType::Directive) {
      eat();
      auto identifer = expect(TType::Identifier).value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifer);
      if (result.is_not_null()) {
        auto statement = dynamic_cast<ASTStatement *>(result.get());
        if (statement) {
          program->statements.push(statement);
        }
      }
      if (semicolon())
        eat();
      continue;
    }
    program->statements.push(parse_statement());
    if (semicolon())
      eat();
  }
  end_token_frame(program);
  return program;
}

// TODO: Cleanup this horrific function before it gets too out of control.
ASTStatement *Parser::parse_statement() {
  begin_token_frame();
  auto tok = peek();

  if (tok.type == TType::Directive) {
    eat();
    auto statement = dynamic_cast<ASTStatement *>(
        process_directive(DIRECTIVE_KIND_STATEMENT,
                          expect(TType::Identifier).value)
            .get());
    if (!statement) {
      throw_error(
          std::format("Directive '{}' did not return a valid statement node",
                      tok.value),
          ERROR_CRITICAL, token_frames.back());
    }
  }

  if (tok.type == TType::Increment || tok.type == TType::Decrement) {
    auto statement = ast_alloc<ASTExprStatement>();
    auto unary = ast_alloc<ASTUnaryExpr>();
    unary->op = eat();
    auto operand = ast_alloc<ASTIdentifier>();
    operand->value = eat();
    unary->operand = operand;
    statement->expression = unary;
    return statement;
  }

  if (tok.type == TType::LCurly) {
    return parse_block();
  } else if (tok.type == TType::Return) {
    expect(TType::Return);
    auto return_node = ast_alloc<ASTReturn>();
    if (peek().type != TType::Semi) {
      return_node->expression = parse_expr();
    }
    end_token_frame(return_node);
    return return_node;
  } else if (tok.type == TType::Break) {
    eat();
    auto _break = ast_alloc<ASTBreak>();
    end_token_frame(_break);
    return _break;
  } else if (tok.type == TType::Continue) {
    eat();
    auto _continue = ast_alloc<ASTContinue>();
    end_token_frame(_continue);
    return _continue;
  } else if (tok.type == TType::For) {
    eat();
    auto node = ast_alloc<ASTFor>();
    tok = peek();

    if (find_type_id(tok.value, {}) != -1) {
      node->tag = ASTFor::CStyle;
      node->value.c_style.decl = parse_declaration();
      expect(TType::Semi);
      node->value.c_style.condition = parse_expr();
      expect(TType::Semi);
      node->value.c_style.increment = parse_expr();
    } else {
      node->tag = ASTFor::RangeBased;
      node->value.range_based.target = parse_expr();
      // TODO: add 'in' keyword
      expect(TType::Semi);
      node->value.range_based.collection = parse_expr();
    }

    node->block = parse_block();

    end_token_frame(node);
    return node;
  } else if (tok.type == TType::While) {
    eat();
    auto node = ast_alloc<ASTWhile>();
    if (peek().type != TType::LCurly) {
      node->condition = parse_expr();
    }
    node->block = parse_block();
    end_token_frame(node);
    return node;
  } else if (tok.type == TType::If) {
    eat();
    auto node = ast_alloc<ASTIf>();
    node->condition = parse_expr();
    node->block = parse_block();

    if (peek().type == TType::Else) {
      eat();
      auto node_else = ast_alloc<ASTElse>();
      if (peek().type == TType::If) {
        auto inner_if = dynamic_cast<ASTIf *>(parse_statement());
        assert(inner_if != NULL);
        node_else->_if = inner_if;
      } else {
        node_else->block = parse_block();
      }
      node->_else = node_else;
    }
    end_token_frame(node);
    return node;
  } else if (find_type_id(tok.value, {}) != -1) {
    auto decl = parse_declaration();
    end_token_frame(nullptr);
    return decl;
  }

  eat();

  if (peek().is_comp_assign()) {
    if (tok.type != TType::Identifier) {
      throw_error(
          std::format("Compound assignment must target an identifier. Got {}",
                      tok.value),
          ERROR_CRITICAL, token_frames.back());
    }
    auto comp_assign = ast_alloc<ASTCompAssign>();
    comp_assign->op = eat();
    comp_assign->name = tok;
    comp_assign->expr = parse_expr();
    end_token_frame(comp_assign);
    return comp_assign;
  } else if (peek().type == TType::DoubleColon) {
    eat();
    if (peek().type == TType::LParen) {
      auto node = parse_function_declaration(tok);
      end_token_frame(node);
      return node;
    }
  } else if (peek().type == TType::Assign) {
    auto statement = ast_alloc<ASTExprStatement>();
    statement->expression = parse_assignment(&tok);
    end_token_frame(statement);
    return statement;
  } else if (tok.type == TType::Identifier && peek().type == TType::LParen) {
    auto statement = parse_call_statement(tok);
    end_token_frame(statement);
    return statement;
  }

  throw_error(
      std::format("Unexpected token when parsing statement: {}", tok.value),
      ERROR_CRITICAL, token_frames.back());
}
ASTDeclaration *Parser::parse_declaration() {
  begin_token_frame();
  ASTDeclaration *decl = ast_alloc<ASTDeclaration>();
  decl->type = parse_type();
  auto iden = eat();
  decl->name = iden;
  if (peek().type == TType::Assign) {
    eat();
    auto expr = parse_expr();
    decl->value = expr;
  }
  context.current_scope->insert(iden.value, -1);
  end_token_frame(decl);
  return decl;
}
ASTFuncDecl *Parser::parse_function_declaration(Token name) {

  if (context.current_scope != context.root_scope) {
    throw_error(
        std::format("cannot declare a non-top level function:: {}", name.value),
        ERROR_CRITICAL, token_frames.back());
  }

  begin_token_frame();
  token_frames.back().push_back(name);
  auto function = ast_alloc<ASTFuncDecl>();
  function->params = parse_parameters();
  function->name = name;

  if (peek().type != TType::Arrow) {
    function->return_type = ASTType::get_void();
  } else {
    expect(TType::Arrow);
    function->return_type = parse_type();
  }

  end_token_frame(function);
  function->block = parse_block();
  return function;
}
ASTBlock *Parser::parse_block() {
  begin_token_frame();
  expect(TType::LCurly);
  ASTBlock *block = ast_alloc<ASTBlock>();
  context.enter_scope();
  while (not_eof() && peek().type != TType::RCurly) {
    block->statements.push(parse_statement());
    if (semicolon())
      eat();
  }
  expect(TType::RCurly);
  block->scope = context.exit_scope();
  end_token_frame(block);
  return block;
}

ASTExpr *Parser::parse_expr() {
  auto expr = parse_assignment(nullptr);
  return expr;
}
ASTExpr *Parser::parse_assignment(Token *iden = nullptr) {
  ASTExpr *left;

  if (iden != nullptr) {
    auto iden_node = ast_alloc<ASTIdentifier>();
    iden_node->value = *iden;
    left = iden_node;
  } else {
    left = parse_logical_or();
  }

  if (peek().type == TType::Assign) {
    auto op = eat();
    auto right = parse_assignment();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    return binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_logical_or() {
  auto left = parse_logical_and();
  while (peek().type == TType::LogicalOr) {
    auto op = eat();
    auto right = parse_logical_and();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_logical_and() {
  auto left = parse_bitwise_or();
  while (peek().type == TType::LogicalAnd) {
    auto op = eat();
    auto right = parse_bitwise_or();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_or() {
  auto left = parse_bitwise_xor();
  while (peek().type == TType::Or) {
    auto op = eat();
    auto right = parse_bitwise_xor();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_xor() {
  auto left = parse_bitwise_and();
  while (peek().type == TType::Xor) {
    auto op = eat();
    auto right = parse_bitwise_and();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_bitwise_and() {
  auto left = parse_equality();
  while (peek().type == TType::And) {
    auto op = eat();
    auto right = parse_equality();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_equality() {
  auto left = parse_relational();
  while (peek().type == TType::EQ || peek().type == TType::NEQ) {
    auto op = eat();
    auto right = parse_relational();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_relational() {
  auto left = parse_shift();
  while (peek().type == TType::LT || peek().type == TType::GT ||
         peek().type == TType::LE || peek().type == TType::GE) {
    auto op = eat();
    auto right = parse_shift();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_shift() {
  auto left = parse_additive();
  while (peek().type == TType::SHL || peek().type == TType::SHR) {
    auto op = eat();
    auto right = parse_additive();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_additive() {
  auto left = parse_multiplicative();
  while (peek().type == TType::Add || peek().type == TType::Sub) {
    auto op = eat();
    auto right = parse_multiplicative();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_multiplicative() {
  auto left = parse_unary();
  while (peek().type == TType::Mul || peek().type == TType::Div ||
         peek().type == TType::Modulo) {
    auto op = eat();
    auto right = parse_unary();
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}
ASTExpr *Parser::parse_unary() {
  if (peek().type == TType::Add || peek().type == TType::Sub ||
      peek().type == TType::Not || peek().type == TType::BitwiseNot ||
      peek().type == TType::Increment || peek().type == TType::Decrement ||
      peek().type == TType::Mul || peek().type == TType::And) {
    auto op = eat();
    auto expr = parse_unary();
    auto unaryexpr = ast_alloc<ASTUnaryExpr>();
    unaryexpr->op = op;
    unaryexpr->operand = expr;
    return unaryexpr;
  }
  return parse_postfix();
}
ASTExpr *Parser::parse_postfix() {
  auto left = parse_primary();

  if (auto identifier = dynamic_cast<ASTIdentifier *>(left)) {
    if (peek().type == TType::LParen) {
      auto tok = identifier->value;
      return parse_call(tok);
    }
  }

  // TODO: add -- and ++?
  // TODO: parse and build AST for dot expressions.
  // while (peek().type == TType::Dot) { }

  return left;
}
ASTExpr *Parser::parse_primary() {
  auto tok = peek();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  switch (tok.type) {
  case TType::Identifier: {
    eat();
    auto iden = ast_alloc<ASTIdentifier>();
    iden->value = tok;
    ;
    return iden;
  }
  case TType::Null: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Null;
    literal->value = tok.value;
    return literal;
  }
  case TType::True: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Bool;
    literal->value = tok.value;
    return literal;
  }
  case TType::False: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Bool;
    literal->value = tok.value;
    return literal;
  }
  case TType::Integer: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Integer;
    literal->value = tok.value;
    return literal;
  }
  case TType::Float: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::Float;
    literal->value = tok.value;
    return literal;
  }
  case TType::String: {
    eat();
    auto literal = ast_alloc<ASTLiteral>();
    literal->tag = ASTLiteral::String;
    literal->value = tok.value;
    return literal;
  }
  case TType::LParen: {
    eat(); // consume '('
    auto expr = parse_expr();
    if (peek().type != TType::RParen) {
      throw_error("Expected ')'", ERROR_FAILURE, token_frames.back());
    }
    eat(); // consume ')'
    return expr;
  }
  default: {
    throw_error(std::format("Invalid primary expression. Token: {}, Type: {}",
                            tok.value, TTypeToString(tok.type)),
                ERROR_FAILURE, token_frames.back());
    return nullptr;
  }
  }
}
ASTType *Parser::parse_type() {
  auto base = eat().value;
  TypeExt extension_info;

  while (true) {
    if (peek().type == TType::LBrace) {
      extension_info.extensions.push(TYPE_EXT_ARRAY);
      expect(TType::LBrace);
      if (peek().type == TType::Integer) {
        auto integer = expect(TType::Integer);
        ;
        extension_info.array_sizes.push(std::stoi(integer.value));
      } else {
        extension_info.array_sizes.push(-1);
      }
      expect(TType::RBrace);
    } else if (peek().type == TType::Mul) {
      expect(TType::Mul);
      extension_info.extensions.push(TYPE_EXT_POINTER);
    } else {
      break;
    }
  }

  auto node = ast_alloc<ASTType>();
  node->base = base;
  node->extension_info = extension_info;
  return node;
}
ASTParamsDecl *Parser::parse_parameters() {
  ASTParamsDecl *params = ast_alloc<ASTParamsDecl>();
  expect(TType::LParen);

  while (peek().type != TType::RParen) {
    auto type = parse_type();
    auto name = expect(TType::Identifier).value;

    auto param = ast_alloc<ASTParamDecl>();
    param->type = type;
    param->name = name;

    if (peek().type == TType::Assign) {
      eat();
      param->default_value = parse_expr();
    }

    params->params.push(param);

    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    } else
      break;
  }

  expect(TType::RParen);
  return params;
}
ASTArguments *Parser::parse_arguments() {
  auto args = ast_alloc<ASTArguments>();
  expect(TType::LParen);

  if (peek().type == TType::RParen) {
    expect(TType::RParen);
    return args;
  }

  while (peek().type != TType::RParen) {
    args->arguments.push(parse_expr());
    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    }
  }
  expect(TType::RParen);
  return args;
}
ASTCall *Parser::parse_call(const Token &name) {
  auto args = parse_arguments();
  ASTCall *call = ast_alloc<ASTCall>();
  call->name = name;
  call->arguments = args;
  return call;
}
ASTStatement *Parser::parse_call_statement(Token iden) {

  auto args = parse_arguments();
  ASTExprStatement *statement = ast_alloc<ASTExprStatement>();
  ASTCall *call = ast_alloc<ASTCall>();
  call->name = iden;
  call->arguments = args;
  statement->expression = call;
  return statement;
}

/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
std::any ASTProgram::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTBlock::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTType::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTExprStatement::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTDeclaration::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTBinExpr::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTUnaryExpr::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTIdentifier::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTLiteral::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTParamDecl::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTParamsDecl::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTFuncDecl::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTCall::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTArguments::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}
std::any ASTReturn::accept(VisitorBase *visitor) {
  return visitor->visit(this);
};
std::any ASTBreak::accept(VisitorBase *visitor) {
  return visitor->visit(this);
};
std::any ASTContinue::accept(VisitorBase *visitor) {
  return visitor->visit(this);
};
std::any ASTFor::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTIf::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTElse::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTWhile::accept(VisitorBase *visitor) { return visitor->visit(this); }
std::any ASTCompAssign::accept(VisitorBase *visitor) {
  return visitor->visit(this);
}

/*
  ###########################################
  ##### DECLARE VISITOR ACCEPT METHODS ######
  ###########################################
*/
// std::any ASTStructDeclaration::accept(VisitorBase *visitor) {
//   return visitor->accept(this);
// }
