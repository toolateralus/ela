#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "nullable.hpp"
#include "scope.hpp"
#include "type.hpp"
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
          parser->fill_buffer_if_needed();
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
           func->flags |= (int)FunctionInstanceFlags::FUNCTION_IS_TEST;
           return func;
         }});
  }

  // #foreign
  // Declare a foreign function, like C's extern. Super janky and bad because
  // our boilerplate is crap and uses stdlib stuff.
  {
    directive_routines.push_back({
      .identifier = "foreign",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) static {
        auto function = ast_alloc<ASTFuncDecl>();
        parser->begin_token_frame();
        auto name = parser->expect(TType::Identifier);
        if (parser->context.current_scope != parser->context.root_scope) 
          throw_error(std::format("cannot declare a non-top level foreign function:: {}", name.value), ERROR_CRITICAL, parser->token_frames.back());
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
        function->meta_type = FunctionMetaType::FUNCTION_TYPE_FOREIGN;
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
           
           parser->states.push_back(Lexer::State::from_file(ss.str(), filename));
           parser->fill_buffer_if_needed();
           
           return nullptr;
         }});
  }
  
  
  
  // TODO: fix this up. temporarily disabled. 2024-09-29 12:50:41
  // #type
  // get a 'Type *' struct ptr to reflect on a given type. 
  // has .fields and .size only currently
  {
    directive_routines.push_back({
      .identifier = "type",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto type = parser->parse_type();
        auto outer = ast_alloc<ASTType>();
        outer->flags = ASTTYPE_EMIT_OBJECT;
        outer->base = "Type";
        outer->extension_info = {
          .extensions = {TYPE_EXT_POINTER}
        };
        outer->pointing_to = type;
        return outer;
      }
    });
  }
  
  
  // #ctor and #dtor
  {
    directive_routines.push_back({
      .identifier = "ctor",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::DoubleColon);
        auto func_decl = parser->parse_function_declaration(Token({}, parser->current_struct_decl.get()->type->base, TType::Identifier, TFamily::Identifier));
        func_decl->flags |= (FUNCTION_IS_CTOR | FUNCTION_IS_METHOD);
        return func_decl; 
      }
    });
    
    directive_routines.push_back({
      .identifier = "dtor",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::DoubleColon);
        auto func_decl = parser->parse_function_declaration(Token({}, parser->current_struct_decl.get()->type->base, TType::Identifier, TFamily::Identifier));
        func_decl->flags |= (FUNCTION_IS_DTOR | FUNCTION_IS_METHOD);
        return func_decl; 
      }
    });
  }
  
  
  // #make, which also serves as a casting and copy construction method, as well as normal ctors.
  {
    directive_routines.push_back({
      .identifier = "make",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto args = parser->parse_arguments();
        auto type = args->arguments[0];
        auto type_arg = dynamic_cast<ASTType*>(type);
        if (!type_arg) {
          throw_error("Expect a type as the first argument in a #make call.", ERROR_FAILURE, args->source_tokens);
        }
        args->arguments.erase(args->arguments.begin());
        auto make = ast_alloc<ASTMake>();
        make->type_arg = type_arg;
        make->arguments = args;
        make->kind = MAKE_CTOR;
        
        if (type_arg->extension_info.is_pointer()) {
          make->kind = MAKE_CAST;
        }
        
        return make;
      }
    });
  }
  
  {
    directive_routines.push_back({
      .identifier = "compiler_flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto string = parser->expect(TType::String).value;
        compile_command.add_compilation_flags(string);
        return nullptr;
      }
    });
  }
  
  
}

Nullable<ASTNode> Parser::process_directive(DirectiveKind kind,
                                            const std::string &identifier) {
  begin_token_frame();                                              
  // compare aganist the kind of the routine with expected type, based on parser
  // location
  for (const auto &routine : directive_routines) {
    if (routine.kind == kind && routine.identifier == identifier) {
      auto result = routine.run(this);
      end_token_frame(result.get());
      return result;
    }
  }
  throw_error(
      std::format("failed to call unknown directive routine: {}", identifier),
      ERROR_FAILURE, token_frames.back());
}

ASTProgram *Parser::parse() {
  begin_token_frame();
  auto program = ast_alloc<ASTProgram>();

  while (true) {
    if (peek().type == TType::Eof && states.size() > 0) {
      states.pop_back();
    }
    
    if (semicolon()) eat();
    
    if (peek().type == TType::Eof && states.empty()) {
      break;
    }
    
    if (peek().type == TType::Directive) {
      eat();
      auto identifer = expect(TType::Identifier).value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifer);
      if (result.is_not_null()) {
        auto statement = dynamic_cast<ASTStatement *>(result.get());
        if (statement) {
          program->statements.push_back(statement);
        }
      }
      if (semicolon())
        eat();
      continue;
    }
    program->statements.push_back(parse_statement());
    if (semicolon())
      eat();
  }
  end_token_frame(program);
  return program;
}

// TODO: Cleanup this horrific function before it gets too out of control.
// TODO(later): I said that and then proceeded to make it out of control
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
    return statement;
  }


  // Increment/ Decrement statements;
  if (tok.type == TType::Increment || tok.type == TType::Decrement) {
    auto statement = ast_alloc<ASTExprStatement>();
    statement->expression = parse_expr();
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
      
      auto decl = parse_declaration();
      node->value.c_style.decl = decl;
      context.current_scope->erase(decl->name.value);
      
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
    
    if (node->tag == ASTFor::CStyle) {
      context.enter_scope(node->block->scope);
      context.current_scope->insert(node->value.c_style.decl->name.value, -1);
      context.exit_scope();
    }

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
  } else if (tok.type == TType::Mul) {
    auto expr = parse_expr();
    auto statement = ast_alloc<ASTExprStatement>();
    statement->expression = expr;
    end_token_frame(statement);
    return statement;
  }

  
  // subscript assignment
  if ((tok.type == TType::Identifier && lookahead_buf()[1].type == TType::LBrace) || 
      (tok.type == TType::Identifier && lookahead_buf()[1].type == TType::Dot) 
      || lookahead_buf()[1].type == TType::Assign
      || lookahead_buf()[1].type == TType::LParen) {
    auto statement = ast_alloc<ASTExprStatement>();
    end_token_frame(statement);
    statement->expression = parse_expr();
    return statement;
  }
  
  if (lookahead_buf()[1].is_comp_assign()) {
    if (tok.type != TType::Identifier) {
      throw_error(
          std::format("Compound assignment must target an identifier. Got {}",
                      tok.value),
          ERROR_CRITICAL, token_frames.back());
    }
    eat();
    auto comp_assign = ast_alloc<ASTCompAssign>();
    comp_assign->op = eat();
    comp_assign->name = tok;
    comp_assign->expr = parse_expr();
    end_token_frame(comp_assign);
    return comp_assign;
  }
  
  if (lookahead_buf()[1].type == TType::DoubleColon) {
    expect(TType::Identifier);
    expect(TType::DoubleColon);
    if (peek().type == TType::LParen) {
      auto node = parse_function_declaration(tok);
      end_token_frame(node);
      return node;
    } else if (peek().type == TType::Struct) {
      auto struct_decl = parse_struct_declaration(tok);
      end_token_frame(struct_decl);
      return struct_decl;
    }
  }
  
  eat();

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
  end_token_frame(decl);
  if (context.current_scope->lookup(iden.value)) {
    throw_error(std::format("re-definition of '{}'", iden.value), ERROR_FAILURE, decl->source_tokens);
  }
  context.current_scope->insert(iden.value, -1);
  return decl;
}

ASTFuncDecl *Parser::parse_function_declaration(Token name) {
  
  begin_token_frame();
  token_frames.back().push_back(name);
  auto function = ast_alloc<ASTFuncDecl>();
  
  const auto isnt_ctor_or_dtor = current_struct_decl.is_not_null() && current_struct_decl.get()->type->base != name.value;
  
  if (context.current_scope->lookup(name.value) && isnt_ctor_or_dtor) {
    throw_error(std::format("re-definition of function '{}'", name.value), ERROR_FAILURE, {});
  }
  
  // to allow for recursion
  context.current_scope->insert(name.value, -1);
  
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
    block->statements.push_back(parse_statement());
    if (semicolon())
      eat();
  }
  expect(TType::RCurly);
  block->scope = context.exit_scope();
  end_token_frame(block);
  return block;
}

ASTStructDeclaration *Parser::parse_struct_declaration(Token name) {
  expect(TType::Struct);
  auto decl = ast_alloc<ASTStructDeclaration>();
  current_struct_decl = decl;
  
  // fwd declare the type.
  auto type_id = create_struct_type(name.value, {});
  
  auto type = ast_alloc<ASTType>();
  decl->type = type;
  decl->type->base = name.value;
  decl->type->extension_info = {};
  decl->type->resolved_type = type_id;
  
  if (!semicolon()) {
    auto block = parse_block();
    block->scope->is_struct_scope = true;
    
    for (const auto &statement: block->statements) {
      if (auto field = dynamic_cast<ASTDeclaration*>(statement)) {
        decl->fields.push_back(field);
      } else if (auto fn_decl = dynamic_cast<ASTFuncDecl*>(statement)) {
        decl->methods.push_back(fn_decl);
      } else {
        throw_error("Non-field or non-method declaration not allowed in struct.", ERROR_FAILURE, statement->source_tokens);
      }
    }
    
    decl->scope = block->scope;
  } else {
    Type *t = get_type(type_id);
    auto info = static_cast<StructTypeInfo *>(t->info.get());
    info->flags |= STRUCT_FLAG_FORWARD_DECLARED;
  }
  
  current_struct_decl = nullptr;
  
  return decl;
}

ASTExpr *Parser::parse_expr() {
  auto expr = parse_assignment();
  return expr;
}
ASTExpr *Parser::parse_assignment() {
  ASTExpr *left = parse_logical_or();

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

  // TODO: Probably add this to the loop below, when and if we have dot calls.
  if (auto identifier = dynamic_cast<ASTIdentifier *>(left)) {
    if (peek().type == TType::LParen) {
      auto tok = identifier->value;
      return parse_call(tok);
    }
  }

  // build dot and subscript expressions
  while (peek().type == TType::Dot || peek().type == TType::LBrace) { 
    if (peek().type == TType::Dot) {
      eat();
      auto dot = ast_alloc<ASTDotExpr>();
      dot->type = ast_alloc<ASTType>();
      dot->left = left;
      dot->right = parse_postfix();
      left = dot;
    } else {
      eat();
      auto index = parse_expr();
      expect(TType::RBrace);
      auto subscript = ast_alloc<ASTSubscript>();
      subscript->left = left;
      subscript->subscript = index;
      left = subscript;
    }
  }

  return left;
}
ASTExpr *Parser::parse_primary() {
  auto tok = peek();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  switch (tok.type) {
  case TType::LCurly: {
    eat();
    auto init_list = ast_alloc<ASTInitializerList>();
    while (peek().type != TType::RCurly) {
      init_list->expressions.push_back(parse_expr());
      if (peek().type == TType::Comma) {
        eat();
      }
    }
    expect(TType::RCurly);
    return init_list;
  }
    
  case TType::Identifier: {
    if (find_type_id(tok.value, {}) != -1) {
      return parse_type();
    }
    eat();
    auto iden = ast_alloc<ASTIdentifier>();
    iden->value = tok;
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
      extension_info.extensions.push_back(TYPE_EXT_ARRAY);
      expect(TType::LBrace);
      if (peek().type == TType::Integer) {
        auto integer = expect(TType::Integer);
        ;
        extension_info.array_sizes.push_back(std::stoi(integer.value));
      } else {
        extension_info.array_sizes.push_back(-1);
      }
      expect(TType::RBrace);
    } else if (peek().type == TType::Mul) {
      expect(TType::Mul);
      extension_info.extensions.push_back(TYPE_EXT_POINTER);
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

    params->params.push_back(param);

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
    args->arguments.push_back(parse_expr());
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

