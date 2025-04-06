#include "ast.hpp"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <set>
#include <string>
#include <unordered_set>
#include "visitor.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"


/* 
  TODO: we should not have a preprocessor at all,
  and all the conditional compilation stuff should get it's own node
  so that we can do type checking on the constants that #if might use.
  

*/
enum PreprocKind {
  PREPROC_IF,
  PREPROC_IFDEF,
  PREPROC_IFNDEF,
};

static void remove_body(Parser *parser) {
  parser->expect(TType::LCurly);
  int depth = 1;
  while (depth > 0) {
    if (parser->peek().type == TType::LCurly)
      depth++;
    if (parser->peek().type == TType::RCurly)
      depth--;
    if (parser->peek().type == TType::LCurly)
      depth++;
    if (parser->peek().type == TType::RCurly)
      depth--;
    parser->eat();
  }
}

static void remove_preproc(Parser *parser) {
  if (parser->peek().type == TType::If ||
      (parser->peek().type == TType::Identifier && parser->peek().value == "ifdef") ||
      (parser->peek().type == TType::Identifier && parser->peek().value == "ifndef")) {
    while (parser->peek().type != TType::LCurly) {
      parser->eat();
    }
    remove_body(parser);
    if (parser->peek().type == TType::Else) {
      parser->expect(TType::Else);
      remove_preproc(parser);
    }
  } else {
    remove_body(parser);
  }
}

static void parse_ifdef_if_else_preprocs(Parser *parser, ASTStatementList *list, PreprocKind kind) {
  bool executed = false;

  if (kind == PREPROC_IFDEF) { // Handling #ifdef
    auto symbol = parser->expect(TType::Identifier).value;
    executed = parser->ctx.scope->has_def(symbol);
  } else if (kind == PREPROC_IFNDEF) { // Handling #ifndef
    auto symbol = parser->expect(TType::Identifier).value;
    executed = !parser->ctx.scope->has_def(symbol);
  } else if (kind == PREPROC_IF) { // Handling #if
    auto condition = parser->parse_expr();
    auto value = evaluate_constexpr(condition, parser->ctx);
    executed = value.is_truthy();
  } else {
    throw_error("internal compiler error: Invalid #if/#ifdef/#ifndef, "
                "unrecognized kind.",
                {});
  }

  if (executed) {
    parser->expect(TType::LCurly);
    while (parser->peek().type != TType::RCurly) {
      list->statements.push_back(parser->parse_statement());
      while (parser->peek().type == TType::Semi)
        parser->eat();
    }
    parser->expect(TType::RCurly);
  } else {
    remove_body(parser);
  }

  if (parser->peek().type == TType::Else) {
    parser->expect(TType::Else);
    if (!executed) { /* No code has been emitted yet. try again if we can. */
      if (parser->peek().type == TType::If) {
        parser->expect(TType::If);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
      } else if (parser->peek().type == TType::Identifier && parser->peek().value == "ifdef") {
        parser->expect(TType::Identifier);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
      } else if (parser->peek().type == TType::Identifier && parser->peek().value == "ifndef") {
        parser->expect(TType::Identifier);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
      } else {
        parser->expect(TType::LCurly);
        while (parser->peek().type != TType::RCurly) {
          list->statements.push_back(parser->parse_statement());
          while (parser->peek().type == TType::Semi)
            parser->eat();
        }
        parser->expect(TType::RCurly);
      }
    } else { /* No code emitted for this case, eat it up. */
      remove_preproc(parser);
    }
  }
}

// clang-format off
std::vector<DirectiveRoutine> Parser:: directive_routines = {
    // #include
    // Just like C's include, just paste a text file right above where the
    // include is used.
    {.identifier = "include",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto filename = parser->expect(TType::String).value;
        if (!std::filesystem::exists(filename.get_str())) {
          throw_error(std::format("Couldn't find included file: {}, current path: {}", filename,
                                  std::filesystem::current_path().string()),
                      {});
        }
        if (include_set.contains(filename)) {
          return nullptr;
        }
        include_set.insert(filename);
        parser->states.push_back(Lexer::State::from_file(filename.get_str()));
        parser->fill_buffer_if_needed();
        NODE_ALLOC(ASTStatementList, list, range, _, parser)
        while (parser->peek().type != TType::Eof) {
          list->statements.push_back(parser->parse_statement());
        }
        parser->expect(TType::Eof);
        parser->states.pop_back();
        std::filesystem::current_path(parser->states.back().path.parent_path());
        return list;
    }},

    // This is only used for debugging the compiler in rare cases.
    {.identifier = "print",
     .kind = DIRECTIVE_KIND_STATEMENT,
     .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto str = parser->expect(TType::String);
        std::cout << str.value.get_str() << "\n";
        return nullptr;
     }
    },

    // #read
    // Read a file into a string at compile time. Nice for embedding resources
    // into your program.
    // FEATURE: add a binary mode, where its just an array of chars or
    // something.
    {.identifier = "read",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) {
        parser->expect(TType::LParen);
        auto filename = parser->expect(TType::String).value;

        InternedString mode = "text";
        if (parser->peek().type == TType::Comma) {
          parser->eat();
          // could be binary, and whatever other options
          mode = parser->eat().value;
        }
        if (!std::filesystem::exists(filename.get_str())) {
          throw_error(std::format("Couldn't find 'read' file: {}", filename), {});
        }

        parser->expect(TType::RParen);
        NODE_ALLOC(ASTLiteral, string, range, _, parser)
        string->tag = ASTLiteral::String;
        std::stringstream ss;
        if (mode == "binary") {
          std::ifstream isftr(filename.get_str(), std::ios::binary);
          ss << isftr.rdbuf();
          string->value = ss.str();
          return string;
        } else {
          std::ifstream isftr(filename.get_str());
          ss << isftr.rdbuf();
          string->value = ss.str();
          return string;
        }
    }},

    // #test
    // declare a test function. Only gets compiled into --test builds, and
    // produces a test main, a builtin test suite.
    // FEATURE: add categories and extra naming stuff to these. would be nice
    // for
    // filtering etc.
    {.identifier = "test",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        // Issue 2
        // TODO: implement something so we can do
        // * #test(group: "My Test Group", expects: false) *
        // so we can have tests that expect to fail, and so we can use
        // --filter="My Test Group" // run only test group
        // --filter=* - "My Test Group" // run all but test group

        auto name = parser->expect(TType::Identifier);
        parser->expect(TType::DoubleColon);
        auto func = parser->parse_function_declaration(name);
        func->flags |= (int)FunctionInstanceFlags::FUNCTION_IS_TEST;

        if (compile_command.has_flag("test")) {
          return func;
        } else {
          parser->ctx.scope->erase(func->name);
          return ast_alloc<ASTNoop>();
        }
    }},

    // #foreign
    // Declare a foreign function, like C's extern.
    {.identifier = "foreign",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        NODE_ALLOC(ASTFunctionDeclaration, function, range, _, parser)
        auto name = parser->expect(TType::Identifier);
        auto last_func_decl = parser->current_func_decl;
        parser->current_func_decl = function;

        Defer deferred = {[&] { parser->current_func_decl = last_func_decl; }};

        parser->expect(TType::DoubleColon);
        parser->expect(TType::Fn);

        function->params = parser->parse_parameters();
        function->name = name.value;
        if (parser->peek().type != TType::Arrow) {
          function->return_type = ASTType::get_void();
        } else {
          parser->expect(TType::Arrow);
          function->return_type = parser->parse_type();
        }
        function->flags |= FUNCTION_IS_FOREIGN;

        parser->expect(TType::Semi);

        parser->end_node(function, range);
        return function;
    }},

    // #location, for getting source location.
    {
      .identifier = "location",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) {
        auto location = parser->peek().location;
        auto formatted = std::format("{}:{}:{}", SourceLocation::files()[location.file], location.line, location.column);
        auto literal = ast_alloc<ASTLiteral>();
        literal->tag = ASTLiteral::String;
        literal->value = formatted;
        if (parser->peek().type == TType::LogicalNot) {
          literal->is_c_string = true;
          parser->eat();
        }
        return literal;
      }
    },

    // #error, for throwing compiler errors.
    {.identifier = "error",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        auto error = parser->parse_primary();
        if (error->get_node_type() != AST_NODE_LITERAL) {
          throw_error("Can only throw a literal as a error", error->source_range);
        }
        auto literal = static_cast<ASTLiteral *>(error);
        throw_error(literal->value.get_str(), error->source_range);
        return nullptr;
    }},

    // #c_flags, for adding stuff like linker options, -g etc from within
    // your program or header.
    {.identifier = "c_flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto string = parser->expect(TType::String).value;
        if (string == "-g") {
          compile_command.flags[string.get_str()] = true;
        }
        compile_command.add_c_flag(string.get_str());
        return nullptr;
    }},
    
    // #flags, for making an enum declaration auto increment with a flags value.
    // #flags MyEnum :: enum {...};
    {.identifier = "flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto name = parser->expect(TType::Identifier);
        parser->expect(TType::DoubleColon);
        auto enum_decl = parser->parse_enum_declaration(name);

        int index = 0;
        for (auto &key_value : enum_decl->key_values) {
          NODE_ALLOC(ASTLiteral, literal, range, _, parser);
          literal->tag = ASTLiteral::Integer;
          literal->value = std::to_string(1 << index);
          key_value.second = literal;
          index++;
        }

        enum_decl->is_flags = true;
        return enum_decl;
    }},

    // #self, return the type of the current declaring struct or union
    {.identifier = "self",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTType, type, range, defer, parser);
        parser->parse_pointer_extensions(type);
        type->kind = ASTType::SELF;
        parser->append_type_extensions(type);
        return type;
    }},

    // #self, return the type of the current declaring struct or union
    {.identifier = "себя",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTType, type, range, defer, parser);
        parser->parse_pointer_extensions(type);
        type->kind = ASTType::SELF;
        parser->append_type_extensions(type);
        return type;
    }},

    // #anon, for declaring anonymous sub-structs in unions primarily, and anonymous unions within struct declarations.
    {.identifier = "anon",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto tok = parser->expect(TType::DoubleColon);
        if (parser->peek().type == TType::Struct || parser->peek().type == TType::Union) {
          auto decl = parser->parse_struct_declaration(get_unique_identifier());
          auto t = global_get_type(decl->resolved_type);
          auto info = (t->get_info()->as<StructTypeInfo>());
          info->flags |= STRUCT_FLAG_IS_ANONYMOUS;
          return decl;
        } else {
          auto range = parser->begin_node();
          parser->eat();
          parser->end_node(nullptr, range);
          throw_error("Expected struct or union after #anon ::...", range);
          return nullptr;
        }
    }},

    // #export, for exporting a non-mangled name to a dll or C library
    // primarily.
    // Equivalent to marking a function extern "C" in C++.
    {.identifier = "export",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto node = parser->parse_statement();
        if (node->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
          auto struct$ = static_cast<ASTStructDeclaration*>(node);
          struct$->is_extern = true;
        } else if (node->get_node_type() == AST_NODE_VARIABLE) {
          auto decl = static_cast<ASTVariable*>(node);
          decl->is_extern = true;
        } else if (node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
          auto func = static_cast<ASTFunctionDeclaration*>(node);
          func->flags |= FUNCTION_IS_EXPORTED;
        }
        return node;
    }},

    // #typeid, integer version of typeof. can be used to compare types without
    // the pointers.
    {.identifier = "typeid",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTLiteral, literal, range, _, parser)
        parser->expect(TType::LParen);
        auto type = parser->parse_type();
        parser->expect(TType::RParen);
        type->accept(parser->typer);
        literal->tag = ASTLiteral::Integer;
        // TODO: we should move this out of here.
        literal->value = std::to_string(type->resolved_type);
        literal->source_range = type->source_range;
        return literal;
    }},

    // #bitfield, for declaring bitfields. Pretty much only to interop with C:
    // most cases for bitfields are completely useless, and can be replaced with
    // a
    // set of flags.
    {.identifier = "bitfield",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        if (parser->current_struct_decl.is_null()) {
          throw_error("Cannot declare bitfields outside of a struct declaration.", {});
        }
        parser->expect(TType::LParen);
        auto size = parser->expect(TType::Integer);
        parser->expect(TType::RParen);
        ASTVariable *decl = parser->parse_variable();
        decl->is_bitfield = true;
        decl->bitsize = size.value;
        return decl;
    }},

    // #static, used exclusively for static globals, and static locals.
    // We do not support static methods or static members.
    {.identifier = "static",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto statement = parser->parse_statement();
        if (auto decl = dynamic_cast<ASTVariable *>(statement)) {
          decl->is_static = true;
        } else if (auto decl = dynamic_cast<ASTFunctionDeclaration *>(statement)) {
          decl->flags |= FUNCTION_IS_STATIC;
        }
        return statement;
    }},

    // #def, define a compile time flag, like C #define but cannot be a macro.
    {.identifier = "def",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->ctx.scope->add_def(parser->expect(TType::Identifier).value);
        while (parser->peek().type == TType::Semi) parser->eat();
        return ast_alloc<ASTNoop>();
    }},

    // #undef, remove a #def
    {.identifier = "undef",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->ctx.scope->undef(parser->expect(TType::Identifier).value);
        while (parser->peek().type == TType::Semi) parser->eat();
        return ast_alloc<ASTNoop>();
    }},

    // #ifdef, conditional compilation based on a #def being present.
    {.identifier = "ifdef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, range, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
        return list;
    }},

    // #ifndef, conditional compilation based on a #def not being present.
    {.identifier = "ifndef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, range, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
        return list;
    }},

    // #if, conditional compilation based on compile time value.
    {.identifier = "if",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, range, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
        return list;
    }},

    // #region, for named/unnnamed regions. just for organization, has no compilation implications.
    // can have anything between the #region directive and the {} block
    // #region My code region 1 {...} is legal.
    {.identifier = "region",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, range, _, parser)
        while (parser->peek().type != TType::LCurly) {
          parser->eat();
        }
        parser->expect(TType::LCurly);
        while (parser->peek().type != TType::RCurly) {
          list->statements.push_back(parser->parse_statement());
          while (parser->peek().type == TType::Semi) parser->eat();
        }
        parser->expect(TType::RCurly);
        return list;
      }
    },
};
// clang-format on

Nullable<ASTNode> Parser::process_directive(DirectiveKind kind, const InternedString &identifier) {
  auto range = begin_node();
  // compare aganist the kind of the routine with expected type, based on parser
  // location
  for (const auto &routine : directive_routines) {
    if ((routine.kind == DIRECTIVE_KIND_DONT_CARE || routine.kind == kind) && routine.identifier == identifier) {
      auto result = routine.run(this);
      end_node(result.get(), range);
      return result;
    }
  }
  throw_error(std::format("failed to call unknown directive routine, or an expression "
                          "routine was used as a statement, or vice versa: {}",
                          identifier),
              range);
  return nullptr;
}

ASTProgram *Parser::parse_program() {
  NODE_ALLOC(ASTProgram, program, range, _, this)

  // put bootstrap on root scope
  if (true) {
    import("bootstrap", &ctx.root_scope);

    while (peek().type != TType::Eof) {
      program->statements.push_back(parse_statement());
    }
    expect(TType::Eof);
    states.pop_back();
    std::filesystem::current_path(states.back().path.parent_path());
  }

  program->end_of_bootstrap_index = program->statements.size();

  // put the rest on the program scope
  ctx.set_scope();
  program->scope = ctx.scope;
  while (true) {
    while (semicolon()) {
      eat();
    }
    if (peek().is_eof()) {
      break;
    }

    if (peek().type == TType::Directive) {
      eat();
      InternedString identifier = eat().value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifier);
      if (result.is_not_null()) {
        auto statement = static_cast<ASTStatement *>(result.get());
        program->statements.push_back(statement);
      }
      if (semicolon()) {
        eat();
      }
      continue;
    }

    auto statement = parse_statement();

    auto type = statement->get_node_type();
    switch (type) {
      case AST_NODE_STRUCT_DECLARATION:
      case AST_NODE_FUNCTION_DECLARATION:
      case AST_NODE_INTERFACE_DECLARATION:
      case AST_NODE_ENUM_DECLARATION:
      case AST_NODE_CHOICE_DECLARATION:
      case AST_NODE_ALIAS:
      case AST_NODE_VARIABLE:
      case AST_NODE_NOOP:
      case AST_NODE_IMPL:
      case AST_NODE_IMPORT:
      case AST_NODE_MODULE:
        break;
      default:
      err:
        throw_error("Statement not allowed at the top-level of a program", statement->source_range);
    }

    program->statements.push_back(statement);
  }
  end_node(program, range);
  return program;
}

ASTArguments *Parser::parse_arguments() {
  NODE_ALLOC(ASTArguments, args, range, _, this)
  expect(TType::LParen);
  while (peek().type != TType::RParen) {
    args->arguments.push_back(parse_expr());
    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    }
  }
  expect(TType::RParen);
  end_node(args, range);
  return args;
}

ASTCall *Parser::parse_call(ASTExpr *function) {
  NODE_ALLOC(ASTCall, call, range, _, this);
  call->function = function;
  if (function->get_node_type() == AST_NODE_DOT_EXPR) {
    throw_error("dot exr", range);
  }
  call->arguments = parse_arguments();
  end_node(call, range);
  return call;
}

ASTExpr *Parser::parse_expr(Precedence precedence) {
  ASTExpr *left = parse_unary();
  while (true) {
    auto op = peek();

    /* Pattern matching / destructuring Choice types. */
    if (op.type == TType::Is) {
      NODE_ALLOC(ASTPatternMatch, pattern_match, range, defer, this);
      eat();
      auto target_type = parse_path();

      if (peek().type == TType::LCurly) {
        eat();
        pattern_match->pattern_tag = ASTPatternMatch::STRUCT;
        while (peek().type != TType::RCurly) {
          StructPattern::Part part;
          part.field_name = expect(TType::Identifier).value;
          part.mutability = CONST;
          expect(TType::Colon);
          if (peek().type == TType::Mut) {
            eat();
            part.mutability = MUT;
          }
          part.var_name = expect(TType::Identifier).value;
          pattern_match->struct_pattern.parts.push_back(part);

          if (peek().type != TType::RCurly) {
            expect(TType::Comma);
          }
        }
        expect(TType::RCurly);
      } else if (peek().type == TType::LParen) {
        eat();
        pattern_match->pattern_tag = ASTPatternMatch::TUPLE;
        while (peek().type != TType::RParen) {
          TuplePattern::Part part;
          part.mutability = CONST;
          if (peek().type == TType::Mut) {
            eat();
            part.mutability = MUT;
          }
          part.var_name = expect(TType::Identifier).value;
          pattern_match->tuple_pattern.parts.push_back(part);
          if (peek().type != TType::RParen) {
            expect(TType::Comma);
          }
        }
        expect(TType::RParen);
      }

      return pattern_match;
    }

    Precedence token_precedence = get_operator_precedence(peek());

    if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GT) {
      token_precedence = PRECEDENCE_SHIFT;
    }

    if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GE) {
      token_precedence = PRECEDENCE_ASSIGNMENT;
    }

    if (token_precedence <= precedence)
      break;

    if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GT) {
      op.type = TType::SHR;
      op.value = ">>";
      eat();
      eat();
    } else if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GE) {
      op.type = TType::CompSHR;
      op.value = ">>=";
      eat();
      eat();
    } else {
      eat();
    }

    ASTBinExpr *binexpr = ast_alloc<ASTBinExpr>();
    binexpr->source_range = left->source_range;
    auto right = parse_expr(token_precedence);

    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  return left;
}

ASTExpr *Parser::parse_unary() {
  // bitwise not is a unary expression because arrays use it as a pop operator,
  // and sometimes you might want to ignore it's result.
  if (peek().type == TType::Add || peek().type == TType::Sub || peek().type == TType::LogicalNot ||
      peek().type == TType::Not || peek().type == TType::Increment || peek().type == TType::Decrement ||
      peek().type == TType::Mul || peek().type == TType::And || peek().type == TType::Not) {
    NODE_ALLOC(ASTUnaryExpr, unaryexpr, range, _, this)
    auto op = eat();

    Mutability mutability;
    if (op.type == TType::And) {
      if (peek().type == TType::Mut) {
        mutability = MUT;
        eat();
      } else if (peek().type == TType::Const) {
        mutability = CONST;
        eat();
      } else {
        end_node(unaryexpr, range);
        throw_error("taking an address-of a variable requires '&mut/&const' to determine the mutability of the pointer",
                    unaryexpr->source_range);
      }
    }

    auto expr = parse_unary();

    // TODO: make a more comprehensive rvalue evaluator.
    // We need to use it later for self* method calls
    auto is_rvalue =
        expr->get_node_type() == AST_NODE_LITERAL || (expr->get_node_type() == AST_NODE_CALL && op.type == TType::And);

    if ((is_rvalue) && (op.type == TType::And || op.type == TType::Mul)) {
      end_node(nullptr, range);
      throw_error("Cannot take the address of, or dereference a literal or "
                  "'rvalue'\nlike a function result or constructor result. It "
                  "is a temporary value\nand would be invalid once this "
                  "statement completed executing",
                  range);
    }

    unaryexpr->mutability = mutability;
    unaryexpr->op = op;
    unaryexpr->operand = expr;
    end_node(unaryexpr, range);
    return unaryexpr;
  }

  return parse_postfix();
}

ASTPath::Segment Parser::parse_path_segment() {
  InternedString identifier = expect(TType::Identifier).value;
  if (peek().type == TType::GenericBrace) {
    auto generics = parse_generic_arguments();
    return {identifier, generics};
  } else {
    return {identifier, std::vector<ASTExpr*>{}};
  }
}

ASTPath *Parser::parse_path() {
  NODE_ALLOC(ASTPath, path, range, defer, this);
  // If we find a double colon, then we continue.
  while (true) {
    path->segments.push_back(parse_path_segment());
    if (peek().type == TType::DoubleColon) {
      eat();
    } else {
      break;
    }
  }
  return path;
}

ASTExpr *Parser::parse_postfix() {
  auto left = parse_primary();
  auto range = begin_node();
  // build dot and subscript expressions
  while (peek().type == TType::Dot || peek().type == TType::LBrace || peek().type == TType::LParen ||
         peek().type == TType::Increment || peek().type == TType::Decrement || peek().type == TType::Range ||
         peek().type == TType::As) {
    if (peek().type == TType::LParen) {
      left = parse_call(left);
    } else if (peek().type == TType::Dot) {
      NODE_ALLOC(ASTDotExpr, dot, range, _, this)
      eat();
      dot->base = left;
      if (peek().type == TType::Integer) {
        dot->member = ASTPath::Segment{eat().value};
      } else if (peek().type == TType::Identifier) {
        dot->member = parse_path_segment();
      } else if (peek().type == TType::LCurly || peek().type == TType::LBrace) {
        // .{} initializer lists.
        auto path = dynamic_cast<ASTPath*>(left);
        if (!path) {
          throw_error("can only use an initializer list on a path, e.g 's32, List!<s32>, std::fmt::formatter!<s32>, etc.", left->source_range);
        }
        if (peek().type == TType::LCurly) {
          eat(); // eat {
          NODE_ALLOC(ASTInitializerList, init_list, range, _, this)
          init_list->target_type = ast_alloc<ASTType>();
          init_list->target_type.get()->normal.path = path;
          if (peek().type == TType::RCurly) {
            init_list->tag = ASTInitializerList::INIT_LIST_EMPTY;
          } else {
            init_list->tag = ASTInitializerList::INIT_LIST_NAMED;
            while (peek().type != TType::RCurly) {
              auto identifier = expect(TType::Identifier).value;
              expect(TType::Colon);
              init_list->key_values.push_back({identifier, parse_expr()});
              if (peek().type == TType::Comma) {
                eat();
              }
            }
          }
          expect(TType::RCurly);
          end_node(init_list, range);
          return init_list;
        } else if (peek().type == TType::LBrace) {
          eat(); // eat [
          NODE_ALLOC(ASTInitializerList, init_list, range, _, this)
          init_list->target_type = ast_alloc<ASTType>();
          init_list->target_type.get()->normal.path = path;
          init_list->tag = ASTInitializerList::INIT_LIST_COLLECTION;
          while (peek().type != TType::RBrace) {
            init_list->values.push_back(parse_expr());
            if (peek().type == TType::Comma) {
              eat();
            }
          }
          expect(TType::RBrace);
          end_node(init_list, range);
          return init_list;
        }
      } else {
        end_node(left, range);
        throw_error("Invalid dot expression right hand side: expected a member name, or for a tuple, an index.", range);
      }

      if (peek().type == TType::LParen) {
        NODE_ALLOC(ASTMethodCall, method, range, defer, this);
        method->dot = dot;
        method->arguments = parse_arguments();
        left = method;
      } else {
        left = dot;
      }

    } else if (peek().type == TType::Increment || peek().type == TType::Decrement) {
      NODE_ALLOC(ASTUnaryExpr, unary, unary_range, _, this)
      unary->operand = left;
      unary->op = eat();
      return unary;
    } else if (peek().type == TType::LBrace) {
      NODE_ALLOC(ASTSubscript, subscript, range, _, this)
      subscript->left = left;
      eat();
      subscript->subscript = parse_expr();
      expect(TType::RBrace);
      left = subscript;
    } else if (peek().type == TType::Range) {
      NODE_ALLOC(ASTRange, node, range, _, this)
      eat();
      node->right = parse_expr();
      node->left = left;
      return node;
    } else if (peek().type == TType::As) {
      NODE_ALLOC(ASTCast, node, range, _, this)
      eat();
      node->target_type = parse_type();
      node->expression = left;
      left = node;
    }
  }

  end_node(left, range);
  return left;
}

ASTExpr *Parser::parse_primary() {
  auto tok = peek();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  // .{} initializer lists.
  if (tok.type == TType::Dot) {
    eat(); // eat .
    if (peek().type == TType::LCurly) {
      eat(); // eat {
      NODE_ALLOC(ASTInitializerList, init_list, range, _, this)
      if (peek().type == TType::RCurly) {
        init_list->tag = ASTInitializerList::INIT_LIST_EMPTY;
      } else {
        init_list->tag = ASTInitializerList::INIT_LIST_NAMED;
        while (peek().type != TType::RCurly) {
          auto identifier = expect(TType::Identifier).value;
          expect(TType::Colon);
          init_list->key_values.push_back({identifier, parse_expr()});
          if (peek().type == TType::Comma) {
            eat();
          }
        }
      }
      expect(TType::RCurly);
      end_node(init_list, range);
      return init_list;
    } else if (peek().type == TType::LBrace) {
      eat(); // eat [
      NODE_ALLOC(ASTInitializerList, init_list, range, _, this)
      init_list->tag = ASTInitializerList::INIT_LIST_COLLECTION;
      while (peek().type != TType::RBrace) {
        init_list->values.push_back(parse_expr());
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RBrace);
      end_node(init_list, range);
      return init_list;
    }
  }

  switch (tok.type) {
    case TType::Dyn_Of: {
      expect(TType::Dyn_Of);
      NODE_ALLOC(ASTDyn_Of, dyn_of, range, _, this)
      expect(TType::LParen);
      dyn_of->object = parse_expr();

      if (peek().type == TType::Comma) {
        expect(TType::Comma);
        dyn_of->interface_type = parse_type();
      }

      expect(TType::RParen);
      return dyn_of;
    }
    case TType::Type_Of: {
      expect(TType::Type_Of);
      NODE_ALLOC(ASTType_Of, type_of, range, _, this)
      expect(TType::LParen);
      type_of->target = parse_type();
      expect(TType::RParen);
      return type_of;
    }
    case TType::Size_Of: {
      NODE_ALLOC(ASTSize_Of, node, range, _, this);
      eat();
      expect(TType::LParen);
      node->target_type = parse_type();
      expect(TType::RParen);
      return node;
    }
    case TType::Fn: {
      return parse_lambda();
    }
    case TType::Switch: {
      NODE_ALLOC(ASTSwitch, node, range, _, this)
      expect(TType::Switch);
      node->target = parse_expr();
      expect(TType::LCurly);
      while (peek().type != TType::RCurly) {
        if (peek().type == TType::Else) {
          eat();
          if (peek().type != TType::ExpressionBody) {
            expect(TType::Colon);
          }
          node->default_case = parse_block();
          if (peek().type == TType::Comma)
            eat();
          continue;
        }
        SwitchCase _case{
            .expression = parse_expr(),
        };
        if (peek().type != TType::ExpressionBody) {
          expect(TType::Colon);
        }
        _case.block = parse_block();
        node->cases.push_back(_case);
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RCurly);
      return node;
    }
    case TType::Char: {
      NODE_ALLOC(ASTLiteral, node, range, _, this)
      eat();
      node->tag = ASTLiteral::Char;
      node->value = tok.value;
      end_node(node, range);
      return node;
    }
    case TType::Identifier: {
      return parse_path();
    }
    case TType::Null: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::Null;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::True: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::False: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Integer: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::Integer;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Float: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::Float;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::String: {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      eat();
      literal->tag = ASTLiteral::String;
      literal->value = tok.value;
      if (peek().type == TType::Identifier && peek().value == "c") {
        literal->is_c_string = true;
        eat();
      }
      end_node(literal, range);
      return literal;
    }
    case TType::LParen: {
      auto range = begin_node();
      expect(TType::LParen); // (

      if (peek().type == TType::RParen) {
        NODE_ALLOC(ASTTuple, tuple, range, _, this);
        eat();
        tuple->values = {};
        return tuple;
      }

      auto expr = parse_expr();
      if (peek().type == TType::Comma) {
        ASTTuple *tuple = ast_alloc<ASTTuple>();
        Defer _([&] { this->end_node(tuple, range); });
        tuple->values.push_back(expr);
        eat();
        while (peek().type != TType::RParen) {
          tuple->values.push_back(parse_expr());
          if (peek().type == TType::Comma)
            eat();
        }
        expect(TType::RParen);
        return tuple;
      }
      if (peek().type != TType::RParen) {
        eat();
        end_node(nullptr, range);
        throw_error("Expected ')'", range);
      }
      eat();
      end_node(expr, range);

      if (expr->get_node_type() == AST_NODE_TYPE) {
        throw_error("using (TYPE)expr style casts are deprecated. use `expr as TYPE` syntax", range);
      }

      return expr;
    }
    default: {
      auto error_range = begin_node();
      throw_error(
          std::format("Invalid primary expression. Token: '{}'... Type: '{}'", tok.value, TTypeToString(tok.type)),
          error_range);
      return nullptr;
    }
  }
}

ASTType *Parser::parse_type() {
  if (peek().type == TType::Fn) {
    return parse_function_type();
  }

  NODE_ALLOC(ASTType, node, range, _, this)
  parse_pointer_extensions(node);

  if (peek().type == TType::Dyn) {
    node->normal.is_dyn = true;
    eat();
  }

  if (peek().type == TType::LParen) {
    node->resolved_type = Type::INVALID_TYPE_ID;
    node->kind = ASTType::TUPLE;
    eat();
    while (peek().type != TType::RParen) {
      node->tuple_types.push_back(parse_type());
      if (peek().type == TType::Comma)
        eat();
    }
    expect(TType::RParen);
    append_type_extensions(node);
    return node;
  }

  // parse #self types.
  if (peek().type == TType::Directive) {
    auto range = begin_node();
    auto expr = try_parse_directive_expr().get();
    if (expr->get_node_type() != AST_NODE_TYPE) {
      throw_error("unable to get type from directive expression where a type "
                  "was expected.",
                  range);
    }
    auto type = static_cast<ASTType *>(expr);
    type->extensions.insert(type->extensions.begin(), node->extensions.begin(), node->extensions.end());
    return type;
  }

  node->kind = ASTType::NORMAL;
  node->normal.path = parse_path();
  node->normal.path->source_range = range;

  append_type_extensions(node);

  end_node(node, range);
  return node;
}

ASTStatement *Parser::parse_statement() {
  auto parent_range = begin_node();

  auto tok = peek();

  while (tok.type == TType::Semi) {
    eat();
    tok = peek();
  }

  if (tok.type == TType::Attribute) {
    std::vector<Attribute> attributes;
    {
      eat();
      expect(TType::LBrace);
      while (peek().type != TType::RBrace) {
        Attribute attribute;
        if (peek().type == TType::Impl) {
          eat();
          attribute.tag = ATTRIBUTE_IMPL;
          expect(TType::LParen);
          while (peek().type != TType::RParen) {
            attribute.arguments.push_back(parse_type());
            if (peek().type != TType::RParen)
              expect(TType::Comma);
          }
          expect(TType::RParen);
        } else if (peek().type == TType::Const) {
          eat();
          attribute.tag = ATTRIBUTE_CONST;
        } else {
          auto ident = expect(TType::Identifier).value.get_str();
          if (ident == "pub") {
            attribute.tag = ATTRIBUTE_PUB;
          } else if (ident == "foreign") {
            attribute.tag = ATTRIBUTE_FOREIGN;
            if (peek().type == TType::LParen) {
              eat();
              // expect a string literal.
              attribute.arguments.push_back(parse_primary());
              expect(TType::RParen);
            }
          } else if (ident == "entry") {
            attribute.tag = ATTRIBUTE_ENTRY;
          } else if (ident == "impl") {
            attribute.tag = ATTRIBUTE_IMPL;
          } else if (ident == "export") {
            attribute.tag = ATTRIBUTE_EXPORT;
          } else if (ident == "no_mangle") {
            attribute.tag = ATTRIBUTE_NO_MANGLE;
          } else if (ident == "inline") {
            attribute.tag = ATTRIBUTE_INLINE;
          } else {
            throw_error(std::format("invalid attribute {}", ident), {peek().location});
          }
        }
        if (peek().type != TType::RBrace) {
          expect(TType::Comma);
        }
        attributes.push_back(std::move(attribute));
      }
      expect(TType::RBrace);
    }

    if (peek().type == TType::Attribute) {
      throw_error("doubling up attributes declarations leads to an overwrite. just use one @[...]", {peek().location});
    }

    auto statement = parse_statement();
    statement->attributes = std::move(attributes);
    return statement;
  }

  if (tok.type == TType::Const) {
    eat();
    auto variable = parse_variable();
    variable->is_constexpr = true;
    ctx.scope->insert_variable(variable->name, -1, variable->value.get(), CONST);
    return variable;
  }

  if (tok.type == TType::Directive && (lookahead_buf()[1].value == "self" || lookahead_buf()[1].value == "себя")) {
    NODE_ALLOC(ASTExprStatement, statment, range, _, this)
    statment->expression = parse_expr();
    end_node(statment, range);
    return statment;
  }

  // * '#' Directives.
  if (tok.type == TType::Directive) {
    auto range = begin_node();
    eat();
    auto directive_name = eat().value;
    auto statement = dynamic_cast<ASTStatement *>(process_directive(DIRECTIVE_KIND_STATEMENT, directive_name).get());

    if (!statement) {
      static auto noop = ast_alloc<ASTNoop>();
      statement = noop;
    }

    end_node(statement, range);
    return statement;
  }

  if (tok.type == TType::Import) {
    expect(TType::Import);
    NODE_ALLOC(ASTImport, import, range, _, this);
    auto module_name = expect(TType::Identifier).value;

    import->module_name = module_name;
    if (peek().type == TType::DoubleColon && lookahead_buf()[1].type == TType::Mul) {
      eat();
      eat();
      import->tag = ASTImport::IMPORT_ALL;
    } else if (peek().type == TType::DoubleColon && lookahead_buf()[1].type == TType::LCurly) {
      eat();
      eat();
      while (peek().type != TType::RCurly) {
        // TODO: support :: imports here too, like
        // import mod::{submod::symbol, submod::submod::symbol};
        import->symbols.push_back(expect(TType::Identifier).value);
        if (peek().type != TType::RCurly) {
          expect(TType::Comma);
        }
      }
      expect(TType::RCurly);
      import->tag = ASTImport::IMPORT_NAMED;
    } else if (peek().type == TType::DoubleColon && lookahead_buf()[1].type == TType::Identifier) {
      // TODO: support :: imports here also, like
      // import mod::submod::symbol; etc
      eat();
      import->symbols.push_back(expect(TType::Identifier).value);
      import->tag = ASTImport::IMPORT_NAMED;
    }

    expect(TType::Semi);

    auto symbol = ctx.scope->lookup(module_name);

    // Import a module that's been defined by a 'module ... {}' statement.
    // this is somewhat problematic because these arent really yet.
    if (symbol && symbol->is_module()) {
      // printf("importing existing module %s\n", module_name.get_str().c_str());
      import->scope = ((ASTModule *)symbol->module.declaration)->scope;
      return import;
    }

    auto old_scope = ctx.scope;
    // TODO: fix the wasteful double allocation that may happen here.
    ctx.set_scope(import->scope = create_child(ctx.root_scope));

    if (this->import(module_name.get_str(), &import->scope)) {
      while (!peek().is_eof()) {
        import->statements.push_back(parse_statement());
      }
      expect(TType::Eof);
      states.pop_back();
      std::filesystem::current_path(states.back().path.parent_path());
    }

    old_scope->create_module(module_name, import);
    ctx.set_scope(old_scope);

    return import;
  }

  if (tok.type == TType::Module) {
    NODE_ALLOC(ASTModule, module, range, _, this);
    eat();
    module->module_name = expect(TType::Identifier).value;
    expect(TType::LCurly);
    auto old_scope = ctx.scope;
    ctx.set_scope();
    module->scope = ctx.scope;
    while (peek().type != TType::RCurly) {
      module->statements.push_back(parse_statement());
    }
    ctx.set_scope(old_scope);
    expect(TType::RCurly);
    return module;
  }

  if (peek().type == TType::Defer) {
    return parse_defer();
  }

  if (peek().type == TType::Impl) {
    return parse_impl();
  }

  if (peek().type == TType::Alias) {
    eat();
    NODE_ALLOC(ASTAlias, alias, range, _, this)
    if (peek().type == TType::GenericBrace) {
      alias->generic_parameters = parse_generic_parameters();
    }
    alias->name = expect(TType::Identifier).value;
    expect(TType::DoubleColon);
    if (peek().type == TType::Mul || peek().type == TType::Fn || lookahead_buf()[1].type == TType::GenericBrace) {
      alias->source_node = parse_type();
    } else {
      alias->source_node = parse_type();
    }
    return alias;
  }

  bool is_const_multiple_assign = (tok.type == TType::Identifier && lookahead_buf()[1].type == TType::Comma) ||
                                  (tok.type == TType::Mul && lookahead_buf()[1].type == TType::Identifier &&
                                   lookahead_buf()[2].type == TType::Comma);

  bool is_mut_multiple_assign =
      (tok.type == TType::Mut && lookahead_buf()[1].type == TType::Identifier &&
       lookahead_buf()[2].type == TType::Comma) ||
      (tok.type == TType::Mut && lookahead_buf()[1].type == TType::Mul &&
       lookahead_buf()[1].type == TType::Identifier && lookahead_buf()[3].type == TType::Comma);

  if (is_const_multiple_assign || is_mut_multiple_assign) {
    return parse_multiple_asssignment();
  }

  // * Variable declarations
  // * 'n := 10;'
  // * 'n : int = 10;'
  // * 'const_n :: 10;' (remove me from here)
  // TODO: this condition seems excessively complicated.

  bool is_colon_or_colon_equals = tok.type == TType::Identifier && (lookahead_buf()[1].type == TType::Colon ||
                                                                    lookahead_buf()[1].type == TType::ColonEquals);

  bool is_mut_decl = tok.type == TType::Mut && lookahead_buf()[1].type == TType::Identifier &&
                     (lookahead_buf()[2].type == TType::Colon || lookahead_buf()[2].type == TType::ColonEquals);

  if (is_mut_decl || is_colon_or_colon_equals) {
    auto decl = parse_variable();
    return decl;
  }

  // * Control flow
  {
    if (tok.type == TType::LCurly) {
      auto block = parse_block();
      return block;
    }

    if (tok.type == TType::Return) {
      NODE_ALLOC(ASTReturn, return_node, range, _, this)
      expect(TType::Return);
      if (peek().type != TType::Semi) {
        return_node->expression = parse_expr();
      }
      end_node(return_node, range);
      return return_node;
    }

    if (tok.type == TType::Break) {
      NODE_ALLOC(ASTBreak, _break, range, _, this)
      eat();
      end_node(_break, range);
      return _break;
    }

    if (tok.type == TType::Continue) {
      NODE_ALLOC(ASTContinue, _continue, range, _, this)
      eat();
      end_node(_continue, range);
      return _continue;
    }

    if (tok.type == TType::For) {
      NODE_ALLOC(ASTFor, node, range, _, this)
      eat();

      // Parse the variables in the for loop
      std::vector<Destructure> destructure;

      while (true) {
        Destructure destruct;
        if (peek().type == TType::Mul) {
          destruct.semantic = ValueSemantic::VALUE_SEMANTIC_POINTER;
          eat();
        } else {
          destruct.semantic = ValueSemantic::VALUE_SEMANTIC_COPY;
        }

        auto identifier = expect(TType::Identifier).value;
        destruct.identifier = identifier;
        destructure.push_back(destruct);

        if (peek().type == TType::Comma) {
          eat();
        } else {
          break;
        }
      }

      // Set the left_tag and left union based on the parsed variables
      if (destructure.size() > 1) {
        node->left_tag = ASTFor::DESTRUCTURE;
        node->left.destructure = destructure;
      } else {
        node->left_tag = ASTFor::IDENTIFIER;
        node->left.identifier = destructure[0].identifier;

        if (destructure[0].semantic == VALUE_SEMANTIC_POINTER) {
          end_node(node, range);
          throw_error("you can only take the elements of a tuple destructure as a pointer, 'for *v in ...' is "
                      "redundant. just use the correct iterator, such as '.iter_mut()'",
                      node->source_range);
        }
      }

      // Ensure the 'in' keyword is present
      if (peek().type == TType::In) {
        eat();
        auto expr = parse_expr();
        node->right = expr;
      } else {
        throw_error("Invalid for syntax. expected 'for i in 0..10 || for elem in iterable || for *elem in iterable'",
                    range);
      }

      node->block = parse_block();
      end_node(node, range);
      return node;
    }

    if (tok.type == TType::While) {
      NODE_ALLOC(ASTWhile, node, range, _, this)
      eat();
      if (peek().type != TType::LCurly) {
        node->condition = parse_expr();
      }
      node->block = parse_block();
      end_node(node, range);
      return node;
    }

    // TODO: we should handle the 'then' statement more gracefully.
    // Also, => is super fricken janky, and is really poorly implemented.
    if (tok.type == TType::If) {
      NODE_ALLOC(ASTIf, node, range, _, this)
      eat();
      node->condition = parse_expr();

      if (peek().type == TType::Then) {
        NODE_ALLOC(ASTBlock, block, _range, defer, this);
        eat();
        node->block = block;
        ctx.set_scope();
        auto statement = parse_statement();
        node->block->statements = {statement};
        if (statement->get_node_type() == AST_NODE_VARIABLE) {
          throw_warning(WarningInaccessibleDeclaration, "Inaccesible declared variable", statement->source_range);
        }
        node->block->scope = ctx.exit_scope();
      } else {
        node->block = parse_block();
      }

      if (peek().type == TType::Else) {
        NODE_ALLOC(ASTElse, node_else, range, _, this)
        eat();
        if (peek().type == TType::If) {
          auto inner_if = parse_statement();
          assert(inner_if->get_node_type() == AST_NODE_IF);
          node_else->_if = static_cast<ASTIf *>(inner_if);
        } else {
          node_else->block = parse_block();
        }
        node->_else = node_else;
      }
      end_node(node, range);
      return node;
    }
  }

  // * Type declarations.
  // * Todo: handle constant 'CONST :: VALUE' Declarations here.
  if (lookahead_buf()[1].type == TType::DoubleColon && lookahead_buf()[2].family == TFamily::Keyword) {
    expect(TType::Identifier);
    expect(TType::DoubleColon);
    if (peek().type == TType::Fn) {
      auto node = parse_function_declaration(tok);
      return node;
    }
    if (peek().type == TType::Interface) {
      auto interface = parse_interface_declaration(tok);
      return interface;
    }
    if (peek().type == TType::Struct || peek().type == TType::Union) {
      auto struct_decl = parse_struct_declaration(tok);
      return struct_decl;
    }
    if (peek().type == TType::Enum) {
      auto enum_decl = parse_enum_declaration(tok);
      return enum_decl;
    }
    if (peek().type == TType::Choice) {
      eat();
      auto tagged_union = parse_tagged_union_declaration(tok);
      return tagged_union;
    }
  } else if (lookahead_buf()[1].type == TType::DoubleColon) {
    NODE_ALLOC(ASTExprStatement, expr, range, defer, this);
    expr->expression = parse_expr();
    return expr;
  }

  // * Expression statements.
  {
    auto next = lookahead_buf()[1];
    auto next_next = lookahead_buf()[2];
    // Both postfix and prefix (inc/dec)rement
    const bool is_increment_or_decrement =
        (next.type == TType::Increment || next.type == TType::Decrement && next_next.type != TType::Semi) ||
        tok.type == TType::Increment || tok.type == TType::Decrement;

    // subscript assignment or dot assign/ call statement.
    const bool is_identifier_with_lbrace_or_dot =
        tok.type == TType::Identifier && (next.type == TType::LBrace || next.type == TType::Dot);

    const bool is_call = next.type == TType::LParen || next.type == TType::GenericBrace;

    const bool is_assignment_or_compound = next.type == TType::Assign || next.type == TType::Comma ||
                                           next.is_comp_assign() ||
                                           (tok.type == TType::Identifier && next.is_relational());

    // .2 != comma for tuple destrucutre.
    const bool is_deref = tok.type == TType::Mul && lookahead_buf()[2].type != TType::Comma;

    const bool is_special_case = tok.type == TType::LParen || // possible parenthesized dereference or something.
                                 tok.type == TType::Switch;

    if (is_call || is_increment_or_decrement || is_identifier_with_lbrace_or_dot || is_assignment_or_compound ||
        is_deref || is_special_case) {
      NODE_ALLOC(ASTExprStatement, statement, range, _, this)
      statement->expression = parse_expr();
      if (ASTSwitch *_switch = dynamic_cast<ASTSwitch *>(statement->expression)) {
        _switch->is_statement = true;
      } else {
        expect(TType::Semi);
      }
      end_node(statement, range);
      return statement;
    }
  }

  end_node(nullptr, parent_range);

  //*  Failure to parse errors

  {
    if (tok.family == TFamily::Operator) {
      throw_error(std::format("Unexpected operator: {} '{}'", TTypeToString(tok.type), tok.value), parent_range);
    }

    if (tok.family == TFamily::Literal) {
      eat();
      throw_error(std::format("Unexpected literal: {} .. {}", tok.value, TTypeToString(tok.type)), parent_range);
    }

    if (tok.family == TFamily::Keyword) {
      eat();
      throw_error(std::format("Unexpected keyword: {}", tok.value), parent_range);
    }

    if (ctx.scope->lookup(tok.value)) {
      eat();
      throw_error(std::format("Unexpected variable {}", tok.value), parent_range);
    }

    if (ctx.scope->find_type_id(tok.value, {}) == Type::INVALID_TYPE_ID) {
      eat();
      throw_error(std::format("Use of an undeclared type or identifier: {}", tok.value), parent_range);
    }

    eat();
    throw_error(std::format("Unexpected token when parsing statement: {}.. This "
                            "is likely an undefined type.",
                            tok.value),
                parent_range);
    exit(1);
  }
}

ASTTupleDeconstruction *Parser::parse_multiple_asssignment() {
  NODE_ALLOC(ASTTupleDeconstruction, node, range, _, this)

  auto parse_destructure = [this]() -> Destructure {
    Destructure destruct;
    destruct.mutability = CONST;

    if (peek().type == TType::Mut) {
      destruct.mutability = MUT;
      eat();
    }

    if (peek().type == TType::Mul) {
      destruct.semantic = VALUE_SEMANTIC_POINTER;
      eat();
    } else {
      destruct.semantic = VALUE_SEMANTIC_COPY;
    }

    return destruct;
  };

  auto destruct = parse_destructure();
  destruct.identifier = expect(TType::Identifier).value;
  node->elements.push_back(destruct);

  while (peek().type == TType::Comma) {
    eat();
    auto destruct = parse_destructure();
    destruct.identifier = expect(TType::Identifier).value;
    node->elements.push_back(destruct);
  }

  if (peek().type == TType::ColonEquals || peek().type == TType::Assign) {
    node->op = eat().type;
    node->right = parse_expr();
  } else {
    // TODO: allow typed tuple deconstructions.
    end_node(nullptr, range);
    throw_error("Currently, you cannot have an explicitly typed tuple deconstruction. Use a, b, c := ....", range);
  }

  end_node(node, range);

  for (const auto &destruct : node->elements) {
    auto symbol = ctx.scope->local_lookup(destruct.identifier);
    if (node->op == TType::ColonEquals) {
      if (symbol)
        throw_error("redefinition of a variable, tuple deconstruction with := doesn't allow redeclaration of any of "
                    "the identifiers",
                    node->source_range);
      ctx.scope->insert_variable(destruct.identifier, Type::INVALID_TYPE_ID, nullptr, destruct.mutability);
    } else {
      // TODO: reimplement this error in a sane way.
      if (!symbol)
        throw_error("use of an undeclared variable, tuple deconstruction with = requires all identifiers already exist",
                    node->source_range);

      ctx.scope->insert_variable(destruct.identifier, Type::INVALID_TYPE_ID, nullptr, destruct.mutability);
    }
  }

  return node;
}

ASTVariable *Parser::parse_variable() {
  NODE_ALLOC(ASTVariable, decl, range, _, this);
  if (current_func_decl) {
    decl->is_local = true;
  }
  if (peek().type == TType::Mut) {
    eat();
    decl->mutability = MUT;
  }
  auto iden = eat();
  decl->name = iden.value;

  if (ctx.scope->find_type_id(iden.value, {}) != Type::INVALID_TYPE_ID || keywords.contains(iden.value.get_str())) {
    end_node(nullptr, range);
    throw_error("Invalid variable declaration: a type or keyword exists with "
                "that name,",
                range);
  }

  if (peek().type == TType::Colon) {
    expect(TType::Colon);
    decl->type = parse_type();
    if (peek().type == TType::Assign) {
      eat();
      auto expr = parse_expr();
      decl->value = expr;
    }
  } else {
    expect(TType::ColonEquals);
    decl->value = parse_expr();
  }

  end_node(decl, range);
  return decl;
}

ASTBlock *Parser::parse_block(Scope *scope) {
  auto last_block = current_block;
  NODE_ALLOC_EXTRA_DEFER(ASTBlock, block, range, _, this, current_block = last_block);
  current_block = block;

  ctx.set_scope(scope);

  if (peek().type == TType::ExpressionBody) {
    NODE_ALLOC(ASTReturn, $return, range, _, this);
    expect(TType::ExpressionBody);
    $return->expression = parse_expr();
    block->statements = {$return};
    block->scope = ctx.exit_scope(); // we do this, even though it owns no scope, because it would get created later
                                     // anyway when entering it.
    if (peek().type == TType::Semi)
      eat();
    return block;
  }

  expect(TType::LCurly);

  while (peek().type != TType::RCurly) {
    if (peek().type == TType::Eof) {
      end_node(nullptr, range);
      throw_error("Imbalanced '{' and '}'", range);
    }
    auto statement = parse_statement();

    if (statement->get_node_type() == AST_NODE_DEFER) {
      if (current_func_decl.get()) {
        current_func_decl.get()->has_defer = true;
      } else {
        throw_error("You can only use defer within a function scope", statement->source_range);
      }
      block->has_defer = true;
      block->defer_count++;
    }

    block->statements.push_back(statement);
    while (semicolon())
      eat();
  }
  expect(TType::RCurly);
  block->scope = ctx.exit_scope();
  end_node(block, range);
  return block;
}

ASTParamsDecl *Parser::parse_parameters(std::vector<GenericParameter> generic_params) {
  NODE_ALLOC(ASTParamsDecl, params, range, defer, this);
  expect(TType::LParen);

  while (peek().type != TType::RParen) {
    NODE_ALLOC(ASTParamDecl, param, range, _, this)
    if (params->is_varargs) {
      end_node(nullptr, range);
      throw_error("var args \"...\" must be the last parameter", range);
    }

    auto subrange = begin_node();
    if (peek().type == TType::Varargs) {
      eat();
      if (!current_func_decl) {
        throw_error("Cannot use varargs outside of a function declaration. "
                    "Only use this for #foreign functions.",
                    range);
      }
      current_func_decl.get()->flags |= FUNCTION_IS_VARARGS;
      params->is_varargs = true;
      continue;
    }

    auto next = peek();
    // parse self parameters.
    if (next.type == TType::Mul || next.value == "self" || next.value == "себя") {
      Mutability mutability = CONST;
      bool is_pointer = false;

      if (next.type == TType::Mul) {
        eat();
        is_pointer = true;
        if (peek().type == TType::Const) {
          eat();
          mutability = CONST;
        } else if (peek().type == TType::Mut) {
          eat();
          mutability = MUT;
        }
      }

      auto ident = expect(TType::Identifier);
      param->self.is_pointer = is_pointer;
      param->mutability = mutability;

      if (ident.value == "self") {
        param->tag = ASTParamDecl::Self;
      } else if (ident.value == "себя") {
        param->tag = ASTParamDecl::Себя;
      } else {
        end_node(nullptr, range);
        throw_error("when we got *mut/*const, we expected \'self\', since the parameter was not named", range);
      }

      if (params->params.size() != 0) {
        end_node(nullptr, range);
        throw_error("'self/себя' must be the first parameter in the signature", range);
      }

      params->has_self = true;
      params->params.push_back(param);

      if (peek().type != TType::RParen) {
        expect(TType::Comma);
      }
      continue;
    }

    Mutability mutability = CONST;
    if (peek().type == TType::Mut) {
      eat();
      mutability = MUT;
    }

    auto name = expect(TType::Identifier).value;

    expect(TType::Colon);
    param->normal.type = parse_type();
    param->tag = ASTParamDecl::Normal;
    param->mutability = mutability;
    param->normal.name = name;

    if (peek().type == TType::Assign) {
      end_node(nullptr, range);
      throw_error("Ela does not support default parameters.", range);
    }

    params->params.push_back(param);
    end_node(param, subrange);

    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    } else {
      break;
    }
  }

  expect(TType::RParen);
  end_node(params, range);
  return params;
}

ASTFunctionDeclaration *Parser::parse_function_declaration(Token name) {
  NODE_ALLOC(ASTFunctionDeclaration, function, range, _, this)
  expect(TType::Fn);

  function->has_defer = false;

  if (peek().type == TType::GenericBrace) {
    function->generic_parameters = parse_generic_parameters();
  }

  auto last_func_decl = current_func_decl;
  Defer deferred([&] { current_func_decl = last_func_decl; });
  current_func_decl = function;

  function->params = parse_parameters(function->generic_parameters);
  function->name = name.value;

  // check for definition.
  auto has_definition = false;
  {
    auto sym = ctx.scope->local_lookup(name.value);
    if (sym && (sym->flags & SYMBOL_IS_FORWARD_DECLARED) == 0) {
      has_definition = true;
    }
  }

  ctx.scope->insert_function(name.value, Type::INVALID_TYPE_ID, function);

  if (peek().type != TType::Arrow) {
    function->return_type = ASTType::get_void();
  } else {
    expect(TType::Arrow);
    function->return_type = parse_type();
  }

  if (peek().type == TType::Where) {
    function->where_clause = parse_where_clause();
  }

  if (peek().type == TType::Semi) {
    function->flags |= FUNCTION_IS_FORWARD_DECLARED;
    auto sym = ctx.scope->local_lookup(name.value)->flags |= SYMBOL_IS_FORWARD_DECLARED;
    end_node(function, range);
    current_func_decl = last_func_decl;
    return function;
  }

  ctx.set_scope();

  if (function->params->has_self) {
    function->flags |= FUNCTION_IS_METHOD;
  }

  // TODO: find a better solution to this.
  for (const auto &param : function->generic_parameters) {
    ctx.scope->forward_declare_type(param, Type::UNRESOLVED_GENERIC_TYPE_ID);
  }

  function->block = parse_block();
  function->block.get()->parent = function;

  if (function->block && has_definition) {
    end_node(nullptr, range);
    throw_error(std::format("Redefinition of function {}", name.value), range);
  }

  for (const auto &stmt : function->block.get()->statements) {
    if (stmt->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      throw_error("local functions are not allowed", stmt->source_range);
    }
  }

  end_node(function, range);
  function->scope = ctx.exit_scope();
  return function;
}

ASTEnumDeclaration *Parser::parse_enum_declaration(Token tok) {
  NODE_ALLOC(ASTEnumDeclaration, node, range, _, this)
  expect(TType::Enum);
  node->name = tok.value;
  expect(TType::LCurly);
  if (ctx.scope->find_type_id(tok.value, {}) != Type::INVALID_TYPE_ID) {
    end_node(node, range);
    throw_error("Redefinition of enum " + tok.value.get_str(), range);
  }
  int last_value = -1;
  bool is_first = true;
  while (peek().type != TType::RCurly) {
    auto iden = expect(TType::Identifier).value;
    ASTExpr *value = nullptr;

    if (peek().type == TType::Assign) {
      expect(TType::Assign);
      value = parse_expr();
      auto evaluated_value = evaluate_constexpr(value, this->ctx);
      if (evaluated_value.tag != Value::INTEGER) {
        throw_error("Enums can only have integers", value->source_range);
      }
      last_value = evaluated_value.integer;
    } else {
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      last_value++;
      literal->value = std::to_string(last_value);
      value = literal;
      end_node(literal, range);
    }

    if (peek().type == TType::Comma) {
      eat();
    }

    node->key_values.push_back({iden, value});
    is_first = false;
  }
  end_node(node, range);
  std::vector<InternedString> keys;
  std::set<InternedString> keys_set;
  for (const auto &[key, value] : node->key_values) {
    if (keys_set.find(key) != keys_set.end()) {
      throw_error(std::format("redefinition of enum variant: {}", key), node->source_range);
    }
    keys.push_back(key);
    keys_set.insert(key);
  }

  if (node->key_values.empty()) {
    end_node(nullptr, range);
    throw_error("Empty `enum` types are not allowed", range);
  }

  expect(TType::RCurly);
  return node;
}

ASTImpl *Parser::parse_impl() {
  NODE_ALLOC_EXTRA_DEFER(ASTImpl, node, range, _, this, current_impl_decl = nullptr)
  expect(TType::Impl);

  ctx.set_scope();
  node->scope = ctx.exit_scope();

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  current_impl_decl = node;
  auto target = parse_type();

  ASTType *interface = nullptr;
  if (peek().type == TType::For) {
    expect(TType::For);
    interface = parse_type();
    node->interface = target;
    node->target = interface;
  } else {
    node->target = target;
  }

  node->target->resolved_type = Type::INVALID_TYPE_ID;
  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto block = parse_block(node->scope);
  end_node(node, range);

  node->scope->symbols.clear();

  for (const auto &statement : block->statements) {
    if (statement->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      auto function = static_cast<ASTFunctionDeclaration *>(statement);
      node->methods.push_back(function);
    } else if (statement->get_node_type() == AST_NODE_ALIAS) {
      node->aliases.push_back(static_cast<ASTAlias *>(statement));
    } else if (statement->get_node_type() == AST_NODE_VARIABLE) {
      auto variable = (ASTVariable *)statement;
      node->constants.push_back(variable);
    } else {
      throw_error("invalid statement: only methods and aliases are allowed in 'impl's", statement->source_range);
    }
  }
  return node;
}

ASTDefer *Parser::parse_defer() {
  NODE_ALLOC(ASTDefer, node, range, _, this)
  expect(TType::Defer);
  node->statement = parse_statement();
  end_node(node, range);
  return node;
}

ASTWhere *Parser::parse_where_clause() {
  NODE_ALLOC(ASTWhere, node, range, _, this);
  std::vector<Constraint> &constraints = node->constraints;
  auto parse_constraint = [&] -> Constraint {
    auto type = parse_type();
    expect(TType::Colon);
    ASTExpr *condition = parse_type();
    while (peek().type == TType::And || peek().type == TType::Or) {
      NODE_ALLOC(ASTBinExpr, binexpr, range, _, this)
      binexpr->op = eat();
      binexpr->right = parse_type();
      binexpr->left = condition;
      condition = (ASTExpr *)binexpr;
    }
    return {type, condition};
  };
  expect(TType::Where);
  constraints.push_back(parse_constraint());
  while (peek().type == TType::Comma) {
    eat();
    constraints.push_back(parse_constraint());
  }
  return node;
}

ASTInterfaceDeclaration *Parser::parse_interface_declaration(Token name) {
  auto previous = current_interface_decl;
  NODE_ALLOC_EXTRA_DEFER(ASTInterfaceDeclaration, node, range, _, this, { current_interface_decl = previous; });
  expect(TType::Interface);

  node->name = name.value;
  current_interface_decl = node;
  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto scope = create_child(ctx.scope);
  node->scope = scope;
  auto block = parse_block(scope);
  for (const auto &stmt : block->statements) {
    if (auto function = dynamic_cast<ASTFunctionDeclaration *>(stmt)) {
      node->methods.push_back(function);
    }
  }
  return node;
}

ASTStructDeclaration *Parser::parse_struct_declaration(Token name) {
  bool is_union = false;
  auto old = current_struct_decl;
  NODE_ALLOC(ASTStructDeclaration, node, range, _, this)

  if (peek().type == TType::Struct) {
    expect(TType::Struct);
  } else {
    is_union = true;
    expect(TType::Union);
  }

  node->is_union = is_union;
  current_struct_decl = node;

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto type_id = ctx.scope->find_type_id(name.value, {});

  if (type_id != Type::INVALID_TYPE_ID) {
    auto type = global_get_type(type_id);
    end_node(nullptr, range);
    if (type->is_kind(TYPE_STRUCT)) {
      auto info = (type->get_info()->as<StructTypeInfo>());
      if (DOESNT_HAVE_FLAG(info->flags, STRUCT_FLAG_FORWARD_DECLARED)) {
        throw_error("Redefinition of struct", range);
      }
    } else {
      throw_error("cannot redefine already existing type", range);
    }
  } else {
    type_id = ctx.scope->create_struct_type(name.value, create_child(ctx.scope), node);
  }

  node->name = name.value;
  node->resolved_type = type_id;
  auto type = global_get_type(type_id);
  auto info = type->get_info()->as<StructTypeInfo>();
  node->scope = info->scope;
  if (is_union)
    info->flags |= STRUCT_FLAG_IS_UNION;

  for (const auto &param : node->generic_parameters) {
    info->scope->forward_declare_type(param, Type::UNRESOLVED_GENERIC_TYPE_ID);
  }

  if (!semicolon()) {
    auto scope = info->scope;
    expect(TType::LCurly);
    std::vector<ASTNode *> directives;
    while (peek().type != TType::RCurly) {
      if (peek().type == TType::Directive) {
        eat();
        auto directive = process_directive(DIRECTIVE_KIND_STATEMENT, expect(TType::Identifier).value);
        if (!directive) { // it yielded no node, just return.
          continue;
        }
        if (directive && directive.get()->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
          node->subtypes.push_back(static_cast<ASTStructDeclaration *>(directive.get()));
        } else if (directive && directive.get()->get_node_type() == AST_NODE_VARIABLE) {
          ASTStructMember member{};
          auto _node = static_cast<ASTVariable *>(directive.get());
          member.name = _node->name;
          member.is_bitfield = true;
          member.bitsize = _node->bitsize;
          member.type = _node->type;
          node->members.push_back(member);
        } else {
          end_node(node, range);
          throw_error("right now, only `#anon :: struct/union` and `#bitfield(n_bits) name: type` definitions are the "
                      "only thing allowed in structs, besides member declarations.",
                      node->source_range);
        }
      } else if (peek().type == TType::Identifier) {
        ASTStructMember member{};
        member.name = eat().value;
        expect(TType::Colon);
        member.type = parse_type();
        node->members.push_back(member);
      }
      if (peek().type != TType::RCurly) {
        expect(TType::Comma);
      }
    }
    expect(TType::RCurly);
    info->flags &= ~STRUCT_FLAG_FORWARD_DECLARED;
  } else {
    info->flags |= STRUCT_FLAG_FORWARD_DECLARED;
    node->is_fwd_decl = true;
  }

  current_struct_decl = old;
  end_node(node, range);
  return node;
}

ASTChoiceDeclaration *Parser::parse_tagged_union_declaration(Token name) {
  NODE_ALLOC(ASTChoiceDeclaration, node, range, _, this)
  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }
  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }
  auto scope = create_child(ctx.scope);
  auto type = global_get_type(ctx.scope->create_tagged_union(name.value, scope, node));
  ctx.set_scope(scope);
  node->name = name.value;
  node->scope = scope;
  node->resolved_type = type->id;

  expect(TType::LCurly);

  while (peek().type != TType::RCurly) {
    ASTChoiceVariant variant;
    variant.name = expect(TType::Identifier).value;
    if (peek().type == TType::Comma || peek().type == TType::RCurly) {
      variant.kind = ASTChoiceVariant::NORMAL;
      node->variants.push_back(variant);
    } else if (peek().type == TType::LCurly) {
      variant.kind = ASTChoiceVariant::STRUCT;
      eat();
      while (peek().type != TType::RCurly) {
        variant.struct_declarations.push_back(parse_variable());
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RCurly);
      node->variants.push_back(variant);
    } else if (peek().type == TType::LParen) {
      variant.kind = ASTChoiceVariant::TUPLE;
      variant.tuple = parse_type();
      assert(variant.tuple->kind == ASTType::TUPLE);
      node->variants.push_back(variant);
    } else {
      end_node(node, range);
      throw_error("Unexpected token in tagged union declaration", node->source_range);
    }
    if (peek().type != TType::RCurly)
      expect(TType::Comma);
  }
  ctx.exit_scope();
  expect(TType::RCurly);
  end_node(node, range);
  return node;
}

Nullable<ASTExpr> Parser::try_parse_directive_expr() {
  if (peek().type == TType::Directive) {
    eat();
    InternedString identifier = eat().value;
    Nullable<ASTNode> node = process_directive(DIRECTIVE_KIND_EXPRESSION, identifier);

    auto expr = Nullable<ASTExpr>(dynamic_cast<ASTExpr *>(node.get()));
    if (expr.is_not_null()) {
      return expr;
    } else {
      auto range = begin_node();
      eat();
      end_node(nullptr, range);
      throw_error("Invalid directive in expression: directives in "
                  "expressions must return a value.",

                  range);
    }
  }
  return nullptr;
}

std::vector<ASTExpr *> Parser::parse_generic_arguments() {
  auto range = begin_node();
  expect(TType::GenericBrace);
  std::vector<ASTExpr *> params;

  while (peek().type != TType::GT) {
    params.push_back(parse_type());
    if (peek().type != TType::GT)
      expect(TType::Comma);
  }

  expect(TType::GT);
  end_node(nullptr, range);
  return params;
}

std::vector<GenericParameter> Parser::parse_generic_parameters() {
  auto range = begin_node();
  expect(TType::GenericBrace);
  std::vector<GenericParameter> params;
  while (peek().type != TType::GT) {
    params.emplace_back(expect(TType::Identifier).value);
    if (peek().type != TType::GT)
      expect(TType::Comma);
  }
  expect(TType::GT);
  end_node(nullptr, range);
  return params;
}

std::vector<ASTType *> Parser::parse_parameter_types() {
  std::vector<ASTType *> param_types;
  expect(TType::LParen);
  while (peek().type != TType::RParen) {
    auto param_type = parse_type();
    param_types.push_back(param_type);
    if (peek().type == TType::Comma) {
      expect(TType::Comma);
    } else {
      break;
    }
  }
  expect(TType::RParen);
  return param_types;
}

void Parser::append_type_extensions(ASTType *&node) {
  while (true) {
    if (peek().type == TType::LBrace) {
      expect(TType::LBrace);
      if (peek().type != TType::RBrace) {
        auto expression = parse_expr();
        node->extensions.insert(node->extensions.begin(), {TYPE_EXT_ARRAY, expression});
      }
      expect(TType::RBrace);
    } else {
      break;
    }
  }
}

ASTDeclaration *find_generic_instance(std::vector<GenericInstance> instantiations, const std::vector<int> &gen_args) {
  for (auto &instantiation : instantiations) {
    if (instantiation.arguments == gen_args) {
      return instantiation.declaration;
    }
  }
  return nullptr;
}

ASTType *Parser::parse_function_type() {
  NODE_ALLOC(ASTType, output_type, range, _, this)
  expect(TType::Fn);
  output_type->kind = ASTType::FUNCTION;

  if (peek().type == TType::Mul) {
    eat();
    output_type->extensions.insert(output_type->extensions.begin(), {TYPE_EXT_POINTER_MUT});
  }

  append_type_extensions(output_type);
  FunctionTypeInfo info{};
  output_type->function.parameter_types = parse_parameter_types();
  if (peek().type == TType::Arrow) {
    eat();
    output_type->function.return_type = parse_type();
  } else {
    output_type->function.return_type = nullptr;
  }
  return output_type;
}

ASTLambda *Parser::parse_lambda() {
  NODE_ALLOC(ASTLambda, node, range, _, this);
  expect(TType::Fn);
  node->params = parse_parameters();
  if (peek().type == TType::Arrow) {
    eat();
    node->return_type = parse_type();
  } else {
    node->return_type = ASTType::get_void();
  }

  node->block = parse_block();

  return node;
}

void Parser::parse_pointer_extensions(ASTType *type) {
  int pointer_depth = 0;

  while (peek().type == TType::Mul) {
    expect(TType::Mul);
    type->extensions.push_back({peek().type == TType::Mut ? TYPE_EXT_POINTER_MUT : TYPE_EXT_POINTER_CONST});
    if (peek().type == TType::Mut) {
      expect(TType::Mut);
    } else if (peek().type == TType::Const) {
      expect(TType::Const);
    } else {
      throw_error(
          "'*const/*mut' are required for all pointer types now, as a prefix. such as '*const s32', '*const *mut s64'",
          {peek().location});
    }
  }
}

static Precedence get_operator_precedence(Token token) {
  if (token.is_comp_assign()) {
    return PRECEDENCE_ASSIGNMENT;
  }
  auto type = token.type;
  switch (type) {
    case TType::Assign:
    case TType::ColonEquals:
      return PRECEDENCE_ASSIGNMENT;
    case TType::LogicalOr:
      return PRECEDENCE_LOGICALOR;
    case TType::LogicalAnd:
      return PRECEDENCE_LOGICALAND;
    case TType::Or:
      return PRECEDENCE_BITWISEOR;
    case TType::Xor:
      return PRECEDENCE_BITWISEXOR;
    case TType::And:
      return PRECEDENCE_BITWISEAND;
    case TType::EQ:
    case TType::NEQ:
      return PRECEDENCE_EQUALITY;
    case TType::LT:
    case TType::GT:
    case TType::LE:
    case TType::GE:
      return PRECEDENCE_RELATIONAL;
    case TType::SHL:
    case TType::SHR:
      return PRECEDENCE_SHIFT;
    case TType::Add:
    case TType::Sub:
      return PRECEDENCE_ADDITIVE;
    case TType::Mul:
    case TType::Div:
    case TType::Modulo:
      return PRECEDENCE_MULTIPLICATIVE;
    default:
      return PRECEDENCE_LOWEST;
  }
}

ASTType *ASTType::get_void() {
  static ASTType *type = [] {
    ASTType *type = ast_alloc<ASTType>();
    type->kind = ASTType::NORMAL;
    auto path = ast_alloc<ASTPath>();
    path->push_part("void");
    type->normal.path = path;
    type->resolved_type = void_type();
    return type;
  }();
  return type;
}

Token Parser::eat() {
  token_idx++;
  fill_buffer_if_needed();
  auto tok = peek();
  lookahead_buf().pop_front();
  lexer.get_token(states.back());
  return tok;
}

void Parser::fill_buffer_if_needed() {
  while (states.back().lookahead_buffer.size() < 8) {
    lexer.get_token(states.back());
  }
}

bool Parser::import(InternedString name, Scope **scope) {
  std::string ela_lib_path;
  if (const char *env_p = std::getenv("ELA_LIB_PATH")) {
    ela_lib_path = env_p;
  } else {
#ifdef _WIN32
    ela_lib_path = "C:\\Program Files\\ela";
#else
    ela_lib_path = "/usr/local/lib/ela";
#endif
  }

  auto module_name = name;
  auto filename = std::filesystem::path(ela_lib_path) / name.get_str();

  if (std::filesystem::exists(module_name.get_str()) || std::filesystem::exists(module_name.get_str() + ".ela")) {
    filename = module_name.get_str();
  }

  // Right now, we just return false if we're double including.
  if (import_map.contains(module_name)) {
    *scope = import_map[module_name];
    return false;
  }

  if (std::filesystem::is_directory(filename)) {
    filename += std::filesystem::path::preferred_separator;
    filename.append("lib.ela");
  } else {
    filename += ".ela";
  }

  if (!std::filesystem::exists(filename)) {
    throw_error(std::format("Couldn't find imported module: {}\nIf you're writing a directory based module, make sure "
                            "you have a 'lib.ela' as your lib main.",
                            module_name),
                {});
  }

  import_map.insert({module_name, *scope});
  states.push_back(Lexer::State::from_file(filename));
  fill_buffer_if_needed();
  return true;
}

Token Parser::expect(TType type) {
  fill_buffer_if_needed();
  if (peek().type != type) {
    SourceRange range = {
        .begin_location = peek().location,
    };
    throw_error(std::format("Expected {}, got {} : {}", TTypeToString(type), TTypeToString(peek().type), peek().value),
                range);
  }
  return eat();
}

SourceRange Parser::begin_node() {
  auto location = peek().location;
  return SourceRange{
      .begin_location = location,
  };
}

void Parser::end_node(ASTNode *node, SourceRange &range) {
  if (node) {
    node->source_range = range;
  }
}

Token Parser::peek() const {
  if (states.empty()) {
    return Token::Eof();
  }

  // nocheckin
  if (!states.back().lookahead_buffer.empty()) {
    return states.back().lookahead_buffer.front();
  } else {
    return Token::Eof();
  }
}

Parser::Parser(const std::string &filename, Context &context)
    : ctx(context), states({Lexer::State::from_file(filename)}) {
  fill_buffer_if_needed();
  typer = new Typer(context);
}

Parser::~Parser() { delete typer; }

Nullable<ASTBlock> Parser::current_block = nullptr;
