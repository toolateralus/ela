#include "ast.hpp"
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <format>
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

enum PreprocKind {
  PREPROC_IF,
  PREPROC_IFDEF,
  PREPROC_IFNDEF,
};

static void remove_body(Parser *parser) {
  parser->expect(Token_Type::LCurly);
  int depth = 1;
  while (depth > 0) {
    if (parser->peek().type == Token_Type::LCurly)
      depth++;
    if (parser->peek().type == Token_Type::RCurly)
      depth--;
    if (parser->peek().type == Token_Type::LCurly)
      depth++;
    if (parser->peek().type == Token_Type::RCurly)
      depth--;
    parser->eat();
  }
}

static void remove_preproc(Parser *parser) {
  if (parser->peek().type == Token_Type::If ||
      (parser->peek().type == Token_Type::Identifier && parser->peek().value == "ifdef") ||
      (parser->peek().type == Token_Type::Identifier && parser->peek().value == "ifndef")) {
    while (parser->peek().type != Token_Type::LCurly) {
      parser->eat();
    }
    remove_body(parser);
    if (parser->peek().type == Token_Type::Else) {
      parser->expect(Token_Type::Else);
      remove_preproc(parser);
    }
  } else {
    remove_body(parser);
  }
}

static void parse_ifdef_if_else_preprocs(Parser *parser, AST *list, PreprocKind kind) {
  bool executed = false;

  if (kind == PREPROC_IFDEF) { // Handling #ifdef
    auto symbol = parser->expect(Token_Type::Identifier).value;
    executed = has_def(symbol);
  } else if (kind == PREPROC_IFNDEF) { // Handling #ifndef
    auto symbol = parser->expect(Token_Type::Identifier).value;
    executed = !has_def(symbol);
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
    parser->expect(Token_Type::LCurly);
    while (parser->peek().type != Token_Type::RCurly) {
      list->statements.push_back(parser->parse_statement());
      while (parser->peek().type == Token_Type::Semi)
        parser->eat();
    }
    parser->expect(Token_Type::RCurly);
  } else {
    remove_body(parser);
  }

  if (parser->peek().type == Token_Type::Else) {
    parser->expect(Token_Type::Else);
    if (!executed) { /* No code has been emitted yet. try again if we can. */
      if (parser->peek().type == Token_Type::If) {
        parser->expect(Token_Type::If);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
      } else if (parser->peek().type == Token_Type::Identifier && parser->peek().value == "ifdef") {
        parser->expect(Token_Type::Identifier);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
      } else if (parser->peek().type == Token_Type::Identifier && parser->peek().value == "ifndef") {
        parser->expect(Token_Type::Identifier);
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
      } else {
        parser->expect(Token_Type::LCurly);
        while (parser->peek().type != Token_Type::RCurly) {
          list->statements.push_back(parser->parse_statement());
          while (parser->peek().type == Token_Type::Semi)
            parser->eat();
        }
        parser->expect(Token_Type::RCurly);
      }
    } else { /* No code emitted for this case, eat it up. */
      remove_preproc(parser);
    }
  }
}

void Parser::parse_parameters(const std::vector<GenericParameter> &generic_parameters, AST *node) {
  expect(Token_Type::LParen);
  AST *type = nullptr;
  auto &function = node->function; 
  while (peek().type != Token_Type::RParen) {
    AST_Parameter_Declaration param;
    if (function.is_varargs) {
      end_node(nullptr, node->source_range);
      throw_error("var args \"...\" must be the last parameter", node->source_range);
    }
    auto subrange = begin_node();
    if (peek().type == Token_Type::Varargs) {
      eat();
      if (!current_func_decl) {
        throw_error("Cannot use varargs outside of a function declaration. "
                    "Only use this for #foreign functions.",
                    node->source_range);
      }
      function.flags |= FUNCTION_IS_VARARGS;
      function.is_varargs = true;
      continue;
    }

    auto name = expect(Token_Type::Identifier).value;

    if (name == "self") {
      if (!function.parameters.empty()) {
        end_node(nullptr, node->source_range);
        throw_error("\"self\" must appear first in method parameters.", node->source_range);
      }
      function.has_self = true;
      param.tag = AST_PARAM_SELF;
      function.parameters.push_back(param);

      if (peek().type == Token_Type::Mul) {
        eat();
        param.self.is_pointer = true;
      }

      if (peek().type != Token_Type::RParen)
        expect(Token_Type::Comma);
      continue;
    }

    if (peek().type == Token_Type::Colon) {
      expect(Token_Type::Colon);
      type = parse_type();
    }

    param.tag = AST_PARAM_NORMAL;
    param.normal.type = type;
    param.normal.name = name;

    if (peek().type == Token_Type::Assign) {
      end_node(nullptr, node->source_range);
      throw_error("Ela does not support default parameters.", node->source_range);
    }
    function.parameters.push_back(param);


    if (peek().type != Token_Type::RParen) {
      expect(Token_Type::Comma);
    } else
      break;
  }
  expect(Token_Type::RParen);
}

// clang-format off
std::vector<DirectiveRoutine> Parser:: directive_routines = {
    // #include
    // Just like C's include, just paste a text file right above where the
    // include is used.
    {.identifier = "include",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        auto filename = parser->expect(Token_Type::String).value;
        if (!std::filesystem::exists(filename.get_str())) {
          throw_error(std::format("Couldn't find included file: {}, current path: {}", filename,
                                  std::filesystem::current_path().string()),
                      {});
        }
        if (import_set.contains(filename)) {
          return nullptr;
        }
        import_set.insert(filename);
        parser->states.push_back(Lexer::State::from_file(filename.get_str()));
        parser->fill_buffer_if_needed();
        return nullptr;
    }},
    // #import
    // Imports from usr/local/lib/ela by identifier and no file ext.
    {.identifier = "import",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        auto iden = parser->expect(Token_Type::Identifier).value;
        parser->import(iden.get_str());
        return nullptr;
    }},
    // This is only used for debugging the compiler in rare cases.
    {.identifier = "print",
     .kind = DIRECTIVE_KIND_STATEMENT,
     .run = [](Parser *parser) -> Nullable<AST> {
        auto str = parser->expect(Token_Type::String);
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
        parser->expect(Token_Type::LParen);
        auto filename = parser->expect(Token_Type::String).value;

        InternedString mode = "text";
        if (parser->peek().type == Token_Type::Comma) {
          parser->eat();
          // could be binary, and whatever other options
          mode = parser->eat().value; 
        }
        if (!std::filesystem::exists(filename.get_str())) {
          throw_error(std::format("Couldn't find 'read' file: {}", filename), {});
        }
        
        parser->expect(Token_Type::RParen);
        NODE_ALLOC(AST_LITERAL, string, range, parser, _);
        string->literal.tag = LITERAL_STRING;
        std::stringstream ss;
        if (mode == "binary") {
          std::ifstream isftr(filename.get_str(), std::ios::binary);
          ss << isftr.rdbuf();
          string->literal.value = ss.str();
          return string;
        } else {
          std::ifstream isftr(filename.get_str());
          ss << isftr.rdbuf();
          string->literal.value = ss.str();
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
      .run = [](Parser *parser) -> Nullable<AST> {
        // Issue 2
        // TODO: implement something so we can do
        // * #test(group: "My Test Group", expects: false) *
        // so we can have tests that expect to fail, and so we can use
        // --filter="My Test Group" // run only test group
        // --filter=* - "My Test Group" // run all but test group

        auto name = parser->expect(Token_Type::Identifier);
        parser->expect(Token_Type::DoubleColon);
        auto func = parser->parse_function_declaration(name);
        func->function.flags |= (int)Function_Instance_Flags::FUNCTION_IS_TEST;

        if (compile_command.has_flag("test")) {
          return func;
        } else {
          func->parent->scope.erase(func->function.name);
          return GLOBAL_NOOP;
        }
    }},
    // #foreign
    // Declare a foreign function, like C's extern.
    {.identifier = "foreign",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        NODE_ALLOC(AST_FUNCTION, function, range, parser, _)
        auto name = parser->expect(Token_Type::Identifier);
        auto last_func_decl = parser->current_func_decl;
        parser->current_func_decl = function;

        Defer deferred = {[&] { parser->current_func_decl = last_func_decl; }};

        parser->expect(Token_Type::DoubleColon);
        parser->expect(Token_Type::Fn);

        function->function.parameters = parser->parse_parameters();
        function->function.name = name.value;
        if (parser->peek().type == Token_Type::Arrow) {
          parser->expect(Token_Type::Arrow);
          function->function.return_type = parser->parse_type();
        }
        function->function.flags |= FUNCTION_IS_FOREIGN;
        parser->expect(Token_Type::Semi);
        return function;
    }},
    // #error, for throwing compiler errors.
    {.identifier = "error",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        auto error = parser->parse_primary();
        if (error->node_type != AST_LITERAL) {
          throw_error("Can only throw a literal as a error", error->source_range);
        }
        auto literal = error->literal;
        throw_error(literal.value.get_str(), error->source_range);
        return nullptr;
    }},
    // #type
    // get a 'Type *' struct ptr to reflect on a given type.
    // has .fields and .size only currently
    {.identifier = "type",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_TYPE, outer, range, parser, _)
        parser->expect(Token_Type::LParen);
        outer->type.pointing_to = parser->parse_expr();
        parser->expect(Token_Type::RParen);

        // TODO: Refactor how this works, the #type() should probably just be it's own node.
        // It would vastly simplify a ton of stuff.

        outer->type.kind = AST_TYPE_REFLECTION;
        outer->type.normal.base = ast_alloc(AST_IDENTIFIER, parser->last_parent);
        outer->type.normal.base->identifier = "Type";
        outer->type.extensions.push_back({TYPE_EXT_POINTER});
        return outer;
    }},
    // #c_flags, for adding stuff like linker options, -g etc from within
    // your program or header.
    {.identifier = "c_flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        auto string = parser->expect(Token_Type::String).value;
        if (string == "-g") {
          compile_command.flags[string.get_str()] = true;
        }
        compile_command.add_compilation_flag(string.get_str());
        return nullptr;
    }},
    // #flags, for making an enum declaration auto increment with a flags value.
    // #flags MyEnum :: enum {...};
    {.identifier = "flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        auto name = parser->expect(Token_Type::Identifier);
        parser->expect(Token_Type::DoubleColon);
        auto enum_decl = parser->parse_enum_declaration(name);
        int index = 0;
        for (auto &key_value : enum_decl->$enum.key_values) {
          NODE_ALLOC(AST_LITERAL, literal, range, parser, _);
          literal->literal.tag = LITERAL_INTEGER;
          literal->literal.value = std::to_string(1 << index);
          key_value.second = literal;
          index++;
        }
        enum_decl->$enum.is_flags = true;
        return enum_decl;
    }},
    // #alias for making type aliases. #alias NewName :: OldName;
    {.identifier = "alias",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_ALIAS, alias, range, parser, _)
        alias->alias.name = parser->expect(Token_Type::Identifier).value;
        parser->expect(Token_Type::DoubleColon);
        alias->alias.type = parser->parse_type();
        return alias;
      }
    },
    // #self, return the type of the current declaring struct or union
    {.identifier = "self",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_TYPE, type, range, parser, defer);
        type->type.kind = AST_TYPE_SELF;
        parser->append_type_extensions(type);
        return type;
    }},
    // #anon, for declaring anonymous sub-structs in unions primarily, and anonymous unions within struct declarations.
    {.identifier = "anon",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        auto tok = parser->expect(Token_Type::DoubleColon);
        if (parser->peek().type == Token_Type::Struct || parser->peek().type == Token_Type::Union) {
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
      .run = [](Parser *parser) -> Nullable<AST> {
        auto node = parser->parse_statement();
        if (node->node_type == AST_STRUCT) {
          auto &$struct = node->$struct;
          $struct.is_extern = true;
        } else if (node->node_type == AST_DECLARATION) {
          auto &decl = node->declaration;
          decl.is_extern = true;
        } else if (node->node_type == AST_FUNCTION) {
          auto &function = node->function;
          function.flags |= FUNCTION_IS_EXPORTED;
        }
        return node;
    }},
    
    // TODO: rewrite #typeid.

    // #bitfield, for declaring bitfields. Pretty much only to interop with C:
    // most cases for bitfields are completely useless, and can be replaced with
    // a
    // set of flags.
    {.identifier = "bitfield",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        if (parser->current_struct_decl.is_null()) {
          throw_error("Cannot declare bitfields outside of a struct declaration.", {});
        }
        parser->expect(Token_Type::LParen);
        auto size = parser->expect(Token_Type::Integer);
        parser->expect(Token_Type::RParen);
        AST *decl = parser->parse_declaration();
        decl->declaration.is_bitfield = true;
        decl->declaration.bitsize = size.value;
        return decl;
    }}, 

    // #static, used exclusively for static globals, and static locals.
    // We do not support static methods or static members.
    {.identifier = "static",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        auto statement = parser->parse_statement();
        if (statement->node_type == AST_DECLARATION) {
          statement->declaration.is_static = true;
        } else if (statement->node_type == AST_FUNCTION) {
          statement->function.flags |= FUNCTION_IS_STATIC;
        }
        return statement;
    }},

    // #def, define a compile time flag, like C #define but cannot be a macro.
    {.identifier = "def",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        add_def(parser->expect(Token_Type::Identifier).value);
        while (parser->peek().type == Token_Type::Semi) parser->eat();
        return GLOBAL_NOOP;
    }},

    // #undef, remove a #def
    {.identifier = "undef",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        undef(parser->expect(Token_Type::Identifier).value);
        while (parser->peek().type == Token_Type::Semi) parser->eat();
        return GLOBAL_NOOP;
    }},

    // #ifdef, conditional compilation based on a #def being present.
    {.identifier = "ifdef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_STATEMENT_LIST, list, range, parser, _)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
        return list;
    }},

    // #ifndef, conditional compilation based on a #def not being present.
    {.identifier = "ifndef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_STATEMENT_LIST, list, range, parser, _)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
        return list;
    }},

    // #if, conditional compilation based on compile time value.
    {.identifier = "if",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_STATEMENT_LIST, list, range, parser, _)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
        return list;
    }},

    // #region, for named/unnnamed regions. just for organization, has no compilation implications.
    // can have anything between the #region directive and the {} block
    // #region My code region 1 {...} is legal.
    {.identifier = "region",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<AST> {
        NODE_ALLOC(AST_STATEMENT_LIST, list, range, parser, _)
        while (parser->peek().type != Token_Type::LCurly) {
          parser->eat();
        }
        parser->expect(Token_Type::LCurly);
        while (parser->peek().type != Token_Type::RCurly) {
          list->statements.push_back(parser->parse_statement());
          while (parser->peek().type == Token_Type::Semi) parser->eat();
        }
        parser->expect(Token_Type::RCurly);
        return list;
      }
    },
};
// clang-format on

Nullable<AST> Parser::process_directive(DirectiveKind kind, const InternedString &identifier) {
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

AST *Parser::parse() {
  NODE_ALLOC(AST_PROGRAM, program, range, this, _)
  while (true) {
    if (peek().type == Token_Type::Eof && !states.empty()) {
      states.pop_back();
      if (!states.empty()) {
        std::filesystem::current_path(states.back().path.parent_path());
      }
    }

    while (semicolon())
      eat();

    if (peek().type == Token_Type::Eof && states.empty()) {
      break;
    } else if (peek().type == Token_Type::Eof) {
      eat();
    }

    if (peek().type == Token_Type::Directive) {
      eat();
      InternedString identifier = eat().value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifier);
      if (result.is_not_null() && result.get()->node_type != AST_NOOP) {
        program->statements.push_back(result.get());
      }
      if (semicolon())
        eat();

      continue;
    }

    auto statement = parse_statement();

    auto type = statement->node_type;
    switch (type) {
      case AST_STRUCT:
      case AST_FUNCTION:
      case AST_INTERFACE:
      case AST_ENUM:
      case AST_ALIAS:
      case AST_DECLARATION:
      case AST_NOOP:
      case AST_IMPL:
        break;
      default:
      err:
        throw_error("Statement not allowed at the top-level of a program", statement->source_range);
    }

    program->statements.push_back(statement);

    while (semicolon())
      eat();
  }
  end_node(program, range);
  return program;
}

void Parser::parse_arguments(AST *call) {
  expect(Token_Type::LParen);
  while (peek().type != Token_Type::RParen) {
    call->call.arguments.push_back(parse_expr());
    if (peek().type != Token_Type::RParen) {
      expect(Token_Type::Comma);
    }
  }
  expect(Token_Type::RParen);
}

AST *Parser::parse_call(AST *function) {
  NODE_ALLOC(AST_CALL, node, range, this, _);
  node->call.function = function;
  if (peek().type == Token_Type::GenericBrace) {
    node->call.generic_arguments = parse_generic_arguments();
  }
  parse_arguments(node);
  end_node(node, range);
  return node;
}

AST *Parser::parse_expr(Precedence precedence) {
  AST *left = parse_unary();
  while (true) {
    Precedence token_precedence = get_operator_precedence(peek());
    if (token_precedence <= precedence)
      break;
    AST *node = ast_alloc(AST_BINARY, last_parent);
    node->source_range = left->source_range;
    auto op = eat();
    auto right = parse_expr(token_precedence);
    node->binary.left = left;
    node->binary.right = right;
    node->binary.op = op.type;
    left = node;
  }
  return left;
}

AST *Parser::parse_unary() {
  // bitwise not is a unary expression because arrays use it as a pop operator,
  // and sometimes you might want to ignore it's result.
  if (peek().type == Token_Type::Add || peek().type == Token_Type::Sub || peek().type == Token_Type::LogicalNot ||
      peek().type == Token_Type::Not || peek().type == Token_Type::Increment || peek().type == Token_Type::Decrement ||
      peek().type == Token_Type::Mul || peek().type == Token_Type::And || peek().type == Token_Type::Not) {
    NODE_ALLOC(AST_UNARY_EXPR, unaryexpr, range, this, _)
    auto op = eat();
    auto expr = parse_unary();

    // TODO: make a more comprehensive rvalue evaluator.
    // We need to use it later for self* method calls
    auto is_rvalue = expr->node_type == AST_LITERAL || (expr->node_type == AST_CALL && op.type == Token_Type::And);

    if ((is_rvalue) && (op.type == Token_Type::And || op.type == Token_Type::Mul)) {
      end_node(nullptr, range);
      throw_error("Cannot take the address of, or dereference a literal or "
                  "'rvalue'\nlike a function result or constructor result. It "
                  "is a temporary value\nand would be invalid once this "
                  "statement completed executing",
                  range);
    }
    unaryexpr->unary.op = op.type;
    unaryexpr->unary.operand = expr;
    end_node(unaryexpr, range);
    return unaryexpr;
  }

  return parse_postfix();
}

AST *Parser::parse_postfix() {
  auto left = parse_primary();
  auto range = begin_node();
  // build dot and subscript expressions
  while (peek().type == Token_Type::DoubleColon || peek().type == Token_Type::Dot ||
         peek().type == Token_Type::LBrace || peek().type == Token_Type::LParen ||
         peek().type == Token_Type::GenericBrace || peek().type == Token_Type::Increment ||
         peek().type == Token_Type::Decrement || peek().type == Token_Type::Range || peek().type == Token_Type::As) {
    if (peek().type == Token_Type::LParen || peek().type == Token_Type::GenericBrace) {
      left = parse_call(left);
    } else if (peek().type == Token_Type::Dot) {
      NODE_ALLOC(AST_DOT_EXPR, dot, range, this, _)
      eat();
      dot->dot.base = left;
      if (peek().type == Token_Type::Integer || peek().type == Token_Type::Identifier) {
        dot->dot.member_name = eat().value;
      } else {
        end_node(left, range);
        throw_error("Invalid dot expression right hand side: expected a member name, or for a tuple, an index.", range);
      }
      left = dot;
    } else if (peek().type == Token_Type::DoubleColon) {
      NODE_ALLOC(AST_SCOPE_RESOLUTION, scope_resolution, range, this, _)
      eat();
      scope_resolution->dot.base = left;
      scope_resolution->dot.member_name = expect(Token_Type::Identifier).value;
      left = scope_resolution;
    } else if (peek().type == Token_Type::Increment || peek().type == Token_Type::Decrement) {
      NODE_ALLOC(AST_UNARY_EXPR, unary, unary_range, this, _)
      unary->unary.operand = left;
      unary->unary.op = eat().type;
      return unary;
    } else if (peek().type == Token_Type::LBrace) {
      NODE_ALLOC(AST_SUBSCRIPT, subscript, range, this, _)
      subscript->subscript.left = left;
      eat();
      subscript->subscript.index_expression = parse_expr();
      expect(Token_Type::RBrace);
      left = subscript;
    } else if (peek().type == Token_Type::Range) {
      NODE_ALLOC(AST_RANGE, node, range, this, _)
      eat();
      node->range.right = parse_expr();
      node->range.left = left;
      return node;
    } else if (peek().type == Token_Type::As) {
      NODE_ALLOC(AST_CAST, node, range, this, _)
      eat();
      node->cast.target_type = parse_type();
      node->cast.expression = left;
      left = node;
    }
  }

  end_node(left, range);
  return left;
}

AST *Parser::parse_primary() {
  auto tok = peek();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  switch (tok.type) {
    case Token_Type::Size_Of: {
      NODE_ALLOC(AST_SIZE_OF, node, range, this, _);
      eat();
      expect(Token_Type::LParen);
      node->size_of = parse_type();
      expect(Token_Type::RParen);
      return node;
    }
    case Token_Type::Fn: {
      return parse_lambda();
    }
    case Token_Type::Switch: {
      NODE_ALLOC(AST_SWITCH, node, range, this, _)
      expect(Token_Type::Switch);
      node->$switch.target = parse_expr();
      expect(Token_Type::LCurly);
      while (peek().type != Token_Type::RCurly) {
        SwitchCase _case{
            .expression = parse_expr(),
        };
        if (peek().type != Token_Type::ExpressionBody) {
          expect(Token_Type::Colon);
        }
        _case.block = parse_block();
        node->$switch.cases.push_back(_case);
        if (peek().type == Token_Type::Comma) {
          eat();
        }
      }
      expect(Token_Type::RCurly);
      return node;
    }
    case Token_Type::Char: {
      NODE_ALLOC(AST_LITERAL, node, range, this, _)
      eat();
      node->literal.tag = LITERAL_CHAR;
      node->literal.value = tok.value;
      end_node(node, range);
      return node;
    }
    case Token_Type::LCurly: {
      NODE_ALLOC(AST_INITIALIZER, init_list, range, this, _)
      eat();
      if (peek().type == Token_Type::RCurly) {
        init_list->initializer.tag = INITIALIZER_EMPTY;
      } else if (lookahead_buf()[1].type != Token_Type::Colon) {
        init_list->initializer.tag = INITIALIZER_COLLECTION;
        while (peek().type != Token_Type::RCurly) {
          init_list->initializer.values.push_back(parse_expr());
          if (peek().type == Token_Type::Comma) {
            eat();
          }
        }
      } else {
        init_list->initializer.tag = INITIALIZER_NAMED;
        while (peek().type != Token_Type::RCurly) {
          auto identifier = expect(Token_Type::Identifier).value;
          expect(Token_Type::Colon);
          init_list->initializer.key_values.push_back({identifier, parse_expr()});
          if (peek().type == Token_Type::Comma) {
            eat();
          }
        }
      }
      expect(Token_Type::RCurly);
      end_node(init_list, range);
      return init_list;
    }
    case Token_Type::Identifier: {
      if (last_parent->find_type_id(tok.value, {}) != Type::INVALID_TYPE_ID) {
        auto type = parse_type();
        if (peek().type == Token_Type::LCurly) {
          auto init_list = parse_expr();
          if (init_list->node_type != AST_INITIALIZER) {
            throw_error("Type {...} syntax can only be used for initializer lists. Was this a typo?",
                        init_list->source_range);
          }
          init_list->initializer.target_type = type;
          return init_list;
        }
        return type;
      }
      NODE_ALLOC(AST_IDENTIFIER, iden, range, this, _)
      eat();
      iden->identifier = tok.value;
      end_node(iden, range);
      return iden;
    }
    case Token_Type::Null: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_NULL;
      literal->literal.value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case Token_Type::True: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_BOOL;
      literal->literal.value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case Token_Type::False: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_BOOL;
      literal->literal.value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case Token_Type::Integer: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_INTEGER;
      literal->literal.value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case Token_Type::Float: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_FLOAT;
      literal->literal.value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case Token_Type::String: {
      NODE_ALLOC(AST_LITERAL, literal, range, this, _)
      eat();
      literal->literal.tag = LITERAL_STRING;
      literal->literal.value = tok.value;
      if (peek().type == Token_Type::Identifier && peek().value == "c") {
        literal->literal.is_c_string = true;
        eat();
      }
      end_node(literal, range);
      return literal;
    }
    case Token_Type::LParen: {
      auto range = begin_node();
      expect(Token_Type::LParen); // (
      const auto lookahead = lookahead_buf();
      auto expr = parse_expr();
      if (peek().type == Token_Type::Comma) {
        AST *node = ast_alloc(AST_TUPLE, last_parent);
        Defer _([&] { this->end_node(node, range); });
        node->tuple.push_back(expr);
        eat();
        while (peek().type != Token_Type::RParen) {
          node->tuple.push_back(parse_expr());
          if (peek().type == Token_Type::Comma)
            eat();
        }
        expect(Token_Type::RParen);
        return node;
      }
      if (peek().type != Token_Type::RParen) {
        eat();
        end_node(nullptr, range);
        throw_error("Expected ')'", range);
      }
      eat();
      end_node(expr, range);

      if (expr->node_type == AST_TYPE) {
        throw_error("using (TYPE)expr style casts are deprecated. use `expr as TYPE` syntax", range);
      }

      return expr;
    }
    default: {
      auto error_range = begin_node();
      throw_error(std::format("Invalid primary expression. Token: '{}'... Type: '{}'", tok.value,
                              Token_Type_To_String(tok.type)),
                  error_range);
      return nullptr;
    }
  }
}

AST *Parser::parse_type() {
  if (peek().type == Token_Type::LParen) {
    NODE_ALLOC(AST_TYPE, node, range, this, _)
    node->resolved_type = Type::INVALID_TYPE_ID;
    node->type.kind = AST_TYPE_TUPLE;
    eat();
    while (peek().type != Token_Type::RParen) {
      node->type.tuple_types.push_back(parse_type());
      if (peek().type == Token_Type::Comma)
        eat();
    }
    expect(Token_Type::RParen);
    // grab up more extensions if they exist.
    append_type_extensions(node);
    return node;
  }

  // parse #self types.
  if (peek().type == Token_Type::Directive) {
    auto range = begin_node();
    auto expr = try_parse_directive_expr().get();
    if (expr->node_type != AST_TYPE) {
      throw_error("unable to get type from directive expression where a type "
                  "was expected.",
                  range);
    }
    return expr;
  }

  if (peek().type == Token_Type::Fn) {
    return parse_function_type();
  }

  NODE_ALLOC(AST_TYPE, type, range, this, _)
  auto base = eat().value;
  type->type.kind = AST_TYPE_NORMAL;
  type->type.normal.base = ast_alloc(AST_IDENTIFIER, last_parent);
  type->type.normal.base->identifier = base;
  type->type.normal.base->source_range = range;

  if (peek().type == Token_Type::GenericBrace) {
    type->type.normal.generic_arguments = parse_generic_arguments();
  }

  if (peek().type == Token_Type::DoubleColon && lookahead_buf()[1].type == Token_Type::Identifier &&
      lookahead_buf()[2].type == Token_Type::LParen) {
    // this is a function call to a static, single depth function.
    return type;
  }

  while (peek().type == Token_Type::DoubleColon) {
    NODE_ALLOC(AST_SCOPE_RESOLUTION, scope_res, range, this, _)
    eat();
    scope_res->scope_resolution.base = type->type.normal.base;
    scope_res->scope_resolution.member_name = expect(Token_Type::Identifier).value;
    type->type.normal.base = scope_res;
  }

  append_type_extensions(type);

  end_node(type, range);
  return type;
}

AST *Parser::parse_statement() {
  auto parent_range = begin_node();

  auto tok = peek();

  while (tok.type == Token_Type::Semi) {
    eat();
    tok = peek();
  }

  // * '#' Directives.
  if (tok.type == Token_Type::Directive) {
    auto range = begin_node();
    eat();
    auto directive_name = eat().value;

    auto statement = process_directive(DIRECTIVE_KIND_STATEMENT, directive_name).get();
    if (!statement)
      statement = GLOBAL_NOOP;

    end_node(statement, range);
    return statement;
  }

  if (peek().type == Token_Type::Defer) {
    return parse_defer();
  }

  if (peek().type == Token_Type::Impl) {
    return parse_impl();
  }

  // * Tuple destructure.
  if (tok.type == Token_Type::Identifier && lookahead_buf()[1].type == Token_Type::Comma) {
    return parse_multiple_asssignment();
  }

  // * Variable declarations
  // * 'n := 10;'
  // * 'n : int = 10;'
  // * 'const_n :: 10;' (remove me from here)
  // TODO: this condition seems excessively complicated.

  bool is_colon_or_colon_equals =
      lookahead_buf()[1].type == Token_Type::Colon || lookahead_buf()[1].type == Token_Type::ColonEquals;

  if (tok.type == Token_Type::Identifier && is_colon_or_colon_equals) {
    auto decl = parse_declaration();
    return decl;
  }

  // * Control flow
  {
    if (tok.type == Token_Type::LCurly) {
      auto block = parse_block();
      return block;
    }

    if (tok.type == Token_Type::Return) {
      NODE_ALLOC(AST_RETURN, return_node, range, this, _)
      expect(Token_Type::Return);
      if (peek().type != Token_Type::Semi) {
        return_node->$return = parse_expr();
      }
      end_node(return_node, range);
      return return_node;
    }

    if (tok.type == Token_Type::Break) {
      NODE_ALLOC(AST_BREAK, _break, range, this, _)
      eat();
      end_node(_break, range);
      return _break;
    }

    if (tok.type == Token_Type::Continue) {
      NODE_ALLOC(AST_CONTINUE, _continue, range, this, _)
      eat();
      end_node(_continue, range);
      return _continue;
    }

    if (tok.type == Token_Type::For) {
      NODE_ALLOC(AST_FOR, node, range, this, _)
      eat();

      node->$for.value_semantic = ValueSemantic::VALUE_SEMANTIC_COPY;

      // reference semantic for iterating over list
      if (peek().type == Token_Type::Mul) {
        node->$for.value_semantic = ValueSemantic::VALUE_SEMANTIC_POINTER;
        eat();
      }

      if (lookahead_buf()[1].type == Token_Type::In) {
        node->$for.iter_identifier = parse_primary();
        expect(Token_Type::In);
        auto expr = parse_expr();
        node->$for.range = expr;
      } else {
        throw_error("Invalid for syntax. expected 'for i in 0..10 || for elem in "
                    "iterable || for *elem in iterable",
                    range);
      }

      node->$for.block = parse_block();
      end_node(node, range);
      return node;
    }

    if (tok.type == Token_Type::While) {
      NODE_ALLOC(AST_WHILE, node, range, this, _)
      eat();
      if (peek().type != Token_Type::LCurly) {
        node->$while.condition = parse_expr();
      }
      node->$while.block = parse_block();
      end_node(node, range);
      return node;
    }

    // TODO: we should handle the 'then' statement more gracefully.
    // Also, => is super fricken janky, and is really poorly implemented.
    if (tok.type == Token_Type::If) {
      NODE_ALLOC(AST_IF, node, range, this, _)
      eat();
      node->$if.condition = parse_expr();

      if (peek().type == Token_Type::Then) {
        NODE_ALLOC(AST_BLOCK, block, _range, this, _);
        eat();
        node->$while.block = block;
        auto statement = parse_statement();
        node->$while.block->statements = {statement};
        if (statement->node_type == AST_DECLARATION) {
          throw_warning(WarningInaccessibleDeclaration, "Inaccesible declared variable", statement->source_range);
        }
      } else {
        node->$while.block = parse_block();
      }

      if (peek().type == Token_Type::Else) {
        NODE_ALLOC(AST_ELSE, $else, range, this, _)
        eat();
        if (peek().type == Token_Type::If) {
          auto inner_if = parse_statement();
          assert(inner_if->node_type == AST_IF);
          $else->$else.elseif = inner_if;
        } else {
          $else->$else.block = parse_block();
        }
        node->$if.$else = $else;
      }
      end_node(node, range);
      return node;
    }
  }

  if (peek().type == Token_Type::Identifier && lookahead_buf()[1].type == Token_Type::DoubleColon &&
      lookahead_buf()[2].type == Token_Type::Identifier &&
      (lookahead_buf()[3].type == Token_Type::GenericBrace || lookahead_buf()[3].type == Token_Type::LParen)) {
    NODE_ALLOC(AST_EXPR_STATEMENT, expr, range, this, _)
    expr->expression_statement = parse_expr();
    end_node(expr, range);
    return expr;
  }

  // * Type declarations.
  // * Todo: handle constant 'CONST :: VALUE' Declarations here.
  if (lookahead_buf()[1].type == Token_Type::DoubleColon) {
    expect(Token_Type::Identifier);
    expect(Token_Type::DoubleColon);
    if (peek().type == Token_Type::Fn) {
      auto node = parse_function_declaration(tok);
      return node;
    }
    if (peek().type == Token_Type::Interface) {
      auto interface = parse_interface_declaration(tok);
      return interface;
    }
    if (peek().type == Token_Type::Struct || peek().type == Token_Type::Union) {
      auto struct_decl = parse_struct_declaration(tok);
      return struct_decl;
    }
    if (peek().type == Token_Type::Enum) {
      auto enum_decl = parse_enum_declaration(tok);
      return enum_decl;
    }

    NODE_ALLOC(AST_DECLARATION, decl, range, this, _);
    decl->declaration.name = tok.value;
    decl->declaration.value = parse_expr();
    decl->declaration.is_constexpr = true;

    if (last_parent->find_type_id(tok.value, {}) != Type::INVALID_TYPE_ID || keywords.contains(tok.value.get_str())) {
      end_node(nullptr, range);
      throw_error("Invalid variable declaration: a type or keyword exists with "
                  "that name,",
                  range);
    }

    end_node(decl, range);
    if (last_parent->local_lookup(tok.value)) {
      throw_error(std::format("re-definition of '{}'", tok.value), decl->source_range);
    }
    last_parent->insert_variable(tok.value, Type::INVALID_TYPE_ID, decl->declaration.value.get());
    return decl;
  }

  // ! BUG:: Somehow we broke 'a.b++' expressions here, it parses the dot then hits the ++; as if that's valid.
  // * Expression statements.
  {
    auto next = lookahead_buf()[1];
    auto next_next = lookahead_buf()[2];
    // Both postfix and prefix (inc/dec)rement
    const bool is_increment_or_decrement = (next.type == Token_Type::Increment ||
                                            next.type == Token_Type::Decrement && next_next.type != Token_Type::Semi) ||
                                           tok.type == Token_Type::Increment || tok.type == Token_Type::Decrement;

    // subscript assignment or dot assign/ call statement.
    const bool is_identifier_with_lbrace_or_dot =
        tok.type == Token_Type::Identifier && (next.type == Token_Type::LBrace || next.type == Token_Type::Dot);

    const bool is_call = next.type == Token_Type::LParen || next.type == Token_Type::GenericBrace;

    const bool is_assignment_or_compound =
        next.type == Token_Type::Assign || next.type == Token_Type::Comma || next.is_comp_assign();

    const bool is_deref = tok.type == Token_Type::Mul;

    const bool is_special_case = tok.type == Token_Type::LParen || // possible parenthesized dereference or something.
                                 tok.type == Token_Type::Switch;

    if (is_call || is_increment_or_decrement || is_identifier_with_lbrace_or_dot || is_assignment_or_compound ||
        is_deref || is_special_case) {
      NODE_ALLOC(AST_EXPR_STATEMENT, statement, range, this, _)
      statement->expression_statement = parse_expr();
      if (statement->node_type == AST_SWITCH) {
        statement->$switch.is_statement = true;
      }
      end_node(statement, range);
      return statement;
    }
  }

  end_node(nullptr, parent_range);

  //*  Failure to parse errors

  {
    if (tok.family == TFamily::Operator) {
      throw_error(std::format("Unexpected operator: {} '{}'", Token_Type_To_String(tok.type), tok.value), parent_range);
    }

    if (tok.family == TFamily::Literal) {
      eat();
      throw_error(std::format("Unexpected literal: {} .. {}", tok.value, Token_Type_To_String(tok.type)), parent_range);
    }

    if (tok.family == TFamily::Keyword) {
      eat();
      throw_error(std::format("Unexpected keyword: {}", tok.value), parent_range);
    }

    if (last_parent->lookup(tok.value)) {
      eat();
      throw_error(std::format("Unexpected variable {}", tok.value), parent_range);
    }

    if (last_parent->find_type_id(tok.value, {}) == Type::INVALID_TYPE_ID) {
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

AST *Parser::parse_multiple_asssignment() {
  NODE_ALLOC(AST_TUPLE_DECONSTRUCTION, node, range, this, _)
  auto first = parse_primary();
  node->tuple_deconstruction.idens.push_back(first);
  while (peek().type == Token_Type::Comma) {
    eat();
    auto expr = parse_primary();
    if (expr->node_type == AST_IDENTIFIER) {
      node->tuple_deconstruction.idens.push_back(expr);
    } else {
      end_node(nullptr, range);
      throw_error("Can only have identifiers on the left hand side of a tuple "
                  "deconstruction expressions",
                  range);
    }
  }
  if (peek().type == Token_Type::ColonEquals || peek().type == Token_Type::Assign) {
    node->tuple_deconstruction.op = eat().type;
    node->tuple_deconstruction.right = parse_expr();
  } else {
    // TODO: allow typed tuple deconstructions.
    end_node(nullptr, range);
    throw_error("Currently, you cannot have an explicitly typed tuple "
                "deconstruction. Use a, b, c := ....",
                range);
  }

  end_node(node, range);

  for (const auto &iden : node->tuple_deconstruction.idens) {
    auto symbol = node->local_lookup(iden->identifier);
    if (node->tuple_deconstruction.op == Token_Type::ColonEquals) {
      if (symbol)
        throw_error("redefinition of a variable, tuple deconstruction with := doesn't allow redeclaration of any of "
                    "the identifiers",
                    node->source_range);
      node->insert_variable(iden->identifier, Type::INVALID_TYPE_ID, nullptr);
    } else {
      // TODO: reimplement this error in a sane way.
      // if (!symbol) throw_error("use of an undeclared variable, tuple deconstruction with = requires all identifiers
      // already exist", node->source_range);
      node->insert_variable(iden->identifier, Type::INVALID_TYPE_ID, nullptr);
    }
  }

  return node;
}

AST *Parser::parse_declaration() {
  NODE_ALLOC(AST_DECLARATION, node, range, this, _);
  auto &decl = node->declaration;
  auto iden = eat();
  decl.name = iden.value;

  if (last_parent->find_type_id(iden.value, {}) != Type::INVALID_TYPE_ID || keywords.contains(iden.value.get_str())) {
    end_node(nullptr, range);
    throw_error("Invalid variable declaration: a type or keyword exists with "
                "that name,",
                range);
  }

  if (peek().type == Token_Type::Colon) {
    expect(Token_Type::Colon);
    decl.type = parse_type();
    if (peek().type == Token_Type::Assign) {
      eat();
      auto expr = parse_expr();
      decl.value = expr;
    }
  } else if (peek().type == Token_Type::DoubleColon) {
    eat();
    decl.value = parse_expr();
    decl.is_constexpr = true;
  } else {
    expect(Token_Type::ColonEquals);
    decl.value = parse_expr();
  }

  end_node(node, range);
  if (last_parent->local_lookup(iden.value)) {
    throw_error(std::format("re-definition of '{}'", iden.value), node->source_range);
  }

  last_parent->insert_variable(iden.value, Type::INVALID_TYPE_ID, decl.value.get(), node);

  return node;
}

AST *Parser::parse_block(Scope *scope) {
  auto last_block = current_block;
  NODE_ALLOC_EXTRA_DEFER(AST_BLOCK, block, range, _, this, current_block = last_block);
  current_block = block;
  auto parent_defer = set_last_parent(block);

  if (peek().type == Token_Type::ExpressionBody) {
    NODE_ALLOC(AST_RETURN, node, range, this, _);
    expect(Token_Type::ExpressionBody);
    node->$return = parse_expr();
    block->statements = {node};
    return block;
  }

  expect(Token_Type::LCurly);

  while (peek().type != Token_Type::RCurly) {
    if (peek().type == Token_Type::Eof) {
      end_node(nullptr, range);
      throw_error("Imbalanced '{' and '}'", range);
    }
    auto statement = parse_statement();

    if (statement->node_type == AST_DEFER) {
      if (current_func_decl.get()) {
        current_func_decl.get()->function.has_defer = true;
      } else {
        throw_error("You can only use defer within a function scope", statement->source_range);
      }
      block->block.has_defer = true;
      block->block.defer_count++;
    }

    block->statements.push_back(statement);
    while (semicolon())
      eat();
  }
  expect(Token_Type::RCurly);
  end_node(block, range);
  return block;
}

AST *Parser::parse_function_declaration(Token name) {
  NODE_ALLOC(AST_FUNCTION, node, range, this, _)
  auto &function = node->function;
  expect(Token_Type::Fn);

  function.has_defer = false;

  if (peek().type == Token_Type::GenericBrace) {
    function.generic_parameters = parse_generic_parameters();
  }

  auto last_func_decl = current_func_decl;
  Defer deferred([&] { current_func_decl = last_func_decl; });
  current_func_decl = node;

  parse_parameters(function.generic_parameters, node);
  function.name = name.value;

  // check for definition.
  auto has_definition = false;
  {
    auto sym = node->local_lookup(name.value);
    if (sym && (sym->flags & SYMBOL_IS_FORWARD_DECLARED) == 0) {
      has_definition = true;
    }
  }

  node->insert_function(name.value, Type::INVALID_TYPE_ID, node);

  if (peek().type != Token_Type::Arrow) {
    // TODO: we might want to change this, it was get_void()
    function.return_type = nullptr;
  } else {
    expect(Token_Type::Arrow);
    function.return_type = parse_type();
  }

  if (peek().type == Token_Type::Where) {
    function.where_clause = parse_where_clause();
  }

  if (peek().type == Token_Type::Semi) {
    function.flags |= FUNCTION_IS_FORWARD_DECLARED;
    auto sym = node->local_lookup(name.value)->flags |= SYMBOL_IS_FORWARD_DECLARED;
    end_node(node, range);
    current_func_decl = last_func_decl;
    return node;
  }

  if (current_impl_decl) {
    if (function.has_self) {
      function.flags |= FUNCTION_IS_METHOD;
    } else {
      function.flags |= FUNCTION_IS_STATIC;
    }
  }

  // TODO: find a better solution to this.
  for (const auto &param : function.generic_parameters) {
    node->forward_declare_type(param, Type::UNRESOLVED_GENERIC_TYPE_ID);
  }

  function.block = parse_block();
  function.block.get()->parent = node;

  if (function.block && has_definition) {
    end_node(nullptr, range);
    throw_error(std::format("Redefinition of function {}", name.value), range);
  }

  for (const auto &stmt : function.block.get()->statements) {
    if (stmt->node_type == AST_FUNCTION) {
      throw_error("local functions are not allowed", stmt->source_range);
    }
  }

  end_node(node, range);
  return node;
}

AST *Parser::parse_enum_declaration(Token tok) {
  NODE_ALLOC(AST_ENUM, node, range, this, _)
  expect(Token_Type::Enum);
  node->$enum.name = tok.value;
  expect(Token_Type::LCurly);
  if (node->find_type_id(tok.value, {}) != Type::INVALID_TYPE_ID) {
    end_node(node, range);
    throw_error("Redefinition of enum " + tok.value.get_str(), range);
  }

  NODE_ALLOC(AST_LITERAL, zero, lit_range, this, _1)
  zero->literal.tag = LITERAL_INTEGER;
  zero->literal.value = "0";

  NODE_ALLOC(AST_LITERAL, one, lit_range2, this, _2)
  one->literal.tag = LITERAL_INTEGER;
  one->literal.value = "1";

  AST *last_value = zero;
  bool was_zero = true;

  while (peek().type != Token_Type::RCurly) {
    auto iden = expect(Token_Type::Identifier).value;
    AST *value = nullptr;

    if (peek().type == Token_Type::Assign) {
      expect(Token_Type::Assign);
      value = parse_expr();
    } else {
      if (was_zero && last_value->node_type == AST_LITERAL && last_value->literal.value == "0") {
        value = zero;
        was_zero = false;
      } else {
        NODE_ALLOC(AST_BINARY, node, range, this, _)
        node->binary.left = last_value;
        node->binary.right = one;
        node->binary.op = Token_Type::Add;
        last_value = node;
        value = node;
        end_node(node, range);
      }
    }
    if (peek().type == Token_Type::Comma) {
      eat();
    }
    node->$enum.key_values.push_back({iden, value});
    last_value = value;
  }
  end_node(node, range);
  std::vector<InternedString> keys;
  std::set<InternedString> keys_set;
  for (const auto &[key, value] : node->$enum.key_values) {
    if (keys_set.find(key) != keys_set.end()) {
      throw_error(std::format("redefinition of enum variant: {}", key), node->source_range);
    }
    keys.push_back(key);
    keys_set.insert(key);
  }

  if (node->$enum.key_values.empty()) {
    end_node(nullptr, range);
    throw_error("Empty `enum` types are not allowed", range);
  }

  expect(Token_Type::RCurly);
  return node;
}

AST *Parser::parse_impl() {
  NODE_ALLOC_EXTRA_DEFER(AST_IMPL, node, range, _, this, current_impl_decl = nullptr)
  expect(Token_Type::Impl);
  auto &impl = node->impl;

  if (peek().type == Token_Type::GenericBrace) {
    impl.generic_parameters = parse_generic_parameters();
  }

  current_impl_decl = node;
  auto target = parse_type();

  // Handle 'impl INTERFACE for TYPE'
  // or normal 'impl TYPE'
  AST *interface = nullptr;
  if (peek().type == Token_Type::For) {
    expect(Token_Type::For);
    AST *interface = parse_type();
    impl.interface = target;
    impl.target = interface;
  } else {
    impl.target = target;
  }

  impl.target->resolved_type = Type::INVALID_TYPE_ID;

  if (peek().type == Token_Type::Where) {
    impl.where_clause = parse_where_clause();
  }
  auto block = parse_block(&node->scope);
  end_node(node, range);

  // TODO: maybe do this differently
  // this is just so you can't call methods directly from within an impl without self
  node->scope.clear();

  for (const auto &statement : block->statements) {
    // TODO: allow constants, aliases, #ifs, #ifdef blah blah whatever a ton more nodes than just 
    // functions.
    if (statement->node_type == AST_FUNCTION) {
      impl.methods.push_back(statement);
    } else {
      throw_error("invalid statement: only methods are allowed in 'impl's", statement->source_range);
    }
  }
  return node;
}

AST *Parser::parse_defer() {
  NODE_ALLOC(AST_DEFER, node, range, this, _)
  expect(Token_Type::Defer);
  node->defer = parse_statement();
  end_node(node, range);
  return node;
}

AST *Parser::parse_where_clause() {
  NODE_ALLOC(AST_WHERE, node, range, this, _);
  expect(Token_Type::Where);
  node->where.target_type = parse_type();
  expect(Token_Type::Is);
  node->where.predicate = parse_type();
  while (peek().type == Token_Type::And || peek().type == Token_Type::Or) {
    NODE_ALLOC(AST_BINARY, binary, range, this, _)
    binary->binary.op = eat().type;
    binary->binary.right = parse_type();
    binary->binary.left = node->where.predicate;
    node->where.predicate = node;
  }
  return node;
}

AST *Parser::parse_interface_declaration(Token name) {
  auto previous = current_interface_decl;
  NODE_ALLOC_EXTRA_DEFER(AST_INTERFACE, node, range, _, this, { current_interface_decl = previous; });
  expect(Token_Type::Interface);
  auto &interface = node->interface;
  interface.name = name.value;
  current_interface_decl = node;

  auto _defer = set_last_parent(node);

  if (peek().type == Token_Type::GenericBrace) {
    interface.generic_parameters = parse_generic_parameters();
  }

  if (peek().type == Token_Type::Where) {
    interface.where_clause = parse_where_clause();
  }

  auto block = parse_block(&node->scope);

  for (const auto &stmt : block->statements) {
    if (stmt->node_type == AST_FUNCTION) {
      // TODO: we should definitely allow methods in impls that are "not overriden", meaning the interface provides
      // a default implementation until the consumer of the interface provides a new one. This wouldn't be virtual
      if (stmt->function.block.is_not_null()) {
        throw_error("Only forward declarations are allowed in interfaces currently", node->source_range);
      }
      interface.methods.push_back(stmt);
    }
  }
  return node;
}

AST *Parser::parse_struct_declaration(Token name) {
  bool is_union = false;
  auto old = current_struct_decl;
  NODE_ALLOC(AST_STRUCT, node, range, this, _)
  auto _defer = set_last_parent(node);
  auto &$struct = node->$struct;
  if (peek().type == Token_Type::Struct) {
    expect(Token_Type::Struct);
  } else {
    is_union = true;
    expect(Token_Type::Union);
  }

  $struct.is_union = is_union;
  current_struct_decl = node;

  if (peek().type == Token_Type::GenericBrace) {
    $struct.generic_parameters = parse_generic_parameters();
  }

  if (peek().type == Token_Type::Where) {
    $struct.where_clause = parse_where_clause();
  }

  auto type_id = node->find_type_id(name.value, {});

  if (type_id != Type::INVALID_TYPE_ID) {
    auto type = global_get_type(type_id);
    end_node(nullptr, range);
    if (type->is_kind(TYPE_STRUCT)) {
      auto info = (type->get_info()->as<StructTypeInfo>());
      if ((info->flags & STRUCT_FLAG_FORWARD_DECLARED) == 0 && info->scope != nullptr) {
        throw_error("Redefinition of struct", range);
      }
    } else {
      throw_error("cannot redefine already existing type", range);
    }
  } else {
    type_id = node->create_struct_type(name.value, nullptr, node);
  }

  node->name = name.value;
  node->resolved_type = type_id;
  auto type = global_get_type(type_id);
  auto info = type->get_info()->as<StructTypeInfo>();
  info->scope = node = create_child(node);
  if (is_union)
    info->flags |= STRUCT_FLAG_IS_UNION;

  for (const auto &param : node->generic_parameters) {
    info->scope->forward_declare_type(param, Type::UNRESOLVED_GENERIC_TYPE_ID);
  }

  if (!semicolon()) {
    auto scope = info->scope;
    expect(Token_Type::LCurly);
    std::vector<AST *> directives;
    while (peek().type != Token_Type::RCurly) {
      if (peek().type == Token_Type::Directive) {
        eat();
        auto directive = process_directive(DIRECTIVE_KIND_STATEMENT, expect(Token_Type::Identifier).value);
        if (directive && directive.get()->node_type == AST_STRUCT) {
          node->subtypes.push_back(static_cast<ASTStructDeclaration *>(directive.get()));
        } else if (directive && directive.get()->node_type == AST_DECLARATION) {
          ASTStructMember member{};
          auto _node = static_cast<AST *>(directive.get());
          member.name = _node->name;
          member.is_bitfield = true;
          member.bitsize = _node->bitsize;
          member.type = _node->type;
          node->members.push_back(member);
        } else if (directive && directive.get()->node_type == AST_ALIAS) {
          node->aliases.push_back(static_cast<ASTAlias *>(directive.get()));
        } else {
          end_node(node, range);
          throw_error("right now, only `#anon :: struct/union` and `#bitfield(n_bits) name: type` definitions are the "
                      "only thing allowed in structs, besides member declarations.",
                      node->source_range);
        }
      } else if (peek().type == Token_Type::Identifier) {
        ASTStructMember member{};
        member.name = eat().value;
        expect(Token_Type::Colon);
        member.type = parse_type();
        node->members.push_back(member);
      }
      if (peek().type != Token_Type::RCurly) {
        expect(Token_Type::Comma);
      }
    }
    expect(Token_Type::RCurly);
    node = scope;
    info->flags &= ~STRUCT_FLAG_FORWARD_DECLARED;
    info->scope = node;
  } else {
    info->flags |= STRUCT_FLAG_FORWARD_DECLARED;
    node->is_fwd_decl = true;
  }

  current_struct_decl = old;
  end_node(node, range);
  return node;
}

Nullable<AST> Parser::try_parse_directive_expr() {
  if (peek().type == Token_Type::Directive) {
    eat();
    InternedString identifier = eat().value;
    Nullable<AST> node = process_directive(DIRECTIVE_KIND_EXPRESSION, identifier);

    auto expr = Nullable<AST>(dynamic_cast<AST *>(node.get()));
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

std::vector<AST_TYPE *> Parser::parse_generic_arguments() {
  auto range = begin_node();
  expect(Token_Type::GenericBrace);
  std::vector<AST_TYPE *> params;
  while (peek().type != Token_Type::RBrace) {
    params.push_back(parse_type());
    if (peek().type != Token_Type::RBrace)
      expect(Token_Type::Comma);
  }
  expect(Token_Type::RBrace);
  end_node(nullptr, range);
  if (params.empty()) {
    throw_error("![] generic arguments cannot be empty!", range);
    ;
  }
  return params;
}

std::vector<GenericParameter> Parser::parse_generic_parameters() {
  auto range = begin_node();
  expect(Token_Type::GenericBrace);
  std::vector<GenericParameter> params;
  while (peek().type != Token_Type::RBrace) {
    params.emplace_back(expect(Token_Type::Identifier).value);
    if (peek().type != Token_Type::RBrace)
      expect(Token_Type::Comma);
  }
  expect(Token_Type::RBrace);
  end_node(nullptr, range);
  if (params.empty()) {
    throw_error("![] generic parameters cannot be empty!", range);
    ;
  }
  return params;
}

std::vector<AST_TYPE *> Parser::parse_parameter_types() {
  std::vector<AST_TYPE *> param_types;
  expect(Token_Type::LParen);
  while (peek().type != Token_Type::RParen) {
    auto param_type = parse_type();
    param_types.push_back(param_type);
    if (peek().type == Token_Type::Comma) {
      expect(Token_Type::Comma);
    } else {
      break;
    }
  }
  expect(Token_Type::RParen);
  return param_types;
}

void Parser::append_type_extensions(AST_TYPE *&node) {
  while (true) {
    if (peek().type == Token_Type::LBrace) {
      expect(Token_Type::LBrace);
      if (peek().type != Token_Type::RBrace) {
        auto expression = parse_expr();
        node->extensions.push_back({TYPE_EXT_ARRAY, expression});
      } else {
        // Syntactic sugar for doing int[] instead of List![int];
        auto type = ast_alloc<AST_TYPE>();
        auto iden = ast_alloc<ASTIdentifier>();
        iden->value = "List";
        type->kind = AST_TYPE::NORMAL;
        type->normal.base = iden;
        type->normal.generic_arguments.push_back(node);
        type->source_range = node->source_range;
        node = type;
      }
      expect(Token_Type::RBrace);
    } else if (peek().type == Token_Type::Mul) {
      expect(Token_Type::Mul);
      node->extensions.push_back({TYPE_EXT_POINTER});
    } else {
      break;
    }
  }
}

AST *Parser::parse_function_type() {
  NODE_ALLOC(AST_TYPE, output_type, range, this, _)
  expect(Token_Type::Fn);
  output_type->kind = AST_TYPE::FUNCTION;
  append_type_extensions(output_type);
  FunctionTypeInfo info{};
  output_type->function.parameter_types = parse_parameter_types();
  if (peek().type == Token_Type::Arrow) {
    eat();
    output_type->function.return_type = parse_type();
  } else {
    output_type->function.return_type = nullptr;
  }
  return output_type;
}

static Precedence get_operator_precedence(Token token) {
  if (token.is_comp_assign()) {
    return PRECEDENCE_ASSIGNMENT;
  }
  auto type = token.type;
  switch (type) {
    case Token_Type::Assign:
    case Token_Type::ColonEquals:
      return PRECEDENCE_ASSIGNMENT;
    case Token_Type::LogicalOr:
      return PRECEDENCE_LOGICALOR;
    case Token_Type::LogicalAnd:
      return PRECEDENCE_LOGICALAND;
    case Token_Type::Or:
      return PRECEDENCE_BITWISEOR;
    case Token_Type::Xor:
      return PRECEDENCE_BITWISEXOR;
    case Token_Type::And:
      return PRECEDENCE_BITWISEAND;
    case Token_Type::EQ:
    case Token_Type::NEQ:
      return PRECEDENCE_EQUALITY;
    case Token_Type::LT:
    case Token_Type::GT:
    case Token_Type::LE:
    case Token_Type::GE:
      return PRECEDENCE_RELATIONAL;
    case Token_Type::SHL:
    case Token_Type::SHR:
      return PRECEDENCE_SHIFT;
    case Token_Type::Add:
    case Token_Type::Sub:
      return PRECEDENCE_ADDITIVE;
    case Token_Type::Mul:
    case Token_Type::Div:
    case Token_Type::Modulo:
      return PRECEDENCE_MULTIPLICATIVE;
    default:
      return PRECEDENCE_LOWEST;
  }
}

AST_TYPE *AST_TYPE::get_void() {
  static AST_TYPE *type = [] {
    AST_TYPE *type = ast_alloc<AST_TYPE>();
    type->kind = AST_TYPE::NORMAL;
    type->normal.base = new (ast_alloc<ASTIdentifier>()) ASTIdentifier("void");
    type->resolved_type = void_type();
    return type;
  }();
  return type;
}

Token Parser::eat() {
  token_idx++;
  fill_buffer_if_needed();
  if (peek().is_eof() && states.size() > 1) {
    states.pop_back();
    std::filesystem::current_path(states.back().path.parent_path());
    fill_buffer_if_needed();
    return peek();
  }
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

void Parser::import(InternedString name) {
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
  // Right now, we just return noop if we're double including.
  if (import_set.contains(module_name)) {
    return;
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

  import_set.insert(module_name);
  states.push_back(Lexer::State::from_file(filename));
  fill_buffer_if_needed();
}

Token Parser::expect(Token_Type type) {
  fill_buffer_if_needed();
  if (peek().type != type) {
    Source_Range range = {
        .begin_location = peek().location,
    };
    throw_error(std::format("Expected {}, got {} : {}", Token_Type_To_String(type), Token_Type_To_String(peek().type),
                            peek().value),
                range);
  }
  return eat();
}

Source_Range Parser::begin_node() {
  auto location = peek().location;
  return Source_Range{
      .begin_location = location,
  };
}

void Parser::end_node(AST *node, Source_Range &range) {
  if (node) {
    node->source_range = range;
  }
}

AST *Parser::parse_lambda() {
  NODE_ALLOC(ASTLambda, node, range, this, _);
  expect(Token_Type::Fn);
  node->params = parse_parameters();
  if (peek().type == Token_Type::Arrow) {
    eat();
    node->return_type = parse_type();
  } else {
    node->return_type = AST_TYPE::get_void();
  }
  node->block = parse_block();

  if (current_func_decl.is_null()) {
    end_node(nullptr, range);
    throw_error("temporarily, lambda functions cannot be used at a global level, only within functions", range);
  }
  return node;
}

Token Parser::peek() const {
  if (states.empty()) {
    return Token::Eof();
  }

  if (!states.back().lookahead_buffer.empty()) {
    return states.back().lookahead_buffer.front();
  } else {
    return Token::Eof();
  }
}

Parser::Parser(const std::string &filename, Context &context)
    : ctx(context), states({Lexer::State::from_file(filename)}) {
  fill_buffer_if_needed();
  import("bootstrap");
  // auto &state = states.back();
  // state.input = "#import bootstrap;\n" + state.input; // TODO: do this in a more structured way. this works, but meh.
  // state.input_len = state.input.length();
  typer = new Typer(context);
}

Parser::~Parser() { delete typer; }

Nullable<ASTBlock> Parser::current_block = nullptr;


