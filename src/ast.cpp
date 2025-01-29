#include "ast.hpp"

#include <algorithm>

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

// TODO: if we encounterthese, just prefix them in tokenizer with $ so they become valid identifiers.
// TODO: we should not have reserved words from host language leak into this langauge.
static std::set<std::string> reserved = {
    "asm",     "double",   "new",      "switch",   "auto",      "else",    "operator", "template", "break",  "enum",
    "private", "this",     "case",     "extern",   "protected", "throw",   "catch",    "float",    "public", "try",
    "char",    "for",      "register", "typedef",  "class",     "friend",  "return",   "union",    "const",  "goto",
    "short",   "unsigned", "continue", "if",       "signed",    "virtual", "default",  "inline",   "sizeof", "void",
    "delete",  "int",      "static",   "volatile", "do",        "long",    "struct",   "while"};

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
      .run = [](Parser *parser) {
        auto filename = parser->expect(TType::String).value;
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
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto iden = parser->expect(TType::Identifier).value;

        std::string ela_lib_path;
        if (const char* env_p = std::getenv("ELA_LIB_PATH")) {
            ela_lib_path = env_p;
        } else {
      #ifdef _WIN32
            ela_lib_path = "C:\\Program Files\\ela";
      #else
            ela_lib_path = "/usr/local/lib/ela";
      #endif
        }

        auto module_name = iden;
        auto filename = std::filesystem::path(ela_lib_path) / iden.get_str();
        // Right now, we just return noop if we're double including.
        if (import_set.contains(module_name)) {
          return nullptr;
        }
        if (std::filesystem::is_directory(filename)) {
            filename += std::filesystem::path::preferred_separator;
            filename.append("lib.ela");
        } else {
          filename += ".ela";
        }

        if (!std::filesystem::exists(filename)) { 
          throw_error(std::format("Couldn't find imported module: {}\nIf you're writing a directory based module, make sure you have a 'lib.ela' as your lib main.", module_name), {});
        }

        import_set.insert(module_name);
        parser->states.push_back(Lexer::State::from_file(filename));
        parser->fill_buffer_if_needed();
        return nullptr;
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

        if (parser->ctx.scope != root_scope)
          throw_error(
              std::format("cannot declare a non-top level foreign function:: {}", name.value),
              range);

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
    // #type
    // get a 'Type *' struct ptr to reflect on a given type.
    // has .fields and .size only currently
    {.identifier = "type",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::LParen);
        auto type = parser->parse_expr();
        parser->expect(TType::RParen);
        NODE_ALLOC(ASTType, outer, range, _, parser)
        
        outer->kind = ASTType::REFLECTION;
        outer->normal.base = new (ast_alloc<ASTIdentifier>()) ASTIdentifier("Type");
        outer->extensions.push_back({TYPE_EXT_POINTER});
        outer->pointing_to = type;
        return outer;
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
        compile_command.add_compilation_flag(string.get_str());
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
        enum_decl->is_flags = true;
        return enum_decl;
    }},
    // #alias for making type aliases. #alias NewName :: OldName;
    {.identifier = "alias",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTAlias, alias, range, _, parser)
        alias->name = parser->expect(TType::Identifier).value;
        parser->expect(TType::DoubleColon);
        alias->type = parser->parse_type();
        return alias;
      }
    },
    // #self, return the type of the current declaring struct or union
    {.identifier = "self",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTType, type, range, defer, parser);
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
        } else if (node->get_node_type() == AST_NODE_DECLARATION) {
          auto decl = static_cast<ASTDeclaration*>(node);
          decl->is_extern = true;
        } else if (node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
          auto func = static_cast<ASTFunctionDeclaration*>(node);
          func->flags |= FUNCTION_IS_EXPORTED;
        }
        return node;
    }},
    // #typeid, integer version of #type. can be used to compare types without
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
        ASTDeclaration *decl = parser->parse_declaration();
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
        if (auto decl = dynamic_cast<ASTDeclaration *>(statement)) {
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
}

ASTProgram *Parser::parse() {
  NODE_ALLOC(ASTProgram, program, range, _, this)
  while (true) {
    if (peek().type == TType::Eof && !states.empty()) {
      states.pop_back();
      if (!states.empty()) {
        std::filesystem::current_path(states.back().path.parent_path());
      }
    }

    while (semicolon())
      eat();

    if (peek().type == TType::Eof && states.empty()) {
      break;
    } else if (peek().type == TType::Eof) {
      eat();
    }

    if (peek().type == TType::Directive) {
      eat();
      InternedString identifier = eat().value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifier);
      if (result.is_not_null()) {
        auto statement = static_cast<ASTStatement *>(result.get());
        if (statement) {
          program->statements.push_back(statement);
        }
      }
      if (semicolon())
        eat();

      continue;
    }

    auto statement = parse_statement();

    auto type = statement->get_node_type();
    switch (type) {
      case AST_NODE_STRUCT_DECLARATION:
      case AST_NODE_FUNCTION_DECLARATION:
      case AST_NODE_INTERFACE_DECLARATION:
      case AST_NODE_ENUM_DECLARATION:
      case AST_NODE_TAGGED_UNION_DECLARATION:
      case AST_NODE_ALIAS:
      case AST_NODE_DECLARATION:
      case AST_NODE_NOOP:
      case AST_NODE_IMPL:
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
  if (peek().type == TType::GenericBrace) {
    call->generic_arguments = parse_generic_arguments();
  }
  call->arguments = parse_arguments();
  end_node(call, range);
  return call;
}

ASTExpr *Parser::parse_expr(Precedence precedence) {
  ASTExpr *left = parse_unary();
  while (true) {
    Precedence token_precedence = get_operator_precedence(peek());
    if (token_precedence <= precedence)
      break;
    auto op = eat();
    auto right = parse_expr(token_precedence);
    NODE_ALLOC(ASTBinExpr, binexpr, range, _, this)
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
    auto op = eat();
    auto expr = parse_unary();
    NODE_ALLOC(ASTUnaryExpr, unaryexpr, range, _, this)

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
    unaryexpr->op = op;
    unaryexpr->operand = expr;
    end_node(unaryexpr, range);
    return unaryexpr;
  }

  return parse_postfix();
}

ASTExpr *Parser::parse_postfix() {
  auto left = parse_primary();
  auto range = begin_node();
  // build dot and subscript expressions
  while (peek().type == TType::DoubleColon || peek().type == TType::Dot || peek().type == TType::LBrace ||
         peek().type == TType::LParen || peek().type == TType::GenericBrace || peek().type == TType::Increment ||
         peek().type == TType::Decrement || peek().type == TType::Range || peek().type == TType::As) {
    if (peek().type == TType::LParen || peek().type == TType::GenericBrace) {
      left = parse_call(left);
    } else if (peek().type == TType::Dot) {
      eat();
      NODE_ALLOC(ASTDotExpr, dot, range, _, this)
      dot->base = left;
      if (peek().type == TType::Integer || peek().type == TType::Identifier) {
        dot->member_name = eat().value;
      } else {
        end_node(left, range);
        throw_error("Invalid dot expression right hand side: expected a member name, or for a tuple, an index.", range);
      }
      left = dot;
    } else if (peek().type == TType::DoubleColon) {
      eat();
      NODE_ALLOC(ASTScopeResolution, dot, range, _, this)
      dot->base = left;
      dot->member_name = expect(TType::Identifier).value;
      left = dot;
    } else if (peek().type == TType::Increment || peek().type == TType::Decrement) {
      NODE_ALLOC(ASTUnaryExpr, unary, range, _, this)
      unary->operand = left;
      unary->op = peek();
      eat();
      end_node(unary, range);
      return unary;
    } else if (peek().type == TType::LBrace) {
      eat();
      auto index = parse_expr();
      expect(TType::RBrace);
      NODE_ALLOC(ASTSubscript, subscript, range, _, this)
      subscript->left = left;
      subscript->subscript = index;
      left = subscript;
    } else if (peek().type == TType::Range) {
      eat();
      auto right = parse_expr();
      NODE_ALLOC(ASTRange, node, range, _, this)
      node->left = left;
      node->right = right;
      return node;
    } else if (peek().type == TType::As) {
      eat();
      NODE_ALLOC(ASTCast, node, range, _, this)
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
  auto range = begin_node();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  switch (tok.type) {
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
      expect(TType::Switch);
      auto expr = parse_expr();
      expect(TType::LCurly);
      std::vector<SwitchCase> cases;
      while (peek().type != TType::RCurly) {
        SwitchCase _case;
        _case.expression = parse_expr();
        if (peek().type != TType::ExpressionBody) {
          expect(TType::Colon);
        }
        _case.block = parse_block();
        cases.push_back(_case);
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RCurly);
      NODE_ALLOC(ASTSwitch, node, range, _, this)
      node->cases = cases;
      node->target = expr;
      end_node(node, range);
      return node;
    }
    case TType::Char: {
      eat();
      NODE_ALLOC(ASTLiteral, node, range, _, this)
      node->tag = ASTLiteral::Char;
      node->value = tok.value;
      end_node(node, range);
      return node;
    }
    case TType::LCurly: {
      eat();
      NODE_ALLOC(ASTInitializerList, init_list, range, _, this)
      if (peek().type == TType::RCurly) {
        init_list->tag = ASTInitializerList::INIT_LIST_EMPTY;
      } else if (lookahead_buf()[1].type != TType::Colon) {
        init_list->tag = ASTInitializerList::INIT_LIST_COLLECTION;
        while (peek().type != TType::RCurly) {
          init_list->values.push_back(parse_expr());
          if (peek().type == TType::Comma) {
            eat();
          }
        }
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
    }
    case TType::Identifier: {
      if (ctx.scope->find_type_id(tok.value, {}) != Type::invalid_id) {
        auto type = parse_type();
        if (peek().type == TType::LCurly) {
          auto init_list = parse_expr();
          if (init_list->get_node_type() != AST_NODE_INITIALIZER_LIST) {
            throw_error("Type {...} syntax can only be used for initializer lists. Was this a typo?",
                        init_list->source_range);
          }
          static_cast<ASTInitializerList *>(init_list)->target_type = type;
          return init_list;
        }
        return type;
      }
      eat();
      NODE_ALLOC(ASTIdentifier, iden, range, _, this)
      iden->value = tok.value;
      end_node(iden, range);
      return iden;
    }
    case TType::Null: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::Null;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::True: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::False: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Integer: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::Integer;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Float: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::Float;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::String: {
      eat();
      NODE_ALLOC(ASTLiteral, literal, range, _, this)
      literal->tag = ASTLiteral::String;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::LParen: {
      expect(TType::LParen); // (
      const auto lookahead = lookahead_buf();
      auto expr = parse_expr();
      if (peek().type == TType::Comma) {
        eat();
        auto exprs = std::vector<ASTExpr *>{expr};
        while (peek().type != TType::RParen) {
          exprs.push_back(parse_expr());
          if (peek().type == TType::Comma)
            eat();
        }
        expect(TType::RParen);
        NODE_ALLOC(ASTTuple, tuple, range, _, this)
        tuple->values = exprs;
        tuple->type = ast_alloc<ASTType>(); // ! why is this here?
        return tuple;
      }
      if (peek().type != TType::RParen) {
        auto range = begin_node();
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
      throw_error(
          std::format("Invalid primary expression. Token: '{}'... Type: '{}'", tok.value, TTypeToString(tok.type)),
          range);
      return nullptr;
    }
  }
}

ASTType *Parser::parse_type() {
  if (peek().type == TType::LParen) {
    eat();
    std::vector<ASTType *> types;
    while (peek().type != TType::RParen) {
      types.push_back(parse_type());
      if (peek().type == TType::Comma)
        eat();
    }
    expect(TType::RParen);
    NODE_ALLOC(ASTType, node, range, _, this)
    node->resolved_type = Type::invalid_id;
    node->kind = ASTType::TUPLE;
    node->tuple_types = types;
    // grab up more extensions if they exist.
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
    return type;
  }

  if (peek().type == TType::Fn) {
    expect(TType::Fn);
    return parse_function_type();
  }

  auto base = eat().value;
  NODE_ALLOC(ASTType, node, range, _, this)
  node->kind = ASTType::NORMAL;
  node->normal.base = new (ast_alloc<ASTIdentifier>()) ASTIdentifier(base);

  if (peek().type == TType::GenericBrace) {
    node->normal.generic_arguments = parse_generic_arguments();
  }

  if (peek().type == TType::DoubleColon && lookahead_buf()[1].type == TType::Identifier &&
      lookahead_buf()[2].type == TType::LParen) {
    // this is a function call to a static, single depth function.
    return node;
  }

  while (peek().type == TType::DoubleColon) {
    eat();
    NODE_ALLOC(ASTScopeResolution, scope_res_node, range, _, this)
    scope_res_node->base = node->normal.base;
    scope_res_node->member_name = expect(TType::Identifier).value;
    node->normal.base = scope_res_node;
  }

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

  // * '#' Directives.
  if (tok.type == TType::Directive) {
    auto range = begin_node();
    eat();
    auto directive_name = eat().value;
    auto statement = dynamic_cast<ASTStatement *>(process_directive(DIRECTIVE_KIND_STATEMENT, directive_name).get());
    if (!statement) {
      throw_error(std::format("Directive '{}' did not return a valid statement node", directive_name), range);
    }
    end_node(statement, range);
    return statement;
  }

  if (peek().type == TType::Defer) {
    return parse_defer();
  }

  if (peek().type == TType::Impl) {
    return parse_impl();
  }

 

  // * Tuple destructure.
  if (tok.type == TType::Identifier && lookahead_buf()[1].type == TType::Comma) {
    return parse_multiple_asssignment();
  }

  // * Variable declarations
  // * 'n := 10;'
  // * 'n : int = 10;'
  // * 'const_n :: 10;' (remove me from here)
  // TODO: this condition seems excessively complicated.

  bool is_colon_or_colon_equals =
      lookahead_buf()[1].type == TType::Colon || lookahead_buf()[1].type == TType::ColonEquals;

  if (tok.type == TType::Identifier && is_colon_or_colon_equals) {
    auto decl = parse_declaration();
    return decl;
  }

  // * Control flow
  {
    if (tok.type == TType::LCurly) {
      auto block = parse_block();
      return block;
    }

    if (tok.type == TType::Return) {
      expect(TType::Return);
      NODE_ALLOC(ASTReturn, return_node, range, _, this)
      if (peek().type != TType::Semi) {
        return_node->expression = parse_expr();
      }
      end_node(return_node, range);
      return return_node;
    }

    if (tok.type == TType::Break) {
      eat();
      NODE_ALLOC(ASTBreak, _break, range, _, this)
      end_node(_break, range);
      return _break;
    }

    if (tok.type == TType::Continue) {
      eat();
      NODE_ALLOC(ASTContinue, _continue, range, _, this)
      end_node(_continue, range);
      return _continue;
    }

    if (tok.type == TType::For) {
      eat();
      NODE_ALLOC(ASTFor, node, range, _, this)

      node->value_semantic = ValueSemantic::VALUE_SEMANTIC_COPY;

      // reference semantic for iterating over list
      if (peek().type == TType::Mul) {
        node->value_semantic = ValueSemantic::VALUE_SEMANTIC_POINTER;
        eat();
      }

      if (lookahead_buf()[1].type == TType::In) {
        node->iden = parse_primary();
        expect(TType::In);
        auto expr = parse_expr();
        node->range = expr;
      } else {
        throw_error("Invalid for syntax. expected 'for i in 0..10 || for elem in "
                    "iterable || for *elem in iterable",
                    range);
      }

      node->block = parse_block();
      end_node(node, range);
      return node;
    }

    if (tok.type == TType::While) {
      eat();
      NODE_ALLOC(ASTWhile, node, range, _, this)
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
      eat();
      NODE_ALLOC(ASTIf, node, range, _, this)
      node->condition = parse_expr();

      if (peek().type == TType::Then) {
        eat();
        NODE_ALLOC(ASTBlock, block, _range, defer, this);
        node->block = block;
        ctx.set_scope();
        auto statement = parse_statement();
        node->block->statements = {statement};
        if (statement->get_node_type() == AST_NODE_DECLARATION) {
          throw_warning(WarningInaccessibleDeclaration, "Inaccesible declared variable", statement->source_range);
        }
        node->block->scope = ctx.exit_scope();
      } else {
        node->block = parse_block();
      }

      if (peek().type == TType::Else) {
        eat();
        NODE_ALLOC(ASTElse, node_else, range, _, this)
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
  if (lookahead_buf()[1].type == TType::DoubleColon) {
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
      if (lookahead_buf()[1].type == TType::LParen && lookahead_buf()[2].type == TType::Union &&
          lookahead_buf()[3].type == TType::RParen) {
        expect(TType::Enum);
        expect(TType::LParen);
        expect(TType::Union);
        expect(TType::RParen);
        auto tagged_union = parse_tagged_union_declaration(tok);
        return tagged_union;
      }
      auto enum_decl = parse_enum_declaration(tok);
      return enum_decl;
    }

    NODE_ALLOC(ASTDeclaration, decl, range, _, this);
    decl->name = tok.value;
    decl->value = parse_expr();
    decl->is_constexpr = true;

    if (ctx.scope->find_type_id(tok.value, {}) != Type::invalid_id || keywords.contains(tok.value.get_str()) ||
        reserved.contains(tok.value.get_str())) {
      end_node(nullptr, range);
      throw_error("Invalid variable declaration: a type or keyword exists with "
                  "that name,",
                  range);
    }

    end_node(decl, range);
    if (ctx.scope->local_lookup(tok.value)) {
      throw_error(std::format("re-definition of '{}'", tok.value), decl->source_range);
    }

    ctx.scope->insert(tok.value, Type::invalid_id, decl->value.get());

    return decl;
  }

  if (peek().type == TType::Identifier && lookahead_buf()[1].type == TType::DoubleColon &&
      lookahead_buf()[2].type == TType::Identifier && lookahead_buf()[3].type != TType::LCurly) {
    NODE_ALLOC(ASTExprStatement, expr, range, _, this)
    expr->expression = parse_expr();
    end_node(expr, range);
    return expr;
  }

  // ! BUG:: Somehow we broke 'a.b++' expressions here, it parses the dot then hits the ++; as if that's valid.
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

    const bool is_assignment_or_compound =
        next.type == TType::Assign || next.type == TType::Comma || next.is_comp_assign();

    const bool is_deref = tok.type == TType::Mul;

    const bool is_special_case = tok.type == TType::LParen || // possible parenthesized dereference or something.
                                 tok.type == TType::Switch;

    if (is_call || is_increment_or_decrement || is_identifier_with_lbrace_or_dot || is_assignment_or_compound ||
        is_deref || is_special_case) {
      NODE_ALLOC(ASTExprStatement, statement, range, _, this)
      statement->expression = parse_expr();

      if (ASTSwitch *_switch = dynamic_cast<ASTSwitch *>(statement->expression)) {
        _switch->is_statement = true;
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

    if (ctx.scope->find_type_id(tok.value, {}) == Type::invalid_id) {
      eat();
      throw_error(std::format("Use of an undeclared type or identifier: {}", tok.value), parent_range);
    }

    eat();
    throw_error(std::format("Unexpected token when parsing statement: {}.. This "
                            "is likely an undefined type.",
                            tok.value),
                parent_range);
  }
}

ASTTupleDeconstruction *Parser::parse_multiple_asssignment() {
  auto first = parse_primary();
  NODE_ALLOC(ASTTupleDeconstruction, node, range, _, this)
  node->idens.push_back(static_cast<ASTIdentifier *>(first));
  while (peek().type == TType::Comma) {
    eat();
    auto expr = parse_primary();
    if (auto iden = dynamic_cast<ASTIdentifier *>(expr)) {
      node->idens.push_back(iden);
    } else {
      end_node(nullptr, range);
      throw_error("Can only have identifiers on the left hand side of a tuple "
                  "deconstruction expressions",
                  range);
    }
  }
  if (peek().type == TType::ColonEquals || peek().type == TType::Assign) {
    node->op = eat().type;
    node->right = parse_expr();
  } else {
    // TODO: allow typed tuple deconstructions.
    end_node(nullptr, range);
    throw_error("Currently, you cannot have an explicitly typed tuple "
                "deconstruction. Use a, b, c := ....",
                range);
  }

  end_node(node, range);

  for (const auto &iden : node->idens) {
    auto symbol = ctx.scope->local_lookup(iden->value);
    if (node->op == TType::ColonEquals) {
      if (symbol)
        throw_error("redefinition of a variable, tuple deconstruction with := doesn't allow redeclaration of any of "
                    "the identifiers",
                    node->source_range);
      ctx.scope->insert(iden->value, Type::invalid_id, node);
    } else {
      // TODO: reimplement this error in a sane way.
      // if (!symbol) throw_error("use of an undeclared variable, tuple deconstruction with = requires all identifiers
      // already exist", node->source_range);
      ctx.scope->insert(iden->value, Type::invalid_id, node);
    }
  }

  return node;
}

ASTDeclaration *Parser::parse_declaration() {
  NODE_ALLOC(ASTDeclaration, decl, range, _, this);
  auto iden = eat();
  decl->name = iden.value;

  if (ctx.scope->find_type_id(iden.value, {}) != Type::invalid_id || keywords.contains(iden.value.get_str()) ||
      reserved.contains(iden.value.get_str())) {
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
  } else if (peek().type == TType::DoubleColon) {
    eat();
    decl->value = parse_expr();
    decl->is_constexpr = true;
  } else {
    expect(TType::ColonEquals);
    decl->value = parse_expr();
  }

  end_node(decl, range);
  if (ctx.scope->local_lookup(iden.value)) {
    throw_error(std::format("re-definition of '{}'", iden.value), decl->source_range);
  }

  ctx.scope->insert(iden.value, Type::invalid_id, decl->value.get());

  return decl;
}

ASTBlock *Parser::parse_block(Scope *scope) {
  auto last_block = current_block;
  NODE_ALLOC_EXTRA_DEFER(ASTBlock, block, range, _, this, current_block = last_block);
  current_block = block;

  ctx.set_scope(scope);

  if (peek().type == TType::ExpressionBody) {
    expect(TType::ExpressionBody);
    NODE_ALLOC(ASTReturn, $return, range, _, this);
    $return->expression = parse_expr();
    block->statements = {$return};
    block->scope = ctx.exit_scope(); // we do this, even though it owns no scope, because it would get created later
                                     // anyway when entering it.
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
  ASTType *type = nullptr;
  while (peek().type != TType::RParen) {
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

    auto name = expect(TType::Identifier).value;

    if (name == "self") {
      if (!params->params.empty()) {
        end_node(nullptr, range);
        throw_error("\"self\" must appear first in method parameters.", range);
      }
      params->has_self = true;
      NODE_ALLOC(ASTParamDecl, param, range, _, this)
      param->tag = ASTParamDecl::Self;
      params->params.push_back(param);

      if (peek().type == TType::Mul) {
        eat();
        param->self.is_pointer = true;
      }

      if (peek().type != TType::RParen)
        expect(TType::Comma);

      continue;
    }

    if (peek().type == TType::Colon) {
      expect(TType::Colon);
      type = parse_type();
    }

    NODE_ALLOC(ASTParamDecl, param, range, _, this)
    param->tag = ASTParamDecl::Normal;
    param->normal.type = type;
    param->normal.name = name;

    if (peek().type == TType::Assign) {
      end_node(nullptr, range);
      throw_error("Ela does not support default parameters.", range);
    }

    params->params.push_back(param);
    end_node(param, subrange);

    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    } else
      break;
  }
  expect(TType::RParen);
  end_node(params, range);
  return params;
}

ASTFunctionDeclaration *Parser::parse_function_declaration(Token name) {
  expect(TType::Fn);

  NODE_ALLOC(ASTFunctionDeclaration, function, range, _, this)
  function->has_defer = false;

  if (peek().type == TType::GenericBrace) {
    function->generic_parameters = parse_generic_parameters();
  }

  auto last_func_decl = current_func_decl;
  Defer deferred([&] { current_func_decl = last_func_decl; });
  current_func_decl = function;

  if (range.begin > 0)
    range.begin = range.begin - 1;
  function->params = parse_parameters(function->generic_parameters);
  function->name = name.value;

  // check for definition.
  {
    auto sym = ctx.scope->local_lookup(name.value);
    if (sym && (sym->flags & SYMBOL_IS_FORWARD_DECLARED) == 0) {
      end_node(nullptr, range);
      throw_error(std::format("Redefinition of function {}", name.value), range);
    }
  }

  ctx.scope->insert(name.value, Type::invalid_id, function, SYMBOL_IS_FUNCTION);

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

  if (current_impl_decl) {
    if (function->params->has_self) {
      function->flags |= FUNCTION_IS_METHOD;
    } else {
      function->flags |= FUNCTION_IS_STATIC;
    }
  }

  // TODO: find a better solution to this.
  for (const auto &param : function->generic_parameters) {
    ctx.scope->types[param] = -2;
  }

  function->block = parse_block();
  function->block.get()->parent = function;

  for (const auto &stmt : function->block.get()->statements) {
    if (stmt->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      static_cast<ASTFunctionDeclaration *>(stmt)->flags |= FUNCTION_IS_LOCAL;
    }
  }

  end_node(function, range);
  function->scope = ctx.exit_scope();
  return function;
}

ASTEnumDeclaration *Parser::parse_enum_declaration(Token tok) {
  expect(TType::Enum);
  NODE_ALLOC(ASTEnumDeclaration, node, range, _, this)
  node->name = tok.value;
  expect(TType::LCurly);
  if (ctx.scope->find_type_id(tok.value, {}) != Type::invalid_id) {
    end_node(node, range);
    throw_error("Redefinition of enum " + tok.value.get_str(), range);
  }

  NODE_ALLOC(ASTLiteral, zero, lit_range, _1, this)
  zero->tag = ASTLiteral::Integer;
  zero->value = "0";

  NODE_ALLOC(ASTLiteral, one, lit_range2, _2, this)
  one->tag = ASTLiteral::Integer;
  one->value = "1";

  ASTExpr *last_value = zero;
  bool was_zero = true;
  Token add_token(tok.location, "+", TType::Add, TFamily::Operator);

  while (peek().type != TType::RCurly) {
    auto iden = expect(TType::Identifier).value;
    ASTExpr *value = nullptr;

    if (peek().type == TType::Assign) {
      expect(TType::Assign);
      value = parse_expr();
    } else {
      if (was_zero && last_value->get_node_type() == AST_NODE_LITERAL &&
          static_cast<ASTLiteral *>(last_value)->value == "0") {
        value = zero;
        was_zero = false;
      } else {
        NODE_ALLOC(ASTBinExpr, bin, range, _, this)
        bin->left = last_value;
        bin->right = one;
        bin->op = add_token;
        last_value = bin;
        value = bin;
        end_node(bin, range);
      }
    }
    if (peek().type == TType::Comma) {
      eat();
    }
    node->key_values.push_back({iden, value});
    last_value = value;
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
  expect(TType::RCurly);
  return node;
}

ASTImpl *Parser::parse_impl() {
  expect(TType::Impl);
  NODE_ALLOC_EXTRA_DEFER(ASTImpl, node, range, _, this, current_impl_decl = nullptr)

  ctx.set_scope();
  node->scope = ctx.exit_scope();

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  current_impl_decl = node;
  auto target = parse_type();

  // Handle 'impl INTERFACE for TYPE'
  // or normal 'impl TYPE'
  ASTType *interface = nullptr;
  if (peek().type == TType::For) {
    expect(TType::For);
    interface = parse_type();
    node->interface = target;
    node->target = interface;
  } else {
    node->target = target;
  }

  // TODO: make it so we dont have to get the scope of the type, we shouldn't be doing much typing
  // during parse time.

  node->target->accept(typer);
  auto type = global_get_type(node->target->resolved_type);
  node->target->resolved_type = Type::invalid_id;

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }
  auto block = parse_block(node->scope);
  end_node(node, range);

  // TODO: maybe do this differently
  // this is just so you can't call methods directly from within an impl without self
  node->scope->symbols.clear();

  for (const auto &statement : block->statements) {
    if (statement->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      auto function = static_cast<ASTFunctionDeclaration *>(statement);
      node->methods.push_back(function);
    } else {
      throw_error("invalid statement: only methods are allowed in 'impl's", statement->source_range);
    }
  }
  return node;
}

ASTDefer *Parser::parse_defer() {
  expect(TType::Defer);
  NODE_ALLOC(ASTDefer, node, range, _, this)
  node->statement = parse_statement();
  end_node(node, range);
  return node;
}

ASTWhere *Parser::parse_where_clause() {
  NODE_ALLOC(ASTWhere, node, range, _, this);
  expect(TType::Where);
  node->target_type = parse_type();
  expect(TType::Is);
  node->predicate = parse_type();
  while (peek().type == TType::And || peek().type == TType::Or) {
    auto op = eat();
    auto right = parse_type();
    NODE_ALLOC(ASTBinExpr, binexpr, range, _, this)
    binexpr->left = node->predicate;
    binexpr->right = right;
    binexpr->op = op;
    node->predicate = binexpr;
  }
  return node;
}

ASTInterfaceDeclaration *Parser::parse_interface_declaration(Token name) {
  expect(TType::Interface);
  auto previous = current_interface_decl;
  NODE_ALLOC_EXTRA_DEFER(ASTInterfaceDeclaration, node, range, _, this, { current_interface_decl = previous; });

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
      if (function->block.is_not_null()) {
        throw_error("Only forward declarations are allowed in interfaces currently", node->source_range);
      }
      node->methods.push_back(function);
    }
  }
  return node;
}

ASTStructDeclaration *Parser::parse_struct_declaration(Token name) {
  bool is_union = false;

  if (peek().type == TType::Struct) {
    expect(TType::Struct);
  } else {
    is_union = true;
    expect(TType::Union);
  }

  auto old = current_struct_decl;
  NODE_ALLOC(ASTStructDeclaration, node, range, _, this)
  node->is_union = is_union;
  current_struct_decl = node;

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto type_id = ctx.scope->find_type_id(name.value, {});

  if (type_id != Type::invalid_id) {
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
    type_id = ctx.scope->create_struct_type(name.value, {});
  }

  node->name = name.value;
  node->resolved_type = type_id;
  auto type = global_get_type(type_id);
  auto info = type->get_info()->as<StructTypeInfo>();
  info->scope = node->scope = create_child(ctx.scope);
  if (is_union)
    info->flags |= STRUCT_FLAG_IS_UNION;

  for (const auto &param : node->generic_parameters) {
    info->scope->types[param] = -2;
  }

  if (!semicolon()) {
    auto scope = info->scope;
    expect(TType::LCurly);
    std::vector<ASTNode *> directives;
    while (peek().type != TType::RCurly) {
      if (peek().type == TType::Directive) {
        eat();
        auto directive = process_directive(DIRECTIVE_KIND_STATEMENT, expect(TType::Identifier).value);
        if (directive && directive.get()->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
          node->subtypes.push_back(static_cast<ASTStructDeclaration *>(directive.get()));
        } else if (directive && directive.get()->get_node_type() == AST_NODE_DECLARATION) {
          ASTStructMember member{};
          auto _node = static_cast<ASTDeclaration *>(directive.get());
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
    node->scope = scope;
    info->flags &= ~STRUCT_FLAG_FORWARD_DECLARED;
    info->scope = node->scope;
  } else {
    info->flags |= STRUCT_FLAG_FORWARD_DECLARED;
    node->is_fwd_decl = true;
  }

  current_struct_decl = old;
  end_node(node, range);
  return node;
}

ASTTaggedUnionDeclaration *Parser::parse_tagged_union_declaration(Token name) {
  NODE_ALLOC(ASTTaggedUnionDeclaration, node, range, _, this)
  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }
  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }
  auto type = global_get_type(ctx.scope->create_tagged_union(name.value, nullptr));
  auto scope = create_child(ctx.scope);
  ctx.set_scope(scope);

  expect(TType::LCurly);

  while (peek().type != TType::RCurly) {
    ASTTaggedUnionVariant variant;
    variant.name = expect(TType::Identifier).value;
    if (peek().type == TType::Comma || peek().type == TType::RCurly) {
      variant.kind = ASTTaggedUnionVariant::NORMAL;
      node->variants.push_back(variant);
    } else if (peek().type == TType::LCurly) {
      variant.kind = ASTTaggedUnionVariant::STRUCT;
      eat();
      while (peek().type != TType::RCurly) {
        variant.struct_declarations.push_back(parse_declaration());
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RCurly);
      node->variants.push_back(variant);
    } else if (peek().type == TType::LParen) {
      variant.kind = ASTTaggedUnionVariant::TUPLE;
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
  node->name = name.value;
  node->scope = ctx.exit_scope();
  type->get_info()->scope = scope;
  node->resolved_type = type->id;
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

std::vector<ASTType *> Parser::parse_generic_arguments() {
  auto range = begin_node();
  expect(TType::GenericBrace);
  std::vector<ASTType *> params;
  while (peek().type != TType::RBrace) {
    params.push_back(parse_type());
    if (peek().type != TType::RBrace)
      expect(TType::Comma);
  }
  expect(TType::RBrace);
  end_node(nullptr, range);
  if (params.empty()) {
    throw_error("![] generic arguments cannot be empty!", range);
    ;
  }
  return params;
}

std::vector<GenericParameter> Parser::parse_generic_parameters() {
  auto range = begin_node();
  expect(TType::GenericBrace);
  std::vector<GenericParameter> params;
  while (peek().type != TType::RBrace) {
    params.emplace_back(expect(TType::Identifier).value);
    if (peek().type != TType::RBrace)
      expect(TType::Comma);
  }
  expect(TType::RBrace);
  end_node(nullptr, range);
  if (params.empty()) {
    throw_error("![] generic parameters cannot be empty!", range);
    ;
  }
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
        node->extensions.push_back({TYPE_EXT_ARRAY, expression});
      } else {
        // Syntactic sugar for doing int[] instead of List![int];
        auto type = ast_alloc<ASTType>();
        auto iden = ast_alloc<ASTIdentifier>();
        iden->value = "List";
        type->kind = ASTType::NORMAL;
        type->normal.base = iden;
        type->normal.generic_arguments.push_back(node);
        node = type;
      }
      expect(TType::RBrace);
    } else if (peek().type == TType::Mul) {
      expect(TType::Mul);
      node->extensions.push_back({TYPE_EXT_POINTER});
    } else {
      break;
    }
  }
}

ASTType *Parser::parse_function_type() {
  NODE_ALLOC(ASTType, output_type, range, _, this)
  output_type->kind = ASTType::FUNCTION;
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

Token Parser::expect(TType type) {
  fill_buffer_if_needed();
  if (peek().type != type) {
    SourceRange range = {
        .begin_location = peek().location,
        .end_location = peek().location,
        .begin = std::max(token_idx - 5, int64_t()),
        .end = token_idx + 5,
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
      .begin = token_idx,
      .begin_loc = (int64_t)location.line,
  };
}

void Parser::end_node(ASTNode *node, SourceRange &range) {
  range.end = token_idx;
  range.end_location = peek().location; // This may be inaccurate.
  if (node) {
    node->source_range = range;
  }
}

ASTLambda *Parser::parse_lambda() {
  expect(TType::Fn);
  NODE_ALLOC(ASTLambda, node, range, _, this);
  node->params = parse_parameters();
  if (peek().type == TType::Arrow) {
    eat();
    node->return_type = parse_type();
  } else {
    node->return_type = ASTType::get_void();
  }
  node->block = parse_block();

  if (current_func_decl.is_null()) {
    end_node(nullptr, range);
    throw_error("temporarily, lambda functions cannot be used at a global level, only within functions", range);
  }
  current_func_decl.get()->lambdas.push_back(node);
  return node;
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
  auto &state = states.back();
  state.input = "#import bootstrap;\n" + state.input; // TODO: do this in a more structured way. this works, but meh.
  state.input_len = state.input.length();
  fill_buffer_if_needed();
  typer = new Typer(context);
}

Parser::~Parser() { delete typer; }

Nullable<ASTBlock> Parser::current_block = nullptr;
