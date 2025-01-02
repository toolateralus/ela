#include "ast.hpp"

#include <algorithm>
#include <any>
#include <cassert>
#include <filesystem>
#include <format>
#include <fstream>
#include <set>
#include <string>
#include <unordered_set>

#include "constexpr.hpp"
#include "core.hpp"
#include "error.hpp"
#include "interned_string.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "type.hpp"
#include "visitor.hpp"

enum PreprocKind {
  PREPROC_IF,
  PREPROC_IFDEF,
  PREPROC_IFNDEF,
};

static void remove_body(Parser *parser) {
  parser->expect(TType::LCurly);
  int depth = 1;
  while (depth > 0) {
    if (parser->peek().type == TType::LCurly) depth++;
    if (parser->peek().type == TType::RCurly) depth--;
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

  if (kind == PREPROC_IFDEF) {  // Handling #ifdef
    auto symbol = parser->expect(TType::Identifier).value;
    executed = parser->ctx.scope->has_def(symbol);
  } else if (kind == PREPROC_IFNDEF) {  // Handling #ifndef
    auto symbol = parser->expect(TType::Identifier).value;
    executed = !parser->ctx.scope->has_def(symbol);
  } else if (kind == PREPROC_IF) {  // Handling #if
    auto condition = parser->parse_expr();
    auto value = evaluate_constexpr(condition, parser->ctx);
    executed = value.is_truthy();
  } else {
    throw_error(
        "INTERNAL COMPILER ERROR: Invalid #if/#ifdef/#ifndef, "
        "unrecognized kind.",
        {});
  }

  if (executed) {
    parser->expect(TType::LCurly);
    while (parser->peek().type != TType::RCurly) {
      list->statements.push_back(parser->parse_statement());
      while (parser->peek().type == TType::Semi) parser->eat();
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
          while (parser->peek().type == TType::Semi) parser->eat();
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
#ifdef _WIN32
        auto filename =
            std::filesystem::path("C:\\Program Files\\ela").string() + "\\" + iden.get_str() + ".ela";
#else
        auto filename =
            std::filesystem::path("/usr/local/lib/ela").string() + "/" + iden.get_str() + ".ela";
#endif
        // Right now, we just return noop if we're double including.
        if (import_set.contains(filename)) {
          return nullptr;
        }
        if (!std::filesystem::exists(filename)) {
          throw_error(std::format("Couldn't find imported module: {}", filename), {});
        }
        import_set.insert(filename);
        parser->states.push_back(Lexer::State::from_file(filename));
        parser->fill_buffer_if_needed();
        return nullptr;
    }},
    // #raw
    // string literals delimited by #raw and can span multiple lines.
    // u8 *string = #raw string literal goes here #raw
    {.identifier = "raw",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        InternedString string;
        while (parser->not_eof()) {
          if (parser->peek().type == TType::Directive &&
              parser->states.back().lookahead_buffer[1].value == "raw") {
            parser->eat();
            parser->eat();
            break;
          }
          string = {string.get_str() += parser->eat().value.get_str()};
        }
        auto literal = ast_alloc<ASTLiteral>();
        literal->tag = ASTLiteral::RawString;
        literal->value = string;
        return literal;
      },
    },

    // #read
    // Read a file into a string at compile time. Nice for embedding resources
    // into your program.
    // FEATURE: add a binary mode, where its just an array of chars or
    // something.
    {.identifier = "read",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) {
        auto filename = parser->expect(TType::String).value;
        if (!std::filesystem::exists(filename.get_str())) {
          throw_error(std::format("Couldn't find 'read' file: {}", filename), {});
        }
        std::stringstream ss;
        std::ifstream isftr(filename.get_str());
        ss << isftr.rdbuf();
        auto string = ast_alloc<ASTLiteral>();
        string->tag = ASTLiteral::RawString;
        string->value = ss.str();
        return string;
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
        return func;
    }},

    // #foreign
    // Declare a foreign function, like C's extern. Super janky and bad because
    // our boilerplate is crap and uses stdlib stuff.
    {.identifier = "foreign",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) {
        auto function = ast_alloc<ASTFunctionDeclaration>();
        auto range = parser->begin_node();
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
        function->name = name;
        if (parser->peek().type != TType::Arrow) {
          function->return_type = ASTType::get_void();
        } else {
          parser->expect(TType::Arrow);
          function->return_type = parser->parse_type();
        }
        function->meta_type = FunctionMetaType::FUNCTION_TYPE_FOREIGN;

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
        auto outer = ast_alloc<ASTType>();
        outer->flags = ASTTYPE_EMIT_OBJECT;
        outer->base = "Type";
        outer->extension_info = {.extensions = {TYPE_EXT_POINTER}};
        outer->pointing_to = type;
        return outer;
    }},

    // '#ctor' for declaring a constructor for a struct
    {.identifier = "ctor",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::DoubleColon);
        auto func_decl = parser->parse_function_declaration(get_unique_identifier());
        func_decl->flags |= (FUNCTION_IS_CTOR | FUNCTION_IS_METHOD);
        return func_decl;
    }},

    // '#dtor' for declaring a destructor for a struct
    {.identifier = "dtor",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::DoubleColon);
        auto func_decl = parser->parse_function_declaration(get_unique_identifier());
        func_decl->flags |= (FUNCTION_IS_DTOR | FUNCTION_IS_METHOD);
        return func_decl;
    }},

    // #make, which also serves as a casting and copy construction method, as
    // well
    // as normal ctors.
    {.identifier = "make",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto range = parser->begin_node();
        auto args = parser->parse_arguments();
        auto type = args->arguments[0];
        if (type->get_node_type() != AST_NODE_TYPE) {
          parser->end_node(type, range);
          throw_error("Expect a type as the first argument in a #make call.", range);
        }
        auto type_arg = static_cast<ASTType *>(type);
        args->arguments.erase(args->arguments.begin());
        auto make = ast_alloc<ASTMake>();
        make->type_arg = type_arg;
        make->arguments = args;
        make->kind = MAKE_CTOR;
        parser->end_node(make, range);

        if (type_arg->extension_info.is_pointer()) {
          make->kind = MAKE_CAST;
        }
        if (type_arg->extension_info.is_fixed_sized_array()) {
          throw_error("Cannot use #make on fixed array types.", range);
        }
        return make;
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
        auto type = global_get_type(enum_decl->type->resolved_type);
        auto info = (type->get_info()->as<EnumTypeInfo>());
        info->is_flags = true;
        return enum_decl;
    }},

    // #alias for making type aliases. #alias NewName :: OldName;
    {.identifier = "alias",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto name = parser->expect(TType::Identifier);
        parser->expect(TType::DoubleColon);
        auto aliased_type = parser->parse_type();
        if (parser->ctx.scope->types.contains(name.value)) {
          throw_error("Redefinition of type. ", aliased_type->source_range);
        }

        if (aliased_type->resolved_type != -1) {
          auto type = global_get_type(aliased_type->resolved_type);

          if (!type) {
            throw_error("Failed to get type for alias.", aliased_type->source_range);
          }

          if (type->is_kind(TYPE_FUNCTION)) {
            auto id = global_find_function_type_id(*(type->get_info()->as<FunctionTypeInfo>()),
                                                  aliased_type->extension_info);

            parser->ctx.scope->types[name.value] = id;
            return ast_alloc<ASTNoop>();
          }
        }

        auto id = parser->ctx.scope->find_type_id(aliased_type->base, aliased_type->extension_info);

        auto type = global_get_type(id);

        if (!type) {
          throw_error("Unable to create type alias: target type does not exist.",
                      aliased_type->source_range);
        }

        parser->ctx.scope->types[name.value] = id;
        return ast_alloc<ASTNoop>();
    }},

    // #self, return the type of the current declaring struct or union
    {.identifier = "self",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        ASTType *type;
        if (parser->current_union_decl) {
          type = parser->current_union_decl.get()->type;
        } else if (parser->current_struct_decl) {
          type = parser->current_struct_decl.get()->type;
        } else {
          throw_error(
              "can only use #self in unions and structs to get the "
              "type name of the current declaring type",
              {});
        }
        parser->append_type_extensions(type);
        return type;
    }},

    // #anon, for declaring anonymous sub-structs in unions primarily.
    {.identifier = "anon",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::DoubleColon);
        auto decl = parser->parse_struct_declaration(get_unique_identifier());
        auto t = global_get_type(decl->type->resolved_type);
        auto info = (t->get_info()->as<StructTypeInfo>());
        info->flags |= STRUCT_FLAG_IS_ANONYMOUS;
        return decl;
    }},

    // #operator, for operator overloads.
    {.identifier = "operator",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto range = parser->begin_node();
        parser->expect(TType::LParen);
        auto op = parser->eat();

        if (parser->peek().type == TType::RParen && parser->lookahead_buf()[1].type == TType::RParen) {
          parser->eat();
        } else if (parser->peek().type == TType::RBrace) {
          parser->eat();
        }

        parser->expect(TType::RParen);

        if (op.family != TFamily::Operator) {
          parser->end_node(nullptr, range);
          throw_error(std::format("Operator overload failed; {} was not a "
                                  "valid operator to overload",
                                  op.value),
                      range);
        }

        parser->expect(TType::DoubleColon);
        Token token;

        // Do we want to do it with the unique identifier?
        auto func_decl = parser->parse_function_declaration(get_unique_identifier());

        if (op.is_comp_assign() || op.type == TType::Increment || op.type == TType::Decrement) {
          func_decl->flags |= FUNCTION_IS_MUTATING;
        }

        func_decl->flags |= (FUNCTION_IS_OPERATOR | FUNCTION_IS_METHOD);

        func_decl->name = op;

        emit_warnings_or_errors_for_operator_overloads(op.type, func_decl->source_range);

        return func_decl;
    }},

    // #export, for exporting a non-mangled name to a dll or C library
    // primarily.
    // Equivalent to marking a function extern "C" in C++.
    {.identifier = "export",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto name = parser->expect(TType::Identifier);
        parser->expect(TType::DoubleColon);
        if (parser->peek().type == TType::Struct) {
          auto decl = parser->parse_struct_declaration(name);
          decl->is_extern = true;
          return decl;
        }
        auto func_decl = parser->parse_function_declaration(name);
        func_decl->flags |= FUNCTION_IS_EXPORTED;
        return func_decl;
    }},

    // #typeid, integer version of #type. can be used to compare types without
    // the pointers.
    {.identifier = "typeid",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::LParen);
        auto type = parser->parse_type();

        parser->expect(TType::RParen);
        auto id = std::any_cast<int>(type->accept(parser->typer));

        auto literal = ast_alloc<ASTLiteral>();
        literal->tag = ASTLiteral::Integer;
        literal->value = std::to_string(id);
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
        decl->bitsize = size;
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
        auto list = ast_alloc<ASTStatementList>();
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
        return list;
    }},

    // #ifndef, conditional compilation based on a #def not being present.
    {.identifier = "ifndef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto list = ast_alloc<ASTStatementList>();
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
        return list;
    }},

    // #if, conditional compilation based on compile time value.
    {.identifier = "if",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto list = ast_alloc<ASTStatementList>();
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
        return list;
    }},

    // #region, for named/unnnamed regions. just for organization, has no compilation implications.
    // can have anything between the #region directive and the {} block
    // #region My code region 1 {...} is legal.
    {.identifier = "region",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        while (parser->peek().type != TType::LCurly) {
          parser->eat();
        }
        auto list = ast_alloc<ASTStatementList>();
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
  auto range = begin_node();
  auto program = ast_alloc<ASTProgram>();

  while (true) {
    if (peek().type == TType::Eof && !states.empty()) {
      states.pop_back();
      if (!states.empty()) {
        std::filesystem::current_path(states.back().path.parent_path());
      }
    }

    while (semicolon()) eat();

    if (peek().type == TType::Eof && states.empty()) {
      break;
    }

    if (peek().type == TType::Directive) {
      eat();
      auto identifer = expect(TType::Identifier).value;
      auto result = process_directive(DIRECTIVE_KIND_STATEMENT, identifer);
      if (result.is_not_null()) {
        auto statement = static_cast<ASTStatement *>(result.get());
        if (statement) {
          program->statements.push_back(statement);
        }
      }
      if (semicolon()) eat();
      continue;
    }

    program->statements.push_back(parse_statement());

    while (semicolon()) eat();
  }
  end_node(program, range);
  return program;
}

ASTArguments *Parser::parse_arguments() {
  auto range = begin_node();
  auto args = ast_alloc<ASTArguments>();
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
  auto range = begin_node();
  ASTCall *call = ast_alloc<ASTCall>();
  call->function = function;
  call->arguments = parse_arguments();
  end_node(call, range);
  return call;
}

ASTExpr *Parser::parse_expr(Precedence precedence) {
  auto range = begin_node();
  ASTExpr *left = parse_unary();
  while (true) {
    Precedence token_precedence = get_operator_precedence(peek());
    if (token_precedence <= precedence) break;
    auto op = eat();
    auto right = parse_expr(token_precedence);
    auto binexpr = ast_alloc<ASTBinExpr>();
    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op;
    left = binexpr;
  }
  end_node(left, range);
  return left;
}

ASTExpr *Parser::parse_unary() {
  auto range = begin_node();

  // bitwise not is a unary expression because arrays use it as a pop operator,
  // and sometimes you might want to ignore it's result.
  if (peek().type == TType::Add || peek().type == TType::Sub || peek().type == TType::LogicalNot ||
      peek().type == TType::Not || peek().type == TType::Increment || peek().type == TType::Decrement ||
      peek().type == TType::Mul || peek().type == TType::And || peek().type == TType::Not) {
    auto op = eat();
    auto expr = parse_unary();
    auto unaryexpr = ast_alloc<ASTUnaryExpr>();
    auto is_rvalue = expr->get_node_type() == AST_NODE_LITERAL || expr->get_node_type() == AST_NODE_CALL;
    // don't need to do this if we already got one of the previous ones.
    auto ctor = is_rvalue || [&] {
      if (expr->get_node_type() != AST_NODE_MAKE) {
        return false;
      }
      auto make = static_cast<ASTMake *>(expr);
      if (make && (make->kind == MAKE_CTOR || make->kind == MAKE_COPY_CTOR)) {
        return true;
      }
      return false;
    }();
    if ((is_rvalue || ctor) && (op.type == TType::And || op.type == TType::Mul)) {
      end_node(nullptr, range);
      throw_error(
          "Cannot take the address of, or dereference a literal or "
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
  auto range = begin_node();
  auto left = parse_primary();
  if (peek().type == TType::Range) {
    eat();
    auto right = parse_expr();
    auto node = ast_alloc<ASTRange>();
    node->left = left;
    node->right = right;
    return node;
  }
  if (peek().type == TType::Increment || peek().type == TType::Decrement) {
    auto unary = ast_alloc<ASTUnaryExpr>();
    unary->operand = left;
    unary->op = peek();
    eat();
    end_node(unary, range);
    return unary;
  }
  // build dot and subscript expressions
  while (peek().type == TType::DoubleColon || peek().type == TType::Dot || peek().type == TType::LBrace ||
         peek().type == TType::LParen) {
    if (peek().type == TType::LParen) {
      left = parse_call(left);
    } else if (peek().type == TType::Dot) {
      eat();
      auto dot = ast_alloc<ASTDotExpr>();
      dot->base = left;
      dot->member_name = expect(TType::Identifier).value;
      left = dot;
    } else if (peek().type == TType::DoubleColon) {
      eat();
      auto dot = ast_alloc<ASTScopeResolution>();
      dot->base = left;
      dot->member_name = expect(TType::Identifier).value;
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

  end_node(left, range);
  return left;
}

ASTExpr *Parser::parse_primary() {
  auto tok = peek();

  // if theres a #... that returns a value, use that.
  if (auto directive_expr = try_parse_directive_expr()) {
    return directive_expr.get();
  }

  // special unary for allocation, new delete.
  if (tok.type == TType::New || tok.type == TType::Delete) {
    eat();
    ASTAllocate::Kind kind = tok.type == TType::New ? ASTAllocate::Kind::New : ASTAllocate::Kind::Delete;
    auto node = ast_alloc<ASTAllocate>();
    if (kind == ASTAllocate::New) {
      auto type = parse_type();
      type->extension_info.extensions.push_back(TYPE_EXT_POINTER);
      node->type = type;
    }
    Nullable<ASTArguments> args = nullptr;
    if (peek().type == TType::LParen) {
      args = parse_arguments();
    }
    node->arguments = args;
    node->kind = kind;
    return node;
  }

  auto range = begin_node();

  switch (tok.type) {
    case TType::Switch: {
      expect(TType::Switch);
      auto expr = parse_expr();
      expect(TType::LCurly);
      std::vector<SwitchCase> cases;
      while (peek().type != TType::RCurly) {
        SwitchCase _case;
        _case.expression = parse_expr();
        expect(TType::Colon);
        _case.block = parse_block();
        cases.push_back(_case);
      }
      expect(TType::RCurly);
      auto node = ast_alloc<ASTSwitch>();
      node->cases = cases;
      node->target = expr;
      end_node(node, range);
      return node;
    }
    case TType::Char: {
      eat();
      auto node = ast_alloc<ASTLiteral>();
      node->tag = ASTLiteral::Char;
      node->value = tok.value;
      end_node(node, range);
      return node;
    }
    case TType::DoubleColon: {
      eat();
      if (peek().type != TType::Identifier) {
        end_node(nullptr, range);
        throw_error(
            "::Something syntax is only for using enum variants that are "
            "in your current scope.",
            range);
      }
      auto scope_res = ast_alloc<ASTScopeResolution>();
      scope_res->base = nullptr;
      scope_res->member_name = expect(TType::Identifier).value;
      end_node(scope_res, range);
      return scope_res;
    }
    case TType::Dollar: {
      auto range = begin_node();
      eat();
      auto str = expect(TType::String);
      auto node = ast_alloc<ASTLiteral>();
      node->tag = ASTLiteral::InterpolatedString;
      auto lexer_state = Lexer::State::from_string(str.value.get_str());
      states.push_back(lexer_state);
      std::vector<ASTExpr *> exprs;
      fill_buffer_if_needed();
      while (lexer_state == states.back() && peek().type != TType::Eof) {
        if (peek().type == TType::LCurly) {
          eat();
          exprs.push_back(parse_expr());
        } else {
          eat();
        }
      }
      if (states.back() == lexer_state) {
        states.pop_back();
      }
      auto pos = 0;
      std::string value = str.value.get_str();
      while (pos < value.length()) {
        if (value[pos] == '{') {
          pos++;
          auto start = pos;
          while (value[pos] != '}') {
            pos++;
          }
          value.erase(start, pos - start);
        } else {
          ++pos;
        }
      }
      node->value = {value};
      node->interpolated_values = exprs;
      end_node(node, range);
      return node;
    }
    case TType::LCurly: {
      auto range = begin_node();
      eat();
      auto init_list = ast_alloc<ASTInitializerList>();
      while (peek().type != TType::RCurly) {
        init_list->expressions.push_back(parse_expr());
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RCurly);
      end_node(init_list, range);
      return init_list;
    }
    case TType::Identifier: {
      auto range = begin_node();
      if (ctx.scope->find_type_id(tok.value, {}) != -1) {
        auto type = parse_type();
        if (peek().type == TType::LCurly) {
          auto init_list = parse_expr();
          auto make = ast_alloc<ASTMake>();
          auto args = ast_alloc<ASTArguments>();
          args->arguments.push_back(init_list);
          make->type_arg = type;
          make->arguments = args;
          return make;
        } else {
          return type;
        }
      }
      eat();
      auto iden = ast_alloc<ASTIdentifier>();
      iden->value = tok.value;
      end_node(iden, range);
      return iden;
    }
    case TType::Null: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Null;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::True: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::False: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Integer: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Integer;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::Float: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::Float;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::String: {
      auto range = begin_node();
      eat();
      auto literal = ast_alloc<ASTLiteral>();
      literal->tag = ASTLiteral::String;
      literal->value = tok.value;
      end_node(literal, range);
      return literal;
    }
    case TType::LParen: {
      auto range = begin_node();
      expect(TType::LParen);  // (
      const auto lookahead = lookahead_buf();

      // for (Type)expr;
      if (ctx.scope->find_type_id(peek().value, {}) != -1 || peek().type == TType::LT) {
        // CLEANUP: We probably don't wanna use ASTMake for so many things,
        // but for now it's okay. Actually, we don't want ASTMake at all, it
        // should get eliminated and ASTConstruct and ASTCast should probably be
        // added to replace it. This would help us have a more consistent and
        // clear syntax.

        auto type = parse_type();
        auto node = ast_alloc<ASTMake>();
        expect(TType::RParen);
        node->type_arg = type;
        node->kind = MAKE_CAST;
        node->arguments = ast_alloc<ASTArguments>();
        node->arguments->arguments.push_back(parse_unary());
        return node;
      }

      auto expr = parse_expr();

      if (peek().type == TType::Comma) {
        eat();
        auto exprs = std::vector<ASTExpr *>{
          expr
        };
        while (peek().type != TType::RParen) {
          exprs.push_back(parse_expr());
          if (peek().type == TType::Comma) eat();
        }
        expect(TType::RParen); 
        auto tuple = ast_alloc<ASTTuple>();
        tuple->values = exprs;
        tuple->type = ast_alloc<ASTType>();
        return tuple;
      }

      if (peek().type != TType::RParen) {
        throw_error("Expected ')'", SourceRange{token_idx - 5, token_idx});
      }

      eat();  // consume ')'
      end_node(expr, range);
      return expr;
    }
    default: {
      throw_error(
          std::format("Invalid primary expression. Token: '{}'... Type: '{}'", tok.value, TTypeToString(tok.type)),
          {token_idx - 5, token_idx});
      return nullptr;
    }
  }
}

ASTType *Parser::parse_type() {
  auto range = begin_node();

  // TODO: refactor tuples to use a different set of symbols.
  // ! Nested tuples think that >> closing them is a shift right symbol
  if (peek().type == TType::LParen) {
    eat();
    std::vector<ASTType *> types;
    while (peek().type != TType::RParen) {
      types.push_back(parse_type());
      if (peek().type == TType::Comma) eat();
    }
    expect(TType::RParen);
    auto node = ast_alloc<ASTType>();
    node->resolved_type = -1;
    node->flags = 0;
    node->tuple_types = types;
    node->flags |= ASTTYPE_IS_TUPLE;
    // grab up more extensions if they exist.
    append_type_extensions(node);
    return node;
  }

  // parse #self types.
  if (peek().type == TType::Directive) {
    auto expr = try_parse_directive_expr().get();
    if (expr->get_node_type() != AST_NODE_TYPE) {
      throw_error(
          "unable to get type from directive expression where a type "
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
  auto node = ast_alloc<ASTType>();
  node->base = base;
  append_type_extensions(node);

  end_node(node, range);
  return node;
}

ASTStatement *Parser::parse_statement() {
  auto range = begin_node();
  auto tok = peek();

  while (tok.type == TType::Semi) {
    eat();
    tok = peek();
  }

  if (peek().type == TType::Identifier && 
      lookahead_buf()[1].type == TType::DoubleColon &&
      lookahead_buf()[2].type == TType::Identifier) {
    auto expr = ast_alloc<ASTExprStatement>();
    expr->expression = parse_expr();
    end_node(expr, range);
    return expr;
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
  bool is_double_colon_with_valid_following =
      lookahead_buf()[1].type == TType::DoubleColon && lookahead_buf()[2].family != TFamily::Keyword;

  if (tok.type == TType::Identifier && (is_colon_or_colon_equals || is_double_colon_with_valid_following)) {
    auto decl = parse_declaration();
    end_node(decl, range);
    return decl;
  }

  // * '#' Directives.
  if (tok.type == TType::Directive) {
    eat();
    auto statement = dynamic_cast<ASTStatement *>(
        process_directive(DIRECTIVE_KIND_STATEMENT, expect(TType::Identifier).value).get());
    if (!statement) {
      throw_error(std::format("Directive '{}' did not return a valid statement node", tok.value), range);
    }
    end_node(statement, range);
    return statement;
  }

  // * Control flow
  {
    if (tok.type == TType::LCurly) {
      auto block = parse_block();
      end_node(block, range);
      return block;
    }

    if (tok.type == TType::Return) {
      expect(TType::Return);
      auto return_node = ast_alloc<ASTReturn>();
      if (peek().type != TType::Semi) {
        return_node->expression = parse_expr();
      }
      end_node(return_node, range);
      return return_node;
    }

    if (tok.type == TType::Break) {
      eat();
      auto _break = ast_alloc<ASTBreak>();
      end_node(_break, range);
      return _break;
    }

    if (tok.type == TType::Continue) {
      eat();
      auto _continue = ast_alloc<ASTContinue>();
      end_node(_continue, range);
      return _continue;
    }

    if (tok.type == TType::For) {
      eat();
      auto node = ast_alloc<ASTFor>();

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
        throw_error(
            "Invalid for syntax. expected 'for i in 0..10 || for elem in "
            "iterable || for *elem in iterable",
            range);
      }

      node->block = parse_block();
      end_node(node, range);
      return node;
    }

    if (tok.type == TType::While) {
      eat();
      auto node = ast_alloc<ASTWhile>();
      if (peek().type != TType::LCurly) {
        node->condition = parse_expr();
      }
      node->block = parse_block();
      end_node(node, range);
      return node;
    }

    if (tok.type == TType::If) {
      eat();
      auto node = ast_alloc<ASTIf>();
      node->condition = parse_expr();

      if (peek().type == TType::Then) {
        eat();
        node->block = ast_alloc<ASTBlock>();
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
        auto node_else = ast_alloc<ASTElse>();
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
      end_node(node, range);
      return node;
    }
    if (peek().type == TType::Struct) {
      auto struct_decl = parse_struct_declaration(tok);
      end_node(struct_decl, range);
      return struct_decl;
    }
    if (peek().type == TType::Enum) {
      auto enum_decl = parse_enum_declaration(tok);
      end_node(enum_decl, range);
      return enum_decl;
    }
    if (peek().type == TType::Union) {
      auto union_decl = parse_union_declaration(tok);
      end_node(union_decl, range);
      return union_decl;
    }

    throw_error("invalid :: statement, expected '(' (for a function), 'struct', or 'enum", range);
  }


  // ! BUG:: Somehow we broke 'a.b++' expressions here, it parses the dot then hits the ++; as if that's valid.
  // * Expression statements.
  {
    auto next = lookahead_buf()[1];
    auto next_next = lookahead_buf()[2];
    // Both postfix and prefix (inc/dec)rement
    const bool is_increment_or_decrement = (next.type == TType::Increment || next.type == TType::Decrement && next_next.type != TType::Semi) || tok.type == TType::Increment || tok.type == TType::Decrement;

    // subscript assignment or dot assign/ call statement.
    const bool is_identifier_with_lbrace_or_dot =
        tok.type == TType::Identifier && (next.type == TType::LBrace || next.type == TType::Dot);

    const bool is_call = next.type == TType::LParen;

    const bool is_assignment_or_compound =
        next.type == TType::Assign || next.type == TType::Comma || next.is_comp_assign();

    const bool is_deref = tok.type == TType::Mul;

    const bool is_special_case = tok.type == TType::Delete ||  // delete statement
                                 tok.type == TType::LParen ||  // possible parenthesized dereference or something.
                                 tok.type == TType::Erase ||   // ~vector, for popping and ignoring.
                                 tok.type == TType::Switch;

    if (is_call || is_increment_or_decrement || is_identifier_with_lbrace_or_dot || is_assignment_or_compound ||
        is_deref || is_special_case) {
      auto statement = ast_alloc<ASTExprStatement>();
      statement->expression = parse_expr();

      if (ASTSwitch *_switch = dynamic_cast<ASTSwitch *>(statement->expression)) {
        _switch->is_statement = true;
      }

      end_node(statement, range);
      return statement;
    }
  }

  end_node(nullptr, range);

  //*  Failure to parse errors

  {
    if (tok.family == TFamily::Operator) {
      throw_error(std::format("Unexpected operator: {} '{}'", TTypeToString(tok.type), tok.value), range);
    }

    if (tok.family == TFamily::Literal) {
      eat();
      throw_error(std::format("Unexpected literal: {} .. {}", tok.value, TTypeToString(tok.type)), range);
    }

    if (tok.family == TFamily::Keyword) {
      eat();
      throw_error(std::format("Unexpected keyword: {}", tok.value), range);
    }

    if (ctx.scope->lookup(tok.value)) {
      eat();
      throw_error(std::format("Unexpected variable {}", tok.value), range);
    }

    if (ctx.scope->find_type_id(tok.value, {}) == -1) {
      eat();
      throw_error(std::format("Use of an undeclared type or identifier: {}", tok.value), range);
    }

    eat();
    throw_error(std::format("Unexpected token when parsing statement: {}.. This "
                            "is likely an undefined type.",
                            tok.value),
                range);
  }
}

ASTTupleDeconstruction *Parser::parse_multiple_asssignment() {
  auto range = begin_node();
  auto first = parse_primary();
  auto node = ast_alloc<ASTTupleDeconstruction>();
  node->idens.push_back(static_cast<ASTIdentifier *>(first));
  while (peek().type == TType::Comma) {
    eat();
    auto expr = parse_primary();
    if (auto iden = dynamic_cast<ASTIdentifier *>(expr)) {
      node->idens.push_back(iden);
    } else {
      end_node(nullptr, range);
      throw_error(
          "Can only have identifiers on the left hand side of a tuple "
          "deconstruction expressions",
          range);
    }
  }
  if (peek().type == TType::ColonEquals) {
    eat();
    node->right = parse_expr();
  } else {
    // TODO: allow typed tuple deconstructions.
    end_node(nullptr, range);
    throw_error(
        "Currently, you cannot have an explicitly typed tuple "
        "deconstruction. Use a, b, c := ....",
        range);
  }
  return node;
}

ASTDeclaration *Parser::parse_declaration() {
  auto range = begin_node();
  ASTDeclaration *decl = ast_alloc<ASTDeclaration>();
  auto iden = eat();
  decl->name = iden;

  static std::set<std::string> reserved = {
      "asm",     "double",   "new",      "switch",   "auto",      "else",    "operator", "template", "break",  "enum",
      "private", "this",     "case",     "extern",   "protected", "throw",   "catch",    "float",    "public", "try",
      "char",    "for",      "register", "typedef",  "class",     "friend",  "return",   "union",    "const",  "goto",
      "short",   "unsigned", "continue", "if",       "signed",    "virtual", "default",  "inline",   "sizeof", "void",
      "delete",  "int",      "static",   "volatile", "do",        "long",    "struct",   "while"};

  if (ctx.scope->find_type_id(iden.value, {}) != -1 || keywords.contains(iden.value.get_str()) ||
      reserved.contains(iden.value.get_str())) {
    end_node(nullptr, range);
    throw_error(
        "Invalid variable declaration: a type or keyword exists with "
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
  ctx.scope->insert(iden.value, -1);
  return decl;
}

ASTBlock *Parser::parse_block() {
  auto range = begin_node();
  expect(TType::LCurly);
  ASTBlock *block = ast_alloc<ASTBlock>();
  ctx.set_scope();
  while (peek().type != TType::RCurly) {
    if (peek().type == TType::Eof) {
      end_node(nullptr, range);
      throw_error("Imbalanced '{' and '}'", range);
    }
    auto statement = parse_statement();
    block->statements.push_back(statement);
    while (semicolon()) eat();
  }
  expect(TType::RCurly);
  block->scope = ctx.exit_scope();
  end_node(block, range);
  return block;
}

ASTParamsDecl *Parser::parse_parameters() {
  auto range = begin_node();
  ASTParamsDecl *params = ast_alloc<ASTParamsDecl>();
  expect(TType::LParen);
  ASTType *type = nullptr;
  // TODO: simplify me.
  // TODO: Make it so repeat type params are post-iden, such as '(a, b, c: s32)' instead of '(a: s32, b, c)'
  while (peek().type != TType::RParen) {
    auto subrange = begin_node();

    if (peek().type == TType::Varargs) {
      eat();
      if (!current_func_decl) {
        throw_error(
            "Cannot use varargs outside of a function declaration. "
            "Only use this for #foreign functions.",
            range);
      }
      current_func_decl.get()->flags |= FUNCTION_IS_VARARGS;
      continue;
    }

    auto name = expect(TType::Identifier).value;
    if (!type || peek().type == TType::Colon) expect(TType::Colon);

    auto next = peek();

    // if the cached type is null, or if the next token isn't
    // a valid type, we parse the type.
    // this should allow us to do things like func :: fn(int a, b, c) {}
    if (next.type == TType::Directive || !type || ctx.scope->find_type_id(next.value, {}) != -1 ||
        next.type == TType::Fn) {
      type = parse_type();
    }

    auto param = ast_alloc<ASTParamDecl>();
    param->type = type;
    param->name = name;

    if (peek().type == TType::Assign) {
      eat();
      param->default_value = parse_expr();
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
  auto range = begin_node();
  auto function = ast_alloc<ASTFunctionDeclaration>();
  auto last_func_decl = current_func_decl;
  Defer deferred([&] { current_func_decl = last_func_decl; });
  current_func_decl = function;

  if (range.begin > 0) range.begin = range.begin - 1;
  function->params = parse_parameters();
  function->name = name;

  auto sym = ctx.scope->local_lookup(name.value);

  if (sym) {
    sym->flags |= SYMBOL_HAS_OVERLOADS;
  } else {
    // to allow for recursion
    ctx.scope->insert(name.value, -1);
    auto sym = ctx.scope->lookup(name.value);
    sym->flags |= SYMBOL_IS_FUNCTION;
    sym->declaring_node = function;
  }

  if (peek().type != TType::Arrow) {
    function->return_type = ASTType::get_void();
  } else {
    expect(TType::Arrow);
    function->return_type = parse_type();
  }

  if (peek().type == TType::Semi) {
    function->flags |= FUNCTION_IS_FORWARD_DECLARED;
    end_node(function, range);
    current_func_decl = last_func_decl;
    return function;
  }

  function->block = parse_block();
  end_node(function, range);
  return function;
}

ASTEnumDeclaration *Parser::parse_enum_declaration(Token tok) {
  expect(TType::Enum);
  auto range = begin_node();
  auto node = ast_alloc<ASTEnumDeclaration>();
  node->type = ast_alloc<ASTType>();
  node->type->base = tok.value;
  expect(TType::LCurly);
  if (ctx.scope->find_type_id(tok.value, {}) != -1) {
    end_node(node, range);
    throw_error("Redefinition of enum " + tok.value.get_str(), range);
  }
  while (peek().type != TType::RCurly) {
    auto iden = expect(TType::Identifier).value;
    ASTExpr *value = nullptr;
    if (peek().type == TType::Assign) {
      expect(TType::Assign);
      value = parse_expr();
    }
    if (peek().type == TType::Comma) {
      eat();
    }
    node->key_values.push_back({iden, value});
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
  auto identifer = node->type->base;
  node->type->resolved_type = ctx.scope->create_enum_type(identifer.get_str(), keys, node->is_flags);
  expect(TType::RCurly);
  return node;
}

ASTStructDeclaration *Parser::parse_struct_declaration(Token name) {
  auto range = begin_node();
  expect(TType::Struct);

  auto old = current_struct_decl;
  auto decl = ast_alloc<ASTStructDeclaration>();
  current_struct_decl = decl;

  auto type_id = ctx.scope->find_type_id(name.value, {});

  if (type_id != -1) {
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

  decl->type = ast_alloc<ASTType>();
  ;
  decl->type->base = name.value;
  decl->type->extension_info = {};
  decl->type->resolved_type = type_id;

  if (!semicolon()) {
    auto block = parse_block();
    block->scope->is_struct_or_union_scope = true;
    for (const auto &statement : block->statements) {
      if (statement->get_node_type() == AST_NODE_DECLARATION) {
        decl->fields.push_back(static_cast<ASTDeclaration *>(statement));
      } else if (statement->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
        decl->methods.push_back(static_cast<ASTFunctionDeclaration *>(statement));
      } else {
        throw_error("Non-field or non-method declaration not allowed in struct.", statement->source_range);
      }
    }
    decl->scope = block->scope;
  } else {
    Type *t = global_get_type(type_id);
    auto info = (t->get_info()->as<StructTypeInfo>());
    info->flags |= STRUCT_FLAG_FORWARD_DECLARED;
    decl->is_fwd_decl = true;
  }

  auto info = global_get_type(type_id)->get_info()->as<StructTypeInfo>();

  info->flags &= ~STRUCT_FLAG_FORWARD_DECLARED;
  info->scope = decl->scope;

  current_struct_decl = old;
  end_node(decl, range);
  return decl;
}

ASTUnionDeclaration *Parser::parse_union_declaration(Token name) {
  auto range = begin_node();
  auto node = ast_alloc<ASTUnionDeclaration>();
  current_union_decl = node;

  node->name = name;
  node->type = ast_alloc<ASTType>();
  node->type->base = name.value;

  auto id = ctx.scope->find_type_id(name.value, {});

  if (id != -1) {
    auto type = global_get_type(id);

    if (!type->is_kind(TYPE_UNION)) {
      end_node(nullptr, range);
      throw_error("cannot redefine already existing type", range);
    }
    auto info = (type->get_info()->as<UnionTypeInfo>());

    if ((info->flags & UNION_IS_FORWARD_DECLARED) == 0) {
      end_node(nullptr, range);
      throw_error("cannot redefine already existing union type", range);
    }
  }

  Defer _([&] { current_union_decl = nullptr; });

  expect(TType::Union);

  auto type_id = ctx.scope->find_type_id(name.value, {});
  auto type = global_get_type(type_id);

  if (type && type->is_kind(TYPE_UNION)) {
    auto info = (type->get_info()->as<UnionTypeInfo>());
    if ((info->flags & UNION_IS_FORWARD_DECLARED) == 0) {
      end_node(nullptr, range);
      throw_error("Redefinition of non forward-declared union type.", range);
    }
    info->flags &= ~UNION_IS_FORWARD_DECLARED;
  } else {
    // if we didn't find a foward declaration, instantiate a new empty union
    // type, and resolve the node type, cause might as well.
    type_id = node->type->resolved_type = ctx.scope->create_union_type(name.value, nullptr, UNION_IS_NORMAL);
  }

  if (peek().type == TType::Semi) {
    eat();
    node->is_fwd_decl = true;
    type = global_get_type(type_id);
    auto info = (type->get_info()->as<UnionTypeInfo>());
    info->flags |= UNION_IS_FORWARD_DECLARED;
    return node;
  }

  std::vector<ASTDeclaration *> fields;
  std::vector<ASTFunctionDeclaration *> methods;
  std::vector<ASTStructDeclaration *> structs;
  auto block = parse_block();
  block->scope->is_struct_or_union_scope = true;

  auto scope = block->scope;

  for (auto &statement : block->statements) {
    if (statement->get_node_type() == AST_NODE_DECLARATION) {
      fields.push_back(static_cast<ASTDeclaration *>(statement));
    } else if (statement->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
      methods.push_back(static_cast<ASTFunctionDeclaration *>(statement));
    } else if (statement->get_node_type() == AST_NODE_STRUCT_DECLARATION) {
      auto struct_decl = static_cast<ASTStructDeclaration *>(statement);
      auto type = global_get_type(struct_decl->type->resolved_type);
      auto info = (type->get_info()->as<StructTypeInfo>());
      if ((info->flags & STRUCT_FLAG_IS_ANONYMOUS) == 0) {
        throw_error("can only use #anon struct declarations within union types.", node->source_range);
      }
      structs.push_back(struct_decl);
      for (const auto &field : struct_decl->fields) {
        block->scope->insert(field->name.value, field->type->resolved_type);
      }
    } else {
      throw_error("Non method/field declarations not allowed in union", range);
    }
  }

  node->fields = fields;
  node->methods = methods;
  node->structs = structs;
  node->scope = block->scope;

  type = global_get_type(type_id);
  auto info = (type->get_info()->as<UnionTypeInfo>());

  info->scope = scope;

  end_node(node, range);
  return node;
}

Nullable<ASTExpr> Parser::try_parse_directive_expr() {
  if (peek().type == TType::Directive) {
    eat();
    auto identifier = expect(TType::Identifier);
    Nullable<ASTNode> node = process_directive(DIRECTIVE_KIND_EXPRESSION, identifier.value);

    auto expr = Nullable<ASTExpr>(dynamic_cast<ASTExpr *>(node.get()));
    if (expr.is_not_null()) {
      return expr;
    } else {
      throw_error(
          "Invalid directive in expression: directives in "
          "expressions must return a value.",

          {std::max(token_idx - 5, int64_t()), std::max(token_idx + 5, int64_t())});
    }
  }
  return nullptr;
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

void Parser::append_type_extensions(ASTType *node) {
  while (true) {
    if (peek().type == TType::LBrace) {
      expect(TType::LBrace);
      if (peek().type != TType::RBrace) {
        auto size = parse_expr();

        if (size->get_node_type() == AST_NODE_TYPE) {
          node->extension_info.extensions.push_back(TYPE_EXT_MAP);
          auto type = static_cast<ASTType *>(size);
          node->extension_info.key_type = ctx.scope->find_type_id(type->base, type->extension_info);
          expect(TType::RBrace);
          continue;
        }

        node->extension_info.extensions.push_back(TYPE_EXT_ARRAY);
        node->extension_info.array_sizes.push_back(size);
      } else {
        node->extension_info.extensions.push_back(TYPE_EXT_ARRAY);
        node->extension_info.array_sizes.push_back(nullptr);
      }
      expect(TType::RBrace);
    } else if (peek().type == TType::Mul) {
      expect(TType::Mul);
      node->extension_info.extensions.push_back(TYPE_EXT_POINTER);
    } else {
      break;
    }
  }
}

ASTType *Parser::parse_function_type() {
  auto output_type = ast_alloc<ASTType>();
  append_type_extensions(output_type);
  FunctionTypeInfo info{};
  auto param_types = parse_parameter_types();

  if (peek().type == TType::Arrow) {
    eat();
    auto return_type = parse_type();
    info.return_type = ctx.scope->find_type_id(return_type->base, return_type->extension_info);
  } else {
    info.return_type = void_type();
  }

  for (size_t i = 0; i < param_types.size(); ++i) {
    info.parameter_types[i] = ctx.scope->find_type_id(param_types[i]->base, param_types[i]->extension_info);
    info.params_len++;
  }

  output_type->resolved_type = global_find_function_type_id(info, output_type->extension_info);
  auto t = global_get_type(output_type->resolved_type);
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
    case TType::Concat:  // this is for appending to arrays
    case TType::Erase:   // this is for erase elements for arrays
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
    type->base = "void";
    type->resolved_type = void_type();
    return type;
  }();
  return type;
}

Token Parser::eat() {
  all_tokens.push_back(peek());
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
    SourceRange range = {std::max(token_idx - 5, int64_t()), token_idx + 5};
    throw_error(std::format("Expected {}, got {} : {}", TTypeToString(type), TTypeToString(peek().type), peek().value),
                range);
  }
  return eat();
}

SourceRange Parser::begin_node() { return SourceRange{.begin = token_idx, .begin_loc = (int64_t)peek().location.line}; }

void Parser::end_node(ASTNode *node, SourceRange &range) {
  range.end = token_idx;
  if (node) node->source_range = range;
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
