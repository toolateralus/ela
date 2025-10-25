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
#include "value.hpp"
#include "visitor.hpp"
#include "interpreter.hpp"
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
    if (parser->peek().type == TType::LCurly) depth++;
    if (parser->peek().type == TType::RCurly) depth--;
    parser->eat();
  }
}

static void remove_preproc(Parser *parser) {
  if (parser->peek().type == TType::If || (parser->peek().type == TType::Identifier && parser->peek().value == "ifdef") ||
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
    auto value = interpret_from_ast(condition, parser->ctx);
    executed = value->is_truthy();
  } else {
    throw_error(
        "internal compiler error: Invalid #if/#ifdef/#ifndef, "
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

// used in libffi
extern std::vector<std::string> DYNAMIC_LIBRARY_LOAD_PATH;

// clang-format off
std::vector<DirectiveRoutine> Parser:: directive_routines = {
  // #load_dynamic_lib
  // used to make a dynamic library (by path, relative to LD_LIBRARY_PATH or absolute)
  // available at compile time.
    {.identifier = "load_dynamic_lib",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->expect(TType::LParen);
        auto string = parser->expect(TType::String);
        parser->expect(TType::RParen);
        DYNAMIC_LIBRARY_LOAD_PATH.push_back(string.value.str());
        printf("adding dynamic library to compile time function dispatch tool: %s\n", string.value.c_str());
        return nullptr;
      }
    },
    // #include
    // Just like C's include, just paste a text file right above where the
    // include is used.
    {.identifier = "include",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto filename = parser->expect(TType::String).value;
        if (!std::filesystem::exists(filename.str())) {
          throw_error(std::format("Couldn't find included file: {}, current path: {}", filename,
                                  std::filesystem::current_path().string()),
                      {});
        }
        if (include_set.contains(filename)) {
          return nullptr;
        }
        include_set.insert(filename);
        parser->states.push_back(Lexer::State::from_file(filename.str()));
        parser->fill_buffer_if_needed(parser->states.back());
        parser->fill_buffer_if_needed(parser->states.back());
        NODE_ALLOC(ASTStatementList, list, _, parser)
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
        std::cout << str.value.str() << "\n";
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
        if (!std::filesystem::exists(filename.str())) {
          throw_error(std::format("Couldn't find 'read' file: {}", filename), {});
        }

        parser->expect(TType::RParen);
        NODE_ALLOC(ASTLiteral, string, _, parser)
        string->tag = ASTLiteral::String;
        std::stringstream ss;
        if (mode == "binary") {
          std::ifstream isftr(filename.str(), std::ios::binary);
          ss << isftr.rdbuf();
          string->value = ss.str();
          return string;
        } else {
          std::ifstream isftr(filename.str());
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
        // TODO: implement something so we can do
        // * #test(group: "My Test Group", expects: false) *
        // so we can have tests that expect to fail, and so we can use
        // --filter="My Test Group" // run only test group
        // --filter=* - "My Test Group" // run all but test group

        auto func = parser->parse_function_declaration();
        func->is_test = true;

        if (compile_command.has_flag("test")) {
          return func;
        } else {
          parser->ctx.scope->erase(func->name);
          return nullptr;
        }
    }},

    // #location, for getting source location.
    {
      .identifier = "location",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) {
        auto location = parser->peek().span;
        auto formatted = std::format("{}:{}:{}", Span::files()[location.file], location.line, location.column);
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
          throw_error("Can only throw a literal as a error", error->span);
        }
        auto literal = static_cast<ASTLiteral *>(error);
        throw_error(literal->value.str(), error->span);
        return nullptr;
    }},

    // #c_flags, for adding stuff like linker options, -g etc from within
    // your program or header.
    {.identifier = "c_flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto string = parser->expect(TType::String).value;
        if (string == "-g") {
          compile_command.flags[string.str()] = true;
        }
        compile_command.add_c_flag(string.str());
        return nullptr;
    }},

    // #flags, for making an enum declaration auto increment with a flags value.
    // #flags MyEnum :: enum {...};
    {.identifier = "flags",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        ASTEnumDeclaration *enum_decl = parser->parse_enum_declaration();
        int index = 0;
        for (auto &key_value : enum_decl->key_values) {
          NODE_ALLOC(ASTLiteral, literal, _, parser);
          literal->tag = ASTLiteral::Integer;
          literal->value = std::to_string(1 << index);
          key_value.second = literal;
          index++;
        }
        enum_decl->is_flags = true;

        parser->current_statement_list->push_back(enum_decl);

        if (!compile_command.has_flag("freestanding") && !compile_command.has_flag("nostdlib")) {
          NODE_ALLOC(ASTImpl, impl, defer, parser);
          impl->scope = create_child(parser->ctx.scope);

          { // create the path & type for the std::util::Flags trait impl.
            NODE_ALLOC(ASTPath, path, defer1, parser);
            path->push_segment("std");
            path->push_segment("util");
            path->push_segment("Flags");
            NODE_ALLOC(ASTType, type, defer2, parser);
            type->kind=ASTType::NORMAL;
            type->normal.is_dyn = false;
            type->normal.path = path;
            impl->trait = type;
          }

          { // create the path & type for the target of the impl.
            NODE_ALLOC(ASTType, type, defer, parser);
            NODE_ALLOC(ASTPath, path, defer1, parser);
            path->push_segment(enum_decl->name);
            type->kind = ASTType::NORMAL;
            type->normal.is_dyn = false;
            type->normal.path = path;
            impl->target = type;
          }

          parser->current_statement_list->push_back(impl);
        }

        return nullptr;
    }},

    // #export, for exporting a non-mangled name to a dll or C library
    // primarily.
    // Equivalent to marking a function extern "C" in C++.
    {.identifier = "export",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto node = parser->parse_statement();
        if (node->get_node_type() == AST_NODE_VARIABLE) {
          auto decl = static_cast<ASTVariable*>(node);
          decl->is_extern = true;
        } else if (node->get_node_type() == AST_NODE_FUNCTION_DECLARATION) {
          auto func = static_cast<ASTFunctionDeclaration*>(node);
          func->is_exported = true;
        }
        return node;
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
    // TODO: make this a keyword.
    {.identifier = "static",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto statement = parser->parse_statement();
        if (auto decl = dynamic_cast<ASTVariable *>(statement)) {
          decl->is_static = true;
        } else {
          throw_error("static is only valid for variables, global or local.", statement->span);
        }
        return statement;
    }},

    // #def, define a compile time flag, like C #define but cannot be a macro.
    {.identifier = "def",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->ctx.scope->add_def(parser->expect(TType::Identifier).value);
        while (parser->peek().type == TType::Semi) parser->eat();
        return nullptr;
    }},

    // #undef, remove a #def
    {.identifier = "undef",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        parser->ctx.scope->undef(parser->expect(TType::Identifier).value);
        while (parser->peek().type == TType::Semi) parser->eat();
        return nullptr;
    }},

    // #ifdef, conditional compilation based on a #def being present.
    {.identifier = "ifdef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFDEF);
        return list;
    }},

    // #ifndef, conditional compilation based on a #def not being present.
    {.identifier = "ifndef",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IFNDEF);
        return list;
    }},

    // #if, conditional compilation based on compile time value.
    {.identifier = "if",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, _, parser)
        parse_ifdef_if_else_preprocs(parser, list, PREPROC_IF);
        return list;
    }},

    // #region, for named/unnnamed regions. just for organization, has no compilation implications.
    // can have anything between the #region directive and the {} block
    // #region My code region 1 {...} is legal.
    {.identifier = "region",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTStatementList, list, _, parser)
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

    // doesn't do anything
    {
      .identifier = "push_context",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        ASTExpr *object = parser->parse_expr(); // Context.{} or whatever.

        if (parser->current_func_decl.is_null()) {
          throw_error("Can only use #push_context within a function", object->span);
        }

        ASTFunctionDeclaration* function = parser->current_func_decl.get();

        // Setup caching.
        NODE_ALLOC(ASTVariable, old, old_defer, parser);
        {
          old->name = "$ctx" + std::to_string(function->context_push_count);
          function->context_push_count++;
          old->value = parser->context_identifier();
          old->is_local = true;
        }


        // set up dynof(&mut object, compiler::Context)
        NODE_ALLOC(ASTDyn_Of, dynof, dynof_defer, parser)
        {
          NODE_ALLOC(ASTUnaryExpr, expr, expr_defer, parser)
          expr->operand = object;
          expr->mutability = MUT;
          expr->op = TType::And;

          dynof->object = expr;
          dynof->trait_type = parser->context_trait_ast_type();
        }

        // setup the assignemnt of the new context
        NODE_ALLOC(ASTBinExpr, context_assignment, assign_defer, parser)
        {
          context_assignment->left = parser->context_identifier();
          context_assignment->op = TType::Assign;
          context_assignment->right = dynof;
        }

        // push caching
        parser->current_statement_list->push_back(old);

        // push assignment statement
        {
          NODE_ALLOC(ASTExprStatement, stmt, stmt_d, parser)
          stmt->expression = context_assignment;
          parser->current_statement_list->push_back(stmt);
        }

        return nullptr;
      }
    },

    // doesn't do anything
    {
      .identifier = "pop_context",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {

        if (parser->current_func_decl.is_null()) {
          throw_error("Can only use #push_context within a function", parser->peek().span);
        }

        ASTFunctionDeclaration* function = parser->current_func_decl.get();

        function->context_push_count--;
        if (function->context_push_count < 0) {
          throw_error("context stack underflow, no contexts to pop.", function->span);
        }

        NODE_ALLOC(ASTPath, path, path_defer, parser)
        path->push_segment("$ctx" + std::to_string(function->context_push_count));

        NODE_ALLOC(ASTBinExpr, context_assignment, assign_defer, parser)
        context_assignment->left = parser->context_identifier();
        context_assignment->op = TType::Assign;
        context_assignment->right = path;

        NODE_ALLOC(ASTExprStatement, stmt, stmt_d, parser)
        stmt->expression = context_assignment;

        parser->current_statement_list->push_back(stmt);

        return nullptr;
      }
    },

    // Don't use this.
    {
      // ! this is a hacky directive to make it so you can forward declare types from other modules without making a submodule within your current file.
      // ! This is just a symptom of having implicit file scoped modules, we should have to declare 'module fmt;' at the top of the file (ish) so anything above
      // !that would be in global namespace.
      .identifier = "global",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto old_scope = parser->ctx.scope;
        parser->ctx.scope = parser->ctx.root_scope;
        auto module_definition = parser->parse_module();
        parser->ctx.scope = old_scope;
        return module_definition;
      }
    },

    // #eval, for evaluating expressions :O
    {
      .identifier = "eval",
      .kind = DIRECTIVE_KIND_EXPRESSION,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTRun, run, defer, parser)
        if (parser->peek().type == TType::LCurly) {
          run->node_to_run = parser->parse_block();
        } else {
          run->node_to_run = parser->parse_expr();
        }
        return run;
      }
    },

    // #run, for running code :O
    {
      .identifier = "run",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        NODE_ALLOC(ASTRun, run, defer, parser)

        run->replace_prev_parent = false;

        if (parser->peek().type == TType::LCurly) {
          run->node_to_run = parser->parse_block();
        } else {
          run->node_to_run = parser->parse_expr();
        }

        NODE_ALLOC(ASTExprStatement, stmt, defer1, parser)
        stmt->expression = run;

        return stmt;
      }
    },


    // #expand, for making a function expand at call site, like a C macro.
    {
      .identifier = "expand",
      .kind = DIRECTIVE_KIND_STATEMENT,
      .run = [](Parser *parser) -> Nullable<ASTNode> {

        if (!parser->current_func_decl.get()) {
          throw_error("expand blocks may only be used in functions", {});
        }

        auto func = parser->current_func_decl.get();

        if (parser->peek().type == TType::LParen) {
          parser->expect(TType::LParen);
          auto argument = parser->expect(TType::Identifier);
          parser->expect(TType::RParen);
          if (argument.value == "hygenic") {
            func->is_hygenic_macro = true;
          }
        }

        auto block = parser->parse_block();
        func->is_macro = true;
        return block;
      }
    },

    // #insert, for use in expand blocks to say this statement should be expanded into the call site.
    {
      .identifier = "insert",
      .kind = DIRECTIVE_KIND_DONT_CARE,
      .run = [](Parser *parser) -> Nullable<ASTNode> {
        auto block = parser->parse_block();
        for (auto node: block->statements) {
          node->is_insert_node = true;
          node->declaring_scope = parser->ctx.scope;
          parser->current_statement_list->push_back(node);
        }
        return nullptr;
      }
    },
};
// clang-format on

Nullable<ASTNode> Parser::process_directive(DirectiveKind kind, const InternedString &identifier) {
  begin_node();
  // compare aganist the kind of the routine with expected type, based on parser
  // location
  for (const auto &routine : directive_routines) {
    if ((routine.kind == DIRECTIVE_KIND_DONT_CARE || routine.kind == kind) && routine.identifier == identifier) {
      auto result = routine.run(this);
      end_node(result.get());
      return result;
    }
  }
  throw_error(std::format("failed to call unknown directive routine, or an expression "
                          "routine was used as a statement, or vice versa: {}",
                          identifier),
              current_span);
  return nullptr;
}

ASTProgram *Parser::parse_program() {
  NODE_ALLOC(ASTProgram, program, _, this)
  ENTER_AST_STATEMENT_LIST(program->statements);
  program_statement_list = &program->statements;
  ENTER_SCOPE(ctx.root_scope);

  // put bootstrap on root scope
  if (!compile_command.has_flag("nostdlib")) {
    if (!try_import("bootstrap", &ctx.root_scope)) {
      throw_error("Unable to find bootstrap lib", {});
      return nullptr;
    }

    while (peek().type != TType::Eof) {
      program->statements.push_back(parse_statement());
    }

    expect(TType::Eof);
    states.pop_back();

    if (states.empty()) {
      end_node(program);
      throw_error("INTERNAL_COMPILER_ERROR: somehow the lexer state stack was empty after including the bootstrap lib.",
                  current_span);
      return nullptr;
    }

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
      case AST_NODE_TRAIT_DECLARATION:
      case AST_NODE_ENUM_DECLARATION:
      case AST_NODE_CHOICE_DECLARATION:
      case AST_NODE_ALIAS:
      case AST_NODE_VARIABLE:
      case AST_NODE_NOOP:
      case AST_NODE_IMPL:
      case AST_NODE_IMPORT:
      case AST_NODE_MODULE:
      case AST_NODE_STATEMENT_LIST:
        break;
      default:
        throw_error("Statement not allowed at the top-level of a program", statement->span);
    }

    program->statements.push_back(statement);
  }
  end_node(program);
  return program;
}

ASTArguments *Parser::parse_arguments() {
  NODE_ALLOC(ASTArguments, args, _, this)
  expect(TType::LParen);
  while (peek().type != TType::RParen) {
    args->arguments.push_back(parse_expr());
    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    }
  }
  expect(TType::RParen);
  end_node(args);
  return args;
}

void Parser::parse_pattern_match_value_semantic(auto &part) {
  if (peek().type == TType::And) {
    expect(TType::And);
    if (peek().type == TType::Mut) {
      expect(TType::Mut);
      part.semantic = PATTERN_MATCH_PTR_MUT;
    } else {
      if (peek().type == TType::Const) {
        expect(TType::Const);
      }
      part.semantic = PATTERN_MATCH_PTR_CONST;
    }
  }
}
ASTExpr *Parser::parse_expr(Precedence precedence) {
  ASTExpr *left = parse_unary();
  while (true) {
    auto op = peek();

    /* Pattern matching / destructuring Choice types. */
    if (op.type == TType::Is) {
      NODE_ALLOC(ASTPatternMatch, pattern_match, defer, this);
      eat();
      pattern_match->target_type_path = parse_path();
      pattern_match->object = left;

      if (peek().type == TType::Dot && lookahead_buf()[1].type == TType::LCurly) {
        eat();
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

          parse_pattern_match_value_semantic(part);

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

          parse_pattern_match_value_semantic(part);

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

    if (token_precedence == PRECEDENCE_INVALID_OPERATOR) {
      return left;
    }

    if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GT) {
      token_precedence = PRECEDENCE_SHIFT;
    }

    if (peek().type == TType::GT && lookahead_buf()[1].type == TType::GE) {
      token_precedence = PRECEDENCE_ASSIGNMENT;
    }

    if (token_precedence <= precedence) break;

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
    binexpr->span = left->span;
    auto right = parse_expr(token_precedence);

    binexpr->left = left;
    binexpr->right = right;
    binexpr->op = op.type;
    left = binexpr;
  }
  return left;
}

ASTExpr *Parser::parse_unary() {
  // bitwise not is a unary expression because arrays use it as a pop operator,
  // and sometimes you might want to ignore it's result.
  if (peek().type == TType::Add || peek().type == TType::Sub || peek().type == TType::LogicalNot || peek().type == TType::Not ||
      peek().type == TType::Increment || peek().type == TType::Decrement || peek().type == TType::Mul ||
      peek().type == TType::And || peek().type == TType::Not) {
    NODE_ALLOC(ASTUnaryExpr, unaryexpr, _, this)
    auto op = eat();

    Mutability mutability = CONST;
    if (op.type == TType::And) {
      if (peek().type == TType::Mut) {
        mutability = MUT;
        eat();
      } else {
        if (peek().type == TType::Const) {
          eat();
        }
        mutability = CONST;
      }
    }

    auto expr = parse_unary();
    unaryexpr->mutability = mutability;
    unaryexpr->op = op.type;
    unaryexpr->operand = expr;
    return unaryexpr;
  }

  return parse_postfix();
}

ASTPath::Segment Parser::parse_path_segment() {
  ASTPath::Segment segment = ASTPath::Segment::Invalid();

  if (peek().type == TType::Identifier) {
    segment.tag = ASTPath::Segment::IDENTIFIER;
    segment.set_identifier(expect(TType::Identifier).value);
  } else {
    TType peeked = peek().type;

    if (peeked != TType::LParen && peeked != TType::Mul && peeked != TType::LBrace && peeked != TType::Self) {
      throw_error("Incomplete path segment. Did you forget the right hand side of a '::'?", current_span);
    }

    segment.tag = ASTPath::Segment::TYPE;
    segment.set_type(parse_type());
  }

  if (peek().type == TType::GenericBrace) {
    segment.generic_arguments = parse_generic_arguments();
  }

  return segment;
}

ASTPath *Parser::parse_path(bool parsing_import_group) {
  NODE_ALLOC(ASTPath, path, defer, this);
  // If we find a double colon, then we continue.
  while (!parsing_import_group ||
         (peek().type != TType::LCurly && peek().type != TType::Mul)) {  // this condition may seem strange, but it's purely in
                                                                         // place to simplify parsing of import groups.

    auto segment = parse_path_segment();
    path->segments.push_back(segment);
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
  begin_node();
  // build dot and index expressions
  while (peek().type == TType::Dot || peek().type == TType::LBrace || peek().type == TType::PtrSubscript ||
         peek().type == TType::LParen || peek().type == TType::Increment || peek().type == TType::Decrement ||
         peek().type == TType::Range || peek().type == TType::As) {
    if (peek().type == TType::LParen) {
      NODE_ALLOC(ASTCall, call, _, this);
      call->callee = left;
      call->arguments = parse_arguments();
      left = call;
    } else if (peek().type == TType::Dot) {
      NODE_ALLOC(ASTDotExpr, dot, _, this)
      eat();
      dot->base = left;
      if (peek().type == TType::Integer) {
        dot->member = ASTPath::Segment::Identifier(expect(TType::Integer).value);
      } else if (peek().type == TType::Identifier) {
        dot->member = parse_path_segment();
      } else if (peek().type == TType::LCurly || peek().type == TType::LBrace) {
        // .{} initializer lists.
        ASTType *target_type = nullptr;
        if (auto path = dynamic_cast<ASTPath *>(left)) {
          target_type = ast_alloc<ASTType>();
          target_type->normal.path = path;
        } else if (auto type = dynamic_cast<ASTType *>(left)) {
          target_type = type;
        } else {
          throw_error(
              "can only use an initializer list on a path or type, e.g 's32, List!<s32>, std::fmt::formatter!<s32>, "
              "etc.",
              left->span);
        }
        if (peek().type == TType::LCurly) {
          eat();  // eat {
          NODE_ALLOC(ASTInitializerList, init_list, _, this)
          init_list->target_type = target_type;
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
          return init_list;
        } else if (peek().type == TType::LBrace) {
          eat();  // eat [
          NODE_ALLOC(ASTInitializerList, init_list, _, this)
          init_list->target_type = target_type;
          init_list->tag = ASTInitializerList::INIT_LIST_COLLECTION;
          while (peek().type != TType::RBrace) {
            init_list->values.push_back(parse_expr());
            if (peek().type == TType::Comma) {
              eat();
            }
          }
          expect(TType::RBrace);
          return init_list;
        }
      } else {
        end_node(left);
        throw_error("Invalid dot expression right hand side: expected a member name, or for a tuple, an index.", left->span);
      }

      if (peek().type == TType::LParen) {
        NODE_ALLOC(ASTMethodCall, method, defer, this);
        method->callee = dot;
        method->arguments = parse_arguments();
        left = method;
      } else {
        left = dot;
      }

    } else if (peek().type == TType::Increment || peek().type == TType::Decrement) {
      NODE_ALLOC(ASTUnaryExpr, unary, unary__, this)
      unary->operand = left;
      unary->op = eat().type;
      return unary;
    } else if (peek().type == TType::LBrace || peek().type == TType::PtrSubscript) {
      NODE_ALLOC(ASTIndex, index, _, this)
      index->base = left;
      index->is_pointer_subscript = peek().type == TType::PtrSubscript;
      eat();
      index->index = parse_expr();
      expect(TType::RBrace);
      left = index;
    } else if (peek().type == TType::Range) {
      NODE_ALLOC(ASTRange, node, _, this)
      eat();
      if (peek().type == TType::Assign) {
        eat();
        node->inclusive = true;
      }

      auto peeked = peek();

      // TODO: im sure theres plenty of cases ive missed here, just trying to guess whether what's ahead is
      // an expression or not.
      Precedence peeked_prec = get_operator_precedence(peeked);

      if (peeked.type == TType::Integer || peeked.type == TType::Identifier || peeked.type == TType::Mul ||
          peeked.type == TType::LParen || peeked.type == TType::Size_Of || peeked.type == TType::Dyn_Of ||
          peeked.type == TType::Type_Of || peeked.type == TType::Bitsize_Of ||
          peeked_prec != Precedence::PRECEDENCE_INVALID_OPERATOR) {
        node->right = parse_expr();
      }

      node->left = left;
      return node;
    } else if (peek().type == TType::As) {
      NODE_ALLOC(ASTCast, node, _, this)
      eat();
      node->target_type = parse_type();
      node->expression = left;
      left = node;
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

  // .{} initializer lists.
  if (tok.type == TType::Dot) {
    eat();  // eat .
    if (peek().type == TType::LCurly) {
      eat();  // eat {
      NODE_ALLOC(ASTInitializerList, init_list, _, this)
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
      return init_list;
    } else if (peek().type == TType::LBrace) {
      eat();  // eat [
      NODE_ALLOC(ASTInitializerList, init_list, _, this)
      init_list->tag = ASTInitializerList::INIT_LIST_COLLECTION;
      while (peek().type != TType::RBrace) {
        init_list->values.push_back(parse_expr());
        if (peek().type == TType::Comma) {
          eat();
        }
      }
      expect(TType::RBrace);
      return init_list;
    }
  }

  switch (tok.type) {
    case TType::Range: {
      NODE_ALLOC(ASTRange, range, defer, this);
      eat();
      if (peek().type == TType::Assign) {
        eat();
        range->inclusive = true;
      }
      range->right = parse_expr();
      return range;
    }
    case TType::Varargs: {
      NODE_ALLOC(ASTUnpack, node, defer, this)
      eat();
      node->expression = parse_expr();
      return node;
    };
    case TType::If: {
      return parse_if();
    }
    case TType::Dyn_Of: {
      expect(TType::Dyn_Of);
      NODE_ALLOC(ASTDyn_Of, dyn_of, _, this)
      expect(TType::LParen);
      dyn_of->object = parse_expr();

      if (peek().type == TType::Comma) {
        expect(TType::Comma);
        dyn_of->trait_type = parse_type();
      }

      expect(TType::RParen);
      return dyn_of;
    }
    case TType::Type_Of: {
      expect(TType::Type_Of);
      NODE_ALLOC(ASTType_Of, type_of, _, this)
      expect(TType::LParen);
      type_of->target = parse_type();
      expect(TType::RParen);
      return type_of;
    }
    case TType::Size_Of: {
      NODE_ALLOC(ASTSize_Of, node, _, this);
      eat();
      expect(TType::LParen);
      node->target_type = parse_type();
      expect(TType::RParen);
      return node;
    }
    case TType::Bitsize_Of: {
      NODE_ALLOC(ASTSize_Of, node, _, this);
      node->asking_for_bits = true;
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
      NODE_ALLOC(ASTSwitch, node, _, this)
      expect(TType::Switch);

      if (peek().type == TType::Is) {
        eat();
        node->is_pattern_match = true;
      }

      node->expression = parse_expr();
      expect(TType::LCurly);

      while (peek().type != TType::RCurly) {
        // Default case., 'else: {}' in our language.
        if (peek().type == TType::Else) {
          eat();
          if (peek().type != TType::ExpressionBody) {
            expect(TType::Colon);
          }
          node->default_branch = parse_block();
          if (peek().type == TType::Comma) eat();
          continue;
        }

        SwitchBranch branch;

        if (node->is_pattern_match) {
          NODE_ALLOC(ASTPatternMatch, pattern_match, defer, this);
          pattern_match->target_type_path = parse_path();
          pattern_match->object = node->expression;
          branch.expression = pattern_match;
          if (peek().type == TType::Dot && lookahead_buf()[1].type == TType::LCurly) {
            eat();
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

              if (peek().type == TType::And) {
                eat();
                if (peek().type == TType::Mut) {
                  eat();
                  part.semantic = PATTERN_MATCH_PTR_MUT;
                } else {
                  expect(TType::Const);
                  part.semantic = PATTERN_MATCH_PTR_CONST;
                }
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

              if (peek().type == TType::And) {
                eat();
                if (peek().type == TType::Mut) {
                  eat();
                  part.semantic = PATTERN_MATCH_PTR_CONST;
                } else {
                  expect(TType::Const);
                  part.semantic = PATTERN_MATCH_PTR_CONST;
                }
              }

              part.var_name = expect(TType::Identifier).value;
              pattern_match->tuple_pattern.parts.push_back(part);
              if (peek().type != TType::RParen) {
                expect(TType::Comma);
              }
            }
            expect(TType::RParen);
          }
        } else {
          branch.expression = parse_expr();
        }

        if (peek().type != TType::ExpressionBody) {
          expect(TType::Colon);
        }
        branch.block = parse_block();
        node->branches.push_back(branch);
        if (peek().type == TType::Comma) {
          eat();
        }
      }

      expect(TType::RCurly);
      return node;
    }
    case TType::Char: {
      NODE_ALLOC(ASTLiteral, node, _, this)
      eat();
      node->tag = ASTLiteral::Char;
      node->value = tok.value;
      return node;
    }
    case TType::Identifier: {
      return parse_path();
    }
    case TType::Null: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::Null;
      literal->value = tok.value;
      return literal;
    }
    case TType::True: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      return literal;
    }
    case TType::False: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::Bool;
      literal->value = tok.value;
      return literal;
    }
    case TType::Integer: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::Integer;
      literal->value = tok.value;
      return literal;
    }
    case TType::Float: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::Float;
      literal->value = tok.value;
      return literal;
    }
    case TType::String: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::String;
      literal->value = tok.value;
      if (peek().type == TType::Identifier && peek().value == "c") {
        literal->is_c_string = true;
        eat();
      }
      return literal;
    }
    case TType::MultiLineString: {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      eat();
      literal->tag = ASTLiteral::MultiLineString;
      literal->value = tok.value;
      if (peek().type == TType::Identifier && peek().value == "c") {
        literal->is_c_string = true;
        eat();
      }
      return literal;
    }
    case TType::LParen: {
      begin_node();
      expect(TType::LParen);  // (

      if (peek().type == TType::RParen) {
        NODE_ALLOC(ASTTuple, tuple, _, this);
        eat();
        tuple->values = {};
        return tuple;
      }

      auto expr = parse_expr();
      if (peek().type == TType::Comma) {
        ASTTuple *tuple = ast_alloc<ASTTuple>();
        tuple->values.push_back(expr);
        eat();
        while (peek().type != TType::RParen) {
          tuple->values.push_back(parse_expr());
          if (peek().type == TType::Comma) eat();
        }
        expect(TType::RParen);
        return tuple;
      }
      if (peek().type != TType::RParen) {
        eat();
        throw_error("Expected ')'", current_span);
      }
      eat();
      end_node(expr);

      if (expr->get_node_type() == AST_NODE_TYPE) {
        throw_error("using (TYPE)expr style casts are deprecated. use `expr as TYPE` syntax", current_span);
      }

      return expr;
    }
    case TType::LBrace:
    case TType::Self:
      return parse_path();
    default: {
      begin_node();
      throw_error(std::format("Invalid primary expression. Token: '{}'... Type: '{}'", tok.value, TTypeToString(tok.type)),
                  current_span);
      return nullptr;
    }
  }
}

ASTType *Parser::parse_type() {
  auto next_type = peek().type;

  if (next_type == TType::Fn) {
    return parse_function_type();
  }

  NODE_ALLOC(ASTType, node, _, this)
  parse_pointer_extensions(node);

  next_type = peek().type;
  switch (next_type) {
    case TType::Union:
    case TType::Struct: {
      node->kind = ASTType::STRUCTURAL_DECLARATIVE_ASCRIPTION;
      node->declaration = parse_struct_declaration();
    } break;
    case TType::Enum: {
      node->normal.path = ast_alloc<ASTPath>();
      auto declaration = parse_enum_declaration();
      current_statement_list->push_back(declaration);
      node->normal.path = ast_alloc<ASTPath>();
      node->normal.path->push_segment({
          declaration->name,
      });
    } break;
    case TType::Choice: {
      auto declaration = parse_choice_declaration();
      current_statement_list->push_back(declaration);
      node->normal.path = ast_alloc<ASTPath>();
      node->normal.path->push_segment({
          declaration->name,
      });
    } break;
    case TType::Trait: {
      auto declaration = parse_trait_declaration();
      current_statement_list->push_back(declaration);
      node->normal.path = ast_alloc<ASTPath>();
      node->normal.path->push_segment({
          declaration->name,
      });
    } break;
    default:
      break;
  }

  if (next_type == TType::Mut && lookahead_buf()[1].type == TType::LBrace) {
    eat();
    eat();
    auto type = parse_type();
    expect(TType::RBrace);
    auto slice = ast_alloc<ASTType>();
    slice->kind = ASTType::NORMAL;
    slice->normal.path = ast_alloc<ASTPath>();
    slice->normal.path->push_segment("SliceMut", {type});
    return slice;
  }

  // slice and array types.
  if (next_type == TType::LBrace) {
    eat();
    auto type = parse_type();
    // array type.
    if (peek().type == TType::Semi) {
      eat();
      auto expr = parse_expr();
      expect(TType::RBrace);
      node = type;
      node->extensions.push_back(ASTTypeExtension{
          .type = TYPE_EXT_ARRAY,
          .expression = expr,
      });
    } else {
      expect(TType::RBrace);
      auto slice = ast_alloc<ASTType>();
      slice->kind = ASTType::NORMAL;
      slice->normal.path = ast_alloc<ASTPath>();
      slice->normal.path->push_segment("Slice", std::vector<ASTExpr *>{type});
      return slice;
    }
    return node;
  }

  if (next_type == TType::Dyn) {
    node->normal.is_dyn = true;
    eat();
  }

  if (next_type == TType::LParen) {
    node->resolved_type = Type::INVALID_TYPE;
    node->kind = ASTType::TUPLE;
    eat();
    while (peek().type != TType::RParen) {
      node->tuple_types.push_back(parse_type());
      if (peek().type == TType::Comma) eat();
    }
    expect(TType::RParen);
    return node;
  }

  if (next_type == TType::Self) {
    eat();
    node->kind = ASTType::SELF;
  }

  if (node->kind == ASTType::NORMAL && !node->normal.path) {
    node->normal.path = parse_path();
  }

  end_node(node);

  if (node->kind == ASTType::NORMAL && node->normal.path && node->normal.path->segments.empty()) {
    throw_error("INTERNAL PARSER ERROR: parsed an empty path for a type", current_span);
  }

  return node;
}

// TODO: we should handle the 'then' statement more gracefully.
// Also, => is super fricken janky, and is really poorly implemented.
// !CLEANUP remove the => operator from everything but switch cases. it's terrible and awful
ASTIf *Parser::parse_if() {
  NODE_ALLOC(ASTIf, node, _, this)
  node->is_expression = true;
  eat();
  node->condition = parse_expr();

  if (peek().type == TType::Then) {
    NODE_ALLOC(ASTBlock, block, defer, this);
    eat();
    node->block = block;
    ctx.set_scope();
    auto statement = parse_statement();
    node->block->statements = {statement};
    if (statement->get_node_type() == AST_NODE_VARIABLE) {
      throw_warning(WARNING_INACCESSIBLE_DECLARATION, "Inaccesible declared variable", statement->span);
    }
    node->block->scope = ctx.exit_scope();
  } else {
    node->block = parse_block();
  }

  if (peek().type == TType::Else) {
    NODE_ALLOC(ASTElse, node_else, _, this)
    eat();
    if (peek().type == TType::If) {
      auto inner_if = parse_if();
      inner_if->is_expression = false;
      node_else->_if = inner_if;
    } else {
      node_else->block = parse_block();
    }
    node->_else = node_else;
  }

  return node;
}

ASTModule *Parser::parse_module() {
  NODE_ALLOC(ASTModule, mod, _, this);
  expect(TType::Module);

  mod->module_name = expect(TType::Identifier).value;

  TType expected_delimiter = TType::Eof;

  if (peek().type == TType::Semi) {
    eat();
  } else if (peek().type == TType::LCurly) {
    eat();
    expected_delimiter = TType::RCurly;
  } else {
    throw_error(
        std::format("expected ';' for a file scoped module, or '{{' to begin a module block, got '{}'", peek().value.str()),
        mod->span);
  }

  if (expected_delimiter == TType::Eof && (current_func_decl || current_impl_decl || current_struct_decl || current_trait_decl)) {
    throw_error("file-scoped modules can only be declared at the top-level", mod->span);
  }

  mod->scope = create_child(ctx.scope);
  mod->scope->is_module_scope = true;
  ENTER_SCOPE(mod->scope);

  while (peek().type != expected_delimiter) {
    mod->statements.push_back(parse_statement());
    if (peek().type == TType::Semi) {
      eat();
    }
  }

  expect(expected_delimiter);
  return mod;
}

ASTImport *Parser::parse_import() {
  expect(TType::Import);
  NODE_ALLOC(ASTImport, import, _, this);

  // Parse the root group (could be a single path, a group, or a wildcard, or a recursive set of groups.)
  import->root_group = parse_import_group(nullptr);
  expect(TType::Semi);

  ASTPath *path = import->root_group.path;
  ASTPath::Segment root_segment = path->segments[0];

  // we always treat modules as if theyre at a root scope when theyre imported so you dont get
  // 'my_module::some_stdlib::module::etc'
  // when you import shit -- it messes up linking and name resolution
  Scope *scope = create_child(ctx.root_scope);
  ENTER_SCOPE(scope)

  auto identifier = root_segment.get_identifier();
  if (this->try_import(identifier, &scope)) {
    NODE_ALLOC(ASTModule, the_module, _, this)
    // again, make certain that we are attached to the root scope for imported modules.
    the_module->declaring_scope = ctx.root_scope;
    the_module->module_name = identifier;
    the_module->scope = scope;
    scope->name = identifier;
    {
      ENTER_AST_STATEMENT_LIST(the_module->statements);
      while (!peek().is_eof()) {
        the_module->statements.push_back(parse_statement());
      }
    }
    current_statement_list->push_back(the_module);
    expect(TType::Eof);
    states.pop_back();
    std::filesystem::current_path(states.back().path.parent_path());
  }

  return import;
}

ASTImport::Group Parser::parse_import_group(ASTPath *base_path) {
  ASTImport::Group group;
  group.is_wildcard = false;

  if (!base_path) {
    NODE_ALLOC(ASTPath, path, _, this);
    group.path = path;

    if (peek().type == TType::Identifier && lookahead_buf()[1].type == TType::Semi) {
      path->push_segment(eat().value);
    } else if (peek().type == TType::Identifier && lookahead_buf()[1].type == TType::DoubleColon) {
      while (peek().type == TType::Identifier && lookahead_buf()[1].type == TType::DoubleColon) {
        path->push_segment(expect(TType::Identifier).value);
        eat();
      }
      if (peek().type == TType::Identifier) {
        auto path = parse_path(true);
        if (peek().type == TType::As) {
          eat();
          group.symbols.push_back(ASTImport::Symbol::Path(path, expect(TType::Identifier).value));
        } else {
          group.symbols.push_back(ASTImport::Symbol::Path(path));
        }
      }
    }
  } else {
    group.path = base_path;
  }

  // Handle wildcard: ::*
  if (peek().type == TType::Mul) {
    eat();  // *
    group.is_wildcard = true;
    return group;
  }

  // Handle group: ::{ ... }
  if (peek().type == TType::LCurly) {
    eat();  // {
    while (peek().type != TType::RCurly) {
      if (peek().type == TType::Identifier) {
        ASTPath *symbol_path = parse_path(true);
        // Nested group: symbol::{
        if (peek().type == TType::LCurly) {
          group.symbols.push_back(ASTImport::Symbol::Group(parse_import_group(symbol_path)));
        } else if (peek().type == TType::Mul) {
          eat();
          group.symbols.push_back(ASTImport::Symbol::Group({.is_wildcard = true, .path = symbol_path}));
        } else {
          if (peek().type == TType::As) {
            eat();
            group.symbols.push_back(ASTImport::Symbol::Path(symbol_path, expect(TType::Identifier).value));
          } else {
            group.symbols.push_back(ASTImport::Symbol::Path(symbol_path));
          }
        }
      }
      if (peek().type != TType::RCurly) {
        expect(TType::Comma);
      }
    }
    expect(TType::RCurly);
  }

  return group;
}

void Parser::parse_destructure_element_value_semantic(DestructureElement &destruct) {
  if (peek().type == TType::And) {
    expect(TType::And);
    if (peek().type == TType::Mut) {
      expect(TType::Mut);
      destruct.semantic = ValueSemantic::VALUE_SEMANTIC_POINTER_MUT;
    } else {
      if (peek().type == TType::Const) {
        expect(TType::Const);
      }
      destruct.semantic = ValueSemantic::VALUE_SEMANTIC_POINTER_CONST;
    }
  } else {
    destruct.semantic = ValueSemantic::VALUE_SEMANTIC_COPY;
  }
}

ASTStatement *Parser::parse_using_stmt() {
  eat();
  ASTVariable *variable = parse_variable();

  ASTBlock *block = current_block.get();
  NODE_ALLOC(ASTDefer, defer_ast, defer7, this);

  bool parsed_block = false;
  if (peek().type == TType::LCurly) {
    parsed_block = true;
    block = parse_block();
    expect(TType::Semi);

    {  // we have to patch these up since this is synthetic
      variable->declaring_block = block;
      variable->declaring_scope = block->scope;
      defer_ast->declaring_block = block;
      defer_ast->declaring_scope = block->scope;
    }

    block->statements.insert(block->statements.begin(), variable);
    block->statements.insert(block->statements.begin() + 1, defer_ast);
  } else {
    // If we're doing the "inline using" we just push them back otherwise we'd mess with a ton of crap.
    block->statements.push_back(variable);
    block->statements.push_back(defer_ast);
  }

  // the variable.destroy() method call.
  NODE_ALLOC(ASTMethodCall, method_call, defer, this);

  NODE_ALLOC(ASTExprStatement, expr_stmt, defer5, this);

  expr_stmt->expression = method_call;
  defer_ast->statement = expr_stmt;

  {  // create the variable.destroy() paths.
    // variable.
    NODE_ALLOC(ASTPath, path, defer1, this);
    path->push_segment(variable->name);

    // .destroy()
    NODE_ALLOC(ASTDotExpr, dot, defer2, this);
    dot->base = path;
    dot->member = ASTPath::Segment::Identifier("destroy");
    method_call->callee = dot;
  }

  // create arguments.
  NODE_ALLOC(ASTArguments, arguments, defer3, this);

  {  // push the recursive: true argument
    NODE_ALLOC(ASTLiteral, literal, defer4, this);
    literal->tag = ASTLiteral::Bool;
    literal->value = "true";
    arguments->arguments.push_back(literal);
  }
  method_call->arguments = arguments;

  if (parsed_block) {
    return block;
  } else {
    static auto noop = ast_alloc<ASTNoop>();
    return noop;
  }
}

int attribute_tag_takes_arguments(AttributeTag tag) {
  switch (tag) {
    case ATTRIBUTE_IMPL:
    case ATTRIBUTE_COMPILER_FEATURE:
      return -1;
    case ATTRIBUTE_DEPRECATED:
      return 2;
    case ATTRIBUTE_CONSTRUCTOR:
      return 1;
    case ATTRIBUTE_INLINE:
    case ATTRIBUTE_ENTRY:
    case ATTRIBUTE_CONST:
    case ATTRIBUTE_PUB:
    case ATTRIBUTE_NO_MANGLE:
    case ATTRIBUTE_NO_RETURN:
    default:
      return 0;
  }
}

bool try_get_attribute_tag_from_string(const std::string &ident, AttributeTag *tag) {
  if (ident == "no_return") {
    *tag = ATTRIBUTE_NO_RETURN;
  } else if (ident == "pub") {
    *tag = ATTRIBUTE_PUB;
  } else if (ident == "entry") {
    *tag = ATTRIBUTE_ENTRY;
  } else if (ident == "impl") {
    *tag = ATTRIBUTE_IMPL;
  } else if (ident == "no_mangle") {
    *tag = ATTRIBUTE_NO_MANGLE;
  } else if (ident == "inline") {
    *tag = ATTRIBUTE_INLINE;
  } else if (ident == "compiler_feature") {
    *tag = ATTRIBUTE_COMPILER_FEATURE;
  } else if (ident == "deprecated") {
    *tag = ATTRIBUTE_DEPRECATED;
  } else if (ident == "constructor") {
    *tag = ATTRIBUTE_CONSTRUCTOR;
  } else {
    return false;
  }
  return true;
}

std::vector<Attribute> Parser::parse_statement_attributes() {
  std::vector<Attribute> attributes;
  expect(TType::Attribute);
  expect(TType::LBrace);
  while (peek().type != TType::RBrace) {
    Attribute attribute;
    if (peek().type == TType::Impl) {
      eat();
      attribute.tag = ATTRIBUTE_IMPL;
      expect(TType::LParen);
      while (peek().type != TType::RParen) {
        attribute.arguments.push_back(parse_type());
        if (peek().type != TType::RParen) expect(TType::Comma);
      }
      expect(TType::RParen);
    } else if (peek().type == TType::Const) {
      eat();
      attribute.tag = ATTRIBUTE_CONST;
    } else {
      AttributeTag tag;
      const std::string &ident = expect(TType::Identifier).value.str();
      if (!try_get_attribute_tag_from_string(ident, &tag)) {
        throw_error(std::format("invalid attribute {}", ident), {peek().span});
      }
      attribute.tag = tag;
      int argument_count = attribute_tag_takes_arguments(tag);

      if (argument_count != 0 && peek().type == TType::LParen) {
        eat();
        argument_count = argument_count == -1 ? INT_MAX : argument_count;
        for (int i = 0; i < argument_count; ++i) {
          attribute.arguments.push_back(parse_expr());
          if (peek().type != TType::RParen) {
            expect(TType::Comma);
          } else {
            break;
          }
        }
        expect(TType::RParen);
      }
    }
    if (peek().type != TType::RBrace) {
      expect(TType::Comma);
    }
    attributes.push_back(std::move(attribute));
  }
  expect(TType::RBrace);
  if (peek().type == TType::Attribute) {
    throw_error("doubling up attributes declarations leads to an overwrite. just use one @[...]", {peek().span});
  }
  return attributes;
}

ASTStatement *Parser::parse_statement() {
  begin_node();
  auto tok = peek();

  while (tok.type == TType::Semi) {
    eat();
    tok = peek();
  }

  if (tok.type == TType::Eof) {
    // ! this should not be neccesary,
    // ! but the way we handle semi colons is absolutely terrible.
    // ! it should be much more structured, not just wishy washy.
    return ast_alloc<ASTNoop>();
  }

  if (tok.type == TType::Import) {
    return parse_import();
  }

  if (tok.type == TType::Module) {
    return parse_module();
  }

  if (tok.type == TType::Attribute) {
    std::vector<Attribute> attributes = parse_statement_attributes();
    auto statement = parse_statement();
    statement->attributes = std::move(attributes);
    return statement;
  }

  if (tok.type == TType::Const) {
    eat();
    auto variable = parse_variable();
    variable->is_constexpr = true;
    ctx.scope->insert_variable(variable->name, Type::INVALID_TYPE, variable->value.get(), CONST, variable);
    return variable;
  }

  if (tok.type == TType::Directive && (lookahead_buf()[1].value == "self")) {
    NODE_ALLOC(ASTExprStatement, statment, _, this)
    statment->expression = parse_expr();
    end_node(statment);
    return statment;
  }

  if (tok.type == TType::Where) {
    return parse_where_statement();
  }

  // * '#' Directives.
  if (tok.type == TType::Directive) {
    begin_node();
    eat();
    auto directive_name = eat().value;
    auto statement = dynamic_cast<ASTStatement *>(process_directive(DIRECTIVE_KIND_STATEMENT, directive_name).get());

    if (!statement) {
      /*
        TODO: is there a way we can do this without returning a node?
        We should probably get rid of most of the directives, if they can be
        replaced with a keyword and a node.

        this is just a useful tool for
      */
      static auto noop = ast_alloc<ASTNoop>();
      statement = noop;
    }

    end_node(statement);
    return statement;
  }

  /*
    I'm getting rid of '#export' and '#foreign' and just replacing it with
    an extern node, as well as making it so we can do blocks of externs.

    the export and foreign were strange and had useless semantics.
    any kind of extern function aliasing can be done via attributes in the future.
  */
  if (tok.type == TType::Extern) {
    expect(TType::Extern);

    if (peek().type == TType::LCurly) {  // block of extern statements
      NODE_ALLOC(ASTStatementList, node, defer, this);
      expect(TType::LCurly);
      while (peek().type != TType::RCurly) {
        if (peek().type == TType::Fn) {
          auto function = parse_function_declaration();
          function->is_extern = true;
          node->statements.push_back(function);
        } else {
          auto variable = parse_variable();
          variable->is_extern = true;
          node->statements.push_back(variable);
        }
        expect(TType::Semi);
      }
      expect(TType::RCurly);
      return node;
    } else if (peek().type == TType::Fn) {  // single extern fn
      auto function = parse_function_declaration();
      function->is_extern = true;
      return function;
    } else {  // single extern variable
      auto variable = parse_variable();
      variable->is_extern = true;
      return variable;
    }
  }

  if (peek().type == TType::Defer) {
    return parse_defer();
  }

  if (peek().type == TType::Impl) {
    return parse_impl();
  }

  if (peek().type == TType::Type) {
    eat();
    NODE_ALLOC(ASTAlias, alias, _, this)
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

  if (peek().type == TType::Using) {
    return parse_using_stmt();
  }

  // * Control flow
  {
    if (tok.type == TType::LCurly) {
      auto block = parse_block();
      return block;
    }

    if (tok.type == TType::Return) {
      NODE_ALLOC(ASTReturn, return_node, _, this)
      expect(TType::Return);
      if (peek().type != TType::Semi) {
        return_node->expression = parse_expr();
      }
      return return_node;
    }

    if (tok.type == TType::Break) {
      NODE_ALLOC(ASTBreak, _break, _, this)
      eat();
      return _break;
    }

    if (tok.type == TType::Continue) {
      NODE_ALLOC(ASTContinue, _continue, _, this)
      eat();
      return _continue;
    }

    if (tok.type == TType::For) {
      const bool mutable_c_style = lookahead_buf()[1].type == TType::Mut && lookahead_buf()[2].type == TType::Identifier &&
                                   (lookahead_buf()[3].type == TType::Colon || lookahead_buf()[3].type == TType::ColonEquals);

      const bool regular_c_style = lookahead_buf()[1].type == TType::Identifier &&
                                   (lookahead_buf()[2].type == TType::Colon || lookahead_buf()[2].type == TType::ColonEquals);

      if (mutable_c_style || regular_c_style) {
        return parse_for_c_style();
      }

      NODE_ALLOC(ASTFor, node, _, this)
      eat();

      // Parse the variables in the for loop
      std::vector<DestructureElement> destructure = parse_destructure_elements();

      // Set the left_tag and left union based on the parsed variables
      if (destructure.size() > 1) {
        node->left_tag = ASTFor::DESTRUCTURE;
        node->left.destructure = destructure;
      } else {
        node->left_tag = ASTFor::IDENTIFIER;
        node->left.identifier = destructure[0].identifier;

        if (is_pointer_semantic(destructure[0].semantic)) {
          throw_error(
              "you can only take the elements of a tuple destructure as a pointer, 'for *v in ...' is "
              "redundant. just use the correct iterator, such as '.iter_mut()'",
              current_span);
        }
      }

      if (peek().type == TType::In) {
        eat();
        auto expr = parse_expr();
        node->right = expr;
      } else {
        constexpr const char *error_string = R"_(
Invalid 'for' syntax.
expected:
  for i in 0..10 { .. } (where 0..10 could be any iterator or iterable, we will use $iter from now on)
  for a, b in $iter { .. }
  for a, *b in $iter { .. }
  for ptr in iter_mut() { .. }

Or, for a "C Style" for loop: (You do not need mut in the declaration. it is implicitly mutable)
  for i: s32 = 0; i < 100; i += 1 { .. }
  for i := 100; i > 0; i -= 1 { .. }

  (I didn't cover _every_ variation, but it's available in the tutorial documentation)
        )_";

        throw_error(error_string, current_span);
      }

      node->block = parse_block();
      return node;
    }

    if (tok.type == TType::While) {
      NODE_ALLOC(ASTWhile, node, _, this)
      eat();
      if (peek().type != TType::LCurly) {
        node->condition = parse_expr();
      }
      node->block = parse_block();
      return node;
    }

    if (tok.type == TType::If) {
      NODE_ALLOC(ASTExprStatement, expr_statement, range1, this);
      auto the_if = parse_if();
      the_if->is_expression = false;
      expr_statement->expression = the_if;
      return expr_statement;
    }
  }

  {  // Variable declarations.
    const bool is_simple_const_destructure = tok.type == TType::Identifier && lookahead_buf()[1].type == TType::Comma;

    const bool is_simple_mut_destructure =
        (tok.type == TType::Mut && lookahead_buf()[1].type == TType::Identifier && lookahead_buf()[2].type == TType::Comma) ||
        (tok.type == TType::Mut && lookahead_buf()[1].type == TType::Mul && lookahead_buf()[1].type == TType::Identifier &&
         lookahead_buf()[3].type == TType::Comma);

    const bool is_simple_const_referential_destructure =
        tok.type == TType::And && lookahead_buf()[1].type == TType::Identifier && lookahead_buf()[2].type == TType::Comma;

    const bool is_complex_referential_destructure =
        tok.type == TType::And && (lookahead_buf()[1].type == TType::Const || lookahead_buf()[1].type == TType::Mut) &&
        lookahead_buf()[2].type == TType::Identifier && lookahead_buf()[3].type == TType::Comma;

    if (is_simple_const_destructure || is_simple_mut_destructure || is_simple_const_referential_destructure ||
        is_complex_referential_destructure) {
      return parse_destructure();
    }

    bool is_colon_or_colon_equals = tok.type == TType::Identifier &&
                                    (lookahead_buf()[1].type == TType::Colon || lookahead_buf()[1].type == TType::ColonEquals);

    bool is_mut_decl = tok.type == TType::Mut && lookahead_buf()[1].type == TType::Identifier &&
                       (lookahead_buf()[2].type == TType::Colon || lookahead_buf()[2].type == TType::ColonEquals);

    if (is_mut_decl || is_colon_or_colon_equals) {
      auto decl = parse_variable();
      return decl;
    }
  }

  if (peek().type == TType::Fn) {
    auto node = parse_function_declaration();
    return node;
  }

  if (peek().type == TType::Trait) {
    auto trait = parse_trait_declaration();
    return trait;
  }
  if (peek().type == TType::Struct || peek().type == TType::Union) {
    auto struct_decl = parse_struct_declaration();
    return struct_decl;
  }
  if (peek().type == TType::Enum) {
    auto enum_decl = parse_enum_declaration();
    return enum_decl;
  }
  if (peek().type == TType::Choice) {
    eat();
    auto tagged_union = parse_choice_declaration();
    return tagged_union;
  }

  if (lookahead_buf()[1].type == TType::DoubleColon) {
    NODE_ALLOC(ASTExprStatement, expr, defer, this);
    expr->expression = parse_expr();
    return expr;
  }

  // * Expression statements.
  {
    auto next = lookahead_buf()[1];
    auto next_next = lookahead_buf()[2];
    // Both postfix and prefix (inc/dec)rement
    const bool is_increment_or_decrement =
        (next.type == TType::Increment || (next.type == TType::Decrement && next_next.type != TType::Semi)) ||
        tok.type == TType::Increment || tok.type == TType::Decrement;

    // index assignment or dot assign/ call statement.
    // also for pointer arithmetic expressions ![]
    const bool is_identifier_with_lbrace_or_dot =
        tok.type == TType::Identifier &&
        (next.type == TType::LBrace || next.type == TType::Dot || next.type == TType::PtrSubscript);

    const bool is_call = next.type == TType::LParen || next.type == TType::GenericBrace;

    const bool is_assignment_or_compound = next.type == TType::Assign || next.type == TType::Comma ||
                                           ttype_is_comp_assign(next.type) ||
                                           (tok.type == TType::Identifier && ttype_is_relational(next.type));

    // .2 != comma for tuple destrucutre.
    const bool is_deref = tok.type == TType::Mul && lookahead_buf()[2].type != TType::Comma;

    const bool is_special_case = tok.type == TType::LParen ||  // possible parenthesized dereference or something.
                                 tok.type == TType::Switch;

    if (is_call || is_increment_or_decrement || is_identifier_with_lbrace_or_dot || is_assignment_or_compound || is_deref ||
        is_special_case) {
      NODE_ALLOC(ASTExprStatement, statement, _, this)
      statement->expression = parse_expr();
      if (ASTSwitch *_switch = dynamic_cast<ASTSwitch *>(statement->expression)) {
        _switch->is_statement = true;
      } else {
        expect(TType::Semi);
      }
      return statement;
    }
  }

  //*  Failure to parse errors

  {
    if (tok.family == TFamily::Operator) {
      throw_error(std::format("Unexpected operator: {} '{}'", TTypeToString(tok.type), tok.value), current_span);
    }

    if (tok.family == TFamily::Literal) {
      eat();
      throw_error(std::format("Unexpected literal: {} .. {}", tok.value, TTypeToString(tok.type)), current_span);
    }

    if (tok.family == TFamily::Keyword) {
      eat();
      throw_error(std::format("Unexpected keyword: {}", tok.value), current_span);
    }

    eat();
    throw_error(std::format("Unexpected token when parsing statement: {}.. This "
                            "is likely an undefined type or identifier.",
                            tok.value),
                current_span);
    exit(1);
  }
}

ASTDestructure *Parser::parse_destructure() {
  NODE_ALLOC(ASTDestructure, node, _, this)
  node->elements = parse_destructure_elements();
  if (peek().type == TType::ColonEquals || peek().type == TType::Assign) {
    node->op = eat().type;
    node->right = parse_expr();
  } else {
    throw_error("Currently, you cannot have an explicitly typed tuple deconstruction. Use a, b, c := ....", current_span);
  }

  return node;
}

ASTVariable *Parser::parse_variable() {
  NODE_ALLOC(ASTVariable, decl, _, this);
  if (current_func_decl) {
    decl->is_local = true;
  }
  if (peek().type == TType::Mut) {
    eat();
    decl->mutability = MUT;
  }
  auto iden = eat();
  decl->name = iden.value;

  if (peek().type == TType::Colon) {
    expect(TType::Colon);
    decl->type = parse_type();
    if (peek().type == TType::Assign) {
      eat();
      if (peek().type == TType::Uninitialized) {
        eat();
        decl->is_uninitialized = true;
      } else {
        auto expr = parse_expr();
        decl->value = expr;
      }
    }
  } else {
    expect(TType::ColonEquals);

    if (peek().type == TType::Uninitialized) {
      throw_error("cannot use uninitialized syntax (---) with an untyped variable declaration", current_span);
    }

    decl->value = parse_expr();
  }
  return decl;
}

ASTBlock *Parser::parse_block(Scope *scope) {
  auto last_block = current_block;
  NODE_ALLOC_EXTRA_DEFER(ASTBlock, block, _, this, current_block = last_block);
  current_block = block;
  ENTER_AST_STATEMENT_LIST(block->statements);

  ctx.set_scope(scope);

  if (peek().type == TType::Directive) {
    eat();
    auto ident = expect(TType::Identifier);
    auto result = process_directive(DIRECTIVE_KIND_STATEMENT, ident.value);
    if (result.is_null() || result.get()->get_node_type() != AST_NODE_BLOCK) {
      throw_error(std::format("directive {} is incompatible with block declarations, it must return a block, which it doesn't",
                              ident.value),
                  current_span);
    }
    return (ASTBlock *)result.get();
  }

  if (peek().type == TType::ExpressionBody) {
    NODE_ALLOC(ASTReturn, $return, _, this);
    expect(TType::ExpressionBody);
    $return->expression = parse_expr();
    block->statements = {$return};
    block->scope = ctx.exit_scope();  // we do this, even though it owns no scope, because it would get created later
                                      // anyway when entering it.
    if (peek().type == TType::Semi) eat();
    return block;
  }

  expect(TType::LCurly);

  while (peek().type != TType::RCurly) {
    if (peek().type == TType::Eof) {
      throw_error("Imbalanced '{' and '}'", current_span);
    }
    auto statement = parse_statement();

    if (statement->get_node_type() == AST_NODE_DEFER) {
      if (current_func_decl.get()) {
        current_func_decl.get()->has_defer = true;
      } else {
        throw_error("You can only use defer within a function scope", statement->span);
      }
      block->has_defer = true;
      block->defer_count++;
    }

    block->statements.push_back(statement);
    while (semicolon()) eat();
  }
  expect(TType::RCurly);
  block->scope = ctx.exit_scope();
  return block;
}

ASTParamsDecl *Parser::parse_parameters() {
  NODE_ALLOC(ASTParamsDecl, params, defer, this);
  expect(TType::LParen);

  while (peek().type != TType::RParen) {
    NODE_ALLOC(ASTParamDecl, param, _, this)
    if (params->is_varargs) {
      throw_error("var args \"...\" must be the last parameter", current_span);
    }

    if (peek().type == TType::Varargs) {
      eat();
      if (!current_func_decl) {
        throw_error(
            "Cannot use varargs outside of a function declaration. "
            "Only use this for #foreign functions.",
            current_span);
      }
      current_func_decl.get()->is_varargs = true;
      params->is_varargs = true;
      continue;
    }

    auto next = peek();
    if (next.type == TType::Mut && lookahead_buf()[1].value == "self") {
      param->tag = ASTParamDecl::Self;
      param->self.is_pointer = false;
      param->mutability = MUT;
      params->has_self = true;
      params->params.push_back(param);
      eat();
      eat();  // eat the dang tokens.
      if (peek().type != TType::RParen) {
        expect(TType::Comma);
      }
      continue;
    } else if (next.type == TType::Mul || next.value == "self") {  // parse self parameters.
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
      } else {
        throw_error("when we got *mut/*const, we expected \'self\', since the parameter was not named", current_span);
      }

      if (params->params.size() != 0) {
        throw_error("'self' must be the first parameter in the signature", current_span);
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
      if (param->tag == ASTParamDecl::Self) {
        throw_error("self parameters cannot have a default value.", param->span);
      }
      eat();
      param->normal.default_value = parse_expr();
    }

    params->params.push_back(param);

    if (peek().type != TType::RParen) {
      expect(TType::Comma);
    } else {
      break;
    }
  }

  expect(TType::RParen);
  return params;
}

ASTFunctionDeclaration *Parser::parse_function_declaration() {
  NODE_ALLOC(ASTFunctionDeclaration, node, _, this)
  expect(TType::Fn);

  auto name = node->name = expect(TType::Identifier).value;

  node->has_defer = false;

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  auto last_func_decl = current_func_decl;
  Defer deferred([&] { current_func_decl = last_func_decl; });
  current_func_decl = node;

  node->params = parse_parameters();
  node->name = name;

  // check for definition.
  auto has_definition = false;
  {
    auto sym = ctx.scope->local_lookup(name);
    if (sym && (sym->is_forward_declared) == 0) {
      has_definition = true;
    }
  }

  // TODO: why are we inserting the functions at parse time?
  // Really, we never ended up removing symbol table interactions from the parser,
  // and this is biting us in the ass later on (now)
  ctx.scope->insert_function(name, Type::INVALID_TYPE, node);

  if (peek().type != TType::Arrow) {
    node->return_type = ASTType::get_void();
  } else {
    expect(TType::Arrow);
    node->return_type = parse_type();
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  if (peek().type == TType::Semi) {
    node->is_forward_declared = true;
    ctx.scope->local_lookup(name)->is_forward_declared = true;
    current_func_decl = last_func_decl;
    return node;
  }

  ctx.set_scope();

  if (node->params->has_self) {
    node->is_method = true;
  }

  node->block = parse_block();
  node->block.get()->parent = node;

  if (node->block && has_definition) {
    throw_error(std::format("Redefinition of function {}", node->name), current_span);
  }

  node->scope = ctx.exit_scope();
  return node;
}

ASTEnumDeclaration *Parser::parse_enum_declaration() {
  NODE_ALLOC(ASTEnumDeclaration, node, _, this)
  expect(TType::Enum);

  if (peek().type == TType::Identifier) {
    node->name = expect(TType::Identifier).value;
  } else {
    node->name = get_unique_identifier().value;
  }

  if (peek().type == TType::Colon) {
    eat();
    node->underlying_type_ast = parse_type();
  }

  expect(TType::LCurly);

  int last_value = -1;

  // Create a dummy scope that gets discarded so that enum variants can refer to other, already declared enum variants
  // in this enum.
  auto old_scope = ctx.scope;
  Defer $scope([&] { ctx.scope = old_scope; });
  ctx.scope = create_child(ctx.scope);

  while (peek().type != TType::RCurly) {
    auto iden = expect(TType::Identifier).value;
    ASTExpr *value = nullptr;

    if (peek().type == TType::Assign) {
      expect(TType::Assign);
      value = parse_expr();
      auto evaluated_value = interpret_from_ast(value, this->ctx);
      if (evaluated_value->value_type != ValueType::INTEGER) {
        throw_error("Enums can only have integers", value->span);
      }
      last_value = evaluated_value->as<IntValue>()->value;
    } else {
      NODE_ALLOC(ASTLiteral, literal, _, this)
      last_value++;
      literal->value = std::to_string(last_value);
      value = literal;
    }

    if (peek().type == TType::Comma) {
      eat();
    }

    ctx.scope->insert_local_variable(iden, s32_type(), value, CONST);
    node->key_values.push_back({iden, value});
  }
  std::vector<InternedString> keys;
  std::set<InternedString> keys_set;
  for (const auto &[key, value] : node->key_values) {
    if (keys_set.find(key) != keys_set.end()) {
      throw_error(std::format("redefinition of enum variant: {}", key), node->span);
    }
    keys.push_back(key);
    keys_set.insert(key);
  }

  if (node->key_values.empty()) {
    throw_error("Empty `enum` types are not allowed", current_span);
  }

  expect(TType::RCurly);
  return node;
}

ASTImpl *Parser::parse_impl() {
  NODE_ALLOC_EXTRA_DEFER(ASTImpl, node, _, this, current_impl_decl = nullptr)
  expect(TType::Impl);

  ctx.set_scope();
  node->scope = ctx.exit_scope();

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  current_impl_decl = node;
  auto target = parse_type();

  ASTType *trait = nullptr;
  if (peek().type == TType::For) {
    expect(TType::For);
    trait = parse_type();
    node->trait = target;
    node->target = trait;
  } else {
    node->target = target;
  }

  node->target->resolved_type = Type::INVALID_TYPE;
  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto block = parse_block(node->scope);

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
      throw_error("invalid statement: only methods and aliases are allowed in 'impl's", statement->span);
    }
  }
  return node;
}

ASTDefer *Parser::parse_defer() {
  NODE_ALLOC(ASTDefer, node, _, this)
  expect(TType::Defer);
  node->statement = parse_statement();
  return node;
}

ASTWhere *Parser::parse_where_clause() {
  NODE_ALLOC(ASTWhere, node, _, this);
  std::vector<Constraint> &constraints = node->constraints;
  auto parse_constraint = [&] -> Constraint {
    auto type = parse_type();
    expect(TType::Colon);
    ASTExpr *condition = parse_type();
    while (peek().type == TType::And || peek().type == TType::Or) {
      NODE_ALLOC(ASTBinExpr, binexpr, _, this)
      binexpr->op = eat().type;
      binexpr->right = parse_type();
      binexpr->left = condition;
      condition = (ASTExpr *)binexpr;
    }
    return {type, condition};
  };

  // Used to reduce repitition of this large(ish) function
  expect(TType::Where);
  constraints.push_back(parse_constraint());
  while (peek().type == TType::Comma) {
    eat();
    constraints.push_back(parse_constraint());
  }
  return node;
}

ASTWhereStatement *Parser::parse_where_statement() {
  const auto parse_clause = [&]() -> ASTWhere * {
    NODE_ALLOC(ASTWhere, node, _, this);
    std::vector<Constraint> &constraints = node->constraints;

    const auto parse_constraint = [&] -> Constraint {
      ASTExpr *expression = parse_expr();
      expect(TType::Colon);
      ASTExpr *condition = parse_type();

      while (peek().type == TType::And || peek().type == TType::Or) {
        NODE_ALLOC(ASTBinExpr, binexpr, _, this)
        binexpr->op = eat().type;
        binexpr->right = parse_type();
        binexpr->left = condition;
        condition = (ASTExpr *)binexpr;
      }

      return Constraint{expression, condition};
    };

    constraints.push_back(parse_constraint());
    while (peek().type == TType::Comma) {
      eat();
      constraints.push_back(parse_constraint());
    }
    return node;
  };

  const auto parse_branch = [&]() -> WhereBranch * {
    expect(TType::Else);
    WhereBranch *branch = (WhereBranch *)ast_arena.allocate(sizeof(WhereBranch));
    if (peek().type == TType::Where) {
      branch->where_stmt = parse_where_statement();
    } else {
      branch->block = parse_block();
    }
    return branch;
  };

  NODE_ALLOC(ASTWhereStatement, node, _, this);
  expect(TType::Where);
  if (peek().type == TType::LogicalNot) {
    node->negative = true;
    eat();
  }
  node->where = parse_clause();
  node->block = parse_block();
  if (peek().type == TType::Else) {
    node->branch = parse_branch();
  }
  return node;
}

ASTTraitDeclaration *Parser::parse_trait_declaration() {
  auto previous = current_trait_decl;
  NODE_ALLOC_EXTRA_DEFER(ASTTraitDeclaration, node, _, this, { current_trait_decl = previous; });
  expect(TType::Trait);

  if (peek().type == TType::Identifier) {
    node->name = expect(TType::Identifier).value;
  } else {
    node->name = get_unique_identifier().value;
  }

  current_trait_decl = node;
  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  if (peek().type == TType::Colon) {
    eat();
    auto &constraints = node->trait_bounds;
    auto parse_constraint = [&] -> Constraint {
      ASTExpr *condition = parse_type();
      while (peek().type == TType::And || peek().type == TType::Or) {
        NODE_ALLOC(ASTBinExpr, binexpr, _, this)
        binexpr->op = eat().type;
        binexpr->right = parse_type();
        binexpr->left = condition;
        condition = (ASTExpr *)binexpr;
      }
      NODE_ALLOC(ASTType, self_type, defer, this);
      self_type->kind = ASTType::SELF;
      return {self_type, condition};
    };
    constraints.push_back(parse_constraint());
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto scope = create_child(ctx.scope);
  node->scope = scope;

  if (peek().type == TType::Semi) {
    node->is_forward_declared = true;
    return node;
  }

  auto block = parse_block(scope);
  for (const auto &stmt : block->statements) {
    if (auto function = dynamic_cast<ASTFunctionDeclaration *>(stmt)) {
      node->methods.push_back(function);
    }
  }
  return node;
}

ASTStructDeclaration *Parser::parse_struct_body(InternedString name, ASTStructDeclaration *node) {
  auto old_struct_decl_state = current_struct_decl;

  node->name = name;
  current_struct_decl = node;

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }

  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  node->scope = create_child(ctx.scope);
  node->scope->name = node->name;

  if (!semicolon()) {
    expect(TType::LCurly);
    std::vector<ASTNode *> directives;
    while (peek().type != TType::RCurly) {
      const auto peeked = peek().type;
      const bool is_anonymous = peeked == TType::Struct || peeked == TType::Union;
      if (is_anonymous) {
        ASTStructDeclaration *decl = parse_struct_declaration();

        if (!decl->is_union) {
          throw_error("We do not support sub-struct that aren't anonymous sub-unions", decl->span);
        }

        decl->is_anonymous = true;
        decl->is_structural = false;
        node->subtypes.push_back(decl);
      }
      if (peek().type == TType::Directive) {
        eat();
        auto directive = process_directive(DIRECTIVE_KIND_STATEMENT, expect(TType::Identifier).value);
        if (!directive) {  // it yielded no node, just return.
          continue;
        }
        if (directive && directive.get()->get_node_type() == AST_NODE_VARIABLE) {
          ASTStructMember member{};
          auto _node = static_cast<ASTVariable *>(directive.get());
          member.name = _node->name;
          member.is_bitfield = true;
          member.bitsize = _node->bitsize;
          member.type = _node->type;
          if (peek().type == TType::Assign) {
            eat();
            member.default_value = parse_expr();
          }
          node->members.push_back(member);
        } else {
          throw_error(
              "right now, `#bitfield(n_bits) name: type` definitions are the "
              "only directive statements allowed in structs. this is slotted to change, as well as many directives "
              "formalized into keywords.",
              current_span);
        }
      } else if (peek().type == TType::Identifier) {
        ASTStructMember member{};
        member.name = eat().value;
        expect(TType::Colon);
        member.type = parse_type();
        if (peek().type == TType::Assign) {
          eat();
          member.default_value = parse_expr();
        }
        node->members.push_back(member);
      }

      if (peek().type != TType::RCurly) {
        expect(TType::Comma);
      }
    }
    expect(TType::RCurly);
  } else {
    node->is_forward_declared = true;
  }

  current_struct_decl = old_struct_decl_state;
  return node;
}

ASTStructDeclaration *Parser::parse_struct_declaration() {
  NODE_ALLOC(ASTStructDeclaration, node, _, this)
  if (peek().type == TType::Struct) {
    expect(TType::Struct);
  } else {
    node->is_union = true;
    expect(TType::Union);
  }
  InternedString name;

  if (peek().type == TType::Identifier) {
    name = expect(TType::Identifier).value;
  } else {
    name = get_unique_identifier().value;
    node->is_structural = true;
  };

  parse_struct_body(name, node);
  return node;
}

ASTChoiceDeclaration *Parser::parse_choice_declaration() {
  NODE_ALLOC(ASTChoiceDeclaration, node, _, this);

  if (peek().type == TType::Identifier) {
    node->name = expect(TType::Identifier).value;
  } else {
    node->name = get_unique_identifier().value;
  }

  if (peek().type == TType::GenericBrace) {
    node->generic_parameters = parse_generic_parameters();
  }
  if (peek().type == TType::Where) {
    node->where_clause = parse_where_clause();
  }

  auto scope = create_child(ctx.scope);
  auto type = ctx.scope->create_tagged_union(node->name, scope, node);
  ctx.set_scope(scope);
  node->scope = scope;
  node->resolved_type = type;

  if (peek().type == TType::Semi) {
    node->is_forward_declared = true;
    ctx.exit_scope();
    return node;
  }

  expect(TType::LCurly);

  while (peek().type != TType::RCurly) {
    ASTChoiceVariant variant;
    variant.name = expect(TType::Identifier).value;
    if (peek().type == TType::Comma || peek().type == TType::RCurly) {
      variant.kind = ASTChoiceVariant::NORMAL;
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
    } else if (peek().type == TType::LParen) {
      variant.kind = ASTChoiceVariant::TUPLE;
      variant.tuple = parse_type();
      assert(variant.tuple->kind == ASTType::TUPLE);
    } else {
      throw_error("Unexpected token in choice type declaration", node->span);
    }
    node->variants.push_back(variant);
    if (peek().type != TType::RCurly) expect(TType::Comma);
  }
  ctx.exit_scope();
  expect(TType::RCurly);
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
      eat();
      throw_error(
          "Invalid directive in expression: directives in "
          "expressions must return a value.",
          current_span);
    }
  }
  return nullptr;
}

std::vector<ASTExpr *> Parser::parse_generic_arguments() {
  begin_node();
  expect(TType::GenericBrace);
  std::vector<ASTExpr *> params;

  while (peek().type != TType::GT) {
    params.push_back(parse_type());
    if (peek().type != TType::GT) expect(TType::Comma);
  }

  expect(TType::GT);
  return params;
}

std::vector<ASTGenericParameter> Parser::parse_generic_parameters() {
  expect(TType::GenericBrace);
  std::vector<ASTGenericParameter> params;
  while (peek().type != TType::GT) {
    ASTGenericParameter parameter;
    parameter.identifier = expect(TType::Identifier).value;
    if (peek().type == TType::Assign) {
      eat();
      auto type = parse_type();
      parameter.default_value = type;
    }
    if (peek().type != TType::GT) expect(TType::Comma);
    params.push_back(parameter);
  }
  expect(TType::GT);
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

ASTDeclaration *find_generic_instance(std::vector<GenericInstance> instantiations, const std::vector<Type *> &gen_args) {
  for (auto &instantiation : instantiations) {
    if (instantiation.arguments == gen_args) {
      return instantiation.declaration;
    }
  }
  return nullptr;
}

ASTType *Parser::parse_function_type() {
  NODE_ALLOC(ASTType, output_type, _, this)
  expect(TType::Fn);
  output_type->kind = ASTType::FUNCTION;

  if (peek().type == TType::Mul) {
    eat();
    throw_warning((WarningFlags)0,
                  "`fn ()` syntax is deprecated. function types are always pointers, when expressed as an annotated "
                  "type (TODO MAKE THIS WARNING MAKE MORE SENSE)",
                  current_span);
  }

  output_type->extensions.insert(output_type->extensions.begin(), {TYPE_EXT_POINTER_MUT});
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
  NODE_ALLOC(ASTLambda, node, _, this);
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
  while (peek().type == TType::Mul) {
    expect(TType::Mul);
    type->extensions.push_back({peek().type == TType::Mut ? TYPE_EXT_POINTER_MUT : TYPE_EXT_POINTER_CONST});
    if (peek().type == TType::Mut) {
      expect(TType::Mut);
    } else {
      if (peek().type == TType::Const) {
        expect(TType::Const);
      }
    }
  }
}

static Precedence get_operator_precedence(Token token) {
  if (ttype_is_comp_assign(token.type)) {
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
      return PRECEDENCE_INVALID_OPERATOR;
  }
}

ASTType *ASTType::get_void() {
  static ASTType *type = [] {
    ASTType *type = ast_alloc<ASTType>();
    type->kind = ASTType::NORMAL;
    auto path = ast_alloc<ASTPath>();
    path->push_segment("void");
    type->normal.path = path;
    type->resolved_type = void_type();
    return type;
  }();
  return type;
}

inline Token Parser::eat() {
  Lexer::State &state = states.back();
  fill_buffer_if_needed(state);
  const Token tok = peek();
  state.lookahead_buffer.pop_front();
  lexer.get_token(state);
  current_span.length += tok.span.length;
  return tok;
}

inline Token Parser::expect(TType type) {
  fill_buffer_if_needed(states.back());
  if (peek().type != type) {
    Span range = peek().span;
    throw_error(std::format("Expected {}, got {} : {}", TTypeToString(type), TTypeToString(peek().type), peek().value), range);
  }
  return eat();
}

inline Token Parser::peek() const {
  if (states.empty()) {
    return Token::Eof();
  }

  const Lexer::State &state = states.back();
  if (!state.lookahead_buffer.empty()) {
    return state.lookahead_buffer.front();
  } else {
    return Token::Eof();
  }
}

inline void Parser::fill_buffer_if_needed(Lexer::State &state) {
  const size_t initial_size = state.lookahead_buffer.size();
  for (size_t i = initial_size; i < 8; ++i) {
    lexer.get_token(state);
  }
}

bool Parser::try_import(InternedString name, Scope **scope) {
  static std::string ela_lib_path = ({
    std::string path;
    if (const char *env_p = std::getenv("ELA_LIB_PATH")) {
      path = env_p;
    } else {
#ifdef _WIN32
      path = "C:\\Program Files\\ela";
#else
      path = "/usr/local/lib/ela";
#endif
    }
    path;
  });

  // TODO: we'll eventually use weave with a local ./pkg_cache/, this is not a great system currently.
  std::string module_name = name.str();
  std::string filename = std::filesystem::path(ela_lib_path) / name.str();

  if (std::filesystem::exists(module_name) || std::filesystem::exists(module_name + ".ela")) {
    filename = module_name;
  }

  // Right now, we just return false if we're double including.
  if (import_scopes.contains(module_name)) {
    *scope = import_scopes[module_name];
    return false;
  }

  if (std::filesystem::is_directory(filename)) {
    filename += std::filesystem::path::preferred_separator;
    filename.append("lib.ela");
  } else {
    filename += ".ela";
  }

  if (!std::filesystem::exists(filename)) {
    return false;
  }

  import_scopes.insert({module_name, *scope});
  states.push_back(Lexer::State::from_file(filename));
  fill_buffer_if_needed(states.back());
  return true;
}

void Parser::begin_node() {
  Span span = peek().span;
  current_span = span;
}

void Parser::end_node(ASTNode *node) {
  if (node) {
    node->span = current_span;
  }
}

Parser::Parser(const std::string &filename, Context &context) : ctx(context), states({Lexer::State::from_file(filename)}) {
  fill_buffer_if_needed(states.back());

  Span::user_entry_file() = std::filesystem::absolute(filename);

  if (states.back().input.empty()) {
    fprintf(stderr, "No code to compile, so exiting.\n");
    exit(1);
  }

  typer = new Typer(context);
}

Parser::~Parser() { delete typer; }

Nullable<ASTBlock> Parser::current_block = nullptr;

std::vector<DestructureElement> Parser::parse_destructure_elements() {
  const auto parse_element = [&]() -> DestructureElement {
    DestructureElement element;
    element.mutability = CONST;
    if (peek().type == TType::Mut) {
      element.mutability = MUT;
      eat();
    }
    parse_destructure_element_value_semantic(element);
    element.identifier = expect(TType::Identifier).value;
    return element;
  };

  std::vector<DestructureElement> elements;
  DestructureElement element = parse_element();
  elements.push_back(element);
  while (peek().type == TType::Comma) {
    eat();
    element = parse_element();
    elements.push_back(element);
  }
  return elements;
}

bool ASTNode::is_expr() {
  switch (get_node_type()) {
    case AST_NODE_BIN_EXPR:
    case AST_NODE_UNARY_EXPR:
    case AST_NODE_LITERAL:
    case AST_NODE_TYPE:
    case AST_NODE_TUPLE:
    case AST_NODE_CALL:
    case AST_NODE_ARGUMENTS:
    case AST_NODE_DOT_EXPR:
    case AST_NODE_INDEX:
    case AST_NODE_INITIALIZER_LIST:
    case AST_NODE_CAST:
    case AST_NODE_RANGE:
    case AST_NODE_SWITCH:
    case AST_NODE_PATH:
      return true;
    // lazy but effective for when we forget to add something to this.
    default:
      return dynamic_cast<ASTExpr *>(this);
  }
}

ASTPath *Parser::context_identifier() {
  static NODE_ALLOC(ASTPath, context_identifier, context_identifier_defer, this);
  if (context_identifier->segments.empty()) {
    context_identifier->push_segment(CONTEXT_IDENTIFIER);
  }
  return context_identifier;
};

ASTType *Parser::context_trait_ast_type() {
  static NODE_ALLOC(ASTType, type, defer, this);
  if (!type->normal.path) {
    type->kind = ASTType::NORMAL;
    type->extensions = {};
    NODE_ALLOC(ASTPath, path, path_defer, this)
    path->push_segment("compiler");
    path->push_segment("Context");
    type->normal.path = path;
  }
  return type;
}

ASTForCStyle *Parser::parse_for_c_style() {
  NODE_ALLOC(ASTForCStyle, ast, _1, this);
  ast->scope = create_child(ctx.scope);
  ENTER_SCOPE(ast->scope);

  NODE_ALLOC(ASTExprStatement, stmt, _3, this);
  expect(TType::For);
  ast->initialization = parse_variable();
  expect(TType::Semi);
  ast->condition = parse_expr();
  expect(TType::Semi);
  stmt->expression = parse_expr();
  ast->increment = stmt;
  ast->block = parse_block();
  return ast;
}
