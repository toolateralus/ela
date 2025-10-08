#include "ast.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "visitor.hpp"
#include <cstdlib>
#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <chrono>
#include <iomanip>
#include <algorithm>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

static std::ofstream log_file;

static std::string current_timestamp() {
  using namespace std::chrono;
  auto now = system_clock::now();
  std::time_t t = system_clock::to_time_t(now);
  auto ms = duration_cast<milliseconds>(now.time_since_epoch()) % 1000;
  std::ostringstream ss;
  ss << std::put_time(std::localtime(&t), "%Y-%m-%d %H:%M:%S") << '.' << std::setfill('0') << std::setw(3)
     << ms.count();
  return ss.str();
}

constexpr const char *LOGFILE = "/home/josh/source/c++/ela/lsp/build/bin/logs.log";

static void log_msg(const std::string &level, const std::string &msg) {
  if (!log_file.is_open()) {
    log_file.open(LOGFILE, std::ios::app);
  }
  if (log_file.is_open()) {
    log_file << current_timestamp() << " [" << level << "] " << msg << std::endl;
    log_file.flush();
  }
}

static void log_info(const std::string &msg) { log_msg("INFO", msg); }
static void log_warn(const std::string &msg) { log_msg("WARN", msg); }
static void log_error(const std::string &msg) { log_msg("ERROR", msg); }

void lsp_panic_handler(const std::string &msg, const SourceRange &source_range, void *) {
  std::string loc = source_range.ToString();
  std::string full_msg = msg + " at " + loc;
  log_error(full_msg);

  json notification = {
      {"jsonrpc", "2.0"}, {"method", "window/showMessage"}, {"params", {{"type", 1}, {"message", full_msg}}}};
  std::cout << notification.dump() << std::endl;
  std::cout.flush();
}

ASTProgram *parse_file(const std::string &filename, Context &context) {
  log_info(std::string("Parsing file: ") + filename);
  Parser parser(filename, context);
  ASTProgram *prog = parser.parse_program();
  if (prog)
    log_info(std::string("Parsed file successfully: ") + filename);
  else
    log_warn(std::string("Failed to parse file: ") + filename);
  return prog;
}

void type_file(ASTProgram *program, Context &context) {
  if (!program) {
    log_warn("type_file called with null program");
    return;
  }
  log_info("Starting type checking");
  Typer typer(context);
  program->accept(&typer);
  log_info("Finished type checking");
}

size_t get_file_index(const std::string &uri) {
  auto &files_vec = SourceRange::files();
  auto it = std::find(files_vec.begin(), files_vec.end(), uri);
  if (it != files_vec.end()) return it - files_vec.begin();
  files_vec.push_back(uri);
  return files_vec.size() - 1;
}

int main() {
  init_type_system();

  log_info("Logger initialized, logs will be written to ./logs.log");

  while (true) {
    std::string line;

    if (!std::getline(std::cin, line)) {
      continue;
    }

    if (line.empty()) continue;

    log_info(std::string("Received message: ") + line);

    json msg;
    try {
      msg = json::parse(line);
    } catch (const std::exception &e) {
      log_error(std::string("Failed to parse JSON message: ") + e.what() + " -- raw: " + line);
      continue;
    }

    std::string method = msg.value("method", "");
    log_info(std::string("Method: ") + method);

    if (method == "textDocument/didOpen" || method == "textDocument/didChange") {
      std::string file = msg["params"]["textDocument"]["uri"];
      log_info(std::string("didOpen/didChange for file: ") + file);

      // Create a fresh context per request (no caching).
      Context context;
      panic_handler = lsp_panic_handler;

      ASTProgram *program = parse_file(file, context);
      if (!program) {
        log_warn(std::string("No AST produced for file: ") + file);
        continue;
      }

      log_info(std::string("Stored file data for: ") + file);

      type_file(program, context);

    }

    else if (method == "textDocument/hover" || method == "textDocument/definition") {
      std::string file = msg["params"]["textDocument"]["uri"];
      int line_num = msg["params"]["position"]["line"];
      int col_num = msg["params"]["position"]["character"];

      log_info(std::string("Hover/Definition request for ") + file + " at line " + std::to_string(line_num) + " col " +
               std::to_string(col_num));

      // Fresh context per request (no caching). Re-parse each time.
      Context context;
      panic_handler = lsp_panic_handler;

      ASTProgram *program = parse_file(file, context);
      if (!program) {
        log_warn(std::string("Failed to parse file for hover/definition: ") + file);
        json reply = {{"jsonrpc", "2.0"}, {"id", msg["id"]}, {"result", json::object()}};
        std::cout << reply.dump() << std::endl;
        std::cout.flush();
        continue;
      }

      size_t file_index = get_file_index(file);

      std::string hover_text;
      SourceRange result_range;
      size_t closest_dist = SIZE_MAX;
      ASTNode *closest_node = nullptr;

      context.on_visit_complete = [&](VisitorBase *sender, ASTNode *node) -> bool {
        if (!dynamic_cast<Typer *>(sender)) {
          return ON_VISIT_COMPLETE_CONTINUE;
        }
        size_t dist = node->source_range.distance(file_index, line_num, col_num);
        if (dist < closest_dist) {
          closest_dist = dist;
          closest_node = node;
          result_range = node->source_range;
        }
        return ON_VISIT_COMPLETE_CONTINUE;
      };

      type_file(program, context);

      json result;
      if (closest_node) {
        log_info(std::string("Found closest node at distance ") + std::to_string(closest_dist));
        if (method == "textDocument/hover") {
          if (closest_node->resolved_type) {
            hover_text = closest_node->resolved_type->basename.get_str();
          } else {
            hover_text = "<no-type>";
          }
          result["contents"] = hover_text;
          log_info(std::string("Hover text: ") + hover_text);
        } else if (method == "textDocument/definition") {
          result = {{"uri", SourceRange::files()[result_range.file]},
                    {"range",
                     {{"start", {{"line", result_range.line}, {"character", result_range.column}}},
                      {"end", {{"line", result_range.line}, {"character", result_range.column}}}}}};
          log_info(std::string("Definition range for file ") + SourceRange::files()[result_range.file]);
        }
      } else {
        log_warn("No AST node found near requested position");
      }

      json reply = {{"jsonrpc", "2.0"}, {"id", msg["id"]}, {"result", result}};
      std::cout << reply.dump() << std::endl;
      std::cout.flush();
      log_info("Replied to hover/definition request");

    } else {
      log_info(std::string("Unhandled method: ") + method);
    }
  }

  log_info("Shutting down");
  if (log_file.is_open()) log_file.close();
  return 0;
}
