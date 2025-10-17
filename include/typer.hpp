#include <deque>
#include "arena.hpp"
#include "ast.hpp"

enum Phase : uint8_t {
  PH_SHELL,      // symbol defined, no types assigned. this is the default state as we do a symbol pre-pass.
  PH_SIGNATURE,  // types assigned for function headers & struct definitions
  PH_BODY,       // process struct bodies and function blocks etc, all executable regions.
  PH_SOLVED,     // fully typed, no dependencies unsolved.
};

struct Task;
struct Dep {
  Task *provider;         // what caused this dependency
  Phase need = PH_SHELL;  // required phase of producer for this dependency to be satisfied
  bool satisfied = false;
};

struct Task {
  Phase phase = PH_SHELL;          // progress made so far.
  ASTNode *node = nullptr;         // a node that yields a declaration, variable, type, impl, function
  std::vector<Dep> depends_on;     // what we depend on
  std::vector<Task *> dependents;  // everyone that depends on us
  size_t indegree = 0;             // number of unsolved dependencies.
  bool queued = false;
};

enum Task_Result {
  TASK_RESULT_ERROR,
  TASK_RESULT_STALLED,
  TASK_RESULT_PROGRESS,
  TASK_RESULT_COMPLETE,
};

struct Typer {
  std::vector<ASTImpl *> impls;
  std::vector<ASTWhereStatement *> where_statements;
  jstl::Arena task_arena{MB(10)};
  std::unordered_map<ASTNode *, Task *> tasks;
  std::deque<Task *> ready_queue;

  void collect_symbols(ASTNode *node);
  void try_generate_tasks_for_node(ASTNode *node, Task *consumer);
  void run(ASTProgram *ast);

  bool is_done(ASTNode *node) {
    auto it = tasks.find(node);
    // if this node didn't have a task, then it couldn't possibly be a dependency, so it's OK
    if (it == tasks.end()) {
      return true;
    }
    return it->second->phase == PH_SOLVED;
  }

  Task *create_task_bind_to_node_create_edge_to_consumer(ASTNode *node, Task *consumer = nullptr,
                                                         Phase required_phase_to_progress = Phase::PH_SHELL);
  void try_add_edge(Task *consumer, Task *provider, Phase required_phase_to_progress);
  void task_advance_phase(Task *provider, Phase phase_reached, std::deque<Task *> &ready);
  void enqueue_if_ready(Task *task);

  Task_Result try_solve_task(Task *task);

  Task_Result try_visit_block(ASTBlock *node);
  Task_Result try_visit_function_declaration(ASTFunctionDeclaration *node);
  Task_Result try_visit_alias(ASTAlias *node);
  Task_Result try_visit_impl(ASTImpl *node);
  Task_Result try_visit_import(ASTImport *node);
  Task_Result try_visit_module(ASTModule *node);
  Task_Result try_visit_return(ASTReturn *node);
  Task_Result try_visit_continue(ASTContinue *node);
  Task_Result try_visit_break(ASTBreak *node);
  Task_Result try_visit_for(ASTFor *node);
  Task_Result try_visit_if(ASTIf *node);
  Task_Result try_visit_else(ASTElse *node);
  Task_Result try_visit_while(ASTWhile *node);
  Task_Result try_visit_struct_declaration(ASTStructDeclaration *node);
  Task_Result try_visit_enum_declaration(ASTEnumDeclaration *node);
  Task_Result try_visit_choice_declaration(ASTChoiceDeclaration *node);
  Task_Result try_visit_trait_declaration(ASTTraitDeclaration *node);
  Task_Result try_visit_variable(ASTVariable *node);
  Task_Result try_visit_expr_statement(ASTExprStatement *node);
  Task_Result try_visit_bin_expr(ASTBinExpr *node);
  Task_Result try_visit_unary_expr(ASTUnaryExpr *node);
  Task_Result try_visit_literal(ASTLiteral *node);
  Task_Result try_visit_path(ASTPath *node);
  Task_Result try_visit_type(ASTType *node);
  Task_Result try_visit_tuple(ASTTuple *node);
  Task_Result try_visit_call(ASTCall *node);
  Task_Result try_visit_method_call(ASTMethodCall *node);
  Task_Result try_visit_arguments(ASTArguments *node);
  Task_Result try_visit_dot_expr(ASTMemberAccess *node);
  Task_Result try_visit_index(ASTIndex *node);
  Task_Result try_visit_initializer_list(ASTInitializerList *node);
  Task_Result try_visit_size_of(ASTSize_Of *node);
  Task_Result try_visit_type_of(ASTType_Of *node);
  Task_Result try_visit_dyn_of(ASTDyn_Of *node);
  Task_Result try_visit_defer(ASTDefer *node);
  Task_Result try_visit_cast(ASTCast *node);
  Task_Result try_visit_lambda(ASTLambda *node);
  Task_Result try_visit_unpack(ASTUnpack *node);
  Task_Result try_visit_unpack_element(ASTUnpackElement *node);
  Task_Result try_visit_range(ASTRange *node);
  Task_Result try_visit_switch(ASTSwitch *node);
  Task_Result try_visit_destructure(ASTDestructure *node);
  Task_Result try_visit_where(ASTWhere *node);
  Task_Result try_visit_pattern_match(ASTPatternMatch *node);
  Task_Result try_visit_statement_list(ASTStatementList *node);
  Task_Result try_visit_where_statement(ASTWhereStatement *node);
  Task_Result try_visit_run(ASTRun *node);
};
