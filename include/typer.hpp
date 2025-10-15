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
};

struct Typisting {
  std::vector<ASTImpl *> impls;
  std::vector<ASTWhereStatement *> where_statements;
  jstl::Arena task_arena{MB(10)};
  std::unordered_map<ASTNode *, Task *> tasks;

  void collect_symbols(ASTNode *node);
  void try_generate_tasks_for_node(ASTNode *node, Task *consumer);
  void run(ASTProgram *ast);
  Task *create_task_bind_to_node_create_edge_to_consumer(ASTNode *node, Task *consumer = nullptr,
                                                         Phase required_phase_to_progress = Phase::PH_SHELL);
  void try_add_edge(Task *consumer, Task *provider, Phase required_phase_to_progress);
  void task_advance_phase(Task *provider, Phase phase_reached, std::deque<Task *> &ready);

  enum Task_Result {
    TASK_RESULT_ERROR,
    TASK_RESULT_PROGRESS,
    TASK_RESULT_COMPLETE,
  };

  Task_Result try_solve_task(Task *task);
  Task_Result try_visit_block(Task *task, ASTBlock *node, Phase phase);
  Task_Result try_visit_function_declaration(Task *task, ASTFunctionDeclaration *node, Phase phase);
  Task_Result try_visit_alias(Task *task, ASTAlias *node, Phase phase);
  Task_Result try_visit_impl(Task *task, ASTImpl *node, Phase phase);
  Task_Result try_visit_import(Task *task, ASTImport *node, Phase phase);
  Task_Result try_visit_module(Task *task, ASTModule *node, Phase phase);
  Task_Result try_visit_return(Task *task, ASTReturn *node, Phase phase);
  Task_Result try_visit_continue(Task *task, ASTContinue *node, Phase phase);
  Task_Result try_visit_break(Task *task, ASTBreak *node, Phase phase);
  Task_Result try_visit_for(Task *task, ASTFor *node, Phase phase);
  Task_Result try_visit_if(Task *task, ASTIf *node, Phase phase);
  Task_Result try_visit_else(Task *task, ASTElse *node, Phase phase);
  Task_Result try_visit_while(Task *task, ASTWhile *node, Phase phase);
  Task_Result try_visit_struct_declaration(Task *task, ASTStructDeclaration *node, Phase phase);
  Task_Result try_visit_enum_declaration(Task *task, ASTEnumDeclaration *node, Phase phase);
  Task_Result try_visit_choice_declaration(Task *task, ASTChoiceDeclaration *node, Phase phase);
  Task_Result try_visit_trait_declaration(Task *task, ASTTraitDeclaration *node, Phase phase);
  Task_Result try_visit_variable(Task *task, ASTVariable *node, Phase phase);
  Task_Result try_visit_expr_statement(Task *task, ASTExprStatement *node, Phase phase);
  Task_Result try_visit_bin_expr(Task *task, ASTBinExpr *node, Phase phase);
  Task_Result try_visit_unary_expr(Task *task, ASTUnaryExpr *node, Phase phase);
  Task_Result try_visit_literal(Task *task, ASTLiteral *node, Phase phase);
  Task_Result try_visit_path(Task *task, ASTPath *node, Phase phase);
  Task_Result try_visit_type(Task *task, ASTType *node, Phase phase);
  Task_Result try_visit_tuple(Task *task, ASTTuple *node, Phase phase);
  Task_Result try_visit_call(Task *task, ASTCall *node, Phase phase);
  Task_Result try_visit_method_call(Task *task, ASTMethodCall *node, Phase phase);
  Task_Result try_visit_arguments(Task *task, ASTArguments *node, Phase phase);
  Task_Result try_visit_dot_expr(Task *task, ASTMemberAccess *node, Phase phase);
  Task_Result try_visit_index(Task *task, ASTIndex *node, Phase phase);
  Task_Result try_visit_initializer_list(Task *task, ASTInitializerList *node, Phase phase);
  Task_Result try_visit_size_of(Task *task, ASTSize_Of *node, Phase phase);
  Task_Result try_visit_type_of(Task *task, ASTType_Of *node, Phase phase);
  Task_Result try_visit_dyn_of(Task *task, ASTDyn_Of *node, Phase phase);
  Task_Result try_visit_defer(Task *task, ASTDefer *node, Phase phase);
  Task_Result try_visit_cast(Task *task, ASTCast *node, Phase phase);
  Task_Result try_visit_lambda(Task *task, ASTLambda *node, Phase phase);
  Task_Result try_visit_unpack(Task *task, ASTUnpack *node, Phase phase);
  Task_Result try_visit_unpack_element(Task *task, ASTUnpackElement *node, Phase phase);
  Task_Result try_visit_range(Task *task, ASTRange *node, Phase phase);
  Task_Result try_visit_switch(Task *task, ASTSwitch *node, Phase phase);
  Task_Result try_visit_destructure(Task *task, ASTDestructure *node, Phase phase);
  Task_Result try_visit_where(Task *task, ASTWhere *node, Phase phase);
  Task_Result try_visit_pattern_match(Task *task, ASTPatternMatch *node, Phase phase);
  Task_Result try_visit_statement_list(Task *task, ASTStatementList *node, Phase phase);
  Task_Result try_visit_where_statement(Task *task, ASTWhereStatement *node, Phase phase);
  Task_Result try_visit_run(Task *task, ASTRun *node, Phase phase);
};
