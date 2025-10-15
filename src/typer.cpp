#include "typer.hpp"
#include "ast.hpp"
#include "error.hpp"
#include "scope.hpp"
#include "interpreter.hpp"
#include "type.hpp"

void Typisting::collect_symbols(ASTNode *node) {
  if (!node) {
    return;
  }
  switch (node->get_node_type()) {
    case AST_NODE_PATTERN_MATCH: {
      ASTPatternMatch *pattern = (ASTPatternMatch *)node;
      switch (pattern->pattern_tag) {
        // for marker traits, therefore doesn't declare variables
        case ASTPatternMatch::MARKER:
          break;
        case ASTPatternMatch::STRUCT:
          for (auto &part : pattern->struct_pattern.parts) {
            pattern->target_block->scope->insert(part.var_name, nullptr, part.mutability, nullptr, {}, false);
          }
          break;
        case ASTPatternMatch::TUPLE:
          for (TuplePattern::Part &part : pattern->tuple_pattern.parts) {
            pattern->target_block->scope->insert(part.var_name, nullptr, part.mutability, nullptr, {}, false);
          }
          break;
      }
    } break;
    case AST_NODE_STATEMENT_LIST: {
      for (auto *stmt : ((ASTStatementList *)node)->statements) {
        collect_symbols(stmt);
      }
      break;
    }
    case AST_NODE_DESTRUCTURE: {
      ASTDestructure *decl = (ASTDestructure *)node;
      for (const auto &element : decl->elements) {
        decl->declaring_scope->insert(element.identifier, nullptr, element.mutability, nullptr, {}, false);
      }
    } break;
    case AST_NODE_VARIABLE: {
      ASTVariable *decl = (ASTVariable *)node;
      // nonlocal variables (module level, global level, etc) can be used out of order, so we must declare them now.
      // it wont hurt to make sure all variables are pre-declared from the symbol pass, in face
      // it should make it simpler later (we won't have to figure out whether it exists already or not)
      if (decl->declaring_scope->find(decl->name)) {
        throw_error(std::format("re-definition of variable '{}'", decl->name), node->source_range);
      }
      decl->declaring_scope->insert(decl->name, nullptr, decl->mutability, decl, {}, false);
    } break;
    case AST_NODE_STRUCT_DECLARATION: {
      ASTStructDeclaration *decl = (ASTStructDeclaration *)node;
      bool is_generic = decl->generic_parameters.size();
      if (decl->declaring_scope->find_type(decl->name, {}, is_generic, {})) {
        throw_error(std::format("re-definition of type '{}'", decl->name), node->source_range);
      }
      Type *type = decl->declaring_scope->create_struct_type(decl->name, decl, {}, is_generic);
      decl->resolved_type = type;
    } break;
    case AST_NODE_TRAIT_DECLARATION: {
      ASTTraitDeclaration *decl = (ASTTraitDeclaration *)node;
      bool is_generic = decl->generic_parameters.size();
      if (decl->declaring_scope->find_type(decl->name, {}, is_generic, {})) {
        throw_error(std::format("re-definition of type '{}'", decl->name), node->source_range);
      }
      Type *type = decl->declaring_scope->create_trait_type(decl->name, decl, {}, decl->generic_parameters.size());
      decl->resolved_type = type;
    } break;
    case AST_NODE_CHOICE_DECLARATION: {
      ASTChoiceDeclaration *decl = (ASTChoiceDeclaration *)node;
      bool is_generic = decl->generic_parameters.size();
      if (decl->declaring_scope->find_type(decl->name, {}, is_generic, {})) {
        throw_error(std::format("re-definition of type '{}'", decl->name), node->source_range);
      }
      Type *type = decl->declaring_scope->create_choice_type(decl->name, decl, {}, decl->generic_parameters.size());
      decl->resolved_type = type;
    } break;
    case AST_NODE_ENUM_DECLARATION: {
      ASTEnumDeclaration *decl = (ASTEnumDeclaration *)node;
      if (decl->declaring_scope->find_type(decl->name, {}, false, {})) {
        throw_error(std::format("re-definition of type '{}'", decl->name), node->source_range);
      }
      Type *type = decl->declaring_scope->create_enum_type(decl->name, decl, {});
      decl->resolved_type = type;
    } break;
    case AST_NODE_ALIAS: {
      ASTAlias *decl = (ASTAlias *)node;
      bool is_generic = decl->generic_parameters.size();
      if (decl->declaring_scope->find_type(decl->name, {}, is_generic, {})) {
        throw_error(std::format("re-definition of type '{}'", decl->name), node->source_range);
      }
      decl->declaring_scope->create_type_alias(decl->name, nullptr, decl, {}, decl->generic_parameters.size());
    } break;
    case AST_NODE_FUNCTION_DECLARATION: {
      ASTFunctionDeclaration *decl = (ASTFunctionDeclaration *)node;
      node->declaring_scope->insert(decl->name, nullptr, CONST, node, {}, decl->generic_parameters.size());
    } break;
    case AST_NODE_IMPL: {
      ASTImpl *impl = (ASTImpl *)(node);
      make_impl_unique_get_id(impl);
      impls.push_back(impl);
    } break;
    case AST_NODE_PROGRAM: {
      auto *program = (ASTProgram *)(node);
      for (auto *stmt : program->statements) {
        collect_symbols(stmt);
      }
      break;
    }
    case AST_NODE_BLOCK: {
      auto *block = (ASTBlock *)(node);
      for (auto *stmt : block->statements) {
        collect_symbols(stmt);
      }
      break;
    }
    case AST_NODE_MODULE: {
      auto *module = (ASTModule *)(node);
      for (auto *stmt : module->statements) {
        collect_symbols(stmt);
      }
      break;
    }
    case AST_NODE_IF: {
      auto *if_stmt = (ASTIf *)(node);
      collect_symbols(if_stmt->block);
      if (if_stmt->_else) {
        collect_symbols(if_stmt->_else.get());
      }
      break;
    }
    case AST_NODE_FOR: {
      auto *for_stmt = (ASTFor *)(node);
      if (for_stmt->left_tag == ASTFor::IDENTIFIER) {
        for_stmt->block->scope->insert(for_stmt->left.identifier, nullptr, MUT, nullptr, {}, false);
      } else {
        for (auto &element : for_stmt->left.destructure) {
          for_stmt->block->scope->insert(element.identifier, nullptr, element.mutability, nullptr, {}, false);
        }
      }
      collect_symbols(for_stmt->block);
      break;
    }
    case AST_NODE_WHILE: {
      auto *while_stmt = (ASTWhile *)(node);
      collect_symbols(while_stmt->block);
      break;
    }
    case AST_NODE_ELSE: {
      ASTElse *else_node = (ASTElse *)node;
      if (else_node->block) {
        collect_symbols(else_node->block.get());
      } else {
        collect_symbols(else_node->_if.get());
      }
    } break;
    case AST_NODE_SWITCH: {
      ASTSwitch *switch_node = (ASTSwitch *)node;
      for (const auto &branch : switch_node->branches) {
        collect_symbols(branch.block);
      }
    } break;
    case AST_NODE_LAMBDA:
      collect_symbols(((ASTLambda *)node)->block);
      break;
    case AST_NODE_DEFER:
      collect_symbols(((ASTDefer *)node)->statement);
      break;
    case AST_NODE_RUN:
      collect_symbols(((ASTRun *)node)->node_to_run);
      break;
    case AST_NODE_WHERE_STATEMENT: {
      ASTWhereStatement *where = (ASTWhereStatement *)node;
      // We have to determine whether this will even compile
      // later when all of its dependencies have been satisfied,
      // then we can submit the actually compiled block's dependencies
      where_statements.push_back(where);
    } break;
    default:
      break;
  }
}

void Typisting::try_generate_tasks_for_node(ASTNode *node, Task *consumer) {
  if (!node) {
    return;
  }

  // task(s) already exist for this node, don't generate duplicates.
  auto it = tasks.find(node);
  if (it != tasks.end()) {
    try_add_edge(consumer, it->second, Phase::PH_SOLVED);
    return;
  }

  // Scope *scope = node->declaring_scope;
  switch (node->get_node_type()) {
    case AST_NODE_TYPE: {
      ASTType *type = (ASTType *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      switch (type->tag) {
        case ASTType::NORMAL: {
          try_generate_tasks_for_node(type->normal.path, task);
        } break;
        case ASTType::TUPLE: {
          for (auto &type : type->tuple_types) {
            try_generate_tasks_for_node(type, task);
          }
        } break;
        case ASTType::FUNCTION: {
          for (auto &type : type->function.parameter_types) {
            try_generate_tasks_for_node(type, task);
          }
          try_generate_tasks_for_node(type->function.return_type, task);
        } break;
        case ASTType::SELF:
          break;  // TODO: how are we going to manage self types out of order?
        case ASTType::STRUCTURAL_DECLARATIVE_ASCRIPTION: {
          try_generate_tasks_for_node(type->declaration, task);
        } break;
      }
    } break;
    case AST_NODE_PATH: {
      ASTPath *path = (ASTPath *)node;
      for (auto &segment : path->segments) {
        switch (segment.tag) {
          case ASTPath::Segment::TYPE: {
            try_generate_tasks_for_node(segment.get_type(), consumer);
          } break;
          case ASTPath::Segment::IDENTIFIER: {
            // we can't really do anything here cause nothings resolved yet.
          } break;
          case ASTPath::Segment::INVALID:
            throw_error("INTERNAL COMPILER ERROR: path had invalid segment", node->source_range);
            break;
        }
      }
    } break;
    case AST_NODE_CALL: {
      ASTCall *call = (ASTCall *)node;
      // Generate tasks for the callee and arguments
      try_generate_tasks_for_node(call->callee, consumer);
      if (call->arguments) {
        try_generate_tasks_for_node(call->arguments, consumer);
      }
    } break;
    case AST_NODE_MEMBER_ACCESS: {
      ASTMemberAccess *dot = (ASTMemberAccess *)node;
      // Generate tasks for the base and member
      try_generate_tasks_for_node(dot->base, consumer);
      // dot->member is usually an identifier, which doesn't need further processing here
    } break;
    case AST_NODE_METHOD_CALL: {
      ASTMethodCall *method_call = (ASTMethodCall *)node;
      // generate dependencies for the dot expression.
      try_generate_tasks_for_node(method_call->callee, consumer);

      if (method_call->arguments) {
        try_generate_tasks_for_node(method_call->arguments, consumer);
      }
    } break;

    case AST_NODE_STATEMENT_LIST: {
      for (auto *stmt : ((ASTStatementList *)node)->statements) {
        try_generate_tasks_for_node(stmt, consumer);
      }
      break;
    }
    case AST_NODE_VARIABLE: {
      ASTVariable *decl = (ASTVariable *)node;

      const auto generate_tasks = [&](Task *consumer) {
        if (decl->type) {
          // TODO: how are we going to patch this back?
          try_generate_tasks_for_node(decl->type, consumer);
        }
        if (decl->value) {
          try_generate_tasks_for_node(decl->value.get(), consumer);
        }
      };

      // if it's a global variable, it's subject to being used out of order,
      // so we create a task for it.
      if (!decl->is_local) {
        Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
        generate_tasks(task);
      } else {
        generate_tasks(consumer);
      }
    } break;
    case AST_NODE_STRUCT_DECLARATION: {
      ASTStructDeclaration *decl = (ASTStructDeclaration *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      for (auto &field : decl->members) {
        try_generate_tasks_for_node(field.type, task);
        if (field.default_value) {
          try_generate_tasks_for_node(field.default_value.get(), task);
        }
      }
    } break;
    case AST_NODE_TRAIT_DECLARATION: {
      ASTTraitDeclaration *decl = (ASTTraitDeclaration *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      if (decl->where_clause) {
        try_generate_tasks_for_node(decl->where_clause.get(), task);
      }
      if (decl->trait_bounds.size()) {
        for (const auto &[target, constraint] : decl->trait_bounds) {
          try_generate_tasks_for_node(constraint, task);
        }
      }
      tasks[node] = task;
      for (auto *method : decl->methods) {
        try_generate_tasks_for_node(method, task);
      }
    } break;
    case AST_NODE_CHOICE_DECLARATION: {
      ASTChoiceDeclaration *decl = (ASTChoiceDeclaration *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      if (decl->where_clause) {
        try_generate_tasks_for_node(decl->where_clause.get(), task);
      }
      for (ASTChoiceVariant &variant : decl->variants) {
        switch (variant.kind) {
          case ASTChoiceVariant::MARKER_VARIANT:
            break;  // this can't depend on anything.
          case ASTChoiceVariant::TUPLE_VARIANT:
            try_generate_tasks_for_node(variant.tuple, task);
            break;
          case ASTChoiceVariant::STRUCT_VARIANT:
            for (auto *member : variant.struct_members) {
              try_generate_tasks_for_node(member->type, task);
              if (member->value) {
                try_generate_tasks_for_node(member->value.get(), task);
              }
            }
            break;
        }
      }
    } break;
    case AST_NODE_ENUM_DECLARATION: {
      ASTEnumDeclaration *decl = (ASTEnumDeclaration *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(decl, consumer);
      for (const auto &[_, enumeration_value] : decl->enumerations) {
        (void)_;  // -.-
        try_generate_tasks_for_node(enumeration_value, task);
      }
    } break;
    case AST_NODE_ALIAS: {
      ASTAlias *decl = (ASTAlias *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(decl, consumer);
      try_generate_tasks_for_node(decl->source_node, task);
    } break;
    case AST_NODE_FUNCTION_DECLARATION: {
      ASTFunctionDeclaration *decl = (ASTFunctionDeclaration *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);

      if (decl->where_clause) {
        try_generate_tasks_for_node(decl->where_clause.get(), task);
      }

      try_generate_tasks_for_node(decl->return_type, task);

      for (const auto &param : decl->parameters.values) {
        switch (param.tag) {
          case PARAM_IS_SELF_BY_VALUE:
          case PARAM_IS_SELF_BY_CONST_POINTER:
          case PARAM_IS_SELF_BY_MUT_POINTER:
            continue;
          case PARAM_IS_NAMED:
            try_generate_tasks_for_node(param.named.type, task);
            break;
          case PARAM_IS_NAMELESS:
            try_generate_tasks_for_node(param.nameless.type, task);
            break;
        }
      }

      if (decl->block) {
        try_generate_tasks_for_node(decl->block.get(), task);
      }
    } break;
    case AST_NODE_IMPL: {
      ASTImpl *decl = (ASTImpl *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      try_generate_tasks_for_node(decl->target, task);
      if (decl->trait) {
        try_generate_tasks_for_node(decl->trait.get(), task);
      }
      if (decl->where_clause) {
        try_generate_tasks_for_node(decl->where_clause.get(), task);
      }
      for (auto *alias : decl->aliases) {
        try_generate_tasks_for_node(alias, task);
      }
      for (auto *constant : decl->constants) {
        try_generate_tasks_for_node(constant, task);
      }
      for (auto *method : decl->methods) {
        try_generate_tasks_for_node(method, task);
      }
    } break;
    case AST_NODE_PROGRAM: {
      auto *program = (ASTProgram *)(node);
      for (auto *stmt : program->statements) {
        // the program doesn't need a task, we don't care.
        try_generate_tasks_for_node(stmt, nullptr);
      }

    } break;
    case AST_NODE_BLOCK: {
      auto *block = (ASTBlock *)(node);
      for (auto *stmt : block->statements) {
        try_generate_tasks_for_node(stmt, consumer);
      }
    } break;
    case AST_NODE_MODULE: {
      auto *module = (ASTModule *)(node);
      Task *task = create_task_bind_to_node_create_edge_to_consumer(module, consumer);
      for (auto *stmt : module->statements) {
        try_generate_tasks_for_node(stmt, task);
      }
    } break;
    case AST_NODE_IF: {
      auto *if_stmt = (ASTIf *)(node);
      try_generate_tasks_for_node(if_stmt->condition, consumer);
      try_generate_tasks_for_node(if_stmt->block, consumer);
      if (if_stmt->_else) {
        try_generate_tasks_for_node(if_stmt->_else.get(), consumer);
      }
    } break;
    case AST_NODE_FOR: {
      auto *for_stmt = (ASTFor *)(node);
      // We don't create a dependency for the left hand side since
      // the right will fully describe that dependency
      try_generate_tasks_for_node(for_stmt->right, consumer);
      try_generate_tasks_for_node(for_stmt->block, consumer);
    } break;
    case AST_NODE_WHILE: {
      auto *while_stmt = (ASTWhile *)(node);
      if (while_stmt->condition) {
        try_generate_tasks_for_node(while_stmt->condition.get(), consumer);
      }
      try_generate_tasks_for_node(while_stmt->block, consumer);
    } break;
    case AST_NODE_ELSE: {
      ASTElse *else_stmt = (ASTElse *)node;
      if (else_stmt->_if) {
        try_generate_tasks_for_node(else_stmt->_if.get(), consumer);
      } else {
        try_generate_tasks_for_node(else_stmt->block.get(), consumer);
      }
    } break;
    case AST_NODE_SWITCH: {
      ASTSwitch *switch_node = (ASTSwitch *)node;
      for (const auto &branch : switch_node->branches) {
        try_generate_tasks_for_node(branch.expression, consumer);
        try_generate_tasks_for_node(branch.block, consumer);
      }
    } break;
    case AST_NODE_LAMBDA: {
      ASTLambda *lambda = (ASTLambda *)node;
      Task *task = create_task_bind_to_node_create_edge_to_consumer(node, consumer);
      for (const auto &param : lambda->parameters.values) {
        switch (param.tag) {
          case PARAM_IS_SELF_BY_VALUE:
          case PARAM_IS_SELF_BY_CONST_POINTER:
          case PARAM_IS_SELF_BY_MUT_POINTER:
            continue;
          case PARAM_IS_NAMED:
            try_generate_tasks_for_node(param.named.type, task);
            break;
          case PARAM_IS_NAMELESS:
            try_generate_tasks_for_node(param.nameless.type, task);
            break;
        }
      }
      if (lambda->return_type) {
        try_generate_tasks_for_node(lambda->return_type, task);
      }
      if (lambda->block) {
        try_generate_tasks_for_node(lambda->block, task);
      }
    } break;
    case AST_NODE_DEFER: {
      ASTDefer *defer = (ASTDefer *)node;
      try_generate_tasks_for_node(defer->statement, consumer);
    } break;
    case AST_NODE_RUN: {
      ASTRun *run = (ASTRun *)node;
      try_generate_tasks_for_node(run->node_to_run, consumer);
    } break;
    case AST_NODE_WHERE_STATEMENT: {
      // We have to determine whether this will even compile
      // later when all of its dependencies have been satisfied,
      // then we can submit the actually compiled block's dependencies

      // so these are actually the last thing to be typed (from a root node perspective)
      // since we don't wanna just type every block of it
      // (incompatible generic constraints would cause errors, the point of this node is to allow that with constraints)
    } break;
    case AST_NODE_RETURN: {
      ASTReturn *return_stmt = (ASTReturn *)node;
      if (return_stmt->expression) {
        try_generate_tasks_for_node(return_stmt->expression.get(), consumer);
      }
    } break;
    case AST_NODE_EXPR_STATEMENT: {
      ASTExprStatement *expr_stmt = (ASTExprStatement *)node;
      try_generate_tasks_for_node(expr_stmt->expression, consumer);
    } break;
    case AST_NODE_BIN_EXPR: {
      ASTBinExpr *binary = (ASTBinExpr *)node;
      try_generate_tasks_for_node(binary->left, consumer);
      try_generate_tasks_for_node(binary->right, consumer);
    } break;
    case AST_NODE_UNARY_EXPR: {
      ASTUnaryExpr *unary = (ASTUnaryExpr *)node;
      try_generate_tasks_for_node(unary->operand, consumer);
    } break;
    case AST_NODE_TUPLE: {
      ASTTuple *tuple = (ASTTuple *)node;
      for (auto *expr : tuple->values) {
        try_generate_tasks_for_node(expr, consumer);
      }
    } break;
    case AST_NODE_ARGUMENTS: {
      ASTArguments *arguments = (ASTArguments *)node;
      for (auto *arg : arguments->values) {
        try_generate_tasks_for_node(arg, consumer);
      }
    } break;
    case AST_NODE_INDEX: {
      ASTIndex *unary = (ASTIndex *)node;
      try_generate_tasks_for_node(unary->base, consumer);
      try_generate_tasks_for_node(unary->index, consumer);
    } break;
    case AST_NODE_INITIALIZER_LIST: {
      ASTInitializerList *list = (ASTInitializerList *)node;
      switch (list->tag) {
        case ASTInitializerList::INIT_LIST_NAMED: {
          for (const auto &[_, value] : list->key_values) {
            (void)_;
            try_generate_tasks_for_node(value, consumer);
          }
        } break;
        case ASTInitializerList::INIT_LIST_COLLECTION: {
          for (const auto value : list->values) {
            try_generate_tasks_for_node(value, consumer);
          }
        } break;
        default:
          break;
      }
    } break;
    case AST_NODE_SIZE_OF: {
      ASTSize_Of *size_of = (ASTSize_Of *)node;
      try_generate_tasks_for_node(size_of->target_type, consumer);
    } break;
    case AST_NODE_TYPE_OF: {
      ASTType_Of *type_of = (ASTType_Of *)node;
      try_generate_tasks_for_node(type_of->target_type, consumer);
    } break;
    case AST_NODE_DYN_OF: {
      ASTDyn_Of *dyn_of = (ASTDyn_Of *)node;
      try_generate_tasks_for_node(dyn_of->object, consumer);
      if (dyn_of->trait_type) {
        try_generate_tasks_for_node(dyn_of->trait_type, consumer);
      }
    } break;
    case AST_NODE_CAST: {
      ASTCast *cast = (ASTCast *)node;
      try_generate_tasks_for_node(cast->target_type, consumer);
      try_generate_tasks_for_node(cast->expression, consumer);
    } break;
    case AST_NODE_RANGE: {
      ASTRange *range = (ASTRange *)node;
      if (range->left) {
        try_generate_tasks_for_node(range->left.get(), consumer);
      }
      if (range->right) {
        try_generate_tasks_for_node(range->right.get(), consumer);
      }
    } break;
    case AST_NODE_DESTRUCTURE: {
      ASTDestructure *destructure = (ASTDestructure *)node;
      try_generate_tasks_for_node(destructure->right, consumer);
    } break;
    case AST_NODE_WHERE: {
      ASTWhere *where = (ASTWhere *)node;
      for (auto &[target, predicate] : where->constraints) {
        try_generate_tasks_for_node(target, consumer);
        try_generate_tasks_for_node(predicate, consumer);
      }
    } break;
    case AST_NODE_PATTERN_MATCH: {
      ASTPatternMatch *pattern = (ASTPatternMatch *)node;
      // I think we only really depend on the object, since the RHS of the 'is'
      // will only use things declared or depended on by the object.
      try_generate_tasks_for_node(pattern->object, consumer);
    } break;
    case AST_NODE_UNPACK: {
      ASTUnpack *unpack = (ASTUnpack *)node;
      try_generate_tasks_for_node(unpack->expression, consumer);
    } break;
    case AST_NODE_UNPACK_ELEMENT: {
      // idk lets ignore this for now
      ASTUnpackElement *element = (ASTUnpackElement *)node;
      (void)element;
    } break;
    // Ignored nodes.
    case AST_NODE_NOOP:
    case AST_NODE_IMPORT:
    case AST_NODE_CONTINUE:
    case AST_NODE_BREAK:
    case AST_NODE_LITERAL:
      break;
  }
}

void Typisting::enqueue_if_ready(Task *task) {
  if (!task || task->phase == PH_SOLVED || task->indegree != 0 || task->queued) {
    return;
  }
  task->queued = true;
  ready_queue.push_back(task);
}

Task *Typisting::create_task_bind_to_node_create_edge_to_consumer(ASTNode *node, Task *consumer,
                                                                  Phase required_phase_to_progress) {
  Task *task = new (task_arena.allocate(sizeof(Task))) Task();
  task->node = node;
  tasks[node] = task;

  if (consumer) {
    try_add_edge(consumer, task, required_phase_to_progress);
  }

  enqueue_if_ready(task);
  return task;
}

void Typisting::try_add_edge(Task *consumer, Task *provider, Phase required_phase_to_progress) {
  if (!consumer || !provider || consumer == provider) return;

  for (Dep &d : consumer->depends_on) {
    if (d.provider == provider) {
      if (required_phase_to_progress > d.need) d.need = required_phase_to_progress;
      bool was_satisfied = d.satisfied;
      d.satisfied = (provider->phase >= d.need);
      if (!was_satisfied && !d.satisfied) consumer->indegree++;
      return;
    }
  }

  Dep dep{provider, required_phase_to_progress, false};
  dep.satisfied = (provider->phase >= dep.need);
  consumer->depends_on.push_back(dep);
  provider->dependents.push_back(consumer);
  if (!dep.satisfied) consumer->indegree++;
}

void Typisting::run(ASTProgram *node) {
  collect_symbols(node);
  try_generate_tasks_for_node(node, nullptr);

  // seed
  for (auto &[_, task] : tasks) {
    enqueue_if_ready(task);
  }

  while (!ready_queue.empty()) {
    Task *t = ready_queue.front();
    ready_queue.pop_front();
    t->queued = false;  // allow re-queueing later

    Phase before = t->phase;
    Task_Result result = try_solve_task(t);

    // Advance phase based on result
    if (result == TASK_RESULT_COMPLETE) {
      t->phase = PH_SOLVED;
    } else if (result == TASK_RESULT_PROGRESS && t->phase < PH_SOLVED) {
      t->phase = (Phase)std::min<int>(t->phase + 1, PH_SOLVED);
    }

    Phase after = t->phase;
    if (after > before) {
      task_advance_phase(t, after, ready_queue);
    }

    // If this task still has work and remains unblocked, keep driving it
    if (t->phase < PH_SOLVED && t->indegree == 0) {
      enqueue_if_ready(t);
    }
  }

  std::set<Task *> unsolved;
  for (auto &[_, task] : tasks) {
    if (task->phase != PH_SOLVED) unsolved.insert(task);
  }

  if (!unsolved.empty()) {
    std::stringstream error_report;
    for (Task *task : unsolved) {
      error_report << format_source_location(task->node->source_range, ERROR_FAILURE);
    }
    throw_error("Type scheduler deadlock: " + std::to_string(unsolved.size()) + " task(s) remain unsolved.\nCauses:\n" +
                    error_report.str(),
                node->source_range);
  }
}

void Typisting::task_advance_phase(Task *provider, Phase phase_reached, std::deque<Task *> &ready) {
  for (Task *consumer : provider->dependents) {
    bool became_ready = false;
    for (Dep &dep : consumer->depends_on) {
      if (!dep.satisfied && dep.provider == provider && dep.need <= phase_reached) {
        dep.satisfied = true;
        if (consumer->indegree > 0) {
          consumer->indegree--;
        }
        if (consumer->indegree == 0) {
          became_ready = true;
        }
      }
    }
    if (became_ready) {
      ready.push_back(consumer);
    }
  }
}

Task_Result Typisting::try_solve_task(Task *task) {
  auto &node = task->node;
  switch (node->get_node_type()) {
    case AST_NODE_BLOCK:
      return try_visit_block((ASTBlock *)node);
      break;
    case AST_NODE_FUNCTION_DECLARATION:
      return try_visit_function_declaration((ASTFunctionDeclaration *)node);
      break;
    case AST_NODE_ALIAS:
      return try_visit_alias((ASTAlias *)node);
      break;
    case AST_NODE_IMPL:
      return try_visit_impl((ASTImpl *)node);
      break;
    case AST_NODE_IMPORT:
      return try_visit_import((ASTImport *)node);
      break;
    case AST_NODE_MODULE:
      return try_visit_module((ASTModule *)node);
      break;
    case AST_NODE_RETURN:
      return try_visit_return((ASTReturn *)node);
      break;
    case AST_NODE_CONTINUE:
      return try_visit_continue((ASTContinue *)node);
      break;
    case AST_NODE_BREAK:
      return try_visit_break((ASTBreak *)node);
      break;
    case AST_NODE_FOR:
      return try_visit_for((ASTFor *)node);
      break;
    case AST_NODE_IF:
      return try_visit_if((ASTIf *)node);
      break;
    case AST_NODE_ELSE:
      return try_visit_else((ASTElse *)node);
      break;
    case AST_NODE_WHILE:
      return try_visit_while((ASTWhile *)node);
      break;
    case AST_NODE_STRUCT_DECLARATION:
      return try_visit_struct_declaration((ASTStructDeclaration *)node);
      break;
    case AST_NODE_ENUM_DECLARATION:
      return try_visit_enum_declaration((ASTEnumDeclaration *)node);
      break;
    case AST_NODE_CHOICE_DECLARATION:
      return try_visit_choice_declaration((ASTChoiceDeclaration *)node);
      break;
    case AST_NODE_TRAIT_DECLARATION:
      return try_visit_trait_declaration((ASTTraitDeclaration *)node);
      break;
    case AST_NODE_VARIABLE:
      return try_visit_variable((ASTVariable *)node);
      break;
    case AST_NODE_EXPR_STATEMENT:
      return try_visit_expr_statement((ASTExprStatement *)node);
      break;
    case AST_NODE_BIN_EXPR:
      return try_visit_bin_expr((ASTBinExpr *)node);
      break;
    case AST_NODE_UNARY_EXPR:
      return try_visit_unary_expr((ASTUnaryExpr *)node);
      break;
    case AST_NODE_LITERAL:
      return try_visit_literal((ASTLiteral *)node);
      break;
    case AST_NODE_PATH:
      return try_visit_path((ASTPath *)node);
      break;
    case AST_NODE_TYPE:
      return try_visit_type((ASTType *)node);
      break;
    case AST_NODE_TUPLE:
      return try_visit_tuple((ASTTuple *)node);
      break;
    case AST_NODE_CALL:
      return try_visit_call((ASTCall *)node);
      break;
    case AST_NODE_METHOD_CALL:
      return try_visit_method_call((ASTMethodCall *)node);
      break;
    case AST_NODE_ARGUMENTS:
      return try_visit_arguments((ASTArguments *)node);
      break;
    case AST_NODE_MEMBER_ACCESS:
      return try_visit_dot_expr((ASTMemberAccess *)node);
      break;
    case AST_NODE_INDEX:
      return try_visit_index((ASTIndex *)node);
      break;
    case AST_NODE_INITIALIZER_LIST:
      return try_visit_initializer_list((ASTInitializerList *)node);
      break;
    case AST_NODE_SIZE_OF:
      return try_visit_size_of((ASTSize_Of *)node);
      break;
    case AST_NODE_TYPE_OF:
      return try_visit_type_of((ASTType_Of *)node);
      break;
    case AST_NODE_DYN_OF:
      return try_visit_dyn_of((ASTDyn_Of *)node);
      break;
    case AST_NODE_DEFER:
      return try_visit_defer((ASTDefer *)node);
      break;
    case AST_NODE_CAST:
      return try_visit_cast((ASTCast *)node);
      break;
    case AST_NODE_LAMBDA:
      return try_visit_lambda((ASTLambda *)node);
      break;
    case AST_NODE_UNPACK:
      return try_visit_unpack((ASTUnpack *)node);
      break;
    case AST_NODE_UNPACK_ELEMENT:
      return try_visit_unpack_element((ASTUnpackElement *)node);
      break;
    case AST_NODE_RANGE:
      return try_visit_range((ASTRange *)node);
      break;
    case AST_NODE_SWITCH:
      return try_visit_switch((ASTSwitch *)node);
      break;
    case AST_NODE_DESTRUCTURE:
      return try_visit_destructure((ASTDestructure *)node);
      break;
    case AST_NODE_WHERE:
      return try_visit_where((ASTWhere *)node);
      break;
    case AST_NODE_PATTERN_MATCH:
      return try_visit_pattern_match((ASTPatternMatch *)node);
      break;
    case AST_NODE_STATEMENT_LIST:
      return try_visit_statement_list((ASTStatementList *)node);
      break;
    case AST_NODE_WHERE_STATEMENT:
      return try_visit_where_statement((ASTWhereStatement *)node);
      break;
    case AST_NODE_RUN:
      return try_visit_run((ASTRun *)node);
      break;
    case AST_NODE_PROGRAM:
    case AST_NODE_NOOP:
      return TASK_RESULT_COMPLETE;
  }
}

Task_Result Typisting::try_visit_block(ASTBlock *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_function_declaration(ASTFunctionDeclaration *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_alias(ASTAlias *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_impl(ASTImpl *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_import(ASTImport *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_module(ASTModule *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_return(ASTReturn *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_continue(ASTContinue *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_break(ASTBreak *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_for(ASTFor *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_if(ASTIf *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_else(ASTElse *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_while(ASTWhile *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_struct_declaration(ASTStructDeclaration *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_enum_declaration(ASTEnumDeclaration *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_choice_declaration(ASTChoiceDeclaration *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_trait_declaration(ASTTraitDeclaration *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_variable(ASTVariable *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_expr_statement(ASTExprStatement *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_bin_expr(ASTBinExpr *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_unary_expr(ASTUnaryExpr *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_literal(ASTLiteral *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_path(ASTPath *) { return TASK_RESULT_COMPLETE; }

std::vector<TypeExtension> accept_extensions(const std::vector<ASTTypeExtension> &ast_extensions) {
  std::vector<TypeExtension> extensions;
  for (auto &ext : ast_extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      auto val = interpret_from_ast(ext.expression);
      if (val->get_value_type() != ValueType::INTEGER) {
        throw_error("Fixed array must have integer size.", ext.expression->source_range);
      }
      extensions.push_back({ext.type, val->as<IntValue>()->value});
    } else {
      extensions.push_back({.type = ext.type});
    }
  }
  return extensions;
}

Task_Result Typisting::try_visit_type(ASTType *ast) {
  switch (ast->tag) {
    case ASTType::NORMAL: {
      if (is_done(ast->normal.path)) {
        Type *type = ast->normal.path->resolved_type;
        std::vector<TypeExtension> extensions = accept_extensions(ast->extensions);
        ast->resolved_type = global_find_type_id(type, extensions);
        return TASK_RESULT_COMPLETE;
      }
      return TASK_RESULT_STALLED;
    } break;
    case ASTType::TUPLE:
    case ASTType::FUNCTION:
    case ASTType::SELF:
    case ASTType::STRUCTURAL_DECLARATIVE_ASCRIPTION:
      break;
  }
  return TASK_RESULT_ERROR;
}
Task_Result Typisting::try_visit_tuple(ASTTuple *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_call(ASTCall *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_method_call(ASTMethodCall *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_arguments(ASTArguments *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_dot_expr(ASTMemberAccess *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_index(ASTIndex *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_initializer_list(ASTInitializerList *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_size_of(ASTSize_Of *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_type_of(ASTType_Of *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_dyn_of(ASTDyn_Of *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_defer(ASTDefer *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_cast(ASTCast *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_lambda(ASTLambda *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_unpack(ASTUnpack *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_unpack_element(ASTUnpackElement *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_range(ASTRange *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_switch(ASTSwitch *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_destructure(ASTDestructure *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_where(ASTWhere *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_pattern_match(ASTPatternMatch *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_statement_list(ASTStatementList *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_where_statement(ASTWhereStatement *) { return TASK_RESULT_COMPLETE; }
Task_Result Typisting::try_visit_run(ASTRun *) { return TASK_RESULT_COMPLETE; }

/*
  void visit_enum_declaration(ASTEnumDeclaration *node, Phase phase) {
    Scope *scope = node->declaring_scope;
    if (scope->find_type(node->name, {}, false, {}) != Type::INVALID_TYPE) {
      throw_error("Redefinition of enum " + node->name.str(), node->source_range);
    }
    auto underlying_type = Type::INVALID_TYPE;
    if (node->underlying_type_ast) {
      visit_node(node->underlying_type_ast, phase);
      underlying_type = node->underlying_type_ast->resolved_type;
    }
    auto enum_type = scope->create_enum_type(node->name, node->is_flags, node);
    enum_type->declaring_node = node;
    auto info = enum_type->info->as<EnumTypeInfo>();
    for (auto &[key, value] : node->enumerations) {
      visit_node(value, phase);
      Type *node_ty = value->resolved_type;
      Value *evaluated = interpret_from_ast(value);
      size_t evaluated_integer = evaluated->as<IntValue>()->value;
      constexpr uint64_t u32_max = 0xFFFFFFFFULL;
      constexpr int64_t s32_max = 0x7FFFFFFF;
      constexpr int64_t s32_min = -0x80000000;
      // ! BUG
      // Why would we change the sign only if it reaches greater than s32, default should be u8 -> u16 -> u32
      // based on the number of enumerations / their value
      // we should dynamically promote the unsigned integer from u8 to u64 if need be.
      if (evaluated_integer > s32_max || evaluated_integer < s32_min) {
        if (evaluated_integer <= u32_max) {
          underlying_type = u32_type();
        } else {
          underlying_type = u64_type();
        }
      }
      TypeMember member = {.name = key, .type = node_ty, .default_value = value};
      info->members.push_back(member);
      if (underlying_type == Type::INVALID_TYPE) {
        underlying_type = node_ty;
      } else {
        assert_types_can_cast_or_are_equal(value, underlying_type, node->source_range,
                                          "inconsistent types in enum declaration.");
      }
    }
    if (underlying_type == void_type()) {
      throw_error("Invalid enum declaration: got void for underlying type", node->source_range);
    }
    node->underlying_type = underlying_type;
    info->underlying_type = underlying_type;
    node->resolved_type = enum_type;
  }
*/