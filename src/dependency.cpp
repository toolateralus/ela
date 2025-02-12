#include "ast.hpp"
#include "core.hpp"
#include "error.hpp"
#include "lex.hpp"
#include "type.hpp"
#include "visitor.hpp"


[[nodiscard]] std::string DependencyEmitter::decl_type(int type_id) {
  auto type = global_get_type(type_id);
  auto extensions = type->meta.extensions;
  for (auto meta : extensions) {
    if (meta.type == TYPE_EXT_POINTER) {
      emitter.forward_decl_type(type);
      return {};
    }
  }
  return define_type(type_id);
}

[[nodiscard]] std::string DependencyEmitter::define_type(int type_id) {
  auto type = global_get_type(type_id);
  if (type->base_id != Type::INVALID_TYPE_ID) {
    type = global_get_type(type->base_id);
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->info.function;
      auto err = decl_type(info.return_type);
      if (!err.empty()) {
        return err;
      }
      for (const auto &parameter: info.parameter_types) {
        err = decl_type(parameter);
        if (!err.empty()) {
          return err;
        }
      }
    } break;
    case TYPE_STRUCT: {
      if (type->declaring_node.is_not_null()) {
        visit(type->declaring_node.get());
        emitter.visit(type->declaring_node.get());
      } else {
        return "internal compiler error: could not locate node for struct";
      }
    } break;
    case TYPE_TUPLE: {
      auto info = type->info.tuple;
      for (auto type : info.types) {
        auto err = define_type(type);
        if (!err.empty()) {
          return err;
        }
      }
      emitter.emit_tuple(type_id);
    } break;
    case TYPE_ENUM:
      // TODO: enums should be handled here
    case TYPE_INTERFACE:
    case TYPE_SCALAR:
      break;
  }
  return {};
}


void DependencyEmitter::visit_program(AST *node) {
  for (auto &statement : node->program_statements) {
    visit(statement);
  }
}

void DependencyEmitter::visit_block(AST *node) {
  for (auto &statement : node->block.statements) {
    visit(statement);
  }
}

void DependencyEmitter::visit_parameters(Source_Range source_range, std::vector<AST_Parameter_Declaration> &params) {
  for (auto &param : params) {
    if (param.tag == AST_PARAM_SELF) {
      // do we even need to do this?
    } else {
      visit(param.normal.type);
    }
  }
}

void DependencyEmitter::visit_function_declaration(AST *node) {
  if (!node->function.generic_parameters.empty()) {
    return;
  }
  auto err = decl_type(node->function.return_type->resolved_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }
  visit_parameters(node->source_range, node->function.parameters);
  if (node->function.block.is_not_null()) {
    visit(node->function.block.get());
  }
}

void DependencyEmitter::visit_declaration(AST *node) {
  visit(node->declaration.type);
  if (node->declaration.value.is_not_null()) {
    visit(node->declaration.value.get());
  }
}

void DependencyEmitter::visit_bin_expr(AST *node) {
  if (node->binary.is_operator_overload) {
    AST call(AST_CALL);
    AST dot(AST_DOT);
    dot.dot.base = node->binary.left;
    dot.dot.member_name = get_operator_overload_name(node->binary.op, OPERATION_BINARY);
    call.call.callee = &dot;
    call.call.arguments = {node->binary.right};
    visit(&call);
  } else {
    visit(node->binary.left);
    visit(node->binary.right);
  }
}

void DependencyEmitter::visit_unary_expr(AST *node) {
  if (node->unary.is_operator_overload) {
    AST call(AST_CALL);
    AST dot(AST_DOT);
    dot.dot.base = node->unary.operand;
    dot.dot.member_name = get_operator_overload_name(node->unary.op, OPERATION_UNARY);
    call.call.callee = &dot;
    visit(&call);
  } else {
    if (node->unary.op == Token_Type::Mul) {
      auto err = define_type(node->unary.operand->resolved_type);
      if (!err.empty()) {
        throw_error(err, node->unary.operand->source_range);
      }
    }
    visit(node->unary.operand);
  }
}

void DependencyEmitter::visit_identifier(AST *node) {
  auto type = global_get_type(node->resolved_type);
  if (type && type->kind == TYPE_ENUM) {
    visit(type->declaring_node.get());
  }
  if (auto symbol = node->scope.lookup(node->identifier)) {
    if (symbol->is_variable() && symbol->variable.declaration) {
      auto decl = symbol->variable.declaration.get();
      if (!decl->declaring_block) {
        visit(decl);
      }
    } else if (symbol->is_function()) {
      visit(symbol->function.declaration);
    }
  }
}

void DependencyEmitter::visit_literal(AST *node) {}

void DependencyEmitter::visit_type(AST *node) {
  auto err = decl_type(node->resolved_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }
}

void DependencyEmitter::visit_call(AST *node) {
  for (auto &generic_arg : node->call.generic_arguments) {
    visit(generic_arg);
  }
  for (auto &arg : node->call.arguments) {
    visit(arg);
  }
  auto symbol_nullable = emitter.typer.get_symbol(node->call.callee);
  if (symbol_nullable.is_not_null()) {
    auto decl = symbol_nullable.get()->function.declaration;
    if (!node->call.generic_arguments.empty()) {
      auto generic_args = emitter.typer.get_generic_arg_types(node->call.generic_arguments);
      decl = find_generic_instance(decl->function.generic_instantiations, generic_args);
    }
    if (decl) {
      visit(decl);
    }
  } else {
    visit(node->call.callee);
  }
}

void DependencyEmitter::visit_return(AST *node) {
  if (node->$return.is_not_null()) {
    visit(node->$return.get());
  }
}

void DependencyEmitter::visit_continue(AST *node) {}

void DependencyEmitter::visit_break(AST *node) {}

void DependencyEmitter::visit_for(AST *node) {
  auto err = define_type(node->$for.iterable_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }
  err = define_type(node->$for.range_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }
  err = define_type(node->$for.identifier_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }

  visit(node->$for.range);

  auto range_scope = global_get_type(node->$for.range_type)->info.scope;
  auto iter_scope = global_get_type(node->$for.iterable_type)->info.scope;

  switch (node->$for.iteration_kind) {
    case Iteration_Kind::ITERABLE: {
      auto iter_sym = range_scope.lookup("iter");
      visit(iter_sym->function.declaration);
    } break;
    case Iteration_Kind::ENUMERABLE: {
      auto enum_sym = range_scope.lookup("enumerator");
      visit(enum_sym->function.declaration);
    } break;
    case Iteration_Kind::ENUMERATOR:
    case Iteration_Kind::ITERATOR:
      break;
  }

  auto done_sym = iter_scope.lookup("done");
  visit(done_sym->function.declaration);

  auto current_sym = iter_scope.lookup("current");
  visit(current_sym->function.declaration);

  auto next_sym = iter_scope.lookup("next");
  visit(next_sym->function.declaration);

  visit(node->$for.block);
}

void DependencyEmitter::visit_if(AST *node) {
  visit(node->$if.condition);
  visit(node->$if.block);
  if (node->$if.$else.is_not_null()) {
    visit(node->$if.$else.get());
  }
}

void DependencyEmitter::visit_else(AST *node) {
  if (node->$else.elseif.is_not_null()) {
    visit(node->$else.elseif.get());
  }
  if (node->$else.block.is_not_null()) {
    visit(node->$else.block.get());
  }
}

void DependencyEmitter::visit_while(AST *node) {
  if (node->$while.condition.is_not_null()) {
    visit(node->$while.condition.get());
  }
  visit(node->$while.block);
}

void DependencyEmitter::visit_struct_declaration(AST *node) {
  if (!node->$struct.generic_parameters.empty()) {
    return;
  }
  for (auto &member : node->$struct.members) {
    auto err = decl_type(member.type->resolved_type);
    if (!err.empty()) {
      throw_error(err, member.type->source_range);
    }
  }
}

void DependencyEmitter::visit_dot_expr(AST *node) {
  auto err = define_type(node->dot.base->resolved_type);
  if (!err.empty()) {
    throw_error(err, node->dot.base->source_range);
  }
  visit(node->dot.base);
}

void DependencyEmitter::visit_scope_resolution(AST *node) { visit(node->scope_resolution.base); }

void DependencyEmitter::visit_subscript(AST *node) {
  if (node->subscript.is_operator_overload) {
    AST call(AST_CALL);
    AST dot(AST_DOT);
    dot.dot.base = node->subscript.left;
    dot.dot.member_name = get_operator_overload_name(Token_Type::LBrace, OPERATION_SUBSCRIPT);
    call.call.callee = &dot;
    call.call.arguments = { node->subscript.index_expression };
    visit(&call);
  } else {
    visit(node->subscript.left);
    visit(node->subscript.index_expression);
  }
}

void DependencyEmitter::visit_initializer_list(AST *node) {
  if (node->initializer.target_type.is_not_null()) {
    visit(node->initializer.target_type.get());
  }
  if (node->initializer.tag == INITIALIZER_COLLECTION) {
    for (const auto &value : node->initializer.values) {
      visit(value);
    }
  } else {
    for (const auto &[key, value] : node->initializer.key_values) {
      visit(value);
    }
  }
}

void DependencyEmitter::visit_enum_declaration(AST *node) {
  for (const auto &[key, value] : node->$enum.key_values) {
    visit(value);
  }
}

void DependencyEmitter::visit_noop(AST *node) {}

void DependencyEmitter::visit_alias(AST *node) {}

void DependencyEmitter::visit_impl(AST *node) {
  if (!node->impl.generic_parameters.empty()) {
    return;
  }
  for (const auto &method : node->impl.methods) {
    visit(method);
  }
}

void DependencyEmitter::visit_interface_declaration(AST *node) {}

void DependencyEmitter::visit_size_of(AST *node) { visit(node->size_of); }

void DependencyEmitter::visit_defer(AST *node) { visit(node->defer); }

void DependencyEmitter::visit_cast(AST *node) {
  visit(node->cast.expression);
  visit(node->cast.target_type);
}

void DependencyEmitter::visit_lambda(AST *node) {
  visit_parameters(node->source_range, node->lambda.parameters);
  visit(node->lambda.block);
}

void DependencyEmitter::visit_range(AST *node) {
  visit(node->range.left);
  visit(node->range.right);
}

void DependencyEmitter::visit_switch(AST *node) {
  visit(node->$switch.target);
  for (const auto &case_ : node->$switch.cases) {
    visit(case_.block);
    visit(case_.expression);
  }
}

void DependencyEmitter::visit_tuple(AST *node) {
  for (const auto &value : node->tuple) {
    visit(value);
  }
}

void DependencyEmitter::visit_tuple_deconstruction(AST *node) {
  auto err = define_type(node->tuple_deconstruction.right->resolved_type);
  if (!err.empty()) {
    throw_error(err, node->source_range);
  }
  visit(node->tuple_deconstruction.right);
}

void DependencyEmitter::visit_where(AST *node) {
  visit(node->where.target_type);
  visit(node->where.predicate);
}