#include <unordered_set>
#include "interned_string.hpp"
#include "thir_resolver.hpp"
#include "thir_emit.hpp"
#include "thir.hpp"
#include "type.hpp"

// formerly `declare_type`
void Resolver::declare_or_define_type(Type *type) {
  if (!type || forward_declared_types.contains(type) || emitted_types.contains(type)) {
    return;
  }

  // this is for function pointers.
  if (type->is_kind(TYPE_FUNCTION)) {
    emit_type_definition(type);
    return;
  }

  for (const auto &ext : type->extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      emitter.forward_declare_type(type);
      forward_declared_types.insert(type);
      return;
    }
  }
  
  emit_type_definition(type);
}

// formerly `define_type`
void Resolver::emit_type_definition(Type *type) {
  if (type->base_type != Type::INVALID_TYPE) {
    type = type->base_type;
  }
  if (!type || emitted_types.contains(type)) {
    return;
  }
  emitted_types.insert(type);

  THIRType thir_type;
  thir_type.type = type;

  for (const auto &member : type->info->members) {
    declare_or_define_type(member.type);
  }

  switch (type->kind) {
    case TYPE_FUNCTION: {
      const auto *info = type->info->as<FunctionTypeInfo>();
      declare_or_define_type(info->return_type);
      for (size_t i = 0; i < info->params_len; ++i) {
        declare_or_define_type(info->parameter_types[i]);
      }
    } break;
    case TYPE_STRUCT: {
      if (type->is_child_of_choice_type()) {
        return;
      }
    }
    case TYPE_TUPLE:
    case TYPE_CHOICE:
    case TYPE_ENUM: {
      emitter.emit_type(&thir_type);
    } break;
    case TYPE_DYN: {
      const auto *info = type->info->as<DynTypeInfo>();
      declare_or_define_type(info->trait_type);
      emitter.emit_dyn_dispatch_object_struct(type);
    } break;
      // No emission needed for these types
    case TYPE_TRAIT:
    case TYPE_SCALAR:
      break;
  }
}

void Resolver::visit_program(const THIRProgram *thir) {
  for (const auto &stmt : thir->statements) {
    if (stmt) {
      visit_node(stmt);
    }
  }
}
void Resolver::visit_bin_expr(const THIRBinExpr *thir) {
  visit_node(thir->left);
  visit_node(thir->right);
}
void Resolver::visit_unary_expr(const THIRUnaryExpr *thir) { visit_node(thir->operand); }
void Resolver::visit_call(const THIRCall *thir) {
  visit_node(thir->callee);
  for (const auto &arg : thir->arguments) {
    visit_node(arg);
  }
}
void Resolver::visit_member_access(const THIRMemberAccess *thir) { 
  visit_node(thir->base); 

  // If we're doing a member access on a pointer type, the type still has to be fully defined by this point,
  // but the declare_or_define_type will just see the pointer and decide to forward declare it.
  if (thir->base->type->is_pointer()) {
    declare_or_define_type(thir->base->type->base_type);
  }
}
void Resolver::visit_cast(const THIRCast *thir) { visit_node(thir->operand); }
void Resolver::visit_index(const THIRIndex *thir) {
  visit_node(thir->base);
  visit_node(thir->base);
}
void Resolver::visit_aggregate_initializer(const THIRAggregateInitializer *thir) {
  declare_or_define_type(thir->type);
  for (const auto &[_, value] : thir->key_values) {
    visit_node(value);
  }
}

void Resolver::visit_collection_initializer(const THIRCollectionInitializer *thir) {
  for (const auto &value : thir->values) {
    visit_node(value);
  }
}

void Resolver::visit_offset_of(const THIROffsetOf *thir) { declare_or_define_type(thir->target_type); }

void Resolver::visit_return(const THIRReturn *thir) {
  if (thir->expression) {
    visit_node(thir->expression);
  }
}
void Resolver::visit_empty_initializer(const THIREmptyInitializer *) {}
void Resolver::visit_break(const THIRBreak *) {}
void Resolver::visit_literal(const THIRLiteral *) {}
void Resolver::visit_type(const THIRType *) {}
void Resolver::visit_continue(const THIRContinue *) {}
void Resolver::visit_for(const THIRFor *thir) {
  visit_node(thir->initialization);
  visit_node(thir->increment);
  visit_node(thir->block);
}
void Resolver::visit_if(const THIRIf *thir) {
  visit_node(thir->condition);
  visit_node(thir->block);
  if (thir->_else) {
    visit_node(thir->_else);
  }
}
void Resolver::visit_while(const THIRWhile *thir) {
  if (thir->condition) {
    visit_node(thir->condition);
  }
  visit_node(thir->block);
}
void Resolver::visit_variable(const THIRVariable *thir) {
  static std::unordered_set<const THIRVariable *> visited;
  if (visited.contains(thir)) {
    return;
  } else {
    visited.insert(thir);
  }
  
  if (thir->value) { 
    visit_node(thir->value);
  }
  if (thir->is_global && !emitted_global_variables.contains(thir)) {
    emitter.emit_variable(thir);
    emitted_global_variables.insert(thir);
  }
}
void Resolver::visit_function(const THIRFunction *thir) {
  static std::set<InternedString> emitted {};

  // TODO: This entire system for preventing double emission is atrocious,
  // we're using like 90 maps instead of rectifying our THIR
  if (thir->is_extern && emitted.contains(thir->name)) {
    return;
  }

  if (thir->block || thir->is_extern) {
    emitted.insert(thir->name);
  }

  if (emitted_functions.contains(thir)) {
    return;
  }

  if (thir->block || thir->is_extern) {
    emitted_functions.insert(thir);
  }

  const auto type = thir->type->info->as<FunctionTypeInfo>();

  for (size_t i = 0; i < type->params_len; ++i) {
    emit_type_definition(type->parameter_types[i]);
  }

  for (const auto &param : thir->parameters) {
    if (param.default_value) {
      visit_node(param.default_value);
    }
  }

  if (!thir->is_extern) {
    // Emit a forward declaration for mutually recursive functions
    emitter.emit_function(thir, true);
  }

  if (thir->block) {
    visit_node(thir->block);
  }

  emitter.emit_function(thir);
}
void Resolver::visit_block(const THIRBlock *thir) {
  for (const auto &stmt : thir->statements) {
    visit_node(stmt);
  }
}
void Resolver::visit_node(const THIR *thir) {
  if (!thir) {
    throw_error("resolver got a null THIR node", {});
  }
  declare_or_define_type(thir->type);
  switch (thir->get_node_type()) {
    case THIRNodeType::ExpressionBlock: {
      visit_expr_block((const THIRExprBlock *)thir);
    }
    case THIRNodeType::Program:
      visit_program((const THIRProgram *)thir);
      break;
    case THIRNodeType::Block:
      visit_block((const THIRBlock *)thir);
      break;
    case THIRNodeType::Variable:
      visit_variable((const THIRVariable *)thir);
      break;
    case THIRNodeType::Function:
      visit_function((const THIRFunction *)thir);
      break;
    case THIRNodeType::Type:
      visit_type((const THIRType *)thir);
      break;
    case THIRNodeType::BinExpr:
      visit_bin_expr((const THIRBinExpr *)thir);
      break;
    case THIRNodeType::UnaryExpr:
      visit_unary_expr((const THIRUnaryExpr *)thir);
      break;
    case THIRNodeType::Literal:
      visit_literal((const THIRLiteral *)thir);
      break;
    case THIRNodeType::Call:
      visit_call((const THIRCall *)thir);
      break;
    case THIRNodeType::MemberAccess:
      visit_member_access((const THIRMemberAccess *)thir);
      break;
    case THIRNodeType::Cast:
      visit_cast((const THIRCast *)thir);
      break;
    case THIRNodeType::Index:
      visit_index((const THIRIndex *)thir);
      break;
    case THIRNodeType::AggregateInitializer:
      visit_aggregate_initializer((const THIRAggregateInitializer *)thir);
      break;
    case THIRNodeType::CollectionInitializer:
      visit_collection_initializer((const THIRCollectionInitializer *)thir);
      break;
    case THIRNodeType::EmptyInitializer:
      visit_empty_initializer((const THIREmptyInitializer *)thir);
      break;
    case THIRNodeType::Return:
      visit_return((const THIRReturn *)thir);
      break;
    case THIRNodeType::Break:
      visit_break((const THIRBreak *)thir);
      break;
    case THIRNodeType::Continue:
      visit_continue((const THIRContinue *)thir);
      break;
    case THIRNodeType::For:
      visit_for((const THIRFor *)thir);
      break;
    case THIRNodeType::If:
      visit_if((const THIRIf *)thir);
      break;
    case THIRNodeType::While:
      visit_while((const THIRWhile *)thir);
      break;
    case THIRNodeType::Offset_Of:
      visit_offset_of((const THIROffsetOf *)thir);
      break;
    case THIRNodeType::Noop:
      break;
  }
}
void Resolver::visit_expr_block(const THIRExprBlock *thir) {
  visit_node(thir->return_register);
  for (const auto &stmt : thir->statements) {
    visit_node(stmt);
  }
}
