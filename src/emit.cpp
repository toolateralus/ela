#include "emit.hpp"
#include "type.hpp"

static inline std::string type_to_string_with_extensions(const TypeExtensions &extensions, const std::string &base) {
  std::stringstream ss;
  ss << base;
  for (const auto ext : extensions.extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(ext.array_size) << "]";
    } else if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      ss << "*";
    }
  }
  return ss.str();
}
static inline std::string type_to_c_code(Type *type);
static inline std::string get_function_pointer_type_string(Type *type, Nullable<std::string> identifier,
                                                           bool type_erase_self) {
  if (!type->is_kind(TYPE_FUNCTION)) {
    throw_error(
        "internal compiler error: tried to get a function pointer from "
        "a non-function type",
        {});
  }

  std::stringstream ss;

  int pointer_depth = 0;
  TypeExtensions other_extensions;
  for (auto ext : type->extensions.extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT)
      pointer_depth++;
    else
      other_extensions.extensions.push_back(ext);
  }

  auto type_prefix = std::string(pointer_depth, '*');
  auto type_postfix = other_extensions.to_string();

  auto info = (type->info->as<FunctionTypeInfo>());
  auto return_type = info->return_type;

  ss << type_to_c_code(return_type) << "(" << type_prefix;

  if (identifier) {
    ss << *identifier.get();
  }

  ss << type_postfix;

  ss << ")(";

  for (size_t i = 0; i < info->params_len; ++i) {
    if (i == 0 && type_erase_self) {
      ss << "void*";
    } else {
      auto type = info->parameter_types[i];
      ss << type_to_c_code(type);
    }

    if (i != info->params_len - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}
static inline std::string type_to_c_code(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_DYN: {
      auto info = type->info->as<DynTypeInfo>();
      output = "dyn$" + type_to_c_code(info->trait_type);
      output = type_to_string_with_extensions(type->extensions, output);
    } break;
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type, nullptr, false);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TRAIT:
    case TYPE_CHOICE: {
      output = type_to_string_with_extensions(type->extensions, type->info->scope->full_name());
      break;
    }
    case TYPE_TUPLE: {
      auto info = (type->info->as<TupleTypeInfo>());
      output = "$tuple";
      for (size_t i = 0; i < info->types.size(); ++i) {
        output += std::to_string(info->types[i]->uid);
        if (i != info->types.size() - 1) {
          output += "$";
        }
      }
      output = type_to_string_with_extensions(type->extensions, output);
      break;
    }
  }
  return output;
}
void Emitter::emit_program(const THIRProgram *thir) {
  for (const auto &stmt : thir->statements) {
    emit_node(stmt);
  }
}
void Emitter::emit_bin_expr(const THIRBinExpr *thir) {
  throw_error("emit_bin_expr is unimplemented", thir->source_range);
}
void Emitter::emit_unary_expr(const THIRUnaryExpr *thir) {
  throw_error("emit_unary_expr is unimplemented", thir->source_range);
}
void Emitter::emit_literal(const THIRLiteral *thir) {
  // TODO: we gotta do more than this of course.
  code << thir->value.get_str();
}

void Emitter::emit_call(const THIRCall *thir) { throw_error("emit_call is unimplemented", thir->source_range); }
void Emitter::emit_member_access(const THIRMemberAccess *thir) {
  throw_error("emit_member_access is unimplemented", thir->source_range);
}
void Emitter::emit_cast(const THIRCast *thir) { throw_error("emit_cast is unimplemented", thir->source_range); }
void Emitter::emit_index(const THIRIndex *thir) { throw_error("emit_index is unimplemented", thir->source_range); }
void Emitter::emit_aggregate_initializer(const THIRAggregateInitializer *thir) {
  throw_error("emit_aggregate_initializer is unimplemented", thir->source_range);
}
void Emitter::emit_collection_initializer(const THIRCollectionInitializer *thir) {
  throw_error("emit_collection_initializer is unimplemented", thir->source_range);
}
void Emitter::emit_empty_initializer(const THIREmptyInitializer *thir) {
  throw_error("emit_empty_initializer is unimplemented", thir->source_range);
}
void Emitter::emit_size_of(const THIRSizeOf *thir) { throw_error("emit_size_of is unimplemented", thir->source_range); }

void Emitter::emit_for(const THIRFor *thir) { throw_error("emit_for is unimplemented", thir->source_range); }
void Emitter::emit_if(const THIRIf *thir) { throw_error("emit_if is unimplemented", thir->source_range); }
void Emitter::emit_while(const THIRWhile *thir) { throw_error("emit_while is unimplemented", thir->source_range); }
void Emitter::emit_switch(const THIRSwitch *thir) { throw_error("emit_switch is unimplemented", thir->source_range); }

void Emitter::emit_struct(const THIRStruct *thir) { throw_error("emit_struct is unimplemented", thir->source_range); }

void Emitter::emit_function(const THIRFunction *thir) {
  auto info = thir->type->info->as<FunctionTypeInfo>();
  code << type_to_c_code(info->return_type);
  code << ' ' << thir->name.get_str() << '(';
  for (size_t i = 0; i < info->params_len; ++i) {
    code << type_to_c_code(info->parameter_types[i]);
    if (i < info->params_len - 1) {
      code << ",";
    }
  }
  code << ')';
  emit_block(thir->block);
}

void Emitter::emit_block(const THIRBlock *thir) {
  code << "{\n";
  indent_level++;
  for (auto stmt : thir->statements) {
    emit_node(stmt);
  }
  indent_level--;
  code << "}";
}

void Emitter::emit_variable(const THIRVariable *thir) {
  indented(type_to_c_code(thir->type) + " " + thir->name.get_str());
  if (thir->value) {
    code << " = ";
    emit_node(thir->value);
  }
  code << ";\n";
}

void Emitter::emit_return(const THIRReturn *thir) {
  indented("return");
  if (thir->expression) {
    code << ' ';
    emit_node(thir->expression);
  }
  code << ";\n";
}
void Emitter::emit_break(const THIRBreak *) { indented_terminated("break"); }
void Emitter::emit_continue(const THIRContinue *) { indented_terminated("continue;\n"); }

void Emitter::emit_node(const THIR *thir) {
  switch (thir->get_node_type()) {
    case THIRNodeType::Program:
      return emit_program((const THIRProgram *)thir);
    case THIRNodeType::Block:
      return emit_block((const THIRBlock *)thir);
    case THIRNodeType::Variable:
      return emit_variable((const THIRVariable *)thir);
    case THIRNodeType::Function:
      return emit_function((const THIRFunction *)thir);
    case THIRNodeType::Struct:
      return emit_struct((const THIRStruct *)thir);
    case THIRNodeType::BinExpr:
      return emit_bin_expr((const THIRBinExpr *)thir);
    case THIRNodeType::UnaryExpr:
      return emit_unary_expr((const THIRUnaryExpr *)thir);
    case THIRNodeType::Literal:
      return emit_literal((const THIRLiteral *)thir);
    case THIRNodeType::Call:
      return emit_call((const THIRCall *)thir);
    case THIRNodeType::MemberAccess:
      return emit_member_access((const THIRMemberAccess *)thir);
    case THIRNodeType::Cast:
      return emit_cast((const THIRCast *)thir);
    case THIRNodeType::Index:
      return emit_index((const THIRIndex *)thir);
    case THIRNodeType::AggregateInitializer:
      return emit_aggregate_initializer((const THIRAggregateInitializer *)thir);
    case THIRNodeType::CollectionInitializer:
      return emit_collection_initializer((const THIRCollectionInitializer *)thir);
    case THIRNodeType::EmptyInitializer:
      return emit_empty_initializer((const THIREmptyInitializer *)thir);
    case THIRNodeType::Size_Of:
      return emit_size_of((const THIRSizeOf *)thir);
    case THIRNodeType::Return:
      return emit_return((const THIRReturn *)thir);
    case THIRNodeType::Break:
      return emit_break((const THIRBreak *)thir);
    case THIRNodeType::Continue:
      return emit_continue((const THIRContinue *)thir);
    case THIRNodeType::For:
      return emit_for((const THIRFor *)thir);
    case THIRNodeType::If:
      return emit_if((const THIRIf *)thir);
    case THIRNodeType::While:
      return emit_while((const THIRWhile *)thir);
    case THIRNodeType::Switch:
      return emit_switch((const THIRSwitch *)thir);
  }
}
