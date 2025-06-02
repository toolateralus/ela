#include "emit.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "thir.hpp"
#include "type.hpp"

// These macros help with formatting the C code correctly.
#define EXPR_BEGIN($node)    \
  if ($node->is_statement) { \
    indented();              \
  }
#define EXPR_TERMINATE($node) \
  if ($node->is_statement) {  \
    code << ";\n";            \
  }

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
static inline std::string c_type_string(Type *type);
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

  ss << c_type_string(return_type) << "(" << type_prefix;

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
      ss << c_type_string(type);
    }

    if (i != info->params_len - 1) {
      ss << ", ";
    }
  }
  ss << ")";
  return ss.str();
}

static inline std::string get_declaration_type_signature_and_identifier(const std::string &name, Type *type) {
  std::stringstream tss;

  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    return get_function_pointer_type_string(type, &identifier, false);
  }

  std::string base_type_str = c_type_string(type->base_type == Type::INVALID_TYPE ? type : type->base_type);
  std::string identifier = name;

  for (const auto &ext : type->extensions.extensions) {
    if (ext.type == TYPE_EXT_POINTER_MUT || ext.type == TYPE_EXT_POINTER_CONST) {
      base_type_str += "*";
    } else if (ext.type == TYPE_EXT_ARRAY) {
      identifier += "[" + std::to_string(ext.array_size) + "]";
    }
  }

  tss << base_type_str << " " << identifier;
  return tss.str();
}

static inline std::string c_type_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_DYN: {
      auto info = type->info->as<DynTypeInfo>();
      output = "dyn$" + c_type_string(info->trait_type);
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
  EXPR_BEGIN(thir);
  emit_expr(thir->left);
  code << ttype_get_operator_string(thir->op, thir->source_range);
  emit_expr(thir->right);
  EXPR_TERMINATE(thir);
}

void Emitter::emit_unary_expr(const THIRUnaryExpr *thir) {
  EXPR_BEGIN(thir);
  code << ttype_get_operator_string(thir->op, thir->source_range);
  emit_expr(thir->operand);
  EXPR_TERMINATE(thir);
}

void Emitter::emit_literal(const THIRLiteral *thir) {
  // TODO: we gotta do more than this of course.
  bool is_string = thir->type == global_find_type_id(u8_type(), {{{TYPE_EXT_POINTER_CONST}}});
  if (is_string) {
    code << '\"';
  }
  code << thir->value.get_str();
  if (is_string) {
    code << '\"';
  }
}

void Emitter::emit_member_access(const THIRMemberAccess *thir) {
  emit_expr(thir->base);
  code << '.' << thir->member.get_str();
}

void Emitter::emit_cast(const THIRCast *thir) { throw_error("emit_cast is unimplemented", thir->source_range); }

void Emitter::emit_index(const THIRIndex *thir) {
  emit_expr(thir->base);
  code << "[";
  emit_expr(thir->index);
  code << "]";
}

void Emitter::emit_aggregate_initializer(const THIRAggregateInitializer *thir) {
  throw_error("emit_aggregate_initializer is unimplemented", thir->source_range);
}
void Emitter::emit_collection_initializer(const THIRCollectionInitializer *thir) {
  throw_error("emit_collection_initializer is unimplemented", thir->source_range);
}

void Emitter::emit_empty_initializer(const THIREmptyInitializer *) { code << "{0}"; }
void Emitter::emit_size_of(const THIRSizeOf *thir) { throw_error("emit_size_of is unimplemented", thir->source_range); }

void Emitter::emit_for(const THIRFor *thir) { throw_error("emit_for is unimplemented", thir->source_range); }
void Emitter::emit_if(const THIRIf *thir) { throw_error("emit_if is unimplemented", thir->source_range); }
void Emitter::emit_while(const THIRWhile *thir) { throw_error("emit_while is unimplemented", thir->source_range); }
void Emitter::emit_switch(const THIRSwitch *thir) { throw_error("emit_switch is unimplemented", thir->source_range); }

void Emitter::emit_anonymous_struct(const THIRStruct *thir) {
  auto info = thir->type->info->as<StructTypeInfo>();
  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
    indented("union {\n");
  } else {
    indented("struct {\n");
  }
  emit_struct_body(thir);
  indented("};\n");
}

void Emitter::emit_struct_body(const THIRStruct *thir) {
  indent_level++;
  for (const auto &subtype : thir->subtypes) {
    emit_anonymous_struct(subtype);
  }
  for (const auto &field : thir->fields) {
    indented(get_declaration_type_signature_and_identifier(field->name.get_str(), field->type) + ";\n");
  }
  indent_level--;
}

void Emitter::emit_struct(const THIRStruct *thir) {
  auto info = thir->type->info->as<StructTypeInfo>();

  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
    code << "typedef union " << thir->name.get_str() << "{\n";
  } else {
    code << "typedef struct " << thir->name.get_str() << "{\n";
  }
  emit_struct_body(thir);
  code << "} " << thir->name.get_str() << ";\n";
}

void Emitter::emit_function(const THIRFunction *thir) {
  auto info = thir->type->info->as<FunctionTypeInfo>();

  if (thir->is_extern || thir->is_exported) {
    code << "extern ";
  }
  if (thir->is_inline) {
    code << "static inline ";
  }
  if (thir->is_test) {
    // TODO: emit test functions into a fixture again.
    throw_error("emit_function:test not yet implemented", thir->source_range);
  }
  if (thir->is_entry) {
    entry_point = thir;
  }

  if (thir->is_no_return) {
    throw_error("emit_function:no_return is not yet implemented", thir->source_range);
  }

  code << c_type_string(info->return_type);
  code << ' ' << thir->name.get_str() << '(';

  auto param_iter = thir->parameters.begin();

  for (size_t i = 0; i < info->params_len; ++i) {
    const auto parameter = *param_iter;
    code << c_type_string(info->parameter_types[i]) << ' ' << parameter.name.get_str();
    param_iter++;
    if (i < info->params_len - 1) {
      code << ",";
    }
  }

  if (thir->is_varargs) {
    code << ", ...";
  }

  code << ')';

  if (thir->block) {
    emit_block(thir->block);
  } else {
    code << ";\n";
  }
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
  indented(get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type));
  code << " = ";
  emit_node(thir->value);
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

// TODO: add default argument emission.
void Emitter::emit_call(const THIRCall *thir) {
  EXPR_BEGIN(thir);
  emit_expr(thir->callee);

  code << '(';
  for (const auto &arg : thir->arguments) {
    emit_expr(arg);
    if (arg != thir->arguments.back()) {
      code << ", ";
    }
  }
  code << ')';
  EXPR_TERMINATE(thir);
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
