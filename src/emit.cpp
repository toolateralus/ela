#include "emit.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
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
#define INDENTED_BLOCK() \
  indent_level++;        \
  Defer $indent_defer([&] { indent_level--; });

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

  tss << base_type_str << ' ' << identifier;
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
  code << '(';
  emit_expr(thir->left);
  code << ' ' << ttype_get_operator_string(thir->op, thir->source_range) << ' ';
  emit_expr(thir->right);
  code << ')';
  EXPR_TERMINATE(thir);
}

void Emitter::emit_unary_expr(const THIRUnaryExpr *thir) {
  EXPR_BEGIN(thir);
  code << ttype_get_operator_string(thir->op, thir->source_range);
  code << '(';
  emit_expr(thir->operand);
  code << ')';
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

  if (thir->base->type->extensions.is_pointer()) {
    code << "->";
  } else {
    code << '.';
  }

  code << thir->member.get_str();
}

void Emitter::emit_cast(const THIRCast *thir) {
  code << '(' << c_type_string(thir->type) << ')';
  emit_expr(thir->operand);
}
void Emitter::emit_size_of(const THIRSizeOf *thir) {
  code << "sizeof(";
  c_type_string(thir->target);
  code << ')';
}

void Emitter::emit_index(const THIRIndex *thir) {
  emit_expr(thir->base);
  code << "[";
  emit_expr(thir->index);
  code << "]";
}

void Emitter::emit_aggregate_initializer(const THIRAggregateInitializer *thir) {
  code << "(" << c_type_string(thir->type) << "){ ";
  for (const auto &[key, value] : thir->key_values) {
    code << '.' << key.get_str() << " = ";
    emit_expr(value);
    code << ", ";
  }
  code << '}';
}

void Emitter::emit_collection_initializer(const THIRCollectionInitializer *thir) {
  code << "(" << c_type_string(thir->type) << "){";
  for (const auto &value : thir->values) {
    emit_expr(value);
    code << ", ";
  }
  code << "}";
}

void Emitter::emit_empty_initializer(const THIREmptyInitializer *thir) {
  code << "(" << c_type_string(thir->type) << "){0}";
}

void Emitter::emit_for(const THIRFor *thir) { throw_error("emit_for is unimplemented", thir->source_range); }

void Emitter::emit_if(const THIRIf *thir) {
  code << "if (";
  emit_expr(thir->condition);
  code << ")";
  emit_node(thir->block);

  if (thir->_else) {
    code << "else ";
    emit_node(thir->_else);
  }
}

void Emitter::emit_while(const THIRWhile *thir) {
  code << "while (";
  if (thir->condition) {
    emit_expr(thir->condition);
  } else {
    code << '1';
  }
  code << ')';
  emit_block(thir->block);
}
void Emitter::emit_switch(const THIRSwitch *thir) { throw_error("emit_switch is unimplemented", thir->source_range); }

void Emitter::emit_tuple(Type *type) {
  const auto type_name = c_type_string(type);
  code << "typedef struct";
  emit_struct_body(type);
  code << ' ' << type_name << ";\n";
}

void Emitter::emit_struct(Type *type) {
  StructTypeInfo *info = type->info->as<StructTypeInfo>();
  const auto type_name = type->basename.get_str();
  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
    code << "typedef union " << type_name;
  } else {
    code << "typedef struct " << type_name;
  }
  emit_struct_body(type);
  code << ' ' << type_name << ";\n";
}

void Emitter::emit_anonymous_struct(Type *type) {
  StructTypeInfo *info = type->info->as<StructTypeInfo>();
  if (HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION)) {
    code << "union";
  } else {
    code << "struct";
  }
  emit_struct_body(type);
}

void Emitter::emit_struct_body(Type *type) {
  indent_level++;
  code << " {\n";
  for (const auto &member : type->info->members) {
    indented();
    if (member.type->basename.str_ptr->starts_with("__anon_D")) {
      emit_anonymous_struct(member.type);
      code << ";\n";
    } else {
      code << get_declaration_type_signature_and_identifier(member.name.get_str(), member.type) << ";\n";
    }
  }
  indent_level--;
  indented();
  code << "}";
}

void Emitter::emit_choice(Type *type) {
  ChoiceTypeInfo *info = type->info->as<ChoiceTypeInfo>();
  const auto choice_type_name = type->basename.get_str();

  for (auto &variant : info->members) {
    if (variant.type->kind == TYPE_STRUCT) {
      const auto variant_name = variant.name.get_str();
      const auto subtype_name = choice_type_name + "$" + variant_name;
      variant.type->basename = subtype_name;
      emit_struct(variant.type);
    }
  }

  // Emit the main choice struct
  code << "typedef struct " << choice_type_name << " {\n";
  indent_level++;
  indentedf("%s %s;\n", "int", DISCRIMINANT_KEY);  // Discriminant, TODO: optimize for different discriminant sizes
  indented("union {\n");
  indent_level++;
  for (const auto &variant : info->members) {
    if (variant.type->kind == TYPE_STRUCT) {
      const auto variant_name = variant.name.get_str();
      const auto subtype_name = choice_type_name + "$" + variant_name;
      indented(subtype_name + ' ' + variant_name + ";\n");
    } else if (variant.type->kind == TYPE_TUPLE) {
      const auto variant_name = variant.name.get_str();
      indented(c_type_string(variant.type) + ' ' + variant_name + ";\n");
    }
  }
  indent_level--;
  indented("};\n");
  indent_level--;
  code << "} " << choice_type_name << ";\n";
}

void Emitter::emit_enum(Type *type) {
  const EnumTypeInfo *info = type->info->as<EnumTypeInfo>();

  code << "typedef enum: " << c_type_string(info->underlying_type) << " {\n";

  for (const auto &[name, _, __, thir_value] : info->members) {
    indented();
    code << type->basename.get_str() + '$' + name.get_str() << " = ";
    emit_expr(thir_value.get());
    code << ",\n";
  }

  code << "} " << type->basename.get_str() << ";\n";
}

void Emitter::forward_declare_type(const Type *) {}
void Emitter::emit_dyn_dispatch_object_struct(const Type *) {}

void Emitter::emit_type(const THIRType *thir) {
  switch (thir->type->kind) {
    case TYPE_SCALAR:
    case TYPE_FUNCTION:
    case TYPE_TRAIT:
      return;
    case TYPE_DYN:
      return emit_dyn_dispatch_object_struct(thir->type);
    case TYPE_TUPLE:
      return emit_tuple(thir->type);
    case TYPE_STRUCT:
      return emit_struct(thir->type);
    case TYPE_ENUM:
      return emit_enum(thir->type);
    case TYPE_CHOICE:
      return emit_choice(thir->type);
  }
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
  INDENTED_BLOCK();
  for (auto stmt : thir->statements) {
    emit_node(stmt);
  }
  code << "}";
}

void Emitter::emit_variable(const THIRVariable *thir) {
  indented(get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type));
  code << " = ";
  emit_expr(thir->value);
  code << ";\n";
}

void Emitter::emit_return(const THIRReturn *thir) {
  indented("return");
  if (thir->expression) {
    code << ' ';
    emit_expr(thir->expression);
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
    case THIRNodeType::Type:
      return emit_type((const THIRType *)thir);
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
