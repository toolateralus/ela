#include "ast.hpp"
#include "builder.hpp"
#include "emit.hpp"
#include "core.hpp"
#include "lex.hpp"
#include "scope.hpp"
#include "strings.hpp"
#include "thir.hpp"
#include "type.hpp"

// These macros help with formatting the C code correctly.
#define EXPR_BEGIN($node)       \
  if ($node->is_statement) {    \
    emit_line_directive($node); \
    indented();                 \
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
  for (const auto ext : extensions) {
    if (ext.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(ext.array_size) << "]";
    } else if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT) {
      ss << "*";
    }
  }
  return ss.str();
}
static inline std::string c_type_string(const Type *type);
static inline std::string get_function_pointer_type_string(const Type *type, Nullable<std::string> identifier,
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
  for (auto ext : type->extensions) {
    if (ext.type == TYPE_EXT_POINTER_CONST || ext.type == TYPE_EXT_POINTER_MUT)
      pointer_depth++;
    else
      other_extensions.push_back(ext);
  }

  auto type_prefix = std::string(pointer_depth, '*');
  auto type_postfix = extensions_to_string(other_extensions);

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

static inline std::string get_declaration_type_signature_and_identifier(const std::string &name, const Type *type) {
  std::stringstream tss;

  if (type->is_kind(TYPE_FUNCTION)) {
    std::string identifier = name;
    return get_function_pointer_type_string(type, &identifier, false);
  }

  std::string base_type_str = c_type_string(type->base_type == Type::INVALID_TYPE ? type : type->base_type);
  std::string identifier = name;

  for (const auto &ext : type->extensions) {
    if (ext.type == TYPE_EXT_POINTER_MUT || ext.type == TYPE_EXT_POINTER_CONST) {
      base_type_str += "*";
    } else if (ext.type == TYPE_EXT_ARRAY) {
      identifier += "[" + std::to_string(ext.array_size) + "]";
    }
  }

  tss << base_type_str << ' ' << identifier;
  return tss.str();
}

static inline std::string c_type_string(const Type *type) {
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
  code << ' ' << ttype_get_operator_string(thir->op, thir->span) << ' ';
  emit_expr(thir->right);
  code << ')';
  EXPR_TERMINATE(thir);
}

void Emitter::emit_unary_expr(const THIRUnaryExpr *thir) {
  EXPR_BEGIN(thir);
  if (thir->op == TType::And && thir->operand->is_temporary_value()) {
    // TODO: copy the pattern from main's builder.hpp to use insert at cursor.
    // this is not thread safe and is really bad.
    code << "({";
    code << " static " << c_type_string(thir->operand->type) << " temp = {};";
    code << " temp = ";
    emit_expr(thir->operand);
    code << "; &temp; })";
  } else if (thir->op == TType::Increment ||
             thir->op == TType::Decrement) {  // ++ and -- are ALWAYS posfix in this language.
    code << '(';
    emit_expr(thir->operand);
    code << ttype_get_operator_string(thir->op, thir->span);
    code << ")";
  } else {
    code << '(';
    code << ttype_get_operator_string(thir->op, thir->span);
    code << '(';
    emit_expr(thir->operand);
    code << "))";
  }
  EXPR_TERMINATE(thir);
}

void Emitter::emit_literal(const THIRLiteral *thir) {
  if (thir->tag == ASTLiteral::Null) {
    code << "nullptr";
    return;
  }

  const bool is_string = thir->type == u8_ptr_type() || thir->is_c_string;

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
  if (thir->base->type->is_pointer()) {
    code << "->";
  } else {
    code << '.';
  }
  code << thir->member.get_str();
}

void Emitter::emit_cast(const THIRCast *thir) {
  code << '(';
  code << '(' << c_type_string(thir->type) << ')';
  emit_expr(thir->operand);
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
  if (thir->is_variable_length_array) {
    code << "(" << c_type_string(thir->type) << "){";
  } else {
    code << '{';
  }
  for (const auto &value : thir->values) {
    emit_expr(value);
    code << ", ";
  }
  code << "}";
}

void Emitter::emit_empty_initializer(const THIREmptyInitializer *thir) {
  if (thir->type->is_kind(TYPE_ENUM)) {
    code << "0";
  } else if (thir->type->is_kind(TYPE_CHOICE) && thir->type->has_no_extensions()) {
    code << "(" << c_type_string(thir->type) << "){ . " << std::string{OPTION_DISCRIMINANT_KEY} << " = 0 }";
  } else if (thir->type->is_fixed_sized_array()) {
    code << "{}";
  } else {
    code << "(" << c_type_string(thir->type) << "){0}";
  }
}

void Emitter::emit_for(const THIRFor *thir) {
  indented("for (");
  emit_node(thir->initialization);
  emit_expr(thir->condition);
  code << "; ";
  emit_expr(thir->increment);
  code << ") ";
  emit_node(thir->block);
}

void Emitter::emit_if(const THIRIf *thir) {
  indented("if (");
  emit_expr(thir->condition);
  code << ")";
  emit_node(thir->block);

  if (thir->_else) {
    indented("else ");
    emit_node(thir->_else);
  }
}

void Emitter::emit_while(const THIRWhile *thir) {
  indented("while (");
  if (thir->condition) {
    emit_expr(thir->condition);
  } else {
    code << '1';
  }
  code << ')';
  emit_block(thir->block);
}

void Emitter::emit_type(Type *type) {
  // TODO: stop doing this hack;
  if (type->basename == "va_list") {
    return;
  }

  switch (type->kind) {
    case TYPE_SCALAR:
    case TYPE_FUNCTION:
    case TYPE_TRAIT:
      return;
    case TYPE_DYN:
      return emit_dyn_dispatch_object_struct(type);
    case TYPE_TUPLE:
      return emit_tuple(type);
    case TYPE_STRUCT:
      return emit_struct(type);
    case TYPE_ENUM:
      return emit_enum(type);
    case TYPE_CHOICE:
      return emit_choice(type);
  }
}

void Emitter::emit_tuple(Type *type) {
  const auto type_name = c_type_string(type);
  code << "typedef struct " << type_name;
  emit_struct_body(type);
  code << ' ' << type_name << ";\n";
}

void Emitter::emit_struct(Type *type) {
  StructTypeInfo *info = type->info->as<StructTypeInfo>();
  const auto type_name = c_type_string(type);
  if (info->is_union) {
    code << "typedef union " << type_name;
  } else {
    code << "typedef struct " << type_name;
  }
  emit_struct_body(type);
  code << ' ' << type_name << ";\n";
}

void Emitter::emit_anonymous_struct(Type *type) {
  StructTypeInfo *info = type->info->as<StructTypeInfo>();
  if (info->is_union) {
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
  const auto choice_type_name = c_type_string(type);

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
  indentedf("%s %s;\n", "int", OPTION_DISCRIMINANT_KEY);  // Discriminant, TODO: optimize for different discriminant sizes
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
    } else if (variant.type == void_type()) {
      // We emit empty structs here because it has no impact on type
      // size and gives us a predictable memory layout, as well as
      // making sense in runtime reflection.
      const auto variant_name = variant.name.get_str();
      indented("struct {}" + variant_name + ";\n");
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

  code << "} " << c_type_string(type) << ";\n";
}

void Emitter::forward_declare_type(const Type *type) {
  // TODO: remove this
  if (type->basename == "va_list") {
    return;
  }

  if (type_is_valid(type->base_type)) {
    type = type->base_type;
  }
  switch (type->kind) {
    case TYPE_FUNCTION: {
      auto info = type->info->as<FunctionTypeInfo>();
      for (size_t i = 0; i < info->params_len; i++) {
        forward_declare_type(info->parameter_types[i]);
      }
      forward_declare_type(info->return_type);
    } break;
    case TYPE_STRUCT: {
      const auto info = type->info->as<StructTypeInfo>();
      const auto name = c_type_string(type);
      if (info->is_union) {
        code << "typedef union " << name << ' ' << name << ";\n";
      } else {
        code << "typedef struct " << name << ' ' << name << ";\n";
      }
    } break;
    case TYPE_TUPLE: {
      auto name = c_type_string(type);
      code << "typedef struct " << name << ' ' << name << ";\n";
    } break;
    case TYPE_DYN:
    case TYPE_CHOICE: {
      auto name = c_type_string(type);
      code << "typedef struct " << name << ' ' << name << ";\n";
    } break;
    default:
      break;
  }
}

void Emitter::emit_dyn_dispatch_object_struct(Type *type) {
  if (type->dyn_emitted) {
    return;
  }

  type->dyn_emitted = true;

  auto info = type->info->as<DynTypeInfo>();
  auto &methods = info->methods;

  auto name = c_type_string(type);
  code << "typedef struct " << name << " {\n";
  indented("void *instance;\n");
  for (const auto &[method_name, method_type] : methods) {
    std::string method_pointer_name = method_name.get_str();
    indented(get_function_pointer_type_string(method_type, &method_pointer_name, false) + ";\n");
  }
  code << "} " << name << ";\n";
}

void Emitter::emit_function(const THIRFunction *thir, bool forward_declaration) {
  auto info = thir->type->info->as<FunctionTypeInfo>();

  if ((emitting_global_initializer && thir->name == "ela_run_global_initializers") || thir->constructor_index == 2) {
    code << "__attribute__((constructor))\n";
  }

  if (thir->is_extern || thir->is_exported) {
    code << "extern ";
  }

  if (thir->is_inline) {
    code << "static inline ";
  }

  if (thir->is_entry) {
    entry_point = thir;
  }

  if (info->return_type->kind == TYPE_FUNCTION) {
    std::string outer_params_decl;
    auto param_iter = thir->parameters.begin();
    for (size_t i = 0; i < info->params_len; ++i) {
      const auto parameter = *param_iter;
      if (i) outer_params_decl += ", ";
      outer_params_decl +=
          get_declaration_type_signature_and_identifier(parameter.name.get_str(), info->parameter_types[i]);
      param_iter++;
    }
    if (thir->is_varargs) {
      if (!outer_params_decl.empty()) outer_params_decl += ", ";
      outer_params_decl += "...";
    }

    std::string ident = std::string("*") + thir->name.get_str() + "(" + outer_params_decl + ")";
    code << get_function_pointer_type_string(info->return_type, &ident, false);
    if (thir->block && !forward_declaration) {
      emit_block(thir->block);
    } else {
      code << ";\n";
    }
    return;
  }

  code << c_type_string(info->return_type);
  code << ' ' << thir->name.get_str() << "(";

  auto param_iter = thir->parameters.begin();

  for (size_t i = 0; i < info->params_len; ++i) {
    const auto parameter = *param_iter;
    code << get_declaration_type_signature_and_identifier(parameter.name.get_str(), info->parameter_types[i]);
    param_iter++;
    if (i < info->params_len - 1) {
      code << ", ";
    }
  }

  if (thir->is_varargs) {
    code << ", ...";
  }

  code << ") ";

  if (thir->block && !forward_declaration) {
    emit_block(thir->block);
  } else {
    code << ";\n";
  }
}

void Emitter::emit_expression_block(const THIRExprBlock *thir) {
  code << "({\n";
  EXPR_BEGIN(thir);
  indent_level++;
  for (auto stmt : thir->statements) {
    emit_node(stmt);
  }
  indented();
  emit_expr(thir->return_register);
  code << ";\n";
  indent_level--;
  code << "})";
  EXPR_TERMINATE(thir);
}

void Emitter::emit_block(const THIRBlock *thir) {
  indented("{\n");
  indent_level++;
  for (auto stmt : thir->statements) {
    // TODO: we shouldn't have to filter this.
    if (stmt->get_node_type() != THIRNodeType::Function && stmt->get_node_type() != THIRNodeType::Type) {
      emit_node(stmt);
    }
  }
  indent_level--;
  indented("}\n");
}

void Emitter::emit_expr(const THIR *thir) {
  if (thir->get_node_type() == THIRNodeType::Function) {
    auto function = static_cast<const THIRFunction *>(thir);
    code << function->name.get_str();
  } else if (thir->get_node_type() == THIRNodeType::Variable) {
    auto variable = static_cast<const THIRVariable *>(thir);
    code << variable->name.get_str();
  } else {
    emit_node(thir);
  }
}

void Emitter::emit_variable(const THIRVariable *thir) {
  if (thir->is_extern) {
    indented("extern " + get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type) + ";\n");
    return;
  } else if (thir->is_constexpr) {
    indented("const " + get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type));
  } else if (thir->is_static) {
    indented("static " + get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type));
  } else {
    indented(get_declaration_type_signature_and_identifier(thir->name.get_str(), thir->type));
  }

  if (!thir->value) {
    code << ";\n";
  } else {
    code << " = ";
    // use the value mutated by interpreter
    if (thir->compile_time_value && thir->use_compile_time_value_at_emit_time) {
      emit_expr(thir->compile_time_value->to_thir());
    } else {
      emit_expr(thir->value);
    }
    code << ";\n";
  }
}

void Emitter::emit_return(const THIRReturn *thir) {
  indented("return");
  if (thir->expression) {
    code << ' ';
    emit_expr(thir->expression);
  }
  code << ";\n";
}

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
  if (!thir) {
    // Defers and other nodes are expectedly null. We should probably just use a THIRNoOp, this allows bugs to creep in
    return;
  }
  emit_line_directive(thir);
  switch (thir->get_node_type()) {
    case THIRNodeType::ExpressionBlock:
      return emit_expression_block((const THIRExprBlock *)thir);
    case THIRNodeType::Program:
      return emit_program((const THIRProgram *)thir);
    case THIRNodeType::Block:
      return emit_block((const THIRBlock *)thir);
    case THIRNodeType::Variable:
      return emit_variable((const THIRVariable *)thir);
    case THIRNodeType::Function:
      return emit_function((const THIRFunction *)thir);
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
    case THIRNodeType::Noop:
      break;
    case THIRNodeType::Type:  // We ignore types here.
      return;
  }
}

std::string Emitter::reflection_prelude(const std::unordered_map<const Type *, ReflectionInfo> &info) {
  StringBuilder code;
  code << "typedef struct Type Type;\n";
  for (const auto &[_, info] : info) {
    code << "extern Type " << info.definition->name.get_str() << ";\n";
  }
  return code.str();
}