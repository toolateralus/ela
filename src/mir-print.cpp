#include "error.hpp"
#include "mir.hpp"

const Type *get_base_type(const Type *t) {
  while (t->has_extensions()) {
    t = t->get_element_type();
  }
  return t;
}

std::string format_type_ref(const Type *t) {
  if (!t) {
    fprintf(stderr, "got a null type in format_type_ref\n");
    return "";
  }

  // If this type has pointer/array extensions, print a compact reference
  // that encodes the base type uid and the extension. Examples:
  //   pointer -> [*<base_uid>]
  //   pointer depth 2 -> [**<base_uid>]
  //   fixed array -> <[<base_uid>]; <size>>
  if (t->has_extensions()) {
    if (type_extensions_is_back_array(t->extensions)) {
      auto ext = t->extensions.back();
      const Type *base = get_base_type(t);
      return "<" + std::to_string(base->uid) + "; " + std::to_string(ext.array_size) + ">";
    }

    // pointer(s) at the back
    int depth = t->pointer_depth();
    if (depth > 0) {
      const Type *base = get_base_type(t);
      std::string stars(depth, '*');
      return "(" + stars + std::to_string(base->uid) + ")";
    }
  }
  return "(" + std::to_string(t->uid) + ")";
}

void collect_dependencies(const Type *t, std::unordered_set<const Type *> &visited, std::vector<const Type *> &ordered_types) {
  if (visited.count(t)) {
    return;
  }
  visited.insert(t);

  if (t->has_extensions() && t->kind != TYPE_FUNCTION &&
      (type_extensions_is_back_pointer(t->extensions) || type_extensions_is_back_array(t->extensions))) {
    const Type *base = get_base_type(t);
    collect_dependencies(base, visited, ordered_types);
    return;
  }
  switch (t->kind) {
    case TYPE_SCALAR:
      break;
    case TYPE_FUNCTION: {
      auto info = t->info->as<FunctionTypeInfo>();
      for (size_t i = 0; i < info->params_len; ++i) {
        collect_dependencies(info->parameter_types[i], visited, ordered_types);
      }
      if (info->return_type && info->return_type != void_type()) {
        collect_dependencies(info->return_type, visited, ordered_types);
      }
      break;
    }
    case TYPE_DYN: {
      auto info = t->info->as<DynTypeInfo>();
      for (const auto &method : info->methods) {
        collect_dependencies(method.second, visited, ordered_types);
      }
      break;
    }
    case TYPE_STRUCT: {
      for (const auto &member : t->info->members) {
        collect_dependencies(member.type, visited, ordered_types);
      }
      break;
    }
    case TYPE_ENUM: {
      auto info = t->info->as<EnumTypeInfo>();
      collect_dependencies(info->underlying_type, visited, ordered_types);
      break;
    }
    case TYPE_TUPLE: {
      auto info = t->info->as<TupleTypeInfo>();
      for (size_t i = 0; i < info->types.size(); ++i) {
        collect_dependencies(info->types[i], visited, ordered_types);
      }
      break;
    }
    case TYPE_CHOICE: {
      for (const auto &variant : t->info->members) {
        if (variant.type != void_type()) {
          collect_dependencies(variant.type, visited, ordered_types);
        }
      }
      break;
    }
    case TYPE_TRAIT:
      // Traits should not be emitted directly
      break;
  }
  ordered_types.push_back(t);
}

void print_type(FILE *f, const Type *t, int indent = 0) {
  auto print_indent = [f, indent]() {
    for (int i = 0; i < indent; ++i) fprintf(f, " ");
  };

  switch (t->kind) {
    case TYPE_SCALAR:
      print_indent();
      fprintf(f, "(%zu): %s\n", t->uid, t->to_string().c_str());
      break;
    case TYPE_FUNCTION: {
      auto info = t->info->as<FunctionTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): fn (", t->uid);
      for (size_t i = 0; i < info->params_len; ++i) {
        fprintf(f, "%s", format_type_ref(info->parameter_types[i]).c_str());
        if (i + 1 < info->params_len) fprintf(f, ", ");
      }
      fprintf(f, ")");
      if (info->return_type && info->return_type != void_type()) {
        fprintf(f, " -> %s", format_type_ref(info->return_type).c_str());
      }
      fprintf(f, "\n");
      break;
    }
    case TYPE_DYN: {
      auto info = t->info->as<DynTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): struct dyn %s {\n", t->uid, info->trait_type->basename.get_str().c_str());

      for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
      fprintf(f, "instance: %s\n", format_type_ref(void_type()->take_pointer_to()).c_str());

      for (const auto &method : info->methods) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s: %s\n", method.first.get_str().c_str(), format_type_ref(method.second).c_str());
      }

      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_STRUCT: {
      auto info = t->info->as<StructTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): %sstruct %s {\n", t->uid, info->is_union ? "union " : "", t->basename.get_str().c_str());
      for (const auto &member : t->info->members) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s: %s\n", member.name.get_str().c_str(), format_type_ref(member.type).c_str());
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_ENUM: {
      auto info = t->info->as<EnumTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): enum %s : %s {\n", t->uid, t->basename.get_str().c_str(),
              format_type_ref(info->underlying_type).c_str());
      for (const auto &member : t->info->members) {
        for (int i = 0; i < indent + 2; ++i) fprintf(f, " ");
        fprintf(f, "%s\n", member.name.get_str().c_str());
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_TUPLE: {
      auto info = t->info->as<TupleTypeInfo>();
      print_indent();
      fprintf(f, "(%zu): tuple (", t->uid);
      for (size_t i = 0; i < info->types.size(); ++i) {
        fprintf(f, "%s", format_type_ref(info->types[i]).c_str());
        if (i + 1 < info->types.size()) fprintf(f, ", ");
      }
      fprintf(f, ")\n");
      break;
    }
    case TYPE_CHOICE: {
      print_indent();
      fprintf(f, "(%zu): choice %s {\n", t->uid, t->basename.get_str().c_str());
      for (size_t i = 0; i < t->info->members.size(); ++i) {
        const auto &variant = t->info->members[i];
        for (int j = 0; j < indent + 2; ++j) fprintf(f, " ");
        fprintf(f, "%s: %zu", variant.name.get_str().c_str(), i);

        if (variant.type == void_type()) {
          fprintf(f, ",\n");
        } else if (variant.type->is_kind(TYPE_TUPLE)) {
          auto tuple_info = variant.type->info->as<TupleTypeInfo>();
          if (tuple_info->types.size() == 1) {
            fprintf(f, "(%s),\n", format_type_ref(tuple_info->types[0]).c_str());
          } else {
            fprintf(f, "(");
            for (size_t j = 0; j < tuple_info->types.size(); ++j) {
              fprintf(f, "%s", format_type_ref(tuple_info->types[j]).c_str());
              if (j + 1 < tuple_info->types.size()) fprintf(f, ", ");
            }
            fprintf(f, "),\n");
          }
        } else if (variant.type->is_kind(TYPE_STRUCT)) {
          fprintf(f, " {\n");
          for (const auto &member : variant.type->info->members) {
            for (int k = 0; k < indent + 4; ++k) fprintf(f, " ");
            fprintf(f, "%s: %s,\n", member.name.get_str().c_str(), format_type_ref(member.type).c_str());
          }
          for (int k = 0; k < indent + 2; ++k) fprintf(f, " ");
          fprintf(f, "},\n");
        } else {
          fprintf(f, ": %s,\n", format_type_ref(variant.type).c_str());
        }
      }
      print_indent();
      fprintf(f, "}\n");
      break;
    }
    case TYPE_TRAIT:
      throw_error("somehow we tried to emit a trait type", {});
      break;
  }
}

void Mir::Module::print(FILE *f) const {
  fprintf(f, "Types: {\n");

  // Collect all dependencies in proper order
  std::unordered_set<const Type *> visited;
  std::vector<const Type *> ordered_types;

  for (const auto *t : used_types) {
    collect_dependencies(t, visited, ordered_types);
  }

  // Print types in dependency order
  for (const auto *t : ordered_types) {
    print_type(f, t, 2);
  }

  fprintf(f, "}\n");

  for (const auto &fn : functions) {
    fn->print(f, (Module &)*this);
  }
}

void Mir::Instruction::print(FILE *f, Module &m) const {
  const char *opcode_name;

  switch (opcode) {
    case OP_ADD:
      opcode_name = "ADD";
      break;
    case OP_SUB:
      opcode_name = "SUB";
      break;
    case OP_MUL:
      opcode_name = "MUL";
      break;
    case OP_DIV:
      opcode_name = "DIV";
      break;
    case OP_MOD:
      opcode_name = "MOD";
      break;
    case OP_AND:
      opcode_name = "AND";
      break;
    case OP_OR:
      opcode_name = "OR";
      break;
    case OP_XOR:
      opcode_name = "XOR";
      break;
    case OP_SHL:
      opcode_name = "SHL";
      break;
    case OP_SHR:
      opcode_name = "SHR";
      break;
    case OP_EQ:
      opcode_name = "EQ";
      break;
    case OP_NE:
      opcode_name = "NE";
      break;
    case OP_LT:
      opcode_name = "LT";
      break;
    case OP_LE:
      opcode_name = "LE";
      break;
    case OP_GT:
      opcode_name = "GT";
      break;
    case OP_GE:
      opcode_name = "GE";
      break;
    case OP_LOGICAL_AND:
      opcode_name = "LOGICAL_AND";
      break;
    case OP_LOGICAL_OR:
      opcode_name = "LOGICAL_OR";
      break;
    case OP_NOT:
      opcode_name = "NOT";
      break;
    case OP_LOGICAL_NOT:
      opcode_name = "LOGICAL_NOT";
      break;
    case OP_NEG:
      opcode_name = "NEG";
      break;
    case OP_LOAD:
      opcode_name = "LOAD";
      break;
    case OP_STORE:
      opcode_name = "STORE";
      break;
    case OP_ALLOCA:
      opcode_name = "ALLOCA";
      break;
    case OP_GEP:
      opcode_name = "GEP";
      break;
    case OP_CAST:
      opcode_name = "CAST";
      break;
    case OP_LOAD_FN_PTR:
      opcode_name = "LOAD_FN_PTR";
      break;
    case OP_PUSH_ARG:
      opcode_name = "PUSH_ARG";
      break;
    case OP_CALL:
      opcode_name = "CALL";
      break;
    case OP_RET:
      opcode_name = "RET";
      break;
    case OP_RET_VOID:
      opcode_name = "RET_VOID";
      break;
    case OP_JMP:
      opcode_name = "JMP";
      break;
    case OP_JMP_TRUE:
      opcode_name = "JMP_TRUE";
      break;
    case OP_BITCAST:
      opcode_name = "BITCAST";
      break;
    case OP_CALL_PTR:
      opcode_name = "CALL_PTR";
      break;
  }

  // Build the instruction text into a string so we can compute its length and pad.
  std::string line;

  auto append_constant = [](const Constant &c) -> std::string {
    switch (c.tag) {
      case Constant::CONST_INVALID:
        return "invalid";
      case Constant::CONST_INT:
        return std::to_string(c.int_lit);
      case Constant::CONST_STRING:
        return std::string("\"") + c.string_lit.get_str() + "\"";
      case Constant::CONST_FLOAT:
        return std::to_string(c.float_lit);
      case Constant::CONST_BOOL:
        return c.bool_lit ? "true" : "false";
      case Constant::CONST_CHAR: {
        char buf[8] = {'\'', (char)c.char_lit, '\'', 0};
        return std::string(buf);
      }
    }
    return "";
  };

  auto print_operand_to_string = [&append_constant](const Operand &op, bool is_destination = false) -> std::string {
    switch (op.tag) {
      case Operand::OPERAND_NULL:
        return "null";
      case Operand::OPERAND_TEMP: {
        std::string s = "t" + std::to_string(op.temp);
        // we don't double print the type of temps because
        // it clutters the format and it's already known by the consumer
        // of the IR what type that local is (via its declaration)
        if (is_destination) {
          s += " ";
          s += format_type_ref(op.type);
          s += " = ";
        }
        return s;
      }
      case Operand::OPERAND_GLOBAL_VARIABLE_REFERENCE: {
        std::string s = "GV(";
        s += op.gv->name.get_str();
        s += ", ";
        s += format_type_ref(op.gv->type);
        s += ")";
        return s;
      }
      case Operand::OPERAND_IMMEDIATE_VALUE: {
        std::string s = "imm(";
        s += append_constant(op.imm);
        s += ", ";
        s += format_type_ref(op.type);
        s += ")";
        return s;
      }
      case Operand::OPERAND_BASIC_BLOCK:
        return std::string("bb(") + op.bb->label.get_str() + ")";
      case Operand::OPERAND_TYPE:
        return format_type_ref(op.type);
    }
    return "";
  };

  std::vector<const Operand *> ops;

  if (dest.tag != Operand::OPERAND_NULL) {
    line += print_operand_to_string(dest, true);
  }

  line += opcode_name;

  if (left.tag != Operand::OPERAND_NULL) {
    if (opcode == OP_CALL) {
      line += " " + m.functions[left.temp]->name.get_str();
    } else {
      ops.push_back(&left);
    }
  }

  if (right.tag != Operand::OPERAND_NULL) {
    ops.push_back(&right);
  }

  if (!ops.empty()) {
    line += " ";
    for (size_t i = 0; i < ops.size(); ++i) {
      if (i) line += ", ";
      line += print_operand_to_string(*ops[i]);
    }
  }

  if (!compile_command.has_flag("release")) {
    const int DEBUG_COMMENT_COLUMN = 80;
    int printed_len = 2 + (int)line.size();
    int pad = DEBUG_COMMENT_COLUMN - printed_len;
    if (pad < 1) {
      pad = 1;
    }

    fprintf(f, "  %s", line.c_str());

    for (int i = 0; i < pad; ++i) {
      fputc(' ', f);
    }

    fprintf(f, "; ");
    std::string source = get_source_line_from_span(span);
    fprintf(f, "\"%s\" :: '%s'\n", source.c_str(), span.ToString().c_str());

  } else {
    fprintf(f, "  %s\n", line.c_str());
  }
}

void Mir::Basic_Block::print(FILE *f, Module &m) const {
  fprintf(f, "%s:", label.get_str().c_str());
  fprintf(f, "\n");
  for (const auto &instruction : code) {
    instruction.print(f, m);
  }
}

void Mir::Function::print(FILE *f, Module &m) const {
  if (HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXPORTED) || HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXTERN)) {
    fprintf(f, "extern ");
  }
  fprintf(f, "fn %s(", name.get_str().c_str());
  for (size_t i = 0; i < type_info->params_len; ++i) {
    fprintf(f, "t%zu: %s", i, format_type_ref(type_info->parameter_types[i]).c_str());
    if (i + 1 < type_info->params_len) {
      fprintf(f, ", ");
    }
  }
  fprintf(f, ")");
  if (type_info->return_type && type_info->return_type != void_type()) {
    fprintf(f, " -> %s", format_type_ref(type_info->return_type).c_str());
  }

  if (HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXPORTED) || HAS_FLAG(flags, FUNCTION_FLAGS_IS_EXTERN)) {
    fprintf(f, " :: [flags: 0x%02x]", flags);
    fprintf(f, ";\n");
    return;
  } else {
    fprintf(f, " :: [flags: 0x%02x, stack: %zu bytes]", flags, stack_size_needed_in_bytes);
    fprintf(f, " {\n");
  }

  for (const auto &bb : basic_blocks) {
    bb->print(f, m);
  }

  fprintf(f, "}\n");
}

void Mir::Constant::print(FILE *f) const {
  switch (tag) {
    case CONST_INVALID:
      fprintf(f, "invalid");
      break;
    case CONST_INT:
      fprintf(f, "%zu", int_lit);
      break;
    case CONST_STRING:
      fprintf(f, "\"%s\"", string_lit.get_str().c_str());
      break;
    case CONST_FLOAT:
      fprintf(f, "%f", float_lit);
      break;
    case CONST_BOOL:
      fprintf(f, "%s", bool_lit ? "true" : "false");
      break;
    case CONST_CHAR:
      fprintf(f, "'%c'", (char)char_lit);
      break;
  }
}