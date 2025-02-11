

bool Emitter::should_emit_function(Emitter *visitor, AST *node, bool test_flag) {
  // if we're not testing, don't emit for test functions
  if (!test_flag && node->flags & FUNCTION_IS_TEST) {
    return false;
  }
  // generate a test based on this function pointer.
  if (test_flag && node->flags & FUNCTION_IS_TEST) {
    visitor->test_functions << "(__COMPILER_GENERATED_TEST){.name = \"" << node->name.get_str() << "\", .function = &"
                            << node->name.get_str() << "},";
    visitor->num_tests++;
  }
  // dont emit a main if we're in test mode.
  if (test_flag && node->name == "main") {
    return false;
  }
  return true;
}

std::string Emitter::to_cpp_string(const Type_Metadata &extensions, const std::string &base) {
  std::stringstream ss;
  ss << base;
  for (const auto meta : extensions.extensions) {
    if (meta.type == TYPE_EXT_ARRAY) {
      ss << "[" << std::to_string(meta.array_size) << "]";
    } else if (meta.type == TYPE_EXT_POINTER) {
      ss << "*";
    }
  }
  return ss.str();
}

std::string Emitter::get_cpp_scalar_type(int id) {
  auto type = global_get_type(id);
  std::string name = "";

  return to_cpp_string(type);

  if (type->meta.has_no_extensions()) {
    return name;
  }

  return to_cpp_string(type->meta, name);
}

std::string Emitter::to_cpp_string(Type *type) {
  auto output = std::string{};
  switch (type->kind) {
    case TYPE_FUNCTION:
      return get_function_pointer_type_string(type);
    case TYPE_SCALAR:
    case TYPE_ENUM:
    case TYPE_STRUCT: {
      output = to_cpp_string(type->meta, type->base.get_str());
      break;
    }
    case TYPE_TUPLE: {
      auto info = (type->info->as<Tuple_Info>());
      output = "$tuple";
      for (int i = 0; i < info->types.size(); ++i) {
        output += std::to_string(info->types[i]);
        if (i != info->types.size() - 1) {
          output += "$";
        }
      }
      output = to_cpp_string(type->meta, output);
      break;
    }
    case TYPE_INTERFACE:
      throw_error("can't declare an instance of an interface", {});
      break;
  }
  return output;
}

void Emitter::call_operator_overload(const Source_Range &range, Type *left_ty, OperationKind operation, Token_Type op,
                                     ASTExpr *left, ASTExpr *right) {
  auto call = ASTCall{};
  auto dot = ASTDotExpr{};
  dot.base = left;
  dot.member_name = get_operator_overload_name(op, operation);
  call.function = &dot;
  auto args = ASTArguments{};
  if (right) {
    args.arguments = {right};
  }
  call.arguments = &args;
  dot.source_range = range;
  call.arguments->source_range = range;
  call.source_range = range;
  call.accept(&typer);
  call.accept(this);
}

Emitter::Emitter(Context &context, Typer &type_visitor) : typer(type_visitor), ctx(context) { ss = &code; }