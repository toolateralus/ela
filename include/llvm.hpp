#pragma once
#include "ast.hpp"
#include "constexpr.hpp"
#include "core.hpp"
#include "type.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DIBuilder.h"
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <map>
#include <memory>
#include <stack>

using llvm::DataLayout;
using llvm::DIBuilder;
using llvm::DIScope;
using llvm::IRBuilder;
using llvm::LLVMContext;

static inline std::string unescape_string_lit(const std::string &s) {
  std::string res;
  std::istringstream iss(s);
  char c;

  while (iss.get(c)) {
    if (c == '\\') {
      char next = iss.peek();
      if (iss.eof()) {
        res += '\\'; // Handle dangling backslash
        break;
      }

      switch (next) {
        case 'n':
          res += '\n';
          iss.get();
          break;
        case 't':
          res += '\t';
          iss.get();
          break;
        case 'r':
          res += '\r';
          iss.get();
          break;
        case 'f':
          res += '\f';
          iss.get();
          break;
        case 'v':
          res += '\v';
          iss.get();
          break;
        case 'a':
          res += '\a';
          iss.get();
          break;
        case 'b':
          res += '\b';
          iss.get();
          break;
        case '\\':
          res += '\\';
          iss.get();
          break;
        case '\'':
          res += '\'';
          iss.get();
          break;
        case '\"':
          res += '\"';
          iss.get();
          break;
        case '\?':
          res += '\?';
          iss.get();
          break;
        case 'e':
          res += '\x1B'; // Escape character
          iss.get();
          break;
        case 'x': {  // Hexadecimal escape sequence
          iss.get(); // Consume 'x'
          std::string hex_digits;
          for (int i = 0; i < 2 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (!hex_digits.empty()) {
            res += static_cast<char>(std::stoi(hex_digits, nullptr, 16));
          } else {
            res += "\\x"; // Invalid \x escape
          }
          break;
        }
        case 'u': {  // Unicode escape sequence (\uXXXX)
          iss.get(); // Consume 'u'
          std::string hex_digits;
          for (int i = 0; i < 4 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (hex_digits.size() == 4) {
            int codepoint = std::stoi(hex_digits, nullptr, 16);
            res += static_cast<char>(codepoint); // Simplified for ASCII range
          } else {
            res += "\\u" + hex_digits; // Invalid \u escape
          }
          break;
        }
        case 'U': {  // Unicode escape sequence (\UXXXXXXXX)
          iss.get(); // Consume 'U'
          std::string hex_digits;
          for (int i = 0; i < 8 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (hex_digits.size() == 8) {
            int codepoint = std::stoi(hex_digits, nullptr, 16);
            res += static_cast<char>(codepoint); // Simplified for ASCII range
          } else {
            res += "\\U" + hex_digits; // Invalid \U escape
          }
          break;
        }
        default:
          if (next >= '0' && next <= '7') { // Octal escape sequence
            std::string oct_digits;
            for (int i = 0; i < 3 && std::isdigit(iss.peek()) && iss.peek() >= '0' && iss.peek() <= '7'; ++i) {
              oct_digits += iss.get();
            }
            res += static_cast<char>(std::stoi(oct_digits, nullptr, 8));
          } else {
            // Unrecognized escape sequence, treat as literal
            res += '\\';
            res += next;
            iss.get();
          }
          break;
      }
    } else {
      res += c; // Non-escaped characters are added as-is
    }
  }

  return res;
}

struct DIManager {
  std::shared_ptr<DIBuilder> di_builder;
  std::stack<DIScope *> scope_stack;

  explicit DIManager(std::shared_ptr<DIBuilder> &builder) : di_builder(builder) {}

  struct ScopeDropStub {
    DIManager *manager;
    ScopeDropStub() = delete;
    explicit ScopeDropStub(DIManager *manager) : manager(manager) {}
    ScopeDropStub(const ScopeDropStub &) = delete;
    ScopeDropStub &operator=(const ScopeDropStub &) = delete;
    ScopeDropStub(ScopeDropStub &&other) noexcept : manager(other.manager) { other.manager = nullptr; }

    ScopeDropStub &operator=(ScopeDropStub &&other) noexcept {
      if (this != &other) {
        manager = other.manager;
        other.manager = nullptr;
      }
      return *this;
    }
    ~ScopeDropStub() { manager->pop_scope(); }
  };

  void push_scope(DIScope *scope) { scope_stack.push(scope); }

  void pop_scope() {
    if (!scope_stack.empty()) {
      scope_stack.pop();
    }
  }

  DIScope *current_scope() const { return scope_stack.empty() ? nullptr : scope_stack.top(); }

  std::tuple<std::string, std::string, unsigned, unsigned> extract_source_range(SourceRange range) {
    auto location = range.begin_location;
    auto line = location.line, column = location.column;
    std::filesystem::path abs_path = location.files()[location.file];

    auto basename = abs_path.filename().string();
    auto dirpath = abs_path.parent_path().string();
    return {basename, dirpath, line, column};
  }

  llvm::DIFile *enter_file_scope(const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *file = di_builder->createFile(basename, dirpath);
    push_scope(file);
    return file;
  }

  llvm::DISubprogram *enter_function_scope(DIScope *parent, llvm::Function *function, const std::string &name,
                                           const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *file = dyn_cast<llvm::DIFile>(parent);
    auto *func_type = di_builder->createSubroutineType(di_builder->getOrCreateTypeArray({}));
    auto *subprogram = di_builder->createFunction(file, name, name, file, line, func_type, line, llvm::DINode::FlagZero,
                                                  llvm::DISubprogram::SPFlagDefinition);
    function->setSubprogram(subprogram);
    push_scope(subprogram);
    return subprogram;
  }

  llvm::DILexicalBlock *enter_lexical_scope(DIScope *parent, const SourceRange &range) {
    auto [basename, dirpath, line, column] = extract_source_range(range);
    auto *block = di_builder->createLexicalBlock(parent, di_builder->createFile(basename, dirpath), line, column);
    push_scope(block);
    return block;
  }

  llvm::DIVariable *create_variable(DIScope *scope, const std::string &name, llvm::DIFile *file, unsigned line,
                                    llvm::DIType *type) {
    return di_builder->createAutoVariable(scope, name, file, line, type, true);
  }

  void attach_debug_info(llvm::Instruction *instruction, llvm::DIVariable *variable, unsigned line, unsigned column) {
    auto *debug_loc = llvm::DILocation::get(instruction->getContext(), line, column, variable->getScope());
    instruction->setDebugLoc(debug_loc);
  }

  llvm::DIType *create_basic_type(const std::string &name, uint64_t size_in_bits, unsigned encoding) {
    return di_builder->createBasicType(name, size_in_bits, encoding);
  }

  llvm::DIType *create_pointer_type(llvm::DIType *pointee_type, uint64_t size_in_bits) {
    return di_builder->createPointerType(pointee_type, size_in_bits);
  }

  llvm::DIType *create_function_type(llvm::ArrayRef<llvm::Metadata *> param_types) {
    return di_builder->createSubroutineType(di_builder->getOrCreateTypeArray(param_types));
  }

  llvm::DIType *create_struct_type(DIScope *scope, const std::string &name, llvm::DIFile *file, unsigned line,
                                   uint64_t size_in_bits, uint64_t align_in_bits, llvm::DINode::DIFlags flags,
                                   llvm::ArrayRef<llvm::Metadata *> elements) {
    return di_builder->createStructType(scope, name, file, line, size_in_bits, align_in_bits, flags, nullptr,
                                        di_builder->getOrCreateArray(elements));
  }

  llvm::DIType *create_array_type(uint64_t size, uint64_t align_in_bits, llvm::DIType *element_type,
                                  llvm::ArrayRef<llvm::Metadata *> subscripts) {
    return di_builder->createArrayType(size, align_in_bits, element_type, di_builder->getOrCreateArray(subscripts));
  }
};

struct LLVMEmitter {
  Context &ctx;
  LLVMContext llvm_ctx;
  IRBuilder<> builder;
  llvm::DIFile *file;

  std::unique_ptr<llvm::Module> module;
  std::shared_ptr<DIBuilder> di_builder;
  llvm::Target *target;
  llvm::DataLayout data_layout;
  llvm::TargetMachine *target_machine;
  bool dont_load;
  DIManager dbg;

  inline LLVMEmitter(Context &ctx)
      : ctx(ctx), llvm_ctx(), builder(llvm_ctx), module(std::make_unique<llvm::Module>("module", llvm_ctx)),
        di_builder(std::make_shared<DIBuilder>(*module)), dont_load(false), dbg(di_builder), data_layout("") {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    auto target_triple = "x86_64-pc-linux-gnu";
    module->setTargetTriple(target_triple);

    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
    if (!target) {
      throw std::runtime_error("Failed to lookup target: " + error);
    }

    llvm::TargetOptions opt;
    auto reloc_model = llvm::Reloc::Model::PIC_;
    auto target_machine = target->createTargetMachine(target_triple, "generic", "", opt, reloc_model);
    module->setDataLayout(data_layout = target_machine->createDataLayout());
    module->setTargetTriple(target_machine->getTargetTriple().str());
  }

  using type_pair = std::pair<llvm::Type *, llvm::DIType *>;

  inline type_pair llvm_typeof_impl(Type *type) {
    static std::map<Type *, type_pair> memoized_types;

    // Check if the type is already memoized
    if (memoized_types.contains(type)) {
      return memoized_types.at(type);
    }

    // Handle pointer types
    if (type->get_ext().is_pointer()) {
      auto elem_ty = global_get_type(type->get_element_type());
      const auto &[element_type, element_di_type] = llvm_typeof_impl(elem_ty);
      auto llvm_type = llvm::PointerType::get(element_type, 0);
      auto di_type = dbg.create_pointer_type(element_di_type, 64);
      type_pair pair = {llvm_type, di_type};
      memoized_types.insert({type, pair});
      return pair;
    }

    // Handle fixed-sized arrays
    if (type->get_ext().is_fixed_sized_array()) {
      auto elem_ty = global_get_type(type->get_element_type());
      const auto &[element_type, element_di_type] = llvm_typeof_impl(elem_ty);
      auto array_size = type->get_ext().extensions.back().array_size;
      auto llvm_type = llvm::ArrayType::get(element_type, array_size);
      auto di_type = dbg.create_array_type(array_size * element_type->getPrimitiveSizeInBits(),
                                           element_type->getPrimitiveSizeInBits(), element_di_type, {});
      type_pair pair = {llvm_type, di_type};
      memoized_types.insert({type, pair});
      return pair;
    }

    switch (type->kind) {
      case TYPE_SCALAR: {
        auto info = type->get_info()->as<ScalarTypeInfo>();
        llvm::Type *llvm_type;
        llvm::DIType *di_type;
        switch (info->scalar_type) {
          case TYPE_VOID:
            llvm_type = llvm::Type::getVoidTy(llvm_ctx);
            di_type = dbg.create_basic_type("void", 0, llvm::dwarf::DW_ATE_unsigned);
            break;
          case TYPE_S8:
            llvm_type = llvm::Type::getInt8Ty(llvm_ctx);
            di_type = dbg.create_basic_type("s8", 8, llvm::dwarf::DW_ATE_signed);
            break;
          case TYPE_S16:
            llvm_type = llvm::Type::getInt16Ty(llvm_ctx);
            di_type = dbg.create_basic_type("s16", 16, llvm::dwarf::DW_ATE_signed);
            break;
          case TYPE_S32:
            llvm_type = llvm::Type::getInt32Ty(llvm_ctx);
            di_type = dbg.create_basic_type("s32", 32, llvm::dwarf::DW_ATE_signed);
            break;
          case TYPE_S64:
            llvm_type = llvm::Type::getInt64Ty(llvm_ctx);
            di_type = dbg.create_basic_type("s64", 64, llvm::dwarf::DW_ATE_signed);
            break;
          case TYPE_U8:
            llvm_type = llvm::Type::getInt8Ty(llvm_ctx);
            di_type = dbg.create_basic_type("u8", 8, llvm::dwarf::DW_ATE_unsigned);
            break;
          case TYPE_U16:
            llvm_type = llvm::Type::getInt16Ty(llvm_ctx);
            di_type = dbg.create_basic_type("u16", 16, llvm::dwarf::DW_ATE_unsigned);
            break;
          case TYPE_U32:
            llvm_type = llvm::Type::getInt32Ty(llvm_ctx);
            di_type = dbg.create_basic_type("u32", 32, llvm::dwarf::DW_ATE_unsigned);
            break;
          case TYPE_U64:
            llvm_type = llvm::Type::getInt64Ty(llvm_ctx);
            di_type = dbg.create_basic_type("u64", 64, llvm::dwarf::DW_ATE_unsigned);
            break;
          case TYPE_FLOAT:
            llvm_type = llvm::Type::getFloatTy(llvm_ctx);
            di_type = dbg.create_basic_type("f32", 32, llvm::dwarf::DW_ATE_float);
            break;
          case TYPE_DOUBLE:
            llvm_type = llvm::Type::getDoubleTy(llvm_ctx);
            di_type = dbg.create_basic_type("f64", 64, llvm::dwarf::DW_ATE_float);
            break;
          case TYPE_CHAR:
            llvm_type = llvm::Type::getInt8Ty(llvm_ctx);
            di_type = dbg.create_basic_type("char", 8, llvm::dwarf::DW_ATE_unsigned_char);
            break;
          case TYPE_BOOL:
            llvm_type = llvm::Type::getInt1Ty(llvm_ctx);
            di_type = dbg.create_basic_type("bool", 1, llvm::dwarf::DW_ATE_boolean);
            break;
        }
        type_pair pair = {llvm_type, di_type};
        memoized_types.insert({type, pair});
        return pair;
      } break;

      case TYPE_STRUCT: {
        auto info = type->get_info()->as<StructTypeInfo>();

        // Forward declaration for recursive types
        auto struct_name = info->scope->full_name();
        auto llvm_struct_type = llvm::StructType::create(llvm_ctx, struct_name);

        type_pair forward_pair = {llvm_struct_type, nullptr};
        memoized_types.insert({type, forward_pair});

        bool is_union = HAS_FLAG(info->flags, STRUCT_FLAG_IS_UNION);

        std::vector<llvm::Type *> member_types;
        std::vector<llvm::Metadata *> member_debug_info;

        llvm::Type *largest_member_type = nullptr;
        uint64_t largest_member_size = 0;

        for (const auto &[name, symbol] : info->scope->symbols) {
          if (!symbol.is_variable())
            continue;

          auto member_type = global_get_type(symbol.type_id);
          auto [llvm_member_type, di_member_type] = llvm_typeof_impl(member_type);

          if (is_union) {
            uint64_t member_size = data_layout.getTypeAllocSize(llvm_member_type);
            if (member_size > largest_member_size) {
              largest_member_size = member_size;
              largest_member_type = llvm_member_type;
            }
          } else {
            // For structs, add all members
            member_types.push_back(llvm_member_type);
          }

          member_debug_info.push_back(
              dbg.create_variable(dbg.current_scope(), name.get_str(), file, 0, di_member_type));
        }

        if (is_union) {
          member_types.push_back(largest_member_type);
        }

        llvm_struct_type->setBody(member_types);

        auto di_struct_type = dbg.create_struct_type(
            dbg.current_scope(), struct_name, file, 0, data_layout.getTypeAllocSize(llvm_struct_type) * 8,
            data_layout.getABITypeAlign(llvm_struct_type).value() * 8, llvm::DINode::FlagZero, member_debug_info);

        type_pair final_pair = {llvm_struct_type, di_struct_type};
        memoized_types[type] = final_pair;
        return final_pair;
      } break;

      case TYPE_ENUM: {
        auto info = type->get_info()->as<EnumTypeInfo>();

        std::vector<llvm::Metadata *> enumerators;
        for (const auto &[name, value] : info->scope->symbols) {
          if (value.is_variable()) {
            auto val = evaluate_constexpr(value.variable.initial_value.get(), ctx);
            enumerators.push_back(di_builder->createEnumerator(name.get_str(), val.integer));
          }
        }

        auto [underlying_type, underlying_di_type] = llvm_typeof_impl(global_get_type(info->element_type));
        auto enum_name = info->scope->full_name();
        auto di_enum_type =
            di_builder->createEnumerationType(dbg.current_scope(), enum_name, file, 0, 32, 32,
                                              di_builder->getOrCreateArray(enumerators), underlying_di_type);

        type_pair pair = {underlying_type, di_enum_type};
        memoized_types.insert({type, pair});
        return pair;
      } break;

      case TYPE_TUPLE: {
        auto info = type->get_info()->as<TupleTypeInfo>();

        // Forward declaration for recursive types
        auto tuple_name = type->get_base().get_str();
        auto llvm_tuple_type = llvm::StructType::create(llvm_ctx, tuple_name);

        // Memoize the forward declaration
        type_pair forward_pair = {llvm_tuple_type, nullptr};
        memoized_types.insert({type, forward_pair});

        // Populate the tuple
        std::vector<llvm::Type *> element_types;
        std::vector<llvm::Metadata *> element_debug_info;

        unsigned index = 0;
        for (const auto &element : info->types) {
          auto element_type = global_get_type(element);
          auto [llvm_element_type, di_element_type] = llvm_typeof_impl(element_type);
          element_types.push_back(llvm_element_type);
          element_debug_info.push_back(
              dbg.create_variable(dbg.current_scope(), std::to_string(index), file, 0, di_element_type));
          ++index;
        }

        llvm_tuple_type->setBody(element_types);

        auto di_tuple_type = dbg.create_struct_type(
            dbg.current_scope(), tuple_name, file, 0, data_layout.getTypeAllocSize(llvm_tuple_type) * 8,
            data_layout.getABITypeAlign(llvm_tuple_type).value() * 8, llvm::DINode::FlagZero, element_debug_info);

        type_pair final_pair = {llvm_tuple_type, di_tuple_type};
        memoized_types[type] = final_pair;
        return final_pair;
      } break;

      case TYPE_CHOICE: {
        auto info = type->get_info()->as<ChoiceTypeInfo>();

        // Forward declaration for recursive types
        auto choice_name = type->get_base().get_str();
        auto llvm_choice_type = llvm::StructType::create(llvm_ctx, choice_name);

        // Memoize the forward declaration
        type_pair forward_pair = {llvm_choice_type, nullptr};
        memoized_types.insert({type, forward_pair});

        // Populate the choice type
        std::vector<llvm::Type *> choice_fields = {
            llvm::Type::getInt32Ty(llvm_ctx) // Discriminant
        };
        std::vector<llvm::Metadata *> variant_debug_info;

        size_t largest_member_size = 0;
        llvm::Type *largest_payload_type = nullptr;

        for (const auto &[variant_name, variant_info] : info->variants) {
          auto variant_type = global_get_type(variant_info);

          if (variant_type->id != void_type()) {
            auto [llvm_payload_type, di_payload_type] = llvm_typeof_impl(variant_type);

            uint64_t member_size = data_layout.getTypeAllocSize(llvm_payload_type);
            if (member_size > largest_member_size) {
              largest_member_size = member_size;
              largest_payload_type = llvm_payload_type;
            }

            variant_debug_info.push_back(dbg.create_struct_type(
                dbg.current_scope(), variant_name.get_str(), file, 0,
                data_layout.getTypeAllocSize(llvm_payload_type) * 8,
                data_layout.getABITypeAlign(llvm_payload_type).value() * 8, llvm::DINode::FlagZero, {di_payload_type}));
          }
        }

        if (largest_payload_type) {
          choice_fields.push_back(largest_payload_type);
        }

        llvm_choice_type->setBody(choice_fields);

        auto di_choice_type = dbg.create_struct_type(
            dbg.current_scope(), choice_name, file, 0, data_layout.getTypeAllocSize(llvm_choice_type) * 8,
            data_layout.getABITypeAlign(llvm_choice_type).value() * 8, llvm::DINode::FlagZero, variant_debug_info);

        type_pair final_pair = {llvm_choice_type, di_choice_type};
        memoized_types[type] = final_pair;
        return final_pair;
      } break;

      case TYPE_DYN: {
        auto info = type->get_info()->as<DynTypeInfo>();

        // Forward declaration for recursive types
        auto dyn_name = type->get_base().get_str();
        auto llvm_dyn_type = llvm::StructType::create(llvm_ctx, dyn_name);

        // Memoize the forward declaration
        type_pair forward_pair = {llvm_dyn_type, nullptr};
        memoized_types.insert({type, forward_pair});

        // Populate the dynamic type
        std::vector<llvm::Type *> dyn_fields = {
            llvm::PointerType::get(llvm::Type::getVoidTy(llvm_ctx), 0) // void* instance
        };
        std::vector<llvm::Metadata *> dyn_debug_info = {
            dbg.create_pointer_type(dbg.create_basic_type("void", 0, llvm::dwarf::DW_ATE_unsigned), 64)};

        for (const auto &[method_name, method_type] : info->methods) {
          auto [llvm_function_type, di_function_type] = llvm_typeof_impl(method_type);
          dyn_fields.push_back(llvm::PointerType::get(llvm_function_type, 0));
          dyn_debug_info.push_back(dbg.create_function_type(di_function_type));
        }

        llvm_dyn_type->setBody(dyn_fields);

        auto di_dyn_type = dbg.create_struct_type(
            dbg.current_scope(), dyn_name, file, 0, data_layout.getTypeAllocSize(llvm_dyn_type) * 8,
            data_layout.getABITypeAlign(llvm_dyn_type).value() * 8, llvm::DINode::FlagZero, dyn_debug_info);

        type_pair final_pair = {llvm_dyn_type, di_dyn_type};
        memoized_types[type] = final_pair;
        return final_pair;
      } break;

      case TYPE_FUNCTION: {
        auto info = type->get_info()->as<FunctionTypeInfo>();

        std::vector<llvm::Type *> param_types;
        std::vector<llvm::Metadata *> param_debug_info;

        for (int i = 0; i < info->params_len; ++i) {
          auto param_type = global_get_type(info->parameter_types[i]);
          auto [llvm_param_type, di_param_type] = llvm_typeof_impl(param_type);
          param_types.push_back(llvm_param_type);
          param_debug_info.push_back(di_param_type);
        }

        auto [llvm_return_type, di_return_type] = llvm_typeof_impl(global_get_type(info->return_type));
        auto llvm_function_type = llvm::FunctionType::get(llvm_return_type, param_types, info->is_varargs);

        auto di_function_type = dbg.create_function_type(param_debug_info);

        type_pair pair = {llvm_function_type, di_function_type};
        memoized_types.insert({type, pair});
        return pair;
      } break;

      case TYPE_INTERFACE: {
        throw_error(
            std::format("interface types should never be lowered to IR. interface type:: {}", type->to_string()), {});
      } break;
    }

    throw_error("failed to lower type", {});
    return {};
  }

  inline llvm::Type *llvm_typeof(Type *type) { return llvm_typeof_impl(type).first; }

  llvm::Value *visit_method_call(ASTMethodCall *node);
  llvm::Value *visit_path(ASTPath *node);
  llvm::Value *visit_pattern_match(ASTPatternMatch *node);
  llvm::Value *visit_dyn_of(ASTDyn_Of *node);
  llvm::Value *visit_type_of(ASTType_Of *node);
  llvm::Value *visit_block(ASTBlock *node);
  llvm::Value *visit_expr_statement(ASTExprStatement *node);
  llvm::Value *visit_bin_expr(ASTBinExpr *node);
  llvm::Value *visit_unary_expr(ASTUnaryExpr *node);
  llvm::Value *visit_literal(ASTLiteral *node);
  llvm::Value *visit_type(ASTType *node);
  llvm::Value *visit_call(ASTCall *node);
  llvm::Value *visit_return(ASTReturn *node);
  llvm::Value *visit_dot_expr(ASTDotExpr *node);
  llvm::Value *visit_subscript(ASTSubscript *node);
  llvm::Value *visit_initializer_list(ASTInitializerList *node);
  llvm::Value *visit_range(ASTRange *node);
  llvm::Value *visit_switch(ASTSwitch *node);
  llvm::Value *visit_tuple(ASTTuple *node);
  llvm::Value *load_value(ASTNode *node, llvm::Value *expr);
  llvm::Value *visit_cast(ASTCast *node);
  llvm::Value *visit_lambda(ASTLambda *node);
  llvm::Value *visit_size_of(ASTSize_Of *node);

  void visit_struct_declaration(ASTStructDeclaration *node);
  void visit_module(ASTModule *node);
  void visit_import(ASTImport *node);
  void visit_program(ASTProgram *node);
  void visit_function_declaration(ASTFunctionDeclaration *node);
  void visit_params_decl(ASTParamsDecl *node);
  void visit_param_decl(ASTParamDecl *node);
  void visit_variable(ASTVariable *node);
  void visit_arguments(ASTArguments *node);
  void visit_continue(ASTContinue *node);
  void visit_break(ASTBreak *node);
  void visit_for(ASTFor *node);
  void visit_if(ASTIf *node);
  void visit_else(ASTElse *node);
  void visit_while(ASTWhile *node);
  void visit_enum_declaration(ASTEnumDeclaration *node);
  void visit_tuple_deconstruction(ASTTupleDeconstruction *node);
  void visit_alias(ASTAlias *node);
  void visit_impl(ASTImpl *node);
  void visit_defer(ASTDefer *node);
  void visit_choice_declaration(ASTChoiceDeclaration *node);
  void visit_interface_declaration(ASTInterfaceDeclaration *node);
  void visit_where(ASTWhere *node);

  inline void visit_statement_list(ASTStatementList *node) {
    for (const auto &stmt : node->statements) {
      visit_node(stmt);
    }
  };

  void visit_statement(ASTStatement *statement) {
    switch (statement->get_node_type()) {
      case AST_NODE_BLOCK:
        visit_block(static_cast<ASTBlock *>(statement));
        break;
      case AST_NODE_FUNCTION_DECLARATION:
        visit_function_declaration(static_cast<ASTFunctionDeclaration *>(statement));
        break;
      case AST_NODE_ALIAS:
        visit_alias(static_cast<ASTAlias *>(statement));
        break;
      case AST_NODE_IMPL:
        visit_impl(static_cast<ASTImpl *>(statement));
        break;
      case AST_NODE_IMPORT:
        visit_import(static_cast<ASTImport *>(statement));
        break;
      case AST_NODE_MODULE:
        visit_module(static_cast<ASTModule *>(statement));
        break;
      case AST_NODE_RETURN:
        visit_return(static_cast<ASTReturn *>(statement));
        break;
      case AST_NODE_CONTINUE:
        visit_continue(static_cast<ASTContinue *>(statement));
        break;
      case AST_NODE_BREAK:
        visit_break(static_cast<ASTBreak *>(statement));
        break;
      case AST_NODE_FOR:
        visit_for(static_cast<ASTFor *>(statement));
        break;
      case AST_NODE_IF:
        visit_if(static_cast<ASTIf *>(statement));
        break;
      case AST_NODE_ELSE:
        visit_else(static_cast<ASTElse *>(statement));
        break;
      case AST_NODE_WHILE:
        visit_while(static_cast<ASTWhile *>(statement));
        break;
      case AST_NODE_STRUCT_DECLARATION:
        visit_struct_declaration(static_cast<ASTStructDeclaration *>(statement));
        break;
      case AST_NODE_ENUM_DECLARATION:
        visit_enum_declaration(static_cast<ASTEnumDeclaration *>(statement));
        break;
      case AST_NODE_CHOICE_DECLARATION:
        visit_choice_declaration(static_cast<ASTChoiceDeclaration *>(statement));
        break;
      case AST_NODE_INTERFACE_DECLARATION:
        visit_interface_declaration(static_cast<ASTInterfaceDeclaration *>(statement));
        break;
      case AST_NODE_PARAMS_DECL:
        visit_params_decl(static_cast<ASTParamsDecl *>(statement));
        break;
      case AST_NODE_VARIABLE:
        visit_variable(static_cast<ASTVariable *>(statement));
        break;
      case AST_NODE_EXPR_STATEMENT:
        visit_expr_statement(static_cast<ASTExprStatement *>(statement));
        break;
      case AST_NODE_DEFER:
        visit_defer(static_cast<ASTDefer *>(statement));
        break;
      case AST_NODE_TUPLE_DECONSTRUCTION:
        visit_tuple_deconstruction(static_cast<ASTTupleDeconstruction *>(statement));
        break;
      case AST_NODE_STATEMENT_LIST:
        visit_statement_list(static_cast<ASTStatementList *>(statement));
        break;
      default:
        break;
    }
  }

  llvm::Value *visit_expr(ASTExpr *expr) {
    switch (expr->get_node_type()) {
      case AST_NODE_LAMBDA:
        return visit_lambda(static_cast<ASTLambda *>(expr));
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr(static_cast<ASTBinExpr *>(expr));
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr(static_cast<ASTUnaryExpr *>(expr));
      case AST_NODE_LITERAL:
        return visit_literal(static_cast<ASTLiteral *>(expr));
      case AST_NODE_PATH:
        return visit_path(static_cast<ASTPath *>(expr));
      case AST_NODE_TYPE:
        return visit_type(static_cast<ASTType *>(expr));
      case AST_NODE_TUPLE:
        return visit_tuple(static_cast<ASTTuple *>(expr));
      case AST_NODE_CALL:
        return visit_call(static_cast<ASTCall *>(expr));
      case AST_NODE_METHOD_CALL:
        return visit_method_call(static_cast<ASTMethodCall *>(expr));
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr(static_cast<ASTDotExpr *>(expr));
      case AST_NODE_SUBSCRIPT:
        return visit_subscript(static_cast<ASTSubscript *>(expr));
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list(static_cast<ASTInitializerList *>(expr));
      case AST_NODE_SIZE_OF:
        return visit_size_of(static_cast<ASTSize_Of *>(expr));
      case AST_NODE_TYPE_OF:
        return visit_type_of(static_cast<ASTType_Of *>(expr));
      case AST_NODE_DYN_OF:
        return visit_dyn_of(static_cast<ASTDyn_Of *>(expr));
      case AST_NODE_CAST:
        return visit_cast(static_cast<ASTCast *>(expr));
      case AST_NODE_RANGE:
        return visit_range(static_cast<ASTRange *>(expr));
      case AST_NODE_SWITCH:
        return visit_switch(static_cast<ASTSwitch *>(expr));
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match(static_cast<ASTPatternMatch *>(expr));
      default:
        return nullptr;
    }
  }

  std::variant<llvm::Value *, std::nullptr_t> visit_node(ASTNode *node) {
    switch (node->get_node_type()) {
      // Expression nodes
      case AST_NODE_LAMBDA:
        return visit_lambda(static_cast<ASTLambda *>(node));
      case AST_NODE_BIN_EXPR:
        return visit_bin_expr(static_cast<ASTBinExpr *>(node));
      case AST_NODE_UNARY_EXPR:
        return visit_unary_expr(static_cast<ASTUnaryExpr *>(node));
      case AST_NODE_LITERAL:
        return visit_literal(static_cast<ASTLiteral *>(node));
      case AST_NODE_PATH:
        return visit_path(static_cast<ASTPath *>(node));
      case AST_NODE_TYPE:
        return visit_type(static_cast<ASTType *>(node));
      case AST_NODE_TUPLE:
        return visit_tuple(static_cast<ASTTuple *>(node));
      case AST_NODE_CALL:
        return visit_call(static_cast<ASTCall *>(node));
      case AST_NODE_METHOD_CALL:
        return visit_method_call(static_cast<ASTMethodCall *>(node));
      case AST_NODE_DOT_EXPR:
        return visit_dot_expr(static_cast<ASTDotExpr *>(node));
      case AST_NODE_SUBSCRIPT:
        return visit_subscript(static_cast<ASTSubscript *>(node));
      case AST_NODE_INITIALIZER_LIST:
        return visit_initializer_list(static_cast<ASTInitializerList *>(node));
      case AST_NODE_SIZE_OF:
        return visit_size_of(static_cast<ASTSize_Of *>(node));
      case AST_NODE_TYPE_OF:
        return visit_type_of(static_cast<ASTType_Of *>(node));
      case AST_NODE_DYN_OF:
        return visit_dyn_of(static_cast<ASTDyn_Of *>(node));
      case AST_NODE_CAST:
        return visit_cast(static_cast<ASTCast *>(node));
      case AST_NODE_RANGE:
        return visit_range(static_cast<ASTRange *>(node));
      case AST_NODE_SWITCH:
        return visit_switch(static_cast<ASTSwitch *>(node));
      case AST_NODE_PATTERN_MATCH:
        return visit_pattern_match(static_cast<ASTPatternMatch *>(node));

      // Statement nodes
      case AST_NODE_BLOCK:
        visit_block(static_cast<ASTBlock *>(node));
        return nullptr;
      case AST_NODE_FUNCTION_DECLARATION:
        visit_function_declaration(static_cast<ASTFunctionDeclaration *>(node));
        return nullptr;
      case AST_NODE_ALIAS:
        visit_alias(static_cast<ASTAlias *>(node));
        return nullptr;
      case AST_NODE_IMPL:
        visit_impl(static_cast<ASTImpl *>(node));
        return nullptr;
      case AST_NODE_IMPORT:
        visit_import(static_cast<ASTImport *>(node));
        return nullptr;
      case AST_NODE_MODULE:
        visit_module(static_cast<ASTModule *>(node));
        return nullptr;
      case AST_NODE_RETURN:
        visit_return(static_cast<ASTReturn *>(node));
        return nullptr;
      case AST_NODE_CONTINUE:
        visit_continue(static_cast<ASTContinue *>(node));
        return nullptr;
      case AST_NODE_BREAK:
        visit_break(static_cast<ASTBreak *>(node));
        return nullptr;
      case AST_NODE_FOR:
        visit_for(static_cast<ASTFor *>(node));
        return nullptr;
      case AST_NODE_IF:
        visit_if(static_cast<ASTIf *>(node));
        return nullptr;
      case AST_NODE_ELSE:
        visit_else(static_cast<ASTElse *>(node));
        return nullptr;
      case AST_NODE_WHILE:
        visit_while(static_cast<ASTWhile *>(node));
        return nullptr;
      case AST_NODE_STRUCT_DECLARATION:
        visit_struct_declaration(static_cast<ASTStructDeclaration *>(node));
        return nullptr;
      case AST_NODE_ENUM_DECLARATION:
        visit_enum_declaration(static_cast<ASTEnumDeclaration *>(node));
        return nullptr;
      case AST_NODE_CHOICE_DECLARATION:
        visit_choice_declaration(static_cast<ASTChoiceDeclaration *>(node));
        return nullptr;
      case AST_NODE_INTERFACE_DECLARATION:
        visit_interface_declaration(static_cast<ASTInterfaceDeclaration *>(node));
        return nullptr;
      case AST_NODE_PARAMS_DECL:
        visit_params_decl(static_cast<ASTParamsDecl *>(node));
        return nullptr;
      case AST_NODE_PARAM_DECL:
        visit_param_decl(static_cast<ASTParamDecl *>(node));
        return nullptr;
      case AST_NODE_VARIABLE:
        visit_variable(static_cast<ASTVariable *>(node));
        return nullptr;
      case AST_NODE_EXPR_STATEMENT:
        visit_expr_statement(static_cast<ASTExprStatement *>(node));
        return nullptr;
      case AST_NODE_DEFER:
        visit_defer(static_cast<ASTDefer *>(node));
        return nullptr;
      case AST_NODE_TUPLE_DECONSTRUCTION:
        visit_tuple_deconstruction(static_cast<ASTTupleDeconstruction *>(node));
        return nullptr;
      case AST_NODE_STATEMENT_LIST:
        visit_statement_list(static_cast<ASTStatementList *>(node));
        return nullptr;
      default:
        throw_error("unhandled node in visit_node", node->source_range);
        return nullptr;
    }
  }

  llvm::Value *binary_signed(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *expr_ty = nullptr);

  llvm::Value *binary_unsigned(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *expr_ty = nullptr);

  llvm::Value *binary_fp(llvm::Value *left, llvm::Value *right, TType op, llvm::Type *expr_ty = nullptr);

  llvm::Value *binary_scalars(ASTExpr *left_ast, ASTExpr *right_ast, TType op, Type *expr_ty);

  llvm::Value *cast_scalar(llvm::Value *value, Type *from, Type *to);
};
