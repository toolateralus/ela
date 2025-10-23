#pragma once

#include "interned_string.hpp"
#include "mir.hpp"
#include "core.hpp"
#include "type.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DIBuilder.h"
#include <llvm/ADT/ArrayRef.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <map>
#include <memory>
#include <system_error>
#include <unordered_map>

using namespace Mir;

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
        res += '\\';  // Handle dangling backslash
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
          res += '\x1B';  // Escape character
          iss.get();
          break;
        case 'x': {   // Hexadecimal escape sequence
          iss.get();  // Consume 'x'
          std::string hex_digits;
          for (int i = 0; i < 2 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (!hex_digits.empty()) {
            res += static_cast<char>(std::stoi(hex_digits, nullptr, 16));
          } else {
            res += "\\x";  // Invalid \x escape
          }
          break;
        }
        case 'u': {   // Unicode escape sequence (\uXXXX)
          iss.get();  // Consume 'u'
          std::string hex_digits;
          for (int i = 0; i < 4 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (hex_digits.size() == 4) {
            int codepoint = std::stoi(hex_digits, nullptr, 16);
            res += static_cast<char>(codepoint);  // Simplified for ASCII span
          } else {
            res += "\\u" + hex_digits;  // Invalid \u escape
          }
          break;
        }
        case 'U': {   // Unicode escape sequence (\UXXXXXXXX)
          iss.get();  // Consume 'U'
          std::string hex_digits;
          for (int i = 0; i < 8 && std::isxdigit(iss.peek()); ++i) {
            hex_digits += iss.get();
          }
          if (hex_digits.size() == 8) {
            int codepoint = std::stoi(hex_digits, nullptr, 16);
            res += static_cast<char>(codepoint);  // Simplified for ASCII span
          } else {
            res += "\\U" + hex_digits;  // Invalid \U escape
          }
          break;
        }
        default:
          if (next >= '0' && next <= '7') {  // Octal escape sequence
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
      res += c;  // Non-escaped characters are added as-is
    }
  }

  return res;
}

struct LLVM_Emitter;
struct DIManager {
  std::shared_ptr<DIBuilder> di_builder;

  llvm::DICompileUnit *cu;
  llvm::DIFile *last_known_file;

  std::vector<llvm::DIFile *> files;
  std::unordered_map<std::filesystem::path, llvm::DIFile *> files_by_path;

  std::stack<llvm::DICompileUnit *> cu_stack;
  std::stack<llvm::DIFile *> file_stack;
  std::stack<llvm::DISubprogram *> subprogram_stack;
  std::stack<llvm::DILexicalBlock *> lexical_stack;

  DIManager() = default;

  explicit inline DIManager(std::shared_ptr<DIBuilder> &builder) : di_builder(builder) {
    for (const std::filesystem::path file : Span::files()) {
      auto abs = std::filesystem::absolute(file).lexically_normal();
      auto basename = abs.filename().string();
      auto dirpath = abs.parent_path().string();
      auto di_file = di_builder->createFile(basename, dirpath);
      files.push_back(di_file);
      files_by_path[abs] = di_file;
    }

    auto entry_file = files_by_path[Span::user_entry_file()];
    last_known_file = entry_file;
    cu = di_builder->createCompileUnit(llvm::dwarf::DW_LANG_C, entry_file, "Ela", false, "", 0);
    cu_stack.push(cu);
    file_stack.push(entry_file);
  }

  enum class Scope { Lexical, Subroutine, File, CU } kind;

  inline void push_scope(llvm::DIScope *scope, Scope kind) {
    switch (kind) {
      case Scope::Lexical:
        lexical_stack.push(llvm::cast<llvm::DILexicalBlock>(scope));
        break;
      case Scope::Subroutine:
        subprogram_stack.push(llvm::cast<llvm::DISubprogram>(scope));
        break;
      case Scope::File:
        file_stack.push(llvm::cast<llvm::DIFile>(scope));
        break;
      case Scope::CU:
        cu_stack.push(llvm::cast<llvm::DICompileUnit>(scope));
        break;
    }
  }

  inline void pop_scope(Scope kind) {
    switch (kind) {
      case Scope::Lexical:
        if (!lexical_stack.empty()) lexical_stack.pop();
        break;
      case Scope::Subroutine:
        if (!subprogram_stack.empty()) subprogram_stack.pop();
        break;
      case Scope::File:
        if (!file_stack.empty()) file_stack.pop();
        break;
      case Scope::CU:
        if (!cu_stack.empty()) cu_stack.pop();
        break;
    }
  }

  inline llvm::DIScope *current_scope(Scope kind) const {
    switch (kind) {
      case Scope::Lexical:
        return lexical_stack.top();
      case Scope::Subroutine:
        return subprogram_stack.top();
      case Scope::File:
        return file_stack.top();
      case Scope::CU:
        return cu_stack.top();
    }
  }

  inline llvm::DIScope *current_scope() const {
    if (!lexical_stack.empty()) return lexical_stack.top();
    if (!subprogram_stack.empty()) return subprogram_stack.top();
    if (!file_stack.empty()) return file_stack.top();
    if (!cu_stack.empty()) return cu_stack.top();
    return cu;
  }

  inline std::tuple<std::string, std::string, unsigned, unsigned> extract_span(Span span) {
    auto line = span.line, column = span.column;
    std::filesystem::path abs_path = span.files()[span.file];
    auto basename = abs_path.filename().string();
    auto dirpath = abs_path.parent_path().string();
    return {basename, dirpath, line, column};
  }

  inline llvm::DIFile *get_file_scope(const Span &span) {
    std::filesystem::path path = std::filesystem::absolute(span.filename()).lexically_normal();
    auto it = files_by_path.find(path);
    if (it != files_by_path.end()) {
      last_known_file = it->second;
      return it->second;
    }
    throw_error(std::format("unable to get file from span: {}", path.string()), span);
    return nullptr;
  }

  llvm::DISubprogram *enter_function_scope(const Type *type, LLVM_Emitter *emitter, llvm::Function *function,
                                           const std::string &name, const Span &span);

  inline llvm::DILexicalBlock *enter_lexical_scope(llvm::DIScope *parent, const Span &span) {
    auto [basename, dirpath, line, column] = extract_span(span);
    auto *block = di_builder->createLexicalBlock(parent, get_file_scope(span), line, column);
    lexical_stack.push(block);
    return block;
  }

  inline llvm::DILocation *get_location(const Span &span, llvm::DIScope *scope, llvm::LLVMContext &ctx) {
    return llvm::DILocation::get(ctx, span.line, span.column, scope, nullptr);
  }

  inline llvm::DIVariable *create_variable(llvm::DIScope *scope, const std::string &name, Span span, llvm::DIType *type,
                                           llvm::Value *alloca, llvm::IRBuilder<> &builder, llvm::LLVMContext &ctx) {
    auto [basename, dirpath, line, column] = extract_span(span);
    llvm::DILocalVariable *var = di_builder->createAutoVariable(scope, name, get_file_scope(span), line, type, true);
    di_builder->insertDeclare(alloca, var, di_builder->createExpression(), get_location(span, scope, ctx),
                              builder.GetInsertBlock());
    return var;
  }

  inline llvm::DIType *create_basic_type(const std::string &name, uint64_t size_in_bits, unsigned encoding) {
    return di_builder->createBasicType(name, size_in_bits, encoding);
  }

  inline llvm::DIType *create_pointer_type(llvm::DIType *pointee_type, uint64_t size_in_bits) {
    return di_builder->createPointerType(pointee_type, size_in_bits);
  }

  inline llvm::DIType *create_function_type(llvm::ArrayRef<llvm::Metadata *> param_types) {
    return di_builder->createSubroutineType(di_builder->getOrCreateTypeArray(param_types));
  }

  inline llvm::DIType *create_struct_type(llvm::DIScope *scope, const std::string &name, llvm::DIFile *file, unsigned line,
                                          uint64_t size_in_bits, uint64_t align_in_bits, llvm::DINode::DIFlags flags,
                                          llvm::ArrayRef<llvm::Metadata *> elements) {
    return di_builder->createStructType(scope, name, file, line, size_in_bits, align_in_bits, flags, nullptr,
                                        di_builder->getOrCreateArray(elements));
  }

  inline llvm::DIType *create_array_type(uint64_t size, uint64_t align_in_bits, llvm::DIType *element_type,
                                         llvm::ArrayRef<llvm::Metadata *> subscripts) {
    return di_builder->createArrayType(size, align_in_bits, element_type, di_builder->getOrCreateArray(subscripts));
  }

  inline llvm::Value *create_dbg(llvm::Value *v, Span span) {
    if (!v) {
      throw_error("create_dbg got a null llvm::Value *", span);
    }
    if (compile_command.has_flag("nl")) return v;
    if (auto *inst = llvm::dyn_cast<llvm::Instruction>(v)) attach_debug_info(inst, span);
    return v;
  }

  inline llvm::Instruction *create_dbg(llvm::Instruction *v, Span span) {
    if (compile_command.has_flag("nl")) return v;
    attach_debug_info(v, span);
    return v;
  }

  inline void attach_debug_info(llvm::Instruction *instruction, Span span) {
    auto [basename, dirpath, line, column] = extract_span(span);
    auto *debug_loc = llvm::DILocation::get(instruction->getContext(), line, column, current_scope());
    instruction->setDebugLoc(debug_loc);
  }

  inline llvm::DIDerivedType *create_type_member(llvm::DIType *parent, llvm::DIFile *file, size_t line, size_t align_in_bits,
                                                 size_t size_in_bits, size_t offset_in_bits, llvm::DIType *t,
                                                 InternedString name) {
    return di_builder->createMemberType(parent,          // The scope is the containing struct.
                                        name.str(),      // The member's name.
                                        file,            // The file it's defined in.
                                        line,            // The line number.
                                        size_in_bits,    // Size in bits.
                                        align_in_bits,   // Alignment in bits.
                                        offset_in_bits,  // Offset in bits.
                                        llvm::DINode::FlagZero,
                                        t  // The member's type.
    );
  }
};

struct LLVM_Emitter {
  inline bool is_temporary_valid(uint32_t temp) const { return temps.contains(temp); }

  void register_constructor(llvm::Function *, uint32_t);

  LLVMContext llvm_ctx;
  IRBuilder<> builder;
  llvm::DIFile *file;

  std::unique_ptr<llvm::Module> llvm_module;
  std::shared_ptr<DIBuilder> di_builder;
  llvm::Target *target;
  llvm::DataLayout data_layout;
  llvm::TargetMachine *target_machine;
  DIManager dbg;

  Nullable<llvm::Value> sret_destination = nullptr;

  Mir::Module &m;

  std::unordered_map<const Global_Variable *, llvm::GlobalVariable *> global_variables;
  std::unordered_map<Mir::Function *, llvm::Function *> function_table;
  std::unordered_map<Mir::Basic_Block *, llvm::BasicBlock *> bb_table;
  std::vector<llvm::Value *> arg_stack;

  struct Allocation {
    Type *type;
    InternedString name;
    llvm::Value *value;
  };

  // this is per each function, is cleared on exit.
  std::unordered_map<uint32_t, Allocation> temps;

  inline llvm::raw_fd_ostream &ostream() {
    static std::error_code ec;
    static llvm::raw_fd_ostream stream{"-", ec};
    return stream;
  }

  inline void insert_temp(uint32_t idx, Mir::Function *f, llvm::Value *v) {
    Type *type = nullptr;
    InternedString name = "";

    assert(f->temps.size() > idx);

    type = f->temps[idx].type;
    name = f->temps[idx].name;

    Allocation allocation = {
        .type = type,
        .name = name,
        .value = v,
    };

    assert(!v->getType()->isVoidTy());
    assert(v != nullptr);
    temps[idx] = allocation;
  }

  inline LLVM_Emitter(Mir::Module &m) : llvm_ctx(), builder(llvm_ctx), data_layout(""), m(m) {
    llvm_module = (std::make_unique<llvm::Module>(Span::files()[m.functions[0]->span.file], llvm_ctx)),

    llvm_module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
    llvm_module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 4);

    di_builder = std::make_shared<DIBuilder>(*llvm_module);
    new (&dbg) DIManager(di_builder);

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    auto target_triple = "x86_64-pc-linux-gnu";
    llvm_module->setTargetTriple(target_triple);

    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
    if (!target) {
      throw std::runtime_error("Failed to lookup target: " + error);
    }

    llvm::TargetOptions opt;
    auto reloc_model = llvm::Reloc::Model::PIC_;
    auto target_machine = target->createTargetMachine(target_triple, "generic", "", opt, reloc_model);
    llvm_module->setDataLayout(data_layout = target_machine->createDataLayout());
    llvm_module->setTargetTriple(target_machine->getTargetTriple().str());
  }

  using type_pair = std::pair<llvm::Type *, llvm::DIType *>;

  inline llvm::FunctionType *llvm_fn_typeof(const Type *type) { return llvm::dyn_cast<llvm::FunctionType>(llvm_typeof(type)); }
  inline llvm::Type *llvm_typeof(const Type *type) { return llvm_typeof_impl(type).first; }

  inline type_pair llvm_typeof_impl(const Type *type) {
    static std::map<const Type *, type_pair> memoized_types;
    if (memoized_types.contains(type)) {
      return memoized_types.at(type);
    }

    if (type->is_pointer()) {
      auto elem_ty = type->get_element_type();
      const auto &[element_type, element_di_type] = llvm_typeof_impl(elem_ty);
      auto llvm_type = llvm::PointerType::get(element_type, 0);
      auto di_type = dbg.create_pointer_type(element_di_type, 64);
      type_pair pair = {llvm_type, di_type};
      memoized_types.insert({type, pair});
      return pair;
    }

    if (type->is_fixed_sized_array()) {
      auto elem_ty = type->get_element_type();
      const auto &[element_type, element_di_type] = llvm_typeof_impl(elem_ty);
      auto array_size = type->extensions.back().array_size;
      auto llvm_type = llvm::ArrayType::get(element_type, array_size);
      auto di_type = dbg.create_array_type(array_size * element_type->getPrimitiveSizeInBits(),
                                           element_type->getPrimitiveSizeInBits(), element_di_type, {});
      type_pair pair = {llvm_type, di_type};
      memoized_types.insert({type, pair});
      return pair;
    }

    switch (type->kind) {
      case TYPE_SCALAR: {
        auto info = type->info->as<ScalarTypeInfo>();
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
        auto info = type->info->as<StructTypeInfo>();

        // Forward declaration for recursive types
        auto struct_name = info->scope->full_name();
        auto llvm_struct_type = llvm::StructType::create(llvm_ctx, struct_name);

        type_pair forward_pair = {llvm_struct_type, nullptr};
        memoized_types.insert({type, forward_pair});

        bool is_union = info->is_union;

        std::vector<llvm::Type *> member_types;
        std::vector<llvm::Metadata *> member_debug_info;

        llvm::Type *largest_member_type = nullptr;
        uint64_t largest_member_size = 0;

        auto di_struct_type = dbg.create_struct_type(dbg.current_scope(), struct_name, file, 0, type->size_in_bytes() * 8,
                                                     data_layout.getABITypeAlign(llvm_struct_type).value() * 8,
                                                     llvm::DINode::FlagZero, member_debug_info);

        for (const auto &[name, symbol] : info->scope->symbols) {
          if (!symbol.is_variable) continue;

          auto member_type = symbol.resolved_type;
          auto [llvm_member_type, di_member_type] = llvm_typeof_impl(member_type);

          if (is_union) {
            uint64_t member_size = member_type->size_in_bytes();
            if (member_size > largest_member_size) {
              largest_member_size = member_size;
              largest_member_type = llvm_member_type;
            }
          } else {
            // For structs, add all members
            member_types.push_back(llvm_member_type);
          }

          llvm::DIDerivedType *member =
              dbg.create_type_member(di_struct_type, file, 0, member_type->alignment_in_bytes() * 8,
                                     member_type->size_in_bytes() * 8, type->offset_in_bytes(name) * 8, di_member_type, name);

          member_debug_info.push_back(member);
        }

        if (is_union) {
          member_types.push_back(largest_member_type);
        }

        llvm_struct_type->setBody(member_types);

        type_pair final_pair = {llvm_struct_type, di_struct_type};
        memoized_types[type] = final_pair;
        return final_pair;
      } break;
      case TYPE_ENUM: {
        auto info = type->info->as<EnumTypeInfo>();

        std::vector<llvm::Metadata *> enumerators;
        for (const auto &[name, value] : info->scope->symbols) {
          if (value.is_variable) {
            // TODO: fix this once our interpreter works on MIR
            // auto val = interpret(value.variable.initial_value.get(), ctx);
            // enumerators.push_back(di_builder->createEnumerator(name.get_str(), 0, false));
          }
        }

        auto [underlying_type, underlying_di_type] = llvm_typeof_impl(info->underlying_type);
        auto enum_name = info->scope->full_name();
        auto di_enum_type = di_builder->createEnumerationType(dbg.current_scope(), enum_name, file, 0, 32, 32,
                                                              di_builder->getOrCreateArray(enumerators), underlying_di_type);

        type_pair pair = {underlying_type, di_enum_type};
        memoized_types.insert({type, pair});
        return pair;
      } break;
      case TYPE_TUPLE: {
        auto info = type->info->as<TupleTypeInfo>();
        auto tuple_name = type->basename.str();
        auto llvm_tuple_type = llvm::StructType::create(llvm_ctx, tuple_name);

        type_pair forward_pair = {llvm_tuple_type, nullptr};
        memoized_types.insert({type, forward_pair});

        std::vector<llvm::Type *> element_types;
        std::vector<llvm::Metadata *> element_debug_info;

        auto di_tuple_type =
            dbg.create_struct_type(dbg.current_scope(), tuple_name, file, 0, 0, 0, llvm::DINode::FlagZero, element_debug_info);

        unsigned offset = 0;
        unsigned index = 0;
        for (const auto &element_type_info : info->types) {
          auto [llvm_element_type, di_element_type] = llvm_typeof_impl(element_type_info);
          element_types.push_back(llvm_element_type);

          auto member_di = dbg.create_type_member(
              di_tuple_type, file, 0, data_layout.getABITypeAlign(llvm_element_type).value() * 8,
              data_layout.getTypeAllocSizeInBits(llvm_element_type), offset, di_element_type, std::to_string(index));

          element_debug_info.push_back(member_di);
          offset += data_layout.getTypeAllocSize(llvm_element_type) * 8;
          ++index;
        }

        llvm_tuple_type->setBody(element_types);

        type_pair final_pair = {llvm_tuple_type, di_tuple_type};
        memoized_types[type] = final_pair;
        return final_pair;
      }

      case TYPE_CHOICE: {
        auto info = type->info->as<ChoiceTypeInfo>();
        auto choice_name = type->basename.str();
        auto llvm_choice_type = llvm::StructType::create(llvm_ctx, choice_name);

        type_pair forward_pair = {llvm_choice_type, nullptr};
        memoized_types.insert({type, forward_pair});

        std::vector<llvm::Type *> fields;
        std::vector<llvm::Metadata *> field_debug_info;

        auto di_choice_type =
            dbg.create_struct_type(dbg.current_scope(), choice_name, file, 0, 0, 0, llvm::DINode::FlagZero, field_debug_info);

        // discriminant
        fields.push_back(llvm::Type::getInt32Ty(llvm_ctx));
        field_debug_info.push_back(dbg.create_type_member(di_choice_type, file, 0, 32, 32, 0, nullptr, "discriminant"));

        size_t largest_size = 0;
        llvm::Type *largest_member = nullptr;

        unsigned offset = 32;
        for (const auto &member : info->members) {
          if (member.type == void_type()) continue;

          auto [llvm_member_type, di_member_type] = llvm_typeof_impl(member.type);
          uint64_t member_size = data_layout.getTypeAllocSize(llvm_member_type);
          if (member_size > largest_size) {
            largest_size = member_size;
            largest_member = llvm_member_type;
          }

          auto member_di =
              dbg.create_type_member(di_choice_type, file, 0, data_layout.getABITypeAlign(llvm_member_type).value() * 8,
                                     member_size * 8, offset, di_member_type, member.name.str());
          field_debug_info.push_back(member_di);
        }

        // Push the largest member into the LLVM struct body so it can hold any variant
        if (largest_member) fields.push_back(largest_member);

        llvm_choice_type->setBody(fields);

        type_pair final_pair = {llvm_choice_type, di_choice_type};
        memoized_types[type] = final_pair;
        return final_pair;
      }

      case TYPE_DYN: {
        auto info = type->info->as<DynTypeInfo>();
        auto dyn_name = type->basename.str();
        auto llvm_dyn_type = llvm::StructType::create(llvm_ctx, dyn_name);

        type_pair forward_pair = {llvm_dyn_type, nullptr};
        memoized_types.insert({type, forward_pair});

        std::vector<llvm::Type *> dyn_fields;
        std::vector<llvm::Metadata *> dyn_debug_info;

        auto di_dyn_type =
            dbg.create_struct_type(dbg.current_scope(), dyn_name, file, 0, 0, 0, llvm::DINode::FlagZero, dyn_debug_info);

        auto void_ptr = llvm::PointerType::get(llvm::Type::getVoidTy(llvm_ctx), 0);

        // instance pointer
        dyn_fields.push_back(void_ptr);
        dyn_debug_info.push_back(dbg.create_type_member(
            di_dyn_type, file, 0, 64, 64, 0,
            dbg.create_pointer_type(dbg.create_basic_type("void", 0, llvm::dwarf::DW_ATE_unsigned), 64), "instance"));

        // vtable pointer
        auto void_ptr_ptr = llvm::PointerType::get(void_ptr, 0);
        dyn_fields.push_back(void_ptr_ptr);
        dyn_debug_info.push_back(dbg.create_type_member(
            di_dyn_type, file, 0, 64, 64, 64,
            dbg.create_pointer_type(dbg.create_basic_type("void", 0, llvm::dwarf::DW_ATE_unsigned), 64), "vtable"));

        unsigned offset = 128;
        for (const auto &[method_name, method_type] : info->methods) {
          auto [llvm_func_type, di_func_type] = llvm_typeof_impl(method_type);
          dyn_fields.push_back(llvm::PointerType::get(llvm_func_type, 0));

          dyn_debug_info.push_back(
              dbg.create_type_member(di_dyn_type, file, 0, data_layout.getABITypeAlign(llvm_func_type).value() * 8,
                                     data_layout.getTypeAllocSize(llvm_func_type) * 8, offset, di_func_type, method_name.str()));
          offset += data_layout.getTypeAllocSize(llvm_func_type) * 8;
        }

        llvm_dyn_type->setBody(dyn_fields);

        type_pair final_pair = {llvm_dyn_type, di_dyn_type};
        memoized_types[type] = final_pair;
        return final_pair;
      }

      case TYPE_FUNCTION: {
        auto info = type->info->as<FunctionTypeInfo>();

        std::vector<llvm::Type *> param_types;
        std::vector<llvm::Metadata *> param_debug_info;

        for (size_t i = 0; i < info->params_len; ++i) {
          auto param_type = info->parameter_types[i];
          auto [llvm_param_type, di_param_type] = llvm_typeof_impl(param_type);
          param_types.push_back(llvm_param_type);
          param_debug_info.push_back(di_param_type);
        }

        auto [llvm_return_type, di_return_type] = llvm_typeof_impl(info->return_type);
        auto llvm_function_type = llvm::FunctionType::get(llvm_return_type, param_types, info->is_varargs);

        auto di_function_type = dbg.create_function_type(param_debug_info);

        type_pair pair = {llvm_function_type, di_function_type};
        memoized_types.insert({type, pair});
        return pair;
      } break;
      case TYPE_TRAIT: {
        throw_error(std::format("trait types should never be lowered to IR. trait type:: {}", type->to_string()), {});
      } break;
    }

    throw_error("failed to lower type", {});
    return {};
  }

  void emit_module();
  void emit_function(Mir::Function *f, llvm::Function *ir_f);
  void emit_basic_block(Mir::Basic_Block *bb, Mir::Function *f, llvm::BasicBlock *entry_bb);

  inline llvm::Value *create_dbg(llvm::Value *v, Span span) {
    return dbg.create_dbg(v, span);  // attaches debug info if v is an instruction
  }

  inline llvm::Instruction *create_dbg(llvm::Instruction *v, Span span) {
    return dbg.create_dbg(v, span);  // attaches debug info if v is an instruction
  }

  inline llvm::CallInst *create_dbg(llvm::CallInst *v, Span span) {
    return (llvm::CallInst *)dbg.create_dbg(v, span);  // attaches debug info if v is an instruction
  }

  llvm::Value *pointer_binary(llvm::Value *left, llvm::Value *right, const Instruction &instr);
  llvm::Value *pointer_unary(llvm::Value *operand, const Instruction &instr);
  llvm::Value *visit_operand(Operand op, Span span);

  llvm::Value *binary_signed(llvm::Value *left, llvm::Value *right, Op_Code op);
  llvm::Value *binary_unsigned(llvm::Value *left, llvm::Value *right, Op_Code op);
  llvm::Value *binary_fp(llvm::Value *left, llvm::Value *right, Op_Code op);
  llvm::Value *perform_cast(llvm::Value *value, Type *from, Type *to, Type **new_type);
};
