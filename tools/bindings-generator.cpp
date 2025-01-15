#include <chrono>
#include <clang-c/CXString.h>
#include <filesystem>
#include <limits.h>
#include <clang-c/Index.h>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <fstream>
#include <vector>
#include <print>

// TODO: 
/*
  Fix anonymous struct declarations being emitted after the type that depends on them

  Fix weird type bugs with emitting unsigned u8 * etc.

  Have an option to either expand, or not expand all dependent headers. We don't always need to include everything, right now you have to 
  parse out and remove stuff like stdint etc.
*/

// used for anonymous structs etc.
std::string get_unique_identifier() {
  static int num = 0;
  auto tok = "__bind_gen__anon_D" + std::to_string(num);
  num++;
  return tok;
}

static std::unordered_map<std::string, bool> emitted_types;

#define LOG(message, data) data->logfile << "[" << current_timestamp() << "] INFO: " << message << "\n";
#define ERROR(message, data) data->logfile << "[" << current_timestamp() << "] ERROR: " << message << "\n";

static inline std::string get_source_location_clang(CXCursor cursor) {
  unsigned line, column;
  CXString fileName;
  CXSourceLocation location = clang_getCursorLocation(cursor);
  clang_getPresumedLocation(location, &fileName, &line, &column);
  std::string fileNameStr = clang_getCString(fileName);
  clang_disposeString(fileName);
  return std::format("{}:{}:{}", line, column, fileNameStr);
}

inline std::string current_timestamp() {
  auto now = std::chrono::system_clock::now();
  auto in_time_t = std::chrono::system_clock::to_time_t(now);
  std::stringstream ss;
  ss << std::put_time(std::localtime(&in_time_t), "%Y-%m-%d %X");
  return ss.str();
}

struct ClangVisitData {
  std::stringstream output;
  std::ofstream logfile;
};

static std::vector<std::pair<std::string, std::string>> type_map = {{"char", "u8"},
                                                                    {"float", "float32"},
                                                                    {"double", "float64"},
                                                                    {"long double", "float64"},
                                                                    {"_Bool", "bool"},
                                                                    {"usize", "u64"},
                                                                    {"signed char", "s8"},
                                                                    {"short", "s16"},
                                                                    {"int", "s32"},
                                                                    {"long", "s64"},
                                                                    {"long long", "s64"},
                                                                    {"ssize_t", "s64"},
                                                                    {"ptrdiff_t", "u64"},
                                                                    {"intptr_t", "s64"},
                                                                    {"intmax_t", "s64"},
                                                                    {"int8_t", "s8"},
                                                                    {"int16_t", "s16"},
                                                                    {"int32_t", "s32"},
                                                                    {"int64_t", "s64"},
                                                                    {"size_t", "u64"},
                                                                    {"unsigned char", "u8"},
                                                                    {"unsigned short", "u16"},
                                                                    {"unsigned long long", "u64"},
                                                                    {"unsigned int", "u32"},
                                                                    {"unsigned long", "u64"},
                                                                    {"uint8_t", "u8"},
                                                                    {"uintptr_t", "u64"},
                                                                    {"uintmax_t", "u64"},
                                                                    {"uint16_t", "u16"},
                                                                    {"uint32_t", "u32"},
                                                                    {"uint64_t", "u64"},
                                                                    {"signed", "s32"},
                                                                    {"unsigned", "u32"},
                                                                    {"void", "void"}};

static inline std::string wrapgen_get_type_name(CXType type) {
  if (type.kind == CXType_Pointer) {
    std::string result = wrapgen_get_type_name(clang_getPointeeType(type)) + " *";
    return result;
  }

  if (type.kind == CXType_ConstantArray) {
    long long arraySize = clang_getArraySize(type);
    std::string result = wrapgen_get_type_name(clang_getArrayElementType(type)) + "[" + std::to_string(arraySize) + "]";
    return result;
  }

  if (type.kind == CXType_Typedef) {
    CXType underlyingType = clang_getTypedefDeclUnderlyingType(clang_getTypeDeclaration(type));
    return wrapgen_get_type_name(underlyingType);
  }

  CXString typeName = clang_getTypeSpelling(type);
  std::string result = clang_getCString(typeName);
  clang_disposeString(typeName);

  size_t constPos = result.find("const");
  if (constPos != std::string::npos) {
    result.erase(constPos, 6);
  }
  size_t restrictPos = result.find("restrict");
  if (restrictPos != std::string::npos) {
    result.erase(restrictPos, 9);
  }

  if (result.find("unnamed at") != std::string::npos) {
    result = get_unique_identifier();
  }

  // Replace all occurrences of keys in type_map with their corresponding values
  for (const auto &[key, value] : type_map) {
    if (result == key) {
      result = value;
      break;
    }
  }

  if (result.starts_with("struct ")) {
    result = result.substr(7);
  } else if (result.starts_with("union ")) {
    result = result.substr(6);
  } else if (result.starts_with("enum ")) {
    result = result.substr(5);
  }

  return result;
}

// Helper function to declare a type in the global module
static inline void wrapgen_declare_type(CXCursor cursor, ClangVisitData *data) {
  CXString name = clang_getCursorSpelling(cursor);
  std::string typeName = clang_getCString(name);
  clang_disposeString(name);

  if (clang_getCursorKind(cursor) == CXCursor_TypedefDecl) {
    CXType underlyingType = clang_getTypedefDeclUnderlyingType(cursor);
    std::string underlyingTypeName = wrapgen_get_type_name(underlyingType);

    if (underlyingTypeName.contains("va_list")) {
      return;
    }

    if (emitted_types.contains(typeName)) {
      return;
    }

    if (underlyingType.kind == CXType_Pointer && clang_getPointeeType(underlyingType).kind == CXType_FunctionProto) {
      CXType pointeeType = clang_getPointeeType(underlyingType);
      std::string pointeeTypeName = wrapgen_get_type_name(pointeeType);

      std::string returnType = wrapgen_get_type_name(clang_getResultType(pointeeType));
      std::string args;
      int numArgs = clang_getNumArgTypes(pointeeType);
      for (int i = 0; i < numArgs; ++i) {
        if (i > 0) {
          args += ", ";
        }
        args += wrapgen_get_type_name(clang_getArgType(pointeeType, i));
      }

      data->output << "#alias " << typeName << " :: fn*(" << args << ") -> " << returnType << ";\n";
    } else if (underlyingType.kind == CXType_FunctionProto) {
      std::string returnType = wrapgen_get_type_name(clang_getResultType(underlyingType));
      std::string args;
      int numArgs = clang_getNumArgTypes(underlyingType);
      for (int i = 0; i < numArgs; ++i) {
        if (i > 0) {
          args += ", ";
        }
        args += wrapgen_get_type_name(clang_getArgType(underlyingType, i));
      }
      data->output << "#alias " << typeName << " :: fn*(" << args << ") -> " << returnType << ";\n";
    } else {
      data->output << "#alias " << typeName << " :: " << underlyingTypeName << ";\n";
    }
  } else {
    data->output << "type " << typeName << ";\n";
  }
}

// Helper function to visit the children of a struct, union, or enum declaration
static inline void wrapgen_visit_struct_union_enum(CXCursor cursor, ClangVisitData *data, const char *kind) {
  CXString name = clang_getCursorSpelling(cursor);
  CXType type = clang_getCursorType(cursor);
  std::string typeName = wrapgen_get_type_name(type);
  clang_disposeString(name);

  if (emitted_types.find(typeName) != emitted_types.end()) {
    return;
  }

  emitted_types[typeName] = true;

  std::stringstream temp_output;
  std::stringstream anon_output;
  temp_output << typeName << " :: " << kind << " {\n";

  clang_visitChildren(
      cursor,
      [](CXCursor c, CXCursor parent, CXClientData client_data) {
        ClangVisitData *data = (ClangVisitData *)client_data;
        std::stringstream &temp_output = *(std::stringstream *)client_data;
        std::stringstream &anon_output = *(std::stringstream *)client_data;
        if (clang_getCursorKind(c) == CXCursor_FieldDecl) {
          CXString fieldName = clang_getCursorSpelling(c);
          std::string fieldNameStr = clang_getCString(fieldName);
          clang_disposeString(fieldName);
          CXType fieldType = clang_getCursorType(c);
          std::string fieldTypeName = wrapgen_get_type_name(fieldType);

          if (fieldNameStr.empty()) {
            if (clang_getCursorKind(c) == CXCursor_StructDecl || clang_getCursorKind(c) == CXCursor_UnionDecl) {
              std::string anonTypeName = get_unique_identifier();
              anon_output << anonTypeName
                          << " :: " << (clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union") << " {\n";
              wrapgen_visit_struct_union_enum(c, data,
                                              clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union");
              anon_output << "};\n";
              temp_output << "  " << anonTypeName
                          << " : #anon :: " << (clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union")
                          << " {\n";
              wrapgen_visit_struct_union_enum(c, data,
                                              clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union");
              temp_output << "  };\n";
            }
          } else {
            if (clang_getCursorKind(c) == CXCursor_StructDecl || clang_getCursorKind(c) == CXCursor_UnionDecl) {
              std::string anonTypeName = get_unique_identifier();
              anon_output << anonTypeName
                          << " :: " << (clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union") << " {\n";
              wrapgen_visit_struct_union_enum(c, data,
                                              clang_getCursorKind(c) == CXCursor_StructDecl ? "struct" : "union");
              anon_output << "};\n";
              temp_output << "  " << fieldNameStr << " : " << anonTypeName << ";\n";
            } else {
              temp_output << "  " << fieldNameStr << " : " << fieldTypeName << ";\n";
            }
          }
        } else if (clang_getCursorKind(c) == CXCursor_EnumConstantDecl) {
          CXString variantName = clang_getCursorSpelling(c);
          std::string variantNameStr = clang_getCString(variantName);
          clang_disposeString(variantName);
          long long enumValue = clang_getEnumConstantDeclValue(c);
          temp_output << "  " << variantNameStr << " = " << enumValue << ",\n";
        }
        return CXChildVisit_Continue;
      },
      &temp_output);

  temp_output << "};\n";
  data->output << anon_output.str();
  data->output << temp_output.str();
}

static inline CXChildVisitResult wrapgen_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
  CXCursorKind kind = clang_getCursorKind(cursor);
  ClangVisitData *data = (ClangVisitData *)client_data;
  if (kind == CXCursor_FunctionDecl) {
    CXString name = clang_getCursorSpelling(cursor);
    std::string fn_name = clang_getCString(name);
    clang_disposeString(name);

    CXType return_type = clang_getCursorResultType(cursor);
    std::string return_type_name = wrapgen_get_type_name(return_type);

    data->output << "#foreign " << fn_name << " :: fn(";

    int n_params = clang_Cursor_getNumArguments(cursor);
    for (int i = 0; i < n_params; ++i) {
      CXCursor arg_cursor = clang_Cursor_getArgument(cursor, i);
      CXString arg_name = clang_getCursorSpelling(arg_cursor);
      std::string arg_name_str = clang_getCString(arg_name);
      clang_disposeString(arg_name);

      if (arg_name_str.empty()) {
        arg_name_str = "param" + std::to_string(i);
      }
      CXType arg_type = clang_getCursorType(arg_cursor);
      std::string arg_type_name = wrapgen_get_type_name(arg_type);
      if (i > 0) {
        data->output << ", ";
      }
      data->output << arg_name_str << ": " << arg_type_name;
    }

    data->output << ") -> " << return_type_name << ";\n";
  } else if (kind == CXCursor_VarDecl) {
    CXString name = clang_getCursorSpelling(cursor);
    std::string variable_name = clang_getCString(name);
    clang_disposeString(name);
    CXType variable_type = clang_getCursorType(cursor);
    std::string variable_type_name = wrapgen_get_type_name(variable_type);
    data->output << variable_name << ": " << variable_type_name << ";\n";
  } else if (kind == CXCursor_StructDecl) {
    wrapgen_visit_struct_union_enum(cursor, data, "struct");
  } else if (kind == CXCursor_UnionDecl) {
    wrapgen_visit_struct_union_enum(cursor, data, "union");
  } else if (kind == CXCursor_EnumDecl) {
    wrapgen_visit_struct_union_enum(cursor, data, "enum");
  } else if (kind == CXCursor_TypedefDecl) {
    wrapgen_declare_type(cursor, data);
  }

  return CXChildVisit_Recurse;
}

static inline std::string preprocess_header(const std::string &filename) {
  std::string command = "clang -E -P -frewrite-includes -I/usr/include -I/usr/local/include " + filename;
  std::string preprocessed_file =
      std::filesystem::current_path().string() + "/" + std::filesystem::path(filename).filename().string() + ".inc";
  command += " -o " + preprocessed_file;

  int result = system(command.c_str());
  if (result != 0) {
    throw std::runtime_error("Preprocessing failed. Quitting.");
  }

  std::ifstream file(preprocessed_file);
  if (!file.is_open()) {
    throw std::runtime_error("Unable to open preprocessed file. Quitting.");
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  file.close();

  return buffer.str();
}

static inline std::string find_system_header(const std::string &header) {
  std::vector<std::string> common_paths = {
#ifdef __linux__
      "/usr/include/",
      "/usr/local/include/",
      "/usr/include/x86_64-linux-gnu/",
#elif defined(__APPLE__)
      "/usr/include/",
      "/usr/local/include/",
      "/Library/Developer/CommandLineTools/usr/include/",
#elif defined(_WIN32)
      "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Include\\",
      "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\",
#endif
  };

  for (const auto &path : common_paths) {
    std::string full_path = path + header;
    char abs_path[PATH_MAX];
    if (realpath(full_path.c_str(), abs_path) != nullptr) {
      return std::string(abs_path);
    }
  }

  return "";
}

static inline ClangVisitData wrapgen_import_c_header_into_module(const std::string &filename) {
  CXIndex index = clang_createIndex(0, 0);
  std::string preprocessed_code = preprocess_header(filename);

  unsigned int options = CXTranslationUnit_DetailedPreprocessingRecord;

  static constexpr const char *args[] = {
      "-I/usr/include",
      "-I/usr/local/include",
  };

  CXUnsavedFile unsaved_file = {filename.c_str(), preprocessed_code.c_str(), preprocessed_code.size()};
  CXTranslationUnit unit = clang_parseTranslationUnit(index, filename.c_str(), args, sizeof(args) / sizeof(char *),
                                                      &unsaved_file, 1, options);
  if (!unit) {
    throw std::runtime_error("Unable to parse translation unit. Quitting.");
  }

  ClangVisitData data;
  data.logfile.open("wrapgen.log");

  CXCursor cursor = clang_getTranslationUnitCursor(unit);
  clang_visitChildren(cursor, &wrapgen_visitor, &data);
  clang_disposeTranslationUnit(unit);
  clang_disposeIndex(index);
  return data;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <header-file>\n";
    return 1;
  }

  std::string filename = argv[1];
  auto path = std::filesystem::current_path();

  try {
    auto data = wrapgen_import_c_header_into_module(filename);
    std::string output_filename = std::filesystem::path(filename).filename().replace_extension(".ela").string();
    std::cout << "at path: " << output_filename << '\n';
    std::ofstream ela_file(path / output_filename);
    if (!ela_file.is_open()) {
      throw std::runtime_error("Unable to open .ela file for writing.");
    }
    ela_file << std::format(R"_(/*
  These bindings were auto generated by 'ela-bindings-generator', from '{}' at {}. 
*/
)_",
                            filename, current_timestamp());

    ela_file << data.output.str();
    ela_file.close();
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return 1;
  }

  return 0;
}