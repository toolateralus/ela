#pragma once

/*
  this is used for std::format() with the type id to get the reflected upon type info.
*/
constexpr auto REFL_TY_FORMAT_STRING = "refl_ty${}";

constexpr auto MAIN_FMT = R"_(
int main (int argc, char** argv) {{
  /* initialize command line args. */
  {}(argc, argv);
  /* run the global initializers */
  ela_run_global_initializers();
  /* call user main, or dispatch tests, depending on the build type. */
  ___MAIN___;
}}
)_";

constexpr auto HELP_STRING = R"_(
  compile a file: `ela <filename.ela>` | 'ela' (compiles main.ela in current directory by default)
  compile & run a 'main.ela' in current directory: `ela run` | 'ela r'
  compile & run a 'test.ela' in current directory: 'ela test' | 'ela t'

  Both 'run' and 'test' can take the filename optionally.
  example:
  'ela r my_main.ela'
  'ela t my_tests.ela'

  initialize a 'main.ela' file in current directory: `ela init`
  Available 'init' args:
    raylib -- a boilerplate raylib program.

  Available flags:
    --release          Compile with -O3 flag and with no debug information from Ela. defaults to false, which is a debug build.
    --no-compile       Transpile to C++ but don't invoke the clang++ compiler automatically.
    --nl               Compile in debug mode, but with no 'line info'. this, often paired with --s, will allow you to debug the output C code.
    --freestanding     Compile without the C Standard Library. equivalent to '--nostdlib & --ffreestanding' for GCC/Clang.
    --nostdlib         Compile without Ela's standard library. Note, this includes many types such as List!<T>,Slice!<T>,str/String, etc. We don't have a "Core" seperated from the stdlib.
    --s                "Save" and don't delete the `.c` file used to transpile
    --metrics          Write performance metrics to stdout.
    --x                Print the command used to compile the outputted C code.
    --test             Only emit functions marked `#test` and bootstrap the default test runner. You still have to run the binary to run the tests.

  Warning Exclusions:
    "--Wignore-all"                   Ignore all warnings.
    "--Wno-arrow-operator"            Ignore warnings about overloading -> operator, since it's not used as an operator. for C++ programmers.
    "--Wno-inaccessible-decl"         Ignore warnings about declarations that cannot be used.
    "--Wno-switch-break"              Ignore warnings about not needing break within switch statements.
  
  In the future, we'll just have a json file to do this configuration in a simpler way (while maintaining this older version)
)_";

constexpr auto RAYLIB_INIT_CODE = R"__(
import raylib::*;

fn main () {
  // Silence the raylib console output
  SetTraceLogLevel(TraceLogLevel::LOG_NONE);

  InitWindow(800, 600, "Hello, Raylib!"c);

  SetConfigFlags(ConfigFlags::FLAG_WINDOW_RESIZABLE);
  SetTargetFPS(60);

  while !WindowShouldClose() {
    BeginDrawing();
    ClearBackground(WHITE);

    // Draw 'label' rect.
    rect_x := (GetScreenWidth() / 2) - 150;
    rect_y := (GetScreenHeight() / 2) - 150;
    DrawRectangle(rect_x, rect_y, 300, 300, BLACK);

    // Draw text.
    text := "Hello, Raylib!"c; // the little 'c' denotes a c string, *const u8
    text_x := (GetScreenWidth() / 2) - (MeasureText(text, 24) / 2);
    text_y := GetScreenHeight() / 2;
    DrawText(text, text_x, text_y, 24, WHITE);

    EndDrawing();
  }
}
)__";

constexpr auto MAIN_INIT_CODE = R"__(
import fmt::*;

fn main() {
  hellos := List!<str>::init(.[
    "Hello, World!",
    "Привет, Мир!",
    "Hola, Mundo!",
    "Bonjour, le Monde!",
    "Hallo, Welt!",
    "Ciao, Mondo!",
    "こんにちは、世界！",
    "안녕하세요, 세계!",
    "Olá, Mundo!",
    "你好，世界！",
    "Hej, Världen!",
    "Salve, Mundi!"
  ]);

  defer hellos.destroy();

  for hello in hellos {
    println(hello);
  }
}
)__";

/* Macro to decide what to run at main. We could just do this in the compiler, idk why we have this. */
static constexpr auto TESTING_BOILERPLATE = R"__(
#ifdef TESTING
  #define ___MAIN___\
    for (int i = 0; i < sizeof(tests) / sizeof($ela_test); i++) { $ela_test_run(&tests[i]); }
#else 
  #define ___MAIN___\
    __ela_main_();
#endif
  )__";

// This is stuff we just can't really get rid of while using a transpiled backend.
static constexpr auto BOILERPLATE_C_CODE = R"__(
  typedef unsigned long long int u64;
  typedef signed long long int s64;

  typedef signed int s32;
  typedef unsigned int u32;

  typedef double f64;
  typedef float f32;

  typedef short int s16;
  typedef unsigned short int u16;

  typedef signed char s8;
  typedef unsigned char u8;

  /* static initializers. */
  void ela_run_global_initializers();

  #include <stddef.h>
  #include <stdarg.h>

  #ifdef TESTING
    #if TEST_VERBOSE
      int printf(u8 *, ...);
    #endif

    typedef struct {
      const char *name;
      void (*function)();
    } $ela_test;

    static void $ela_test_run($ela_test *test) {
      #if TEST_VERBOSE
        printf("running %s\n", test->name);
      #endif
      test->function();
    }
  #endif
  )__";
