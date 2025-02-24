#pragma once

constexpr auto HELP_STRING = R"_(
Ela compiler:
   compile a file: `ela <filename.ela>`
   compile & run a 'main.ela' in current directory: `ela run`
   initialize a 'main.ela' file in current directory: `ela init`

   Available flags:
   --release     Compile with -O3 flag and with no debug information from Ela. defaults to false, which is a debug build.
   --no-compile  Transpile to C++ but don't invoke the clang++ compiler automatically.
   --s           Don't delete the `.c` file used to transpile.
   --metrics     Write performance metrics to stdout.
   --test        Only emit functions marked `#test` and bootstrap the default test runner. You still have to run the binary to run the tests.
)_";

constexpr auto RAYLIB_INIT_CODE = R"__(
#import raylib;

main :: fn() {
  // Silence the raylib console output spewage
  SetTraceLogLevel(TraceLogLevel::LOG_NONE);

  InitWindow(800, 600, "Hello, Raylib!");

  // Allow resizable window
  SetConfigFlags(ConfigFlags::FLAG_WINDOW_RESIZABLE);

  // 60 fps cap.
  SetTargetFPS(60);

  while !WindowShouldClose() {
    BeginDrawing();
    ClearBackground(WHITE);

    // Draw 'label' rect.
    rect_x := (GetScreenWidth() / 2) - 150;
    rect_y := (GetScreenHeight() / 2) - 150;
    DrawRectangle(rect_x, rect_y, 300, 300, BLACK);

    // Draw text.
    text := "Hello, Raylib!";
    text_x := (GetScreenWidth() / 2) - (MeasureText(text, 24) / 2);
    text_y := GetScreenHeight() / 2;
    DrawText(text, text_x, text_y, 24, WHITE);

    // Отлично! (excellent!)
    EndDrawing();
  }
}
)__";

constexpr auto MAIN_INIT_CODE = R"__(
main :: fn() {
  // 'Env::args()' returns a `u8*[]` of the C runtime's arguments
  // We do this instead of taking `(int argc, char** argv)` in the main function, main _CANNOT_ take args.
  i := 0;
  for arg in Env::args() {
    println($"arg {i++}: {arg}");
  }
  println("Hello World!");
}
)__";


static constexpr auto TESTING_MAIN_BOILERPLATE_AAAAGHH = R"__(
  #ifdef TESTING
  #define __TEST_RUNNER_MAIN                                                                                             \
    int main() {                                                                                                         \
      for (int i = 0; i < sizeof(tests) / sizeof($ela_test); i++) {                                      \
        $ela_test_run(&tests[i]);                                                                        \
      }                                                                                                                  \
    }                                                                                                                     
  #endif
  )__";
  
  // This is stuff we just can't really get rid of while using a transpiled backend.
  static constexpr auto INESCAPABLE_BOILERPLATE_AAAGHHH = R"__(
  
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
  #include <stddef.h>
  
  #if USE_STD_LIB
    #include <stdint.h>
    #include <errno.h>
    #undef RAND_MAX
  #endif
  
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
  