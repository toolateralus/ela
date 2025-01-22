#pragma once

constexpr auto HELP_STRING = R"_(
Ela compiler:
   compile a file: `ela <filename.ela>`
   compile & run a 'main.ela' in current directory: `ela run`
   initialize a 'main.ela' file in current directory: `ela init`

   Available flags:
   --release     Compile with -O3 flag and with no debug information from Ela. defaults to false, which is a debug build.
   --no-compile  Transpile to C++ but don't invoke the clang++ compiler automatically.
   --s           Don't delete the `.hpp` and `.cpp` files used to transpile.
   --metrics     Write performance metrics to stdout.
   --test        Only emit functions marked `#test` and create a test runner. You still have to run the binary to run the tests.
)_";

constexpr auto RAYLIB_INIT_CODE = R"__(
#import core;
#import raylib;

main :: fn() {
  InitWindow(800, 600, "Hello, Raylib");
  SetTargetFPS(60);

  x_origin, y_origin := (330, 250);
  amplitude, speed, time := (15, 1.0, 0.0 as float64);
  x, y := (x_origin, y_origin);

  while !WindowShouldClose() {
    time += GetFrameTime() * speed;

    BeginDrawing();
      ClearBackground(BLACK);
      DrawCircle(400, 300, 100, RED);
      y = y_origin + sin(time) * amplitude;
      DrawText("Hello, Raylib", x, y, 24, WHITE);
    EndDrawing();
  }
}
)__";

constexpr auto MAIN_INIT_CODE = R"__(
#import core; // for println among many other common utilities.

main :: fn() {
  // 'Env::args()' returns a 'string[]' of the runtime's arguments.
  for arg in Env::args() {
    #static i: s32;
    println($"arg {i++}: {arg}")
  }
  println("Hello World!")
}

)__";