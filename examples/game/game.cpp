#include "game.hpp"
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
float randf() {
  return (float(rand()) / float(Random_Max));
};
bool str_starts_with(char* str, char* substr) {
  if (((str == nullptr) || (substr == nullptr))) {
    return false;
  };
  auto str_len = strlen(str);
  auto substr_len = strlen(substr);
  if ((str_len < substr_len)) {
    return false;
  };
  if (((str_len == substr_len) && (strcmp(str, substr) == 0))) {
    return true;
  };
  return (strncmp(str, substr, substr_len) == 0);
};
bool string_ends_with(char* str, char* pattern) {
  if (((!str) || (!pattern))) {
    return false;
  };
  auto len = strlen(str);
  auto patlen = strlen(pattern);
  if ((len < patlen)) {
    return false;
  };
  return (strncmp((str + (len - patlen)), pattern, patlen) == 0);
};
string string_substr(char* src, int start, int end) {
  if ((!src)) {
    return string(nullptr);
  };
  auto len = strlen(src);
  if ((start > end)) {
    return string(nullptr);
  };
  if ((len < (end - start))) {
    return string(nullptr);
  };
  char* dest  = (char*)malloc(((sizeof(char) * (end - start)) + 1));
  memcpy(dest, (src + start), (end - start));
  (dest[(end - start)] = "\0"[0]);
  auto v = string(dest);
  free(dest);
  return v;
};
void println(string str) {
  printf("%s\n", (char*)str);
};
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
u32 compile_shader(u32 kind, char* source) {
  u32 shader  = glCreateShader(kind);
  glShaderSource(shader, 1, (&source), nullptr);
  glCompileShader(shader);
  int success {};
  glGetShaderiv(shader, GL_COMPILE_STATUS, (&success));
  if ((success == 0)) {
    int length {};
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, (&length));
    char* info_log  = (char*)malloc((sizeof(char*) * length));
    glGetShaderInfoLog(shader, length, (&length), info_log);
    printf("Shader compilation failed: %s\n", info_log);
    free(info_log);
    return u32(0);
  };
  return shader;
};
u32 create_program(char* vert_source, char* frag_source) {
  u32 vertex_shader  = compile_shader(GL_VERTEX_SHADER, vert_source);
  u32 fragment_shader  = compile_shader(GL_FRAGMENT_SHADER, frag_source);
  if (((vertex_shader == 0) || (fragment_shader == 0))) {
    return u32(0);
  };
  u32 program  = glCreateProgram();
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);
  int success {};
  glGetProgramiv(program, GL_LINK_STATUS, (&success));
  if ((success == 0)) {
    int length {};
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, (&length));
    char* info_log  = (char*)malloc((sizeof(char*) * length));
    glGetProgramInfoLog(program, length, (&length), info_log);
    printf("Program linking failed: %s\n", info_log);
    free(info_log);
    return u32(0);
  };
  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);
  return program;
};
void check_error(char* location) {
  int err  = glGetError();
  if ((err == GL_INVALID_ENUM)) {
    printf("error: %s, from: %s\n", "GL.INVALID_ENUM", location);
  } else   if ((err == GL_INVALID_VALUE)) {
    printf("error: %s, from: %s\n", "GL.INVALID_VALUE", location);
  } else   if ((err == GL_INVALID_OPERATION)) {
    printf("error: %s, from: %s\n", "GL.INVALID_OPERATION", location);
  } else   if ((err == GL_STACK_OVERFLOW)) {
    printf("error: %s, from: %s\n", "GL.STACK_OVERFLOW", location);
  } else   if ((err == GL_STACK_UNDERFLOW)) {
    printf("error: %s, from: %s\n", "GL.STACK_UNDERFLOW", location);
  } else   if ((err == GL_OUT_OF_MEMORY)) {
    printf("error: %s, from: %s\n", "GL.OUT_OF_MEMORY", location);
  } else   if ((err != 0)) {
    printf("unknown gl error: %d, from: %s\n", err, location);
  };
};
union Vec4{
float values[4]{ float(), float(), float(), float()};
Vec4() {
  };
Vec4(float _x, float _y, float _z, float _w) {
    (x = _x);
    (y = _y);
    (z = _z);
    (w = _w);
  };
Vec4(float value) {
    (x = value);
    (y = value);
    (z = value);
    (w = value);
  };
Vec4 operator +(Vec4 other) {
    (other.x += x);
    (other.y += y);
    (other.z += z);
    (other.w += w);
    return other;
  };
Vec4 operator *(Vec4 other) {
    (other.x *= x);
    (other.y *= y);
    (other.z *= z);
    (other.w *= w);
    return other;
  };
Vec4 operator /(Vec4 other) {
    (other.x /= x);
    (other.y /= y);
    (other.z /= z);
    (other.w /= w);
    return other;
  };
Vec4 operator -(Vec4 other) {
    (other.x -= x);
    (other.y -= y);
    (other.z -= z);
    (other.w -= w);
    return other;
  };
float sqr_mag() {
    return ((((x * x) + (y * y)) + (z * z)) + (w * w));
  };
struct {
    float x ;
    float y ;
    float z ;
    float w ;
};
;
};
;
Vec4 Vec4_one() {
  return Vec4(1.0f);
};
Vec4 Vec4_zero() {
  return Vec4(0.0f);
};
union Vec3{
float values[3]{ float(), float(), float()};
Vec3(float value) {
    (x = value);
    (y = value);
    (z = value);
  };
Vec3() {
  };
Vec3(float _x, float _y, float _z) {
    (x = _x);
    (y = _y);
    (z = _z);
  };
Vec3 operator +(Vec3 other) {
    (other.x += x);
    (other.y += y);
    (other.z += z);
    return other;
  };
Vec3 operator *(Vec3 other) {
    (other.x *= x);
    (other.y *= y);
    (other.z *= z);
    return other;
  };
Vec3 operator /(Vec3 other) {
    (other.x /= x);
    (other.y /= y);
    (other.z /= z);
    return other;
  };
Vec3 operator -(Vec3 other) {
    (other.x -= x);
    (other.y -= y);
    (other.z -= z);
    return other;
  };
float sqr_mag() {
    return (((x * x) + (y * y)) + (z * z));
  };
struct {
    float x ;
    float y ;
    float z ;
};
;
};
;
Vec3 Vec3_one() {
  return Vec3(1.0f);
};
Vec3 Vec3_zero() {
  return Vec3(0.0f);
};
union Vec2{
float values[2]{ float(), float()};
Vec2() {
  };
Vec2(float _x, float _y) {
    (x = _x);
    (y = _y);
  };
Vec2(float value) {
    (x = value);
    (y = value);
  };
Vec2 operator +(Vec2 other) {
    (other.x += x);
    (other.y += y);
    return other;
  };
Vec2 operator *(Vec2 other) {
    (other.x *= x);
    (other.y *= y);
    return other;
  };
Vec2 operator /(Vec2 other) {
    (other.x /= x);
    (other.y /= y);
    return other;
  };
Vec2 operator -(Vec2 other) {
    (other.x -= x);
    (other.y -= y);
    return other;
  };
float sqr_mag() {
    return ((x * x) + (y * y));
  };
struct {
    float x ;
    float y ;
};
;
};
;
Vec2 Vec2_one() {
  return Vec2(1.0f);
};
Vec2 Vec2_zero() {
  return Vec2(0.0f);
};
union mat4{
float values[16]{ float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()};
struct {
    float M00 ;
    float M01 ;
    float M02 ;
    float M03 ;
    float M10 ;
    float M11 ;
    float M12 ;
    float M13 ;
    float M20 ;
    float M21 ;
    float M22 ;
    float M23 ;
    float M30 ;
    float M31 ;
    float M32 ;
    float M33 ;
};
;
};
;
mat4 mat4_identity() {
  mat4 matrix {};
  (matrix.M00 = 1.0f);
  (matrix.M01 = 0.0f);
  (matrix.M02 = 0.0f);
  (matrix.M03 = 0.0f);
  (matrix.M10 = 0.0f);
  (matrix.M11 = 1.0f);
  (matrix.M12 = 0.0f);
  (matrix.M13 = 0.0f);
  (matrix.M20 = 0.0f);
  (matrix.M21 = 0.0f);
  (matrix.M22 = 1.0f);
  (matrix.M23 = 0.0f);
  (matrix.M30 = 0.0f);
  (matrix.M31 = 0.0f);
  (matrix.M32 = 0.0f);
  (matrix.M33 = 1.0f);
  return matrix;
};
struct Shader{
  u32 handle {};
  void use() {
    glUseProgram(handle);
  };
  Shader(char* vertexSource, char* fragSource) {
    (handle = create_program(vertexSource, fragSource));
  };
  ~Shader() {
    glDeleteProgram(handle);
  };
  void set_mat4(char* name, mat4* v) {
    auto index = glGetUniformLocation(handle, name);
    if ((index != (-1))) {
      glUniformMatrix4fv(index, 1, false, (float*)v->values);
    } else  {
      printf("failed to get uniform : %s\n", name);
    };
  };
  void set_Vec4(char* name, Vec4* v) {
    auto index = glGetUniformLocation(handle, name);
    if ((index != (-1))) {
      glUniform4fv(index, 1, (float*)v->values);
    } else  {
      printf("failed to get uniform : %s\n", name);
    };
  };
  void set_Vec3(char* name, Vec3* v) {
    auto index = glGetUniformLocation(handle, name);
    if ((index != (-1))) {
      glUniform3fv(index, 1, (float*)v->values);
    } else  {
      printf("failed to get uniform : %s\n", name);
    };
  };
  void set_Vec2(char* name, Vec2* v) {
    auto index = glGetUniformLocation(handle, name);
    if ((index != (-1))) {
      glUniform2fv(index, 1, (float*)v->values);
    } else  {
      printf("failed to get uniform : %s\n", name);
    };
  };
  void set_float(char* name, float v) {
    auto index = glGetUniformLocation(handle, name);
    if ((index != (-1))) {
      glUniform1f(index, v);
    } else  {
      printf("failed to get uniform : %s\n", name);
    };
  };
};
;
struct Window{
  GLFWwindow* handle {};
  Window(int width, int height, char* title) {
    if ((!glfwInit())) {
      printf("Failed to initialize glfw\n");
      return;
    };
    (handle =(GLFWwindow*) glfwCreateWindow(width, height, title, nullptr, nullptr));
    if ((handle == nullptr)) {
      printf("Failed to create window\n");
      glfwTerminate();
      return;
    };
    glfwMakeContextCurrent(handle);
    if ((glewInit() != 0)) {
      printf("Failed to initialize glew\n");
      glfwTerminate();
      return;
    };
  };
  bool should_close() {
    return glfwWindowShouldClose(handle);
  };
  void present() {
    glfwSwapBuffers(handle);
    glfwPollEvents();
  };
  void clear() {
    glClear(GL_COLOR_BUFFER_BIT);
  };
};
;
auto intensity = 0.12f;
auto vertexCount = 36;
auto indexCount = 36;
auto WINDOW_WIDTH = 800;
auto WINDOW_HEIGHT = 600;
u32 ebo {};
u32 vbo {};
u32 vao {};
struct Vertex{
  Vec2 position {};
  Vec2 normal {};
  Vec2 uv {};
  Vertex() {
  };
  Vertex(Vec2 _position, Vec2 _normal, Vec2 _uv) {
    (position = _position);
    (normal = _normal);
    (uv = _uv);
  };
};
;
Window window  = Window(WINDOW_WIDTH, WINDOW_HEIGHT, "Xaryu's Banana");
Shader shader  = Shader(R"__(#version 450 core

layout(location = 0) in vec2 position;
layout(location = 1) in vec2 normal;
layout(location = 2) in vec2 uv;

layout(location = 0) out vec2 Normal;
layout(location = 1) out vec2 UV;

uniform mat4 viewProjectionMatrix;
uniform vec2 modelPosition;

void main() {
  UV = uv;
  Normal = normal;
  gl_Position = viewProjectionMatrix * vec4(modelPosition + position, 0, 1.0);
})__", R"__(#version 450 core
out vec4 color;

in vec2 Normal;
in vec2 UV;

void main() {
  color = vec4(Normal.xy, 1, 1.0);
})__");
Vertex vertices[6]{ Vertex(), Vertex(), Vertex(), Vertex(), Vertex(), Vertex()};
_array<Vec2> positions  = {Vec2((-1.0f), (-1.0f)), Vec2(1.0f, (-1.0f)), Vec2(1.0f, 1.0f), Vec2((-1.0f), 1.0f)};
Vec2 normal  = Vec2(0.0f, 0.0f);
_array<Vec2> uvs  = {Vec2(0.0f, 0.0f), Vec2(1.0f, 0.0f), Vec2(1.0f, 1.0f), Vec2(0.0f, 1.0f)};
_array<int> indices  = {0, 1, 2, 2, 3, 0};
void generate_quad_vertices() {
  auto len = 6;
  for (int i {}; (i < len); (++i)) {
    auto idx = indices[i];
    auto uv = uvs[idx];
    (vertices[i].position = positions[idx]);
    (vertices[i].normal = normal);
    (vertices[i].normal = uv);
  };
};
s8 run_game() {
  if ((window.handle == nullptr)) {
    return 1;
  };
  generate_quad_vertices();
  glClearColor(0.0f, intensity, intensity, 1.0f);
  glViewport(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
  glfwSwapInterval(1);
 {
    glGenBuffers(1, (&ebo));
    glGenVertexArrays(1, (&vao));
    glGenBuffers(1, (&vbo));
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    auto stride = sizeof(Vertex);
    glVertexAttribPointer(0, 2, GL_FLOAT, false, stride, (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 2, GL_FLOAT, false, stride, (void*)(sizeof(float) * 2));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(2, 2, GL_FLOAT, false, stride, (void*)(sizeof(float) * 4));
    glEnableVertexAttribArray(2);
    glBufferData(GL_ARRAY_BUFFER, (sizeof(Vertex) * vertexCount), (void*)vertices, GL_STATIC_DRAW);
  };
  shader.use();
  Vec2 modelPosition {};
  auto viewProjectionMatrix = mat4_identity();
  auto is_key_down = [&](int key) -> bool {
    auto state = glfwGetKey(window.handle, key);
    return ((state == KeyPress_Pressed) || (state == KeyPress_Repeating));
  };
  auto take_input = [&](Vec2* position) -> void {
    auto old_pos = (*position);
    Vec2 move_vec {};
    if (is_key_down(Key_W)) {
      (move_vec.y -= 0.1f);
    };
    if (is_key_down(Key_S)) {
      (move_vec.y += 0.1f);
    };
    if (is_key_down(Key_A)) {
      (move_vec.x += 0.1f);
    };
    if (is_key_down(Key_D)) {
      (move_vec.x -= 0.1f);
    };
    if ((move_vec.sqr_mag() != 0)) {
      ((*position) = ((*position) + move_vec));
    };
  };
  while ((!window.should_close())) {
    window.clear();
    if (is_key_down(Key_ESCAPE)) {
      break;
    };
    shader.set_mat4("viewProjectionMatrix", (&viewProjectionMatrix));
    shader.set_Vec2("modelPosition", (&modelPosition));
    take_input((&modelPosition));
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    window.present();
  };
  glfwTerminate();
  return 0;
};
int main() {
  auto matrix = mat4_identity();
  run_game();
};
