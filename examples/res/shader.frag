#version 450 core
out vec4 color;

in vec2 Normal;
in vec2 UV;

void main() {
  color = vec4(Normal.xy, 1, 1.0);
}