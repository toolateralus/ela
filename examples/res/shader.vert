#version 450 core

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
}