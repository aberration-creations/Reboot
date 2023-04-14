#version 450

layout(location = 0) in vec2 a_pos;
layout(location = 1) in vec3 a_color;

layout(location = 0) out vec3 v_color;

layout (binding = 0) uniform UniformBufferObject {
    uint width;
    uint height;
	float time;
} ubo;

void main() {
    float angle = ubo.time + sin(ubo.time) * 10;
    float c = cos(angle);
    float s = sin(angle);
    mat2 rotation = mat2(
        c, s,
        -s, c
    );
    float minSize = min(ubo.width, ubo.height);
    vec2 aspectFix = vec2(minSize/float(ubo.width), minSize/float(ubo.height));
    //gl_Position = vec4(a_pos * rotation * aspectFix * scale, 0.0, 1.0);
    
    // fullscreen triangle
    float scale = 1000.0; // make it big!
    gl_Position = vec4(a_pos * rotation * aspectFix * scale, 0.0, 1.0);

    v_color = a_color;

    //fix for upside down coordinate system
    gl_Position.y = -gl_Position.y;
}
