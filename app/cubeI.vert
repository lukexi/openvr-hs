#version 330 core

uniform mat4 uProjectionView;

in      vec3 aPosition;
in      vec3 aNormal;
in      mat4 aInstanceTransform;
in      vec4 aInstanceColor;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec4 vColor;

void main() {
	mat4 model = transpose(aInstanceTransform);
    // Apply all matrix transformations to vert
    gl_Position = uProjectionView * model * vec4(aPosition, 1.0);
    
    // Pass some variables to the fragment shader
    vPosition = vec3(model * vec4(aPosition, 1.0));
    vNormal   = vec3(model * vec4(aNormal, 0.0));
    vColor    = aInstanceColor;
}
