// shaders/lcd-gameboy-pocket.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// Neutral 4-shade Game Boy Pocket grayscale palette
const vec3 GBP_SHADE_0 = vec3(0.78, 0.81, 0.72); // Brightest (Silver/Off-White Background)
const vec3 GBP_SHADE_1 = vec3(0.55, 0.58, 0.52); // Light Gray
const vec3 GBP_SHADE_2 = vec3(0.31, 0.33, 0.31); // Dark Gray
const vec3 GBP_SHADE_3 = vec3(0.09, 0.10, 0.11); // Darkest (Near Black Pixels)

// Matrix grid darkness (0.0 = black gaps, 1.0 = solid)
const float screenDoorGrid = 0.85; 

void main() {
    vec2 texCoord = vertTexCoord.st;
    vec4 baseColor = texture2D(texture, texCoord);

    // 1. Standard NTSC Grayscale Conversion
    float luminance = dot(baseColor.rgb, vec3(0.299, 0.587, 0.114));

    // 2. Quantize into 4 Distinct Game Boy Pocket Gray Shades
    vec3 gbpColor;
    if (luminance > 0.75) {
        gbpColor = GBP_SHADE_0;
    } else if (luminance > 0.50) {
        gbpColor = GBP_SHADE_1;
    } else if (luminance > 0.25) {
        gbpColor = GBP_SHADE_2;
    } else {
        gbpColor = GBP_SHADE_3;
    }

    // 3. Screen Door Effect (LCD Grid)
    vec2 pixelPos = texCoord * vec2(160.0, 144.0);
    vec2 gridFactor = abs(sin(pixelPos * 3.14159265));
    float gridWidth = pow(gridFactor.x * gridFactor.y, 0.2); 
    float gridElement = mix(screenDoorGrid, 1.0, gridWidth);

    vec3 finalColor = gbpColor * gridElement;

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
