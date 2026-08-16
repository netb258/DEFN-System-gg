// resources/crt.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// --- Visual Tweak Parameters (Optimized for Maximum Brightness) ---
const float WARP_X = 0.04;           // Horizontal screen curvature intensity
const float WARP_Y = 0.05;           // Vertical screen curvature intensity
const float scanlineIntensity = 0.20;// Lighter scanlines to prevent overall dimming
const float maskDark = 0.25;          // Brighter phosphor mask layout
const float BLOOM_SHARP = 0.0015;    // Controls horizontal color bleeding / blur

// 3D Glass Tube Curvature Math
vec2 warp(vec2 uv) {
    uv = uv * 2.0 - 1.0;
    uv.x *= 1.0 + (uv.y * uv.y) * WARP_X;
    uv.y *= 1.0 + (uv.x * uv.x) * WARP_Y;
    return uv * 0.5 + 0.5;
}

void main() {
    // 1. Apply Screen Curvature Geometry
    vec2 texCoord = warp(vertTexCoord.st);

    // Hard cutoff to create black outer bezels around the curved screen edges
    if (texCoord.x < 0.0 || texCoord.x > 1.0 || texCoord.y < 0.0 || texCoord.y > 1.0) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }

    // 2. Analog Signal Blur & Chromatic Aberration
    vec4 baseColor;
    baseColor.r = texture2D(texture, vec2(texCoord.x - BLOOM_SHARP, texCoord.y)).r;
    baseColor.g = texture2D(texture, texCoord).g;
    baseColor.b = texture2D(texture, vec2(texCoord.x + BLOOM_SHARP, texCoord.y)).b;
    baseColor.a = 1.0;

    // Mix in a slight vertical blur sample to eliminate digital raw sharpness
    vec4 blurColor = texture2D(texture, vec2(texCoord.x, texCoord.y + 0.001));
    baseColor = mix(baseColor, blurColor, 0.25);

    // 3. Bright Scanline Generation (Restored your light mixing logic)
    float pos = texCoord.y * 192.0; 
    float scanline = sin(pos * 3.14159265 * 2.0);
    scanline = mix(1.0, (scanline + 1.0) * 0.5, scanlineIntensity); 

    // 4. Bright Aperture Grille Phosphor Mask Simulation
    float maskPos = texCoord.x * 256.0 * 3.0; 
    float mask = 1.0 - maskDark * abs(sin(maskPos * 3.14159265));

    // 5. Combine Features (VIGNETTE REMOVED completely to stop corner dimming)
    vec3 finalColor = baseColor.rgb * scanline * mask;
    
    // 6. Balanced Exposure Boost to ensure zero loss of peak brightness
    finalColor *= 1.30; 

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
