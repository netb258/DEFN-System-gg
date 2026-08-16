// resources/crt-heavy.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// Configuration variables for a heavy retro look
const float scanlineIntensity = 0.55; // High intensity for dark, thick gaps
const float maskDark = 0.50;          // Deep vertical shadow mask lines
const float gammaBoost = 1.35;        // Heavy boost to prevent the dark lines from dimming the screen

void main() {
    vec2 texCoord = vertTexCoord.st;

    // Keep it crisp and sharp (no horizontal blur)
    vec4 baseColor = texture2D(texture, texCoord);

    // Deep, heavy scanlines matching the 192p vertical resolution
    float pos = texCoord.y * 192.0; 
    float scanline = sin(pos * 3.14159265 * 2.0);
    scanline = mix(1.0, (scanline + 1.0) * 0.5, scanlineIntensity);

    // Pronounced vertical aperture grille structure
    float maskPos = vertTexCoord.x * 256.0 * 3.0; // 3 subpixels per original SMS pixel
    float mask = 1.0 - maskDark * abs(sin(maskPos * 3.14159265));

    // Combine the crisp textures with the heavy CRT structures
    vec3 finalColor = baseColor.rgb * scanline * mask;
    
    // Apply heavy gamma/brightness compensation so the game elements still pop
    finalColor *= gammaBoost; 

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
