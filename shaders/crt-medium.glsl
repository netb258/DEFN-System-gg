// resources/crt-medium.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// Balanced configuration variables for a medium CRT look
const float scanlineIntensity = 0.38; // Right between light (0.20) and heavy (0.55)
const float maskDark = 0.35;          // Right between light (0.25) and heavy (0.50)
const float gammaBoost = 1.22;        // Clean, balanced brightness compensation

void main() {
    vec2 texCoord = vertTexCoord.st;

    // Keep it crisp and sharp (no horizontal blur)
    vec4 baseColor = texture2D(texture, texCoord);

    // Balanced scanlines matching the 192p vertical resolution
    float pos = texCoord.y * 192.0; 
    float scanline = sin(pos * 3.14159265 * 2.0);
    scanline = mix(1.0, (scanline + 1.0) * 0.5, scanlineIntensity);

    // Present but non-intrusive vertical aperture grille structure
    float maskPos = vertTexCoord.x * 256.0 * 3.0; 
    float mask = 1.0 - maskDark * abs(sin(maskPos * 3.14159265));

    // Combine features cleanly
    vec3 finalColor = baseColor.rgb * scanline * mask;
    
    // Balanced exposure boost to keep colors punchy
    finalColor *= gammaBoost; 

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
