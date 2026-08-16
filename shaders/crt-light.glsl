// resources/crt-light.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// Configuration variables
const float scanlineIntensity = 0.20; // Lowered from 0.4 for much lighter scanlines
const float maskDark = 0.25;          // Lowered from 0.5 to make the phosphor grille brighter

void main() {
    // 1. Perfectly flat coordinates
    vec2 texCoord = vertTexCoord.st;

    // 2. REMOVED BLUR: Sample only the direct center pixel for raw sharpness
    vec4 baseColor = texture2D(texture, texCoord);

    // 3. Lighter Scanline Generation
    float pos = texCoord.y * 192.0; // Base SMS height resolution
    float scanline = sin(pos * 3.14159265 * 2.0);
    // Mixes between full brightness (1.0) and the scanline pattern
    scanline = mix(1.0, (scanline + 1.0) * 0.5, scanlineIntensity); 

    // 4. Lighter Shadow Mask / Aperture Grille simulation
    float maskPos = vertTexCoord.x * 256.0 * 3.0; // 3 phosphors per pixel
    float mask = 1.0 - maskDark * abs(sin(maskPos * 3.14159265));

    // 5. Combine features (Vignette completely removed for maximum brightness)
    vec3 finalColor = baseColor.rgb * scanline * mask;
    
    // 6. Boost overall exposure slightly to counteract CRT grid dimming
    finalColor *= 1.15; 

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
