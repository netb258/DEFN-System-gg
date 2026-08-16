// shaders/lcd-gamegear-subtle.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture; 
uniform sampler2D prevTexture; 

varying vec4 vertColor;
varying vec4 vertTexCoord;

// LCD Tuning Parameters (Adjusted for a very subtle look)
const float screenDoorGrid = 0.75;    // Increased from 0.25 (gaps are now very faint instead of dark)
const float subpixelIntensity = 0.12; // Reduced from 0.40 (RGB stripes are now barely noticeable)
const float responseTimeBlend = 0.20; // Reduced from 0.45 (much sharper movement, minimal ghosting)

void main() {
    vec2 texCoord = vertTexCoord.st;
    
    // 1. Core LCD Ghosting (Motion Blur)
    vec4 currentFrame = texture2D(texture, texCoord);
    vec4 previousFrame = texture2D(prevTexture, texCoord);
    vec3 blendedColor = mix(currentFrame.rgb, previousFrame.rgb, responseTimeBlend);

    // 2. Screen Door Effect (Horizontal and Vertical pixel gaps)
    vec2 pixelPos = texCoord * vec2(160.0, 144.0);
    vec2 gridFactor = abs(sin(pixelPos * 3.14159265));
    float gridWidth = pow(gridFactor.x * gridFactor.y, 0.15); 
    float gridElement = mix(screenDoorGrid, 1.0, gridWidth);

    // 3. Vertical RGB Subpixel Mask
    float subpixelX = texCoord.x * 160.0 * 3.0;
    int subpixelIndex = int(mod(subpixelX, 3.0));
    
    vec3 mask = vec3(1.0);
    
    if (subpixelIndex == 0) {
        mask = vec3(1.0, 1.0 - subpixelIntensity, 1.0 - subpixelIntensity); 
    } else if (subpixelIndex == 1) {
        mask = vec3(1.0 - subpixelIntensity, 1.0, 1.0 - subpixelIntensity); 
    } else {
        mask = vec3(1.0 - subpixelIntensity, 1.0 - subpixelIntensity, 1.0); 
    }

    // 4. Combine and Boost Exposure
    // Reduced the brightness boost from 1.35 to 1.10 because the mask elements are much brighter now
    vec3 finalColor = blendedColor * gridElement * mask * 1.10;

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
