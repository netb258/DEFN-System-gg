// shaders/crt-gamegear-subtle.glsl
#ifdef GL_ES
precision mediump float;
#endif

uniform sampler2D texture;
varying vec4 vertColor;
varying vec4 vertTexCoord;

// Намалени стойности за по-незабележим ефект
const float scanlineIntensity = 0.10; // Намалено от 0.20 за много по-бледи сканиращи линии
const float maskDark = 0.12;          // Намалено от 0.25 за по-светла и фина фосфорна решетка

void main() {
    vec2 texCoord = vertTexCoord.st;
    vec4 baseColor = texture2D(texture, texCoord);

    // Сканиращи линии за 144p
    float pos = texCoord.y * 144.0; 
    float scanline = sin(pos * 3.14159265 * 2.0);
    scanline = mix(1.0, (scanline + 1.0) * 0.5, scanlineIntensity); 

    // Апертурна решетка за 160p
    float maskPos = vertTexCoord.x * 160.0 * 3.0; 
    float mask = 1.0 - maskDark * abs(sin(maskPos * 3.14159265));

    // Комбиниране на ефектите
    vec3 finalColor = baseColor.rgb * scanline * mask;
    
    // Намален лек баланс на експозицията, тъй като решетката вече е по-ярка
    finalColor *= 1.08; 

    gl_FragColor = vec4(finalColor, 1.0) * vertColor;
}
