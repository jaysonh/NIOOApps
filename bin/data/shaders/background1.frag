uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iTime;                 // shader playback time (in seconds)
uniform float     iTimeDelta;            // render time (in seconds)
uniform int       iFrame;                // shader playback frame
uniform float     iChannelTime[4];       // channel playback time (in seconds)
uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2DRect iChannel0;              // input channel. XX = 2D/Cube
uniform sampler2DRect iChannel1;
uniform sampler2DRect iChannel2;
uniform sampler2DRect iChannel3;
uniform vec4      iDate;                 // (year, month, day, time in seconds)
uniform float     iSampleRate;           // sound sample rate (i.e., 44100)
uniform float     iForm;
uniform vec3      iCol0;
uniform vec3      iCol1;


uniform float     soundInst;
uniform float     soundCount;
uniform float     soundScale;



float polygonDistance(vec2 p, float radius, float angleOffset, int sideCount) {
    float a = atan(p.x, p.y)+ angleOffset;
    float b = 6.28319 / float(sideCount);
    return cos(floor(.5 + a / b) * b - a) * length(p) - radius;
}

// from https://www.shadertoy.com/view/4djSRW
#define HASHSCALE1 443.8975
float hash11(float p) // assumes p in ~0-1 range
{
    vec3 p3  = fract(vec3(p) * HASHSCALE1);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

#define HASHSCALE3 vec3(.1031, .1030, .0973)
vec2 hash21(float p) // assumes p in larger integer range
{
    vec3 p3 = fract(vec3(p) * HASHSCALE3);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract(vec2((p3.x + p3.y)*p3.z, (p3.x+p3.z)*p3.y));
}

void main( void )
{
    vec2 uv = vec2(0.5) - (gl_FragCoord.xy / iResolution.xy);
    uv.x *= iResolution.x / iResolution.y;
    
    float accum = 0.;
    for(int i = 0; i < 83; i++) {
        float fi = float(i);
        float thisYOffset = mod(hash11(fi * 0.017) * (iTime + 19.) * 0.2, 4.0) - 2.0;
        vec2 center = (hash21(fi) * 2. - 1.) * vec2(1.1, 1.0) - vec2(0.0, thisYOffset);
        float radius = 0.5;
        vec2 offset = uv - center;
        float twistFactor = (hash11(fi * 0.0347) * 2. - 1.) * 1.9;
        float rotation = 0.1 + iTime * 0.2 + sin(iTime * 0.1) * 0.9 + (length(offset) / radius) * twistFactor;
        accum += pow(smoothstep(radius, 0.0, polygonDistance(uv - center, 0.1 + hash11(fi * 2.3) * 0.2, rotation, 5) + 0.1), 3.0);
    }
    
    vec3 subColor = vec3(0.4, 0.8, 0.2); //vec3(0.4, 0.2, 0.8);
    vec3 addColor = vec3(0.3, 0.2, 0.1);//vec3(0.3, 0.1, 0.2);
    
    gl_FragColor = vec4(vec3(1.0) - accum * subColor + addColor, 1.0);
}