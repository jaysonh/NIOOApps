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

// Found this on GLSL sandbox. I really liked it, changed a few things and made it tileable.
// :)
// by David Hoskins.


// Water turbulence effect by joltz0r 2013-07-04, improved 2013-07-07


// Redefine below to see the tiling...
#define SHOW_TILING

#define TAU 6.28318530718
#define MAX_ITER 5

void main( void )
{
    float time = iTime*0.1 * .5+23.0;
    // uv should be the 0-1 uv of texture...
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    
#ifdef SHOW_TILING
    vec2 p = mod(uv*TAU*2.0, TAU)-250.0;
#else
    vec2 p = mod(uv*TAU, TAU)-250.0;
#endif
    vec2 i = vec2(p);
    float c = 1.0;
    float inten = .005;
    
    for (int n = 0; n < MAX_ITER; n++)
    {
        float t = /*time * */(1.0 - (3.5 / float(n+1)))* soundCount*0.04;
        i = p + vec2(cos(t - i.x) + sin(t + i.y), sin(t - i.y) + cos(t + i.x));
        c += 1.0/length(vec2(p.x / (sin(i.x+t)/inten),p.y / (cos(i.y+t)/inten)));
    }
    c /= float(MAX_ITER);
    c = 1.17-pow(c, 1.4) ;
    vec3 colour = vec3(pow(abs(c), 8.0));
    colour = clamp(colour + vec3(0.0, 0.55, 0.05), 0.0, 1.0);
    
    

    gl_FragColor = vec4(colour, 1.0);
}
