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
uniform float     soundScale;
uniform float     soundCount;

void main( void )
{
    vec2 uv = gl_FragCoord.xy / iResolution.xy - .5 -iTime*.01;
    vec3 c = cos(vec3(soundCount*0.1*.06,
                      soundCount*0.1*.45,
                      soundCount*0.1*.015))*2.+2.;
    for (int i = 0; i < 27; i++) {
        vec3 p = vec3(uv*float(i),float(i));
        //      c += abs( cos( ( c + sin( p )).yzx ) );
        c += abs( vec3( cos(c.y+sin(p.x)),
                       cos(c.z+sin(p.z)),
                       -cos(c.x+sin(p.y)) ) );
    }
    gl_FragColor = vec4((c*.04-.66)*3.,1.);
}
