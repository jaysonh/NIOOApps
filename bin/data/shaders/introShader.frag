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

void main( void )
{
    vec2 uv = gl_FragCoord.xy / iResolution.xy - .5 -iTime*.05;
    vec3 c = cos(vec3(iTime*.006,
                  iTime*.025,
                  iTime*.0095))*2.+2.;
    for (int i = 0; i < 27; i++) {
        vec3 p = vec3(uv*float(i),float(i));
        c += vec3( sin(c.y+sin(p.x)),
                   cos(c.z+sin(p.z))+0.4,
                   -sin(c.x+sin(p.y)) );
        
    }
    gl_FragColor = vec4(c*c*.004,1.);
}


