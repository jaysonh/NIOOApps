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
uniform float     iTimeline;

uniform float     soundInst;
uniform float     soundCount;
uniform float     soundScale;

uniform int OCTAVES ;

#ifdef GL_ES
precision mediump float;
#endif

#ifdef GL_OES_standard_derivatives
#extension GL_OES_standard_derivatives : enable
#endif

#define MAX_MARCHING_STEPS 55
#define MIN_DIST 0.0
#define MAX_DIST 75.0
#define EPSILON 0.1
#define numshapes 10

float hash(float n) { return fract(sin(n) * 1e4); }
float hash (in vec2 st) {
return fract(sin(dot(st.xy,
                        vec2(12.9898,78.233)))*
    43758.5453123);
}

// Based on Morgan McGuire @morgan3d
// https://www.shadertoy.com/view/4dS3Wd
// float noise (in vec2 st) {
//     vec2 i = floor(st);
//     vec2 f = fract(st);

//     // Four corners in 2D of a tile
//     float a = random(i);
//     float b = random(i + vec2(1.0, 0.0));
//     float c = random(i + vec2(0.0, 1.0));
//     float d = random(i + vec2(1.0, 1.0));

//     vec2 u = f * f * (3.0 - 2.0 * f);

//     return mix(a, b, u.x) +
//             (c - a)* u.y * (1.0 - u.x) +
//             (d - b) * u.x * u.y;
// }

// This one has non-ideal tiling properties that I'm still tuning
float noise(vec3 x) {
const vec3 step = vec3(110, 241, 171);

vec3 i = floor(x);
vec3 f = fract(x);

// For performance, compute the base input to a 1D hash from the integer part of the argument and the 
// incremental change to the 1D based on the 3D -> 1D wrapping
float n = dot(i, step);

vec3 u = f * f * (3.0 - 2.0 * f);
return mix(mix(mix( hash(n + dot(step, vec3(0, 0, 0))), hash(n + dot(step, vec3(1, 0, 0))), u.x),
            mix( hash(n + dot(step, vec3(0, 1, 0))), hash(n + dot(step, vec3(1, 1, 0))), u.x), u.y),
        mix(mix( hash(n + dot(step, vec3(0, 0, 1))), hash(n + dot(step, vec3(1, 0, 1))), u.x),
            mix( hash(n + dot(step, vec3(0, 1, 1))), hash(n + dot(step, vec3(1, 1, 1))), u.x), u.y), u.z);
}

vec4 noised( in vec3 x ){
vec3 p = floor(x);
vec3 w = fract(x);
vec3 u = w*w*(3.0-2.0*w);
vec3 du = 6.0*w*(1.0-w);

float n = p.x + p.y*157.0 + 113.0*p.z;

float a = hash(n+  0.0);
float b = hash(n+  1.0);
float c = hash(n+157.0);
float d = hash(n+158.0);
float e = hash(n+113.0);
float f = hash(n+114.0);
float g = hash(n+270.0);
float h = hash(n+271.0);

float k0 =   a;
float k1 =   b - a;
float k2 =   c - a;
float k3 =   e - a;
float k4 =   a - b - c + d;
float k5 =   a - c - e + g;
float k6 =   a - b - e + f;
float k7 = - a + b + c - d + e - f - g + h;

return vec4( k0 + k1*u.x + k2*u.y + k3*u.z + k4*u.x*u.y + k5*u.y*u.z + k6*u.z*u.x + k7*u.x*u.y*u.z, 
            du * (vec3(k1,k2,k3) + u.yzx*vec3(k4,k5,k6) + u.zxy*vec3(k6,k4,k5) + k7*u.yzx*u.zxy ));
}

float pow2(in float x) {
return x * x;
}

float pow4(in float x) {
return pow2(pow2(x));
}

float pow8(in float x) {
return pow4(pow2(x));
}

float fbm (in vec3 st) {

// Initial values
float value = 0.0;
float amplitude = .4;
float frequency = 0.0;
//
// Loop of octaves
for (int i = 0; i < OCTAVES; i++) {
    value += amplitude * noise(st);
    st *= 2.0;
    amplitude *= 0.6;
}
return value;
}

// --------------------------------------------------------
// IQ
// https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

//
// SDF from iq
// 

float sdSphere(vec3 p) {
return length(p) - 1.0;
}

float sdBox( vec3 p, vec3 b ) {
vec3 d = abs(p) - b;
return length(max(d,0.0))
        + min(max(d.x,max(d.y,d.z)),0.0); // remove this line for an only partially signed sdf 
}

float sdTorus( vec3 p, vec2 t ) {
vec2 q = vec2(length(p.xz)-t.x,p.y);
return length(q)-t.y;
}

float sdPlane( vec3 p, vec4 n ) {
// n must be normalized
return dot(p,n.xyz) + n.w;
}

float sdBlend(float d1, float d2, float a) {
return a * d1 + (1.0 - a) * d2;
}

float sdUnion(float distA, float distB) {
return min(distA, distB);
}

// rotation matrix x
mat3 rotateX(float theta) {
float c = cos(theta);
float s = sin(theta);
return mat3(
    vec3(1, 0, 0),
    vec3(0, c, -s),
    vec3(0, s, c)
);
}

// scale matrix
mat3 scale(float s) {
return mat3(
    vec3(s, 0, 0),
    vec3(0, s, 0),
    vec3(0, 0, s)
);
}

mat3 translate(float x, float y, float z) {
return mat3(
    vec3(x, 0, 0),
    vec3(0, y, 0),
    vec3(0, 0, z)
);
}

// rotation matrix y
mat3 rotateY(float theta) {
float c = cos(theta);
float s = sin(theta);
return mat3(
    vec3(c, 0, s),
    vec3(0, 1, 0),
    vec3(-s, 0, c)
);
}

// rotation matrix z
mat3 rotateZ(float theta) {
float c = cos(theta);
float s = sin(theta);
return mat3(
    vec3(c, -s, 0),
    vec3(s, c, 0),
    vec3(0, 0, 1)
);
}

float blob(float shapes[numshapes])
{
float k = 2.0;
float b = 0.0;

for (int i = 0; i < numshapes; i++) {
    b += exp(-k * shapes[i]);
}

return -log(b) / k;

//return -log(exp(-k*d1)+exp(-k*d2)+exp(-k*d3)+exp(-k*d4)+exp(-k*d5)+exp(-k*d6)+exp(-k*d7))/k;
}

// Signed distance function describing the scene.
// Absolute value of the return value indicates the distance to the surface.
// Sign indicates whether the point is inside or outside the surface,
// negative indicating inside.
float map(vec3 p) {
vec2 pos = (p.xy / iResolution.xy);
pos.y = 1.0-pos.y;
vec2 mouse = vec2(iMouse.x, iMouse.y) / iResolution.xy;
//mouse.x *= iResolution.x / iResolution.y;
vec2 dist = -0.5 + mouse / iResolution.xy - pos.xy;
dist.x *= iResolution.x / iResolution.y;
float t = iTime * 0.5;
float l = 10.0;//1.0 - length(dist) - 0.5;
//float d = pow8((l * 3.8) * fbm(p.xyz * 3.65 + t));
float d = pow4(fbm(0.5 * p.xyz * length(p)));
//d = smoothstep(0.0, 1.0, d);
float shapes[numshapes];

for (int i = 0; i < numshapes; i++) {
    float speed = hash(float(i+1) + 33.151154) * 26.0;
    float scale = hash(float(i+1) + 4123.4151);
    float f = mod(float(i), 2.0) + 1.0;
    vec3 m = vec3(
        -f + sin(iTime + float(i) * speed) * f + f * 0.5,
        f + cos(iTime + float(i) * speed) * f - f * 0.5,
        f + sin(iTime + float(i) * speed) * f - f * 0.5
    );

    if (i == 0) {
        shapes[i] = sdPlane(p, normalize(vec4(
            0.0,
            0.12,
            0.1,
            0.0
            ))
        );
    }
        
    if (i == 1) {
        vec3 torusPoint = rotateX(-iTime * 0.5) * p + 0.5;
        shapes[i] = sdBox(torusPoint - m / scale, vec3(0.5, 0.4, 0.5)) * scale;
    }
    
    if (i > 1) {
        //vec3 sph = (p - vec3(-10.0 + mouse.x * 10.0, -10.0 + mouse.y * 10.0, sin(iTime * 2.0) * 5.0)) * 0.35 + d; 
        vec3 sph = p - m  + d; 
        shapes[i] = sdSphere(sph / scale) * scale;
    }
    
}

// float shape;
// shape = sdSphere((p * 0.1) + d) + 0.6;

//if (u_shape == 0) {
    // p.y += -d * 4.0;
//}

//return shapes[0];
return blob(
    shapes
);
}

// raymaching
float intersect(vec3 ro, vec3 rd, float start, float end) {
float depth = start;
for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
    float dist = map(ro + depth * rd);
    if (dist < EPSILON) {
        return depth;
    }
    depth += dist;
    if (depth >= end) {
        return end;
    }
}
return end;
}
        

// ray direction
vec3 rayDirection(float fieldOfView, vec2 size, vec2 fragCoord) {
vec2 xy = fragCoord - size / 2.0;
float z = size.y / tan(radians(fieldOfView) / 2.0);
return normalize(vec3(xy, -z));
}

// calculate normal
vec3 calcNormal(vec3 pos) {
float t = 1.0;
vec3 eps = vec3( max(0.02,0.001*t),0.0,0.0);
return normalize( vec3(
    map(pos+eps.xyy) - map(pos-eps.xyy),
    map(pos+eps.yxy) - map(pos-eps.yxy),
    map(pos+eps.yyx) - map(pos-eps.yyx) ) );
}

// calculate AO
float calcAO( in vec3 pos, in vec3 nor ) {
float occ = 0.0;
float sca = 1.0;
for( int i=0; i<5; i++ ) {
    float hr = 0.01 + 0.12*float(i)/4.0;
    vec3 aopos = nor * hr + pos;
    float dd = map( aopos );
    occ += -(dd-hr)*sca;
    sca *= 0.95;
}
return clamp( 1.0 - 3.0 * occ, 0.0, 1.0 );    
}

// sem / matcap
vec2 matcap( vec3 ro, vec3 normal ) {
vec3 reflected = reflect(ro, normal);
float m = 2.8284271247461903 * sqrt( reflected.z + 1.0 );
return reflected.xy / m + 0.5;
}

// https://github.com/glslify/glsl-aastep/blob/master/index.glsl
float aastep(float threshold, float value) {
#ifdef GL_OES_standard_derivatives
    float afwidth = length(vec2(dFdx(value), dFdy(value))) * 0.70710678118654757;
    return smoothstep(threshold-afwidth, threshold+afwidth, value);
#else
    return step(threshold, value);
#endif
}


void main( void )
{
vec2 position = (gl_FragCoord.xy / iResolution.xy) - vec2(0.5);	
// ray direction 
vec3 rd = rayDirection(85.0, iResolution.xy, gl_FragCoord.xy);
// ray origin / eye
vec3 ro = vec3(-0.5, -0.5, 10.0);
// test intersection of ray with sdf scene
float dist = intersect(ro, rd, MIN_DIST, MAX_DIST);

// vignette
const float RADIUS = 0.85;
const float SOFTNESS = 0.25;
float len = length(position);
float vignette = 1.0;//smoothstep(RADIUS, RADIUS-SOFTNESS, len);

if (dist > MAX_DIST - EPSILON) {
    // Didn't hit anything
    //gl_FragColor = mix(vec4(vignette, 1.0), vec4(0.3, 0.3, 0.3, 1.0), 0.5);
    vec4 color = vec4(0.8, 0.8, 0.8, 1.0);
   	gl_FragColor = mix(color, color * vignette, 0.9);
    return;
}

// The closest point on the surface to the eyepoint along the view ray
vec3 p = ro + dist * rd;
vec3 normal = calcNormal(p);
vec3 K_a = (normal + vec3(1.0)) / 2.0; //* (length(p) - 12.0);

// mixed matcap / normal
vec3 color = K_a; //mix(K_a, texture(u_matcap, matcap(rd, normal)).bgr, 0.5);
//color = mix(spectrum(p.x), color, 0.5);
color = mix(color, color * vignette, 0.95);

// ambient occlusion
float ao = calcAO( p, normal );
color *= ao * 1.2;
vec3 inv = vec3(abs(iTimeline-color.r),color.g,abs(iTimeline-color.b));
gl_FragColor = vec4(inv, 1.0);
}