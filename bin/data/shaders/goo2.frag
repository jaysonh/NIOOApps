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

#define AA 1
#define DROP_COUNT 5

float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

float sdSphere(vec3 p, float s){
    return length(p)-s ;
}

float lolRand(float seed){
    return sin(seed*1000.)*0.5+0.5;
}



// ============== noise stuff
// src: https://www.shadertoy.com/view/Xtl3W2



vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0; }

float mod289(float x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0; }

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

float permute(float x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
  return 1.79284291400159 - 0.85373472095314 * r;
}

float taylorInvSqrt(float r)
{
  return 1.79284291400159 - 0.85373472095314 * r;
}

vec4 grad4(float j, vec4 ip)
{
  const vec4 ones = vec4(1.0, 1.0, 1.0, -1.0);
  vec4 p,s;

  p.xyz = floor( fract (vec3(j) * ip.xyz) * 7.0) * ip.z - 1.0;
  p.w = 1.5 - dot(abs(p.xyz), ones.xyz);
  s = vec4(lessThan(p, vec4(0.0)));
  p.xyz = p.xyz + (s.xyz*2.0 - 1.0) * s.www;

  return p;
  }

// (sqrt(5) - 1)/4 = F4, used once below
#define F4 0.309016994374947451

float snoise(vec4 v)
  {
  const vec4  C = vec4( 0.138196601125011,  // (5 - sqrt(5))/20  G4
                        0.276393202250021,  // 2 * G4
                        0.414589803375032,  // 3 * G4
                       -0.447213595499958); // -1 + 4 * G4

// First corner
  vec4 i  = floor(v + dot(v, vec4(F4)) );
  vec4 x0 = v -   i + dot(i, C.xxxx);

// Other corners

// Rank sorting originally contributed by Bill Licea-Kane, AMD (formerly ATI)
  vec4 i0;
  vec3 isX = step( x0.yzw, x0.xxx );
  vec3 isYZ = step( x0.zww, x0.yyz );
//  i0.x = dot( isX, vec3( 1.0 ) );
  i0.x = isX.x + isX.y + isX.z;
  i0.yzw = 1.0 - isX;
//  i0.y += dot( isYZ.xy, vec2( 1.0 ) );
  i0.y += isYZ.x + isYZ.y;
  i0.zw += 1.0 - isYZ.xy;
  i0.z += isYZ.z;
  i0.w += 1.0 - isYZ.z;

  // i0 now contains the unique values 0,1,2,3 in each channel
  vec4 i3 = clamp( i0, 0.0, 1.0 );
  vec4 i2 = clamp( i0-1.0, 0.0, 1.0 );
  vec4 i1 = clamp( i0-2.0, 0.0, 1.0 );

  //  x0 = x0 - 0.0 + 0.0 * C.xxxx
  //  x1 = x0 - i1  + 1.0 * C.xxxx
  //  x2 = x0 - i2  + 2.0 * C.xxxx
  //  x3 = x0 - i3  + 3.0 * C.xxxx
  //  x4 = x0 - 1.0 + 4.0 * C.xxxx
  vec4 x1 = x0 - i1 + C.xxxx;
  vec4 x2 = x0 - i2 + C.yyyy;
  vec4 x3 = x0 - i3 + C.zzzz;
  vec4 x4 = x0 + C.wwww;

// Permutations
  i = mod289(i);
  float j0 = permute( permute( permute( permute(i.w) + i.z) + i.y) + i.x);
  vec4 j1 = permute( permute( permute( permute (
             i.w + vec4(i1.w, i2.w, i3.w, 1.0 ))
           + i.z + vec4(i1.z, i2.z, i3.z, 1.0 ))
           + i.y + vec4(i1.y, i2.y, i3.y, 1.0 ))
           + i.x + vec4(i1.x, i2.x, i3.x, 1.0 ));

// Gradients: 7x7x6 points over a cube, mapped onto a 4-cross polytope
// 7*7*6 = 294, which is close to the ring size 17*17 = 289.
  vec4 ip = vec4(1.0/294.0, 1.0/49.0, 1.0/7.0, 0.0) ;

  vec4 p0 = grad4(j0,   ip);
  vec4 p1 = grad4(j1.x, ip);
  vec4 p2 = grad4(j1.y, ip);
  vec4 p3 = grad4(j1.z, ip);
  vec4 p4 = grad4(j1.w, ip);

// Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;
  p4 *= taylorInvSqrt(dot(p4,p4));

// Mix contributions from the five corners
  vec3 m0 = max(0.6 - vec3(dot(x0,x0), dot(x1,x1), dot(x2,x2)), 0.0);
  vec2 m1 = max(0.6 - vec2(dot(x3,x3), dot(x4,x4)            ), 0.0);
  m0 = m0 * m0;
  m1 = m1 * m1;
  return 49.0 * ( dot(m0*m0, vec3( dot( p0, x0 ), dot( p1, x1 ), dot( p2, x2 )))
               + dot(m1*m1, vec2( dot( p3, x3 ), dot( p4, x4 ) ) ) ) ;

}

// ======================



float drop(vec3 a, vec3 b, float ra, float rb, float s){
    vec3 ba = b - a;
    float h = dot(-a,ba)/dot(ba,ba);
    float d = (log2(1.44269/s))/s;
    float start = pow(2.,d*s);
    float minf = pow(2.,(h+d-start)*s);
    float maxf = 1.-pow(2.,(-h+1.+d-start)*s);
    if(h < start){
        h = minf;
    }
    if(h > 1.-start){
        h = maxf;
    }
    h = clamp(h, 0., 1.);//* soundScale;
    return length( -a - ba*h ) - mix(ra, rb, h);
}

float map(vec3 pos){
    float sphereNoiseAmount = clamp(1.-pos.y,0.,1.)*0.05* soundScale*50.0;
    float sphereNoise = snoise(vec4(pos.x, pos.y+iTime*0.5, pos.z, 1.))*sphereNoiseAmount;
    float sph = sdSphere(pos+sphereNoise, 1.);
    float floorSph = sdSphere(pos * vec3(0.5, 3., 0.5) + vec3(0., 7.7, 0.), 1.);
    float res = min(sph, floorSph);
    
    //drops
    for(int i=0; i<DROP_COUNT; i++){
        float seed = float(i)*10.;
        float t = iTime+lolRand(seed*3.324)*5.;
        const float dropDuration = 2.8; // Adjust this to make drops different
        seed += floor(t/dropDuration);
        t= mod(t,dropDuration);
        t += 1.;
        float dropRot = lolRand(seed)*6.5;
        float dropDist = pow(-lolRand(seed*2.34)+1.,1.8) * 0.8;
        vec3 dropPos = pos + vec3(cos(dropRot)*dropDist, 0., sin(dropRot)*dropDist);
        float size = 0.1-dropDist*0.5;
        size = 0.96-dropDist*0.04; // adjust 0.96 to make blobs different

        float y1, y2, s1, s2;
        //drop pos
        if(t < 1.756){
            y1 = 0.;
        }else if(t < 2.9){
            y1 = pow(t-1.756,4.);
        }else{
            y1 = -pow(100.,-t+2.82)+2.4;
        }
        y2 = pow(min(t,2.08)*0.6,4.);
        y2 += 0.08;
        
        //drop size
        s1 = -pow((t/5.)-0.5,2.)+0.2;
        s2 = -pow(2.,-t-0.8)+0.6;
        s1 *= size*0.5;
        s2 *= size* soundScale*5.0;
        s1 = max(s1, 0.);
        s2 = max(s2, 0.);
        float dropd = drop(dropPos+ vec3(0., y1, 0.), dropPos+ vec3(0., y2, 0.), s1, s2, 5.);
        res = smin(res, dropd, 0.2);
    }
    return res;
}

const int MAX_RAY_STEPS = 64;
const float HIT_PRECISION = 0.009;
const float FAR_CLIPPING = 10.;

float castRay(in vec3 ro, in vec3 rd){
    float h = HIT_PRECISION * 2.;
    float t = 0.;

    for(int i=0; i<MAX_RAY_STEPS; i++){
        h = map(ro+rd*t);
        if(h < HIT_PRECISION || t > FAR_CLIPPING) break;
        t += h;
    }

    float res = -1.;
    if(t <= FAR_CLIPPING) res = t;
    return res;
}

vec3 getNormal(vec3 p){
    vec2 e = vec2(.0001, 0);
    return normalize(vec3(
        map(p + e.xyy) - map(p - e.xyy),
        map(p + e.yxy) - map(p - e.yxy),
        map(p + e.yyx) - map(p - e.yyx)
    ));
}

vec4 render( in vec3 ro, in vec3 rd){
    vec3 light = vec3(7., 11., -1.);

    float z = castRay(ro, rd);
    //return vec4(matInfo);
    //return vec4(z/10.);
    if(z == -1.){ // BG
        vec3 bgDir = rd;
        bgDir.y *= -1.;
        return texture2DRect(iChannel0, bgDir.xy);
    }
    vec3 pos = ro + rd * z;
    vec3 normal = getNormal(pos);
    
    //see through amount
    float seeThrough = clamp(3.-abs(pos.y*4.2+6.5),0.,1.);
    //return vec4(seeThrough);
    
    //relative normal 
    float relNorm = dot(-rd, normal);
    
    //soft reflection / refraction
    float reflectSoftMid = (snoise(vec4(pos*vec3(30., 1., 30.)+vec3(0.,1.,0.)*iTime, 0.))+0.3)*3.*seeThrough;
    //float floorReflSoftDist = distance(vec2(0.),pos.xz+0.4*snoise(vec4(pos.xz*7.,0.,0.)));
    //float floorReflSoft = 0.3*snoise(vec4(floorReflSoftDist*3.-iTime*0.1));
    float floorReflSoftDisp = 0.3*snoise(vec4(pos.xz*7.,iTime*0.1,0.));
    float floorReflSoftDisp2 = 1.*snoise(vec4(pos.xz*0.5,1.,0.));
    float floorReflSoftDist = distance(vec2(0.),pos.xz+floorReflSoftDisp+floorReflSoftDisp2);
    float floorReflSoft = sin(floorReflSoftDist*10.-iTime)*3.;
    floorReflSoft *= clamp(-10.3-pos.y*5.,0.,1.);
    float reflSoftTop = snoise(vec4(pos*10.+iTime*vec3(0.,3.,0.),0.))*10.+0.5;
    reflSoftTop *= clamp(pos.y*5.+5.,0.,1.);
    float reflectSoftAmount = clamp(reflSoftTop + reflectSoftMid + floorReflSoft, 0., 1.);
    //return vec4(floorReflSoft);
    
    //refraction
    vec3 refrDir = normalize(refract(rd, normal, 0.8));
    vec3 refrColHard = texture2DRect(iChannel0, refrDir.xy).rgb;
    vec3 refrColSoft = texture2DRect(iChannel1, refrDir.xy).rgb;
    vec3 refrCol = mix(refrColHard, refrColSoft, reflectSoftAmount);
    
    //reflection
    vec3 reflColHard = texture2DRect(iChannel0, normalize(reflect(rd, normal)).xy).rgb;
    vec3 reflColSoft = texture2DRect(iChannel1, normalize(reflect(rd, normal)).xy).rgb;
    float reflAmount = pow(1. - relNorm,4.-seeThrough*3.);
    reflAmount = clamp(reflAmount, 0., 1.);
    vec3 reflCol = mix(reflColHard, reflColSoft, reflectSoftAmount);
    //return vec4(reflAmount);
    //return vec4(reflCol,1.);
    
    //color DROP_COUNT
    //vec3 absorbCol = mix(vec3(0., 1., 0.), vec3(0., 0.2, 0.), relNorm);
    float m = clamp(soundScale * 2.0,0.0,1.0);
    
    vec3 absorbCol = mix(vec3(m, 1.-m, m), vec3(0., 0.6, 0.), relNorm);
    absorbCol = mix(vec3(1.), absorbCol, min(relNorm*3., 1.));
    float absorbAmount = min(relNorm*2.5,1.) - seeThrough*0.5*soundScale;
    //return vec4(absorbAmount);

    //vec4 col = vec4(normal, 1.);
    vec3 col = mix(refrCol, absorbCol, absorbAmount);
    col = mix(col, reflCol, reflAmount);
    return vec4(col, 1.);
}

mat3 setCamera( in vec3 ro, in vec3 ta, float cr ){
    vec3 cw = normalize(ta-ro);
    vec3 cp = vec3(sin(cr), cos(cr),0.0);
    vec3 cu = normalize( cross(cw,cp) );
    vec3 cv = normalize( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

const float CAM_DIST = 4.;

void main( void )
{
    float camRot = iMouse.x*.01;
    float camHeight = -iMouse.y*.01;
    
    camRot = 15.0*0.2;
    camHeight = -1.2;
    vec3 camPos = vec3(cos(camRot)*CAM_DIST, camHeight, sin(camRot)*CAM_DIST);
    
    vec3 camTarget = vec3(4., -1., 0.);
    mat3 camMat = setCamera(camPos, camTarget, 0.);
    
    vec2 uv = ( -iResolution.xy + 2.0 * gl_FragCoord.xy) / iResolution.y;
    vec3 rayDir = normalize(camMat * vec3(uv.xy,2.));
    gl_FragColor = render(camPos, rayDir);
    
}
