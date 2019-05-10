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

uniform float     structure;

float EPSILON = 0.001;
vec2 twist = vec2(2.0,7.0);
float planesDistance = 0.3;
vec4 bumpMapParams1 = vec4(2.0,7.0,0.01,-0.01);
vec4 bumpMapParams2 = vec4(2.0,3.0,-0.01,0.01);
vec4 heightMapParams = vec4(3.0,1.0,0.0,0.01);
vec4 heightInfluence = vec4(-0.025,-0.05,0.8,1.8);
float fogDensity = 0.2;
float fogDistance = 0.1;
vec3 groundColor1 = vec3(0.2,0.3,0.3);
vec3 groundColor2 = vec3(0.4,0.8,0.4);
vec3 columnColors = vec3(0.9,0.3,0.3);
vec4 ambient = vec4(0.2,0.3,0.4,0.0);
vec3 lightColor = vec3(0.4,0.7,0.7);
vec4 fogColor = vec4(0.0,0.1,0.5,1.0);
vec3 rimColor = vec3(1.0,0.75,0.75);

float pi = 3.14159265359;

mat2 rot(float a) 
{
    vec2 s = sin(vec2(a, a + pi/2.0));
    return mat2(s.y,s.x,-s.x,s.y);
}

float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

float sphere(vec3 pos, float radius, vec3 scale)
{
    return length(pos*scale)-radius;
}
vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v)
  {
  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                     -0.577350269189626,  // -1.0 + 2.0 * C.x
                      0.024390243902439); // 1.0 / 41.0
// First corner
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);

// Other corners
  vec2 i1;
  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
  //i1.y = 1.0 - i1.x;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  // x0 = x0 - 0.0 + 0.0 * C.xx ;
  // x1 = x0 - i1 + 1.0 * C.xx ;
  // x2 = x0 - 1.0 + 2.0 * C.xx ;
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

// Permutations
  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
    + i.x + vec3(0.0, i1.x, 1.0 ));

  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;

// Gradients: 41 points uniformly over a line, mapped onto a diamond.
// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

// Normalise gradients implicitly by scaling m
// Approximation of: m *= inversesqrt( a0*a0 + h*h );
  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );

// Compute final noise value at P
  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

float heightmap(vec2 uv)
{
    return snoise(uv + iTime *heightMapParams.zw)* heightMapParams.y * 1.0;
    
    //return heightMapParams.x * texture( iChannel0, (uv + iTime * heightMapParams.zw ) * heightMapParams.y).x;
}

float bumpmap(vec2 uv)
{
    //float b1 = bumpMapParams1.x*(1.0 - texture2D(iChannel0, (uv + iTime*bumpMapParams1.zw)*bumpMapParams1.y).x);
    //float b2 = bumpMapParams2.x*(1.0-texture(iChannel0, (uv + iTime*bumpMapParams2.zw)*bumpMapParams2.x).x);
    
    float b1 = bumpMapParams1.x*(1.0 - snoise((uv
     + iTime*bumpMapParams1.zw )*bumpMapParams1.y));
    float b2 = bumpMapParams2.x*(1.0 - snoise((uv + iTime*bumpMapParams2.zw)*bumpMapParams2.y));
    return b1+b2;
}

float distfunc(vec3 pos)
{
    vec3 p2 = pos;
    p2.x += sin(p2.z*3.0 + p2.y*5.0)*0.15;
    p2.xy *= rot(floor(p2.z*2.0)*twist.y);
    pos.xy *= rot(pos.z*twist.x);
    
    float h = heightmap(pos.xz)*heightInfluence.x;
    
    vec3 columnsrep = vec3(0.75,1.0,0.5);
    vec3 reppos = (mod(p2 + vec3(iTime*0.01 + sin(pos.z*0.5),0.0,0.0),columnsrep)-0.5*columnsrep);
    
    float columnsScaleX = 1.0 + sin(p2.y*20.0*sin(p2.z) + iTime*5.0 + pos.z)*0.15;
    float columnsScaleY = (sin(iTime + pos.z*4.0)*0.5+0.5);
    
    float columns = sphere(vec3(reppos.x, pos.y+0.25, reppos.z), 0.035, vec3(columnsScaleX,columnsScaleY,columnsScaleX));
    float corridor = planesDistance - abs(pos.y)* structure + h; // multiplying this makes it crazy!
    float d = smin(corridor, columns, 0.25); 
           
    return d;
}

float rayMarch(vec3 rayDir, vec3 cameraOrigin)
{
    const int MAX_ITER = 50;
  const float MAX_DIST = 30.0;
    
    float totalDist = 0.0;
    float totalDist2 = 0.0;
  vec3 pos = cameraOrigin;
  float dist = EPSILON;
    vec3 col = vec3(0.0);
    float glow = soundInst;
    
    for(int j = 0; j < MAX_ITER; j++)
  {
    dist = distfunc(pos);
    totalDist = totalDist + dist;
    pos += dist*rayDir;
        
        if(dist < EPSILON || totalDist > MAX_DIST)
    {
      break;
    }
  }
    
    return totalDist  ;
}

//Taken from https://www.shadertoy.com/view/Xds3zN
mat3 setCamera( in vec3 ro, in vec3 ta, float cr )
{
  vec3 cw = normalize(ta-ro);
  vec3 cp = vec3(sin(cr), cos(cr),0.0);
  vec3 cu = normalize( cross(cw,cp) );
  vec3 cv = normalize( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

vec3 calculateNormals(vec3 pos)
{
  vec2 eps = vec2(0.0, EPSILON*1.0 );
  vec3 n = normalize(vec3(
  distfunc(pos + eps.yxx) - distfunc(pos - eps.yxx),
  distfunc(pos + eps.xyx) - distfunc(pos - eps.xyx),
  distfunc(pos + eps.xxy) - distfunc(pos - eps.xxy)));
    
  return n;
}

//Taken from https://www.shadertoy.com/view/XlXXWj
vec3 doBumpMap(vec2 uv, vec3 nor, float bumpfactor)
{
   
     float eps = 0.01 ;
    float ref = bumpmap(uv); 
    
    vec3 grad = vec3(bumpmap(vec2(uv.x-eps, uv.y))-ref, 0.0, bumpmap(vec2(uv.x, uv.y-eps))-ref); 
             
    grad -= nor*dot(nor, grad);          
                      
    return normalize( nor + grad*bumpfactor* soundInst );
}

void main( void )
{ 
    vec3 cameraOrigin = vec3(0.0, 0.0, iTime*-0.1);
    vec3 cameraTarget = cameraOrigin + vec3(0.0, 0.0, 1.0);;
    
  vec2 screenPos = (gl_FragCoord.xy/iResolution.xy)*2.0-1.0;
  screenPos.x *= iResolution.x/iResolution.y;
    
  mat3 cam = setCamera(cameraOrigin, cameraTarget, 0.0 );
    
    vec3 rayDir = cam* normalize( vec3(screenPos.xy,2.0) );
    rayDir.xy *= rot(iTime*0.1);
    float dist = rayMarch(rayDir, cameraOrigin);
   
    vec3 pos = cameraOrigin + dist*rayDir;
    vec2 uv = pos.xy * rot(pos.z*twist.x);
    float h = heightmap(vec2(uv.x, pos.z));
    vec3 n = calculateNormals(pos);
    vec3 bump = doBumpMap(vec2(uv.x, pos.z), n, 3.0);
    float m = smoothstep(-0.15,0.2, planesDistance - abs(uv.y) + h*heightInfluence.y + sin(iTime)*0.05);
    vec3 color = mix(mix(groundColor1, groundColor2, smoothstep(heightInfluence.z,heightInfluence.w,h)), columnColors, m);
    float fog = dist*fogDensity-fogDistance;
    float heightfog = pos.y;
    float rim = (1.0-max(0.0, dot(-normalize(rayDir), bump))) ;
    vec3 lightPos = pos - (cameraOrigin + vec3(0.0,0.0,1.0));
    vec3 lightDir = -normalize(lightPos);
    float lightdist = length(lightPos);
    float atten = 1.0 / (1.0 + lightdist*lightdist*3.0);
    float light = max(0.0, dot(lightDir, bump));
    vec3 r = reflect(normalize(rayDir), bump);
    float spec = clamp (dot (r, lightDir),0.0,1.0);
    float specpow = pow(spec,20.0);
    vec3 c = color*(ambient.xyz + mix(rim*rim*rim, rim*0.35+0.65, m)*rimColor + lightColor*(light*atten*2.0 + specpow*1.5));
    vec4 res = mix(vec4(c, rim), fogColor, clamp(fog+heightfog,0.0,1.0));

    
    gl_FragColor = res;
}

