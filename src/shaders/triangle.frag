#version 450

layout(location = 0) in vec3 v_color;

layout(location = 0) out vec4 f_color;

layout (binding = 0) uniform UniformBufferObject {
    uint width;
    uint height;
	float time;
} ubo;

// COMMON /////////////////////////////////////////////////////////////////////

#define _SONG_BPM 123.0
#define fGlobalTime (ubo.time/120.0*_SONG_BPM)
#define out_texcoord vec2(gl_FragCoord.xy/vec2(ubo.width,ubo.height)*vec2(1,-1)+vec2(0,1))
#define v2Resolution vec2(ubo.width, ubo.height)
#define ss smoothstep
#define mid(A,B,C) max(A,min(B,C))
float ST=0;
float PT=0;

mat2 rot(float a){
  float c=cos(a), s=sin(a);
  return mat2(c,s,-s,c);
}

// namespace bt (balltunnel) //////////////////////////////////////////////////

float bt_g_time;
mat2 bt_g_rot1;
float bt_g_osc1;
float bt_g_osc2;

float bt_df(vec3 pos) {
  vec3 p3=pos-vec3(0,0,bt_g_time);
  float tun = 4.0-length(p3.xy);
  tun = min(tun, max(abs(fract((p3.xz*rot(2)).x)-0.5)-0.1, tun-0.2));
  tun = min(tun, max(abs(fract((p3.xz*rot(3)).x)-0.5)-0.1, tun-0.2));
  tun = min(tun, max(abs(fract((p3.yz*rot(4)).x)-0.5)-0.1, tun-0.2));
  pos.xy*=bt_g_rot1;
  pos.yz*=bt_g_rot1;
  vec3 p2 = abs(pos)-1.0+bt_g_osc1*0.5;
  float balls = length(p2)-1.0;
  return min(balls, tun);
}

vec3 bt_nf(vec3 pos, float d) {
  vec2 e = vec2(0,d);
  float c = bt_df(pos-e.yyy);
  return normalize(vec3(
    bt_df(pos+e.yxx)-bt_df(pos-e.yxx),
    bt_df(pos+e.xyx)-bt_df(pos-e.xyx),
    bt_df(pos+e.xxy)-bt_df(pos-e.xxy)
  ));
}

void bt_updateGlobals(){
  bt_g_time = fGlobalTime;
  bt_g_rot1 = rot(bt_g_time);
  bt_g_osc1 = sin(bt_g_time*3.14159);
  bt_g_osc1 = cos(bt_g_time*20)*smoothstep(0.5,1.0, bt_g_osc1);
}

vec3 bt_mat_em(vec3 pos1, vec3 norm1){
  
  #define PAT(A) smoothstep(.5,.9,sin(pos1.z+bt_g_time+bt_df(pos1*vec3(-1.5,-1.5,1))*A))
  #define len length
  float em_pat = PAT(8)*PAT(64)*max(0,dot(norm1, normalize(vec3(-pos1.xy,0))))*smoothstep(0,1,len(pos1.xz));
  vec3 color = mix(vec3(0.1,.4,1.2), vec3(0.9,.3,.1), sin(pos1.z+bt_g_time*2.0)) * em_pat * 8.0;
  return max(vec3(.0), color*1.0);
  #undef PAT
  #undef len
}
  
  
vec3 bt_main(void)
{
  bt_updateGlobals();
	vec2 uv = out_texcoord;
	uv -= 0.5;
  uv += fract(-uv.x*43431.5363+uv.xy*uv.xy*43254.33+uv.yx*uv.yx*43433.9+fract(bt_g_time*34.34))/v2Resolution.xy*2.0+cos(bt_g_time*.125)*.125;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
 
  float rt_T = bt_g_time;
  uv.x+=(uv.y-mod(uv.y+rt_T,0.1))*pow(ss(0.05,0,mod(PT,1.0)),4.0);
  uv.y+=(uv.x-mod(uv.x+rt_T,0.1))*pow(ss(0.05,0,mod(PT+0.5,1.0)),4.0);
  float tiltt = 2*rt_T - mod(rt_T, 3) - mod(rt_T, 4);
  uv.xy+=vec2(sin(tiltt),cos(tiltt*9.42))*1.0*ss(1,0,PT);
  
  vec3 pos0 = vec3(0,0,-8);
  vec3 dir0 = normalize(vec3(uv.xy,+1.0));
  float d0t1 = 0.0;
  
  for (int i=0; i<100; i+=1){
    float d = bt_df(pos0 + dir0 * d0t1);
    d0t1 += d;
  }
  
  
  vec3 pos1 = pos0 + dir0*d0t1;
  vec3 norm1 = bt_nf(pos1, 0.001);
  
  float d12 = 0.0;
  vec3 dir12 = normalize(reflect(dir0,norm1));
  
  for (int i=0; i<100; i+=1)
  {
    float d = bt_df(pos1 + dir12*d12);
    d12 += d;
  }
  
  vec3 pos2 = pos1 + dir12*d12;
  vec3 norm2 = bt_nf(pos2, 0.01);
  
  float fres = min(1.0,max(0.0,pow(1.0+dot(dir0, norm1),1.0)));
  #define AOS(A) bt_df(pos1+norm1/A)*A
  float ao = smoothstep(-4.5,4.5,AOS(0.25)*4+AOS(0.5)+AOS(1)+AOS(3)+AOS(9)+AOS(24));
  vec3 em1 = bt_mat_em(pos1, norm1);
  vec3 em2 = bt_mat_em(pos2, norm2);
  vec3 color = vec3((fres+.5)*ao)*.2+em1*ao;
  vec3 color2 = mix(em2, vec3(.3,.4,.5), 1.0-1.0/(1.0+d12*0.1));
  color = mix(color, color2, fres*0.8+0.2);
  color = mix(color, vec3(.3,.4,.5), 1.0-1.0/(1.0+d0t1*0.1));
  
  float wb=dot(color, vec3(0.3,0.5,0.2));
  color = max(vec3(0),mix(color, vec3(wb), -0.5));

  color = max(vec3(.0),color + vec3(- 0.2)) * 2.0;
  color *=mix(vec3(4.0,1.5,2),vec3(16.0,0.4,3),ss(0,1,PT));
  color = 4.0*color / (1.0 + color);
  
  return color * 0.5;
  #undef AOS
}

// namespace br (broccoli) ////////////////////////////////////////////////////

float br_T;
float br_FT;
vec3 br_H;
mat2 br_GROT;
vec2 br_TILT;

void br_globals(){
  br_T=fGlobalTime;
  br_FT=br_T-mod(br_T,2)+br_T-mod(br_T,5);
  br_GROT=rot(br_T/4+sin(br_T)/2.0);
  br_TILT=sin(vec2(br_FT*6,br_FT*8+1.7))*vec2(.2,.05);
  br_H=vec3(sin(br_T),sin(br_FT),cos(br_T/4.0));
}

float br_df(vec3 p){
  p.xz=mod(p.xz+4.0,8.0)-4.0;
  float dist=1e3;
  vec3 p2 = p+br_H;
  float s = 0.5;
  for (int i=0; i<=5; i+=1) {    
    float bll = length(p2)-s;
    dist = min(dist, bll);
    p2.xy*=br_GROT;
    p2.yz*=br_GROT;
    p2=abs(p2)-s;
    s*=0.5;
  }
  float flr = p.y+1.0;
  return min(dist, max(max(min(.8-dist, flr+.005),min(.4-dist, flr+.05)), max(.3-dist, flr)));
}

void br_trace(vec3 pos, vec3 dir, int steps, vec2 range, out float t, out float sh)
{
  t = range[0];
  float shadow = 1.0;
  for (int i=0;i<steps;i+=1)
  {
    float dist = br_df(pos+dir*t);
    shadow = min(shadow, dist/t);
    t += dist;
    if (dist<1e-3||dist>1e+3||t>=range[1]) {
      break;
    }
  }  
  if (t>range[1]){
    t=range[1];
  }
  sh=max(0,min(1,shadow));
}

vec3 br_nf(vec3 p, float d){
  vec2 e=vec2(0,d);
  return normalize(vec3(
    br_df(p+e.yxx) - br_df(p-e.yxx),
    br_df(p+e.xyx) - br_df(p-e.xyx),
    br_df(p+e.xxy) - br_df(p-e.xxy)
  ));
}

vec3 br_main(float q)
{
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);

    br_globals();
    
    vec2 uvo=sin(vec2(br_T,br_T+1))*2.0;
    uvo=uvo-fract(uvo);
    vec3 p0 = vec3(0,0,-4);
    vec3 d01 = normalize(vec3(uv.xy+br_TILT*ss(2,1.9,PT), +1 + 2*ss(2,1.5,PT)));
    float fd =3.1;
    float dfs=ss(2,1.5,PT)*.5+.5;
    p0+=d01*fd;
    d01.xy+=fract(sin(uv.xy*uv.yx*8331.313-uv.yx*uv.yx*33423.432+br_T)*3233.23)*.025*dfs;
    d01=normalize(d01);
    p0-=d01*fd;
    if (PT <= 2)
    {
      d01.xz*=rot(br_FT);
      p0.xz*=rot(br_FT);
    }
    
    float t01 = 0, _;
    
    br_trace(p0, d01, 50, vec2(0, 100.0), t01, _);
    
    vec3 p1 = p0+d01*t01;
    vec3 n1 = br_nf(p1,0.01);
    vec3 md = (sin(n1.yxz*2.0)+1.0)/2.0;
    
    float ao,aos, shadow;
    br_trace(p1, n1, 10, vec2(0.005, 2.0), ao, aos);
    ao = mix(ao/2.0,aos,.5);
    br_trace(p1, normalize(vec3(10,4,-4)), 50, vec2(0.1, 20.0), _, shadow);
    
    vec3 color = md* min(1,10*smoothstep(.0,.9,shadow))*vec3(.9,.4,.2)*2.0 + md * ao *vec3(.1,.2,.3)/2.0;
    
    color = mix(vec3(.1,.2,.3), color, 1/(1+pow(t01,2)*0.001));
    
    color = max(vec3(0),mix(color, vec3(length(color)), -0.1));
    color *= 2.0-length(uv);
    color = 2.0*color/(1.0+color);
    
    return color;
}

// namespace ch (chocolate) ///////////////////////////////////////////////////

float ch_T,ch_PT,ch_O1,ch_O2,_;
vec3 ch_LDIR,ch_LIGHT,ch_IND;
mat2 ch_GROT,ch_GR2,ch_SR;

void ch_gl(){
  ch_T=fGlobalTime;
  ch_PT=ch_T*3.14159;
  ch_O1=sin(ch_PT/2);ch_O2=cos(ch_PT/3);
  ch_LDIR=normalize(vec3(1,2,-2));
  ch_GROT=rot(ch_T/4);ch_GR2=rot(ch_PT/4);
  ch_SR=rot(0.313);
  ch_LIGHT=vec3(.9,.8,.7)*3.0;
  ch_IND=vec3(.9);
}

float ch_df(vec3 p){
  vec3 op=p;
  float flr = p.y+1.0;
  p.xz=mod(p.xz+4,8)-4;
  vec3 id=op-p; float h=fract(dot(id.xz,vec2(3.33,51.53)));
  vec3 bp=p;
  float ball=length(bp)-1.0;
  if (ball<8.0){
    bp.y+=sin(h*3+ch_PT/4);
    bp.xy*=ch_GROT;bp.yz*=ch_GROT;bp.xz*=rot(h*43.43);
    bp=abs(bp)-.5;bp.xy*=ch_GROT;
    bp=abs(bp)-.25;bp.xy*=ch_GR2;
    ball = length(abs(bp)-.125+ch_O2*.011)-0.02-fract(h*43.334)*.1;
  }
  flr=min(flr,max(flr-.1,1-ball));
  flr=min(flr,max(flr-.2,2-ball));
  return min(flr+sin(ball*16.0+ch_T*-10)*.05*ss(1,0,ball),ball);
}
  
float ch_dfball(vec3 p){
  vec3 op=p;
  float flr = p.y+1.0;
  p.xz=mod(p.xz+4,8)-4;
  vec3 id=op-p; float h=fract(dot(id.xz,vec2(3.33,51.53)));
  vec3 bp=p;
  float ball=length(bp)-1.0;
  if (ball<8.0){
    bp.y+=sin(h*3+ch_PT/4);
    bp.xy*=ch_GROT;bp.yz*=ch_GROT;bp.xz*=rot(h*43.43);
    bp=abs(bp)-.5;bp.xy*=ch_GROT;
    bp=abs(bp)-.25;bp.xy*=ch_GR2;
    ball = length(abs(bp)-.125+ch_O2*.011)-0.02-fract(h*43.334)*.1;
  }
  flr=min(flr,max(flr-.1,1-ball));
  flr=min(flr,max(flr-.2,2-ball));
  return ball;
}

vec3 ch_nf(vec3 p, float s){ vec2 e=vec2(0,s);return normalize(vec3(ch_df(p+e.yxx)-ch_df(p-e.yxx),ch_df(p+e.xyx)-ch_df(p-e.xyx),ch_df(p+e.xxy)-ch_df(p-e.xxy))); }

void ch_trace(vec3 p, vec3 d, float st, float et, out float t, out float s, float ts){
  t=st;
  s=1.0;
  for (int i=0;i<100;i+=1){
    float dist = ch_df(p+d*t)*ts;
    s=min(s,dist/t/ts);
    t+=dist;
    if(t<1e-3){return;}
    if(t>et){t=et;return;}
  }
}

void ch_mat(vec3 d, vec3 p, out vec3 c, out vec3 r, out float refl) {
  c=p;
  if (ch_df(p)>1){
    r=d;
    c=vec3(0);
    refl=1.0;
    return;
  }
  vec3 n=ch_nf(p,1e-3);
  float diffuse=max(0,dot(n,ch_LDIR));
  float ao,sh;
  ch_trace(p,n,1e-2,1,_,ao,.5); ao=ss(-1.0,1.0,ao);
  ch_trace(p,ch_LDIR,1e-2,10,_,sh,.5); 
  vec3 a=vec3(.9);
  if (ch_dfball(p)<=1.02) { 
    a=vec3(.15,.11,.11);
    vec3 mp,op=p;
    mp.xz=mod(p.xz+4,8)-4;
    vec3 id=op-mp; float h=fract(dot(id,vec3(2.33,1.53,51.53)));
    float ah=fract(id.x*0.179+id.z*0.9134);
    if (ah<.66){
      a=vec3(.80,.70,.65);
    }
    if (ah<.33){
      //a=vec3(.80,.50,.55);
      a=vec3(.80,.50,.55);
    }
  }
  vec3 z3=(fract(pow(fract(p.yzx*44.3439+p.zxy*-34.1345)*43.331,vec3(4.0)))-.5);
  r=normalize(reflect(d,n)+z3*.1);
  float w=64;
  float spec=pow(max(.0,dot(r,ch_LDIR)),w)*w;
  refl=pow(1+dot(d,n),4)+.01;
  c=ch_LIGHT*a*ss(.0,.9,sh) + a*ch_LIGHT*ch_IND*ao*.1+vec3(spec)*ss(.0,.9,sh)*refl*ch_LIGHT;
  //c=vec3(fres);
}

vec3 ch_bgf(vec3 d){
  return vec3(.9)*2.0;
}

vec3 ch_main(void)
{
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
    
    ch_gl();
    
    float p0x=ch_T;
    float p0xs=floor(p0x)+ss(0.5,0.7,fract(p0x));
    float p0ys=floor(p0x/4)+ss(0.5,0.55,fract(p0x/4));
    //p0x=32;
    vec3 p0=vec3(p0xs*8+14,4,-4+p0ys*8);
    vec3 d0=normalize(vec3(uv,+1-.15*pow(length(uv),3.0))),d1,d2;
    d0.xz*=rot(.33);d0.xy*=rot(.1);d0.yz*=rot(-.73);
    float t0,t1,_,r1,r2;

    vec3 c1,c2;
    
    ch_trace(p0,d0,1e-3,1e+3,t0,_,1.0);
    vec3 p1=p0+d0*t0;
    ch_mat(d0,p1,c1,d1,r1);
    
    ch_trace(p1,d1,1e-2,1e+3,t1,_,1.0);
    vec3 p2=p1+d1*t1;
    ch_mat(d1,p2,c2,d2,r2);
    
    vec3 c=mix(c1,c2,r1);
    c=mix(c,ch_bgf(d2),r1*r2);
    
    //c=mix(c,vec3(0),1-1/(1+t0*t0*1e-3));
    //c+=vec3(-.01,.09,.05);
    //c*=vec3(1.40,1.50,1.90);
    float white=dot(c,vec3(.3,.5,.2));
    c=mix(c,vec3(white),-1.5);
    c+=0.04;
    c*=2.0;
    c=max(vec3(0),c);
    c=1.2*c/(1+c);
    c=c*c;

    return c;
}

// namespace dg (digits) //////////////////////////////////////////////////////

float dg_T,dg_O1;
mat2 dg_R45;
float dg_Z;

void dg_g(){
  dg_T=fGlobalTime;
  dg_R45=rot(3.14159/4);
  dg_O1=sin(dg_T*3.14159*2)*.03+0.04;
  dg_Z=2-PT;
}

float dg_rbox(vec2 p, float r){
  p.xy -= mid(vec2(-r),p.xy,vec2(+r));
  return length(p)+r;
}

// UNUSED?
float dg_bg(vec2 p, float s){
  float ms=.02;
  float top = abs(ms/2+mod((dg_rbox(p+vec2(0,-2.4),.9)-.5)+ms,ms+ms)-ms);
  float bot = abs(ms/2+mod((dg_rbox(p+vec2(.9,+.5),.3)-.5)+ms,ms+ms)-ms);
  float d = min(top,bot);
  return ss(-s,s,d);
}

// good value for sd=0.04
float dg_ssd(vec2 p, float s, int i, float sd){
  float ms=.02;
  vec2 pt = p+vec2(0,-.3);
  vec2 pb = p+vec2(0,+.3);
  float top = abs((dg_rbox(pt,.1)-.3))-.072;
  float bot = abs((dg_rbox(pb,.1)-.3))-.072;
  vec2 ptx = pt*dg_R45;
  vec2 pbx = pb*dg_R45;
  float txx = abs(min(abs(ptx.x),abs(ptx.y)))-sd;
  float bxx = abs(min(abs(pbx.x),abs(pbx.y)))-sd;
  float d=1;
  float s0 = max(top,max(-ptx.x,-ptx.y));
  
  float s1 = max(top,max(-ptx.x,+ptx.y));
  float s4 = max(bot,max(+pbx.x,-pbx.y));
  float s3 = max(bot,max(+pbx.x,+pbx.y));
  float s2 = max(bot,max(-pbx.x,+pbx.y));
  float s5 = max(top,max(+ptx.x,-ptx.y));
  float s6t = max(top,max(+ptx.x,+ptx.y));
  float s6b = max(bot,max(-pbx.x,-pbx.y));
  float s6 = min(s6t,s6b);
  
  const int lut[]= int[10](2+4,1+2+64+16+8,1+2+64+4+8,2+4+64+32,1+4+8+32+64,1+4+8+16+32+64,1+2+4,255,255-16,255-64);
  i=lut[i];
  
  if ((i&1)!=0) d=min(d,s0);
  if ((i&2)!=0) d=min(d,s1);
  if ((i&4)!=0) d=min(d,s2);
  if ((i&8)!=0) d=min(d,s3);
  if ((i&16)!=0) d=min(d,s4);
  if ((i&32)!=0) d=min(d,s5);
  if ((i&64)!=0) d=min(d,s6);
  
  d = max(-bxx,max(-txx,d));;
  return ss(-s,s,d);
}

vec3 dg_main(void)
{
    dg_g();
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
  uv*=1.0+length(uv)*0.1;
  
  vec3 c=vec3(1);

  float ha=fract(42.53*(uv.x*uv.x+uv.y*uv.y)+43.47*fract(dg_T+dot(gl_FragCoord.xy, vec2(4.234,43.742))));
  float s=100*dg_Z+0.1*fract(sin(ha)*473.415);
  
  for (int i=0;i<20;i+=1){
      vec2 p=uv;
      p*=s;
      p+=vec2(1.39,-0.67);
      float rx=mod(p.x, 10);
      float rowid=p.x-rx;
      p.x=rx;
      p.x+=float(i)-4.5;
      p.y+=10+dg_T;
      float my=fract(p.y/2)*2-1.0;
      float id=(p.y-my)-rowid*13;
      if (i>fract(id*34.34)*20+2) break;
      p.y=my;
      float tt=floor(dg_T*10.0)*0.1*pow(10,fract(id*3.344)*10);
      float sd=max(sin(dg_T*3.1459+id+float(i))*.25+sin(id+dg_T)-1,0.01);
      c*=vec3(dg_ssd(p,1/v2Resolution.y*s,(int(float(i*i)*id)+int(floor(tt/pow(10,i))))%10,sd));
  }
  return c;
	return mix(c,c*.75+.25, ss(1.5,2,PT));
}

// namespace fd (field) ////////////////////////////////////////////////////////

float fd_T,fd_BALLY;
vec3 fd_BALLPOS,fd_LDIR;
mat2 fd_GROT;
float fd_JT;

float fd_df(vec3 p) 
{
  float flr = p.y+1.0;
  vec3 p2=p;
  p2.z+=fd_T;
  float s = 16.0;
  for (int i=0; i<16; i+=1){
    p2.xy = p2.xy*fd_GROT;
    p2.yz = p2.yz*fd_GROT;
    vec3 mp = abs(mod(p2+fd_T/20, vec3(1.0*s))-vec3(0.5*s));
    flr = min(flr, max(flr-(0.02+i*.001)*s,abs(min(mp.x,mp.y)-.2*s)-.02*s));
    s*=0.8;
  }
  float ball = length(p-fd_BALLPOS)-0.0;
  return min(flr, ball);
  
}

vec3 fd_nf(vec3 p, float d){ 
  vec2 e = vec2(0,d);
  return normalize(vec3(
    fd_df(p+e.yxx) - fd_df(p-e.yxx),
    fd_df(p+e.xyx) - fd_df(p-e.xyx),
    fd_df(p+e.xxy) - fd_df(p-e.xxy)
  ));
}

void fd_globals() {
  fd_T = fGlobalTime;
  fd_BALLY = sin(fd_T*3.14159);
  fd_BALLPOS = vec3(sin(fd_T),abs(fd_BALLY),cos(fd_T*0.8)+2);
  fd_GROT = rot(.4);
  fd_LDIR = normalize(vec3(sin(fd_T/4)*2,2,3));
  fd_JT = fd_T - mod(fd_T,4)/2;
}


vec3 fd_main(void)
{
    fd_globals();
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
  
    vec3 pos0 = vec3(sin(fd_T/4),2-PT/2,-4);
    vec3 dir01 = normalize(vec3(uv, +4-PT));
    dir01.yz*=rot(-.2+ss(2,1,PT)*.6-(3.14159/2-0.2)*ss(3,4,PT));
    dir01.xy*=rot(sin(-fd_T/4)/2*ss(4,3,PT));
    float d01 = 0.0;
    
    for (int i=0; i<100; i+=1)
    {
        float dist = fd_df(pos0 + dir01*d01);
        d01 += dist;
        if (dist<1e-3||dist>1e+3) {
        break;
        }
    }
    
    vec3 pos1 = pos0 + dir01*d01;
    vec3 norm1 = fd_nf(pos1, 0.001);
    
    float shadow = 1.0;
    vec3 spos=pos1+fd_LDIR;
    vec3 sdir = fd_LDIR;
    float st = 0.001;
    for (int i=0; i<20; i+=1){
        float dist = fd_df(spos+sdir*st);
        shadow = min(shadow, dist*2.0);
        st+=dist;
    }
    shadow = max(0, min(1,shadow));
    
    float diffuse = max(0, dot(norm1, fd_LDIR));
    
    #define AOS(A) smoothstep(-1.0, 4.0, fd_df(pos1+norm1/A))*A
    float ao = (AOS(1)+AOS(2)+AOS(4)+AOS(8)+AOS(16)+AOS(32))/6.0;
    
    vec3 color = diffuse*vec3(1)*shadow*vec3(.9,.8,.7)*2.0 + ao*vec3(.4,.5,.9)*.1;
    
    color = mix(color, vec3(.4,.5,.9)*4.0, 1-1/(1+d01*d01*0.002));
    
    color = mix(color*vec3(1.3,1,0.9)-vec3(.01), vec3(length(color)),-0.5);
    color = sqrt(1.0*color/(1.0+color));
	return color * ss(4,3.75,PT);
}

// namespace rt (refltunnel) //////////////////////////////////////////////////

float rt_T;
vec3 rt_BPOS;
mat2 rt_ROT;

float rt_dfbox(vec3 p){
  vec3 ap=abs(p);
  return max(ap.x,max(ap.y,ap.z));
}

float rt_df(vec3 p) {
  float ball=length(p-rt_BPOS)-0.8;
  vec3 p2=p-vec3(0,0,-rt_T);
  p2.xy*=rot(p2.z*.1);
  float wall=1.0-max(abs(p2.x),abs(p2.y));
  wall=max(wall,min(abs(p.x),abs(p.y))-1.0);
 
  p2.xy*=rt_ROT; p2.yz*=rt_ROT;
  vec3 rp2=mod(p2+.5,vec3(1.0))-.5;
  wall=min(wall, max(wall-.1, rt_dfbox(rp2)-0.3));
  
  vec3 p3=p2; p3.xz*=rt_ROT;
  vec3 rp3=mod(p2+.25,vec3(1.0))-.5;
  wall=min(wall, max(wall-.1, rt_dfbox(rp3)-0.3));
  
  for (int i=0; i<3; i+=1)
  {
    p3.xz*=rt_ROT; p3.yz*=rt_ROT;
    rp3=mod(p2+float(i),vec3(1.0))-.5;
    wall=min(wall, max(wall-.05, rt_dfbox(rp3)-0.3/float(i+1)));
  }
  
  return min(ball,wall);
}
void rt_trace(vec3 p, vec3 d, float mint, float maxt, out float t)
{
  t = mint;
  for (int i=0; i<100; i+=1)
  {
    float dist=rt_df(p + d*t);
    t+=dist;
    if (t>maxt) {
      t=maxt;
      break;
    }
    if(dist<1e-4) break;
  }  
}

vec3 rt_nf(vec3 p, float y) {
  vec2 e=vec2(0,y); return normalize(vec3(
    rt_df(p+e.yxx)-rt_df(p-e.yxx),
    rt_df(p+e.xyx)-rt_df(p-e.xyx),
    rt_df(p+e.xxy)-rt_df(p-e.xxy)));
}

void rt_mat(vec3 ind, vec3 pos, float e, out vec3 color, out vec3 w, out vec3 ref)
{
  if (rt_df(pos)>.1){
    color=vec3(0.0);
    w=vec3(0);
    ref=ind;
    return;
  }
  vec3 n = rt_nf(pos, e);
  vec3 ldir = normalize(vec3(1,9,3));
  float diffuse = max(.0,dot(ldir, n));
  float st;
  rt_trace(pos, ldir, 1e-2, 1.0, st);
  st=min(rt_df(pos+ldir*st),1.0);
  vec3 c= diffuse*vec3(.9,.9,.9)*st*4.0;
  float ao=smoothstep(-1,1,rt_df(pos+n));
  color=c*ao + vec3(.1,.5,.2)*max(0,smoothstep(.9,.95,sin(pos.z+rt_T*2.0)))*16.0;
  float fres=1+dot(ind,n);
  w=vec3(pow(fres,4)*.9+.01);
  ref = normalize(reflect(ind, n)+(.005*fract(34.553*sin(pos.yzx*43.34+gl_FragCoord.xyx+rt_T))));
}


vec3 rt_main(void)
{
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
    
    rt_T=fGlobalTime;
    rt_BPOS=vec3(sin(rt_T/2),0, cos(rt_T)*8+6);
    rt_ROT=rot(.4);

    uv.x+=(uv.y-mod(uv.y+rt_T,0.2))*pow(ss(0.05,0,mod(PT,1.0)),4.0);
    uv.y+=(uv.x-mod(uv.x+rt_T,0.2))*pow(ss(0.05,0,mod(PT+0.5,1.0)),4.0);
    float tiltt = 2*rt_T - mod(rt_T, 3) - mod(rt_T, 4);
    uv.xy+=vec2(sin(tiltt),cos(tiltt*9.42))*.9*ss(3,2,PT);
    
    vec3 p0=vec3(sin(rt_T)*.1,0,-4+sin(rt_T/3)); 
    vec3 d01=normalize(vec3(uv.xy, +1 +1*ss(3,4,PT))); float t01;
    d01.xy*=rot(.4+rt_T/10);
    
    
    vec3 acc = vec3(0); vec3 acw=vec3(0);
    vec3 wp = vec3(1);
    vec3 pA=p0; vec3 dirAB = d01; float tAB; float tt=0;
    vec3 fogc=vec3(1,3,3)*.1;
    for (int i=0; i<3; i+=1){
        rt_trace(pA, dirAB, 1e-3, 1e+3, tAB);
        vec3 pB=pA+dirAB*tAB;
        vec3 dirBC, c, w;
        rt_mat(dirAB, pB, (tAB+tt)/v2Resolution.y*4.0, c, w, dirBC);
        c=mix(c, fogc, 1.0-1.0/(1.0+tAB*tAB*0.05));
        
        acc += c*wp;
        acw += wp;
        //wp *= w;
        pA=pB;
        dirAB=dirBC;
        tt+=tAB;
    }
    
    vec3 c = acc/acw;
    c=mix(c,fogc,1-1/(1+tt*tt*.01));
    //c=max(vec3(.0),c+vec3(-.1,.1,.2));
    c=max(vec3(.0),c+vec3(-.1,-.1,-.1));
    c=mix(c,vec3(length(c)),-.2);
    c*=vec3(1.5,0.3,.3);
    c=2*c/(1+c);
  
	return c;
}

// namespace sg (segment) /////////////////////////////////////////////////////

float sg_T,sg_PT;
vec3 sg_LDIR,sg_LCOL;
mat2 sg_ROT;

void sg_g(){ 
  sg_T=fGlobalTime/4;
  sg_PT=3.14159*sg_T;
  sg_ROT=rot(sg_PT/4);
  sg_LDIR=normalize(vec3(1,2,-2));
  sg_LCOL=vec3(0.8,0.7,0.6)*15.0;
}

float sg_box(vec3 p){vec3 a=abs(p);return max(max(a.x,a.y),a.z); }

float sg_df(vec3 p){
  vec3 p2=p+64;
  float s=0.5;
  float b=100;
  vec3 id;
  for (int i=0;i<4;i+=1){
    vec3 rp2=mod(p2,vec3(s+s))-s;
    id=p2-rp2;
    float h=fract(dot(id,vec3(1.76,34.34,125.132))*34.343+floor(sg_T)/3);
    b=length(rp2)-s*.9;
    s*=0.5;
    if (h<0.2) break;
  }
  float ball=max(b,sg_box(p)-0.99)*0.9;
  vec3 p3=p;
  p3.xy*=sg_ROT;
  p3.xz*=sg_ROT;
  float wall=abs(min(2-abs(p3.y),2-abs(p3.x)))-.1;
  float flr = p.y+1000.0;
  wall=max(wall,abs(abs(abs(p3.z)-1.0)-0.2)-0.1);
  return min(wall,ball);
}

float sg_dfh(vec3 p){
  vec3 p2=p+64;
  float s=0.5;
  float b=100;
  float h=0;
  vec3 id;
  for (int i=0;i<4;i+=1){
    vec3 rp2=mod(p2,vec3(s+s))-s;
    id=p2-rp2;
    h=fract(dot(id,vec3(1.76,34.34,125.132))*34.343+floor(sg_T)/3);
    b=length(rp2)-s;
    s*=0.5;
    if (h<0.2) break;
  }
  float ball=max(b,sg_box(p)-1.0);
  vec3 p3=p;
  p3.xy*=sg_ROT;
  p3.xz*=sg_ROT;
  float wall=abs(min(2-abs(p3.y),2-abs(p3.x)))-.1;
  float flr = p.y+1.0;
  wall=max(wall,abs(abs(p3.z)-1.0)-0.2);
  if (wall<ball||flr<ball) {
    return -1;
  }
  return h;
}

vec3 sg_nf(vec3 p, float s) { vec2 e=vec2(0,s); return normalize(vec3(sg_df(p+e.yxx)-sg_df(p-e.yxx),sg_df(p+e.xyx)-sg_df(p-e.xyx),sg_df(p+e.xxy)-sg_df(p-e.xxy)));}

float sg_trace(vec3 p, vec3 d, float st, float et){
  float t=st;
  for (int i=0; i<100; i+=1){
    float dist=sg_df(p+d*t);
    t+=dist*.5;
    if (dist>et||abs(dist)<1e-4){
      break;
    }
  }
  return t;
}
  
float sg_strace(vec3 p, vec3 d, float st, float et){
  float t=st;
  float s=1.0;
  for (int i=0; i<100; i+=1){
    float dist=sg_df(p+d*t);
    s=min(s,dist/t);
    t+=dist/2;
    if (dist>et||dist<1e-3){
      break;
    }
  }
  return s;
}

void sg_mat(vec3 d, vec3 p, out vec3 c, out vec3 w, out vec3 d1){
  if (sg_df(p)>.1) {
    c=vec3(.5,.6,.9)*2.0;
    w=vec3(0);
    d1=d;
    return;
  }
  vec3 n=sg_nf(p,1e-3);
  float fres=1.0+dot(d,n);
  float diffuse=max(0,dot(n,sg_LDIR));
  float sh=sg_strace(p,sg_LDIR,1e-2,10);
  float h=fract(sg_dfh(p)*343.43431);
  float h2=fract(h*4.3143);
  float h3=fract(h*4.3143);
  vec3 a=vec3(1,.1,.1);
  a=mix(a,vec3(.5,0.5,0.5),h);
  d1=normalize(reflect(d,n))+h2*fract(fract(p.yzx*43.3143)+fract(p*9343.334+sg_T)*435.3234)*.2;
  sh=min(1,sh*4.0);
  c=a*diffuse*sg_LCOL*sh*h;
  w=vec3(fres*fres*0.95+0.05);
  if (h3<0.16&&h3>0.1) c+=vec3(9,2,0);
  if (h3<0.1) w*=a;
}

vec3 sg_main(void)
{
    sg_g();
    vec2 uv = out_texcoord;
    uv -= 0.5;
    uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
    float q1=ss(0.5,0.55,fract(PT+0.25))-ss(1,0.95,fract(PT));
    float q2=ss(0.5,0.55,fract(PT+0.75))-ss(1,0.95,fract(PT+.5));
    float ft=PT-mod(PT, 0.5);
    vec2 tilt=vec2(cos(ft*19),sin(ft*14))*sin(ft*17)*0.25;
    vec3 p0=vec3(0,0,-6), d0=normalize(vec3(uv.xy+=tilt, +2.5+cos(ft*42)*.5));
    p0.xz*=rot(sg_T-q1);d0.xz*=rot(sg_T-q1);
    p0.yz*=rot(sg_T-q2);d0.yz*=rot(sg_T-q2);
    float tt=0;
    vec3 c=vec3(0);
    vec3 wc=vec3(1);
    
    for (int i=0;i<4;i+=1){
        float t=sg_trace(p0,d0,1e-1, 1e+4);
        vec3 p1=p0+d0*t,c1,w1,d1;
        sg_mat(d0,p1,c1,w1,d1);
        c+=max(vec3(0),c1*wc);
        wc*=w1;
        p0=p1;
        d0=d1;
        tt+=t;
    }
    
    float w=dot(c,vec3(0.3,0.5,0.2));
    c=mix(c,vec3(w),-0.5);
    c=1.5*c/(1.0+c);
  
	return c;
}

// namespace sr(structure) ////////////////////////////////////////////////////

float sr_T,sr_PT,sr_WH,sr_WHO;
vec3 sr_LDIR,sr_BPOS,sr_LCOLOR,sr_BGCOL;
mat2 sr_ROT;

void sr_g() {
  sr_T=fGlobalTime;
  sr_PT=sr_T*3.14159;
  sr_BPOS=vec3(sin(sr_PT/4),cos(sr_PT/6),0);
  sr_LDIR=normalize(vec3(-2+sin(sr_PT/4)*.1,9+cos(sr_PT/4),-4));
  sr_ROT=rot(3.14159/3);
  sr_WHO=sin(sr_PT/10);
  sr_LCOLOR=vec3(.9,.85,.83)*40*max(0,(1+sr_WHO)*.5);
  sr_WH=-4.2+sr_WHO*4.2;
  sr_BGCOL=mix(vec3(.5,.6,.9),sr_LCOLOR/10,ss(-1.0,1.0,sr_WHO));
}

float sr_df(vec3 p){
  vec3 wp=p;
  wp.z+=sr_T;
  //wp.xy*=rot(wp.z/100);
  float wall = max(-(max(abs(wp.x),abs(wp.y))-8.0),p.y+sr_WH);
  vec3 p3=mod(p+vec3(0,0,sr_T),5.0)-2.5;
  wall=max(wall, -(abs(p3.z)-1.0));
  vec3 bp=p-sr_BPOS;
  bp.z=mod(bp.z+8.0,16.0)-8.0;
  float ball = length(bp)-1.0;
  vec3 pw2=p;
  pw2.z+=sr_T;
  float s=16.0;
  for (int i=0;i<4;i+=1)
  {
    vec3 pw3=pw2;
    //pw3.xyz+=sr_T/4;
    pw3.xy*=sr_ROT;
    //pw3.xz*=sr_ROT;
    
    pw3=abs(mod(pw3,s+s)-s);
    float pat=abs(min(pw3.y,min(pw3.x,pw3.z))-0.13*s)-0.02*s;
    wall=min(wall,max(abs(wall)-.1*s,pat));
    s*=0.5;
  }
  return wall;
}

float sr_trace(vec3 p, vec3 d, float t, float et)
{
  for (int i=0; i<100; i+=1){
    float dist=sr_df(p+d*t);   t+=dist; if (abs(dist)<1e-3 || t>et) break;   
  }
  return t;
}

float sr_strace(vec3 p, vec3 d, float t, float et)
{
  float s=1.0;
  for (int i=0; i<100; i+=1){
    float dist=sr_df(p+d*t);   
    s=min(s,dist/t);
    t+=dist*0.9;
    if (dist<1e-4 || t>et) break;   
  }
  return max(0,s);
}

vec3 sr_nf(vec3 p, float s){ vec2 e=vec2(0,s);return normalize(vec3(sr_df(p+e.yxx)-sr_df(p-e.yxx),sr_df(p+e.xyx)-sr_df(p-e.xyx),sr_df(p+e.xxy)-sr_df(p-e.xxy)));}

void sr_mat(vec3 d, vec3 p, out vec3 color, out vec3 refl, out vec3 d1, float s)
{
  if (sr_df(p)>.1){
    d1=d;
    refl=vec3(0);
    //color=1.13*vec3(.2,.3,.4)*(1.0/(.01+d.y*d.y)) + pow(max(0,dot(sr_LDIR,d)),16)*4.0;
    color = sr_BGCOL*4.0;
    return;
  }
  vec3 n=sr_nf(p,s);
  float WW=sr_WHO*.5+.5;
  float empatb=sin(sr_PT+sr_df(p-14.0)*sr_df(p+4.0)*10.0)*sin(sr_df(p-3.0)*sr_df(p+2.0)*4.0)*ss(.1-(1-WW)*.2,.11,length(p.xy)*0.01);
  float epat1=ss(.9,.95,empatb);
  float epat2=ss(.9,.5,empatb);
  float h=mix(0.02,0.02, epat2);
  d1=normalize(reflect(d,n))+fract(fract(fract(p.zxy*634.0+p.yzx*333+gl_FragCoord.xyx*4.3830-sr_T)*43.343)*4.33)*h;
  refl = mix(pow(max(0,min(1.0,1.0+dot(d,n))),2.0),1.0,0.1)*vec3(.9,.9,.9);
  float diffuse=max(0,dot(n,sr_LDIR));
  vec3 a=vec3(.543,.5,.4)*.3;
  float ao=max(0,min(1.0,sr_trace(p,n,1e-2,1.0)));
  //refl*=ao;
  float sh=sr_strace(p, sr_LDIR, 1e-2, 20);
  a=mix(a,vec3(.45,.43,.4),epat2);
  vec3 em=epat1*vec3(1,0.5,0.45)*32.0;
  color=sr_LCOLOR*(a*diffuse)*(1-refl)*ss(0.0,0.01,sh)+ao*a*(sr_WHO*.5+.5)*sr_LCOLOR*0.05+em;
}

vec3 sr_main(void)
{
	vec2 uv = out_texcoord;
	uv -= 0.5;
	uv /= vec2(v2Resolution.y / v2Resolution.x, 1);
  
    sr_g();
    
    vec3 p0=vec3(0,0,-9), d0=normalize(vec3(uv.xy, +1));
    vec3 c=vec3(0);
    vec3 ca=vec3(1);
    float CT=sr_T/4-fract(sr_T/4);
    d0.xz*=rot(.5+sin(CT*9));
    d0.yz*=rot(-.3+cos(CT*14));
    d0.xy*=rot(.5);
    float tt=0;
    
    for (int i=0; i<3; i+=1){
        float t0=sr_trace(p0,d0,1e-2,1e+4);
        vec3 p1=p0+t0*d0;
        vec3 d1,co;
        vec3 refl;
        tt+=t0;
        sr_mat(d0,p1,co,refl,d1,tt*1.0/v2Resolution.y);
        c=mix(c,sr_BGCOL,ca-ca/(1.0+t0*t0*0.001));
        c+=co*ca;
        ca*=refl;
        if(length(ca)<0.01)break;
        p0=p1;
        d0=d1;
    }
    
    float w=dot(c,vec3(.3,.5,.2));
    c=max(vec3(0),c+vec3(-.0));
    c*=vec3(1.0,1.0,1.0)*0.5;
    c=max(vec3(0),mix(c,vec3(w),-.5));
    c=vec3(1.4)*c/(1.0+c);

	return c;
}

// REALMAIN ///////////////////////////////////////////////////////////////////

void main() {
    float MT = fGlobalTime/4;
    ST = MT/2;
    PT = ST;
    vec3 c = vec3(0);
  
    // dg_main;

    if (ST < 4.0) {
      PT = ST; // build up 
      c = fd_main(); 
      if (ST < 2.0){
        c *= dg_main();
      }
      c *= ss(0,1,ST);
    }
    else if (ST < 8)
    {
      PT = ST - 4; // P1
      // drop
      // slow
      if (ST < 6)
      {
        c = rt_main();
      }
      else 
      {
        // arp
        c = rt_main();
      }
    }
    else if (ST < 12)
    {
      PT = ST - 8; // P2 fast br_main
      if (ST < 9) c = br_main(0); // load
      else if (ST < 10) c = br_main(1);
      else if (ST < 11) c = sr_main(); // tone shift
      else if (ST < 12) c = sr_main(); // bdown
    }
    else if (ST < 16)
    {
      PT = ST - 12; // P3 slow
      // drop
      if (ST < 14)
      {
        c = sr_main();
      }
      else {
        // building
        c = ch_main();
      }
    }
    else if (ST < 18)
    {
      PT = ST - 16; // P4 intr
      if (ST < 18)
      {
        // tw
        c = sg_main();
      }
    }
    else if (ST < 22)
    {
      PT = ST - 20;  // P6 slower tw
      c = bt_main();
      c *= ss(22,20,ST);
      if (ST > 20){
        PT = (28-ST)*0.25;
        c += vec3(1)-dg_main();
      }
    }
    else
    {
      PT = (28-ST)*0.25; // P7 fade out
      c = vec3(1)-dg_main();
      c = mix(c,vec3(0),ss(22,23,ST));
    }

    // else if (ST < 5.0) c = rt_main();
    // else if (ST < 6.0) c = sr_main();
    // else if (ST < 7.0) c = dg_main(); // move to overlay
    // else if (ST < 8.0) c = ch_main();
    // else if (ST < 9.0) c = sg_main();
    // else if (ST < 10.0) c = bt_main();
    // else if (ST < 11.0) c = br_main();
    // // repeating scenes
    // else if (ST < 12.0) c = rt_main();
    // else c = sr_main();
    // fadeout
    f_color = vec4(c, 1.0);
}
