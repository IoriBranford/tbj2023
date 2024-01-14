pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
-- asset handling

function apply_spr_base(spr,base)
 local typ=type(spr)
 if typ=="number" then
  return spr+base
 end

 if typ=="table" then
  local i=spr.i
  if i then
   spr.i=i+base
  end

  for i=1,#spr do
   spr[i]=apply_spr_base(spr[i],base)
  end
 end
 return spr
end

function apply_sprs_bases(sprs)
 for name,spr in pairs(sprs) do
  local base=type(spr)=="table"
   and spr._base
  if base then
   for name,subspr in pairs(spr) do
    spr[name]=apply_spr_base(subspr,base)
   end
  end
 end
end

-->8
--utility

function nop() end

function xor(a,b)
  return a and not b or b and not a
end

function scopy(a)
 if a then
  local c={}
  for i=1,#a do
   c[i]=a[i]
  end
  return c
 end
end

function swappop(a,i)
 local e=a[i]
 a[i]=a[#a]
 a[#a]=nil
 return e
end

function cleanup(a, cond)
 for i=#a,1,-1 do
  local o=a[i]
  if cond(o) then
   swappop(a,i)
  end
 end
end

function dir_input_x()
 return
  (btn(⬅️) and -1 or 0) +
  (btn(➡️) and 1 or 0)
end

function dir_input_y()
 return
  (btn(⬆️) and -1 or 0) +
  (btn(⬇️) and 1 or 0)
end

function dir_input()
 return dir_input_x(),
  dir_input_y()
end

function aabbs(x,y,w,h,a,b,m,n)
 return x+w>a and x<a+m
  and y+h>b and y<b+n
end

-->8
--object

local objs={}

function draw_obj_text(o)
 pal()
 local text=o.text
 local typ=type(text)
 if typ=="table" then
  local y=o.y
  local lineh=o.lineh or 8
  for i=1,#text do
   print(text[i],o.x,y,o.clr)
   y=y+8
  end
 elseif typ=="string" then
  print(text,o.x,o.y,o.clr)
 end
end

function draw_obj_circ(o)
 local circ=o.fill and circfill
  or circ
 circ(o.x,o.y,o.rad,o.clr)
end

function draw_obj_sspr(o)
  local sx,sy=o.sx,o.sy
  local sw,sh=o.sw,o.sh
  local i=o.spri
  if i and not (sx and sy) then
   sx=(i&0xF)<<3
   sy=(i&~0xF)>>1
  end
  sw=sw or ((o.w or 1)<<3)
  sh=sh or ((o.h or 1)<<3)
  pal(o.pal)
  palt(o.palt or 0x8000)
  sspr(sx or 0,sy or 0,
   sw,sh,
   o.x,o.y,
   o.dw or sw,o.dh or sh,
   xor(o.flpx,o.frmflpx),
   xor(o.flpy,o.frmflpy))
end
 
function draw_obj_spr(o)
 if o.dw or o.dh then
  draw_obj_sspr(o)
  return
 end
 pal(o.pal)
 palt(o.palt or 0x8000)
 spr(o.spri,o.x,o.y,o.w,o.h,
  xor(o.flpx,o.frmflpx),
  xor(o.flpy,o.frmflpy))
end

function draw_obj_map(o)
 pal(o.pal)
 palt(o.palt or 0x8000)
 map(o.celx,o.cely,o.x,o.y,o.celw,o.celh,o.layer)
end

function add_obj(o)
 add(objs,o)
 o.age=0
 o.x=o.x or 0
 o.y=o.y or 0
 o.update=o.update or nop
 o.draw=o.draw or nop
end

function add_obj_circ(o)
 add_obj(o)
 o.rad=o.rad or 1
 o.clr=o.clr or 1
 o.draw=draw_obj_circ
end

function add_obj_text(o)
 add_obj(o)
 o.clr=o.clr or 7
 o.lineh=o.lineh or 8
 o.draw=draw_obj_text
end

function set_obj_frm(o,fi)
 fi=fi or 1
 local ani=o.ani
 local spri,ft,
  flpx,flpy,
  palt,pal,
  snd
 local typ=type(ani)
 if typ=="number" then
  spri=ani
 elseif typ=="table" then
  local f=ani[fi]
  typ=type(f)
  if typ=="table" then
   spri=f.i
   ft=f.t
   flpx=xor(f.flpx,ani.flpx)
   flpy=xor(f.flpy,ani.flpy)
   palt=f.palt
   pal=f.pal
   snd=f.sfx
  else
   spri=typ=="number" and f
   flpx=ani.flpx
   flpy=ani.flpy
  end
  spri=spri or ani.i
  ft=ft or ani.t
  palt=palt or ani.palt
  pal=pal or ani.pal
  snd=snd or ani.sfx
 end
 if snd then
  sfx(snd)
 end
 o.spri=spri or 1
 o.fi=fi
 o.ft=ft or 1
 o.frmflpx=flpx
 o.frmflpy=flpy
 o.palt=palt
 o.pal=pal
end

function start_obj_ani(o,ani,fi)
 o.ani=ani
 set_obj_frm(o,fi)
end

function update_obj_ani(o,ani)
 if ani and ani~=o.ani then
  start_obj_ani(o,ani)
  return
 end
 ani=o.ani
 local typ=type(ani)
 local n=typ=="table" and #ani or 1
 local ft=o.ft-1
 if ft==0 then
  local fi=o.fi
  if fi>=n then
   fi=typ=="table" and ani.loop or 1
  else
   fi=fi+1
  end
  set_obj_frm(o,fi)
 else
  o.ft=ft
 end
end

function add_obj_spr(o)
 add_obj(o)
 o.w=o.w or 1
 o.h=o.h or 1
 if (o.draw or nop)==nop then
  o.draw=draw_obj_spr
 end
 if o.ani then
  start_obj_ani(o,o.ani,o.fi)
 end
 o.spri=o.spri or 1
 return o
end

function obj_frm_ending(o)
 return (o.ft or 1)<=1
end

function obj_ani_ending(o)
 local ani=o.ani
 local n=type(ani)=="table" and #ani or 1
 return n<=(o.fi or 1) and obj_frm_ending(o)
end

function add_obj_map(o)
 add_obj(o)
 o.draw=draw_obj_map
 o.celx=o.celx or 0
 o.cely=o.cely or 0
 o.celw=o.celw or 1
 o.celh=o.celh or 1
end

function obj_dead(o)
  return o.age<0
end

function kill_obj(o)
 o.age=-0x8000
end

function update_objs()
 for i=1,#objs do
  local o=objs[i]
  o.age=o.age+1
  o:update()
 end
end

function draw_objs()
 for i=1,#objs do
  local o=objs[i]
  o:draw()
 end
end

function cleanup_dead_objs()
 cleanup(objs, obj_dead)
end

function clear_objs()
 for i=#objs,1,-1 do
  objs[i]=nil
 end
end
-->8
-- camera
local cam={
 x=0,y=0
}

local camtopspd=4

-->8
-- game assets
local snds={
 gamemus=0,
 titlemus=9,
 deadmus=15,
 endmus=16,
 finalmus=22,

 step=32,
 jump=33,
 nindie=34,
 expl=35,
 expl2=36,
 catch=38,
 engetup=39,
 throw=40,
 split=41,
 bdrop=42,
 fwall=43,
 change=47
}

local pals={
 burn1={4,9,10,4,9,10,4,9,10,4,9,10,4,9,10},
 burn2={9,10,4,9,10,4,9,10,4,9,10,4,9,10,4},
 burn3={10,4,9,10,4,9,10,4,9,10,4,9,10,4,9},
}

local sprs={
 heart=64,
 expl={
  _base=67,
  tl={t=6,0,1,2,3},
  tr={t=6,flpx=true,0,1,2,3},
  bl={t=6,flpy=true,0,1,2,3},
  br={t=6,flpx=true,flpy=true,0,1,2,3},
  ctr={t=6,{i=4,sfx=snds.expl},5,4,5}
 },
 bomb={
  _base=16,
  normal={
   0,{i=0,pal={[5]=8,[1]=2}},
   t=6
  },
  split={
   0,{i=0,pal={[5]=11,[1]=3}},
   t=6
  },
  fwall={
   0,{i=0,pal={[5]=9,[1]=4}},
   t=6
  }
 },
 puff={t=3,
  28,29,30,31
 },
 splitpuff={t=3,pal={[6]=3},
  {i=28,sfx=snds.split,},29,30,31
 },
 fbomb={t=6,
  34,{i=35,pal={[5]=10,[1]=9}}
 },
 flame={t=3,
  36,37,38,39
 },
 flameexpl={
  _base=80,
  right={0,1,2,3,4,t=3},
  left={0,1,2,3,4,t=3,flpx=true},
 },
 flamepuff={t=3,pal={[6]=9,[5]=8},
  {i=28,sfx=snds.fwall},29,30,31
 },
 enemyskull={
  _base=88,
  run={t=6,0,4},
  throw={i=2,t=30},
  jump=2,
  knocked={t=3,
   {i=4,pal=pals.burn1},
   {i=4,pal=pals.burn2},
   {i=4,pal=pals.burn3},
  },
  getup={i=4,t=60,sfx=snds.change},
 },
 ninja={
  _base=128,
  idle={0,1,t=15},
  prejump=2,
  jump={loop=6,
   {i=3,t=9,sfx=snds.jump},
   {i=20,t=3},
   {i=21,t=3},
   {i=20,t=3,flpx=true,flpy=true},
   {i=21,t=3,flpx=true,flpy=true},
   {i=39,t=3},
   {i=40,t=3}
  },
  blownup={t=3,loop=3,
   {i=4,t=6,pal=pals.burn2,sfx=snds.nindie},
   {i=5,t=6,pal=pals.burn3},
   {i=35,pal=pals.burn1},
   {i=36,pal=pals.burn2},
   {i=35,pal=pals.burn3},
   {i=36,pal=pals.burn1},
   {i=35,pal=pals.burn2},
   {i=36,pal=pals.burn3},
  },
  die={4,5,6,t=12},
  swim={7,8,9,8,t=6},
  run={16,
   {i=17,sfx=snds.step},
   18,
   {i=17,sfx=snds.step},
   t=6
  },
  roll={20,21,
   {i=20,flpx=true,flpy=true},
   {i=21,flpx=true,flpy=true},
   t=6
  },
  crawl={22,23,t=12},
  duck=23,
  froze=24,
  hang=33,
  climb={32,
   {i=33,sfx=snds.step},
   {i=32,flpx=true},
   {i=33,flpx=true,sfx=snds.step},
   t=6
  },
  wall=34,
  fall={35,36,t=3},
  fire={37,38,37,t=6},
  zip={39,40,t=3},
  swing={48,49,t=6},
  attack={t=6,
   {i=50,palt=0b1100100001100000,pal={[5]=13}},
   {i=50,palt=0b1000110001100000,pal={[1]=13}},
   {i=50,palt=0b1000110001000000,pal={[1]=13}},
   {i=50,palt=0b1000110000100000,pal={[1]=13}},
   {i=50,palt=0b1000010001100000,pal={[1]=13}},
  },
  poss={51,52,53,t=6},
 },
 enemy={
  _base=192,
  idle={0,2,t=10},
  run={4,6,8,10,t=6},
  jump=12,
  drop=14,
  prethrow=34,
  throw={i=32,t=30,sfx=snds.bdrop},
  knocked={t=3,
   {i=40,pal=pals.burn1},
   {i=40,pal=pals.burn2},
   {i=40,pal=pals.burn3},
  },
  getup={i=46,t=30,sfx=snds.engetup},
 },
 ladder=189,
 plane={
  _base=1,
  fwd=0,
  down=2,
  up=4
 }
}
apply_sprs_bases(sprs)
-->8
--game world
local ninja,enemy,enbombs,ninbombs,expls
local hazeclr,hazeptn
local solidflag=0
local ladderflag=1
local breakflag=2

local rooms={ --<y,{celx,cely}>
 [0]={0,0},
 [128]={0,16},
 [256]={16,0},
 [384]={16,16},
 [512]={32,0},
 [640]={32,16},
}
local mapbtm=768
local worldbtm=mapbtm+88

function clear_game_objs()
 clear_objs()
 ninja=nil
 enemy=nil
 enbombs={}
 ninbombs={}
 expls={}
 hazeclr=nil
 hazeptn=nil
end

function room_cell(x,y)
 x,y=flr(x),flr(y)
 local room=rooms[y&~0x7F]
 if room then
  return
   room[1]+((x&0x7F)>>3),
   room[2]+((y&0x7F)>>3)
 end
end

function cell_bound(x,y,axis,dir)
 local c,r=room_cell(x,y)
 if c and fget(mget(c,r),solidflag) then
  local v=axis>0 and y or x
  local bnd=(flr(v)&~0x7)
  if (dir or 1)<0 then
   bnd=bnd+8
  end
  return bnd
 end
end

function obj_ground(o)
 local w,h=o.w<<3,o.h<<3
 local x,y=o.x,o.y+h
 for x=x,x+w-1,w-1 do
  local bnd=cell_bound(x,y,1,1)
  if bnd then
   return bnd
  end
 end
end

function cell_ladder(x,y,axis)
 local c,r=room_cell(x,y)
 if c and fget(mget(c,r),ladderflag) then
  local v=axis>0 and y or x
  return flr(v)&~0x7
 end
end

function obj_ladder(o)
 local w,h=o.w<<3,o.h<<3
 local x,y=o.x,o.y
 for x=x,x+w-1,w-1 do
  local ldr=cell_ladder(x,y,0)
  if ldr then
   return ldr
  end
 end
end

function add_rooms()
 for y,room in pairs(rooms) do
  add_obj_map({
   y=y,
   celx=room[1],
   cely=room[2],
   celw=16,
   celh=16
  })
 end
end

function obj_fall_out_y(o)
 if o.y<0
  and o.y+(o.w<<3)<cam.y
  or o.y>cam.y+128 then
  kill_obj(o)
 end
end

-->8
--bombs
local splitbombwholegrav=-1/16
local splitbombhalfgrav=1/16
local fwallgrav=-1/32
local flamegrav=-1/32
local fbombexpldist=16
local splitbombmaxvely=1.5
local fusecolors={8,14,7}

function draw_bomb(o)
 local secs=ceil((o.fuse or -1)/60)
 local clr=fusecolors[secs]
 if clr then
  print(secs,o.x+2,o.y-8,clr)
 end
 draw_obj_spr(o)
 if clr then
  pset(o.x+2+rnd(4),
   o.y-1+rnd(4),
   clr)
 end
end

function update_expl(o)
 o.x=o.x+(o.vx or 0)
 o.y=o.y+(o.vy or 0)
 update_obj_ani(o)
 if obj_ani_ending(o) then
  kill_obj(o)
 end
end

function add_puff(o)
 add_obj_spr(o)
 o.update=update_expl
 return o
end

function add_normal_expl(cx,cy)
 add_puff {
  x=cx-8,y=cy-8,
  ani=sprs.expl.tl,
 }
 add_puff {
  x=cx,y=cy-8,
  ani=sprs.expl.tr,
 }
 add_puff {
  x=cx-8,y=cy,
  ani=sprs.expl.bl,
 }
 add_puff {
  x=cx,y=cy,
  ani=sprs.expl.br,
 }
 add(expls, add_obj_spr {
  x=cx-4,y=cy-4,
  ani=sprs.expl.ctr,
  update=update_expl
 })
end

function add_flame_puffs(cx,cy)
 add_puff {
  x=cx-8,y=cy-8,
  ani=sprs.flameexpl.left,
 }
 add_puff {
  x=cx,y=cy-8,
  ani=sprs.flameexpl.right,
 }
end

function add_flame_expl(cx,cy)
 add_flame_puffs(cx,cy)
 add(expls, add_obj_spr {
  x=cx-4,y=cy-4,
  ani=sprs.flamepuff,
  update=update_expl
 })
end

function bomb_explode(o)
 o.addexpl(o.x+(o.w<<2),
  o.y+(o.h<<2))
 kill_obj(o)
end

function update_bomb_normal(o)
 o.y=o.y+o.vy
 update_obj_ani(o)
 obj_fall_out_y(o)
end

function update_bomb_split_whole(o)
 local bombs,ay=enbombs,splitbombwholegrav
 if o.target==enemy then
  bombs,ay=ninbombs,-ay
 end
 local vy=o.vy
 vy=ay<0
  and max(0,vy+ay)
  or min(0,vy+ay)
 if vy==0 then
  add(bombs,add_bomb({
   x=o.x,y=o.y,
   vx=-1,vy=0
  }, "splithalf"))
  add(bombs,add_bomb({
   x=o.x,y=o.y,
   vx=1,vy=0
  }, "splithalf"))
  add_puff {
   x=o.x,y=o.y,
   vy=-.5,
   ani=sprs.splitpuff
  }
  kill_obj(o)
 end
 o.vy=vy
 o.x,o.y=o.x+o.vx,o.y+vy
 update_obj_ani(o)
 obj_fall_out_y(o)
end

function update_bomb_split_half(o)
 local vx,vy=o.vx,o.vy
 if vx<0 then
  vx=min(0,vx+.0625)
 elseif vx>0 then
  vx=max(0,vx-.0625)
 end
 local ay=o.target==enemy
  and -splitbombhalfgrav
  or splitbombhalfgrav
 vy=min(vy+ay,splitbombmaxvely)
 o.vx,o.vy=vx,vy
 o.x,o.y=o.x+vx,o.y+vy
 update_obj_ani(o)
 obj_fall_out_y(o)
end

function update_fwall_bomb(o)
 local bombs,ay=enbombs,fwallgrav
 if o.target==enemy then
  bombs,ay=ninbombs,-ay
 end
 local vy=o.vy
 vy=ay<0
  and max(.25,vy+ay)
  or min(-.25,vy+ay)
 o.dist=(o.dist or 0)+abs(vy)
 if o.dist>=32 then
  for vx=-2,2 do
   add(bombs,add_bomb({
    target=o.target,
    x=o.x,y=o.y,
    vx=vx,
    vy=o.target==enemy
     and -1.5 or 1.5,
    ax=vx*-.0625
   }, "flame"))
  end
  add_puff {
   x=o.x,y=o.y,
   vy=-.5,
   ani=sprs.flamepuff
  }
  kill_obj(o)
 end
 o.vy=vy
 o.x,o.y=o.x+o.vx,o.y+vy
 update_obj_ani(o)
 obj_fall_out_y(o)
end

function update_fwall_flame(o)
 local vx,vy,ax=o.vx,o.vy,o.ax
 if vx<0 then
  vx=min(0,vx+ax)
 elseif vx>0 then
  vx=max(0,vx+ax)
 end
 local ay=o.target==enemy
  and -flamegrav
  or flamegrav
 if ay<0 then
  vy=max(vy+ay,1)
 else
  vy=min(vy+ay,-1)
 end
 o.dist=(o.dist or 0)+abs(vy)
 o.vx,o.vy=vx,vy
 o.x,o.y=o.x+vx,o.y+vy
 update_obj_ani(o)
 if o.dist>=80 then
  add_flame_puffs(
   o.x+(o.w<<2),
   o.y+(o.h<<2))
  kill_obj(o)
 end
end

function update_fbomb_expls(o)
 if o.age%6==0 then
  local dist=(o.expldist or 0)+4
  o.expldist=dist
  local cx,cy=
   o.x+(o.w<<2),
   o.y+(o.h<<2)
  for dy=-1,1,2 do
   for dx=-1,1,2 do
    add(expls,add_normal_expl(
     cx+dx*dist,cy+dy*dist
    ))
   end
  end
 end
 if o.age>=30 then
  kill_obj(o)
 end
end

function update_fbomb_preexpl(o)
 update_obj_ani(o)
 o.x,o.y=o.x+o.vx,o.y+o.vy
 local t=(o.t or 30)-1
 o.t=t
 if t<=0 then
  bomb_explode(o)
  add_obj({
   x=o.x,y=o.y,
   w=o.w,h=o.h,
   update=update_fbomb_expls
  })
 end
end

function update_fbomb_fall(o)
 update_obj_ani(o)
 if o.x<8 then
  o.vx=abs(o.vx)
 elseif o.x>112 then
  o.vx=-abs(o.vx)
 end
 o.x,o.y=o.x+o.vx,o.y+o.vy
 local tx,ty=o.target.x,o.target.y
 if not o.target.dying
 and abs(tx-o.x)<fbombexpldist
 and abs(ty-o.y)<fbombexpldist then
  o.vx,o.vy=0,0
  o.update=update_fbomb_preexpl
 end
end

function update_bomb_fuse(o)
 o.fuse=o.fuse-1
 if o.fuse<=0 then
  bomb_explode(o)
 end
 update_obj_ani(o)
end

function start_bomb_fuse(o)
 o.fuse=abs(o.fuse)
 o.updateonthrow=o.updateonthrow or o.update
 o.update=update_bomb_fuse
end

function start_bomb_thrown(o)
 o.update=o.updateonthrow
 o.updateonthrow=nil
 o.fuse=-1
 o.target=enemy
 o.flpy=true
 o.dist=nil
end

local bombtmpls={
 normal={
  fuse=180,
  vx=0,vy=1.5,
  ani=sprs.bomb.normal,
  update=update_bomb_normal,
 },
 split={
  fuse=180,
  vx=0,vy=2,
  ani=sprs.bomb.split,
  update=update_bomb_split_whole,
 },
 splithalf={
  fuse=180,
  ani=sprs.bomb.split,
  update=update_bomb_split_half,
 },
 fwall={
  fuse=180,
  vx=0,vy=1.5,
  ani=sprs.bomb.fwall,
  update=update_fwall_bomb,
 },
 flame={
  h=2,
  fuse=0,
  cantcatch=true,
  ani=sprs.flame,
  addexpl=add_flame_expl,
  update=update_fwall_flame,
 },
 fbomb={
  fuse=180,
  vx=1.5,vy=1.5,
  ani=sprs.fbomb,
  update=update_fbomb_fall,
  updateonthrow=update_fbomb_fall,
 }
}

function add_bomb(o,tmpl)
 if type(tmpl)=="string" then
  tmpl=bombtmpls[tmpl]
 end
 tmpl=type(tmpl)=="table" and tmpl
  or bombtmpls.normal
 for k,v in pairs(tmpl) do
  o[k]=v
 end
 add_obj_spr(o)
 o.draw=draw_bomb
 o.fuse=-o.fuse
 o.addexpl=o.addexpl
  or add_normal_expl
 return o
end

function obj_explode_bombs(o,bombs)
 local heldbomb=o.bomb
 for bomb in all(bombs) do
  if bomb~=o
  and bomb~=heldbomb
  and aabbs(o.x,o.y,
   o.w<<3,o.h<<3,
   bomb.x+(bomb.w<<1),
   bomb.y+(bomb.w<<1),
   bomb.w<<2,bomb.h<<2)
  then
   bomb_explode(bomb)
  end
 end
end

function obj_hit_any_expl(o)
 for expl in all(expls) do
  if aabbs(o.x,o.y,
   o.w<<3,o.h<<3,
   expl.x,expl.y,
   expl.w<<3,expl.h<<3)
  then
   return expl
  end
 end
end
-->8
--ninja

local ninstartlife=5
local ninmaxstartlife=9
local ninrunaccel=.25
local nintoprunspd=1.5
local nintopfallspd=4
local ningrav=1/8
local ninclimbaccel=.5
local nintopclimbspd=1.5
local ninjumpvely=-2
local ninjumpinvely=-3
local ninblownoutvely=-3
local ninthrowbombvely=-4
local nininvul=180

function update_nin_invul(o)
 if o.invul then
  o.invul=o.invul-1
  if o.invul<=0 then
   o.invul=nil
  end
 end
end

function nin_update_held_bomb(o)
 local b=o.bomb
 if b then
  if obj_dead(b) then
   o.bomb=nil
  else
   b.x,b.y=o.x,o.y-4
  end
 end
end

function cam_on_nin(o)
 local vy=mid(-camtopspd,o.y-96-cam.y,camtopspd)
 cam.y=min(cam.y+vy,worldbtm-128)
end

function update_nin_death(o)
 update_obj_ani(o,sprs.ninja.blownup)
 o.vy=o.vy+ningrav
 o.y=o.y+o.vy
 if o.y>cam.y+128 then
  if o.life>0 then
   o.dying=nil
   o.y=cam.y+128
   start_nin_jumpin(o)
  else
   sfx(-1)
   music(snds.deadmus)
   kill_obj(o)
  end
 end
end

function nin_start_dying(o)
 o.life=o.life-1
 if o.life<=0 then
  music(-1)
 end
 o.vx=0
 o.vy=ninblownoutvely
 o.dying=true
 o.update=update_nin_death
 if o.bomb then
  bomb_explode(o.bomb)
  o.bomb=nil
 end
end

function nin_hit_objs(o)
 if not o.invul then
  obj_explode_bombs(o,enbombs)
  if obj_hit_any_expl(o) then
   nin_start_dying(o)
  end
 end
end

function nin_coll_vy(x,y,w,h,vy)
 local bnd,edge,cmp

 if vy<0 then
  bnd=-10000
  edge=y
  cmp=max
 elseif vy>0 then
  bnd=mapbtm
  edge=y+h
  cmp=min
 else
  return 0
 end

 for x=x,x+w-1,w-1 do
  local cbnd=cell_bound(x,edge,1,vy)
  if cbnd then
   bnd=cbnd
   break
  end
 end
 return cmp(0,bnd-edge)
end

function nin_coll_vx(x,y,w,h,vx)
 local bnd,edge,cmp

 if vx<0 then
  bnd=0
  edge=x
  cmp=max
 elseif vx>0 then
  bnd=128
  edge=x+w
  cmp=min
 else
  return 0
 end

 -- check only his top corner against block
 -- so he can mantle onto block's top corner
 -- for y=y,y+h-1,h-1 do
  local cbnd=cell_bound(edge,y,0,vx)
  if cbnd then
   bnd=cbnd
   -- break
  end
 -- end
 return cmp(0,bnd-edge)
end

function nin_move_x(o)
 local inx=
  (btn(⬅️) and -1 or 0) +
  (btn(➡️) and 1 or 0)
 local vx,vy=o.vx,o.vy
 if inx==0 then
  if vx<0 then
   vx=min(vx+ninrunaccel,0)
  elseif vx>0 then
   vx=max(0,vx-ninrunaccel)
  end
 else
  vx=mid(-nintoprunspd,vx+inx*ninrunaccel,nintoprunspd)
 end
 vx=vx+nin_coll_vx(o.x+vx,o.y,o.w<<3,o.h<<3,vx)
 o.vx=vx
 o.x=o.x+vx
end

function nin_climb_y(o)
 local iny=
  (btn(⬆️) and -1 or 0) +
  (btn(⬇️) and 1 or 0)
 local vy=o.vy
 if iny==0 then
  if vy<0 then
   vy=min(vy+ninclimbaccel,0)
  elseif vy>0 then
   vy=max(0,vy-ninclimbaccel)
  end
 else
  vy=mid(-nintopclimbspd,vy+iny*ninclimbaccel,nintopclimbspd)
 end
 local collvy=nin_coll_vy(o.x,o.y+vy,o.w<<3,o.h<<3,vy)
 vy=vy+collvy
 o.vy=vy
 o.y=o.y+vy
 return collvy
end

function nin_drop_y(o)
 local vx,vy=o.vx,o.vy
 vy=min(vy+ningrav,nintopfallspd)
 vy=vy+nin_coll_vy(o.x,o.y+vy,o.w<<3,o.h<<3,vy)
 o.vy=vy
 o.y=o.y+vy
end

function update_nin_flpx(o,dirx)
 if dirx~=0 then
  o.flpx=dirx<0
 end
end

function update_nin_air_ani(o)
 update_nin_flpx(o,dir_input_x())
 update_obj_ani(o,sprs.ninja.jump)
end

function update_nin_ground_ani(o)
 local inx=dir_input_x()
 update_nin_flpx(o,inx)
 local ani
 if inx~=0 then
  ani=sprs.ninja.run
 else
  ani=sprs.ninja.idle
 end
 update_obj_ani(o,ani)
end

function nin_try_jump(o,holdok)
 if holdok and btn(🅾️) or btnp(🅾️) then
  o.vy=ninjumpvely
  o.update=update_nin_air
  return true
 end
end

function nin_catch_allowed(o)
 return not o.bomb and not o.dying
  and not o.climbing
end
function nin_catch_box(o)
 local w,h=o.w<<3,o.h<<2
 return o.x,o.y-h,w,h
end

function nin_find_catch_bomb(o)
 local x,y,w,h=nin_catch_box(o)
 for i=1,#enbombs do
  local b=enbombs[i]
  if not b.cantcatch
  and aabbs(x,y,w,h,
   b.x,b.y,
   b.w<<3,b.h<<3)
  then
   return b,i
  end
 end
end

function nin_find_coming_bomb(o)
 local bomb,dst=nil,0x7fff
 local x,y,w,h=nin_catch_box(o)
 local warnh=96
 y,h=y-warnh,h+warnh
 local heldbomb=o.bomb
 for b in all(enbombs) do
  if b~=heldbomb
  and not b.cantcatch
  and aabbs(x,y,w,h,
   b.x,b.y,
   b.w<<3,b.h<<3)
  then
   local d=y+warnh-b.y-(b.h<<3)
   if d<dst then
    bomb,dst=b,d
   end
  end
 end
 return bomb,dst
end

function nin_try_catch_bomb(o)
 if not btnp(⬆️) then
  return
 end
 local bomb=o.bomb
 if bomb then
  o.bomb=nil
  add(ninbombs,bomb)
  start_bomb_thrown(bomb)
  bomb.vy=ninthrowbombvely
  sfx(snds.throw)
 else
  local bi
  bomb,bi=nin_find_catch_bomb(o)
  if bomb then
   bomb.vx=0
   bomb.vy=0
   o.bomb=bomb
   swappop(enbombs,bi)
   start_bomb_fuse(bomb)
   sfx(snds.catch)
   return bomb
  end
 end
end

function update_nin_climb_ani(o)
 local iny=dir_input_y()
 local ani
 if iny~=0 then
  ani=sprs.ninja.climb
 else
  ani=sprs.ninja.hang
 end
 update_obj_ani(o,ani)
end

function update_nin_climb(o)
 o.climbing=true
 nin_hit_objs(o)
 if o.dying then
  o.climbing=nil
  return
 end
 update_nin_invul(o)
 if nin_try_jump(o) then
  o.vx=nintoprunspd*dir_input_x()
  o.climbing=nil
 else
  local collvy=nin_climb_y(o)
  if collvy<0 then
   o.update=update_nin_ground
   o.climbing=nil
  elseif not obj_ladder(o) then
   o.update=update_nin_air
   o.climbing=nil
  end
 end
 update_nin_climb_ani(o)
 cam_on_nin(o)
end

function nin_try_climb(o)
 local iny=dir_input_y()
 local climbldr=iny~=0
  and not o.bomb
  and obj_ladder(o)
 if climbldr then
  o.jumpagain=nil
  o.x=climbldr
  o.vx=0
  o.vy=min(0,o.vy)
  o.update=update_nin_climb
  return true
 end
end

function update_nin_air(o)
 nin_try_catch_bomb(o)
 nin_hit_objs(o)
 if o.dying then
  return
 end
 update_nin_invul(o)
 nin_drop_y(o)
 nin_move_x(o)
 o.jumpagain=o.jumpagain or btnp(🅾️)
 if nin_try_climb(o) then
 elseif obj_ground(o) then
  if o.jumpagain and nin_try_jump(o,true) then
  else
   o.update=update_nin_ground
  end
  o.jumpagain=nil
 end
 nin_update_held_bomb(o)
 update_nin_air_ani(o)
 cam_on_nin(o)
end

function update_nin_ground(o)
 nin_try_catch_bomb(o)
 nin_hit_objs(o)
 if o.dying then
  return
 end
 update_nin_invul(o)
 nin_move_x(o)
 if nin_try_climb(o) then
 elseif nin_try_jump(o) then
 else
  local grnd=obj_ground(o)
  if grnd then
   o.vy=grnd-o.y-(o.h<<3)
   o.y=o.y+o.vy
  else
   o.update=update_nin_air
  end
 end
 nin_update_held_bomb(o)
 update_nin_ground_ani(o)
 cam_on_nin(o)
end

function update_nin_jumpin(o)
 local vy=o.vy
 vy=min(vy+ningrav,nintopfallspd)
 o.vy=vy
 o.y=o.y+vy
 update_nin_air_ani(o)
 if o.vy>0 then
  o.update=update_nin_air
  return
 end
end

function start_nin_jumpin(o)
 o.vx=0
 o.vy=ninjumpinvely
 o.invul=nininvul
 o.update=update_nin_jumpin
end

function update_plane_flyin(o)
 o.vx=o.vx+o.ax
 o.vy=o.vy+o.ay
 o.x=o.x+o.vx
 o.y=o.y+o.vy
 if o.nin then
  o.nin.x=o.x+4
  o.nin.y=o.y-4
  -- cam_on_nin(o.nin)
 end
 if o.vy>=0 then
  o.ax=abs(o.ax)
  if o.nin then
   start_nin_jumpin(o.nin)
   o.nin=nil
  end
 end
 if o.x>128 then
  kill_obj(o)
 end
 update_obj_ani(o,
  o.vy<-1 and sprs.plane.up
  or o.vy>1 and sprs.plane.down
  or sprs.plane.fwd)
end

function add_plane()
 return add_obj_spr {
  ani=sprs.plane.up,
  x=-16,y=cam.y+88,
  w=2,h=2,
  vx=2.5,vy=-2,
  ax=-1/32,ay=.0625,
  nin=ninja,
  update=update_plane_flyin
 }
end

function draw_ninja(o)
 if (o.invul or 0)%2==0 then
  draw_obj_spr(o)
 end
 if nin_catch_allowed(o)
 or o.climbing then
  local b,d=nin_find_coming_bomb(o)
  if b then
   d=max(0,d)
   local x,y,w,h=nin_catch_box(o)
   local clr,fp=5,░
   if d<32 then
    clr,fp=7,█
   elseif d<64 then
    clr,fp=6,▒
   end
   pal()
   fillp(fp)
   oval(x-(d<<1),y-d,x+w+(d<<1),y+h+d,clr)
   if o.climbing then
    print("⬅️\n🅾️",o.x-8,o.y,7)
    print("➡️\n🅾️",o.x+8,o.y,7)
   else
    print("⬆️",x+1,y,clr)
   end
   fillp()
  end
 end
end

function add_ninja()
 return add_obj_spr {
  x=-8,y=worldbtm-8,
  ani=sprs.ninja.duck,
  life=ninstartlife,
  draw=draw_ninja
 }
end

function draw_life(life)
 pal()
 local y=121
 local x=0
 for i=1,life do
  spr(sprs.heart,x,y)
  x=x+8
 end
end
-->8
--enemy

local enemyrunspd=2
local enemyhopvely=-2
local enemyhopgrav=1/32
local enemyhurtvely=-2
local enemyhurttime=60
local enemyjumpvely=-4

function enemy_hit_objs(o)
 obj_explode_bombs(o,ninbombs)
 if obj_hit_any_expl(o) then
  start_enemy_hurt(o)
  return true
 end
end

function update_enemy_shot(o)
 if enemy_hit_objs(o) then
  return
 end
 if obj_ani_ending(o) then
  start_enemy_move(o)
  return
 end
 update_obj_ani(o,o.sprset.throw)
end

function start_enemy_shot(o)
 o.vx=0
 o.vy=0
 o.readytofire=nil
 o.update=update_enemy_shot
 update_obj_ani(o,o.sprset.throw)
 local b=add_bomb({
  target=ninja,
  x=o.x+(o.w<<1),
  y=o.y+(o.h<<1)
 },o.bombtmpl)
 if o.flpx and b.vx then
  b.vx=-b.vx
 end
 add(enbombs,b)
end

function set_enemy_level(o,l)
 o.level=l
 local lvl=enemylevels[l]
 if lvl then
  for k,v in pairs(lvl) do
   o[k]=v
  end
  o.ladderdrops=scopy(o.ladderdrops)
 end
end

function update_enemy_jump(o)
 local ladders=o.ladderdrops
 if ladders then
  local ly=o.laddery or o.y
  for i,lx in ipairs(ladders) do
   if lx then
    local c,r=room_cell(lx,ly)
    local spri=mget(c,r)
    if fget(spri, breakflag)
    or not fget(spri,solidflag) then
     mset(c,r,sprs.ladder)
    else
     ladders[i]=false
    end
   end
  end
  o.laddery=ly+8
 end
 o.y=o.y+o.vy
 update_obj_ani(o,o.sprset.jump)
 if o.y<=o.floory then
  o.y=o.floory
  o.laddery=nil
  set_enemy_level(o,o.level+1)
  start_enemy_move(o)
 end
end

function start_enemy_jump(o)
 o.vy=enemyjumpvely
 o.floory=o.y-128
 o.update=update_enemy_jump
end

function update_enemy_getup(o)
 update_obj_ani(o,o.sprset.getup)
 if obj_ani_ending(o) then
  start_enemy_jump(o)
 end
end

function update_enemy_dying(o)
 update_obj_ani(o)
 local i=(o.dyingtime or 0)+1
 o.dyingtime=i
 if i<=300 then
  if i%6==0 then
   add_normal_expl(o.x+rnd(16),o.y+rnd(16))
  end
  if i==300 then
   hazeptn=░
   hazeclr=0
  end
 elseif i==330 then
  hazeptn=▒
 elseif i==360 then
  hazeptn=█
 elseif i==480 then
  kill_obj(o)
 end
end

function enemy_change(o)
 add_obj_spr({
  x=o.x,y=o.y,
  w=o.w,h=o.h,
  flpx=o.flpx,
  ani=o.ani,
  draw=function(o)
   fillp((o.age<=20
    and ▒ or ░)
    +0b.01)
   draw_obj_spr(o)
   fillp()
  end,
  update=function(o)
   update_obj_ani(o)
   o.y=o.y-.5
   if o.age>40 then
    kill_obj(o)
   end
  end
 })
 o.sprset=sprs.enemyskull
end

function update_enemy_hurt(o)
 update_obj_ani(o,o.sprset.knocked)
 o.vy=o.vy+ningrav
 o.y=o.y+o.vy
 if o.y>=o.floory then
  o.y=o.floory
  local time=o.time or 0
  time=time+1
  if time>enemyhurttime then
   o.time=nil
   if o.level<#enemylevels then
    if o.level+1==#enemylevels then
     enemy_change(o)
    end
    o.update=update_enemy_getup
    local mus=enemylevels[o.level+1].music
    if mus then
     music(mus)
    end
   else
    o.update=update_enemy_dying
   end
  else
   o.time=time
  end
 end
end

function enemy_say_taunt(o)
 local taunt=o.taunt
 if taunt then
  o.taunt=nil
  add_obj_text {
   text=taunt,
   x=0,y=16,
   clr=8,
   update=function(o)
    o.x=cam.x
    o.y=cam.y+24
    if o.age>=180 then
     kill_obj(o)
    end
   end
  }
 end
end

function start_enemy_hurt(o)
 o.vx=0
 o.vy=enemyhurtvely
 o.update=update_enemy_hurt
 ninja.invul=180
 if o.level>=#enemylevels
 or enemylevels[o.level+1].music then
  music(-1)
 end
 sfx(snds.expl2)
end

function enemy_move_x(o)
 local x=o.x+o.vx
 if x>=104 then
  o.x=104
  o.vx=-o.vx
  o.readytofire=true
 elseif x<8 then
  o.x=8
  o.vx=-o.vx
  o.readytofire=true
 else
  o.x=x
 end
 o.flpx=o.vx<0
end

function enemy_hop_y(o)
 local vy=o.vy+enemyhopgrav
 local y=o.y+vy
 if y>=o.floory then
  y=o.floory
  vy=enemyhopvely
 end
 o.y,o.vy=y,vy
end

function enemy_try_fire(o)
 if ninja.y<o.y+128
 and o.readytofire then
  local firedistx=o.firedistx or 2
  local distx=abs(
   ninja.x+(ninja.w<<2)
   -(o.x+(o.w<<2)))
  if distx<firedistx
  and o.y==o.floory then
   start_enemy_shot(o)
   return true
  end
 end
end

function update_enemy_hop(o)
 if enemy_hit_objs(o) then
  return
 end
 if ninja.y<o.floory+128 then
  enemy_say_taunt(o)
 end
 if enemy_try_fire(o) then
  return
 end
 enemy_move_x(o)
 enemy_hop_y(o)
 update_obj_ani(o,
  o.vy<0 and o.sprset.jump
  or o.sprset.drop)
end

function update_enemy_run(o)
 if enemy_hit_objs(o) then
  return
 end
 if ninja.y<o.y+128 then
  enemy_say_taunt(o)
 end
 if enemy_try_fire(o) then
  return
 end
 enemy_move_x(o)
 update_obj_ani(o,o.sprset.run)
end

function start_enemy_move(o)
 o.vx=o.flpx and -enemyrunspd
  or enemyrunspd
 o.vy=0
 o.readytofire=nil
 o.update=o.movefunc or update_enemy_run
end

enemylevels={
 [1]={
  bombtmpl=bombtmpls.normal,
  ladderdrops={32,88},
  movefunc=update_enemy_run,
  taunt="          here, catch!          "
 },
 [2]={
  bombtmpl=bombtmpls.split,
  ladderdrops={24},
  movefunc=update_enemy_run,
  taunt="        double your pain!       "
 },
 [3]={
  bombtmpl=bombtmpls.fwall,
  ladderdrops={104},
  movefunc=update_enemy_run,
  taunt="          you're toast!         "
 },
 [4]={
  bombtmpl=bombtmpls.fbomb,
  firedistx=128,
  ladderdrops={16,32,48,72,88,104},
  movefunc=update_enemy_hop,
  taunt="       x marks your grave!      "
 },
 [5]={
  music=snds.finalmus,
  bombtmpl=bombtmpls.fbomb,
  movefunc=update_enemy_run,
  firedistx=32,
 }
}

function add_enemy(lvl)
 lvl=lvl or 1
 local o=add_obj_spr{
  x=56,y=mapbtm-128*(1+lvl),
  vx=0,vy=0,
  w=2,h=2,
  sprset=sprs.enemy
 }
 o.floory=o.y
 set_enemy_level(o,lvl)
 start_enemy_move(o)
 return o
end
-->8
--game phases

local stars={}
for y=16,80,32 do
 for x=16,80,32 do
  add(stars,x+rnd(32))
  add(stars,y+rnd(32))
 end
end
local clouds={}
for y=120,128,8 do
 for x=0,112,16 do
  add(clouds,x+rnd(16))
  add(clouds,y+rnd(16))
 end
end

function draw_sky()
 pal()
 fillp(░)
 rectfill(0,104,128,108,1)
 fillp(▒)
 rectfill(0,108,128,128,1)
 fillp()
 circfill(96,24,4,6)
 for i=2,#stars,2 do
  pset(stars[i-1],stars[i],6)
 end
 for i=2,#clouds,2 do
  local x,y=clouds[i-1],clouds[i]
  circfill(x,y,16,13)
  circfill(x-1,y+1,16,1)
 end
end

function draw_credits()
 cls()
 draw_sky()
 draw_objs()
end

function update_credits()
 update_objs()
 if btnp(🅾️) then
  start_title()
 end
end

function start_credits()
 camera()
 clear_game_objs()
 add_obj_text {
  x=16,y=128,
  update=function(o)
   local vy=(btn(⬆️) or btn(⬇️))
    and -1 or -.125
   o.y=max(8,o.y+vy)
  end,
  text=[[
A toy box jam 2023 GAME

CONCEPT
DESIGN
PROGRAM

 iori branford

GRAPHICS
SOUNDS

 tom hall
 lafolie
 toby hefflin

MUSIC

 gruber
]]
 }
 add_obj_text {
  text="🅾️close",
  x=96,y=120
 }
 music(snds.endmus)
 _update60=update_credits
 _draw=draw_credits
end

function update_game()
 update_objs()
 cleanup_dead_objs()
 cleanup(enbombs,obj_dead)
 cleanup(ninbombs,obj_dead)
 cleanup(expls,obj_dead)

 if obj_dead(ninja) then
  if btn()&0x3f~=0 then
   start_title()
  end
 elseif obj_dead(enemy) then
  start_credits()
 end
end

function draw_game()
 cls()
 draw_sky()
 camera(cam.x,cam.y)
 draw_objs()
 camera()
 draw_life(ninja.life)
 if hazeptn and hazeclr then
  fillp(hazeptn)
  rectfill(0,0,128,128,hazeclr)
 end
end

function start_game()
 local lvl=nil
 cam.x=0
 cam.y=worldbtm-128
 if lvl then
  cam.y=cam.y-128*lvl
 end
 clear_game_objs()
 add_rooms()
 enemy=add_enemy(lvl)
 ninja=add_ninja()
 add_plane()
 poke(0X5F5C, 255)
 music(snds.gamemus)
 _update60=update_game
 _draw=draw_game
end

function update_title()
 if btnp(🅾️) then
  start_game()
 elseif btnp(❎) then
  start_credits()
 elseif btnp(⬅️) then
  ninstartlife=max(1,ninstartlife-1)
 elseif btnp(➡️) then
  ninstartlife=min(ninmaxstartlife,
   ninstartlife+1)
 end
end

function draw_title()
 cls()
 draw_sky()
 draw_objs()
 draw_life(ninstartlife)
end

function start_title()
 reload(0x2000,0x2000,0x1000)
 pal()
 poke(0X5F5C, 0)
 clear_game_objs()
 camera()
 add_obj_text {
  x=8,y=48,
  text=[[
rise to the commander
  of the wolf triad

⬅️/➡️ set starting life 1-9
🅾️ start game
❎ view credits
]]
 }
 music(snds.titlemus)
 _update60=update_title
 _draw=draw_title
end

_init=start_title

__gfx__
000120000000000000000000000000000000000000000000000000000aaaaa000aaaaa00aa000aa00aaaaaa0099099000b0dd030777777674f9f4fff7999a999
07d12570000000000000000000000000000000000000000000000000a99999a0a99999a099a0a990a99999909aa9aa90d3000b0d76777777fffff9f49999979a
057d57d000000000000000000000009eeeee000000000088888800009900000099000990999a9990990000009a999a90000b030077777677ff4fffff99a99999
22566d1100000000000000000000009eeeeee00000000022228880009900aaa0990009909949499099aa000049444940b0030000777677779fff9ff999997997
11d66522000000888888e004000000488888800400000044444440049900099099aaa9909904099099000000049494003000dd0b677777774fffff9fa9999979
0d75d75088800002228880040000008888888004000000229229200449999940994449909900099049999990004940000b00000377777776ff4fffff999a9999
07521d708a980000900976748884004888888674888400288228867404444400440004404400044004444440000400000300b00076777777ff9ff9ff99999799
0002100089a88888800885678998884888888567888888888888856700000000000000000000000000000000000000000dd030b077776777f9ffff4f979999a9
00000000088888888888811408888822228801140ff88884444441140aaaaa00aa000aa00aaaaaa0aaaaaa000990990000666000006600000060000000000000
0000f0000ff00004444440040000000444444004000000044f7f4004a99999a099000990a9999990999999a09229229006666660066650600665000000600000
00066000000000000f7f00040000000000f000040000000000f00004990009909900099099000000990009909222229006666060056650000050000000000000
005555000000000000f0000000000000000000000000000000000000990009909900099099aa0000990009404222224066666666005506600000000000000000
0555555000000000000000000000000000000000000000000000000099000990499099409900000099aaa9000422240050666665600566650006006600000000
01555510000000000000000000000000000000000000000000000000499999400499940049999990994449900042400006665555006656650000066500000060
00111100000000000000000000000000000000000000000000000000044444000044400004444440440004400004000005555550066505500660065000000050
00011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000550000005000000650000006000000
0bb3b3b030bbb0030150051001500510000000000000000000000000000000007667060000065000d777777dd55550000076dc0000999900000000000007d000
bb3b3b350bbb3300157556511575515100020000000200000000000000000000641605000065d650566666657665d650075555d0094444900000000000766d00
b3b33333bb3bbb305757651557576515000220200000000000000000000200006666666065616560566666657661656001c6dc109444444900000000076666d0
b3333335b3b3b33505766650057656500000000000000000002820000002202011111156006176d011111155766176d007cc6d50999aa9990000000000044000
0b4334503bbb3b3505666650056565500000000000288800208980000008202276d176d57661110076d176d57661110007cc6d50955aa5590007d00000094000
0009450033b3b355575665155516551502228800228988000089808000282000656165606161d650656165607661d65007cc6d509544444900766d0000094000
0009450003335550156551511155515128988800228998200288888802882000d650d65064616560d650d6507661656007cc6d5095444449076666d000094000
095454540033350301500510015005108899982002289882022888982899880000000000766176d000000000d55176d00066d500999999990004400000094000
000990000777770000077000007dd500288999820288989822888822899998200007000099999999750705607776777677777776777777767777777677777776
049aa94075666660007667000007500002899998088999882999982089999980007a900090040405565656507665766576666665766666657766665576666665
49a99a940065d56000077000077665500899999809999982899999802899998207aaa90094444445057775007665766576555565766776657676656576666665
9a9aa9a90066666007666670776666552899979809999982089799820889999807aaa90090004005767766606555655576566765767665657667566576666665
9a9aa9a900655d607655556776666665299779920897799008997798089977980a99990094444445057665007677767776566765767665657667566576666665
49a99a94006666606500005676666665289777920297779008977790089777927556559095555555565656506576657676577765766556657676656576666665
049aa940006777775650056577666655029777920097779000977790029777920aaaa90000055000750605606576657676666665766666657766665576666665
00499400005555500567765007766550008979800089798000897980008979800000000005064005000000005565556565555555655555556555555565555555
00000000000005d9007a4200000000000000000900009999900a000000000000000000000049400000040000a7a9999900076000000000000001000000000000
0e82e82000555d5507a9942000000000000909aa009999aa09000a900009000009009090049a94000049400004a994400007610000111000001c10000eeeee20
e788888205d6d5550a999940000000000000aaaa09a9aaaa00009000008aa800008aa80049a7a940049a9400097999400007610001ccc10001c7c1007262626c
e88888825d7ddd500a99994000000009090a9a9a099a9909a000000000a77a9009a77a009a777a9449a7a94009a99990707765071c777c1001c7c10015252520
0888882056dddd500a9999400000a09a00a9a9a999a997900090000009a77a0000a77a9049a7a940049a9400099a99407667665601ccc10001c7c10002e50000
0088820055ddd5500ae999400000099a09aa9a7799a970000a000000008aa800008aa800049a940000494000009994007676656500111000001c10005e200000
000820000555550007fe9420000099a70aa9a7779aa090000900000000009000090900900049400000040000000a900007655651000000000001000025200000
0000000000555000007942000009aa779aaa97779aa90000000000000000000000000000000400000000000007a9994000766510000000000000000000000000
000000000222200000222200000000000000000000ddd00000000000000330000000222222220000000222222220000000000000000000002888888212888821
88800000222222000222222000002200000002200d666d0003333330033bb33000224444444422000224444444422000000222222222000088eeee88288ee882
8888800022222220022222220020222000000220d67666d033bbbb3333b77b330024444444494200024444001491200002244444444422008ea77ae888eaae88
8888880022222220222882220020222000200000d66666d03b7777b33b7777b30224444444444420224444400141420002444444444942008e7777e88ea77ae8
9998880088822220222888220000222000000000dd666d503b7777b33b7777b30224444111444120224444444214420022444444444444208e7777e88ea77ae8
99998800888822202822882200000000000000000dddd50033bbbb3333b77b330224444001140120224444442411410022444444444444208ea77ae888eaae88
99998800888822202222222220000200000000000055500003333330033bb33001224444001401201224442244442000224444400144412088eeee88288ee882
99998800888222002220222020000000000000000000000000000000000330000122222444412420112222244444200012244444001401202888888212888821
999880002822200022200000000000000000000050222205bb0bb0bb0b0bb0b000112224422122100112111299a0900012222244444124200076660000766600
99980000882200008222000002200000000000000528825003abba30b3abba3b0001111224112000001110111000000001122224221122100712826007282160
9980000082220000222200000220000000200000225885220bbbbbb00bbbbbb00000111244442000001100110000000000111112444421000612825006282150
88800000222200002222000000000000000000002718817203baab3003baab300000111499a0900000110011000000000000101499a090000066550000665500
880000002220000022200000000000000000000028888882b003300b0003300000011200110000000122001200000000000010199a99900007d75d6007d75d60
0000000000000000000000000000000000000000288188820b3bb3b00b3bb3b00001144001400000011440124000000000001122444420007d7dd5d67d7dd5d6
00000000000000000000000000000000000000009288882000bbbb00b0bbbb0b0000024901190000000249012900000000001111222200007d7dd5d57d7dd5d5
0000000000000000000000000000000000000000092222990bb33bb000b33b00000000229a9000000000249a9000000000000000000000000665565006655650
08000080a00700b00056650000077000004aa4000077770000777700000000076776d7765000000000d7cd0009aaaa900000567700a7777d0007700000077000
0000000007a00bba056766500076650044a77a4407666670000666700000007676675665650000000d77ccd09a1aa1a9000567760a6666dd0076670000700700
00880800077bba7b5676666500766500aa7777aa71166117a0776657000007667667566566500000d777cccd9a5aa5a905677775a7777d5d0766667007000070
8008e808b0b7aab067666666007665004aa77aa4712662177a6666660000766676675665666500007777cccc9aaaaaa95677775076666d5d7666666770000007
008ee80000ba7ab0666666660076650004a77a40066116606d666666000766667667566566665000dcccdddd09affa900567777676666d5d0005500000077000
000888000b7b77ab56666665007665004a7aa7a405666650d05661150076666676675665666665000dccddd09a9aa9a95677766576666d5d0006600000700700
000000800ab0b7aa05666650076666504aa44aa4006116000006665007666666766756656666665000dcdd00a900009a6777655076666dd00006600007000070
08008000ab0000a00056650006555550aa4004aa0056650000665000766666666552155666666665000dd0009a9009a9776650006ddddd000006600070000007
2002821000028210202000000006822d02822222020220d000000000000000000000000000000000007665000076650005555555555555555555555055677655
0211111122111111022282100026cdcd1111110002200d0000000000000000000000000000000000075006500750065055666666666666666666665556555565
11ddcdcd01ddcdcd001111110216ddddddcdcddd21ddd00002000000000000000000000000000000065006500650000056676767676767676767766556677665
006ddddd106ddddd66ddcdcd0016dddd66666d0081cddd0022ddd000000000000000000000000000766666657666666556777777777777777777776556677665
006d5ddd006d5ddd600ddddd0015ddd066dddd001ddddd008dddd000002282000202820002222200766166657663666556777676767676767676776555677655
0065111d0065111d0005ddd00052111056d111111c66d1111dddd1000221166600211110002282dd766166657663666556766676666666666767766556555565
00520010005200100552211100520010052200000d6661001d66611100666c10011dddd000111110766666657666666556776756666666667577666556677665
0502001005020010500200100502001000502000000552221d666222666dddc066666666666dddd0655555556555555556766665555555555667766556677665
0028210020000000002821002200000002228200005000000000000000000000c0c6cc0000777700056650000000000056677665555575555566765555555555
02111110222821000211111002282100221116660205002002022210202221000cccccc0071111605600650007a00a7056776665565755665555555556677665
d21ddcd60111111021ddcdcd0111111000666c10022560220022822102282210cdd7d7d071111115607006000a9009a056677665565757676565565655555555
d1dd66660ddddcd0666ddddd0dddcdc0066dddcd101d5682011111111111111006ddddd071100115600006000000000056776665575757777576755757777775
00d66d00066dddd06066dd00066dddd05555dd0011ddd62206ddcdcd0ddcdcd00d665ddd71100115560065000000000056677665575756766557675675555557
202211000066dd00001221000066dd00021dd00000dd661260d5dddd6d5dddd000c5ccc071111115056694500a90000056776665565756666565565655677655
02000010002212000110020000221100200100000dd6dc116552ddd16522dd11005c00c0061111500000094507a0000056677665565755665555555556776665
0000000100012000000000200002100000100000d000c1105220011152220001050c00c000555500000000940000000056776665555575555567665556677665
0028226000000000628210000022000022000000222200001112000006822d0026822d0077777777002820000077770056776675555755555677666556776665
002222600028220026111100081d0000820d0000228110001112800026cdcd0016cdcd0000000000028e8200076566d056676756665575656577666556677665
061221600022222006dcdc00621d0000612d000011dcd00011dc600016dddd0006dddd000600600608e7e8007665666d56777667676575657667766555776655
06d11dd0061221160ddddd00611c0200611c0200d66665d5dddd656506dddd0006dddd000000000008eee8007665556d56677777777575757777766575555557
0dd1d1d00dd11ddd05dddd006cdd52016cdd5201dddd0d00ddd6060005ddd00005ddd00000500500028e82007666666d56667676767575756767666557777775
005111000dd1d1dd522dd0d0d66d5211d6665211211100001112000005221110052211100000000000282000076666d056666666666575656666666555555555
0015000000551110220100000d6652100dd6521020001000100020005002000150020001010100100028200000dddd0055666666665575656666665556677665
00105000001051000110000000dd510000dd51002000010010000200500000005000000000000000002820000000000005555555555755555555555055555555
062281100000000000400000202821000028210000282100000000000000000000000000000000007777777711111100566666660015d0005666666500000000
6d6dcdc00000122240900040111111102111111021111110030100000606330000003300000000007555555717777610655115510015d0006666666600000000
506dddd0000dd18090a040900ddbdbd00ddbdbd01ddbdbd003013300663138300031383000077000756556571777610065155551001d50006000000601111110
506dddd0000ddd11a00090a40666dddd1666dddd0666dddd00313830633313300633133000766700755555571776610051155551000d15006000000605555550
5006ddd000ddddd10405a00900d5dd0000d5dd0000d5dd00003313303331301363313013005665007555555717667610655115110001d5006000000605555550
00021111002d6dd00905004a005111000052110000521100033130131110000011100000000550007565565716116761655551510001d0006000000605155150
000200010222166d0a5000900520001005002000052201001110000010000000100000000000000075555557010016716555515100105d006000000605111150
0002000020011006dd1110a05020000050010000500001001000000000000000000000000000000077777777000001105111111500150d000000000005111150
0000000dddd000000000000000000000000000dddd0000000000000000000000000000dddd000000000000000000000000000dddd00555000000000000000000
000000daa3ad00000000000dddd0000000000daaa3d00000000000dddd00000000000daa3ad000000000000dddd00000000666a3ad0555000000000dddd00000
00000daaaa3ad000000000daa3ad00000000daaaaa3d000000000daa3ad000000000daaaa3ad0000000000daaa3d0000000666aa3ad05500000000daa3ad0000
00000daaaa3ad00000000daaaa3ad0000000daaa666d00000000daaaa3ad00000000daa6666d066600000daaaaa3d000000566666650550000000daaaa3ad000
000005aa6666500000000daaaa3ad00000005aa6717005550000daa6666d000000005a671710066600000daa6666d0000005667171055500000005aa66665000
000005a671710000000005aa6666500000005a677775055500005a67171000000000567777750066000005a6771700000005227777555000000055a671710000
0000005777775000000005a671710000062225777775005500005677777500000055557777752666000005677777500000052277775500000062225777775000
00022225777550000000005777775000062222555555555500000577777500000555525552222660000005577777506600002255552000000062222577755000
00622222555200000002222577750055066002222225555500000255555005500550022222222000000055555555006600002222222000000066222255550000
00660022222250550062222255525055066602222200000000002226622505500555002222200000000055222222266600000222222000000066622222220000
00666022222555550066002222255555066602222200000000002266622555500555002222200000000055222222266000000022cccc00000066602222200000
0066602222205555006660222220550000000111ccc000000000666662555500000780ccc110000000000022222000000000001ccccc000007777cccc1100000
000000ccc1100000006660ccc1100000052101110ccc0000000052111150000000788ccc01110000000078222200000000000011008800000008cccc11100000
00000ccc0111000000000ccc011100000521111000ccc0700000521cc00000000070ccc000111050000788cccc00000000052111007777000000ccc001110500
000088800022000000008880022200000501110000088870000550088000000000000c000001225000070ccc22000000000521110000000000000c0000222500
00077770005555000007777000555500000000000000770000000007777000000000000000005500000000005555000000050000000000000000000000055000
0000000000000000000000000000000000000000000000000005555000007777000000000000000000000dddd0000000000000dddd0000000000000000000000
0000000000000000000000000000000000000000000000000000022000c8880000000000000000000000da3aad00000000000daa3ad000050000000000000000
0000000dddd00000000dd55000000000000000000000000000000110cccc00005555500000000700000da3aaaad006000000daaaa3ad0050000dd55000000000
000000daa3ad000000daa3a50555000007700000000000000000011cccc000005555550000000700000da3aaaad006600000daaaa3ad055000daa3a500000000
00000daaaa3ad0000daaaa6166650000008705550000000000000022222055505500555000cc8700000da3aaaad0666600005aa6666555550daaaa6150555000
00000daaaa3ad0000daaa617666500000c880555555dd000000000222220555000d5055222cc87000005a3aaa650066000005a67171555000daaa61700555000
000005aa666650000daa6777566500000cc2222556aaad0000000022222005500da6177522cc0000000056666750666000000577777550000daa677750055000
000005a6717100000daa6775266000000cc22225776aaad00000222555555550da36777522cc0000055555777522660000222257775500000daa677755555000
000002277777500000da67522200000052c222257776aad00066225777555550d3a61775222c111005555555522200000622222555250000006a677525550000
0000022277665550000555222200000052c222257776aad00666057666650000daa6777522211110055002222200000066600222222000000006655225000000
00000222666655500000052222200000500022257716a3d00660056aaa3a5000d666622222000110055502222200000066000022222000000000222222100000
000000226666555000000022222000005000022577763d0066660daaaa3ad0000666622220000220055502222200000066600022222000000002222221110000
000000cc11110000000000cc1111000000000220550dd00006600daaaa3ad000066555500000055500000cccc1100000666000ccc11100000002205ccccc1000
0000ccccc011100000000cccc0111000000006600000000000600daaaa3ad00000000000000000000000cccc011000000000cccc00111000006620000ccc1000
000888c000222000000888cc002220000000066660000000000000daa3ad0000000000000000000000888c0002200000000888c0002220000066000088822200
0777700000555500077770000055550000000066600000000000000dddd000000000000000000000777700000555500007777000006666006666000077775555
__label__
07700770077070000000077007707700777000007070777077707770000000000000000000000000000000000000000000000000000000000000000000000000
70007070707070000000700070707070700000007070700070707000000000000000000000000000000000000000000000000000000000000000000000000000
70007070707070000000700070707070770000007770770077007700000000000000000000000000000000000000000000000000000000000000000000000000
70007070707070000000700070707070700000007070700070707000000000000000000000000000000000000000000000000000000000000000000000000000
07707700770077700000077077007770777000007070777070707770000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__gff__
000101010181010001000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000c0000040400000000000000000000000000000000000000000000000000000000000c0c00000000000000000001000000000000000001000000
0000000000000000000001010101010100000000000000000000000001010101000000000000000000000000010101010000000000000000000000000502000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
8f00000000000000000000000000008f9c00bd0000000000000000000000009c9c00bd00000000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000000000000000000000009c9c00bd00000000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9e8d8d8d8d8d8d8d8d8d8d8d8d8d8d9e9e8d8d8d8d8d8d8d8d8d8d8d8dbc8d9eac8d8d8ebc9d8d8d8d8dadbc8c8d8dae0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000000000000000000000009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000000000000000000000009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000000000000000000000009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd009d8d8d9e8d8d8d8e0000009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000bd009c00bd00ac8d8d8d9e0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000bd009c00bd000000bd009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000bd009c00bd000000bd009c0000009c00000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00bd0000bd009c00bd000000bd009c008c8dae0000000000000000ac8d8e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c00000000bd009c00bd008f00bd009c8cae000000000000000000000000ac8e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9e8dbd8dbd8dbd8d8dbd8dbd8dbd8d9e9c00000000bd00af00bd009c00bd009c9c00000000000000000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd00bd00bd0000bd00bd00bd009c9c0000000000000000bd009c00bd009c9c0000009d8d8d8d8d8d8dad0000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd00bd00bd0000bd00bd00bd009c9c0000000000000000bd009c00bd009c9c000000bd000000000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd00bd00bd0000bd00bd00bd009c9e8d8d8d8d8d8d8d8d8d8dae00bd009c9ead0000bd0000000000000000009d9e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c000000000000000000000000bd009c9e8d8d8dbd8d8d8d8d8d8d8d8d8d8d9e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c000000000000000000000000bd009c9c000000bd0000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9e8dbc8dbc8dbc8d8dbc8dbc8dbc8d9e9e8d8dbc8d8d8d8d8d8d8d8d8d8d8d9e9c000000bd0000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000000000000000bd00009c9c000000bd0000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000000000000000bd00009c9c000000bd0000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000000000000000bd00009c9c000000bd0000009d8d8dad00bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd00009d8d8dad0000bd00009c9e8dad00000000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000bd0000bd0000bd00009c9c00bd00000000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000bd0000bd0000bd00009c9c00bd00000000000000000000bd009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000bd0000bd0000bd00009c9c00bd009d8d8d8dad0000009d8d8d9e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00000000000000000000000000009c9c0000bd0000bd0000bd0000bd00009c9c00bd0000000000000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c000000008c8d8d8d8d8e000000009c9c0000bd0000bd0000bd0000bd00009c9c00bd0000000000000000000000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9e8d8d8d8dae00000000ac8d8dbd8d9e9e8d8d8d8dadbd9dadbd9d8d8d8d8d9e9c8d8dad009d8dad009d8dad0000009c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd00000000000000000000bd009c9c00bd000000bd0000bd000000bd009cac8e0000000000000000000000008cae0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd00000000000000000000bd009c9c00bd000000bd0000bd000000bd009c00ac8e000000000000000000008cae000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9c00bd009d8d8d8d8d8d8d8d8d8d8d9e9c00bd009d8d8d8d8d8d8dad00bd009c0000ac8d8d8d8d8d8d8d8d8d8dae00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
010e00002042524325293252c4251d3252032524425293252c3251d4252032524325294252c3251d3252042524325293252c4251d3252032524412293252c3251d4252032524325294252c3251d3252042524325
010e00000c0430544505435054450543505445054350544501435014450143501445014350144501435014450c0430344503435034450343503445034350344500435004450043500445004350c0430043500445
010e0000184251d3252032524425356152c325184251d32520325184251d3252c325356151d32520325184251d32520325184251d32535615244251d32520325184251d3252c3252442535615203251842529325
010e00000c043014350144501435014450143520415014350c04320415014350143501435014451d415204150c043014350144501435014450143501445014350c04300445004350044500435004350043500445
091000011804000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
111000011805000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e0000182151d3251d3251d325356151d325304201d3252e4202e4201d3251d325356151d325292202c2202c2201d3251d3251d325356151d3252e4201d325294201b3251b32527420356151b3251b3251b325
010e00000c043014450143501425034450343503425034150c04305445054350542508445084350842508415356150a4450a4350a425356150c4350c4250c4150c04300445004450044500445004450043500435
010e000029420294112941229415356152b4202b4112b4122d4202d4112d4122d4123561530420304123041232411324103241032412354113541235412294163541635416294162941635416354162941629416
010e00000c0430010500100000050c0430010000005003040c0430010000005001000c0431202403031000310c0430010500100000050c0430010000005003040c0430010000005001000c043000140c01118021
010e00000c0450015500140000350015500140000350032400155001400003500140000351861430600003240c045001550014000035001550014000035003240015500140000350014000035186143060000324
010e00000c0433f2153f215243032461018615243033f2150c043243033f2153f215246101203403041000410c043001053f2153f21524610186153f215003040c0433f215000053f21524610000140c02118031
010e00000c0450015500140000350c043001400003500324001550014000035001400c043186153f215003240c0450015500140000350c043001400003500324001550014000035001400c043186153f21500324
010e00000c0433f2153f215000052461018615000053f2150c043001003f2153f215246101200403000000000c043001053f2153f21524610186153f215003040c0433f215000053f21524610000040c00018000
010e00000c0450015500150000050c043001500000500304001550015000005001500c043186153f215003040c0450015500150000050c043001500000500304001550015000005001500c043186153f21500304
012000001474014731147211471516740167311672116715197401973119721197151b7401b7311b7211b7111b7101b7121b7121b7121b7151970019700197001970019700197001b7001b7001b7001b7001b700
012000001272012720127251270510720107201072510705117201172011725117051572015720157201572015722157221572215725057000570005700007000070006705087050970009700097000970009700
012000000102001020010200102506020060200602006025080200802008020080250402004020040200402004020040200402204022040250400500000000000000000000000000000000000000000000000000
01501000027400271112d2012d15077400771117d2017d15027400271112d2012d15077400771117d2017d1500000000000000000000000000000000000000000000000000000000000000000000000000000000
c92800101ec141211519115151151ec141a115191151511523c14171151a115211151ec141f1151e1151511500000000000000000000000000000000000000000000000000000000000000000000000000000000
b128000012310123151ec241531019310193151ec241a310173101731523c141a310213102131523c141f3101e3101e3152ac141a31015310153151931019315173101731523c141a31015310153151331013315
b128000012310123151ec241531019310193151ec241a31017310173151a3101a315213102131523c142631025310253152ac14213101e3101e3152131021315233102331523c141f3101a3101a3152331023315
01501000047400471113d1013d15067400671121c1021c15047400471113d2013d15027400271112d2012d1500000000000000000000000000000000000000000000000000000000000000000000000000000000
c928000013d14131151a1151711513d1413115211151a11523c041f1151e115151151ec041a11519115151151ec04131151a115171151ec0413115211151a1151ec141211519115151151ec141a1151911515115
b12800002131021310213102131510d2010d1110d15000001a3101a3101a3101a3151ac201ac111ac1523310213102131021310213151cd201cd111cd15000001a3101a3101a3101a3151ac201ac111ac152f310
01501000047400471113d2013d15067400671121c1021c15077400771113d2013d15097400971113d2013d1500000000000000000000000000000000000000000000000000000000000000000000000000000000
c928000013d14131151a1151711513d1413115211151a11523c041f1151e115151151ec042111525115261151ec042111525115261151ec042811525115261151ec042811525115211151ec04211151f11515115
b12800002d3102d3102d3102d3151cd101cd111cd111cd152631026310263102631526c2026c1126c152531023310233102331523c1426310263102631523c143231032310323151fd142831028310283151fd14
b1ff0000263151ec1423c141ed1423c141ec041cd001cd052630026300263002630526c0026c0126c052530023300233002330523c0426300263002630523c043230032300323051fd042830028300283051fd04
01180000021100211002110021120e1140e1100e1100e1120d1140d1100d1100d1120d1120940509110091120c1100c1100c1100c1120b1110b1100b1100b1120a1100a1100a1100a11209111091100911009112
01180000117201172011722117221d7201d7201d7221d7221c7211c7201c7201c7201c7221c72218720187221b7211b7201b7201b7201b7221b7221d7221d7221a7201a7201a7201a7201a7221a7221672016722
011800001972019720197221972218720187201872018720147201472015720157201f7211f7201d7201d7201c7201c7201c7221c7221a7201a7201a7221a7251a7201a7201a7221a72219721197201972219722
000100000f12500000000000710500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000700000c6241c6252b6002f60024600286002b6002f6003060034600376001360415604176040c6040e60410604116041360400000000000000000000000000000000000000000000000000000000000000000
000f00002d27321363164530c3430733303323013130d50309503075031550300003000030000300003000031d303123031b0030000300003000030000300003153030b3031a7031f5031b003217031d50322003
00020000296632866528604276532765426605256432564524604236432264421603206351e6351c6031b6341762314604106230c625086030661503613026040c0040740400604083040c004172041160400404
0002000000373016732b3730167300473233731c26301663053631a26301663016530d253024531e3530164300343054431c2430163325333016330033325423016230162309323016231d313016131021300413
000300000c343236450933520621063311b6210432116611023210f611013110a6110361104600036000260001600016000460003600026000160001600016000160004600036000260001600016000160001600
00010000312502b250252502025019250122500e2500e6300e6300e6351520010200072000420000200002000d20009200082000820000200002000120026100121001e100061000d10019100251000c10024100
000600001c36311000103331031310303107031070513005306041070310705000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200001c620385503455031550305502e5502d5501d6201d6201d6001d600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00020000207101c71019720316202d61028620246202261022610236001b710187101f60017710166100d61008610056200261000700000000000000000000000000000000000000000000000000000000000000
00050000307342b751237511d75117751127510d75108751037310271501713007050c7000a700077000670004700027000170000700007000070000700007000070000700017000070000700007000070000700
000900000b6500b6500b6531c6001c6501c650156300e630096300763005610036100161001615000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800001a7201a7201a7221a7221c7201c7201c7221c7221e7201e7202172021720247212472023720237202272022720227202272022722227221f7201f7202272122720227202272221721217202172221722
0118000002114021100211002112091140911009110091120e1140e1100c1100c1120911209110081100811207110071100711007112061110611006110061120111101110011100111202111021100211002112
0018000020720207202072220722217202172021722217222b7212b72029720297202872128720267202672526720267202672026720267222672228721287202672026720267202672225721257202572225722
0003000017630106300e6500e6301063213652186521e6522a6523663236632306323062221622126220661200612006120161200612006150060000600006000060000600006000060000600006000060000600
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 00014344
00 00014344
00 02014344
00 02014344
00 03064344
00 03064344
00 03064344
00 01064344
02 07084344
01 090a4344
00 090a4344
00 0b0c4344
00 0b0c4344
00 0d0e4344
02 0d0e4344
04 0f101144
00 12134344
01 12131444
00 12131544
00 16171844
00 191a1b44
02 12131c44
01 1d1e4344
00 1d1f4344
00 1d1e4344
00 1d1f4344
00 2c2d4344
02 1d2e4344

