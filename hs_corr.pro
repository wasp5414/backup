pro hs_corr,day,xdrmod=1
;+---------------------------------------------------------------------------+
; RUN : hs_corr, 20060608, xdrmod=1
; in first run, xdrmod must be 0
;
; INPUT : 8 length integer (yyyymmdd), 1 integer (0,1,2) layer type (total, trop, strat)
; OUTPUT: None, plot image and save as png
;
; default : total column
;+---------------------------------------------------------------------------+
;if not keyword_set() then =0
if not keyword_set(xdrmod) then xdrmod=0

day=strtrim(day,2)
unit_of_o3 = '(DU)' ; '(ppb)'
respath='/home/o3p_hs/results/'
xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
gems_files = file_search(outpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*')
xdr_files  = file_search(xdrpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*')

name1=['Total','Stratosphere','Troposphere']
name2=['total','strat','trop']
pressure=[200,100,500]
limit   =[20,100,45,140]

; cross section profile
sel_lat=0
sel_lon=120

fnum=n_elements(gems_files)


;+---------------------------------------------------------------------------+
; PLOT column ozone
;+---------------------------------------------------------------------------+

prefix=respath+'map_l2_'+name2[layer]+strtrim(pressure[layer],2)+'_'+day  ;  _o'+STRING(orb,F='(I05)')

ps=prefix+'.ps'
png=prefix+'.png'
cgps_open, ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
pos=[0.05,0.05,0.82,0.83]
!p.background=0
!p.position=pos
!p.charsize=1.3
loadct_wb,72
MAP_SET,sel_lat,sel_lon,limit=limit,/conic,latdel=10,londel=10,position=pos,standard_parallels=[20,60];,title=title

nlev=51
barcol   =reverse(fix(findgen(nlev-1)*253/nlev))
totallev =findgen(nlev)*1.5+ 270
strlev   =findgen(nlev)    + 240
troplev  =findgen(nlev)    + 30
lev      =[[totallev],[strlev],[troplev]]



end
