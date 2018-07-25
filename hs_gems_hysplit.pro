;===================================================================================================
; PROGRAM NAME:
;  hs_gems_hysplit
;
; PURPOSE:
;  mapping column ozone with wind vector & hysplit trajectory
;
; PROCESS:
;
; REFERENCE:
;
; INPUT:
;
; OUTPUT:
;
; DEVELOPER:
;  Hyeonsic Nam (hs)
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  original sorce from jbak 
;  Updated by geun
;  Updated by hs
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================


PRO hs_gems_hysplit,day,layer=layer,xdrmod=xdrmod
;+---------------------------------------------------------------------------+
; RUN : hs_gems_hysplit, 200180126, layer=0, xdrmod=1
; in first run, xdrmod must be 0
;
; INPUT : 8 length integer (yyyymmdd), 1 integer (0,1,2) layer type (total, trop, strat)
; OUTPUT: None, plot image and save as png
;
; default : total column
;+---------------------------------------------------------------------------+
if not keyword_set(layer) then layer=0
if not keyword_set(xdrmod) then xdrmod=0


day=strtrim(day,2)
unit_of_o3 = '(DU)' ; '(ppb)'
respath='/home/o3p_hs/results/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2008/'
orbits=[] & pixcor_files=[]
gems_files    = file_search(outpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*')

for i=0,n_elements(gems_files)-1 do begin
  orbit=strmid((strsplit(gems_files[i],'_',/ext))[-4],1,5)
  pixcor_file  = file_search(pixpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*'+'o'+orbit+'*')
  pixcor_files = [pixcor_files,pixcor_file]
endfor 

if not n_elements(pixcor_files) eq n_elements(gems_files) then $
message,'   Numbers of corner files & data files do not match!'

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

prefix=respath+'polyfill_l2_'+name2[layer]+strtrim(pressure[layer],2)+'_'+day  ;  _o'+STRING(orb,F='(I05)')

ps=prefix+'.ps'
png=prefix+'.png'
cgps_open, ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
;window,0,xs=1000,ys=1000
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

for i=0,fnum-1 do begin
  hs_read_gems,gems_files[i],gems,xdrmod=xdrmod
  hs_omi_pixcor,pixcor_files[i],pixcor,xdrmod=xdrmod
  data=gems.co3[layer] 
  npix=n_elements(gems.lon)
  for j=0,npix-1 do begin
    corind=where(abs(pixcor.lat-gems[j].lat) eq min(abs(pixcor.lat-gems[j].lat))) ; find index of pixcor file with gems latitude
    corind=array_indices(pixcor.lat,corind)
    clon=reform(pixcor.clon[corind[0],corind[1],*])
    clat=reform(pixcor.clat[corind[0],corind[1],*])
    ;stop
    for k=0,nlev-2 do begin
      if (data[j] ge lev[k,layer] and data[j] lt lev[k+1,layer]) then col=barcol[k]
    endfor
    ;if (data[j] eq -999.) then continue ;bad pixel skip
    TVLCT, CGColor('GRAY',/Triple), 251 ; bad pixel
    if (data[j] eq -999.) then col=251 ;bad pixel skip
    ;if (data[j] eq -999. or gems[j].exval lt 6) then col=251 ;bad pixel skip
    ;plots,gems[j].lon,gems[j].lat,psym=plotsym_fn(/cir,scale=0.7,/fill),color=col,noclip=0
    polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0
    ;stop
  endfor
endfor

prepare_fnl_geun,day,fnlout
fpres=fnlout.pres

selpo=WHERE(ABS(fpres-pressure[layer]) EQ MIN(ABS(fpres-pressure[layer])))

lon0=fnlout.lon
lat0=fnlout.lat
fuwnd=fnlout.uwind[*,*,selpo]
fvwnd=fnlout.vwind[*,*,selpo]

title=strtrim(day,2)+' '+name1[layer]+' column O3, '+STRING(fpres[selpo],F='(I4)')+'hPa Wind'

nlon=N_ELEMENTS(lon0)  &  nlat=N_ELEMENTS(lat0)
flon=FLTARR(nlon,nlat)
flat=FLTARR(nlon,nlat)
FOR i = 0, nlon-1 DO flon[i,*]=lon0[i]
FOR j = 0, nlat-1 DO flat[*,j]=lat0[j]

partvelvec, fuwnd,fvwnd,flon,flat,color=255,fraction=0.7,/over,noclip=0,length=0.01,thick=3

colorbar_2, lev[*,layer], barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
            format='(I3)',unit='DU',lowleft=[0.87,0.05],/nofirst,/right

map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
map_grid,latdel=10,londel=10,thick=0.7,/box_axes
xyouts,mean([pos[0],pos[2]]),pos[3]+0.1,align=0.5,title,charsize=1.5,/normal
;stop
cgps_close,density=800,/png
hs_filetrans,png
end
