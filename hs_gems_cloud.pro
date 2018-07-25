;==============================================================================
; PROGRAM NAME:
;  hs_gems_cloud
;
; PURPOSE:
;  mapping cloud fraction or cloud top pressure.
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
;  forked from hs_wind_over_vector.pro 
;
;  Copyright (C) 2018 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;==============================================================================



PRO hs_gems_cloud,day,type=type,xdrmod=xdrmod
;+---------------------------------------------------------------------------+
; RUN : hs_gems_cloud, 200180126,type=0,xdrmod=1
; in first run, xdrmod must be 0
;
; INPUT : 8 length integer (yyyymmdd), 1 integer (0,1) type (cloud fraction,cloud pressure)
; OUTPUT: None, plot image and save as png
;
;+---------------------------------------------------------------------------+
if not keyword_set(type) then type=0
if not keyword_set(xdrmod) then xdrmod=0

day=strtrim(day,2)
respath='/home/o3p_hs/results/'
xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2008/'
orbits=[] & pixcor_files=[]
gems_files = file_search(outpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*')

for i=0,n_elements(gems_files)-1 do begin
  orbit=strmid((strsplit(gems_files[i],'_',/ext))[-4],1,5)
  pixcor_file  = file_search(pixpath+'*'+strmid(day,0,4)+'m'+strmid(day,4,4)+'*'+'o'+orbit+'*')
  pixcor_files = [pixcor_files,pixcor_file]
endfor

if not n_elements(pixcor_files) eq n_elements(gems_files) then $
message,'   Numbers of corner files & data files do not match!'

name1=['Cloud fraction','Cloud top pressure']
name2=['cfrac','ctp']
limit=[20,100,45,140] 

; cross section profile
sel_lat=0
sel_lon=120

fnum=n_elements(gems_files)

;+---------------------------------------------------------------------------+
; PLOT cloud  
;+---------------------------------------------------------------------------+

prefix=respath+'polyfill_l2_'+name2[type]+'_'+day
ps=prefix+'.ps'
png=prefix+'.png'
cgps_open, ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
;window,0,xs=1000,ys=1000
pos=[0.05,0.05,0.82,0.83]
!p.background=0
!p.position=pos
!p.charsize=1.3

;loadct_wb,72
loadct_wb,49
MAP_SET,sel_lat,sel_lon,limit=limit,/conic,latdel=10,londel=10,position=pos,standard_parallels=[20,60];,title=title

nlev=51
barcol   =reverse(fix(findgen(nlev-1)*253/nlev))
cfraclev =reverse(findgen(nlev)/50    + 0)
ctplev   =findgen(nlev)*10    + 300
lev      =[[cfraclev],[ctplev]]

for i=0,fnum-1 do begin
  hs_read_gems,gems_files[i],gems,xdrmod=xdrmod
  hs_omi_pixcor,pixcor_files[i],pixcor,xdrmod=xdrmod
  case type of
    0 : data=gems.cfrac  
    1 : data=gems.ctp
    else : print,'Data type error'
  endcase
  npix=n_elements(gems.lon)
  for j=0,npix-1 do begin
    ;corind=where(abs(pixcor.lat-gems[j].lat) eq min(abs(pixcor.lat-gems[j].lat))) ; find index of pixcor file with gems latitude
    minval=min(abs(pixcor.lat-gems[j].lat),corind)
    corind=array_indices(pixcor.lat,corind)
    clon=reform(pixcor.clon[corind[0],corind[1],*])
    clat=reform(pixcor.clat[corind[0],corind[1],*])
    for k=0,nlev-2 do begin
      case type of
      0 : if (data[j] lt lev[k,type] and data[j] ge lev[k+1,type]) then col=barcol[k]
      1 : if (data[j] ge lev[k,type] and data[j] lt lev[k+1,type]) then col=barcol[k]
      endcase
    endfor 
  TVLCT, cgcolor('gray',/triple),251 ;bad pixel color set
  if (data[j] eq -999.) then col=251 ;bad pixel skip
  polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0
  ;plots,gems[j].lon,gems[j].lat,psym=plotsym_fn(/cir,scale=0.7,/fill),color=col,noclip=0
  endfor
endfor

title=strtrim(day)+' '+name1[type]  ;' column O3, '+STRING(fpres[selpo],F='(I4)')+'hPa Wind'

case type of
0: begin
colorbar_2, lev[*,type]*100, barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
            format='(I3)',unit='%',lowleft=[0.87,0.05],/nofirst,/right
end
1: begin
colorbar_2, lev[*,type], barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
            format='(I3)',unit='hPa',lowleft=[0.87,0.05],/nofirst,/right
end
else : print,'Type error'
endcase

map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
map_grid,latdel=10,londel=10,thick=0.7,/box_axes
xyouts,mean([pos[0],pos[2]]),pos[3]+0.1,align=0.5,title,charsize=1.5,/normal
cgps_close,density=800,/png

hs_filetrans,png
end
