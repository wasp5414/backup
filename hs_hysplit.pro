;===================================================================================================
; PROGRAM NAME:
;  hs_hysplit
;
; PURPOSE:
;  Read hysplit results from ascii file to structure, and plot trajectory.
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
;===================================================================================================



PRO hs_hysplit, day, direction=direction
;+---------------------------------------------------------------------------+
; RUN : hs_gems_cloud, 200180126
;
; INPUT : 8 length integer (yyyymmdd)
; OUTPUT: None, plot image and save as png
;
;+---------------------------------------------------------------------------+
if not keyword_set(direction) then direction='forward'

day=strtrim(day,2)
path='/home/o3p_hs/results/HYSPLIT/'
respath='/home/o3p_hs/results/'
all_files=file_search(path+'*')
files=file_search(path+day+'_'+direction+'*',count=nfiles)

if (direction eq 'forward' and nfiles eq 0) then begin
  print, 'There is no file to read, changing direction option to backward ... '
  direction='backward' 
  files=file_search(path+day+'_'+direction+'*',count=nfiles)
endif


if (nfiles eq 0) then message,'There is no file to read, check direction option and directory list! - all_files'

case direction of
  'forward' : begin
    barcol  = fix(findgen(25-1)*240/(25-2))          ; forward time color
    nameidx=0
   end
  
  'backward' : begin
    barcol  = reverse(fix(findgen(25-1)*240/(25-2)))  ; backward time color
    nameidx=1
  end
  else : message,'::  Direction input error!  ::'
endcase

name1=['HYSPLIT Forward trajectory','HYSPLIT Backward trajectory']
name2=['ftraj','btraj']
limit=[20,100,45,140] 

; cross section profile
sel_lat=0
sel_lon=120


;+---------------------------------------------------------------------------+
; PLOT hysplit
;+---------------------------------------------------------------------------+

prefix=respath+'map_'+name2[nameidx]+'_'+day

ps=prefix+'.ps'
png=prefix+'.png'
cgps_open, ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
;window,0,xs=1000,ys=1000
pos=[0.05,0.05,0.82,0.83]
!p.background=0
!p.position=pos
!p.charsize=1.3

loadct_wb,54
MAP_SET,sel_lat,sel_lon,limit=limit,/conic,latdel=10,londel=10,position=pos,standard_parallels=[20,60]

for i=0,nfiles-1 do begin
  hs_read_hysplit,files[i],hys
  ;stop
  slon=hys.lon[*,0]
  slat=hys.lat[*,0]
  data=hys.press
  ntime=n_elements(hys.press[0,*])
  ntraj=hys.ntraj
  loadct_wb,54
  lev     = fix(indgen(ntime-1)*2*data[0,0]/(ntime-2))  ; pressure level
  pbarcol = fix(indgen(ntraj)*253/(ntraj-1))


  for t=0,ntraj-1 do begin
    for j=0,ntime-2 do begin
      oplot,hys.lon[t,j:j+1],hys.lat[t,j:j+1],color=barcol[j]
      ;print,'traj done'
    endfor  
  endfor
  
  loadct_wb,43
  for t=0,ntraj-1 do begin ; start point plot
    plots,slon[t],slat[t],psym=plotsym_fn(/cir,scale=0.5,/fill),col=pbarcol[t],noclip=0
    ;plots,slon[t],slat[t],psym=plotsym_fn(/cir,scale=2,/fill),col=pbarcol,noclip=0
    ;print,'point done'
  ;stop
  endfor
  
endfor

title=strtrim(day)+' '+name1[nameidx]  

;colorbar_2, lev, barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
            ;format='(I3)',unit='hour',lowleft=[0.87,0.05],/nofirst,/right

map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
map_grid,latdel=10,londel=10,thick=0.7,/box_axes
xyouts,mean([pos[0],pos[2]]),pos[3]+0.1,align=0.5,title,charsize=1.5,/normal
cgps_close,density=800,/png
;stop
hs_filetrans,png
;stop
end
