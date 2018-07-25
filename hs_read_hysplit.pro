;===================================================================================================
; PROGRAM NAME:
;  hs_read_hysplit
;
; PURPOSE:
;  Read hysplit results from ascii file to structure
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

pro hs_read_hysplit,fn,hys
;+---------------------------------------------------------------------------+
; INPUT  : file name, strings contain file name and directory path 
; OUTPUT : hys, structure 
;+---------------------------------------------------------------------------+

dum=''
direction=''
vertical=''

print,fn
nlines=file_lines(fn)
openr,lun,fn,/get_lun

; Model info read
readf,lun,n1  ; # of model grids 
readcol,fn,model,year,month,day,hour,f_hour,format='A,I,I,I,I,I',skipline=1,numline=n1
for line=1,n1 do readf,lun,dum

; Start point info read
readf,lun,n2,direction,vertical,format='(I6,1X,A8,1X,A8)' ; # of trajectories
readcol,fn,syear,smonth,sday,shour,slat,slon,slevel,format='',skipline=n1+2,numline=n2
for line=0,n2-2 do readf,lun,dum


; Trajectory info read
readf,lun,n3,direction,vertical,format='(I6,1X,A8)' ; # of diagonostic output (1 for now, only pressure)
readcol,fn,traj,grid,pyear,pmon,pday,phour,pmin,f_phour,age,plat,plon,height,press,$
        format='I,I,I,I,I,I,I,I,F,F,F,F,F',skipline=n1+n2+3
close,lun

; Get Trajectory info only!
n2=fix(n2)

traj=reform(traj,[n2,25])   & pyear=reform(pyear,[n2,25])   & pday=reform(pday,[n2,25])   & pmon=reform(pmon,[n2,25]) 
phour=reform(phour,[n2,25]) & pmin=reform(pmin,[n2,25])     & age=reform(age,[n2,25])     & plat=reform(plat,[n2,25])
plon=reform(plon,[n2,25])   & height=reform(height,[n2,25]) & press=reform(press,[n2,25])

hys={ntraj:n2,traj:traj,year:pyear,mon:pmon,day:pday,hour:phour,min:pmin,age:age,lat:plat,lon:plon,height:height,press:press}

end


