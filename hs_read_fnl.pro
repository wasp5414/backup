;===================================================================================================
;
; PROGRAM NAME:
;  hs_read_fnl.pro 
;
; PURPOSE:
;  read fnl daily and monthly data 
;
; PROCESS:
;
; REFERENCE:
;  analy_fnl_v2.pro  (by geun)
;
; INPUT:
;
; OUTPUT:
;
; DEVELOPER:
;  Hyeonsic Nam
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  forked by hs
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO hs_read_fnl,fnl,fn=fn,ymd=ymd,xdrmod=xdrmod

testfn = '/home/o3p_hs/GEMS/o3p/dat/ATMOS/fnl13.75LST/fnltp/fnltp_20051127.dat'
testflag=0
if not keyword_set(xdrmod) then xdrmod=0
if (not keyword_set(fn)) and (not keyword_set(ymd)) then begin
  print,'****** TEST RUN FOR READFNL *******'
  fn = testfn
  testflag=1
endif

;fnl parameter
nlon  = 360 & nlat  = 180 & nl    = 26

; ****SETTING FORMAT****

fnldir  = '/home/o3p_hs/GEMS/o3p/dat/ATMOS/fnl13.75LST/'
xdrpath = '/home/o3p_hs/data/xdr/'
vars    = ['tp','sp','st','tt','temp'] & nvars   = n_elements(vars)
formats = ['(360I3)','(360I4)','(360I3)','(360I3)','(360I3)']
dim2  = intarr(nlon,nlat) & dim3 = intarr(nlon,nlat,nl) & tmp   = intarr(nlon)
dims    = ['dim2','dim2','dim2','dim2','dim3']
fnl   = {tp:dim2,sp:dim2,st:dim2,tt:dim2,temp:dim3,lon:dim2,lat:dim2}


lon1d = findgen(nlon)-180+0.5
lat1d = findgen(nlat)-90 +0.5
lon2d = fltarr(nlon,nlat)
lat2d = fltarr(nlon,nlat)

for i=0,nlon-1 do begin
  for j=0,nlat-1 do begin
    lon2d[i,j]=lon1d[i]
    lat2d[i,j]=lat1d[j]
  endfor
endfor

if (not testflag) and keyword_set(ymd) then  begin
  for i=0,nvars-1 do begin
    fn = fnldir+'fnl'+vars[i]+'/fnl'+vars[i]+'_'+ymd+'*'
    openr,1,fn
    for k=0,nl-1 do begin
      for j=0,nlat-1 do begin
        readf,1,tmp,format=formats[i]
        void=execute(dims[i]+'[*,j]+=tmp')
      endfor
      if not (vars[i] eq 'temp') then break
    endfor
    void=execute('fnl.'+vars[i]+'='+dims[i])
    if vars[i] eq 'temp' then void=execute('fnl.'+vars[i]+'='+dims[i])
    close,1
    fnl.lon=lon2d & fnl.lat=lat2d
  endfor
endif else begin ; only use for tp
  fnl = intarr(nlon,nlat)
  l = strlen(fn)
  prefix  = strmid(fn,0,l-4)
  xdrfn   = prefix+'.xdr'
  openr,1,fn
  for j=0,nlat-1 do begin
    readf,1,tmp,format='(360I3)'
    fnl[*,j] = tmp
  endfor
  close,1
endelse
end

