;=============================================================================
; PROGRAM NAME:
;  hs_collocation
;
; PURPOSE:
;  mapping collocated OMI TO3 and MODIS CTP
;
; PROCESS:
;
; REFERENCE:
;
; REQUIRED:
;  read_modis_myd06.pro 
;  read_l2_v8.pro
;  convert_bit2flag.pro
;  h5read.pro
;
; REQUIRED 
;
;
; DEVELOPER:
;  Hyeonsic Nam (hs)
;  Daegeun Shin (geun)
;  Kanghyun Back
;
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;=============================================================================

PRO hs_collocation,xdrmod=xdrmod,tai93mod=tai93mod,$
printit=printit


start = systime(2) ; time check
tai93mod=1

  ; user control
if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(loop) then loop=0
if not keyword_set(tai93mod) then tai93mod=0
if not keyword_set(printit) then printit=0


dates = '2005'+string( findgen(7) + 601 ,f='(I4.4)')
;xdrmod=  ; 0: xdr generation, 1: use xdr
tai93mod=1
respath='/home/o3p_hs/results/'
to3path='/home/Data/OMI/2_OML2TO/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/20180525/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2005/'
xdrpath='/home/o3p_hs/data/xdr/'

myd06path='/home/Data2/5_MODIS/MYD06/'
myd03path='/home/Data2/5_MODIS/MYD03/'

limit=[-35,-175,20,-115]


type='ORIGIN'
gems_files = file_search(outpath+'GEM_'+type+'*')
fnum=n_elements(gems_files)
fdates=[] & forbits=[]

for i=0,fnum-1 do begin
  fdates=[dates,strmid((strsplit(gems_files,'_',/ext))[i,6],0,8)]
  forbits=[forbits,(strsplit(gems_files,'_',/ext))[i,8]]
endfor

dates=fdates[uniq(fdates,sort(fdates))]
orbits=forbits[uniq(forbits,sort(forbits))]

for id=0,n_elements(dates)-1 do begin
  date=dates[id]

;+---------------------------------------------------------------------------+
; MODIS FILE READ 
;+---------------------------------------------------------------------------+

  year=strmid(date,0,4) & mon=strmid(date,4,2) & day=strmid(date,6,2)
  caldat,julday(fix(mon),fix(day),fix(year))+1,mon2,day2,year2

; Need MODIS data of the day after for the pixels around 150~180W.
   mon2=string(mon2 ,f='(I02)') & day2=string(day2,f='(I02)')
  year2=string(year2,f='(I04)')

  julmod =STRING(JULDAY(mon ,day ,year )-JULDAY(12,31,year-1),F='(I03)')
  julmod2=STRING(JULDAY(mon2,day2,year2)-JULDAY(12,31,year-1),F='(I03)')

  modpath='/home/Data2/5_MODIS/MYD06/'
  modfiles =FILE_SEARCH(modpath+'MYD06_L2.A'+year +julmod +'*.hdf', $
  count=nmodfile)
  modfiles2=FILE_SEARCH(modpath+'MYD06_L2.A'+year2+julmod2+'*.hdf', $
  count=nmodfile2)

  nmodfile=nmodfile+nmodfile2 & modfiles=[modfiles2,modfiles]

  IF nmodfile EQ 0 THEN BEGIN
    PRINT, '  No Modfile'
    STOP
  ENDIF  ; nmodfile
    
  modlons=[]    &  modlats=[]   & modtimes  =[] 
  modcfs =[]    &  modctps=[]   & modcphases=[]
    
  FOR ifile=0,nmodfile-1 DO BEGIN
    modfile=modfiles[ifile]
    myd06=hs_read_myd06(modfile,xdrmod=xdrmod)
    
    mlon  = myd06.lon   & mlat = myd06.lat
    tmp=WHERE(mlon GE limit[1] AND mlon LE limit[3] AND $
              mlat GE limit[0] AND mlat LE limit[2], ntmp)
    
    
    IF ntmp NE 0 THEN BEGIN
      print, '  READ modfile : ', modfile, ntmp

      modlon=myd06.lon[tmp]
      modlat=myd06.lat[tmp]
      modtime=myd06.jtime[tmp]
      modctp=myd06.ctp[tmp]
      modcf=myd06.cf[tmp]
    
      modlons       = [modlons, modlon]
      modlats       = [modlats, modlat]
      modtimes      = [modtimes, modtime]
      modctps       = [modctps, modctp]
      modcfs        = [modcfs, modcf]
    ENDIF  ; ntmp
    
  ENDFOR  ; ifile
    
  modvars={lon:modlons, lat:modlats, time:modtimes, ctp:modctps, $
           cf:modcfs} 
 
;+---------------------------------------------------------------------------+
; GEMS & PIXCORFILE READ
;+---------------------------------------------------------------------------+

  for ifile=0,n_elements(orbits)-1 do begin

    orbit=forbits[ifile]
    pixcor_file=file_search(pixpath+'*'+'o'+orbit+'*')
    hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod
    hs_read_gems,gems_files[ifile],gems,xdrmod=xdrmod,tai93mod=tai93mod  
    nline=(size(gems,/dim))[1] & npix=30.    
;+---------------------------------------------------------------------------+
; COLLOCATION 
;+---------------------------------------------------------------------------+

; collocation
    npo=N_ELEMENTS(gems[where(gems.lon ge -180)].lon)
    time_bo=120*60  ; (unit: second)
  
; distance limit for nearest modis center from omi center (unit : km)
    dist_bo=20   
    orbctp=FLTARR(npix*nline)
    orbcf=FLTARR(npix*nline)
    orbtime = gems.time
    
; 5km modis value
    mlon  = modvars.lon    &  mlat = modvars.lat
    mtime = modvars.time

  PRINT,"POINT SEARCH LOOP START"
  PRINT,"NUMBER OF LOOP TIME : ",npo

  FOR ipo=0,npo-1 DO BEGIN

    if ipo/1000. eq ipo/1000 then print, 'POSITION DONE: ', ipo

    minval=min(abs(pixcor.lat-gems[ipo].lat) + $
               abs(pixcor.lon-gems[ipo].lon), corind)

    d2pos=array_indices(pixcor.lon,corind)

    lon_min=MIN(pixcor.clon[d2pos[0],d2pos[1],*])
    lon_max=MAX(pixcor.clon[d2pos[0],d2pos[1],*])
    lat_min=MIN(pixcor.clat[d2pos[0],d2pos[1],*])
    lat_max=MAX(pixcor.clat[d2pos[0],d2pos[1],*])

; 5km spatial resolution
    tmp=WHERE(mlon GE lon_min AND mlon LE lon_max AND $
              mlat GE lat_min AND mlat LE lat_max AND $
              ABS(mtime-orbtime[ipo]) LE time_bo, ntmp) 

    IF ntmp NE 0 THEN BEGIN

      ; ctp collocation
      ctps=modvars.ctp[tmp]
      val=WHERE(ctps NE -999, nval)
      avgctp=(nval NE 0) ? MEAN(ctps[val]) : -999
      orbctp[ipo]=avgctp     

      ; cf collocation
      cfs=modvars.cf[tmp]
      val=WHERE(cfs NE -999, nval)
      avgcf=(nval NE 0) ? MEAN(cfs[val]) : -999
      orbcf[ipo]=avgcf     

    if printit then begin
      print,'NOT ZERO'
      print,'POSITION : ',ipo
      print,'Value : '   ,avgctp
      print,'Value2: '   ,avgcf
    endif

    ENDIF ELSE BEGIN ; ntmp
      orbctp[ipo] =-999
      orbcf[ipo]  =-999
    ENDELSE ; ntmp
      
  ENDFOR  ; ipix
  PRINT,"POINT LOOP END"
  
    res={gems:gems, ctp:reform(orbctp,size(gems.lon,/dim)), $
         cf:reform(orbcf,size(gems.lon,/dim)) }
    file = 'final_col_modis_omi_'+orbit+'.xdr'
    SAVE,file=xdrpath+file,/xdr,res
    PRINT, "saving : ",file
stop

; Make ascii type output
outfn='MYD2OMI_o'+string(orbs[id],F='(I05)')+'.dat'

openw,lun,outfn,/get_lun
dim=size(gems.ctp,/dim) & nx=dim[0] & ny=dim[1]
aout=fltarr(nx,ny) - 999.
pix=gems.pix & line=gems.line

for ipo=0,npo-1 do begin
  aout[pix[ipo]-1,d
endfor

form='('+string(nx,f='(I2)')+'I04)'
for i=0,ny-1 do begin
  ;printf,lun,
endfor
free_lun,lun
print,'    Write output in :: ' +outfn
          
  ENDFOR ; for gems orbit files
ENDFOR ; for dates
stop
end
