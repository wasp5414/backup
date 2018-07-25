;===================================================================================================
; PROGRAM NAME:
;  col_mod2omi_hs
;
; PURPOSE:
;  mapping collocated OMI TO3 and MODIS CTP
;
; DESCRIPTION:
;  added omi orb information from v2
;  optimized for 
;
; REFERENCE:
;
; REQUIRED:
;  read_modis_myd06.pro 
;  read_l2_v8.pro
;  convert_bit2flag.pro
;  h5read.pro
;
;
;
; DEVELOPER:
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
;===================================================================================================

;+---------------------------------------------------------------------------+
; Main Procedure 
;+---------------------------------------------------------------------------+
PRO col_mod2omi_hs

  ; user control
  write_on = 0 
  call_xdr = 0  ; 0,1: run without xdr     2: use xdr
  plot_on  = 1 
  cot_save = 1

  limit=[-20,-180,20,-140]
  file='/home/o3p_hs/GEMS/o3p/v0.4.3/src/runlist_week2.list'
  xdrpath='/home/o3p_hs/data/xdr/'
  ;file='hs_runlist.list'
  OPENR, lun, file, /GET_LUN
  line=''
  dates=[]  &  orbs=[]  
  pixs1=[]  &  pixs2=[]  &  lines1=[]  &  lines2=[]
  WHILE NOT EOF(lun) DO BEGIN
    READF, lun, line
    tmp=STRSPLIT(line,/EXT)
    date=STRMID(tmp[0],0,4)+STRMID(tmp[0],5,4)
    dates=[dates, date]
    orbs=[orbs,tmp[5]]
    pixs1=[pixs1,FIX(tmp[3])]
    pixs2=[pixs2,FIX(tmp[4])]
    lines1=[lines1,FIX(tmp[1])]
    lines2=[lines2,FIX(tmp[2])]
  ENDWHILE
  FREE_LUN,LUN

  omipath='/home/Data/OMI/1_OML1BRUG/'
  FOR id=0,N_ELEMENTS(dates)-1 DO BEGIN 
  ;FOR id=0,0 DO BEGIN 
TIC
    date=dates[id]
    print,id, '  ',  date,'  ', orbs[id], pixs1[id], pixs2[id], lines1[id], lines2[id]
    xdr_ctp=xdrpath+'CTP_o'+STRING(orbs[id],F='(I05)')+'.xdr'
    xdr_cot=xdrpath+'COT_o'+STRING(orbs[id],F='(I05)')+'.xdr'

    IF call_xdr LE 1 THEN BEGIN

      year=STRMID(date,0,4) 
      mon=STRMID(date,4,2)
      day=STRMID(date,6,2)


      omifile=FILE_SEARCH(omipath+year+'/'+'OMI-Aura*'+year+'m'+mon+day+'*-o'+orbs[id]+'*.he4', count=nfiles)
      IF nfiles NE 1 THEN BEGIN
        PRINT, '  No L1Bfile'
        STOP
      ENDIF ; npixfile
      omifile=omifile[0]

      swathname = 'Earth UV-1 Swath'
      stat1 = OMI__LOAD_HDF_DATA(omifile, swathname,'Latitude', omilat)
      stat1 = OMI__LOAD_HDF_DATA(omifile, swathname,'Longitude', omilon)
      stat1 = OMI__LOAD_HDF_DATA(omifile, swathname,'Time', omitime0)

      print, '  READ omifile : ', omifile
      timepos=STRPOS(omifile,'OML1BRUG_')
      time=strmid(omifile, timepos+9,14)
      header='OMI-Aura_L2-OMPIXCOR_'
      pixfile=FILE_SEARCH('/home/Data/OMI/2_OML2PIXCOR/'+year+'/'+header+time+'*.he5', count=npixfile)
      pixfile=pixfile[0]
      IF npixfile NE 1 THEN BEGIN
        PRINT, '  No Pixfile'
        STOP
      ENDIF ; npixfile
      dim=SIZE(omilon,/DIM)
      nx=dim[0]
      ny=dim[1]

      omitime = DBLARR(nx,ny)
      FOR i=0,nx-1 do omitime[i,*] = omitime0
      omipix=INTARR(nx,ny)
      omiline=INTARR(nx,ny)
      FOR ipix=0,nx-1 DO omipix[ipix,*]=ipix+1
      FOR iline=0,ny-1 DO omiline[*,iline]=iline+1
      READ_L2_PIXCOR, pixfile, pixres
      pixlon0=pixres.corlon
      pixlat0=pixres.corlat     
      
      pixlon=FLTARR(nx, ny, 4)
      pixlat=FLTARR(nx, ny, 4)
      FOR ipix=0,nx-1 DO BEGIN     
        pixlon[ipix,*,0]=pixlon0[ipix*2,*,0]
        pixlon[ipix,*,1]=pixlon0[ipix*2+1,*,1]
        pixlon[ipix,*,2]=pixlon0[ipix*2+1,*,2]
        pixlon[ipix,*,3]=pixlon0[ipix*2,*,3]
        pixlat[ipix,*,0]=pixlat0[ipix*2,*,0]
        pixlat[ipix,*,1]=pixlat0[ipix*2+1,*,1]
        pixlat[ipix,*,2]=pixlat0[ipix*2+1,*,2]
        pixlat[ipix,*,3]=pixlat0[ipix*2,*,3]
      ENDFOR ; ipix

      omival=WHERE(omipix GE pixs1[id] AND omipix LE pixs2[id] AND omiline GE lines1[id] AND omiline LE lines2[id], nval)
       
      pixlon_1d=REFORM(pixlon,[nx*ny,4])
      pixlat_1d=REFORM(pixlat,[nx*ny,4])     
      pixlon_1d=pixlon_1d[omival,*]
      pixlat_1d=pixlat_1d[omival,*]
      omilon_1d=omilon[omival]
      omilat_1d=omilat[omival]
      omipix_1d=omipix[omival]
      omiline_1d=omipix[omival]
   
      caldat,(julday(1,1,1993,0,0,0)*86400.0d0+MINMAX(omitime0))/86400.0d0,omimon,omiday,omiyear,omihour,omimin,omisec
      omijulmin=julday(omimon[0],omiday[0],omiyear[0],omihour[0],omimin[0],omisec[0])
      omijulmax=julday(omimon[1],omiday[1],omiyear[1],omihour[1],omimin[1],omisec[1])

      julmod=STRING(JULDAY(mon,day,year)-JULDAY('01','01',year)+1,F='(I03)')
      modpath='/home/Data2/5_MODIS/MYD06/'
      ;modfiles=FILE_SEARCH(modpath+'MYD06_L2.A'+year+julmod+'*.hdf', count=nmodfile)
      modfiles=FILE_SEARCH(modpath+'MYD06_L2.A'+year+'*.hdf', count=nmodfile)

      mod03path='/home/Data2/5_MODIS/MYD03/'
      ;mod03files=FILE_SEARCH(mod03path+'MYD03.A'+year+julmod+'*.hdf', count=nmod03file)
      mod03files=FILE_SEARCH(mod03path+'MYD03.A'+year+'*.hdf', count=nmod03file)

      IF nmodfile EQ 0 or nmod03file eq 0 THEN BEGIN
        PRINT, '  No Modfile'
        STOP
      ENDIF  ; nmodfile

      IF nmodfile ne nmod03file then begin
        modtpos1=STRPOS(modfiles[0],'MYD06_L2.A')
        modtpos2=STRPOS(mod03files[0],'MYD03.A')
        ;times06 =  strmid(modfiles,strlen(modpath)+18,4)
        times06 =  strmid(modfiles,modtpos1+10,12)
        times03 =  strmid(mod03files,modtpos2+7,12)
        ;times03 =  strmid(mod03files,strlen(mod03path)+15,4)
       
        loc06 = cmset_op(times06,'AND',times03,/index) 
        loc03 = cmset_op(times03,'AND',times06,/index) 

        modfiles = modfiles[loc06]
        mod03files = mod03files[loc03]
    
      ENDIF

      modtpos=STRPOS(modfiles[0],'MYD06_L2.A')
      modyear=STRMID(modfiles,modtpos+10,4)    ; modis years
      modhm=STRMID(modfiles,modtpos+18,4)      ; modis hourmins
      moddaymon0=STRMID(modfiles,modtpos+14,3)   ; modis juldays
      ;modjuls=JULDAY(1,1,modyear,FIX(STRMID(modhm,0,2)),FIX(STRMID(modhm,2,2)))+FIX(julmod)-1
      modjuls=JULDAY(1,1,modyear,FIX(STRMID(modhm,0,2)),FIX(STRMID(modhm,2,2)))+moddaymon0-1
      ;CALDAT,modjuls,modmon,modday,modyear,modhour,modmin

      jul_1h=0.041666
      modval=WHERE(modjuls GE omijulmin-2*jul_1h AND modjuls LE omijulmax+2*jul_1h,nmodfile)
      modfiles=modfiles[modval]
      mod03files=mod03files[modval]

      modlons=[]  &  modlats=[]  & modtimes=[] 
      mod03lons=[]  &  mod03lats=[]  & mod03times=[] 
      modcfs=[]   &  modctps=[]  & modcphases=[]
      mod03cots = []  
      modids = []  & mod03ids = []
;plot,pixlon,pixlat, /nodat, xrange=[-180,-160],yrange=[-20,20] 
;oplot, mlon,mlat,psym=2

      FOR ifile=0,nmodfile-1 DO BEGIN
        modfile=modfiles[ifile]
        mod03file=mod03files[ifile]
        modres=READ_MODIS_MYD06(modfile)
        mod03res=READ_MODIS_MOD03(mod03file)
        tmp=WHERE(modres.lon GE limit[1] AND modres.lon LE limit[3] AND $
                  modres.lat GE limit[0] AND modres.lat LE limit[2], ntmp)

        tmp1=WHERE(mod03res.lon GE limit[1] AND mod03res.lon LE limit[3] AND $
                  mod03res.lat GE limit[0] AND mod03res.lat LE limit[2], ntmp1)

        IF ntmp NE 0 THEN BEGIN
          print, '  READ modfile : ', modfile, ntmp
          modlon=modres.lon[tmp]
          modlat=modres.lat[tmp]
          modtime=modres.jtime[tmp]
          modctp=modres.ctp[tmp]
          modcf=modres.cf[tmp]
          modcphase=modres.cphase[tmp]     
   
          modlons       = [modlons, modlon]
          modlats       = [modlats, modlat]
          modtimes      = [modtimes, modtime]
          modctps       = [modctps, modctp]
          modcfs        = [modcfs, modcf]
          modcphases    = [modcphases, modcphase]

          modids        = [modids, STRARR(ntmp)+STRMID(modfile,34,17,/rev)]
          mod03ids      = [mod03ids, STRARR(ntmp1)+STRMID(mod03file,34,17,/rev)]
        ENDIF  ; ntmp

        IF ntmp1 NE 0 THEN BEGIN
          mod03lons       = [mod03lons, mod03res.lon[tmp1]]
          mod03lats       = [mod03lats, mod03res.lat[tmp1]]
          mod03times       = [mod03times, mod03res.jtime[tmp1]]
          mod03cots       = [mod03cots, modres.cot[tmp1]]
        ENDIF
      ENDFOR  ; ifile

      ;omivars={lon:omilons, lat:omilats, time:omitimes, toz:omitozs,$
               ;pixlat:pixlats, pixlon:pixlons, pixs:omipixs, lines:omilines, orbs:omiorbs, $
               ;ref331:omir331s, cf:omicfs, pcld:omipclds, qf:omiqfs, bloz:omiblozs}
      ;modvars={lon:modlons, lat:modlats, time:modtimes, ctp:modctps, cf:modcfs, cphase:modcphases,$
               ;hlon:mod03lons,hlat:mod03lats,htime:mod03times,cot:mod03cots,ids:modids,hids:mod03ids}
      ;IF call_xdr EQ 1 THEN BEGIN
        ;SAVE, file=xdrfile, omivars, modvars, /xdr
      ;ENDIF ; call_xdr


      ;RESTORE, xdrfile


  ;  ; collocation
      npo=N_ELEMENTS(omilon_1d)
      ;time_bo=120*60  ; (unit: second)
      dist_bo=20   ; distance limit for nearest modis center from omi center (unit : km)
      omictp=FLTARR(npo)
      omicf=FLTARR(npo)
      omicphase=FLTARR(npo)
      omicot = fltarr(npo)

      ;umodids = modids[UNIQ(modids,SORT(modids))]  ; extract uniq ids
      ;nids=N_ELEMENTS(umodids) 
      ;id_list=list()
      ;FOR i=0,nids -1 DO BEGIN
        ;hid_tmp=WHERE(mod03ids EQ umodids[i])
        ;id_list.add,hid_tmp
      ;ENDFOR

      opixlon=pixlon_1d
      opixlat=pixlat_1d

      hlon=mod03lons
      hlat=mod03lats
      htime=mod03times
      hcot=mod03cots

      mlon=modlons
      mlat=modlats
      mtime=modtimes

  ;plot,pixlon,pixlat, /nodat, xrange=[-180,-160],yrange=[-20,20] 
  ;oplot, mlon,mlat,psym=2
      FOR ipo=0,npo-1 DO BEGIN
        lon_min=MIN(opixlon[ipo,*])
        lon_max=MAX(opixlon[ipo,*])
        lat_min=MIN(opixlat[ipo,*])
        lat_max=MAX(opixlat[ipo,*])
        IF lon_min GT lon_max THEN BEGIN
        ;5km spatial resolution
          ;tmp=WHERE(modvars.lon GE lon_min AND modvars.lon LE lon_max AND $
                    ;modvars.lat GE lat_min AND modvars.lat LE lat_max AND $
                    ;ABS(modvars.time-omivars.time[ipo]) LE time_bo, ntmp) 
          tmp=WHERE(mlon GE lon_min AND mlon LE 180     AND $
                    mlon GE -180    AND mlon LE lon_max AND $
                    mlat GE lat_min AND mlat LE lat_max, ntmp) 
          ;the_id=modids[tmp[0]]

        ;1km spatial resolution
          ;idpos=(WHERE(umodids EQ the_id))[0]
          ;idval=id_list[idpos]

          ;tmp1=WHERE(modvars.hlon[idval] GE lon_min AND modvars.hlon[idval] LE lon_max AND $
                     ;modvars.hlat[idval] GE lat_min AND modvars.hlat[idval] LE lat_max AND $
                    ;ABS(modvars.htime[idval]-omivars.time[ipo]) LE time_bo, ntmp1) 
          ;tmp1=WHERE(hlon[idval] GE lon_min AND hlon[idval] LE 180      AND $
                     ;hlon[idval] GE -180    AND hlat[idval] LE lon_max  AND $
                     ;hlat[idval] GE lat_min AND hlat[idval] LE lat_max, ntmp1) 
          tmp1=WHERE(hlon GE lon_min AND hlon LE 180      AND $
                     hlon GE -180    AND hlat LE lon_max  AND $
                     hlat GE lat_min AND hlat LE lat_max, ntmp1) 
        ENDIF ELSE BEGIN

        ;5km spatial resolution
          ;tmp=WHERE(modvars.lon GE lon_min AND modvars.lon LE lon_max AND $
                    ;modvars.lat GE lat_min AND modvars.lat LE lat_max AND $
                    ;ABS(modvars.time-omivars.time[ipo]) LE time_bo, ntmp) 
          tmp=WHERE(mlon GE lon_min AND mlon LE lon_max AND $
                    mlat GE lat_min AND mlat LE lat_max, ntmp) 
          ;the_id=modids[tmp[0]]

        ;1km spatial resolution
          ;idpos=(WHERE(umodids EQ the_id))[0]
          ;idval=id_list[idpos]

          ;tmp1=WHERE(modvars.hlon[idval] GE lon_min AND modvars.hlon[idval] LE lon_max AND $
                     ;modvars.hlat[idval] GE lat_min AND modvars.hlat[idval] LE lat_max AND $
                    ;ABS(modvars.htime[idval]-omivars.time[ipo]) LE time_bo, ntmp1) 
          ;tmp1=WHERE(hlon[idval] GE lon_min AND hlon[idval] LE lon_max AND $
                     ;hlat[idval] GE lat_min AND hlat[idval] LE lat_max, ntmp1) 
          tmp1=WHERE(hlon GE lon_min AND hlon LE lon_max AND $
                     hlat GE lat_min AND hlat LE lat_max, ntmp1) 

        ENDELSE

  ; check point
  ;print, ipo, ntmp
  ;if ntmp EQ 0 THEN BEGIN 
  ;oplot, [lon_min,lon_max,lon_max,lon_min,lon_min],[lat_min,lat_min,lat_max,lat_max,lat_min],color=cgcolor('red')
  ;ENDIF

        IF ntmp NE 0 THEN BEGIN
          ; ctp collocation
          ctps=modctps[tmp]
          val=WHERE(ctps NE -999, nval)
          avgctp=(nval NE 0) ? MEAN(ctps[val]) : -999
          omictp[ipo]=avgctp     

          ; cf collocation
          cfs=modcfs[tmp]
          val=WHERE(cfs NE -999, nval)
          avgcf=(nval NE 0) ? MEAN(cfs[val]) : -999
          omicf[ipo]=avgcf     

          ; cf collocation
          cphases=modcphases[tmp]
          val=WHERE(cphases NE -999, nval)
          IF nval NE 0 THEN BEGIN
            dists=ABS(omilon_1d[ipo]-modlons[tmp[val]])+ABS(omilat_1d[ipo]-modlats[tmp[val]])
            mindist=WHERE(dists EQ MIN(dists), nmindist)
            mindist=mindist[0]
            mdist=MAP_2POINTS(omilon_1d[ipo], omilat_1d[ipo], $
                              modlons[tmp[val[mindist]]], modlats[tmp[val[mindist]]], /meters)/1000.
            sel_cphase=(mdist LE dist_bo) ? cphases[val[mindist]] : -999
            
          ENDIF ELSE BEGIN  ; nval
            sel_cphase=-999
          ENDELSE
          omicphase[ipo]=sel_cphase

        ENDIF ELSE BEGIN ; ntmp
          omictp[ipo] =-999
          omicf[ipo]  =-999
          omicphase[ipo] =-999
        ENDELSE ; ntmp

        IF ntmp1 ne 0 then begin 
          ; ctp collocation
          ;cots=hcot[idval[tmp1]]
          cots=hcot[tmp1]
          val=WHERE(cots NE -999, nval)
          avgcot=(nval NE 0) ? MEAN(cots[val]) : -999
          omicot[ipo]=avgcot    
        ENDIF ELSE BEGIN
          omicot[ipo]= -999
        ENDELSE
        ; check point
        ;CALDAT,(JULDAY(1,1,1993,0,0,0)*86400.0d0+ABS(omivars.time[ipo]-modvars.time[tmp]))/86400.0d0,mon,day,year,hour,min,sec
        ;time_diff='h'+STRING(hour,F='(I02)')+'-m'+ STRING(min,F='(I02)')+'-s'+STRING(sec,F='(I02)')
        ;print, ipo, omicphase[ipo], mdist
        ;print, ipo, time_diff[0]

      ENDFOR  ; ipix
      SAVE, FILE=xdr_ctp, nx,ny, omictp, omival, omilon,omilat,pixlon, pixlat, /XDR
      PRINT, '  SAVE :: ', xdr_ctp
    ENDIF ELSE BEGIN  ; call_xdr
      RESTORE, xdr_ctp
    ENDELSE  ; call_xdr


;plot,pixlon,pixlat, /nodat, xrange=[-180,-160],yrange=[-20,20] 
;oplot, omilon_1d, omilat_1d,psym=2
;oplot, modlons,modlats,psym=2,color=cgcolor('RED')

    mod2omi_ctp=FLTARR(nx,ny)-999
    mod2omi_cot=FLTARR(nx,ny)-999
    ;omi2mod_qflag=FLTARR(nx,ny)
    mod2omi_ctp[omival]=omictp    
    mod2omi_cot[omival]=omicot    
    nan=WHERE(mod2omi_ctp EQ -999,nnan)
    IF nnan NE 0 THEN mod2omi_ctp[nan]=-999    
    nan2=WHERE(mod2omi_cot EQ -999,nnan)
    IF nnan NE 0 THEN mod2omi_ctp[nan]=-999    

    IF write_on THEN BEGIN
      out_ctp='/home/o3p_hs/GEMS/o3p/v0.4.3/src/tmp_modctp/CTP_o'+STRING(orbs[id],F='(I05)')+'.dat'
      OPENW, lun, out_ctp, /GET_LUN
        form='('+STRING(nx,F='(I2)')+'I04)'
        FOR i = 0 , ny-1 do begin
          PRINTF,LUN, ROUND(mod2omi_ctp(*,i)), format=form
        ENDFOR
      FREE_LUN, LUN
      PRINT,'    WRITE outfiles : '+out_ctp

    ENDIF 

    if cot_save then begin
      SAVE, FILE=xdr_cot, mod2omi_cot,omilon,omilat,pixlon,pixlat ,/XDR
      PRINT, '  SAVE :: ', xdr_cot
    endif
    
    if plot_on then begin
      ;+---------------------------------------------------------------------------+
      ; PLOT column ozone
      ;+---------------------------------------------------------------------------+
      out_name='/home/o3p_hs/results/COT_o'+STRING(orbs[id],F='(I05)')
      out_ps=out_name+'.ps'
      out_png=out_name+'.png'

      CGPs_OPEN, out_ps ,xsize=10, ysize=7, /nomatch, /inch, xoffset=0.5, yoffset=0.5
      pos=[0.12,0.15,0.89,0.92]
      !p.background=0
      !p.position=pos
      !p.charsize=1.9
      ct=72
      loadct_wb,ct

      MAP_SET,  LIMIT=limit, XMARGIN=0, YMARGIN=0,/CYLIN,/NOERA,/NOBOR,POS=pos

      nlev=51
      ;lev=(findgen(nlev))*20  ; ctp
      lev=(findgen(nlev))  ; cot
      barcol=REVERSE(FIX(FINDGEN(nlev-1)*253/nlev))
      lev[0]=-999  &  lev[nlev-1]=9999

      
      xx=omilon
      yy=omilat
      ;zz=mod2omi_ctp  
      zz=mod2omi_cot  
      ;xx=modlons
      ;yy=modlats
      ;zz=modctps

      ;xx=hlon
      ;yy=hlat
      ;zz=hcot

      corlon=pixlon
      corlat=pixlat
      nan = where( zz EQ -999 OR xx LE limit[1] or xx GE limit[3] or yy LE limit[0] OR yy GE limit[2], nnan)
      zz[nan]=!values.f_nan
      zzval=WHERE(FINITE(zz) EQ 1,nval)
      npo=nval
      IF npo NE 0 THEN BEGIN
        z=zz[zzval]
        tlon=xx[zzval]  &  tlat=yy[zzval]
        tpix=omipix[zzval]  &  tline=omiline[zzval]
        FOR i=0, npo-1 DO BEGIN
          col=-999
          FOR ilev=0,nlev-2 DO BEGIN
            IF (z[i] GE lev[ilev] AND z[i] LT lev[ilev+1]) THEN col=barcol[ilev]
          ENDFOR
            ;PLOTS, tlon[i], tlat[i], PSYM=plotsym_fn(/box,scale=0.7,/fill), COLOR=col
            tcorlon=REFORM(corlon[tpix[i]-1,tline[i]-1,*])
            tcorlat=REFORM(corlat[tpix[i]-1,tline[i]-1,*])
            POLYFILL, [tcorlon,tcorlon[0]], [tcorlat,tcorlat[0]], COLOR=col
        ENDFOR
        ;for plotting prfile cross section
      ENDIF ;npo
      ;CONTOUR, tlons[sel_po], tlats[sel_po], PSYM=plotsym_fn(/CIR,scale=0.7,/fill), COLOR=CGCOLOR('MAGENTA')

      MAP_CONTINENTS,/CONTINENT,/COUNT,MLINETHICK=1.8,/COUNTRY,/HIRES
      ;MAP_GRID, /box, thick=5
      CONTOUR, indgen(2,2), indgen(2,2), indgen(2,2), /NODAT,/NOERA,CHARSIZE=1.9,CHARTHICK=2, $
               XSTYLE=1, YSTYLE=1,YTICKFORMAT='(I3)',XTICKFORMAT='(i4)',XTICKINTERVAL=10,YTICKINTERVAL=10, $
               XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)], $
               TITLE=title, XTITLE = 'Longitude',YTITLE='Latitude', POS=pos

      COLORBAR_2, lev, barcol, /COL, YS =0.77, XS = 0.015, CHARSIZE=1.9, CHARTHICK=2, LEVELIND = FINDGEN(100)*5, $
                               FORMAT='(I4)',UNIT='hPa',/IVE,/NOFIRST;,/NOFRAME

      cgPs_Close,density=800, /png
      hs_filetrans,out_png,/DEL
    endif
TOC
  ENDFOR  ;id






stop
END
