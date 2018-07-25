;===================================================================================================
; PROGRAM NAME:
;  map_l2_coz_wnd
;
; PURPOSE:
;  mapping column ozone with wind vector
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
;  Daegeun Shin (geun)
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
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO map_l2_coz_wnd

  unit_of_o3 = '(DU)' ; '(ppb)'

  xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr/'
  outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
  omil2_files = file_search(outpath+'GEM*')


  ;sel_date='20050601'
  coz_ty=2  ;0:oc, 1:soc, 2: toc
  IF coz_ty EQ 0 THEN BEGIN
    selpr=500   ; wind press
  ENDIF ELSE IF coz_ty EQ 1 THEN BEGIN
    selpr=100
  ENDIF ELSE IF coz_ty EQ 2 THEN BEGIN
    selpr=700
  ENDIF
  
  limit= [5,100,45,140] 
  

  ; cross section profile
  sel_lat=42
  sel_lon=123


  tozprof=[]
  tpmid=[]
  tlon=[]
  tlat=[]
  tozcol=[]
  tntp=[]
  tpres=[]
  
  fnum=n_elements(omil2_files)

  FOR ifile=0,fnum-1 DO BEGIN
    omil2=omil2_files[ifile] 
    print,omil2
    tmp=STRSPLIT(omil2,'/',COUNT=n) 
    fname=STRMID(omil2, tmp[n-1],STRLEN(omil2)-tmp[n-1]-4)   
    omil2xdr=xdrpath+fname+'.xdr' 
    read_xdr = 0 
    IF read_xdr eq 0 then begin
        read_omil2_file, omil2, nl, nf, nalb, ngas, naer, ominprof, $
        omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, omicfrac, omictp, omicldflg, omiai, omiutc,  $ 
        omintp, ominw,  omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow, $
        omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos, $
        ozprofs,omicol, omitrace,  omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec, $
        omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
        nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
        fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow, $
        /get_snr, /get_fitvar,/get_avgk ,/get_wf, /get_covar, /get_correl, /get_contri, /get_fitres,/get_radcal,$
        varname=varname, omiorb=omiorb
      print, 'open data:', omil2
      stop
      SAVE,FILE=omil2xdr, omil2, nl, nf, nalb, ngas, naer, ominprof, $
        omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, omicfrac, omictp, omicldflg, omiai, omiutc,  $ 
        omintp, ominw,  omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow, $
        omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos, $
        ozprofs,omicol, omitrace,  omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec, $
        omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
        nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
        fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow, $
        omiorb, varname, $
        /XDR
      PRINT,'  :: SAVE '+omil2xdr
    ENDIF ELSE BEGIN
      restore, omil2xdr
      PRINT,'  :: RESTORE '+omil2xdr
    ENDELSE

    print, ifile, omiyear[0],omimon[0],omiday[0],omiutc[0]
    date=STRING(omiyear[0],F='(I04)')+STRING(omimon[0],F='(I02)')+STRING(omiday[0],F='(I02)')

    ;IF date EQ sel_date THEN BEGIN
      sel = where( omiavgres[*,0] le 2 and omirms[*,0] le 2 and omiexval gt 0 and omiexval lt 100 and omisza LE 70 and $
                   omilon[*,4] GT limit[1] AND omilon[*,4] LT limit[3] AND omilat[*,4] GT limit[0] AND omilat[*,4] LT limit[2] $
                   , nsel)

      IF nsel NE 0 THEN BEGIN
        lat0   = omilat(sel,4)
        lon0   = omilon(sel,4)
        ozcol  = omicol(sel,coz_ty,0)
        ozprof  = REFORM(ozprofs[sel,2,*])
        tlon   =  [tlon,lon0]
        tlat   =  [tlat,lat0]
        tntp   =  [tntp,omintp[sel]]
        tozcol =  [tozcol,ozcol]
        tozprof = [tozprof,ozprof]

        ps    = REFORM(atmos[sel,0,*])
        pmid  = fltarr(nsel,nl) & for i = 0 , nl-1 do pmid(*,i)=exp((alog(ps(*,i)) + alog(ps(*,i+1)))*0.5)
        tpmid = [tpmid,pmid]
        tpres = [tpres, ps]
      ENDIF ELSE BEGIN
         print , 'No pixl selected'
      ENDELSE
    ;ENDIF
  ENDFOR ; daily files loop

  IF mean_on EQ 1 THEN BEGIN
    ;+---------------------------------------------------------------------------+
    ; PLOT mean field  
    ;+---------------------------------------------------------------------------+
    out_name='avgmap_'  ;  _o'+STRING(orb,F='(I05)')
    out_ps=out_name+'.ps'
    out_png=out_name+'.png'

    ;CGPs_OPEN, out_ps ,xsize=10, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
    CGPs_OPEN, out_ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
    pos=[0.05,0.15,0.82,0.92]

    !p.background=0
    !p.position=pos
    loadct_wb,72
   
    MAP_SET, /CONIC, 40, 120, STANDARD_PARALLELS=[20,60], LIMIT=limit,/GRID, LATDEL=10, LONDEL=10, pos=pos 

    nlev=51
    IF coz_ty EQ 0 THEN BEGIN
      lev=(findgen(nlev))*3 + 250  ; toz
    ENDIF ELSE IF coz_ty EQ 1 THEN BEGIN
      lev=(findgen(nlev))*3 + 220  ; str oz
    ENDIF ELSE IF coz_ty EQ 2 THEN BEGIN
      lev=(findgen(nlev))*1 + 20  ; tro oz
    ENDIF
    barcol=REVERSE(FIX(FINDGEN(nlev-1)*253/nlev))
    lev[0]=-999  &  lev[nlev-1]=9999

    z=tozcol
    ;npo=N_ELEMENTS(tlon)
    ;FOR i=0, npo-1 DO BEGIN
      ;col=-999
      ;FOR ilev=0,nlev-2 DO BEGIN
        ;IF (z[i] GE lev[ilev] AND z[i] LT lev[ilev+1]) THEN col=barcol[ilev]
      ;ENDFOR
       ;PLOTS, tlon[i], tlat[i], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=col
    ;ENDFOR

    ;gridding
    dlon=0.5
    dlat=0.5
    slon=limit[1]  &  elon=limit[3]
    slat=limit[0]  &  elat=limit[2]
    nlon = (elon-slon)/dlon
    nlat = (elat-slat)/dlat
    sumgr=fltarr(nlon,nlat)  
    numgr=fltarr(nlon,nlat)
    glon=fltarr(nlon,nlat)
    glat=fltarr(nlon,nlat)
    data=z

    glon1=[]
    glat1=[]
    sumgr1=[]
    numgr1=[]

    for ilon=0,nlon-1 do begin
    for ilat=0,nlat-1 do begin
      tmp=WHERE(tlon GE slon+ilon*dlon AND tlon LE slon+(ilon+1)*dlon AND $
                tlat GE slat+ilat*dlat AND tlat LE slat+(ilat+1)*dlat, ntmp)
      if ntmp ne 0 then begin
        sumgr[ilon,ilat]=total(data[tmp])
      endif
      numgr[ilon,ilat]=ntmp
      glon[ilon,ilat]=slon+ilon*dlon+0.5*dlon
      glat[ilon,ilat]=slat+ilat*dlat+0.5*dlat
    endfor
    endfor
    avggr=sumgr/numgr

    contour,avggr, glon, glat, c_colors=barcol, levels=lev, /over,/cell
    MAP_CONTINENTS,/CONTINENT,/COUNT,MLINETHICK=1.8,/COUNTRY,/HIRES
    MAP_GRID, /box, latdel=10, londel=10, thick=5

    COLORBAR_2, lev, barcol, /COL, YS =0.77, XS = 0.015, CHARSIZE=1.9, CHARTHICK=2, LEVELIND = FINDGEN(100)*5, $
                             FORMAT='(I3)',UNIT='DU', lowleft=[0.87,0.15],/NOFIRST,/right;,/NOFRAME

    cgPs_Close,density=800, /png
    file_trans_geun,out_png,/DEL

  ENDIF ELSE BEGIN

    sellat_po=WHERE(ABS(tlat-sel_lat) LE 0.1,nsellat)
    sellat_po=sellat_po[sort(tlon[sellat_po])]

    ;sellon_po=WHERE(ABS(tlon-sel_lon) LE 0.02,nsellon)
    ;sellon_po=sellon_po[sort(tlat[sellon_po])]
    ;+---------------------------------------------------------------------------+
    ; PLOT column ozone
    ;+---------------------------------------------------------------------------+
    out_name='map_l2'  ;  _o'+STRING(orb,F='(I05)')
    out_ps=out_name+'.ps'
    out_png=out_name+'.png'

    ;CGPs_OPEN, out_ps ,xsize=10, ysize=7, /nomatch, /inch, xoffset=0.5, yoffset=0.5
    ;pos=[0.12,0.15,0.89,0.92]

    ;CGPs_OPEN, out_ps ,xsize=10, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
    CGPs_OPEN, out_ps ,xsize=7, ysize=5, /nomatch, /inch, xoffset=0.5, yoffset=0.5
    pos=[0.05,0.15,0.82,0.92]

    !p.background=0
    !p.position=pos
    !p.charsize=1.9
    loadct_wb,72
   
    ;MAP_SET,  LIMIT=limit, XMARGIN=0, YMARGIN=0,/CYLIN,/NOERA,/NOBOR,POS=pos
    MAP_SET, /CONIC, 40, 120, STANDARD_PARALLELS=[20,60], LIMIT=limit,/GRID, LATDEL=10, LONDEL=10, pos=pos 

    nlev=51
    IF coz_ty EQ 0 THEN BEGIN
      lev=(findgen(nlev))*3 + 250  ; toz
    ENDIF ELSE IF coz_ty EQ 1 THEN BEGIN
      lev=(findgen(nlev))*3 + 220  ; str oz
    ENDIF ELSE IF coz_ty EQ 2 THEN BEGIN
      lev=(findgen(nlev))*1 + 20  ; tro oz
    ENDIF
    barcol=REVERSE(FIX(FINDGEN(nlev-1)*253/nlev))
    lev[0]=-999  &  lev[nlev-1]=9999

    z=tozcol
    npo=N_ELEMENTS(tlon)
    FOR i=0, npo-1 DO BEGIN
      col=-999
      FOR ilev=0,nlev-2 DO BEGIN
        IF (z[i] GE lev[ilev] AND z[i] LT lev[ilev+1]) THEN col=barcol[ilev]
      ENDFOR
       PLOTS, tlon[i], tlat[i], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=col
    ENDFOR
    prepare_fnl_geun,sel_date,fnlout

    fpres=fnlout.pres
    selpo=WHERE(ABS(fpres-selpr) EQ MIN(ABS(fpres-selpr)))
    lon0=fnlout.lon
    lat0=fnlout.lat
    fuwnd=fnlout.uwind[*,*,selpo]
    fvwnd=fnlout.vwind[*,*,selpo]
    title=STRING(fpres[selpo],F='(I4)')+'hPa Wind' 

    nlon=N_ELEMENTS(lon0)  &  nlat=N_ELEMENTS(lat0)
    flon=FLTARR(nlon,nlat)
    flat=FLTARR(nlon,nlat)
    FOR i = 0, nlon-1 DO flon[i,*]=lon0[i]
    FOR j = 0, nlat-1 DO flat[*,j]=lat0[j]

    MAP_CONTINENTS,/CONTINENT,/COUNT,MLINETHICK=1.8,/COUNTRY,/HIRES
    MAP_GRID, /box, latdel=10, londel=10, thick=5
    ;CONTOUR, indgen(2,2), indgen(2,2), indgen(2,2), /NODAT,/NOERA,CHARSIZE=1.9,CHARTHICK=2, $
             ;XSTYLE=1, YSTYLE=1,YTICKFORMAT='(I3)',XTICKFORMAT='(i4)',XTICKINTERVAL=20,YTICKINTERVAL=10, $
             ;XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)], $
             ;TITLE=title, XTITLE = 'Longitude',YTITLE='Latitude', POS=pos
    ;PARTVELVEC, fuwnd, fvwnd, flon, flat, COLOR=255, FRACTION=0.15,/OVER, noclip=0, length=0.03
    PARTVELVEC, fuwnd, fvwnd, flon, flat, COLOR=255, FRACTION=0.7,/OVER, noclip=0, length=0.02

    ;PLOTS,[limit[1],limit[3]] , [30,30], COLOR=CGCOLOR('red'), thick=2
    ;PLOTS, tlon[sellat_po], tlat[sellat_po], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=CGCOLOR('red')
    ;PLOTS, tlon[sellon_po], tlat[sellon_po], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=CGCOLOR('GREEN')

    ;COLORBAR_2, lev, barcol, /COL, YS =0.77, XS = 0.015, CHARSIZE=1.9, CHARTHICK=2, LEVELIND = FINDGEN(100)*5, $
                             ;FORMAT='(I3)',UNIT=unit,/IVE,/NOFIRST;,/NOFRAME
    COLORBAR_2, lev, barcol, /COL, YS =0.77, XS = 0.015, CHARSIZE=1.9, CHARTHICK=2, LEVELIND = FINDGEN(100)*5, $
                             FORMAT='(I3)',UNIT='DU', lowleft=[0.87,0.15],/NOFIRST,/right;,/NOFRAME

    cgPs_Close,density=800, /png
    file_trans_geun,out_png,/DEL
  stop

    ;+---------------------------------------------------------------------------+
    ; PLOT column cross section (profile)
    ;+---------------------------------------------------------------------------+
    out_name='o3p_l2'  ;  _o'+STRING(orb,F='(I05)')
    out_png=out_name+'.png'
    CGPs_OPEN, out_name+'.ps' ,xsize=8, ysize=4, /nomatch, /inch, xoffset=0.5, yoffset=0.5

    loadct_dg,33
    !p.position=[0.12,0.16,0.90,0.90]
    !p.charsize =1.5

    xdata = tlon[sellat_po] &  xtitle = '!6Longitude'
    ydata = tpmid[sellat_po,*] & ytitle = '!6Pressure(hPa)'
    ;xdata = tlat[sellon_po] &  xtitle = '!6Latitude'
    ;ydata = tpmid[sellon_po,*] & ytitle = '!6Pressure(hPa)'

    ;xrange = [min(xdata),max(xdata)]
    xrange = [limit[1],limit[3]] & xtick = 10
    ;xrange = [limit[0],limit[2]] & xtick = 10

    yrange = [1000,10]
    ytickv    = [1000,100,10,1,0.1]
    ytickname ='!6'+['10!U3!N','10!U2!N','10!U1!N','10!U0!N', '10!U-1!N']
    seltick   = where ( ytickv ge yrange(1), nytick )
    ytickv    = ytickv(seltick)
    ytickname = ytickname(seltick)

    ;+++++++++ ozone++++++++++++
    level  = [-999, findgen(21)*2.5,1000]

    ncol   = n_elements(level)-1
    color  = findgen(ncol)*250/ncol + 2

    ;data = tozprof[sellon_po,*]  & title = '!6 Ozone Profile Difference'
    data = tozprof[sellat_po,*]  & title = '!6 Ozone Profile Difference'
    plot,   indgen(2), indgen(2), /nodata, xrange=xrange, yrange=yrange, xtickinterval = xtick,$
          title=title, xtitle = xtitle, ytitle=ytitle, yticks=n_elements(ytickv)-1,ytickv=ytickv, ytickname=ytickname, /ylog
    contour, data, xdata, ydata, levels=level, c_colors=color, /cell, /over
    PLOTS, xdata, tpres[sellat_po,tntp[sellat_po]], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=CGCOLOR('red')
    ;PLOTS, xdata, tpres[sellon_po,tntp[sellon_po]], PSYM=plotsym_fn(/cir,scale=0.7,/fill), COLOR=CGCOLOR('red')

    colorbar_2, level, color, format='(F5.2)', charsize=1.25, /ive, /col, /nofra,/nofirst, $
               xs=0.01,levelind=findgen(100)*5,unit='DU'
    cgPs_Close,density=800, /png
    file_trans_geun,out_name+'.png',/DEL

  ENDELSE
stop

END
