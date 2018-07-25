;===================================================================================================
;
; PROGRAM NAME:
;  analy_fnl_v2
;
; PURPOSE:
;  analyzing fnl daily and monthly data 
;
; PROCESS:
;  filtering bad date
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
;  Updated by geun
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO analy_fnl_v2
 
  fnl_base='/home/geun/2_O3PR/ATMOS/fnl13.75LST/'
  year='2015'
  ;year='2016'
  save_xdr =0

; user selection
  fnlty=1  ; 1: fnlsp, 2: fnlst, 3: fnltt, 4: fnltp, 5: fnltemp
  ;sel_lay=17 ; layer selection for fnlty=5 condition 2 : 30 hPa (o3p max) 5: 100hPa, 13: 500hPa, 17: 700hPa, 25: 1000hPa
  ztype=1 ; 0: monthly average, 1:monthly stddev, 2: daily seletion diff, 3 : daily selection_diff
  abson=0 ; 0: difference, 1: absolute difference
  omion=0 ; 1: omi track
  ;sel_day='20160607' ; day selection for ztype=2 condition
  ;sel_day='20160901' ; day selection for ztype=2 condition
  ;sel_day='20080701' ; day selection for ztype=2 condition
  ;sel_day='20081101' ; day selection for ztype=2 condition


;fnl parameter
  nlon=360
  nlat=180
  nl=26

  ;glbfnl_xdr='../xdr/glbfnl.xdr'
  ;glbfnl_xdr='../xdr/glbfnl_2016.xdr'
  ;glbfnl_xdr='glbfnl_2012.xdr'
  ;glbfnl_xdr='../xdr/glbfnl_2008.xdr'
  glbfnl_xdr='../xdr/glbfnl_'+year+'.xdr'
TIC

; omi track 
  base_dir='/home/geun/2_O3PR/OZBOREAS-OMI/src/geun_storage/out_conserv/'  
  omil2=base_dir+'case_atmcontrol_sfcp/OMIO3PROF_atmcontrol_base-o19744_X15-15.out' 

  read_omil2_file, omil2, nl_omi, nf, nalb, ngas, naer, ominprof, $
       omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, $
       omicfrac, omictp, omicldflg, omiai, omiutc, omintp,  $
       ominw, omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow,            $
       omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos,          $
       ozprofs, omicol, omitrace, omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec,   $
       omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
       nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
       fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow, $
       orbits=orbits, orbspix=orbspix, orbepix=orbepix,  omiorb=omiorb, $
       /get_correl, /get_avgk, /get_fitres, /get_radcal,/get_covar, /get_snr
  olats=omilat[*,4]
  olons=omilon[*,4]

  IF save_xdr EQ 1 THEN BEGIN
;+---------------------------------------------------------------------------+
; generating fnl daily/monthly variable 
;+---------------------------------------------------------------------------+
    fnl_day1=FILE_SEARCH(fnl_base+'fnlsp/fnlsp_'+year+'*.dat', COUNT=nday)
    fnl_day2=FILE_SEARCH(fnl_base+'fnlst/fnlst_'+year+'*.dat',COUNT=nday)
    ;fnl_day3=FILE_SEARCH(fnl_base+'fnltt/fnltt_'+year+'*',COUNT=nday)
    fnl_day4=FILE_SEARCH(fnl_base+'fnltp/fnltp_'+year+'*.dat',COUNT=nday)
    fnl_day5=FILE_SEARCH(fnl_base+'fnltemp/fnltemp_'+year+'*.dat',COUNT=nday)

    fnl_mon1=FILE_SEARCH(fnl_base+'fnlsp/fnlspavg*.dat', COUNT=nmon)
    fnl_mon2=FILE_SEARCH(fnl_base+'fnlst/fnlstavg*.dat', COUNT=nmon)
    ;fnl_mon3=FILE_SEARCH(fnl_base+'fnltt/fnlttavg*',COUNT=nmon)
    fnl_mon4=FILE_SEARCH(fnl_base+'fnltp/fnltpavg*.dat',COUNT=nmon)
    fnl_mon5=FILE_SEARCH(fnl_base+'fnltemp/fnltempavg*.dat',COUNT=nmon)

    glbfnl_day_list=LIST()
    glbfnl_mon_list=LIST()

    julday_ref=JULDAY(1,1,year)
    FOR idaymon=0,1 DO BEGIN ; 0 : day 1: mon

      fnl1=(idaymon EQ 0) ? fnl_day1 : fnl_mon1
      fnl2=(idaymon EQ 0) ? fnl_day2 : fnl_mon2
      ;fnl3=(idaymon EQ 0) ? fnl_day3 : fnl_mon3
      fnl4=(idaymon EQ 0) ? fnl_day4 : fnl_mon4
      fnl5=(idaymon EQ 0) ? fnl_day5 : fnl_mon5

      fnl1_julday=(idaymon EQ 0 ) ? JULDAY(STRMID(fnl1,7,2,/REV),STRMID(fnl1,5,2,/REV),year) : STRMID(fnl1,5,2,/REV)
      fnl2_julday=(idaymon EQ 0 ) ? jULDAY(STRMID(fnl2,7,2,/REV),STRMID(fnl2,5,2,/REV),year) : STRMID(fnl2,5,2,/REV) 
      fnl4_julday=(idaymon EQ 0 ) ? jULDAY(STRMID(fnl4,7,2,/REV),STRMID(fnl4,5,2,/REV),year) : STRMID(fnl4,5,2,/REV)
      fnl5_julday=(idaymon EQ 0 ) ? jULDAY(STRMID(fnl5,7,2,/REV),STRMID(fnl5,5,2,/REV),year) : STRMID(fnl5,5,2,/REV)

      nfile=(idaymon EQ 0) ? 365 : 11
      FOR ifile=0,nfile DO BEGIN
        julday0=(idaymon EQ 0) ? julday_ref+ifile : STRING(ifile+1,F='(I02)')

        tmp1=WHERE(fnl1_julday EQ julday0, ntmp1)        
        tmp2=WHERE(fnl2_julday EQ julday0, ntmp2)        
        tmp4=WHERE(fnl4_julday EQ julday0, ntmp4)        
        tmp5=WHERE(fnl5_julday EQ julday0, ntmp5)        
        
        fnlfile1=fnl1[tmp1[0]]  &  fnlfile2=fnl2[tmp2[0]] ; &  fnlfile3=fnl3[ifile] 
        fnlfile4=fnl4[tmp4[0]]  &  fnlfile5=fnl5[tmp5[0]] 
        CALDAT, julday0, mm, dd
        date=(idaymon EQ 0) ? year+STRING(mm,F='(I02)')+STRING(dd,F='(I02)') : STRING(ifile+1,F='(I02)')
        ;date=(idaymon EQ 0) ? STRMID(fnlfile1,11,8,/REV) : STRMID(fnlfile1,5,2,/REV)

        print,'ifile = ', STRING(ifile), '       date = ', date

        OPENR, 1, fnlfile1  &  OPENR, 2, fnlfile2 ; &  OPENR, 3, fnlfile3
        OPENR, 4, fnlfile4  &  OPENR, 5, fnlfile5

        glbfnl1=INTARR(nlon,nlat)  &  glbfnl2=INTARR(nlon,nlat)  ;   &  glbfnl3=INTARR(nlon,nlat)  
        glbfnl4=INTARR(nlon,nlat)  &  glbfnl5=INTARR(nlon,nlat,nl)  &  tmp=INTARR(nlon)

        FOR j=0,nlat-1 DO BEGIN
          IF ntmp1 NE 0 THEN READF, 1, tmp, F='(360I4)'   &   glbfnl1[*,j]=tmp
          IF ntmp2 NE 0 THEN READF, 2, tmp, F='(360I3)'   &   glbfnl2[*,j]=tmp
          ;READF, 3, tmp, F='(360I3)'   &   glbfnl3[*,j]=tmp
          IF ntmp4 NE 0 THEN READF, 4, tmp, F='(360I3)'   &   glbfnl4[*,j]=tmp
        ENDFOR  ; j
        IF ntmp5 NE 0 THEN BEGIN
          FOR k=nl-1,0,-1 DO BEGIN 
            FOR j=0,nlat-1 DO BEGIN
              READF, 5, tmp, F='(360I3)'  &   glbfnl5[*,j,k]=tmp
            ENDFOR  ; j
          ENDFOR  ; k
        ENDIF

        CLOSE, 1  &  CLOSE, 2  &  CLOSE, 3  &  CLOSE, 4  &  CLOSE, 5

        fnlstr=CREATE_STRUCT('date',date)
        IF ntmp1 NE 0 THEN fnlstr=CREATE_STRUCT(fnlstr,'fnlsp',glbfnl1)
        IF ntmp2 NE 0 THEN fnlstr=CREATE_STRUCT(fnlstr,'fnlst',glbfnl2)
        IF ntmp4 NE 0 THEN fnlstr=CREATE_STRUCT(fnlstr,'fnltp',glbfnl4)
        IF ntmp5 NE 0 THEN fnlstr=CREATE_STRUCT(fnlstr,'fnltemp',glbfnl5)

        IF idaymon EQ 0 THEN glbfnl_day_list.add, fnlstr $
        ELSE glbfnl_mon_list.add, fnlstr
      ENDFOR ; ifile
    ENDFOR ; idaymon
    SAVE, FILE=glbfnl_xdr, glbfnl_day_list, glbfnl_mon_list, /XDR
    PRINT, ' :: COMPLETE to save glbfnl_xdr file'

  ENDIF ELSE BEGIN
    RESTORE, glbfnl_xdr 
    PRINT, ' :: COMPLETE to call glbfnl_xdr file'
  ENDELSE  ; save_xdr
TOC

;+---------------------------------------------------------------------------+
; selecting fnl daily/monthly variable 
;+---------------------------------------------------------------------------+
  IF fnlty EQ 1 THEN BEGIN
    varname='fnlsp'
    unit='hPa'
  ENDIF ELSE IF fnlty EQ 2 THEN BEGIN
    varname='fnlst'
    unit='K'
  ENDIF ELSE IF fnlty EQ 3 THEN BEGIN
    varname='fnltt'
    unit='K'
  ENDIF ELSE IF fnlty EQ 4 THEN BEGIN
    varname='fnltp'
    unit='hPa'
  ENDIF ELSE IF fnlty EQ 5 THEN BEGIN
    varname='fnltemp'
    unit='K'
  ENDIF ELSE BEGIN
    PRINT, ' :: Invalid fnl type was selected.'
    STOP
  ENDELSE
  PRINT, ' :: Fnl variable "'+varname+'" was selected.'

  nday=N_ELEMENTS(glbfnl_day_list)
  date=[]
  fnlval=[]
  IF fnlty NE 5 THEN BEGIN
    fnlvar_day=FLTARR(nlon,nlat,nday)*!VALUES.F_NAN
    fnlvar_mon=FLTARR(nlon,nlat,nday)*!VALUES.F_NAN
    FOR iday=0,nday-1 DO BEGIN
      tnames=tag_names(glbfnl_day_list[iday])
      tmp=WHERE(STRLOWCASE(tnames) EQ varname,ntmp)
      IF ntmp NE 0 THEN BEGIN
        res=EXECUTE('fnlvar_day[*,*,iday]=glbfnl_day_list[iday].'+varname)
        mon_idx=FIX(STRMID(glbfnl_day_list[iday].date,4,2))-1
        ;print, iday, STRMID(glbfnl_day_list[iday].file,7,2,/REV), mon_idx, F='(3I5)' 
        res=EXECUTE('fnlvar_mon[*,*,iday]=glbfnl_mon_list[mon_idx].'+varname)
      ENDIF
      date=[date,glbfnl_day_list[iday].date]
    ENDFOR
    fnlvar_diff=fnlvar_mon-fnlvar_day
    title=varname+'_diff (monthly-daily)'
  ENDIF ELSE BEGIN
    fnlvar_day=FLTARR(nlon,nlat,nl,nday)*!VALUES.F_NAN
    fnlvar_mon=FLTARR(nlon,nlat,nl,nday)*!VALUES.F_NAN
    FOR iday=0,nday-1 DO BEGIN
      tnames=tag_names(glbfnl_day_list[iday])
      tmp=WHERE(STRLOWCASE(tnames) EQ varname,ntmp)
      IF ntmp NE 0 THEN BEGIN
        res=EXECUTE('fnlvar_day[*,*,*,iday]=glbfnl_day_list[iday].'+varname)
        mon_idx=FIX(STRMID(glbfnl_day_list[iday].date,4,2))-1
        ;print, iday, STRMID(glbfnl_day_list[iday].file,7,2,/REV), mon_idx, F='(3I5)' 
        res=EXECUTE('fnlvar_mon[*,*,*,iday]=glbfnl_mon_list[mon_idx].'+varname)
      ENDIF
      date=[date,glbfnl_day_list[iday].date]
    ENDFOR
    pres=[10,20,30,50,70,FINDGEN(16)*50+100,FINDGEN(5)*25+900]
    fnlvar_diff=fnlvar_mon-fnlvar_day
    IF ztype GT 1 THEN BEGIN
      fnlvar_diff=REFORM(fnlvar_diff[*,*,sel_lay,*])
      print, ' :: '+STRING(pres[sel_lay],F='(I04)')+' was selected'
      title=varname+'_diff'+STRING(pres[sel_lay],F='(I-4)')+' (monthly-daily)'
    ENDIF
  ENDELSE
  DELVARX, glbfnl_day_list, glbfnl_mon_list

;+---------------------------------------------------------------------------+
; Mapping results
;+---------------------------------------------------------------------------+
  IF abson EQ 1 THEN fnlvar_diff=ABS(fnlvar_diff)

  IF fnlty NE 5 OR ztype GT 2 THEN BEGIN

    out_ps='fnl_comp.ps'
    out_png='fnl_comp.png'
    CGPS_OPEN, out_ps ,XSIZE=10,YSIZE=14,/NOMATCH,/INCH

    ;!P.POSITION=[0.12,0.12,0.9,0.91]
    !P.MULTI=[0, 1, 2]
    mxs=0.12  &  mxint=0.00  &  mxsize=0.77
    mys=0.07  &  myint=0.13  &  mysize=0.37

    multi_pos=FLTARR(4,2)
    multi_pos[*,1]=[mxs, mys,                    mxs+mxsize, mys+mysize]
    multi_pos[*,0]=[mxs, mys+1*mysize+myint*1.0, mxs+mxsize, mys+2*mysize+myint*1.0]

    FOR id=0,1 DO BEGIN
      pos=multi_pos[*,id]
      gems=id   ; 0: global domain, 1: gems domain+alpha

      ;limit=(gems EQ 1) ? [-5,75,45,145] : [-90,-180,90,180]
      limit=(gems EQ 1) ? [-5,75,60,160] : [-90,-180,90,180]
      fnl_lon = findgen(nlon)-180+ 0.5
      fnl_lat = findgen(nlat)-90 + 0.5

      IF ztype EQ 0 THEN BEGIN
        avg_diff=MEAN(fnlvar_diff,DIMENSION=3,/NAN)
        z=avg_diff
        print, ' :: monthly average data was selected'
        ;ct=70
        ct=73
      ENDIF ELSE IF ztype EQ 1 THEN BEGIN
        std_diff=STDDEV(fnlvar_diff,DIMENSION=3,/NAN)
        z=std_diff
        print, ' :: monthly stddev data was selected'
        ct=73
      ENDIF ELSE IF ztype EQ 2 THEN BEGIN
        sel=WHERE(date EQ sel_day,nsel)
        IF nsel NE 1 THEN STOP
        z=fnlvar_diff[*,*,sel]
        print, ' :: '+sel_day+' data was selected'
        ct=70
      ENDIF ELSE IF ztype EQ 3 THEN BEGIN
        sel=WHERE(date EQ sel_day,nsel)
        IF nsel NE 1 THEN STOP
        z=fnlvar_day[*,*,sel]
        print, ' :: '+sel_day+' data was selected'
        ct=70
   
        IF fnlty NE 5 THEN BEGIN
          title=varname
        ENDIF ELSE BEGIN
          title=varname+STRING(pres[sel_lay],F='(I-4)')
        ENDELSE
       
      ENDIF
      ;znan=WHERE(z LT MEAN(z)-3*STDDEV(z) AND z GT mean(z)+3*STDDEV(z), zval)
      ;z[znan]=!VALUES.F_NAN

      nlon=N_ELEMENTS(fnl_lon)  &  nlat=N_ELEMENTS(fnl_lat)
      lon=FLTARR(nlon,nlat)
      lat=FLTARR(nlon,nlat)
      FOR i = 0, nlon-1 DO BEGIN
      FOR j = 0, nlat-1 DO BEGIN
        lon[i,j]=fnl_lon[i]
        lat[i,j]=fnl_lat[j]
      ENDFOR
      ENDFOR
      
      IF fnlty EQ 1 THEN BEGIN
        levtop = (ztype NE 2) ? 7 : 25
        levtop = (ztype LE 1) ? 15 : levtop
      ENDIF ELSE IF fnlty EQ 4 THEN BEGIN 
        levtop = (ztype NE 2) ? 15 : 100
        levtop = (ztype EQ 1) ? 65 : levtop
      ENDIF ELSE BEGIN
        levtop = (ztype NE 2) ? 3 : 10
        levtop = (ztype EQ 1) ? 10 : levtop
      ENDELSE

      nlev=51
      IF ztype EQ 1 OR abson EQ 1 THEN BEGIN
        lev=(findgen(nlev))/(nlev) 
        lev=lev*levtop
      ENDIF ELSE IF ztype EQ 3 THEN BEGIN 
        lev=(findgen(nlev))*5+850
      ENDIF ELSE BEGIN 
        lev=(findgen(nlev)-(nlev-1)/2.)/((nlev-1)/2.)  ;nlev=odd number, min=-(nlev-1)/2, max=-(nlev-1)/2
        lev=lev*levtop
      ENDELSE
  ;lev=findgen(51)*6+800
      barcol=REVERSE(FIX(FINDGEN(nlev-1)*253/nlev))
      lev[0]=-999  &  lev[nlev-1]=9999
      clev_idx=FIX(INDGEN(10.)*((nlev-1)/10.))


      LOADCT, ct
      TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
      TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
      !P.COLOR=255
      !P.BACKGROUND=254
      !P.CHARSIZE=2
      !P.CHARTHICK=2

      clabel=INTARR(N_ELEMENTS(clev_idx))
      cstep=1
      clabel[INDGEN(N_ELEMENTS(clev_idx)/cstep)*cstep]=1  
      MAP_SET,  LIMIT=limit, XMARGIN=0, YMARGIN=0,/CYLIN,/NOERA,/NOBOR,POS=pos,title='!6'
      CONTOUR, z, lon, lat, /CELL_FILL, C_COLORS=barcol, LEVELS=lev,/OVER, $ 
               XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)],POS=pos
      CONTOUR, z, lon, lat, LEVELS=lev[clev_idx],/OVER, XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)],$
                            C_THICK=0.2,C_CHARTHICK=0.2,C_CHARSIZE=0.5, C_LABELS=clabel, POS=pos

      IF gems EQ 1 THEN MAP_CONTINENTS,/CONTINENT,/COUNT,MLINETHICK=1.8,/COUNTRY,/HIRES $
      ELSE              MAP_CONTINENTS,/CONTINENT,/COUNT,MLINETHICK=1.8;,COLOR=CGCOLOR('black');,/river
      xtlen=(gems EQ 1) ? 20 : 50
      ytlen=(gems EQ 1) ? 10 : 30
      title='' 
      CONTOUR, z, z, z, /NODAT,/NOERA,CHARSIZE=2,CHARTHICK=2, $
               XSTYLE=1, YSTYLE=1,YTICKFORMAT='(I3)',XTICKFORMAT='(i4)',XTICKINTERVAL=xtlen,YTICKINTERVAL=ytlen, $
               XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)], $
               TITLE=title, XTITLE = 'Longitude',YTITLE='Latitude', POS=pos
      IF omion EQ 1 THEN OPLOT,olons,olats, color=CGCOLOR('BLACK'),PSYM=cgSymCat(15),symsize=0.5
      COLORBAR_2, lev, barcol, /COL, YS =0.77, XS = 0.015, CHARSIZE=2, CHARTHICK=2, LEVELIND = clev_idx, $
                               FORMAT='(I-4)',UNIT=unit,/IVE,/NOFIRST;,/NOFRAME


    ENDFOR ; idomain

    cgPs_Close,density=800, /png
    file_trans_geun,out_png,/DEL

  ENDIF ELSE BEGIN   ;fnlty = 5

    out_ps='fnl_comp.ps'
    out_png='fnl_comp.png'
    CGPS_OPEN, out_ps ,XSIZE=10,YSIZE=14,/NOMATCH,/INCH

    loadct_wb, 33
    !x.minor =10  &  !x.style =1  &  !p.CHARSIZE=2.5
    !P.POSITION=[0.14,0.1,0.87,0.9]

    nsam=366*360L
    tot_diff=FLTARR(180,26,nsam)    
    itmp=0
    
    FOR iday=0,366-1 DO BEGIN
    FOR ilon=0,360-1 DO BEGIN      
      tot_diff[*,*,itmp]=fnlvar_diff[ilon,*,*,iday]
      itmp=itmp+1
    ENDFOR
    ENDFOR
    tavg_diff=MEAN(tot_diff,/NAN)
    tstd_diff=STDDEV(tot_diff,/NAN)

    nan_diff=WHERE(tot_diff LT tavg_diff-5*tstd_diff AND tot_diff GT tavg_diff+5*tstd_diff)
    ;tot_diff[nan_diff]=!VALUES.F_NAN

    IF ztype EQ 0 THEN BEGIN 
      std_diff= MEAN(tot_diff,DIMENSION=3,/NAN)
    ENDIF ELSE IF ztype EQ 1 THEN BEGIN
      std_diff= STDDEV(tot_diff,DIMENSION=3,/NAN)
    ENDIF
    ;std_diff=REFORM(fnlvar_mon[100,*,*,30])
    ;std_diff=REFORM(fnlvar_day[100,*,*,30]-fnlvar_mon[100,*,*,30])

    xrange= [0,6.5]
    ;xrange= [0,10]
    ;xrange= [0,30]
    ;xrange= [-10,10]
    ;xrange= [170,330]
    yrange = [1000,10]
    prepare_fnl_geun,'20080101',fnlout,lsttime=13.75
    pres=REVERSE(fnlout.pres)
    lats=fnlout.lat

    plot, std_diff[0,*], pres, xrange=xrange,/nodata,yrange=yrange,/ylog,$
          xtitle='Temperature difference (K)',ytitle='!6Pressure (hPa)', title='!6', $
          yminor=10,xminor=5, ytickname='!610'+'!U'+['3','2','1','0']+'!N', ystyle=9
  
    nlev=181
    levs=findgen(nlev)-90
    ncol=nlev-1
    color = findgen(ncol)*252/ncol+1

  ;+-------------------------+
    ; spectrum for each orbit
  ;+-------------------------+
    FOR i = 0 , nlev-2  do begin
      oplot, std_diff[i,*], pres, color=color[i], thick=1.5
    ENDFOR
    axis, yaxis=1, ytitle='Altitude (km)', yrange= [ptoz(yrange[0]), ptoz(yrange[1])], ystyle=1,ylog=0
    colorbar_2,levs, color, YS =0.02, XS = 0.73,CHARTHICK=2,LEVELIND = FINDGEN(100)*15, $
                    FORMAT='(i4)', /NOFIRST, LOWLEFT=[0.14,0.94],UNIT='Lat',/nofra, charsize=2

    cgPs_Close,density=800, /png
    file_trans_geun,out_png,/DEL

stop


  ENDELSE


stop

   

END

