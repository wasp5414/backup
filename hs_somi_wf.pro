;===================================================================================================
; This program plots OMI Weighting Functions
;
; PROGRAM NAME:
;  hs_somi_wf
;
; PURPOSE:
;  plotting OMI Weighting Function
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
;  Jbak
;  Daegeun Shin (geun)
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  Jbak version 0.
;  Updated by geun
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO hs_somi_wf

read_xdr = 0   ;  1: read xdr , 0: read original output (*.out)
 
;omil2 = '/home/geun/2_O3PR/OZBOREAS-OMI/src/geun_storage/out_conserv/OMIO3PROF_geun-o08738_L0001-2000_X16-16-BY3.out'
omil2 = '/home/o3p_hs/SOMIPROF/OZBOREAS-OMI/src/OMIO3PROF_sellonlat_test-o21272_L1060-1163_X01-07.out'

IF read_xdr eq 0 then begin

read_omil2_file, omil2, nl, nf, nalb, ngas, naer, ominprof, $
     omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, omicfrac, omictp, omicldflg, omiai, omiutc,  $ 
     omintp, ominw,  omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow, $
     omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos, $
     ozprofs,omicol, omitrace,  omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec, $
     omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
     nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
     fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow, $
     /get_snr, /get_fitvar,/get_avgk ,/get_wf, /get_covar, /get_correl, /get_contri, /get_fitres,$
     varname=varname, omiorb=omiorb
     print, 'open data:', omil2
;stop
ENDIF ELSE BEGIN
     omil2xdr = STRMID(omil2, 0,STRLEN(omil2)-4 ) + '.xdr'
     RESTORE, omil2xdr
     PRINT, 'open data:', omil2xdr
ENDELSe

;;; filtering offset
maxavgres   = 2    & maxrms   = 3      ;& maxai = 1.0    & maxglint = 20.0  
maxcfrac    = 1    & mincfrac = 0.0    & minalb = 0.0   & maxalb  = 1  
minsza      = 0    & maxsza   = 50     
minvza      = 20   & maxvza   = 45
maxyear     = 2008 & minmon   = 1      & maxmon  = 12
minlat      = 0    & maxlat   = 90

;;;;
;sel = where(omiavgres le maxavgres and omirms le maxrms and  $
sel = where( $
            omiavgres[*,0] le maxavgres and omirms[*,0] le maxrms and  $
            omiexval gt 0 and omiexval lt 100 and $  
            omiday ne 0 and omiyear le maxyear and omimon le maxmon and omimon ge minmon and $              
            omisza ge minsza and omisza le maxsza and $
            ;omivza ge minvza and omivza le maxvza and $
            omicfrac le maxcfrac and omicfrac ge mincfrac and $
            omialb(*,0) ge minalb and omialb(*,0) le maxalb and $
            ;omictp le 600 and  $
            omilat(*,4) ge minlat and omilat(*,4) le maxlat and omiline ge 1000 , nsel)


IF nsel eq 0 then begin
   print , 'No pixl selected'
   da = 0 
ENDIF
IF nsel ne 1 then  print , 'Multiple pixl selected'
da = sel(0)

print, omimon(da), omiday(da),  omicfrac(da), omialb(da,0)
fitvar = reform(omifitvar(da,0,*))
alb    = reform(omialb(da,*))
zs     = reform(atmos(da,1, *))    & PS = reform( atmos(da,0, *))
ozprof = reform(ozprofs(da,2,*))
nlamda     = reform(ominw(da))   ; # of wavelength
nw = nlamda(0)
waves  = reform(omiwaves(da,0:nw-1))
wsel1  = where( waves le 310)       ; uv1
wsel2  = where( waves ge 310)       ; uv2
wfs        = fltarr(nw, nf, 2)
wfs(*,*,0) = reform(omiwf(da, 0:nw-1, *))
FOR i = 0 , nf -1 do begin
    var = fitvar(i)
    tmp = reform(wfs(*,i,0))
    wfs(*,i,1) = tmp(*)*var
ENDFOR
        
snr    = reform(omisnr(da,0:nw-1))            ; signal to noise
rad    = reform(omifitspec(da,0:nw-1))        ; fitting spectrum
ozfidx = where(varname eq 'oz01')                              ; ozindex first 
ozlidx = where(varname eq 'oz'+string(nl, format='(i2.2)'))    ; ozindex last
albidx = min(where(strpos(varname,'ba') ne -1, nalb))          ; albedo, nalb=3
cfidx  = min(where(strpos(varname,'ecfr') ne -1, ncf))         ; cf index


;++++++++++++++++++++++++++++++++++++++++++++++++++++++
; PLOT process
p_mid  = 0.5*(ps(0:nl-1)+ps(1:nl))   
z_mid = 0.5*(zs(0:nl-1)+zs(1:nl))
;set_plot, 'ps'
;figname = 'test.ps'
;device, file=figname, /portrait, /inches, /color, xsize=8, ysize=9.0, xoffset=0.5, yoffset=0.5
out_name='test_reffitparam'
out_ps=out_name+'.ps'
out_png=out_name+'.png'
;++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;;;; common varialbes for WF as a function of wavelength
xtitle   = '!6Wavelength (nm)'
ytitles  = ['!6dnlI/dX','!6dlnI/dlnX']
xrange   = [min(waves),max(waves)] 
yrange   = [0, max(zs)]

plot_ty = 0   ; 0: o3prof, 1: o3, 2: albedo, 3: fitting param, 4: reference fitting param 


IF plot_ty EQ 0 THEN BEGIN
  CGPs_OPEN, out_ps ,xsize=10, ysize=7,/nomatch, /inch, xoffset=0.5, yoffset=0.5

  ; multiple window configuration
  mxs=0.1      ; xstart
  mxint=0.12    ; xinterval
  mxsize=0.37   ; xsize
  mys=0.12      ; ystart
  myint=0.00    ; yinterval
  mysize=0.8   ; ysize

  multi_pos1=[mxs,mys,mxs+mxsize,mys+mysize]
  multi_pos2=[mxs+1*mxsize+mxint,mys,mxs+2*mxsize+mxint,mys+mysize]
  position = [[multi_pos1],[multi_pos2]]

  !p.multi = [0, 2, 1]
  !x.minor =10
  !x.style =1
  !x.ticklen=0.02

ENDIF ELSE BEGIN
  CGPs_OPEN, out_ps ,xsize=9, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5

  ; multiple window configuration
  mxs=0.18      ; xstart
  mxint=0.00    ; xinterval
  mxsize=0.8   ; xsize
  mys=0.09      ; ystart
  myint=0.15    ; yinterval
  mysize=0.35    ; ysize

  multi_pos1=[mxs,mys,mxs+mxsize,mys+mysize]
  multi_pos2=[mxs,mys+mysize+myint,mxs+mxsize,mys+2*mysize+myint]
  position = [[multi_pos1],[multi_pos2]]

  !p.multi   = [0, 1, 2]
  !x.minor   = 10
  !x.style   = 1
  !x.ticklen = 0.04

ENDELSE

loadct_wb, 33
color3 = [255,cgcolor('red'), cgcolor('blue')]

IF plot_ty EQ 0 THEN BEGIN
  ;@ O3 WF as a function of altitude
  title = '!6Ozone WF'

  xrange=[-0.4, 0]
  plot, findgen(10), findgen(10), xrange=xrange, yrange=yrange, /nodata,$
       xtitle='!6dlnI/dX',title=title, position = position(*,0), ytitle = 'Altitude(km)'
  FOR k =0,nw-1  do  oplot, reform(wfs(k,ozfidx:ozlidx,0)), z_mid, color=k

  xrange=[-0.4, 0]
  plot, findgen(10), findgen(10), xrange=xrange, yrange=yrange   , /nodata,$
        xtitle='!6dlnI/dlnX',title=title, position= position(*,1), ytitle = 'Altitude(km)'
  FOR k =0,nw-1  do oplot,reform(wfs(k,ozfidx:ozlidx,1)),z_mid, color=k
  
ENDIF ELSE IF plot_ty EQ 1 THEN BEGIN
  ;@ O3 WF as a function of wavelength
  ;title = 'Ozone WF'
  title = '!6Ozone WF'

  yrange=[-0.4, 0]
  plot, indgen(10), indgen(10), xrange=xrange, yrange=yrange, /nodata,$
        ytitle=ytitles(0),xtitle=xtitle,title=title, position=position(*,0)
  FOR k =0,nl-1  do oplot,waves, wfs(*,k+ozfidx,0), color=k*10

  yrange=[-0.4, 0]
  plot, indgen(10), indgen(10), xrange=xrange, yrange=yrange, /nodata,$
        ytitle=ytitles(1),xtitle=xtitle,title=title, position=position(*,1)
  FOR k =0,nl-1  do oplot,waves, wfs(*,k+ozfidx,1), color=k*10

ENDIF ELSE IF plot_ty EQ 2 THEN BEGIN
;@ albedo WF as a function of wavelength
  IF nalb ne 0 then begin
    title = '!6Albedo WF'
    item = ['oth UV1','oth UV2','1th UV2']
    form = ['(f8.2)','(f8.2)','(e15.2)']
    var =  fitvar(albidx:albidx+nalb-1,0)
    svar = strarr(nalb)
    FOR i = 0 , 1 do begin
      yrange=[-0.01, max(wfs(*,albidx:albidx+nalb-1,i))*1.2]
      plot, indgen(10), indgen(10), xrange=xrange, yrange=yrange, /nodata,$
            ytitle=ytitles(i),xtitle=xtitle,title=title, position=position(*,i)
      FOR k = 0 , nalb -1 do begin
        svar(k) = item(k)+' : '+strtrim(string(var(k), format=form(k)),2)
        wsel = wsel2
        if k eq 0 then wsel = wsel1
        oplot, waves(wsel),wfs(wsel,albidx(0)+k,i),psym=1, color=color3(k)
      ENDFOR
      plots, xrange,[yrange(1), yrange(1)], linestyle=0
      legend2,svar,textcolor=color3(0:nalb-1), box=0 
    ENDFOR
  ENDIF ELSE BEGIN
    PRINT, "No albed WF due to 'nalb = 0'"  
  ENDELSE

ENDIF ELSE IF plot_ty EQ 3 THEN BEGIN
;@ fitting parameter as fitting window
  nlist     = 7
  list_name = ['ozs',     'sld',    'shi',    'rin',    'dec',     'ins',   'inr']
  list_title = ['I/O3 shift',     'slit diff',    'I/F shift',    'ring',    'dec',     'ins',   'inr'] + 'WF'
  list_form = ['(f10.5)','(f10.5)','(f10.5)','(f10.2)','(f10.5)', '(f10.5)', '(e15.2)']
  list_unit = ['nm',       'nm',      '',    '',     '',             '',          '']
  item      = ['UV1','UV2']
  ;FOR li = 0, nlist-1 do begin
  sel_li = 0 
  FOR li = sel_li, sel_li do begin
    unit = list_unit(li)
    idx  = min( where(strpos(varname,list_name(li)) ne -1 ,nvar))
    form = list_form(li)
    IF nvar eq 0 then stop ;continue
    svar = strarr(nvar)
    var  = fitvar(idx:idx+nvar-1,0)
    ;print, 'li','list_name', 'nvar',  format='(3a10)'   ;,var
    ;print, li, list_name(li), nvar, format='(i10, a10, i10)'  ; ,var
    print, li, list_name(li), nvar, var

    title ='!6'+ list_title(li)
    FOR i = 0 , 1 do begin
      yrange=[ min(wfs(*,idx:idx+nvar-1,i))*0.8, max(wfs(*,idx:idx+nvar-1,i))*1.2 ]
      if min(wfs(*,idx:idx+nvar-1,i)) lt 0 then yrange(0) = min(wfs(*,idx:idx+nvar-1,i))*1.2
      plot, indgen(10), indgen(10), xrange=xrange, yrange=yrange, /nodata,$
            ytitle=ytitles(i),xtitle=xtitle,title=title, position=position(*,i)
      FOR k = 0 , nvar -1 do begin
        if nvar ne 1 then svar(k) = item(k)+' : '+strtrim(string(var(k), format=form),2)+unit
        if nvar eq 1 then svar(k) = list_name(li)+' : '   +strtrim(string(var(k), format=form),2)+unit
        wsel = wsel1
        if k eq 1 then wsel = wsel2
        oplot, waves(wsel),wfs(wsel,idx+k,i), color=color3(k)
      ENDFOR
      plots, xrange,[yrange(1), yrange(1)], linestyle=0
      legend2,svar,textcolor=color3(0:nvar-1), box=0 
    ENDFOR
  ENDFOR

ENDIF ELSE IF plot_ty EQ 4 THEN BEGIN
;@ reference fitting parameter 
  nlist     = 5 
  list_name = ['bro',     'hcho',    'so2',    'rin',    'dec',     'ins',   'inr']
  list_title= ['bro',     'hcho',    'so2',    'ring',    'dec',     'ins',   'inr'] + 'WF'
  list_form = ['(e15.2)', '(e15.2)', '(e15.2)','(e15.2)','(e15.2)','(e15.2)' ]
  list_unit = ['',       '',      '',    '',     '',             '',          '']

  ;FOR li = 0, nlist-1 do begin
  sel_li = 3 
  FOR li = sel_li, sel_li do begin
    unit = list_unit(li)
    idx  = min( where(strpos(varname,list_name(li)) ne -1 ,nvar))
    form = list_form(li)
    IF nvar eq 0 then continue
    svar = strarr(nvar)
    var  = fitvar(idx:idx+nvar-1,0)
    print, li, list_name(li), nvar, var

    title ='!6'+ list_title(li)
    FOR i = 0 , 1 do begin
       yrange=[ min(wfs(*,idx:idx+nvar-1,i)), max(wfs(*,idx:idx+nvar-1,i))*1.2 ]
       if min(wfs  (*,idx:idx+nvar-1,i)) lt 0 then yrange(0) = min(wfs(*,idx:idx+nvar-1,i))*1.2
       plot, indgen(10), indgen(10), xrange=xrange, yrange=yrange, /nodata,$
             ytitle=ytitles(i),xtitle=xtitle,title=title, position=position(*,i)
       FOR k = 0 , nvar -1 do begin
         svar(k) = list_name(li)+' : '   +strtrim(string(var(k), format=form),2)+unit
         oplot, waves,wfs(*,idx+k,i), color=color3(k)
       ENDFOR

      plots, xrange,[yrange(1), yrange(1)], linestyle=0
      legend2,svar,textcolor=color3(0:nvar-1), box=0 
    ENDFOR
  ENDFOR

ENDIF ; plot_type loop

cgPs_Close, density=800, /png

;device, /close
;display, figname

stop
END
