;===================================================================================================
; This program plots OMI Averaging Kernal
;
; PROGRAM NAME:
;  hs_somi_avgk
;
; PURPOSE:
;  plotting OMI Averaging Kernel
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

PRO hs_somi_avgk

;++++++++++++++++++++++++++++++++++++++++++++++++++++++
; DATA process
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
read_xdr = 0   ;  0: read xdr , 1: read original output (*.out)
mean_ak  = 0   ;  0: process with selected pixel
               ;  1: process with mean of selecte pixels 

;omil2 = '/home/geun/2_O3PR/OZBOREAS-OMI/src/geun_storage/out_conserv/OMIO3PROF_geun-o08738_L0001-2000_X16-16-BY3.out'
;omil2 = '/home/geun/2_O3PR/OZBOREAS-OMI/src/OMIO3PROF_fnl_daily-o08738_L1000-1010_X16-16-BY3.out'
omil2 = '/home/geun/2_O3PR/OZBOREAS-OMI/src/geun_storage/out_conserv/OMIO3PROF_atmcontrol_line_daily-o09158_X09-09.out'
;omil2 = '/home/geun/2_O3PR/OZBOREAS-OMI/src/OMIO3PROF_atmcontrol_line_mon-o09158_X09-09.out'

PRINT, omil2
IF read_xdr EQ 1 THEN BEGIN
   omil2xdr = STRMID(omil2, 0,STRLEN(omil2)-4 ) + '.xdr'
   RESTORE, omil2xdr  
   PRINT, 'open data:', omil2xdr
ENDIF ELSE BEGIN
   read_omil2_file, omil2, nl, nf, nalb, ngas, naer, ominprof, $
      omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, $
      omicfrac, omictp, omicldflg, omiai, omiutc, omintp,  $
      ominw, omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow,            $
      omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos,          $
      ozprofs, omicol, omitrace, omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec,   $
      omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
      nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
      fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow, $
      orbits=orbits, orbspix=orbspix, orbepix=orbepix,  omiorb=omiorb, /get_avgk
ENDELSE

;;; filtering offset
maxavgres   = 2    & maxrms     = 3   ;  & maxai = 1.0     & maxglint = 20.0  
mincfrac    = 0.0  & maxcfrac   = 0.2   & minalb = 0.0    & maxalb  =0.2
minsza      = 0    & maxsza     = 30
minvza      = 0    & maxvza     = 20
maxyear     = 2014 & minmon     = 1     & maxmon  = 12
minlat      = 0    & maxlat     = 30
;;;;

;sel = where(omiavgres le maxavgres and omirms le maxrms and  $
sel = where( $
            ;omiavgres[*,0] le maxavgres and omirms[*,0] le maxrms and  $
            ;omiexval gt 0 and omiexval lt 100 and $  
            ;omiday ne 0 and omiyear le maxyear and omimon le maxmon and omimon ge minmon and $              
            ;omisza ge minsza and omisza le maxsza and $
            ;omivza ge minvza and omivza le maxvza and $
            ;omicfrac le maxcfrac and omicfrac ge mincfrac and $
            ;omialb(*,0) ge minalb and omialb(*,0) le maxalb and  $
            omiline EQ 1040   $
             ;omictp le 600 and  $
            ;abs(omilat(*,4)) ge minlat and omilat(*,4) le maxlat $
             , nsel)


IF nsel eq 0 then begin
  print , 'No pixl selected'
  sel = 0 & nsel = 1
ENDIF

IF mean_ak eq 0 then begin

  da = sel(0)
  print, omimon(da), omiday(da),  omicfrac(da), omialb(da,0), omisza(da,0), omivza(da,0), omictp(da,0)
  avgk   = reform(omiavgk(da, *, *))
  xa     = reform(ozprofs(da, 0, *)) & xae    = reform(ozprofs(da, 1, *))
  atmosz = reform(atmos(da,1, *))    & atmosp = reform( atmos(da,0, *))
  ztrp   = atmosz(omintp(da))        & ptrp   = atmosp(omintp(da))
  sza    = reform(omisza(da))        & cfrac = reform(omicfrac(da))  &  alb = reform(omialb(da,0))
  dfs    = diag_matrix (avgk)
  dfs_col = reform(omicol(da,*,3))
  dfs_col = ['TO:','SCO:','TCO:'] +string(dfs_col, format='(f4.2)')
  ctp= reform(omictp(da))

ENDIF ELSE BEGIN

  print, '# of averaging=',nsel, ominprof
  avgk = fltarr(nl, nl) & xa = fltarr(nl) & xae = fltarr(nl)
  atmosz = fltarr(nl+1) & atmosp = fltarr(nl+1) 
  ztrp   = fltarr(nsel) & ptrp= fltarr(nsel)
  sza    = mean(reform(omisza(sel)))
  cfrac =  mean(reform(omicfrac(sel))) & alb = mean(reform(omialb(sel,0)))
  ctp   =  mean(reform(omictp(sel)))
 
  FOR i = 0 , nsel-1 do begin
    ztrp(i) = atmos(sel(i),1,omintp(sel(i)))
    ptrp(i) = atmos(sel(i),0,omintp(sel(i)))
  ENDFOR

  ztrp = mean(ztrp) & ptrp = mean(ptrp)

  FOR i = 0, nl -1 do begin
    xa (i)    =  mean(ozprofs(sel, 0, i))
    xae (i)   =  mean(ozprofs(sel, 1, i))
    atmosz (i) =  mean(atmos(sel,1, i))
    atmosp (i) =  mean(atmos(sel,0, i))
    FOR j = 0 , nl -1 do begin
      avgk(i,j) = mean(omiavgk(sel,i,j))
      ;print  , i, j, avgk(i,j), stddev(omiavgk(sel,i,j))
    ENDFOR
  ENDFOR
  atmosz (nl) =  mean(atmos(sel,1, nl))
  atmosp (nl) =  mean(atmos(sel,0, nl))   

ENDELSE ; mean_ak cond


;++++++++++++++++++++++++++++++++++++++++++++++++++++++
; PLOT process
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
IF size(avgk,/n_di) ne 2 then begin
   print , 'check data dimension'
endif
;set_plot, 'ps'
;figname = 'test.ps'
;device, file=figname, /portrait, /inches, /color, xsize=10, ysize=6, xoffset=0.5, yoffset=0.5

out_name='test_avgk'
out_ps=out_name+'.ps'
out_png=out_name+'.png'
CGPs_OPEN, out_ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5
;window, xs=700, ys=700
;erase

loadct_wb, 33

;------------------------------------------------
;(1 ) plot of avgk
st = 0
ak     = reform(avgk) & ak0   = ak
zs     = atmosz       & ps    = atmosp
p_mid  = 0.5*(ps(0:nl-1)+ps(1:nl))     & z_mid = 0.5*(zs(0:nl-1)+zs(1:nl))
nl     = n_elements(zs)-1
aksym  = 1; 0 no sym // 1;makpeak //2;ak(i,i) ; default = 1
aktype = 0 ; 1  = ak//0=ak0 ; default = 1
cap= '!6SZA=' + string(round(sza),format='(I2)') + $
          '!9%!X, !8f!X!Dc!N=' +  string(cfrac, format='(f4.2)') + $
          ', !4a!X!Ds!N=' +  string(alb, format='(f4.2)')


;------------------------------------------------
xaperr  = xae
xaperrn = xaperr / (zs(0:nl-1) - zs(1:nl))
for  j = 0, nl - 1 do begin
  ak(*, j) = ak(*, j) / xaperrn(j) * xaperrn
  ak(*, j) = ak(*, j) / (zs(j)-zs(j+1))*2.5
endfor
xrange = [ -0.1, 0.6]
title  =  'Asjusted AVGK'
IF aktype eq 0 then begin
   ak = ak0
   xrange = [min(ak), max(ak)]
   xrange = [-2,2]
   title = ' Origianl AVGK'
endif

; drawing option
;!p.multi = [0,2,1]

cols = fix(findgen(nl) * (252. / (nl-1)) + 2)      
;Position = get_pos( 2, 1, 0.08, 0.35, 0.15, 0.86, 0.74,0.1)
Position = [0.1,0.08,0.9,0.85]

loadct_wb, 33
!p.position = position
yrange = [1000,0.3] & trp =  ptrp &  alt = p_mid
plot, ak(*, 0), p_mid(*), xrange=xrange,/nodata,yrange=yrange,/ylog,$
      xtitle='!6Rows of averaging Kernel matrix',ytitle='Pressure (hPa)', title=cap, $
      yminor=10,xminor=5, ytickname='!610'+'!U'+['3','2','1','0']+'!N', ystyle=9
plots, [0, 0], yrange, linestyle=1, color=1, thick=3
if cfrac ge 0.5 then plots, xrange, [1,1]*ptoz(ctp), thick=2, linestyle=1
plots,xrange, [trp,trp], thick=2, linestyle=1
For j = st, nl - 1 do begin
  oplot, ak(st:nl-1, j), alt(st:nl-1), color=cols(j), thick=2.
  if alt(j) lt min(yrange) or alt(j) gt max(yrange) then continue
  if aksym eq 1 then begin
    tmp = ak(*,j)
    da = where( tmp eq max(tmp))
    tmp=tmp(da)
    plots,tmp, alt(da), psym=plotsym_fn(/box,scale=1.5), color=cols(j)
  endif else if aksym eq 2 then begin
    plots,ak(j,j), alt(j), psym=sym(2,symsize=1.5), color=cols(j)
  endif
    ;plots, total(ak0(j,*)), alt(j), psym=1
endfor

legend2, string(z_mid(st:nl-1), format='(f4.1)'),textcolor=cols(st:nl-1), spacing=0.01,/right, box=0
;  legend2, dfs_col, box=0, /right
axis, yaxis=1, ytitle='Altitude (km)', yrange= [ptoz(1000), ptoz(0.3)], ystyle=1,ylog=0
xyouts, !P.position(0), !p.position(3)+0.1, title, /normal


;device, /close
;display, figname, /noand
;display_ps2png, figname

cgPs_Close,density=800, /png
file_trans_geun,out_png,/DEL



;------------------------------------------------
;(2) map of avgk

out_name='test_avgk2'
out_ps=out_name+'.ps'
out_png=out_name+'.png'
CGPs_OPEN, out_ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5

; parameter
ak     = reform(avgk)  & ak0 = ak
zs     = atmosz        & ps  = atmosp
p_mid  = zmid(ps)
nl     = n_elements(zs)-1
;------------------------------------------------
xaperr = xae
xaperrn= xaperr / (zs(0:nl-1) - zs(1:nl))

for  j = 0, nl - 1 do begin
  ak(*, j) = ak(*, j) / xaperrn(j) * xaperrn
  ak(*, j) = ak(*, j) / (zs(j)-zs(j+1))
endfor

data = ak
loadct_wb, 33
xrange=[1200, 10]
nlev = 13 & ncol=nlev-1
levels = findgen(nlev)*0.02-0.1-0.02
levels(0) = min(avgk) & levels(nlev-1) = max(avgk)
del = fix(255/(ncol))
colors = indgen(ncol)*del

plot, findgen(10), findgen(10), /nodata,/ylog,/xlog, xrange=xrange, yrange=xrange,$
      xtitle = '!6Pressure(hPa)', ytitle='!6Pressure(hPa)', xminor=9,yminor=9, $
      title  = 'OMI Averaging Kernel matrix',position=[0.12,0.1,0.9,0.92]
FOR j = 0 , nl-1 do begin        
   FOR i = 0, nl-1 do begin
      xcoord = [ps(i), ps(i+1), ps(i+1), ps(i)]
      ycoord = [ps(j), ps(j), ps(j+1), ps(j+1)]
      tmp = abs( levels-data(i,j))
      da  = where( tmp eq min(tmp))
      if data(i,j) lt levels(0) then      da = 0
      if data(i,j) gt levels(ncol) then   da = ncol-1
      if levels(da) gt data(i,j) then begin
         da = da -1
      endif
      
      mycol = colors(da)

      if  ps(i) ge 10 and ps(j) ge 10 then begin 
        ;print, levels(da), data(i,j), levels(da+1), format='(3f7.3)        
        if da ne 0 and levels(da)     gt data(i,j) then stop
        if da ne ncol-1 and data(i,j) ge levels(da+1)  then stop
        polyfill, xcoord, ycoord, color = mycol(0)
      endif
  ENDFOR
ENDFOR
plots,[ptrp, ptrp], [max(ps), ptrp], thick=5
plots, [max(ps), ptrp], [ptrp, ptrp], thick=5
colorbar_2, levels, colors,format='(f5.2)', $
             xs = 0.02, ys=0.8, unit='',/col, /nofirst, /ive
    
cgPs_Close,density=800, /png
file_trans_geun,out_png,/DEL



stop
END
